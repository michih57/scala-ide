/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.eclipse
package refactoring
package rename

import scala.collection.mutable.ListBuffer
import scala.language.reflectiveCalls
import scala.tools.eclipse.ScalaProject
import scala.tools.eclipse.javaelements.ScalaSourceFile
import scala.tools.eclipse.logging.HasLogger
import scala.tools.eclipse.refactoring.FullProjectIndex
import scala.tools.eclipse.refactoring.RefactoringAction
import scala.tools.eclipse.refactoring.ScalaIdeRefactoring
import scala.tools.eclipse.refactoring.ui.NewNameWizardPage
import scala.tools.refactoring.analysis.GlobalIndexes
import scala.tools.refactoring.analysis.NameValidation
import scala.tools.refactoring.implementations.Rename
import org.eclipse.core.resources.IFile
import org.eclipse.core.runtime.IProgressMonitor
import org.eclipse.jdt.core.ICompilationUnit
import org.eclipse.jdt.core.IJavaElement
import org.eclipse.jdt.core.JavaCore
import org.eclipse.jdt.core.dom.AST
import org.eclipse.jdt.core.dom.ASTParser
import org.eclipse.jdt.core.dom.ASTVisitor
import org.eclipse.jdt.core.dom.CompilationUnit
import org.eclipse.jdt.core.dom.MethodInvocation
import org.eclipse.jdt.core.dom.rewrite.ASTRewrite
import org.eclipse.jdt.core.refactoring.IJavaRefactorings
import org.eclipse.jdt.core.refactoring.descriptors.RenameJavaElementDescriptor
import org.eclipse.jdt.core.search.IJavaSearchConstants
import org.eclipse.jdt.core.search.SearchEngine
import org.eclipse.jdt.core.search.SearchMatch
import org.eclipse.jdt.core.search.SearchPattern
import org.eclipse.jdt.core.search.SearchRequestor
import org.eclipse.jdt.internal.corext.refactoring.changes.RenameCompilationUnitChange
import org.eclipse.ltk.core.refactoring.Change
import org.eclipse.ltk.core.refactoring.CompositeChange
import org.eclipse.ltk.core.refactoring.NullChange
import org.eclipse.ltk.core.refactoring.Refactoring
import org.eclipse.ltk.core.refactoring.RefactoringCore
import org.eclipse.ltk.core.refactoring.RefactoringDescriptor
import org.eclipse.ltk.core.refactoring.RefactoringStatus
import org.eclipse.ltk.core.refactoring.TextEditBasedChange
import org.eclipse.ltk.core.refactoring.resource.RenameResourceChange
import org.eclipse.ltk.core.refactoring.DocumentChange
import scala.tools.eclipse.util.FileUtils
import org.eclipse.ltk.core.refactoring.TextFileChange
import org.eclipse.ui.PlatformUI
import org.eclipse.ui.texteditor.DocumentProviderRegistry
import org.eclipse.jdt.ui.JavaUI
import org.eclipse.ltk.core.refactoring.TextEditChangeGroup
import org.eclipse.text.edits.TextEditGroup

/**
 * Renames using a wizard and a change preview. This action is used
 * for all global rename refactorings and also from the RenameParticipant.
 *
 * When a class is renamed that has the same name as the source file,
 * the file is renamed too.
 */
class GlobalRenameAction extends RefactoringAction {

  def createRefactoring(start: Int, end: Int, file: ScalaSourceFile) = new RenameScalaIdeRefactoring(start, end, file)

  /**
   * The actual refactoring instance that is used by the RefactoringAction.
   */
  class RenameScalaIdeRefactoring(start: Int, end: Int, file: ScalaSourceFile, withJavaParticipant: Boolean = true)
    extends ScalaIdeRefactoring("Rename", file, start, end) with FullProjectIndex {

    val project = file.scalaProject

    var name = ""

    def refactoringParameters = name

    val refactoring = withCompiler { compiler =>
      new Rename with GlobalIndexes with NameValidation {
        val global = compiler

        /* The initial index is empty, it will be filled during the initialization
         * where we can show a progress bar and let the user cancel the operation.*/
        var index = EmptyIndex
      }
    }

    val javaParticipant =
      if(withJavaParticipant) Some(new ScalaRenameParticipant(this)) else None

    /**
     * A cleanup handler, will later be set by the refactoring
     * to remove all loaded compilation units from the compiler.
     */
    var cleanup = () => ()

    /* (non-Javadoc)
     * @see scala.tools.eclipse.refactoring.ScalaIdeRefactoring#checkInitialConditions(org.eclipse.core.runtime.IProgressMonitor)
     */
    override def checkInitialConditions(pm: IProgressMonitor): RefactoringStatus = {
      logger.debug("checking initial conditions in RenameScalaIdeRefactoring")
      val status = super.checkInitialConditions(pm)

      if(!status.hasError) {

        val selectedSymbol = preparationResult.right.get.renameProcessor.selectedSymbol

        name = selectedSymbol match {
          case sym if sym.isSetter => sym.getter(sym.owner).nameString
          case sym => sym.nameString
        }

        val (index, cleanupHandler) = buildFullProjectIndex(pm, name :: Nil)

        import scala.language.reflectiveCalls

        refactoring.index = index

        // will be called after the refactoring has finished
        cleanup = cleanupHandler
      }

      if(pm.isCanceled) {
        status.addWarning("Indexing was cancelled, types will not be renamed.")
      }

      if(!status.hasError() && javaParticipant.isDefined){
        status.merge(javaParticipant.get.checkInitialConditions(pm))
      }

      status
    }

    override def checkFinalConditions(pm: IProgressMonitor): RefactoringStatus = {
      logger.debug("checking final conditions in RenameScalaIdeRefactoring")
      val status = super.checkFinalConditions(pm)

      val selectedSymbol = selection().selectedSymbolTree map (_.symbol) getOrElse refactoring.global.NoSymbol

      refactoring.global.askOption { () =>
        refactoring.doesNameCollide(name, selectedSymbol)
      } map {
        case Nil => ()
        case collisions =>
          val names = collisions map (s => s.fullName) mkString ", "
          status.addWarning("The name \""+ name +"\" is already in use: "+ names)
      }

      if(!status.hasError() && javaParticipant.isDefined) {
        status.merge(javaParticipant.get.checkFinalConditions(pm))
      }

      status
    }

    override def getPages = new NewNameWizardPage((s => name = s), refactoring.isValidIdentifier, name, "refactoring_rename") :: Nil

    override def createChange(pm: IProgressMonitor) = {
      logger.debug("creating change in RenameScalaIdeRefactoring")
      val compositeChange = super.createChange(pm)

      val selectedTree = preparationResult.right.get.renameProcessor.selectedTree

      selectedTree match {
        case impl: refactoring.global.ImplDef if impl.name.toString + ".scala" == file.file.name =>
          file.getCorrespondingResource match {
            case ifile: IFile =>
              compositeChange.add(new RenameResourceChange(ifile.getFullPath, name + ".scala"))
            case _ =>
          }
        case _ =>
      }

      val participantChange = javaParticipant.map(_.createChange(pm))

      participantChange.foreach(compositeChange.add(_))

      cleanup()

      compositeChange
    }
  }

  // FIXME work in progress...
  class ScalaRenameParticipant(scalaRename: RenameScalaIdeRefactoring) extends HasLogger {

    var foreignScalaRenamer: Option[ForeignScalaRenamer] = None

//    var renamer: Option[RenameJavaElementDescriptor] = None
//    var wrappedRefactoring: Option[Refactoring] = None

    def checkInitialConditions(pm: IProgressMonitor): RefactoringStatus = {
      logger.debug("checking initial conditions in ScalaRenameParticipant")
      val selection = scalaRename.selection()
      val selectedSymbol = {
        val s = scalaRename.preparationResult().right.get.renameProcessor.selectedSymbol
        if(s.isModule) s.moduleClass else s
      }

      val noSym = scalaRename.refactoring.global.NoSymbol

      val jElem = scalaRename.refactoring.global.ask{() =>
        scalaRename.refactoring.global.getJavaElement(selectedSymbol)
      }

      def javaRefactoringConstantForSymbol(s: scalaRename.refactoring.global.Symbol): Option[String] = {
        if(s.isClass || s.isTrait) {
          logger.debug("have scala class or trait")
          Some(IJavaRefactorings.RENAME_TYPE)
        } else if(s.isModuleClass) {
          logger.debug("have scala module class")
          Some(IJavaRefactorings.RENAME_TYPE)
        } else if(s.isModule){
          logger.debug("have scala module")
          Some(IJavaRefactorings.RENAME_TYPE)
        } else if(s.isMethod) {
          Some(IJavaRefactorings.RENAME_METHOD) // FIXME not working since java refactoring fails if it can't find the method definition
        } else if(s.isVal) {
          logger.debug("have scala val")
          Some(IJavaRefactorings.RENAME_FIELD)
        }
        else {
          None
        }
      }

      val id = javaRefactoringConstantForSymbol(selectedSymbol)

      foreignScalaRenamer = jElem.flatMap{ j =>
        if(id == Some(IJavaRefactorings.RENAME_TYPE)) {
          Some(new TypeRenamer(scalaRename, j))
        } else if(id == Some(IJavaRefactorings.RENAME_METHOD)) {
          Some(new MethodRenamer(scalaRename, j))
        } else {
          None
        }
      }

      new RefactoringStatus


    }

    def checkFinalConditions(pm: IProgressMonitor): RefactoringStatus = {
      logger.debug("checking final conditions in ScalaRenameParticipant")

      foreignScalaRenamer.map(_.checkConditions(pm)).getOrElse(new RefactoringStatus)
    }

    def createChange(pm: IProgressMonitor): Change = {
      foreignScalaRenamer.map(_.createChange(pm)).getOrElse(new NullChange)
    }



  }

  trait ForeignScalaRenamer {
    def checkConditions(pm: IProgressMonitor): RefactoringStatus
    def createChange(pm: IProgressMonitor): Change
  }

  class TypeRenamer(nativeScalaRename: RenameScalaIdeRefactoring, javaElem: IJavaElement) extends ForeignScalaRenamer with HasLogger {

    val renamer: Option[RenameJavaElementDescriptor] = {
      val contrib = RefactoringCore.getRefactoringContribution(IJavaRefactorings.RENAME_TYPE)
      if (contrib.createDescriptor().isInstanceOf[RenameJavaElementDescriptor]) {
        val desc: RenameJavaElementDescriptor = contrib.createDescriptor().asInstanceOf[RenameJavaElementDescriptor]
        desc.setJavaElement(javaElem)
        desc.setUpdateReferences(true)
        Some(desc)
      } else {
        None
      }
    }

    var wrappedRefactoring: Option[Refactoring] = None

    def checkConditions(pm: IProgressMonitor): RefactoringStatus = {
      logger.debug("checking conditions in TypeRenamer")
      val initialConditionsState = renamer map { r =>
        r.setNewName(nativeScalaRename.refactoringParameters)
        val state = r.validateDescriptor()
        val refactoring = r.createRefactoring(state)
        wrappedRefactoring = Some(refactoring)
        state.merge(refactoring.checkInitialConditions(pm))
        state
      } getOrElse (new RefactoringStatus)


      val finalConditionsState = wrappedRefactoring.map(r => r.checkFinalConditions(pm)).getOrElse(new RefactoringStatus)
      finalConditionsState.merge(initialConditionsState)
      finalConditionsState
    }

    def createChange(pm: IProgressMonitor): Change = {
      logger.debug("creating change in TypeRenamer")
      def isNotPureJavaChange(c: Change) = c match {
        case _: RenameCompilationUnitChange => true
        case _ => false
      }

      def debugChange(c: Change): Unit = c match {
        case composite: CompositeChange =>
          logger.debug("composite change, listing children:")
          composite.getChildren().foreach(debugChange)
        case _ => logger.debug(s"change class: ${c.getClass().getCanonicalName()}")
      }

      def filterPureJavaChanges(c: Change): Change = c match {
        case composite: CompositeChange =>
          val children = composite.getChildren
          val nonPureChildren = children.filter(isNotPureJavaChange)
          nonPureChildren.foreach(composite.remove)
          val compositeChildren = children.collect{case comp: CompositeChange => comp}
          val pureComposites = compositeChildren.map(filterPureJavaChanges).collect{case cc: CompositeChange => cc}
          val emptyComposites = pureComposites.filter(_.getChildren().isEmpty)
          emptyComposites.foreach(composite.remove)
          composite
        case _ => c
      }

      new CompositeChange("java participant") {
        wrappedRefactoring.foreach { r =>
          logger.debug("creating change in java participant of scala rename")
          val change = r.createChange(pm)
          debugChange(change)
          val pureJavaChanges = filterPureJavaChanges(change)
          debugChange(change)
          add(pureJavaChanges)
        }
      }
    }
  }

  class MethodRenamer(nativeScalaRename: RenameScalaIdeRefactoring, method: IJavaElement) extends ForeignScalaRenamer with HasLogger {

    def checkConditions(pm: IProgressMonitor): RefactoringStatus = {
      logger.debug("checking conditions in MethodRenamer")
      new RefactoringStatus
    }

    def createChange(pm: IProgressMonitor): Change = {
      logger.debug("creating change in MethodRenamer")
      val project = nativeScalaRename.project

      val changes = ForeignScalaMethodRename.createChange(method, project, pm, nativeScalaRename.name)

      val compositeChange = new CompositeChange("all method renames") {
        changes foreach add
      }
      compositeChange
    }

   private def findCusOfJavaReferences(jElem: IJavaElement, project: ScalaProject, pm: IProgressMonitor): List[ICompilationUnit] = {
      val pattern = SearchPattern.createPattern(jElem, IJavaSearchConstants.ALL_OCCURRENCES)
      val javaProject: IJavaElement = JavaCore.create(project.underlying)
      val scope = SearchEngine.createJavaSearchScope(Array(javaProject))
      val engine = new SearchEngine()
      var cus: ListBuffer[IJavaElement] = ListBuffer.empty
      val requestor = new SearchRequestor {
        override def acceptSearchMatch(m: SearchMatch) {

          logger.debug(s"match resource class: ${m.getResource().getClass().getCanonicalName()}")
          val compilationUnit = JavaCore.create(m.getResource())
          logger.debug(s"found element in cu: ${compilationUnit}")
          logger.debug(s"found element: $m")
          if (m.getElement().isInstanceOf[IJavaElement]) {
            cus += compilationUnit
          }
        }
      }
      engine.search(pattern, Array(SearchEngine.getDefaultSearchParticipant()), scope, requestor, pm)

      cus.toList.collect{case cu: ICompilationUnit => cu}
    }

  }


}

object ForeignScalaMethodRename extends HasLogger {

  def createChange(method: IJavaElement, project: ScalaProject, pm: IProgressMonitor, newName: String) = {
    val references = findCusOfJavaReferences(method, project, pm)
    logger.debug("method references:")
    references.foreach(ref => logger.debug(s"reference: (class: ${ref.getClass.getCanonicalName}), $ref"))

    def createChangeForAST(root: CompilationUnit, file: IFile) = {
      val rewrite = ASTRewrite.create(root.getAST())
      val newSimpleName = rewrite.getAST().newSimpleName(newName)
      val invocations: ListBuffer[MethodInvocation] = ListBuffer.empty
      root.accept(new ASTVisitor {
        override def visit(methodInvocation: MethodInvocation): Boolean = {
          logger.debug(s"visiting invocation: $methodInvocation, name: ${methodInvocation.getName()}")
          if(methodInvocation.getName().getIdentifier() == method.getElementName()) {
            invocations += methodInvocation
          }
          super.visit(methodInvocation)
        }
      })

      val textEditGroup = new TextEditGroup(root.getJavaElement().getElementName())

      logger.debug("nr invocations: " + invocations.size)

      invocations.foreach{i =>
        logger.debug(s"invocation: ${i}")
        val oldName = i.getName()
        rewrite.replace(oldName, newSimpleName, textEditGroup)
      }

      val textEdit = rewrite.rewriteAST()

      logger.debug(s"text edit: $textEdit")
      val change = new TextFileChange("java change", file)
      change.setEdit(textEdit)
      change
    }

    val changes = references map { cu =>

      val path = cu.getResource().getFullPath().removeFirstSegments(1)

      logger.debug(s"path is: $path")
      val file = project.underlying.getFile(path)
      logger.debug(s"file is: $file, exists: ${file.exists()}")
      val parser = ASTParser.newParser(AST.JLS4);
      parser.setSource(cu);
      val astRoot = parser.createAST(null).asInstanceOf[CompilationUnit];
      createChangeForAST(astRoot, file)
    }

    changes
  }

  private def findCusOfJavaReferences(jElem: IJavaElement, project: ScalaProject, pm: IProgressMonitor): List[ICompilationUnit] = {
    val pattern = SearchPattern.createPattern(jElem, IJavaSearchConstants.REFERENCES)
    val javaProject: IJavaElement = JavaCore.create(project.underlying)
    val scope = SearchEngine.createJavaSearchScope(Array(javaProject))
    val engine = new SearchEngine()
    var cus: ListBuffer[IJavaElement] = ListBuffer.empty
    val requestor = new SearchRequestor {
      override def acceptSearchMatch(m: SearchMatch) {
        logger.debug(s"match resource class: ${m.getResource().getClass().getCanonicalName()}")
        val compilationUnit = JavaCore.create(m.getResource())
        logger.debug(s"found element in cu: ${compilationUnit}")
        logger.debug(s"found element: $m")
        if (m.getElement().isInstanceOf[IJavaElement]) {
          cus += compilationUnit
        }
      }
    }
    engine.search(pattern, Array(SearchEngine.getDefaultSearchParticipant()), scope, requestor, pm)

    cus.toList.collect{case cu: ICompilationUnit => cu}
  }

}