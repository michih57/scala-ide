package scala.tools.eclipse.refactoring.rename

import scala.tools.eclipse.refactoring.ScalaIdeRefactoring
import org.eclipse.core.runtime.IProgressMonitor
import org.eclipse.ltk.core.refactoring.RefactoringStatus
import org.eclipse.ltk.core.refactoring.Change
import scala.tools.nsc.Global
import scala.tools.eclipse.ScalaPresentationCompiler
import org.eclipse.jdt.core.IJavaElement
import org.eclipse.ltk.core.refactoring.NullChange
import org.eclipse.ltk.core.refactoring.RefactoringCore
import org.eclipse.jdt.core.refactoring.descriptors.RenameJavaElementDescriptor
import org.eclipse.jdt.core.refactoring.IJavaRefactorings
import org.eclipse.ltk.core.refactoring.Refactoring
import scala.tools.eclipse.logging.HasLogger
import org.eclipse.jdt.internal.corext.refactoring.changes.RenameCompilationUnitChange
import org.eclipse.ltk.core.refactoring.CompositeChange
import org.eclipse.jdt.core.search.SearchRequestor
import org.eclipse.jdt.core.search.SearchMatch
import scala.tools.eclipse.ScalaProject
import org.eclipse.jdt.core.search.SearchEngine
import org.eclipse.jdt.core.JavaCore
import org.eclipse.jdt.core.ICompilationUnit
import scala.collection.mutable.ListBuffer
import org.eclipse.jdt.core.search.SearchPattern
import org.eclipse.jdt.core.search.IJavaSearchConstants
import org.eclipse.jdt.core.dom.MethodInvocation
import org.eclipse.text.edits.TextEditGroup
import org.eclipse.core.resources.IFile
import org.eclipse.jdt.core.dom.rewrite.ASTRewrite
import org.eclipse.ltk.core.refactoring.TextFileChange
import org.eclipse.jdt.core.dom.CompilationUnit
import org.eclipse.jdt.core.dom.ASTVisitor
import org.eclipse.jdt.core.dom.ASTParser
import org.eclipse.jdt.core.dom.AST
import org.eclipse.jdt.core.IJavaProject
import org.eclipse.jdt.ui.search.ElementQuerySpecification
import org.eclipse.jdt.internal.ui.search.JavaSearchQuery
import org.eclipse.jdt.internal.ui.search.SearchParticipantsExtensionPoint
import org.eclipse.jdt.ui.search.ISearchRequestor
import org.eclipse.search.ui.text.Match

trait ScalaRenameParticipantProvider {

  def createParticipant(global: ScalaPresentationCompiler)(selectedSymbol: global.Symbol): Option[ScalaRenameParticipant]

}

trait ScalaRenameParticipant {

  def checkConditions(newName: String, pm: IProgressMonitor): RefactoringStatus
  def createChange(newName: String, pm: IProgressMonitor): Option[Change]

}

object ScalaRenameParticipantProviders {
  def providers(): List[ScalaRenameParticipantProvider] = {
    List(JavaParticipantForScalaTypeRename, JavaParticipantForScalaMethodRename)
  }
}

object JavaParticipantForScalaTypeRename extends ScalaRenameParticipantProvider {

  def createParticipant(global: ScalaPresentationCompiler)(selectedSymbol: global.Symbol): Option[ScalaRenameParticipant] = {
    if(selectedSymbol.isClass || selectedSymbol.isTrait || selectedSymbol.isModuleClass || selectedSymbol.isModule) {
      val javaType = global.getJavaElement(selectedSymbol)
      javaType.map(jt => new JavaParticipantForScalaTypeRename(global, jt))
    } else {
      None
    }
  }

}

class JavaParticipantForScalaTypeRename(global: ScalaPresentationCompiler, javaType: IJavaElement) extends ScalaRenameParticipant with HasLogger {

  val renamer: Option[RenameJavaElementDescriptor] = {
    val contrib = RefactoringCore.getRefactoringContribution(IJavaRefactorings.RENAME_TYPE)
    if (contrib.createDescriptor().isInstanceOf[RenameJavaElementDescriptor]) {
      val desc: RenameJavaElementDescriptor = contrib.createDescriptor().asInstanceOf[RenameJavaElementDescriptor]
      desc.setJavaElement(javaType)
      desc.setUpdateReferences(true)
      Some(desc)
    } else {
      None
    }
  }

  var wrappedRefactoring: Option[Refactoring] = None

  def checkConditions(newName: String, pm: IProgressMonitor): RefactoringStatus = {
    logger.debug("checking conditions in TypeRenamer")
    val initialConditionsState = renamer map { r =>
      r.setNewName(newName)
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

  def createChange(newName: String, pm: IProgressMonitor): Option[Change] = {
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

    wrappedRefactoring map { r =>
      logger.debug("creating change in java participant of scala rename")
      val change = r.createChange(pm)
      debugChange(change)
      val pureJavaChanges = filterPureJavaChanges(change)
      debugChange(change)
      new CompositeChange("java participant") {
        add(pureJavaChanges)
      }
    }
  }

}

object JavaParticipantForScalaMethodRename extends ScalaRenameParticipantProvider {
  def createParticipant(global: ScalaPresentationCompiler)(selectedSymbol: global.Symbol): Option[ScalaRenameParticipant] = {
    if(selectedSymbol.isMethod) {
      val javaElement = global.getJavaElement(selectedSymbol)
      javaElement.map(je => new JavaParticipantForScalaMethodRename(global, je, je.getJavaProject()))
    } else {
      None
    }
  }
}

class JavaParticipantForScalaMethodRename(global: ScalaPresentationCompiler, method: IJavaElement, project: IJavaProject) extends ScalaRenameParticipant with HasLogger {

  def checkConditions(newName: String, pm: IProgressMonitor): RefactoringStatus = {
    logger.debug("checking conditions in JavaParticipantForScalaMethodRename")
    new RefactoringStatus
  }

  def createChange(newName: String, pm: IProgressMonitor): Option[Change] = {
    val references = findCusOfJavaReferences(method, project, pm)
    logger.debug("cus containing method references:")
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
      val file = project.getProject.getFile(path)
      logger.debug(s"file is: $file, exists: ${file.exists()}")
      val parser = ASTParser.newParser(AST.JLS4);
      parser.setSource(cu);
      val astRoot = parser.createAST(null).asInstanceOf[CompilationUnit];
      createChangeForAST(astRoot, file)
    }

    changes match {
      case Nil => None
      case _ =>
        val compositeChange = new CompositeChange("java participant") {
          changes.foreach(add)
        }
        Some(compositeChange)
    }
  }

  private def findCusOfJavaReferences(jElem: IJavaElement, project: IJavaProject, pm: IProgressMonitor): List[ICompilationUnit] = {
    val pattern = SearchPattern.createPattern(jElem, IJavaSearchConstants.REFERENCES)
    val javaProject: IJavaElement = JavaCore.create(project.getProject)
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

    val participantRequestor = new ISearchRequestor {
      override def reportMatch(m: Match) {
        logger.debug(s"found participant match: ${m}, element: ${m.getElement}")
      }
    }
    val querySpecification = new ElementQuerySpecification(jElem, IJavaSearchConstants.REFERENCES, scope, "workspace scope description -> HAE?")
    engine.search(pattern, Array(SearchEngine.getDefaultSearchParticipant()), scope, requestor, pm)
    val searchParticipantRecords = SearchParticipantsExtensionPoint.getInstance().getSearchParticipants(Array(project.getProject))
    for(searchParticipantRecord <- searchParticipantRecords) {
      logger.debug("doing participant search")
      val participant = searchParticipantRecord.getParticipant()
      participant.search(participantRequestor, querySpecification, pm)
    }

    cus.toList.collect{case cu: ICompilationUnit => cu}

//    val querySpecification = new ElementQuerySpecification(jElem, IJavaSearchConstants.REFERENCES, scope, "workspace scope description -> HAE?")
//    val query = new JavaSearchQuery(querySpecification)
//    logger.debug(s"query run result: ${query.run(pm)}")
//    val searchResult = query.getSearchResult()
//
//
//    Nil
  }
}