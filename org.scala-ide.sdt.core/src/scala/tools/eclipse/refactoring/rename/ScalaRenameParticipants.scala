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
import org.eclipse.jdt.core.refactoring.CompilationUnitChange

trait ScalaRenameParticipantProvider {

  def createParticipant(global: ScalaPresentationCompiler)(selectedSymbol: global.Symbol, newName: String): Option[ScalaRenameParticipant]

}

trait ScalaRenameParticipant {

  def checkConditions(pm: IProgressMonitor): RefactoringStatus
  def createChange(pm: IProgressMonitor): Option[Change]

}

object ScalaRenameParticipantProviders {
  def providers(): List[ScalaRenameParticipantProvider] = {
    List(JavaParticipantForScalaTypeRename, JavaParticipantForScalaMethodRename)
  }
}

object JavaParticipantForScalaTypeRename extends ScalaRenameParticipantProvider with HasLogger {

  def createParticipant(global: ScalaPresentationCompiler)(selectedSymbol: global.Symbol, newName: String): Option[ScalaRenameParticipant] = {
    if(selectedSymbol.isClass || selectedSymbol.isTrait || selectedSymbol.isModuleClass || selectedSymbol.isModule) {
      // TODO: works only if companion class exists!
      val symbol = if(selectedSymbol.isModule) selectedSymbol.companionClass else selectedSymbol
      val javaType = global.getJavaElement(symbol)
      javaType.map(jt => new JavaParticipantForScalaTypeRename(global, jt, newName))
    } else {
      None
    }
  }

}

class JavaParticipantForScalaTypeRename(global: ScalaPresentationCompiler, javaType: IJavaElement, newName: String) extends ScalaRenameParticipant with HasLogger {

  private val descriptor: Option[RenameJavaElementDescriptor] = {
    val contrib = RefactoringCore.getRefactoringContribution(IJavaRefactorings.RENAME_TYPE)
    if (contrib.createDescriptor().isInstanceOf[RenameJavaElementDescriptor]) {
      val desc: RenameJavaElementDescriptor = contrib.createDescriptor().asInstanceOf[RenameJavaElementDescriptor]
      desc.setJavaElement(javaType)
      desc.setUpdateReferences(true)
      desc.setNewName(newName)
      Some(desc)
    } else {
      None
    }
  }

  private val validationState = descriptor.map(_.validateDescriptor)

  val wrappedRefactoring: Option[Refactoring] = (descriptor, validationState) match {
    case (Some(desc), Some(vs)) =>
      Some(desc.createRefactoring(vs))
    case _ => None
  }

  def checkConditions(pm: IProgressMonitor): RefactoringStatus = (wrappedRefactoring, validationState) match {
    case (Some(ref), Some(vs)) =>
      vs.merge(ref.checkInitialConditions(pm))
      if(vs.isOK()) {
        vs.merge(ref.checkFinalConditions(pm))
      }
      vs
    case _ =>
      val errorState = new RefactoringStatus
      errorState.addError("Failed to create Java participant for Scala Type Rename")
      errorState
  }

  def createChange(pm: IProgressMonitor): Option[Change] = {
    def isNotPureJavaChange(c: Change) = c match {
      case _: RenameCompilationUnitChange => true
      case cuc: CompilationUnitChange =>
        logger.debug(s"file extension: ${cuc.getFile().getFileExtension}")
        cuc.getFile().getFileExtension() != "java"
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

    wrappedRefactoring flatMap { r =>
      logger.debug("creating change in java participant of scala rename")
      val change = r.createChange(pm)
      debugChange(change)
      val pureJavaChanges = filterPureJavaChanges(change)
      debugChange(change)

      pureJavaChanges match {
        case cc: CompositeChange if cc.getChildren.isEmpty => None
        case _ => Some(new CompositeChange("java participant") {
          add(pureJavaChanges)
        })
      }


    }
  }

}

object JavaParticipantForScalaMethodRename extends ScalaRenameParticipantProvider {
  def createParticipant(global: ScalaPresentationCompiler)(selectedSymbol: global.Symbol, newName: String): Option[ScalaRenameParticipant] = {
    if(selectedSymbol.isMethod) {
      val javaElement = global.getJavaElement(selectedSymbol)
      javaElement.map(je => new JavaParticipantForScalaMethodRename(global, je, newName, je.getJavaProject()))
    } else {
      None
    }
  }
}

class JavaParticipantForScalaMethodRename(global: ScalaPresentationCompiler, method: IJavaElement, newName: String, project: IJavaProject) extends ScalaRenameParticipant with HasLogger {

  def checkConditions(pm: IProgressMonitor): RefactoringStatus = {
    new RefactoringStatus
  }

  def createChange(pm: IProgressMonitor): Option[Change] = {
    val references = findCusOfJavaReferences(method, project, pm)

    def createChangeForAST(root: CompilationUnit, file: IFile) = {
      val rewrite = ASTRewrite.create(root.getAST())
      val newSimpleName = rewrite.getAST().newSimpleName(newName)
      val invocations: ListBuffer[MethodInvocation] = ListBuffer.empty
      root.accept(new ASTVisitor {
        override def visit(methodInvocation: MethodInvocation): Boolean = {
          if(methodInvocation.getName().getIdentifier() == method.getElementName()) {
            invocations += methodInvocation
          }
          super.visit(methodInvocation)
        }
      })

      val textEditGroup = new TextEditGroup(root.getJavaElement().getElementName())

      invocations.foreach{i =>
        val oldName = i.getName()
        rewrite.replace(oldName, newSimpleName, textEditGroup)
      }

      val textEdit = rewrite.rewriteAST()

      val change = new TextFileChange("java change", file)
      change.setEdit(textEdit)
      change
    }

    val changes = references map { cu =>

      val path = cu.getResource().getFullPath().removeFirstSegments(1)

      val file = project.getProject.getFile(path)
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
    val pattern = SearchPattern.createPattern(jElem, IJavaSearchConstants.ALL_OCCURRENCES)
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
  }
}