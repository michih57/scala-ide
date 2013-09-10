package scala.tools.eclipse.refactoring.rename.java

import org.eclipse.ltk.core.refactoring.participants.RenameParticipant
import org.eclipse.core.runtime.IProgressMonitor
import org.eclipse.ltk.core.refactoring.participants.CheckConditionsContext
import org.eclipse.ltk.core.refactoring.RefactoringStatus
import org.eclipse.ltk.core.refactoring.Change
import org.eclipse.ltk.core.refactoring.NullChange
import org.eclipse.ltk.core.refactoring.TextFileChange
import scala.tools.eclipse.logging.HasLogger
import org.eclipse.ltk.core.refactoring.participants.RefactoringArguments
import org.eclipse.jdt.core.IType
import scala.tools.eclipse.sourcefileprovider.SourceFileProviderRegistry
import scala.tools.eclipse.ScalaPlugin
import scala.tools.eclipse.ScalaPresentationCompiler
import scala.tools.eclipse.refactoring.rename.GlobalRenameAction
import scala.tools.refactoring.common.TextChange
import org.eclipse.ltk.core.refactoring.CompositeChange
import org.eclipse.jdt.core.IJavaElement
import org.eclipse.jdt.core.IMember
import org.eclipse.jdt.core.IAnnotation
import org.eclipse.jdt.core.IPackageDeclaration
import org.eclipse.jdt.core.IPackageFragment
import scala.tools.eclipse.javaelements.ScalaElement

class JavaRenameParticipant extends RenameParticipant with HasLogger with SymbolFinder {

  private var selectedElement: Option[IJavaElement] = None

  private var change: Change = null

  private var refactoring: Option[GlobalRenameAction#RenameScalaIdeRefactoring] = None

  override def initialize(element: Any) = {
    logger.debug(s"initializing JavaRenameParticipant for: ${element.getClass.getName}")

    // TODO: is this clean enough?
    def isScalaElement(element: Any) = element match {
      case scalaElement: ScalaElement => true
      case _ => false
    }

    if(!isScalaElement(element)) {
        element match {
        case iType: IType => {
          logger.debug(s"have an IType: $iType")
          selectedElement = Some(iType)
        }
        case iMember: IMember => {
          logger.debug(s"have an IMember: $iMember")
          selectedElement = Some(iMember)
        }
        case iAnnotation: IAnnotation => {
          logger.debug(s"have an IAnnotation: $iAnnotation")
          selectedElement = Some(iAnnotation)
        }
        case iPackageDeclaration: IPackageDeclaration => {
          logger.debug(s"have an IPackageDeclaration: $iPackageDeclaration")
          selectedElement = Some(iPackageDeclaration)
        }
        case iPackageFragment: IPackageFragment=> {
          logger.debug(s"have an IPackageDeclaration: $iPackageFragment")
          selectedElement = Some(iPackageFragment)
        }
        case _ => logger.debug(s"unknown element: $element")
      }
    }

    logger.debug(s"selected element: $selectedElement")

    true
  }

  def checkConditions(pm: IProgressMonitor, context: CheckConditionsContext): RefactoringStatus = {
    logger.debug("checking conditions in JavaRenameParticipant")
    val scalaMatch = selectedElement.flatMap { javaElement =>
      logger.debug("looking for occurrence of selected type in scala source")
      val scalaProject = ScalaPlugin.plugin.getScalaProject(javaElement.getJavaProject().getProject)
      val matchOpt = find(pm, javaElement, scalaProject)
      if (matchOpt.isEmpty)
        logger.debug("no match found")
      matchOpt.foreach(m => logger.debug(s"found a match in: ${m.sourceFile.file.path}, start <${m.start}>, end <${m.end}>."))
      matchOpt
    }

    refactoring = scalaMatch collect {
      case Match(scalaSourceFile, start, end) =>
        val renameAction = new GlobalRenameAction
        new renameAction.RenameScalaIdeRefactoring(start, end, scalaSourceFile, false)
    }

    logger.debug(s"have a refactoring: $refactoring")

    val args = getArguments()
    logger.debug(s"arguments: $args")

    val statusOpt = refactoring.map { rename =>
      val initialStatus = rename.checkInitialConditions(pm)
      if(initialStatus.isOK()) {
        val finalStatus = rename.checkFinalConditions(pm)
        finalStatus.getEntries().foreach(initialStatus.addEntry)
      }

      initialStatus
    }

    logger.debug(s"status opt: $statusOpt")

    statusOpt.getOrElse(new RefactoringStatus)
  }

  // We need to do the change computation here, because for package renames
  // the resources change and this leads to troubles when the changes are applied to the old resources.
  override def createPreChange(pm: IProgressMonitor): Change = {
    logger.debug("creating JavaRenameParticipant change")

    val result = refactoring map { renameRefactoring =>
      import renameRefactoring._

      if(!pm.isCanceled()) {
        name = getArguments().getNewName()
        val allChanges = performRefactoring()
        allChanges.foreach { c => logger.debug(s"change: ${c}") }

        val changes = allChanges collect {
          case tc: TextChange => tc
        }
        if(!changes.isEmpty) {
          val affectedFiles = changes.map(tc => tc.sourceFile.path).toSet
          logger.debug(s"affectedFiles: $affectedFiles")
          change = new CompositeChange("Update occurrences in Scala code") {
            scalaChangesToEclipseChanges(changes).foreach(add)
          }
        } else {
          logger.debug("no changes generated by refactoring")
        }
      }

      renameRefactoring.cleanup
    }

    change
  }

  def createChange(pm: IProgressMonitor): Change = null

  def getName(): String = "Scala participant for Java Rename refactorings"

}