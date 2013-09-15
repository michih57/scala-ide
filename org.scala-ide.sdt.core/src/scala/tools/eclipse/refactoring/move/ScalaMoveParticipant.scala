package scala.tools.eclipse
package refactoring.move

import scala.tools.eclipse.ScalaPlugin
import scala.tools.eclipse.javaelements.ScalaSourceFile
import scala.tools.eclipse.logging.HasLogger
import scala.tools.eclipse.refactoring.ProgressHelpers
import org.eclipse.core.resources.IFile
import org.eclipse.core.resources.IFolder
import org.eclipse.core.runtime.IProgressMonitor
import org.eclipse.jdt.core.IPackageFragmentRoot
import org.eclipse.ltk.core.refactoring.Change
import org.eclipse.ltk.core.refactoring.CompositeChange
import org.eclipse.ltk.core.refactoring.RefactoringStatus
import org.eclipse.ltk.core.refactoring.participants.CheckConditionsContext
import org.eclipse.ltk.core.refactoring.participants.MoveParticipant
import org.eclipse.ui.PlatformUI
import org.eclipse.ltk.core.refactoring.participants.RefactoringArguments
import org.eclipse.ltk.core.refactoring.participants.RefactoringProcessor
import org.eclipse.ltk.core.refactoring.participants.MoveArguments
import org.eclipse.core.runtime.IPath

class ScalaMoveParticipant extends MoveParticipant with HasLogger {

  val getName = "Scala Move Participant"

  private var resourceToMove: IFile = _

  private var change: Change = _

  private var isPartOfRenamePackage: Boolean = false

  protected def initialize(element: Object) = element match {
    case f: IFile =>
      resourceToMove = f
      f.getName.endsWith("scala")
    case _ => false
  }

  override def initialize(processor: RefactoringProcessor, element: Object, args: RefactoringArguments) = {
    isPartOfRenamePackage = processor.isInstanceOf[org.eclipse.jdt.internal.corext.refactoring.rename.RenamePackageProcessor]
    super.initialize(processor, element, args)
  }

  private def pkgNameForPath(path: IPath) = {
    val javaProject = ScalaPlugin.plugin.getJavaProject(resourceToMove.getProject)
    val allRoots = javaProject.getAllPackageFragmentRoots().toList
    val sourceRoots = allRoots.filter(_.getKind() == IPackageFragmentRoot.K_SOURCE)
    val parentRootsOfMovingResource = sourceRoots.filter(_.getPath.isPrefixOf(path))
    val pkgRoot = sourceRoots.headOption
    val rootPath = pkgRoot.map(_.getPath)
    rootPath.map { rp =>
      val nrCommonSegments = rp.matchingFirstSegments(path)
      val newPkgPath = path.removeFirstSegments(nrCommonSegments)
      newPkgPath.segments.mkString(".")
    }
  }

  def checkConditions(pm: IProgressMonitor, context: CheckConditionsContext): RefactoringStatus = {
    getArguments.getDestination match {
      case destination: IFolder =>
        val javaProject = ScalaPlugin.plugin.getJavaProject(resourceToMove.getProject)
        val targetPackage = {
          val existingPkg = javaProject.findPackageFragment(destination.getFullPath)
          if(existingPkg == null) {
            logger.debug(s"faking package fragment for ${destination.getFullPath.toOSString()}")
            val toMovePath = resourceToMove.getFullPath()
            val allRoots = javaProject.getAllPackageFragmentRoots().toList
            val sourceRoots = allRoots.filter(_.getKind() == IPackageFragmentRoot.K_SOURCE)
            val parentRootsOfMovingResource = sourceRoots.filter(_.getPath.isPrefixOf(toMovePath))

            val pkgRoot = sourceRoots.headOption
            logger.debug(s"detected fragment root: ${pkgRoot.map(_.getElementName())}")
            val newPkgName = {
              val destPath = destination.getFullPath()
              val rootPath = pkgRoot.map(_.getPath)
              rootPath.map { rp =>
                val nrCommonSegments = rp.matchingFirstSegments(destPath)
                val newPkgPath = destPath.removeFirstSegments(nrCommonSegments)
                newPkgPath.segments.mkString(".")
              }
            }
            newPkgName.flatMap(name => pkgRoot.map(_.getPackageFragment(name))).getOrElse(null)
          } else {
            existingPkg
          }
        }

        if(targetPackage == null) {
          val msg = s"Could not find the target package for ${destination.getFullPath}. Scala source files will not be refactored."
          return RefactoringStatus.createWarningStatus(msg)
        }

        ScalaSourceFile.createFromPath(resourceToMove.getFullPath.toOSString) map { scalaSourceFile =>

          val moveRefactoring = {
            val action = new MoveClassAction
            new action.MoveClassScalaIdeRefactoring(/*selection is unimportant: */ 0, 0, scalaSourceFile)
          }

          var initialConditions: Option[RefactoringStatus] = None

          def runRefactoring(pm: IProgressMonitor) {
            initialConditions = Some(moveRefactoring.checkInitialConditions(pm))
            moveRefactoring.setMoveSingleImpl(false /*move all classes in the file*/)
            moveRefactoring.target = targetPackage
            if(isPartOfRenamePackage) {
              val pkgToIgnore = pkgNameForPath(resourceToMove.getFullPath.removeLastSegments(1))
              moveRefactoring.ignorePackages = pkgToIgnore.map(List(_)).getOrElse(Nil)
              logger.debug(s"ignored packages: ${moveRefactoring.ignorePackages}")
            }

            if(pm.isCanceled) {
              // when the user cancelled we still want to do the refactoring,
              // but we skip our part. Really? Test! Add warning to the status.
              pm.setCanceled(false)
            } else {
              change = new CompositeChange("Move Scala Class") {
                val changes = moveRefactoring.createRefactoringChanges(pm)._1
                moveRefactoring.scalaChangesToEclipseChanges(changes) foreach add
              }
            }
          }

          val isRunAsEclipseMoveResource = {
            // If there's no active workbench window, then we are run as
            // part of Eclipse's generic Move Resource refactoring.
            PlatformUI.getWorkbench.getActiveWorkbenchWindow == null
          }

          if(isRunAsEclipseMoveResource) {
            runRefactoring(pm)
          } else {
            // The drag-and-drop Move refactoring in JDT is so fast that it doesn't
            // need a cancelable progress monitor, so we run the refactoring in our own.
            ProgressHelpers.runInProgressDialogNonblocking(runRefactoring _)
          }

          moveRefactoring.cleanup()

          new RefactoringStatus {
            initialConditions foreach (_.getEntries foreach addEntry)
          }
        } getOrElse null

      case _ => null
    }
  }

  /**
   * The refactoring needs to be executed before the file is moved, otherwise the
   * underlying IFile changes and the refactoring is applied to the old file.
   */
  override def createPreChange(pm: IProgressMonitor)= change

  def createChange(pm: IProgressMonitor) = null
}
