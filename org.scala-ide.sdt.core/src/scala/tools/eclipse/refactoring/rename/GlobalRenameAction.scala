/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.eclipse
package refactoring
package rename

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
import org.eclipse.core.runtime.NullProgressMonitor
import org.eclipse.jdt.core.IJavaElement
import org.eclipse.jdt.core.JavaCore
import org.eclipse.jdt.core.refactoring.IJavaRefactorings
import org.eclipse.jdt.core.refactoring.descriptors.RenameJavaElementDescriptor
import org.eclipse.jdt.core.search.IJavaSearchConstants
import org.eclipse.jdt.core.search.SearchEngine
import org.eclipse.jdt.core.search.SearchMatch
import org.eclipse.jdt.core.search.SearchPattern
import org.eclipse.jdt.core.search.SearchRequestor
import org.eclipse.jdt.ui.refactoring.RenameSupport
import org.eclipse.ltk.core.refactoring.CompositeChange
import org.eclipse.ltk.core.refactoring.NullChange
import org.eclipse.ltk.core.refactoring.RefactoringCore
import org.eclipse.ltk.core.refactoring.RefactoringStatus
import org.eclipse.ltk.core.refactoring.resource.RenameResourceChange

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
  class RenameScalaIdeRefactoring(start: Int, end: Int, file: ScalaSourceFile)
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

    val javaParticipant = new ScalaRenameParticipant(this)

    /**
     * A cleanup handler, will later be set by the refactoring
     * to remove all loaded compilation units from the compiler.
     */
    var cleanup = () => ()

    /* (non-Javadoc)
     * @see scala.tools.eclipse.refactoring.ScalaIdeRefactoring#checkInitialConditions(org.eclipse.core.runtime.IProgressMonitor)
     */
    override def checkInitialConditions(pm: IProgressMonitor): RefactoringStatus = {

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

//      if(!status.hasError()){
//        status.merge(javaParticipant.checkInitialConditions(pm))
//      }

      status
    }

    override def checkFinalConditions(pm: IProgressMonitor): RefactoringStatus = {
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

      if(!status.hasError()) {
        status.merge(javaParticipant.checkFinalConditions(pm))
      }

      status
    }

    override def getPages = new NewNameWizardPage((s => name = s), refactoring.isValidIdentifier, name, "refactoring_rename") :: Nil

    override def createChange(pm: IProgressMonitor) = {
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

      val participantChange = javaParticipant.createChange(pm)

      compositeChange.add(participantChange)

      cleanup()

      compositeChange
    }
  }

  // FIXME work in progress...
  class ScalaRenameParticipant(scalaRename: RenameScalaIdeRefactoring) extends HasLogger {

    var renamer: Option[RenameJavaElementDescriptor] = None

    def checkInitialConditions(pm: IProgressMonitor): RefactoringStatus = {
      val selection = scalaRename.selection()
      val selectedSymbol = scalaRename.preparationResult().right.get.renameProcessor.selectedSymbol

      val noSym = scalaRename.refactoring.global.NoSymbol

      val jElem = scalaRename.refactoring.global.ask{() =>
        scalaRename.refactoring.global.getJavaElement(selectedSymbol)
      }

      val jOcc = jElem map { j =>
        findJavaOccurrence(j, scalaRename.project)
      }

      logger.debug("java element: " + jElem)
      logger.debug("occ: " + jOcc)

      val rsFlag = RenameSupport.UPDATE_REFERENCES

      // not sure about the right way to initiate the java side of this rename
      // currently not working (NPE)
      renamer = jElem flatMap { j =>
        val id = IJavaRefactorings.RENAME_TYPE
        val contrib = RefactoringCore.getRefactoringContribution(id)
        if(contrib.createDescriptor().isInstanceOf[RenameJavaElementDescriptor]) {
          val desc: RenameJavaElementDescriptor = contrib.createDescriptor().asInstanceOf[RenameJavaElementDescriptor]
          desc.setJavaElement(j)
          desc.setNewName("NewName")
          desc.setUpdateReferences(true)
          Some(desc)
        } else {
          None
        }
      }

      new RefactoringStatus()
    }
    def checkFinalConditions(pm: IProgressMonitor): RefactoringStatus = {
      new RefactoringStatus
    }
    def createChange(pm: IProgressMonitor): CompositeChange = {
      new CompositeChange("java participant") {
        renamer.foreach { r =>
          logger.debug("creating change in java participant of scala rename")
          val ref = r.createRefactoring(new RefactoringStatus)
          val change = ref.createChange(pm)
          add(change)
        }
      }
    }

    private def findJavaOccurrence(jElem: IJavaElement, project: ScalaProject): Option[IJavaElement] = {

      val pattern = SearchPattern.createPattern(jElem, IJavaSearchConstants.REFERENCES)
      val javaProject: IJavaElement = JavaCore.create(project.underlying)
      val scope = SearchEngine.createJavaSearchScope(Array(javaProject))
      val engine = new SearchEngine()
      var reference: Option[IJavaElement] = None
      val requestor = new SearchRequestor {
        override def acceptSearchMatch(m: SearchMatch) {
          logger.debug(s"found element: $m")
          if (m.getElement().isInstanceOf[IJavaElement]) {
            reference = Some(m.getElement().asInstanceOf[IJavaElement])
          }
        }
      }
      engine.search(pattern, Array(SearchEngine.getDefaultSearchParticipant()), scope, requestor, new NullProgressMonitor) // TODO: use proper progress monitor

      reference
    }

  }


}

