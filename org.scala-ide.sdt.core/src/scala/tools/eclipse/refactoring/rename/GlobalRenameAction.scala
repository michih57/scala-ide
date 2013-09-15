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

    var participants: List[ScalaRenameParticipant] = Nil

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

        participants = ScalaRenameParticipantProviders.providers() flatMap {
          p => p.createParticipant(refactoring.global)(selectedSymbol)
        }
      }

      if(pm.isCanceled) {
        status.addWarning("Indexing was cancelled, types will not be renamed.")
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

      val aggregatedStatus = participants.foldLeft(status)((s, participant) => {
        s.merge(participant.checkConditions(name, pm))
        s
      })

      aggregatedStatus
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

      participants.flatMap(p => p.createChange(name, pm)).foreach(compositeChange.add)

      cleanup()

      compositeChange
    }
  }

}