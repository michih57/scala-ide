/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.eclipse
package refactoring
package rename

import scala.tools.eclipse.refactoring.ActionAdapter
import scala.tools.eclipse.refactoring.EditorHelpers
import scala.tools.refactoring.analysis.GlobalIndexes
import scala.tools.refactoring.implementations.Rename
import org.eclipse.jface.action.IAction
import scala.tools.eclipse.logging.HasLogger
import org.eclipse.jdt.ui.actions.{RenameAction => JRenameAction}
import org.eclipse.jface.viewers.IStructuredSelection
import org.eclipse.jface.text.ITextSelection
import org.eclipse.jface.viewers.StructuredSelection

/**
 * This implementation supports renaming of all identifiers that occur in the program.
 * For example, local values and variables, method definitions and parameters, class
 * fields, variable bindings in pattern matches, classes, objects, traits, and types parameters.
 *
 * Two different modes are available: inline renaming and a wizard based implementation.
 *
 * Inline renaming is automatically chosen if the identifier that is renamed has only a local scope.
 * For example, a local variable. All names that can potentially be accessed from other compilation
 * units in the program are renamed with the wizard and show a preview of the changes.
 *
 * The actual renaming is done in LocalRenameAction and GlobalRenameAction.
 */
class RenameAction extends ActionAdapter with HasLogger {

  override def run(action: IAction) {
    val renameAction: Either[RefactoringAction, (JRenameAction, ITextSelection)] = getRenameAction
    renameAction match {
      case Left(scalaAction) => scalaAction.run(action)
      case Right((javaAction, selection)) => javaAction.run(selection)
    }
  }

  // TODO: get rid of this (used in quickfix.ProposalRefactoringActionAdapter) 
  @Deprecated
  def getScalaRenameAction = if (isLocalRename) new LocalRenameAction else new GlobalRenameAction

  /**
   * Using the currently opened file and selection, determines whether the
   * selected SymbolTree is only locally visible or not.
   */
  private def isLocalRename: Boolean = {

    val isLocalRename = EditorHelpers.withScalaFileAndSelection { (scalaFile, selected) =>
      scalaFile.withSourceFile{(file, compiler) =>
        val refactoring = new Rename with GlobalIndexes {
          val global = compiler
          val index = EmptyIndex
        }

        val selection = refactoring.askLoadedAndTypedTreeForFile(file).left.toOption map { tree =>
          val start = selected.getOffset
          val end = start + selected.getLength
          new refactoring.FileSelection(file.file, tree, start, end)
        }
        
        // TODO: return java refactoring action if is java symbol
        val selectedSymbol = selection.flatMap(_.selectedSymbolTree).map(_.symbol)
        val isJavaSymbol = selectedSymbol.map(_.isJava)
        logger.info("is java symbol rename: " + isJavaSymbol)
        
        

        selection map refactoring.prepare flatMap (_.right.toOption) map {
          case refactoring.PreparationResult(_, isLocal) => isLocal
          case _ => false
        }
      }()
    } getOrElse false

    isLocalRename
  }
  
  //private def getJavaEditorForSelection(selection: ITextSelection): J
  
  private def getRenameAction: Either[RefactoringAction, (JRenameAction, ITextSelection)] = {
    val actionOpt = EditorHelpers.withScalaFileAndSelection { (scalaFile, selected) =>
      scalaFile.withSourceFile{ (file, compiler) =>
        val refactoring = new Rename with GlobalIndexes {
          val global = compiler
          val index = EmptyIndex
        }

        val selection = refactoring.askLoadedAndTypedTreeForFile(file).left.toOption map { tree =>
          val start = selected.getOffset
          val end = start + selected.getLength
          new refactoring.FileSelection(file.file, tree, start, end)
        }
        
        val isJavaSymbol = selection.flatMap(_.selectedSymbolTree).map(_.symbol.isJava)
        logger.info("is java symbol rename: " + isJavaSymbol)
        
        // TODO: need to search for occurrence in JavaCode -> get either an IStructuredSelection or a JavaEditor!
        val editor = isJavaSymbol.flatMap(_ => EditorHelpers.withCurrentEditor(e => Some(e.getEditorSite())))
        val jRename = editor.map(e => (new JRenameAction(e), selected)).map(Right(_))
        val action = jRename orElse {
          val preparationResult = selection.map(refactoring.prepare).flatMap(_.right.toOption)
          val scalaRename = preparationResult map {
            case refactoring.PreparationResult(_, isLocal) => Left(new LocalRenameAction)
            case _ => Left(new GlobalRenameAction)
          }
          scalaRename
        }
        action
      }()
    }
    
    actionOpt.getOrElse(Left(new GlobalRenameAction))
  }
}
