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
import org.eclipse.jdt.core.search.SearchPattern
import org.eclipse.jdt.core.search.IJavaSearchConstants
import org.eclipse.jdt.core.search.SearchEngine
import org.eclipse.jdt.core.JavaCore
import org.eclipse.jdt.core.IJavaElement
import org.eclipse.core.runtime.NullProgressMonitor
import org.eclipse.jdt.core.search.SearchRequestor
import org.eclipse.jdt.core.search.SearchMatch
import scala.tools.nsc.Global
import org.eclipse.jdt.core.IMethod
import org.eclipse.jdt.ui.refactoring.RenameSupport
import org.eclipse.jdt.core.IType
import org.eclipse.ui.PlatformUI
import org.eclipse.jdt.core.IField

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
    logger.debug("performing rename in scala plugin code")
    val renameAction: Either[RefactoringAction, RenameSupport] = getRenameAction
    renameAction match {
      case Left(scalaAction) => {
        logger.debug("running scala refactoring action")
        scalaAction.run(action)
      }
      case Right(renameSupport) => {
        logger.debug("running java refactoring action")
        renameSupport.openDialog(PlatformUI.getWorkbench().getDisplay().getActiveShell())
      }
    }
  }

  // TODO: get rid of this (used in quickfix.ProposalRefactoringActionAdapter)
  @Deprecated
  def getScalaRenameAction = if (isLocalRename) new LocalRenameAction else new GlobalRenameAction

  /**
   * Using the currently opened file and selection, determines whether the
   * selected SymbolTree is only locally visible or not.
   */
  @Deprecated
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

  // TODO: find some way to sanely deal with selections that can't be renamed (FailAction?)
  private def getRenameAction: Either[RefactoringAction, RenameSupport] = {
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

//        val selectedSymbol = selection.flatMap(_.selectedSymbolTree).map(_.symbol)
        val selectedAnnotation = selection.flatMap(_.selectedAnnotation)
        val selectedSymbol = selection.flatMap { s =>
          s.selectedAnnotation.orElse(s.selectedSymbolTree.map(_.symbol))
        }

        val isJavaSymbol = selectedSymbol.map(s => s.isJava).getOrElse(false)
        logger.info("is java symbol rename: " + isJavaSymbol)

        val javaElement = if(isJavaSymbol) selectedSymbol.flatMap(s => findJavaDeclaration(s, scalaFile)) else None

        logger.debug(s"java element: $javaElement")

        val renameSupport = javaElement collect {
          case method: IMethod => RenameSupport.create(method, method.getElementName(), RenameSupport.UPDATE_REFERENCES)
          case tpe: IType => RenameSupport.create(tpe, tpe.getElementName(), RenameSupport.UPDATE_REFERENCES)
          case field: IField => RenameSupport.create(field, field.getElementName(), RenameSupport.UPDATE_REFERENCES)
        }

        val action: Option[Either[RefactoringAction, RenameSupport]] = (renameSupport.map(Right(_))) orElse {
          val preparationResult = selection.map(refactoring.prepare).flatMap(_.right.toOption)
          val scalaRename = preparationResult map {
            case refactoring.PreparationResult(_, true) => new LocalRenameAction
            case _ => new GlobalRenameAction
          } map (Left(_))
          scalaRename
        }
        action
      }()
    }

    actionOpt.getOrElse(Left(new GlobalRenameAction))
  }

  private def findJavaDeclaration(symbol: Global#Symbol, scalaFile: InteractiveCompilationUnit): Option[IJavaElement] = {

    def patternTarget(): Option[Int] = {
      if(symbol.isClass)
        Some(IJavaSearchConstants.TYPE)
      else if(symbol.isMethod)
        Some(IJavaSearchConstants.METHOD)
      else if(symbol.isVar || symbol.isVal)
        Some(IJavaSearchConstants.FIELD)
      else
        None
    }

    logger.debug(s"symbol is val: ${symbol.isVal}")
    logger.debug(s"symbol is var: ${symbol.isVar}")
    logger.debug(s"symbol is method: ${symbol.isMethod}")

    val fullName = symbol.fullNameString

    val targetKind = patternTarget

    targetKind flatMap { tk =>
      val pattern = SearchPattern.createPattern(fullName, tk, IJavaSearchConstants.DECLARATIONS, SearchPattern.R_EXACT_MATCH)
      val javaProject: IJavaElement = JavaCore.create(scalaFile.workspaceFile.getProject())
      val scope = SearchEngine.createJavaSearchScope(Array(javaProject))
      val engine = new SearchEngine()
      var declaration: Option[IJavaElement] = None
      val requestor = new SearchRequestor {
        override def acceptSearchMatch(m: SearchMatch) {
          logger.debug(s"found element: $m")
          if (m.getElement().isInstanceOf[IJavaElement]) {
            declaration = Some(m.getElement().asInstanceOf[IJavaElement])
          }
        }
      }
      engine.search(pattern, Array(SearchEngine.getDefaultSearchParticipant()), scope, requestor, new NullProgressMonitor) // TODO: use proper progress monitor

      declaration
    }
  }
}
