package scala.tools.eclipse
package refactoring

import java.util.regex.Pattern
import org.eclipse.core.resources.IResource
import org.eclipse.core.runtime.IProgressMonitor
import org.eclipse.search.core.text.TextSearchRequestor
import org.eclipse.search.core.text.TextSearchMatchAccess
import org.eclipse.search.core.text.TextSearchEngine
import org.eclipse.search.ui.text.FileTextSearchScope
import scala.tools.eclipse.ScalaProject
import scala.tools.eclipse.javaelements.ScalaSourceFile
import scala.tools.eclipse.logging.HasLogger
import scala.reflect.internal.util.SourceFile
import scala.tools.refactoring.MultiStageRefactoring
import scala.tools.refactoring.analysis.GlobalIndexes
import scala.tools.refactoring.common.InteractiveScalaCompiler
import org.eclipse.search.internal.core.text.PatternConstructor

/**
 * A trait that can be mixed into refactorings that need an index of the whole
 * project (e.g. Global Rename, Move Class).
 *
 * This loads all the files in the project into the presentation compiler, which
 * takes significant time. Once the Scala IDE has its own index, we should be able
 * to make this much more efficient than it currently is.
 */
trait FullProjectIndex extends HasLogger {

  val refactoring: MultiStageRefactoring with InteractiveScalaCompiler with GlobalIndexes

  val project: ScalaProject

  /**
   * A cleanup handler, will later be set by the refactoring
   * to remove all loaded compilation units from the compiler.
   */
  type CleanupHandler = StandaloneProjectIndex.CleanupHandler

  /**
   * Builds an index from all the source files in the current project. The returned
   * CleanupHandler needs to be called when the index isn't used anymore, this will
   * then unload all the originally unloaded files from the presentation compiler.
   *
   * @param hints If present, only files that contain one of these Strings is added
   *              to the index. It uses the JDT SearchEngine to search files.
   */
  def buildFullProjectIndex(pm: IProgressMonitor, hints: List[String]): (refactoring.IndexLookup, CleanupHandler) =
    StandaloneProjectIndex.buildFullProjectIndex(pm, refactoring, project, hints)
}
