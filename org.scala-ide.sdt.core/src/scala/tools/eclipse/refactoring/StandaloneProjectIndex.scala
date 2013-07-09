package scala.tools.eclipse.refactoring

import scala.tools.refactoring.analysis.GlobalIndexes
import scala.tools.refactoring.common.InteractiveScalaCompiler
import org.eclipse.core.resources.IResource
import org.eclipse.core.runtime.IProgressMonitor
import org.eclipse.search.core.text.TextSearchEngine
import org.eclipse.search.core.text.TextSearchMatchAccess
import org.eclipse.search.core.text.TextSearchRequestor
import org.eclipse.search.ui.text.FileTextSearchScope
import scala.tools.eclipse.javaelements.ScalaSourceFile
import java.util.regex.Pattern
import scala.reflect.internal.util.SourceFile
import scala.tools.eclipse.ScalaProject

object StandaloneProjectIndex {

  type CleanupHandler = () => Unit  
  
  /**
   * Builds an index from all the source files in the current project. The returned
   * CleanupHandler needs to be called when the index isn't used anymore, this will
   * then unload all the originally unloaded files from the presentation compiler.
   *
   * @param hints If present, only files that contain one of these Strings is added
   *              to the index. It uses the JDT SearchEngine to search files.
   */
  def buildFullProjectIndex(pm: IProgressMonitor, indexes: GlobalIndexes, project: ScalaProject, hints: List[String] = Nil): (indexes.IndexLookup, CleanupHandler) = {

    import indexes.global
    
    def allProjectSourceFiles: Seq[String] = {
      if(hints.isEmpty) {
        project.allSourceFiles map (_.getFullPath.toString) toSeq
      } else {
        val searchInProject = Array[IResource](project.javaProject.getResource)
        val scope = FileTextSearchScope.newSearchScope(searchInProject, Array("*.scala"), false /*ignore derived resources*/)

        var result = List[String]()

        val requestor = new TextSearchRequestor {
          override def acceptPatternMatch(matchAccess: TextSearchMatchAccess) = {
            result ::= matchAccess.getFile.getFullPath.toString
            false // don't report any more matches
          }
        }

        val patternForHints = Pattern.compile(hints.mkString("|"))
        val status = TextSearchEngine.create.search(scope, requestor, patternForHints, pm)

        result
      }
    }

    def collectAllScalaSources(files: Seq[String]): List[SourceFile] = {
      val allScalaSourceFiles = files flatMap { f =>
        if(pm.isCanceled)
          return Nil
        else
          ScalaSourceFile.createFromPath(f)
      } toList

      allScalaSourceFiles map { ssf =>
        if(pm.isCanceled)
          return Nil
        else ssf.withSourceFile { (sourceFile, _) => sourceFile}()
      }
    }

    /**
     * First loads all the source files into the compiler and then starts
     * typeckecking them. The method won't block until typechecking is done
     * but return all the Response objects instead.
     *
     * If the process gets canceled, no more new typechecks will be started.
     */
    def mapAllFilesToResponses(files: List[SourceFile], pm: IProgressMonitor) = {

      pm.subTask("reloading source files")
      val r = new global.Response[Unit]
      global.askReload(files, r)
      r.get

      files flatMap { f =>
        if(pm.isCanceled) {
          None
        } else {
          val r = new global.Response[global.Tree]
          global.askType(f, forceReload = false /*we just loaded the files*/, r)
          Some(r)
        }
      }
    }

    /**
     * Waits until all the typechecking has finished. Every 200 ms, it is checked
     * whether the user has canceled the process.
     */
    def typeCheckAll(responses: List[global.Response[global.Tree]], pm: IProgressMonitor) = {

      def waitForResultOrCancel(r: global.Response[global.Tree]) = {

        var result = None: Option[global.Tree]

        do {
          if (pm.isCanceled) r.cancel()
          else r.get(200) match {
            case Some(Left(data)) if r.isComplete /*no provisional results*/ =>
              result = Some(data)
            case _ => // continue waiting
          }
        } while (!r.isComplete && !r.isCancelled && !pm.isCanceled)

        result
      }

      responses flatMap {
        case r if !pm.isCanceled =>
          waitForResultOrCancel(r)
        case r =>
          None
      }
    }

    pm.beginTask("loading files: ", 3)

    // we need to store the already loaded files so that we don't
    // remove them from the presentation compiler later.
    val previouslyLoadedFiles = global.unitOfFile.values map (_.source) toList

    val files = collectAllScalaSources(allProjectSourceFiles)

    val responses = mapAllFilesToResponses(files, pm)

    pm.subTask("typechecking source files")

    val trees = typeCheckAll(responses, pm)

    // will be called after the refactoring has finished
    val cleanup = { () =>
      (files filterNot previouslyLoadedFiles.contains) foreach {
        f => global.removeUnitOf(f)
      }
    }

    val cus = if(!pm.isCanceled) {

      pm.subTask("creating index")

      trees flatMap { tree =>

          project.withPresentationCompiler { compiler =>
            compiler.askOption { () =>
              indexes.CompilationUnitIndex(tree)
          }
        }()
      }
    } else Nil

    (indexes.GlobalIndex(cus), cleanup)
  }
  
}