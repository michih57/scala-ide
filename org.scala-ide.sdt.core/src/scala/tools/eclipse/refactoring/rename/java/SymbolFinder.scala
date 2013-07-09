package scala.tools.eclipse.refactoring.rename.java

import scala.tools.eclipse.ScalaProject
import org.eclipse.core.runtime.IProgressMonitor
import org.eclipse.jdt.core.IType
import scala.tools.refactoring.analysis.GlobalIndexes
import scala.tools.eclipse.refactoring.StandaloneProjectIndex
import scala.tools.eclipse.javaelements.ScalaSourceFile
import scala.tools.eclipse.logging.HasLogger
import org.eclipse.core.runtime.Path
import org.eclipse.jdt.core.IJavaElement
import org.eclipse.jdt.core.IMember
import org.eclipse.jdt.core.IPackageDeclaration
import org.eclipse.jdt.core.IAnnotation
import org.eclipse.jdt.core.IPackageFragment

trait SymbolFinder extends HasLogger {

  case class Match(sourceFile: ScalaSourceFile, start: Int, end: Int)
  
  def find(pm: IProgressMonitor, javaElement: IJavaElement, project: ScalaProject): Option[Match] = {
    
    val globalIndexesOpt = project.withPresentationCompiler(compiler => { 
      val globalIndexes = new GlobalIndexes {
        val global = compiler
        var index = EmptyIndex
        
        val jSymbol= global.ask { () =>
          javaElement match {
            case iType: IType => global.rootMirror.getClass(global.newTypeName(iType.getFullyQualifiedName()))
            case iMember: IMember => {
              val declaringType = iMember.getDeclaringType()
              val classSymbol = global.rootMirror.getClass(global.newTypeName(declaringType.getFullyQualifiedName()))
              classSymbol.info.member(global.newTermName(iMember.getElementName()))
            }
            case iAnnotation: IAnnotation => ???
            case iPackageDeclaration: IPackageDeclaration => global.rootMirror.getPackage(global.newTermName(iPackageDeclaration.getElementName()))
            case iPackageFragment: IPackageFragment => global.rootMirror.getPackage(global.newTermName(iPackageFragment.getElementName()))
          }
        }
      }
      Option(globalIndexes)
    })(None)
    
    val result = globalIndexesOpt.flatMap { globalIndexes =>
      val (index, cleanup) = StandaloneProjectIndex.buildFullProjectIndex(pm, globalIndexes, project, List(javaElement.getElementName()))
      project.withPresentationCompiler { compiler =>
        compiler.ask { () =>
          val occs = index.occurences(globalIndexes.jSymbol)
          val position = occs.headOption.map(t => t.pos)
          val matched = position.flatMap(pos => {
            val start = pos.start
            val end = pos.end
            val path = new Path(pos.source.path)
            val jProject = project.underlying
            val projectName = jProject.getDescription().getName()
            val projectPath = jProject.getLocation
            val relativePath = path.makeRelativeTo(projectPath)
            val hackedPath = new Path(projectName).append(relativePath).makeAbsolute()
            val file = ScalaSourceFile.createFromPath(hackedPath.toString())
            file.map(Match(_, start, end))
          })
          matched
        }

      }(None)
    }
    
    result
  }
  
}