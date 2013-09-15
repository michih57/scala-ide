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
import org.eclipse.core.resources.IResource

trait SymbolFinder extends HasLogger {

  case class Match(sourceFile: ScalaSourceFile, start: Int, end: Int)

  def find(pm: IProgressMonitor, javaElement: IJavaElement, project: ScalaProject): Option[Match] = {

    logger.debug(s"finding symbol for java element: ${javaElement.getElementName()}")

    val globalIndexesOpt = project.withPresentationCompiler(compiler => {
      val globalIndexes = new GlobalIndexes {
        val global = compiler
        var index = EmptyIndex

        val jSymbol = global.ask { () =>
          javaElement match {
            case iType: IType if iType.isAnnotation() =>
              logger.debug("got an annotation")
              val clazz = global.rootMirror.getClassByName(global.newTypeName(iType.getFullyQualifiedName()))
              logger.debug(s"annotation class: $clazz")
              clazz
            case iType: IType => global.rootMirror.getClassByName(global.newTypeName(iType.getFullyQualifiedName()))
            case iMember: IMember => {
              val declaringType = iMember.getDeclaringType()
              val classSymbol = global.rootMirror.getClassByName(global.newTypeName(declaringType.getFullyQualifiedName()))
              logger.debug(s"members of owning class: ${classSymbol.info.members}")
              val memberName = global.newTermName(iMember.getElementName())
              val classMember = classSymbol.info.member(memberName)
              if(global.NoSymbol != classMember) {
                classMember
              } else {
                val companion = classSymbol.companionModule
                logger.debug(s"members of companion: ${companion.info.declarations}")
                companion.info.member(global.newTermName(iMember.getElementName()))
              }
            }
            // TODO: consider remaining cases
//            case iAnnotation: IAnnotation =>
//              logger.debug(s"java element is IAnnotation: $iAnnotation")
//              ???
//            case iPackageDeclaration: IPackageDeclaration => global.rootMirror.getPackage(global.newTermName(iPackageDeclaration.getElementName()))
//            case iPackageFragment: IPackageFragment => global.rootMirror.getPackage(global.newTermName(iPackageFragment.getElementName()))
            case _ => global.NoSymbol
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
          val occ = occs.headOption
          val position = javaElement match {
            case iType: IType if iType.isAnnotation() => occ.flatMap { o =>
              val annotations = o.symbol.annotations
              annotations.find(a => a.symbol == globalIndexes.jSymbol).map { a =>
                a.pos
              }
            } orElse(occ.map(_.pos))
            case _ => occ.map(t => t.pos)
          }
          val matched = position.flatMap(pos => {
            val start = pos.start
            val end = pos.end
            val path = new Path(pos.source.path)
            val jProject = project.underlying
            val projectName = jProject.getDescription().getName()
            val projectPath = jProject.getLocation
            val relativePath = path.makeRelativeTo(projectPath)
            val hackedPath = new Path(projectName).append(relativePath).makeAbsolute()
            // TODO Mirko: is there a better way to get the ScalaSourceFile?
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