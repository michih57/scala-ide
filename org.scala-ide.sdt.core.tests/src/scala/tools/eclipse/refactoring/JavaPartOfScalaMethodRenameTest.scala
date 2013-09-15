package scala.tools.eclipse.refactoring

import scala.tools.eclipse.testsetup.TestProjectSetup
import org.junit.Test
import org.eclipse.core.runtime.NullProgressMonitor
import org.eclipse.core.runtime.Path
import scala.tools.eclipse.util.FileUtils
import org.eclipse.jdt.core.search.SearchPattern
import org.eclipse.jdt.core.search.IJavaSearchConstants
import org.eclipse.jdt.core.search.SearchEngine
import org.eclipse.jdt.core.JavaCore
import org.eclipse.jdt.core.IJavaElement
import org.eclipse.jdt.core.search.SearchRequestor
import org.eclipse.jdt.core.search.SearchMatch
import scala.tools.eclipse.refactoring.rename.JavaParticipantForScalaMethodRename
import org.junit.Assert
import org.eclipse.text.edits.MultiTextEdit
import org.eclipse.ltk.core.refactoring.TextFileChange
import org.eclipse.ltk.core.refactoring.CompositeChange
import org.eclipse.text.edits.InsertEdit
import org.eclipse.text.edits.DeleteEdit

object JavaPartOfScalaMethodRenameTest extends TestProjectSetup("refactoring")

class JavaPartOfScalaMethodRenameTest {

  import JavaPartOfScalaMethodRenameTest._

  @Test
  def textEditsForJavaMethodReference() {
    val scalaClass = scalaCompilationUnit("rename/ScalaClass.scala")
    val method = scalaClass.findPrimaryType().getMethod("method", Array("I"))
    val pm = new NullProgressMonitor
    project.doWithPresentationCompiler { pc =>
      val jRename = new JavaParticipantForScalaMethodRename(pc, method, "foo", method.getJavaProject())
      val change = jRename.createChange(pm)
      val realChange = change.get
      val textFileChanges = change.toList.collect{case cc: CompositeChange => cc}.flatMap(_.getChildren).collect{case tfc: TextFileChange => tfc}
      textFileChanges.foreach{tfc => println(s"tfc: $tfc")}
      val edits = textFileChanges.map{tfc => tfc.getEdit()}
      val childEdits = edits.collect{case mte: MultiTextEdit => mte}.flatMap(_.getChildren)
      val inserts = childEdits.collect{case ie: InsertEdit => ie}
      val deletes = childEdits.collect{case de: DeleteEdit => de}
      Assert.assertEquals(List((141, 0, "foo")), inserts.map(i => (i.getOffset, i.getLength, i.getText)))
      Assert.assertEquals(List((141, 6)), (deletes.map(d => (d.getOffset, d.getLength))))
    }
  }

//  @Test
//  def testJavaSearch() {
//    val javaClass = compilationUnit("rename/JavaClass.java")
//    val method = javaClass.findPrimaryType().getMethod("method", Array("I"))
//    println(s"method: $method")
//    val pattern = SearchPattern.createPattern(method, IJavaSearchConstants.DECLARATIONS)
//    val javaProject: IJavaElement  = JavaCore.create(project.underlying)
//    val scope = SearchEngine.createJavaSearchScope(Array(javaProject))
//    val engine = new SearchEngine
//    val requestor = new SearchRequestor {
//      override def acceptSearchMatch(m: SearchMatch) {
//        println(s"found match: $m")
//      }
//    }
//
//    engine.search(pattern, Array(SearchEngine.getDefaultSearchParticipant()), scope, requestor, new NullProgressMonitor)
//
//
//
//  }

}