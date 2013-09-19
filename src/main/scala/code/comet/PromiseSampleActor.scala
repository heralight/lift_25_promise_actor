
package code.comet



import net.liftweb.common._
import net.liftweb.http._
import S._
import net.liftweb.util._
import Helpers._
import scala.xml._
import net.liftweb.http.js.JsCmds.{JsCrVar, Script}
import net.liftweb.json._
import net.liftweb.json.JsonAST.JString

class PromiseSampleActor extends PromiseActor {
  // Based on http://blog.goodstuff.im/roundtrip_promises


  // Save the text… we get a JSON blog and manually decode it
  // If an exception is thrown during the save, the client automatically
  // gets a Failure
  def doSave(info: JValue): JValue = {

    for {
      JString(path) <- info \ "path"
      JString(text) <- info \ "text"
    } {
      // save the text
    }
    //JNull // a no-op
    info // a no-op
  }

  // Load the file
  def doLoad(fileName: String): String = {
    // load the named file, turn it into a String and return it
    "Yooo : " + fileName
  }

  override def render = {

    val roundTrip =  buildRoundtrip(List[RoundTripInfo](
      "save" -> doSave _, "load" -> doLoad _))
      "#serverFunc" #> Script(
        JsCrVar("serverFuncs", roundTrip))
  }

}