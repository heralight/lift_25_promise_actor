
package code.snippet

import net.liftweb.common._
import net.liftweb.http._
import S._
import net.liftweb.util._
import Helpers._
import scala.xml._
import net.liftweb.http.js.JsCmds.{JsCrVar, Script}
import net.liftweb.json._
import net.liftweb.json.JsonAST.JString
import code.comet.{PromiseSnippet, RoundTripInfo, PromiseActor}

class PromiseSampleSnippet extends PromiseSnippet {

  override def dispatch = {
    case _ => render
    //    case "bid" => bid
  }

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

  // Load the file
  def doLoad2(fileName: String): String = {
   var s = ""

    for {
      req <-  S.request
      _ <- Option(req.request)
      if LiftRules.getLiftSession(req).running_?
    } {
      //  logger.info("get GOOD REQ")
      S.getRequestHeader("X-Real_IP") openOr (S.getRequestHeader("X-Forwarded_For") openOr ("NoIP"))
      s = "S.request get With No problem."
    }
    if (s == "")
      s = "S.request get null => problem"
    // load the named file, turn it into a String and return it
    "Yooo : " + s
  }


  def render = {

    val roundTrip =  buildRoundtrip(List[RoundTripInfo](
      "save" -> doSave _, "load" -> doLoad _,  "load2" -> doLoad2 _))
    "#serverFunc" #> Script(
      JsCrVar("serverFuncs", roundTrip))
  }

}