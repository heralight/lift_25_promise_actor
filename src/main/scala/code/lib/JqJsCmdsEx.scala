package code.lib

import net.liftweb.common._
import net.liftweb.http._
import S._
import net.liftweb.util._
import Helpers._
import scala.xml._
import net.liftweb.http.js.{JsCmd, JsMember, JsExp}

object JqJsCmdsEx {
  /**
   * JsSchedule the execution of the JsCmd using setTimeout()
   * @param what the code to execute
   */
  case class JsSchedule(what: JsCmd) extends JsCmd {
    def toJsCmd = s"""setTimeout(function()
    {
      ${what.toJsCmd}
    } , 0);"""
  }
}
