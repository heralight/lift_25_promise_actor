package code.comet




import net.liftweb.common._
import net.liftweb.http._
import S._
import net.liftweb.util._
import Helpers._
import scala.xml._
import net.liftweb.json.JsonAST.{JString, JValue}
import net.liftweb.http.js.{JsCmds, JsExp, JsCmd}
import net.liftweb.http.js.JE.{JsObj, JsRaw}
import net.liftweb.json.{Extraction, DefaultFormats, JsonAST, Printer}
import net.liftweb.http.js.JsCmds._Noop
import net.liftweb.actor.LAScheduler
import code.lib.JqJsCmdsEx

trait PromiseActor extends CometActor with Loggable {
   ca =>

  /**
   * It's the main method to override, to define what is rendered by the CometActor
   *
   * There are implicit conversions for a bunch of stuff to
   * RenderOut (including NodeSeq).  Thus, if you don't declare the return
   * turn to be something other than RenderOut and return something that's
   * coercible into RenderOut, the compiler "does the right thing"(tm) for you.
   * <br/>
   * There are implicit conversions for NodeSeq, so you can return a pile of
   * XML right here.  There's an implicit conversion for NodeSeq => NodeSeq,
   * so you can return a function (e.g., a CssBindFunc) that will convert
   * the defaultHtml to the correct output.  There's an implicit conversion
   * from JsCmd, so you can return a pile of JavaScript that'll be shipped
   * to the browser.<br/>
   * Note that the render method will be called each time a new browser tab
   * is opened to the comet component or the comet component is otherwise
   * accessed during a full page load (this is true if a partialUpdate
   * has occurred.)  You may want to look at the fixedRender method which is
   * only called once and sets up a stable rendering state.
   */
  def render: RenderOut = NodeSeq.Empty



  //override def lifespan = Full(LiftRules.clientActorLifespan.vend.apply(this))

  override def hasOuter = false

  override def parentTag = <div style="display: none"/>

  override def lowPriority: PartialFunction[Any, Unit] = {
    case jsCmd: JsCmd => partialUpdate(JqJsCmdsEx.JsSchedule(JsCmds.JsTry(jsCmd, false)))
    case jsExp: JsExp => partialUpdate(JqJsCmdsEx.JsSchedule(JsCmds.JsTry(jsExp.cmd, false)))

    case ItemMsg(guid, value) =>
      partialUpdate(JqJsCmdsEx.JsSchedule(JsRaw(s"liftAjax.sendEvent(${guid.encJs}, {'success': ${Printer.compact(JsonAST.render(value))}} )").cmd))
    case DoneMsg(guid) =>
      partialUpdate(JqJsCmdsEx.JsSchedule(JsRaw(s"liftAjax.sendEvent(${guid.encJs}, {'done': true} )").cmd))

    case FailMsg(guid, msg) =>
      partialUpdate(JqJsCmdsEx.JsSchedule(JsRaw(s"liftAjax.sendEvent(${guid.encJs}, {'failure': ${msg.encJs} })").cmd))
    case _ =>

  }
  override def sendInitialReq_? = true

  var initialReq : Box[Req] = Empty

  def getReq(): Box[Req] =  {
    logger.info("Get Req: " + initialReq.isEmpty)
    initialReq
  }

  override protected def captureInitialReq(initialReq: Box[Req]) {
    logger.info("Set Req: " + initialReq.isEmpty)
    this.initialReq = initialReq
  }

  val session = S.session.get

  def buildRoundtrip(info: Seq[RoundTripInfo]): JsExp = {
    session.testStatefulFeature{




      implicit val defaultFormats = DefaultFormats

   //   ca ! PerformSetupComet2(Empty)

      //ca ! SetDeltaPruner(lastWhenDeltaPruner)

//      val node: Elem = ca.buildSpan(ca.renderClock, NodeSeq.Empty)
//
//      S.addCometAtEnd(node)

      //val currentReq: Box[Req] = S.request.map(_.snapshot)
      val currentReq: Box[Req] = getReq()

      // val renderVersion = RenderVersion.get

      val jvmanifest: Manifest[JValue] = implicitly

      val map = Map(info.map(i => i.name -> i) :_*)

      def fixIt(in: Any): JValue = {
        in match {
          case jv: JValue => jv
          case a => Extraction.decompose(a)
        }
      }

      def localFunc(in: JValue): JsCmd = {
        LAScheduler.execute(() => {
          session.executeInScope(currentReq, renderVersion)(
            for {
              JString(guid) <- in \ "guid"
              JString(name) <- in \ "name"
              func <- map.get(name)
              payload = in \ "payload"
              reified <- if (func.manifest == jvmanifest) Some(payload) else {
                try {Some(payload.extract(defaultFormats, func.manifest))} catch {
                  case e: Exception =>
                    logger.error("Failed to extract "+payload+" as "+func.manifest, e)
                    ca ! FailMsg(guid, "Failed to extract payload as "+func.manifest+" exception "+ e.getMessage)
                    None

                }
              }
            } {
              func match {
                case StreamRoundTrip(_, func) =>
                  try {
                    for (v <- func.asInstanceOf[Function1[Any, Stream[Any]]](reified)) {
                      v match {
                        case jsCmd: JsCmd => ca ! jsCmd
                        case jsExp: JsExp => ca ! jsExp
                        case v => ca ! ItemMsg(guid,fixIt(v))
                      }
                    }
                    ca ! DoneMsg(guid)
                  } catch {
                    case e: Exception => ca ! FailMsg(guid, e.getMessage)
                  }

                case SimpleRoundTrip(_, func) =>
                  try {
                    func.asInstanceOf[Function1[Any, Any]](reified ) match {
                      case jsCmd: JsCmd => ca ! jsCmd
                      case jsExp: JsExp => ca ! jsExp
                      case v => ca ! ItemMsg(guid, fixIt(v))
                    }
                    ca ! DoneMsg(guid)
                  } catch {
                    case e: Exception => ca ! FailMsg(guid, e.getMessage)
                  }

                case HandledRoundTrip(_, func) =>
                  try {
                    func.asInstanceOf[Function2[Any, RoundTripHandlerFunc, Unit]](reified, new RoundTripHandlerFunc {
                      @volatile private var done_? = false
                      def done() {
                        if (!done_?) {
                          done_? = true
                          ca ! DoneMsg(guid)
                        }
                      }

                      def failure(msg: String) {
                        if (!done_?) {
                          done_? = true
                          ca ! FailMsg(guid, msg)
                        }
                      }


                      /**
                       * Send some JavaScript to execute on the client side
                       * @param value
                       */
                      def send(value: JsCmd): Unit = {
                        if (!done_?) {
                          ca ! value
                        }

                      }


                      /**
                       * Send some javascript to execute on the client side
                       * @param value
                       */
                      def send(value: JsExp): Unit = {
                        if (!done_?) {
                          ca ! value
                        }

                      }

                      def send(value: JValue) {
                        if (!done_?) {
                          ca ! ItemMsg(guid, value)
                        }
                      }
                    })
                  } catch {
                    case e: Exception => ca ! FailMsg(guid, e.getMessage)
                  }

              }
            })
        })

        _Noop
      }


      lazy val theFunc = JsRaw(s"""function(v) {${SHtml.jsonCall(JsRaw("v"), localFunc(_)).toJsCmd}}""")

      lazy val build: (String, JsExp) = "_call_server" -> theFunc

      JsObj(build :: info.map(info => info.name -> JsRaw(
        s"""
          |function(param) {
          |  var promise = new liftAjax.Promise();
          |  liftAjax.associate(promise);
          |  this._call_server({guid: promise.guid, name: ${info.name.encJs}, payload: param});
          |  return promise;
          |}
          |""".stripMargin)).toList :_*)
    }
  }


  private case class ItemMsg(guid: String, item: JValue)
  private case class DoneMsg(guid: String)
  private case class FailMsg(guid: String, msg: String)

}


/**
 * Stuff related to round trip messages
 */
sealed trait RoundTripInfo {
  def name: String
  def manifest: Manifest[_]
}

/**
 * The companion objects. Has tasty implicits
 */
object RoundTripInfo {
  implicit def streamBuilder[T](in: (String, T => Stream[Any]))(implicit m: Manifest[T]): RoundTripInfo =
    StreamRoundTrip(in._1, in._2)(m)

  implicit def simpleBuilder[T](in: (String, T => Any))(implicit m: Manifest[T]): RoundTripInfo =
    SimpleRoundTrip(in._1, in._2)(m)

  implicit def handledBuilder[T](in: (String, (T, RoundTripHandlerFunc) => Unit))(implicit m: Manifest[T]): RoundTripInfo =
    HandledRoundTrip(in._1, in._2)(m)
}

/**
 * A function (well, an interface with a bunch of methods on it) to call
 * depending on the state of the round trip function.
 */
trait RoundTripHandlerFunc {
  /**
   * Send data back to the client. This may be called
   * many times and each time, more data gets sent back to the client.
   * @param value the data to send back.
   */
  def send(value: JValue): Unit

  /**
   * Send some JavaScript to execute on the client side
   * @param value
   */
  def send(value: JsCmd): Unit

  /**
   * Send some javascript to execute on the client side
   * @param value
   */
  def send(value: JsExp): Unit

  /**
   * When you are done sending data back to the client, call this method
   */
  def done(): Unit

  /**
   * If there's a failure related to the computation, call this method.
   * @param msg
   */
  def failure(msg: String): Unit
}

final case class StreamRoundTrip[T](name: String, func: T => Stream[Any])(implicit val manifest: Manifest[T]) extends RoundTripInfo
final case class SimpleRoundTrip[T](name: String, func: T => Any)(implicit val manifest: Manifest[T]) extends RoundTripInfo
final case class HandledRoundTrip[T](name: String, func: (T, RoundTripHandlerFunc) => Unit)(implicit val manifest: Manifest[T]) extends RoundTripInfo