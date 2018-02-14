import models.NREL_Client
import org.scalatestplus.play.PlaySpec
import org.scalatestplus.play._
import org.scalatest._
import org.scalatestplus.play._
import org.scalatestplus.play.guice.GuiceOneServerPerSuite
import play.api.Configuration
import play.api.libs.json.JsValue
import play.api.test._
import play.api.test.Helpers.{GET => GET_REQUEST, _}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future}
import play.api.mvc._
import play.api.test._
import play.api.test.Helpers._
import play.api.libs.ws.WSClient

class NREL_ClientSpec extends PlaySpec with GuiceOneServerPerSuite {

  "RestController" should {
    "make requests no NREL" in {
      val wsClient = app.injector.instanceOf[WSClient]
      val configuration = app.injector.instanceOf[Configuration]
      val client = new NREL_Client(wsClient, configuration)
      val result: Future[JsValue] = client.makeWsRequest()
//      status(result) must be(OK)

      val f1: Future[JsValue] = result.map { r =>
        Console.println(r)
        r
      }

      Await.result(f1, 5 second)
      //must be("aaa")

    }
  }

}
