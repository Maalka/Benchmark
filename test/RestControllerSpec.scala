import controllers.RestController
import org.scalatestplus.play.PlaySpec
import org.scalatestplus.play._
import org.scalatest._
import org.scalatestplus.play._
import org.scalatestplus.play.guice.GuiceOneServerPerSuite
import play.api.Configuration
import play.api.test._
import play.api.test.Helpers.{GET => GET_REQUEST, _}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import play.api.mvc._
import play.api.test._
import play.api.test.Helpers._
import play.api.libs.ws.WSClient

class RestControllerSpec extends PlaySpec with GuiceOneServerPerSuite {

  "RestController" should {
    "make requests no NREL" in {
      val wsClient = app.injector.instanceOf[WSClient]
      val configuration = app.injector.instanceOf[Configuration]
      val controller = new RestController(wsClient, configuration, stubControllerComponents())
      val result: Future[Result] = controller.makeRequest().apply(FakeRequest())
      status(result) must be(OK)
      Console.println(contentAsJson(result)) //must be("aaa")

    }
  }

}
