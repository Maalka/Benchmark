package BaselineControllerSpec

import controllers.BaselineActions
import org.scalatestplus.play.{ PlaySpec, OneAppPerSuite }
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.libs.json.{JsLookup, Reads, Json, JsValue}
import play.api.mvc.{Result, Controller, Results}
import play.api.test.Helpers._
import play.api.test.{FakeHeaders, Helpers, FakeRequest, FakeApplication}

import scala.concurrent.Future

class BaselineControllerSpec extends PlaySpec with OneAppPerSuite {
  implicit override lazy val app: FakeApplication =
    FakeApplication(
      additionalConfiguration = Map("ehcacheplugin" -> "disabled")
    )

  def loadJsonFile(path: String): JsValue = {
    val js = scala.io.Source.fromInputStream(getClass.getResourceAsStream(path)).mkString
    Json.parse(js)
  }

  def makeFakeRequest(buildingType: String, resource: String): FakeRequest[JsValue] = {
    val js = scala.io.Source.fromInputStream(getClass.getResourceAsStream(resource)).mkString
    FakeRequest(Helpers.POST, controllers.routes.BaselineController.makeBaseline().url,
      FakeHeaders(), Json.parse(js))
  }

  def getBaselineBody(testJson: String):JsValue = {
    val controller = new TestController()
    val fakeRequest = makeFakeRequest("", testJson)
    val result: Future[Result] = controller.makeBaseline().apply(fakeRequest)
    contentAsJson(result)
  }

  // general building response test
  def successfulBaselineTest(testJson: String, expectedJson: String) = {
    val body = getBaselineBody(testJson)
    loadJsonFile(expectedJson) mustEqual body
  }

  // test for a field
  def errorFieldBaselineTest(testJson: String, field: String) = {
    val body = getBaselineBody(testJson)
    (body \ "errors" \\ field) must have size(1)
  }

  // test for a field
  def successFieldBaselineTest(testJson: String, field: String):JsValue = {
    val body = getBaselineBody(testJson)
    val fieldResults = body \ "values" \\ field
    fieldResults must have size(1)
    fieldResults.head
  }

  class TestController() extends Controller with BaselineActions

  "makeBaseline - adult_education post body" should {
    "should be valid" in successfulBaselineTest("/baseline/adult_education/adult_education.json", "/baseline/adult_education/expected/successful_baseline_test.json")
    "ES response be invalid" in errorFieldBaselineTest("/baseline/adult_education/adult_education.json", "ES")
    "totalSiteEnergy response be valid" in {
      successFieldBaselineTest("/baseline/adult_education/adult_education.json", "totalSiteEnergy").as[Double] mustBe 8824366.286854066
    }


  }

  "makeBaseline - empty post body" should {
    "should be invalid" in {
      val controller = new TestController()
      val fakeRequest = FakeRequest(Helpers.POST, controllers.routes.BaselineController.makeBaseline().url,
        FakeHeaders(), Json.parse(""))

      val result: Future[Result] = controller.makeBaseline().apply(fakeRequest)
      val bodyTest = contentAsString(result)
      Console.println(bodyTest)
      bodyTest mustBe "ok" // need to check the return valuse
    }
  }
}
