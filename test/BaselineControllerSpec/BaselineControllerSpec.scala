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
    FakeRequest(Helpers.POST, controllers.routes.BaselineController.getZEPIMetrics().url,
      FakeHeaders(), Json.parse(js))
  }

  def getBaselineBody(testJson: String):JsValue = {
    val controller = new TestController()
    val fakeRequest = makeFakeRequest("", testJson)
    val result: Future[Result] = controller.getZEPIMetrics().apply(fakeRequest)
    contentAsJson(result)
  }

  // general building response test
  def successfulBaselineTest(testJson: String, expectedJson: String) = {
    val body = getBaselineBody(testJson)
    loadJsonFile(expectedJson) mustEqual body
  }

  // test for an error field existing
  def errorFieldBaselineTest(testJson: String, field: String) = {
    val body = getBaselineBody(testJson)
    (body \ "errors" \\ field) must have size(1)
  }

  // test for an error field not existing
    def noErrorFieldBaselineTest(testJson: String, field: String) = {
      val body = getBaselineBody(testJson)
      (body \ "errors" \\ field) must have size (0)
  }

  // test for a value field existing, check against value
  def successFieldBaselineTest(testJson: String, field: String):JsValue = {
    val body = getBaselineBody(testJson)
    val fieldResults = body \ "values" \\ field
    fieldResults must have size(1)
    fieldResults.head
  }

   // test for value field not existing
  def errorValueBaselineTest(testJson: String, field: String) = {
    val body = getBaselineBody(testJson)
    val fieldResults = body \ "values" \\ field
    fieldResults must have size(0)
  }

  class TestController() extends Controller with BaselineActions

  //check response for generic building type that is missing algorithm
  "makeBaseline - adult_education post body" should {
    "be valid" in successfulBaselineTest("/baseline/adult_education/adult_education.json", "/baseline/adult_education/expected/successful_baseline_test.json")
    "totalSiteEnergy response be valid" in {
      successFieldBaselineTest("/baseline/adult_education/adult_education.json", "totalSiteEnergy").as[Double] mustBe 1558616.647128
    }
  }

  //check response for generic building type that is missing site energy inputs
  "makeBaseline - adult_education_no_energy post body" should {
    "be valid" in successfulBaselineTest("/baseline/adult_education/adult_education_no_energy.json", "/baseline/adult_education/expected/successful_baseline_no_energy_test.json")
    "siteEUI be invalid" in errorFieldBaselineTest("/baseline/adult_education/adult_education_no_energy.json", "siteEUI")
    "siteEUI response not be valid" in {
      errorValueBaselineTest("/baseline/adult_education/adult_education_no_energy.json", "siteEUI")
    }
  }

}
