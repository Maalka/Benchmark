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
    "actualES response be invalid" in errorFieldBaselineTest("/baseline/adult_education/adult_education.json", "actualES")
    "totalSiteEnergy response be valid" in {
      successFieldBaselineTest("/baseline/adult_education/adult_education.json", "totalSiteEnergy").as[Double] mustBe 5611.019929660801
    }
  }

  //check response for generic building type that is missing site energy inputs
  "makeBaseline - adult_education_no_energy post body" should {
    "be valid" in successfulBaselineTest("/baseline/adult_education/adult_education_no_energy.json", "/baseline/adult_education/expected/successful_baseline_no_energy_test.json")
    "siteEUI be invalid" in errorFieldBaselineTest("/baseline/adult_education/adult_education_no_energy.json", "siteEUI")
    "percentBetterSourceEnergy response be valid" in {
      successFieldBaselineTest("/baseline/adult_education/adult_education_no_energy.json", "percentBetterSourceEnergy").as[Double] mustBe 11312000
    }
    "medianSourceEUI response be valid" in {
      successFieldBaselineTest("/baseline/adult_education/adult_education_no_energy.json", "medianSourceEUI").as[Double] mustBe 141.4
    }
    "medianSiteEUI response be valid" in {
      successFieldBaselineTest("/baseline/adult_education/adult_education_no_energy.json", "medianSiteEUI").as[Double] mustBe 83.07482609513066
    }
    "siteEUI response not be valid" in {
      errorValueBaselineTest("/baseline/adult_education/adult_education_no_energy.json", "siteEUI")
    }
  }

  //check response for specific building type with algorithm
  "makeBaseline - office post body" should {
    "be valid" in successfulBaselineTest("/baseline/office/office.json", "/baseline/office/expected/successful_baseline_test.json")
    "totalSiteEnergy response be valid" in {
      successFieldBaselineTest("/baseline/office/office.json", "totalSiteEnergy").as[Double] mustBe 7118295.470290095
    }
  }

  //check response for specific canadian building type with algorithm
  "makeBaseline - canada_k12_school post body" should {
    "be valid" in successfulBaselineTest("/baseline/canada_k12_school/canada_k12_school.json", "/baseline/canada_k12_school/expected/successful_baseline_test.json")
    "ES errors be invalid" in noErrorFieldBaselineTest("/baseline/canada_k12_school/canada_k12_school.json", "ES")
    "targetES errors be invalid" in noErrorFieldBaselineTest("/baseline/canada_k12_school/canada_k12_school.json", "targetES")
    "totalSiteEnergy response be valid" in {
      successFieldBaselineTest("/baseline/canada_k12_school/canada_k12_school.json", "totalSiteEnergy").as[Double] mustBe 7510.199296608001
    }
    "percentBetterSourceEUI response be valid" in {
      successFieldBaselineTest("/baseline/canada_k12_school/canada_k12_school.json", "percentBetterSourceEUI").as[Double] mustBe 0.8771376088057311
    }
    "medianSourceEnergy response be valid" in {
      successFieldBaselineTest("/baseline/canada_k12_school/canada_k12_school.json", "medianSourceEnergy").as[Double] mustBe 30558.281383643694
    }
  }

  //check response for specific building type with algorithm
  "makeBaseline - multifamily post body" should {
    "be valid" in successfulBaselineTest("/baseline/multifamily/multifamily.json", "/baseline/multifamily/expected/successful_baseline_test.json")
    "totalSourceEnergy response be valid" in {
      successFieldBaselineTest("/baseline/multifamily/multifamily.json", "totalSourceEnergy").as[Double] mustBe 18171274.263660695
    }
    "percentBetterSourceEUI response be valid" in {
      successFieldBaselineTest("/baseline/multifamily/multifamily.json", "percentBetterSourceEUI").as[Double] mustBe 105.78107078423999
    }
  }
  //check response for specific building type with algorithm
  "makeBaseline - combo bank hospital adult post body" should {
    "be valid" in successfulBaselineTest("/baseline/combo_bank_hospital_adult/bank_hospital_adult.json", "/baseline/combo_bank_hospital_adult/expected/successful_baseline_bank_hospital_adult.json")
  }

  //check response for specific building type with algorithm
  "makeBaseline - combo hospital bar post body" should {
    "be valid" in successfulBaselineTest("/baseline/combo_hospital_bar/hospital_bar.json", "/baseline/combo_hospital_bar/expected/successful_baseline_hospital_bar.json")
  }

  //check response for specific building type with algorithm
  "makeBaseline - combo hospital bar hotel post body" should {
    "be valid" in successfulBaselineTest("/baseline/combo_hospital_bar_hotel/hospital_bar_hotel.json", "/baseline/combo_hospital_bar_hotel/expected/successful_baseline_hospital_bar_hotel.json")
  }
  //check response for specific building type with algorithm
  "makeBaseline - combo office k12school post body" should {
    "be valid" in successfulBaselineTest("/baseline/combo_office_k12/office_k12.json", "/baseline/combo_office_k12/expected/successful_baseline_office_k12.json")
  }

//check when response comes back empty
  /*"makeBaseline - empty post body" should  {
    "throw[JsonException]" in {
      val controller = new TestController()
      val fakeRequest = FakeRequest(Helpers.POST, controllers.routes.BaselineController.getZEPIMetrics().url,
        FakeHeaders(), Json.parse("null"))

      val result: Future[Result] = controller.getZEPIMetrics().apply(fakeRequest)

      result must not be  "ok"
      //Console.println(bodyTest)
      //bodyTest mustBe "ok" // need to check the return values
    }
  }*/
}
