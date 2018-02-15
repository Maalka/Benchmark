import models.NREL_Client
import org.scalatestplus.play.PlaySpec
import org.scalatestplus.play._
import org.scalatest._
import org.scalatestplus.play._
import org.scalatestplus.play.guice.GuiceOneServerPerSuite
import play.api.Configuration
import play.api.libs.json.{JsValue, Json}
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

    val wsClient = app.injector.instanceOf[WSClient]
    val configuration = app.injector.instanceOf[Configuration]
    val client = new NREL_Client(wsClient, configuration)

    "make requests no NREL" in {

      val result: Future[JsValue] = client.makeWsRequest()
      val f1: Future[JsValue] = result.map { r =>
        Console.println(r)
        r
      }

      Await.result(f1, 5 second)

    }

    "parse error response" in {
      val jsonErrorResponse = Json.parse("{\"error\":{\"code\":\"API_KEY_INVALID\",\"message\":\"An invalid api_key was supplied. Get one at https://developer.nrel.gov/signup/\"}}")
      assert(client.parseResponse(jsonErrorResponse) == Left("{\"code\":\"API_KEY_INVALID\",\"message\":\"An invalid api_key was supplied. Get one at https://developer.nrel.gov/signup/\"}"))
    }

    "parse success rsponse" in {
      val jsonSuccessResponse = Json.parse(
        """
          |{
          |  "inputs": {
          |    "format": "json",
          |    "system_capacity": "147418.77408607837",
          |    "array_type": "0",
          |    "tilt": "20",
          |    "inv_eff": "90",
          |    "lon": "-105",
          |    "module_type": "0",
          |    "losses": "0.2",
          |    "lat": "40",
          |    "azimuth": "45"
          |  },
          |  "errors": [],
          |  "warnings": [],
          |  "version": "1.3.0",
          |  "ssc_info": {
          |    "version": 45,
          |    "build": "Linux 64 bit GNU/C++ Jul  7 2015 14:24:09"
          |  },
          |  "station_info": {
          |    "lat": 40.016666412353516,
          |    "lon": -105.25,
          |    "elev": 1634,
          |    "tz": -7,
          |    "location": "94018",
          |    "city": "BOULDER",
          |    "state": "CO",
          |    "solar_resource_file": "94018.tm2",
          |    "distance": 21235
          |  },
          |  "outputs": {
          |    "ac_monthly": [
          |      5793937.5,
          |      8088598.5,
          |      15576799,
          |      19900610,
          |      23480250,
          |      24282220,
          |      23838736,
          |      20820408,
          |      15682576,
          |      10872192,
          |      6330860,
          |      5058957.5
          |    ],
          |    "poa_monthly": [
          |      47.213233947753906,
          |      63.59735107421875,
          |      119.50402069091797,
          |      154.41964721679688,
          |      186.1233367919922,
          |      197.4545440673828,
          |      198.12228393554688,
          |      172.67459106445312,
          |      128.7928466796875,
          |      88.92039489746094,
          |      51.91674041748047,
          |      41.725399017333984
          |    ],
          |    "solrad_monthly": [
          |      1.5230075120925903,
          |      2.271333932876587,
          |      3.854968309402466,
          |      5.147321701049805,
          |      6.003978729248047,
          |      6.581818103790283,
          |      6.391041278839111,
          |      5.57014799118042,
          |      4.293095111846924,
          |      2.8683998584747314,
          |      1.7305580377578735,
          |      1.3459806442260742
          |    ],
          |    "dc_monthly": [
          |      6595704.5,
          |      9108044,
          |      17391508,
          |      22180134,
          |      26157594,
          |      27055020,
          |      26560844,
          |      23197480,
          |      17515152,
          |      12200231,
          |      7186789,
          |      5788353
          |    ],
          |    "ac_annual": 179726160,
          |    "solrad_annual": 3.965137481689453,
          |    "capacity_factor": 13.917281150817871
          |  }
          |}
        """.stripMargin
      )
      assert(client.parseResponse(jsonSuccessResponse) == Right(jsonSuccessResponse))
    }
  }

}
