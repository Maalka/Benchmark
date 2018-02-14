package controllers

import javax.inject.Inject

import play.api.Configuration
import play.api.libs.ws.{WSClient, WSResponse}
import play.api.mvc.{AbstractController, ControllerComponents}

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}
import scala.concurrent.ExecutionContext.Implicits.global

class RestController @Inject()(ws: WSClient, config: Configuration, cc: ControllerComponents) extends AbstractController(cc) with Logging {

  val url = config.get[String]("pv_system_details.url")

  def makeRequest = Action.async { implicit request =>

    makeWsRequest().map { r =>
        Ok(r.json).as("application/json")
      }
  }

  def makeWsRequest(): Future[WSResponse]  = {

    ws.url(url)
      .addQueryStringParameters(
        ("api_key", config.get[String]("pv_system_details.api_key")),
        ("format", config.get[String]("pv_system_details.format")),
        ("lat", "40"), // required if address or file_id not specified
        ("lon", "-105"), // required if address or file_id not specified
        ("system_capacity", config.get[String]("pv_system_details.system_capacity")),
        ("azimuth", config.get[String]("pv_system_details.azimuth")),
        ("tilt", config.get[String]("pv_system_details.tilt")),
        ("array_type", config.get[String]("pv_system_details.array_type")),
        ("inv_eff", config.get[String]("pv_system_details.inv_eff")),
        ("module_type", config.get[String]("pv_system_details.module_type")),
        ("losses", config.get[String]("pv_system_details.losses"))
        // no such field is API spec
        // ("solar_filed_id", config.get[String]("pv_system_details.solar_filed_id"))
      )
      .get()
  }
}
