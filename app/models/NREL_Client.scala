package models

import javax.inject._

import play.api.Configuration
import play.api.libs.json.{JsDefined, JsValue, Json}
import play.api.libs.ws.{WSClient, WSResponse}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

@Singleton
class NREL_Client @Inject()(ws: WSClient, config: Configuration) {

  val url = config.get[String]("pv_system_details.url")

  def makeWsRequest(): Future[JsValue]  = {

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
      .get().map(_.json)
  }

  def parseResponse(json: JsValue): Either[String, JsValue] = {
    val err: Option[JsValue] = json \ "error" match {
      case e:JsDefined => Some(e.get)
      case _ => None
    }
   err.map { e => Left(e.toString()) }.getOrElse( Right(json))
  }
}
