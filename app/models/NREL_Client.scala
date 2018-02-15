package models

import javax.inject._

import play.api.{Configuration, Logger}
import play.api.libs.json.{JsDefined, JsValue, Json}
import play.api.libs.ws.{WSClient, WSResponse}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

@Singleton
class NREL_Client @Inject()(ws: WSClient, config: Configuration) {

  val url = config.get[String]("pv_system_details.url")

  def makeWsRequest(queryParameters: Seq[(String, String)]): Future[JsValue]  = {

    Logger.info("calling WS with params: " + queryParameters)

    ws.url(url)
      .addQueryStringParameters(
        Seq(
          ("api_key", "w3WkKGmAPAji3glLuVK44Qdk2zDbvzbtlZ3SPaGw"),//config.get[String]("pv_system_details.api_key")),
          ("format", "json")//config.get[String]("pv_system_details.format"))
        ) ++ queryParameters: _*
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

