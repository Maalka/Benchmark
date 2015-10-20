/*
package models


import play.api.libs.json._
import scala.concurrent.Future
import play.api.cache.Cache
import scala.concurrent.ExecutionContext.Implicits.global
import play.api.Play.current

import scala.concurrent.Future

case class PropertyRest(portfolio: Option[buildingProperties], user: MaalkaUser) {
  def toJson: Future[JsObject] = {
    obj match {
      case Some(o) =>
        for {
          json <- Future {
            Json.obj(
              "profile" -> Json.obj(
                "buildingType" -> o.building_type,
                "buildingCountry" -> o.country,
                "buildingSize" -> o.gross_floor_area_ft2,
                "buildingEnergy" -> o.energy_use,
                "buildingEnergyUnits" -> o.energy_units,
                "buildingEnergyType" -> o.energy_type
              )
            )
          }
        } yield json

      case None =>
        Future.successful(Json.obj())
    }
  }
}*/
