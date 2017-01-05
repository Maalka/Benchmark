package models

/**
 * Created by rimukas on 10/20/15.
 */

import play.api.libs.json._
import play.api.libs.json.Reads._
import squants.energy.{Gigajoules, KBtus}
import squants.space.{SquareMeters, SquareFeet, Area}
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.language.implicitConversions
import play.api.libs.functional.syntax._
import squants.energy._
import EnergyConversions.EnergyNumeric

import scala.util._
import scala.util.control.NonFatal

case class NormalizedWeather(parameters: JsValue) {

  val building:Building = {
    parameters.asOpt[Building] match {
      case Some(a:Building) => a
      case _ => throw new Exception("Unable to parse building data")
    }
  }
  val meter:Meter = {
    parameters.asOpt[Meter] match {
      case Some(a:Meter) => a
      case _ => throw new Exception("Unable to parse meter data")
    }
  }

  println(building.buildingName)
  meter.filter.map(println)

}



case class Building(buildingName: String, postalCode: Int)

object Building {
  implicit val buildingRead: Reads[Building] = Json.reads[Building]
}

case class Meter(meterName: String, startDate: String, endDate: String, frequency: String, ddThreshold: Int,
                 ddType: String, filter:Array[String])

object Meter {
  implicit val meterRead: Reads[Meter] = Json.reads[Meter]
}