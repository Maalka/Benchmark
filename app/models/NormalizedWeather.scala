package models

/**
 * Created by rimukas on 1/8/16.
 */

import org.joda.time._
import play.api.libs.json._

import scala.language.implicitConversions
import play.api.libs.functional.syntax._

import scala.util._
import scala.util.control.NonFatal
import scala.concurrent.Future



case class NormalizedWeather(parameters: JsValue, entries:CSVcompute) {

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

  //convert date strings to date intervals using normalizeDate in CSVMetrics
  val userInterval:Interval = {
    new Interval(entries.normalizeDate(meter.startDate).get, entries.normalizeDate(meter.endDate).get)
  }

//if row start date and end date are within user submitted start and end, then return
  val filteredTempEnergy = entries.goodEntries.collect {
    case CSVLine(a, b, c, d, e) if (userInterval.contains(a) && userInterval.contains(b)) => (c,d)
  }

  filteredTempEnergy.map(println)




}



case class Building(buildingName: String, postalCode: Int)

object Building {
  implicit val buildingRead: Reads[Building] = Json.reads[Building]
}

case class Meter(meterName: String, startDate: String, endDate: String, frequency:String, ddThreshold:Int,
                 ddType:String, filter:Array[String])

object Meter {
  implicit val meterRead: Reads[Meter] = Json.reads[Meter]
}