package models

import java.io.InputStream

import play.api.Play
import play.api.libs.json._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import play.api.libs.functional.syntax._

/**
 * Created by rimukas on 7/5/16.
 */
object DegreeDays {
  lazy val zipStationLookupTable = loadLookupTable("zip_station.json")
  lazy val cddStationLookupTable = loadLookupTable("station_cdd.json")
  lazy val hddStationLookupTable = loadLookupTable("station_hdd.json")

  def loadLookupTable(filename:String): Future[JsValue] = {
    for {
      is <- Future(Play.current.resourceAsStream(filename))
      json <- Future {
        is match {
          case Some(is: InputStream) => {
            Json.parse(is)
          }
          case _ => throw new Exception("Could not open file")
        }
      }
    } yield json
  }

}

case class DegreeDays(parameters:JsValue) {
  import DegreeDays._

  def lookupWeatherStation: Future[String] = {
    for {
      zipCode <- getZipCode
      zipTable <- zipStationLookupTable
      zipStation <-
      Future {
        (zipTable \ zipCode \ "station").toOption match {
          case Some(a) => a.as[String]
          case _ => throw new Exception("Could not retrieve Zip Code for Lookup")
        }
      }
    } yield zipStation
  }

  def getCDD: Future[Int] = {
    for {
      lookUpTable <- lookupCDD
      ddSum <- computeDD("all",lookUpTable)
    } yield ddSum
  }

  def getHDD: Future[Int] = {
    for {
      lookUpTable <- lookupHDD
      ddSum <- computeDD("all",lookUpTable)
    } yield ddSum
  }

  def lookupCDD: Future[DDmonths] = {
    for {
      zipStation <- lookupWeatherStation
      futureTable <- cddStationLookupTable
      ddMonths <- {
        Future{
          (futureTable \ zipStation \ "months").get.validate[DDmonths] match {
            case JsSuccess(a,_) => a
            case _ => throw new Exception("Could not match Zip Code to Weather Station")
          }
        }
      }
    } yield ddMonths
  }

  def lookupHDD: Future[DDmonths] = {
    for {

      zipStation <- lookupWeatherStation
      futureTable <- hddStationLookupTable
      ddMonths <- {
        Future{
          (futureTable \ zipStation \ "months").get.validate[DDmonths] match {
            case JsSuccess(a,_) => a
            case _ => throw new Exception("Could not match Zip Code to Weather Station")
          }
        }
      }
    } yield ddMonths
  }

  def computeDD(month:String,table:DDmonths):Future[Int] = Future {


    val ddsum = month match {
      case "JAN" => table.JAN.sum
      case "FEB" => table.FEB.sum
      case "MAR" => table.MAR.sum
      case "APR" => table.APR.sum
      case "MAY" => table.MAY.sum
      case "JUN" => table.JUN.sum
      case "JUL" => table.JUL.sum
      case "AUG" => table.AUG.sum
      case "SEP" => table.SEP.sum
      case "OCT" => table.OCT.sum
      case "NOV" => table.NOV.sum
      case "DEC" => table.DEC.sum

      case "all" => {
        table.JAN.sum + table.FEB.sum + table.MAR.sum + table.APR.sum + table.MAY.sum + table.JUN.sum +
          table.JUL.sum + table.AUG.sum + table.SEP.sum + table.OCT.sum + table.NOV.sum + table.DEC.sum
      }
    }
    ddsum.toInt
  }


  val monthList:List[String] = List("JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC")

  def getZipCode:Future[String] = Future{
    parameters.validate[ZipCode] match {
      case JsSuccess(a, _) => a.postalCode
      case JsError(err) => throw new Exception("Could not find Zip Code!")
    }
  }
}


case class ZipCode(postalCode:String)
object ZipCode {
  implicit val zipCodeRead: Reads[ZipCode] = Json.reads[ZipCode]
}
case class DDmonths(JAN:Seq[Double],FEB:Seq[Double],MAR:Seq[Double],APR:Seq[Double],MAY:Seq[Double],JUN:Seq[Double],
  JUL:Seq[Double],AUG:Seq[Double],SEP:Seq[Double],OCT:Seq[Double],NOV:Seq[Double],DEC:Seq[Double])
object DDmonths {
  implicit val ddRead: Reads[DDmonths] = Json.reads[DDmonths]
}

case class WeatherStation(Station:String,Elevation:String)
object WeatherStation {
  implicit val weatherStationRead: Reads[WeatherStation] = Json.reads[WeatherStation]
}


case class PostalDegreeDays(postalCode:String) {
  import DegreeDays._

  def lookupWeatherStation: Future[String] = {
    for {
      zipTable <- zipStationLookupTable
      zipStation <-
      Future {
        (zipTable \ postalCode \ "station").toOption match {
          case Some(a) => a.as[String]
          case _ => throw new Exception("Could not retrieve Zip Code for Lookup")
        }
      }
    } yield zipStation
  }

  def getCDD: Future[Int] = {
    for {
      lookUpTable <- lookupCDD
      ddSum <- computeDD("all",lookUpTable)
    } yield ddSum
  }

  def getHDD: Future[Int] = {
    for {
      lookUpTable <- lookupHDD
      ddSum <- computeDD("all",lookUpTable)
    } yield ddSum
  }

  def lookupCDD: Future[DDmonths] = {
    for {
      zipStation <- lookupWeatherStation
      futureTable <- cddStationLookupTable
      ddMonths <- {
        Future{
          (futureTable \ zipStation \ "months").get.validate[DDmonths] match {
            case JsSuccess(a,_) => a
            case _ => throw new Exception("Could not match Zip Code to Weather Station")
          }
        }
      }
    } yield ddMonths
  }

  def lookupHDD: Future[DDmonths] = {
    for {

      zipStation <- lookupWeatherStation
      futureTable <- hddStationLookupTable
      ddMonths <- {
        Future{
          (futureTable \ zipStation \ "months").get.validate[DDmonths] match {
            case JsSuccess(a,_) => a
            case _ => throw new Exception("Could not match Zip Code to Weather Station")
          }
        }
      }
    } yield ddMonths
  }

  def computeDD(month:String,table:DDmonths):Future[Int] = Future {


    val ddsum = month match {
      case "JAN" => table.JAN.sum
      case "FEB" => table.FEB.sum
      case "MAR" => table.MAR.sum
      case "APR" => table.APR.sum
      case "MAY" => table.MAY.sum
      case "JUN" => table.JUN.sum
      case "JUL" => table.JUL.sum
      case "AUG" => table.AUG.sum
      case "SEP" => table.SEP.sum
      case "OCT" => table.OCT.sum
      case "NOV" => table.NOV.sum
      case "DEC" => table.DEC.sum

      case "all" => {
        table.JAN.sum + table.FEB.sum + table.MAR.sum + table.APR.sum + table.MAY.sum + table.JUN.sum +
          table.JUL.sum + table.AUG.sum + table.SEP.sum + table.OCT.sum + table.NOV.sum + table.DEC.sum
      }
    }
    ddsum.toInt
  }


  val monthList:List[String] = List("JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC")

  /*def getZipCode:Future[String] = Future{
    parameters.validate[ZipCode] match {
      case JsSuccess(a, _) => a.postalCode
      case JsError(err) => throw new Exception("Could not find Zip Code!")
    }
  }*/
}
