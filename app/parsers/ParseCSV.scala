package parsers

import java.io.{InputStream, InputStreamReader}

import akka.NotUsed
import akka.actor.ActorSystem
import akka.stream.scaladsl._
import com.github.tototoshi.csv.CSVReader
import com.google.common.base.Charsets
import javax.inject.Inject
import parsers.ParseCSV.NotValidCSVRow
import play.api.libs.json._

import scala.concurrent.ExecutionContext
import scala.util.{Success, Try}

object ParseCSV {
  import rowCheck._
  case class NotValidCSVRow(row:Seq[Option[String]]) extends Exception {
    val badEntriesWithErrors = row match {
        case List(a, Some(b: String), Some(c: String), Some(d: String), Some(e: String), Some(f: String)) => {
          val nameEntry = if (a.isDefined) {
            a.get.trim
          } else {
            "---" +" No Name Supplied!" + "---"
          }
          val stateEntry = if (states.contains(b.trim)) {
            b.trim
          } else {
            "---" + b.trim + "---"
          }
          val postalCodeEntry = if (c.trim.length == 5) {
            c.trim
          } else {
            "---" + c.trim + "---"
          }
          val buildingEntry = if (buildingTypes.contains(d.trim)) {
            d.trim
          } else {
            "---" + d.trim + "---"
          }
          val GFAEntry = if (tryFormat(e, "double")) {
            e.toDouble
          } else {
            "---" + e.trim + "---"
          }
          val unitsEntry = if (unitList.contains(f.trim)) {
            f.trim
          } else {
            "---" + f.trim + "---"
          }
          Seq(nameEntry, stateEntry, buildingEntry, unitsEntry, postalCodeEntry, GFAEntry)
        }
        case _ => Seq("Unspecified Error Detected")
      }
  }

}

class ParseCSV @Inject()(implicit val actorSystem: ActorSystem, executionContext: ExecutionContext) {
  import rowCheck._

  private def source(stream: Stream[Seq[String]]) = Source
    .fromIterator(() => stream.toIterator)
    .map(_.map {
      case "" => None
      case a => Some(a)
    }.toSeq)

  private def toStream(is: InputStream): Stream[Seq[String]] = {
    CSVReader.open(new InputStreamReader(is, Charsets.UTF_8))
      .toStream()
  }

  def toPortfolioFlow(is:InputStream): Source[Either[NotValidCSVRow, JsValue], NotUsed] = {

    source(toStream(is)).via(portfolioSegmentFlow())

  }



  def rowValid(row:Seq[Option[String]]):Boolean = {

    (
      row(0).isDefined &&
      states.contains(row(1).get.trim) &&
      buildingTypes.contains(row(3).get.trim) &&
      unitList.contains(row(5).get.trim) &&
      (row(2).get.trim.length == 5) &&
      tryFormat(row(4).get, "double")
    )
  }


  private def portfolioSegmentFlow(): Flow[Seq[Option[String]], Either[NotValidCSVRow, JsValue], NotUsed] = Flow[Seq[Option[String]]]
    .map { row =>
        if (rowValid(row)) {
          Right(
            Json.arr(
              Json.obj(
              "buildingName" -> JsString(row(0).get.trim),
              "state" -> JsString(row(1).get.trim),
              "postalCode" -> JsString(row(2).get.trim),
              "buildingType" -> JsString(getBuildingType(row(3).get.trim)),
              "GFA" -> JsNumber(row(4).get.trim.toDouble),
              "areaUnits" -> JsString(convertGFAUnits(row(5).get.trim)),
              "reportingUnits" -> JsString("us"),
              "country" -> JsString("USA")
              )
            )
          )
      } else {
          Left(NotValidCSVRow(row))
      }
  }


  def getBuildingType(bldgType:String):String = {
    bldgType match {
      case "BankBranch" => "FinancialOffice"
      case "DistributionCenter" => "Warehouse"
      case "ConvenienceStoreWithGas" => "GasStation"
      case "ConvenienceStoreNoGas" => "ConvenienceStore"
      case "MixedUseProperty" => "MixedUse"
      case "WarehouseRefrigerated" => "RefrigeratedWarehouse"
      case "WarehouseUnRefrigerated" => "Warehouse"
      case "EnergyStation" => "PowerStation"
      case _ => bldgType
    }
  }


  def convertGFAUnits(CSVUnits:String):String = {
    if (Seq("sq.m","sqm","m2","mSQ").contains(CSVUnits)){
      "mSQ"
    } else {
      "ftSQ"
    }
  }
}

object rowCheck {

  def tryFormat(CSVvalue:String,checkType:String):Boolean = {
    checkType match {
      case "int" => {
        Try {
          CSVvalue.trim.toInt
        } match {
          case Success(a) => true
          case _ => false
        }
      }
      case "double" => {
        Try {
          CSVvalue.trim.toDouble
        } match {
          case Success(a) => true
          case _ => false
        }
      }
    }
  }

  val unitList:Seq[String]=Seq("sq.ft","sq.m","sqft","sqm","ft2","m2","mSQ","ftSQ")

  val states: List[String] = List (
  "AL",
  "AK",
  "AZ",
  "AR",
  "CA",
  "CO",
  "CT",
  "DE",
  "FL",
  "GA",
  "HI",
  "ID",
  "IL",
  "IN",
  "IA",
  "KS",
  "KY",
  "LA",
  "ME",
  "MD",
  "MA",
  "MI",
  "MN",
  "MS",
  "MO",
  "MT",
  "NE",
  "NV",
  "NH",
  "NJ",
  "NM",
  "NY",
  "NC",
  "ND",
  "OH",
  "OK",
  "OR",
  "PA",
  "RI",
  "SC",
  "SD",
  "TN",
  "TX",
  "UT",
  "VT",
  "VA",
  "WA",
  "WV",
  "WI",
  "WY"
  )

  val buildingTypes: List[String] = List (
  "AdultEducation",
  "College",
  "PreSchool",
  "VocationalSchool",
  "OtherEducation",
  "ConventionCenter",
  "MovieTheater",
  "Museum",
  "PerformingArts",
  "BowlingAlley",
  "FitnessCenter",
  "IceRink",
  "RollerRink",
  "SwimmingPool",
  "OtherRecreation",
  "Stadium",
  "FinancialOffice",
  "Retail",
  "DistributionCenter",
  "Warehouse",
  "SpecialtyHospital",
  "MedicalOffice",
  "OutpatientCenter",
  "PhysicalTherapyCenter",
  "SeniorCare",
  "UrgentCareCenter",
  "Barracks",
  "Hotel",
  "MultiFamily",
  "Prison",
  "ResidenceHall",
  "ResidentialLodging",
  "MixedUse",
  "Office",
  "VeterinaryOffice",
  "Courthouse",
  "OtherUtility",
  "SelfStorageFacility",
  "RefrigeratedWarehouse",
  "IndoorArena",
  "RaceTrack",
  "Aquarium",
  "Bar",
  "Nightclub",
  "Casino",
  "Zoo",
  "OtherEntertainment",
  "GasStation",
  "ConvenienceStore",
  "FastFoodRestaurant",
  "Restaurant",
  "Supermarket",
  "WholesaleClub",
  "FoodSales",
  "FoodService",
  "AmbulatorySurgicalCenter",
  "Hospital",
  "StripMall",
  "DrinkingWaterTreatment",
  "FireStation",
  "Library",
  "PostOffice",
  "PoliceStation",
  "MeetingHall",
  "TransportationTerminal",
  "WastewaterCenter",
  "OtherPublicServices",
  "WorshipCenter",
  "AutoDealership",
  "EnclosedMall",
  "DataCenter",
  "PersonalServices",
  "RepairServices",
  "OtherServices",
  "PowerStation",
  "EnergyStation",
  "K12School",

  "SingleFamilyAttached",
  "SingleFamilyDetached",
  "MobileHome",

  "BankBranch", // FinancialOffice
  "K12School",
  "DistributionCenter", // Warehouse
  "ConvenienceStoreWithGas", // GasStation
  "ConvenienceStoreNoGas", // GasStation
  "MixedUseProperty", // MixedUse
  "WarehouseRefrigerated", //RefrigeratedWarehouse
  "WarehouseUnRefrigerated", // Warehouse
  "Other"
  )
}

