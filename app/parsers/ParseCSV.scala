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
        case List(a, b,c,d,e,f) => {
          val nameEntry = if (a.isDefined) {
            a.get.trim
          } else {
            "---" +" No Name Supplied" + "---"
          }
          val stateEntry = if (b.isDefined){
              if(states.contains(b.getOrElse("").trim)) {
                b.get.trim
              } else {
                "---" + b.get.trim + "---"
              }
          } else {
            "---No Building Name---"
          }

          val postalCodeEntry = if (c.isDefined){
              if(c.getOrElse("").trim.length == 5) {
                c.get.trim
              } else {
                "---" + c.get.trim + "---"
              }
          } else {
            "---No Postal Code---"
          }
          val buildingEntry = if (d.isDefined){
            if(buildingTypes.contains(d.getOrElse("").trim)) {
              d.get.trim
            } else {
                "---" + d.get.trim + "---"
              }
          } else {
            "---No Building Type---"
          }

          val GFAEntry = if (e.isDefined){
            if (tryFormat(e.get, "double")) {
              e.get.toDouble
            } else {
                "---" + e.get.toString.trim + "---"
              }
          } else {
            "---No GFA Value---"
          }
          val unitsEntry = if (f.isDefined){
            if(unitList.contains(f.getOrElse("").trim)) {
              f.get.trim
            } else {
              "---" + f.get.trim + "---"
            }
          } else {
            "---No GFA Units---"
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
      .toStream
  }

  def toPortfolioFlow(is:InputStream,reportingUnits:String): Source[Either[NotValidCSVRow, JsValue], NotUsed] = {

    source(toStream(is)).via(portfolioSegmentFlow(reportingUnits))

  }



  def rowValid(row:Seq[Option[String]]):Boolean = {
    row(0).isDefined &&
      states.contains(row(1).getOrElse("").trim) &&
      buildingTypes.contains(row(3).getOrElse("").trim) &&
      unitList.contains(row(5).getOrElse("").trim) &&
      (row(2).getOrElse("").trim.length == 5) &&
      tryFormat(row(4).getOrElse(""), "double")
  }


  private def portfolioSegmentFlow(reportingUnits:String): Flow[Seq[Option[String]], Either[NotValidCSVRow, JsValue], NotUsed] = Flow[Seq[Option[String]]]
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
              "reportingUnits" -> JsString(reportingUnits),
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
      case "ConvenienceStoreAndGas" => "GasStation"
      case "ConvenienceStoreNoGas" => "ConvenienceStore"
      case "OtherResidentialLodging" => "ResidentialLodging"
      case "MixedUse" => "MixedUse"
      case "WarehouseRefrigerated" => "WarehouseRefrigerated"
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
  "OtherResidentialLodging",
  "MixedUse",
  "Office",
  "VeterinaryOffice",
  "Courthouse",
  "OtherUtility",
  "SelfStorageFacility",
  "WarehouseRefrigerated",
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
  "ConvenienceStoreAndGas", // GasStation
  "ConvenienceStoreNoGas", // GasStation
  "MixedUse", // MixedUse
  "WarehouseRefrigerated", //WarehouseRefrigerated
  "WarehouseUnRefrigerated", // Warehouse
  "Other"
  )
}

