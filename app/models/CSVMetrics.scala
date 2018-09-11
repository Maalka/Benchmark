package models

import java.io.{File, InputStream}

import play.api.Play
import play.api.libs.json._

import scala.util.{Success, Try}
import play.api.Play.current

import scala.collection.generic.SeqFactory
import scala.concurrent.Future


case class CSVcompute(parameters: List[List[String]]) {

  val validZipCodes:String = {
    play.api.Environment.simple().resourceAsStream("valid_zipcodes.json") match {
      case Some(is: InputStream) => {
        Json.parse(is).toString()
      }
      case _ => throw new Exception("Could not open file")
    }
  }

  def tryFormat(CSVvalue:String,checkType:String):Boolean = {
    checkType match {
      case "int" => {
        Try{CSVvalue.trim.toInt} match {
          case Success(a) => true
          case _ => false
        }
      }
      case "double" => {
        Try{CSVvalue.trim.toDouble} match {
          case Success(a) => true
          case _ => false
        }
      }
    }
  }

  val propTypes:List[String] = List(
    "DataCenter",
    "Hospital",
    "Hotel",
    "K12School",
    "MedicalOffice",
    "MultiFamily",
    "Office",
    "FinancialOffice",
    "Parking",
    "ResidenceHall",
    "Retail",
    "SeniorCare",
    "Supermarket",
    "Warehouse",
    "RefrigeratedWarehouse",
    "WastewaterCenter",
    "WorshipCenter",
    "FinancialOffice"
  )

  val states:List[String] = List(
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

  val buildingTypes:List[String] = List(
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



  val GFAUnits:List[String] = List(
    "sq.m",
    "sq.ft"
  )

  val outputUnits:String = {
    val units = parameters.flatten.dropWhile(!GFAUnits.contains(_)).headOption
    if (units.isDefined) {
      units.get
    } else {
      "Bad Units"
    }
  }
  val reportingUnits:String = {
    if (outputUnits == "sq.m") {
      "metric"
    } else {
      "us"
    }
  }
  def convertGFAUnits(CSVUnits:String):String = {
    if (CSVUnits == "sq.m") {
      "mSQ"
    } else {
      "ftSQ"
    }
  }


  val badEntries = parameters.filterNot {
    case List(a,b,c,d,e,f) if {
      states.contains(b.trim) && GFAUnits.contains(f.trim) &&  (c.trim.length == 5) &&
        tryFormat(e,"double") && validZipCodes.contains(c.trim) && buildingTypes.contains(d.trim)
    }  => true
    case _ => false
  }

  val badEntriesWithErrors = badEntries.map{
     _ match {
       case List(a,b,c,d,e,f) if (a == "Building ID") => List(a,b,c,d,e,f)
       case List(a,b,c,d,e,f) => {
         val stateEntry = if (states.contains(b.trim)){b.trim}else{"ERROR"}
         val unitsEntry = if (GFAUnits.contains(f.trim)){f.trim}else{"ERROR"}
         val postalCodeEntry = if (validZipCodes.contains(c.trim) && c.trim.length == 5 ){c.trim}else{"ERROR"}
         val GFAEntry = if (tryFormat(e,"double")){e.toDouble}else{"ERROR"}
         val buildingEntry = if (buildingTypes.contains(d.trim)){d.trim}else{"ERROR"}
         List(a,stateEntry,postalCodeEntry,buildingEntry,GFAEntry,unitsEntry)
       }
       case a => List("Error in Row (check for blank columns in CSV): " + a.mkString(" "))
    }
  }


}

case class GoodJsonBuilding(buildingName: String, state: String, postalCode:String,
                           buildingType: String, GFA:Double, areaUnits:String, reportingUnits:String,
                            baselineConstant:Int=100, country:String="USA",netMetered:Boolean=false,
                            percentBetterThanMedian:Double=20)

object GoodJsonBuilding {
  implicit val jsonBuildingWrites: Writes[GoodJsonBuilding] = Json.writes[GoodJsonBuilding]
}

