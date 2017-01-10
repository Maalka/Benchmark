package models

import java.util.Locale

import org.joda.time.format._
import org.joda.time._
import play.api.libs.json._

import scala.collection.generic.SeqFactory
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Success, Try}


case class CSVcompute(parameters: List[List[String]]) {

  def tryFormat(CSVvalue:String,checkType:String):Boolean = {
    checkType match {
      case "int" => {
        Try{CSVvalue.toInt} match {
          case Success(a) => true
          case _ => false
        }
      }
      case "double" => {
        Try{CSVvalue.toDouble} match {
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
    "WastewaterCenter",
    "WorshipCenter"
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

  val GFAUnits:List[String] = List(
    "ftSQ",
    "mSQ"
  )

  val buildingList:Seq[DefaultBuilding] ={
    parameters.collect {
      case List(a,b,c,d,e,f,g) if {
        states.contains(b.trim) && propTypes.contains(e.trim) && GFAUnits.contains(g.trim) && tryFormat(c,"int") &&
          tryFormat(d,"double")
      } => DefaultBuilding(a.trim,b.trim,c.trim,d.toDouble,e,f.toDouble,g)
    }
  }

  val badEntries = parameters.filterNot {
    case List(a,b,c,d,e,f,g) if {
      states.contains(b.trim) && propTypes.contains(e.trim) && GFAUnits.contains(g.trim) && tryFormat(c,"int") &&
        tryFormat(d,"double")
    }  => true
    case _ => false
  }

  val buildingJsonList = buildingList.map(getDefaults(_))
  buildingJsonList.map(println)

  println("-----------------------")
  println(buildingList, badEntries)

  def roundAt(p: Int)(n: Double): Double = { val s = math pow (10, p); (math round n * s) / s }

  def getDefaults(building:DefaultBuilding):JsValue = {
    val defaults = building.buildingType match {
      case "DataCenter" => JsObject(Map (
        "annualITEnergy" -> JsNull
      ))
      case "Hospital" => JsObject(Map (
        "numStaffedBeds" -> JsNumber(roundAt(2)(0.46 * building.GFA / 1000)),
        "numFTEWorkers" -> JsNumber(roundAt(2)(2.6 * building.GFA / 1000)),
        "numMRIMachines" -> JsNumber(1)
      ))
      case "Hotel" => JsObject(Map (
        "numBedrooms" -> JsNumber(roundAt(2)(1.95 * building.GFA / 1000)),
        "numWorkersMainShift" -> JsNumber(roundAt(2)(0.32 * building.GFA / 1000)),
        "hasFoodPreparation" -> JsBoolean(false),
        "numRefrUnits" -> JsNumber(roundAt(2)(0.023*building.GFA/1000)),
        "percentHeated" -> JsNumber(100),
        "percentCooled" -> JsNumber(100)
      ))
      case "K12School" => JsObject(Map (
        "isHighSchool" -> JsBoolean(false),
        "numWalkinRefrUnits" -> JsNumber(roundAt(2)(0.01*building.GFA/1000)),
        "isOpenWeekends" -> JsBoolean(false),
        "hasCooking" -> JsBoolean(true),
        "numComputers" -> JsNumber(roundAt(2)(1.75*building.GFA/1000)),
        "percentHeated" -> JsNumber(100),
        "percentCooled" -> JsNumber(100)
      ))
      case "MedicalOffice" => JsObject(Map (
        "weeklyOperatingHours" -> JsNumber(65),
        "numWorkersMainShift" -> JsNumber(roundAt(2)(2.2*building.GFA/1000)),
        "percentHeated" -> JsNumber(100),
        "percentCooled" -> JsNumber(100)
      ))
      case "MultiFamily" => JsObject(Map (
        "numRezUnits" -> JsNumber(roundAt(2)(1.2*building.GFA/1000)),
        "numUnitsLowRise1to4" -> JsNumber(roundAt(2)(1.2*building.GFA/1000)),
        "numUnitsMidRise5to9" -> JsNumber(0.0),
        "numUnitsHighRise10plus" -> JsNumber(0.0),
        "numBedrooms" -> JsNumber(roundAt(2)(1.4*building.GFA/1000))
      ))
      case "Office" => JsObject(Map (
        "weeklyOperatingHours" -> JsNumber(65),
        "numWorkersMainShift" -> JsNumber(roundAt(2)(2.3*building.GFA/1000)),
        "numComputers" -> JsNumber(roundAt(2)(2*building.GFA/1000)),
        "percentHeated" -> JsNumber(100),
        "percentCooled" -> JsNumber(100)
      ))
      case "FinancialOffice" => JsObject(Map (
        "weeklyOperatingHours" -> JsNumber(65),
        "numWorkersMainShift" -> JsNumber(roundAt(2)(2.3*building.GFA/1000)),
        "numComputers" -> JsNumber(roundAt(2)(2*building.GFA/1000)),
        "percentHeated" -> JsNumber(100),
        "percentCooled" -> JsNumber(100),
        "isSmallBank" -> JsBoolean(true)
      ))
      case "ResidenceHall" => JsObject(Map (
        "numBedrooms" -> JsNumber(100),
        "percentHeated" -> JsNumber(100),
        "percentCooled" -> JsNumber(100)
      ))
      case "Retail" => JsObject(Map (
        "weeklyOperatingHours" -> JsNumber(65),
        "numOpenClosedRefrCases" -> JsNumber(0.0),
        "numWalkinRefrUnits" -> JsNumber(0.0),
        "numCashRegisters" -> JsNumber(roundAt(2)(0.3*building.GFA/1000)),
        "numWorkersMainShift" -> JsNumber(roundAt(2)(1.0*building.GFA/1000)),
        "numComputers" -> JsNumber(roundAt(2)(0.2*building.GFA/1000)),
        "percentHeated" -> JsNumber(100),
        "percentCooled" -> JsNumber(100),
        "isSmallBank" -> JsBoolean(true)
      ))
      case "SeniorCare" => JsObject(Map (
        "maxNumResidents" -> JsNumber(roundAt(2)(2.374*building.GFA/1000)),
        "avgNumResidents" -> JsNumber(roundAt(2)(2.075*building.GFA/1000)),
        "numRezUnits" -> JsNumber(roundAt(2)(1.584*building.GFA/1000)),
        "numWorkersMainShift" -> JsNumber(roundAt(2)(0.9523*building.GFA/1000)),
        "numComputers" -> JsNumber(roundAt(2)(0.367*building.GFA/1000)),
        "numRezWashingMachines" -> JsNumber(roundAt(2)(0.05757*building.GFA/1000)),
        "numCommWashingMachines" -> JsNumber(roundAt(2)(0.04422*building.GFA/1000)),
        "numElectronicLifts" -> JsNumber(roundAt(2)(0.0704*building.GFA/1000)),
        "numRefrUnits" -> JsNumber(roundAt(2)(0.09065*building.GFA/1000)),
        "percentHeated" -> JsNumber(100),
        "percentCooled" -> JsNumber(100),
        "isSmallBank" -> JsBoolean(true)
      ))
      case "Supermarket" => JsObject(Map (
        "weeklyOperatingHours" -> JsNumber(105),
        "numWorkersMainShift" -> JsNumber(roundAt(2)(1.0*building.GFA/1000)),
        "numWalkinRefrUnits" -> JsNumber(roundAt(2)(0.25*building.GFA/1000)),
        "percentHeated" -> JsNumber(100),
        "percentCooled" -> JsNumber(100),
        "hasCooking" -> JsBoolean(true)
      ))
      case "WastewaterCenter" => JsObject(Map (
        "wastewaterInfluentBiologicalOxygenDemand" -> JsNumber(200.0),
        "wastewaterEffluentBiologicalOxygenDemand" -> JsNumber(8.0),
        "wastewaterAvgInfluentInflow" -> JsNull,
        "wastewaterPlantDesignFlowRate" -> JsNull,
        "wastewaterLoadFactor" -> JsNull,
        "wastewaterHasTrickleFiltration" -> JsBoolean(false),
        "wastewaterHasNutrientRemoval" -> JsBoolean(false),
        "numWorkersMainShift" -> JsNumber(roundAt(2)(1.0*building.GFA/1000)),
        "numWalkinRefrUnits" -> JsNumber(roundAt(2)(0.25*building.GFA/1000)),
        "percentHeated" -> JsNumber(100),
        "percentCooled" -> JsNumber(100),
        "hasCooking" -> JsBoolean(true)
      ))
      case "WorshipCenter" => JsObject(Map (
        "weeklyOperatingHours" -> JsNumber(33),
        "numWalkinRefrUnits" -> JsNumber(0.0),
        "seatingCapacity" -> JsNumber(roundAt(2)(40.0*building.GFA/1000)),
        "numRefrUnits" -> JsNumber(roundAt(2)(0.018*building.GFA/1000)),
        "numComputers" -> JsNumber(roundAt(2)(0.2*building.GFA/1000)),
        "percentHeated" -> JsNumber(100),
        "percentCooled" -> JsNumber(100),
        "isOpenAllWeekdays" -> JsBoolean(false),
        "hasFoodPreparation" -> JsBoolean(false)
      ))
      case "Warehouse" => JsObject(Map (
        "weeklyOperatingHours" -> JsNumber(60),
        "numWorkersMainShift" -> JsNumber(roundAt(2)(0.59*building.GFA/1000)),
        "numWalkinRefrUnits" -> JsNumber(0.0),
        "percentHeated" -> JsNumber(50),
        "percentCooled" -> JsNumber(20)
      ))
    }

    val buildingJson = Json.toJson(building)
    val defaultNulls = JsObject(Map(
      "energies" -> JsNull,
      "renewableEnergies" -> JsNull
    ))

    buildingJson.as[JsObject].deepMerge(defaults).deepMerge(defaultNulls)
  }
}


case class DefaultBuilding(address: String, state: String, postalCode:String, percentBetterThanMedian:Double,
                           buildingType: String, GFA:Double, areaUnits:String, baselineConstant:Int=100,
                           country:String="USA", reportingUnits:String="us",netMetered:Boolean=false,
                           HDD:Double=6346, CDD:Double=815)

object DefaultBuilding {
  implicit val defaultBuildingWrites: Writes[DefaultBuilding] = Json.writes[DefaultBuilding]
}