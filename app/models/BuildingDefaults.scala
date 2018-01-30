package models

import java.io.{File, InputStream}

import play.api.Play
import play.api.libs.json._

import scala.util.{Success, Try}
import play.api.Play.current

import scala.collection.generic.SeqFactory
import scala.concurrent.Future


case class CSVcompute(parameters: List[List[String]]) {

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


  def roundAt(p: Int)(n: Double): Double = { val s = math pow (10, p); (math round n * s) / s }

  def getDefaults(building:GoodJsonBuilding):JsValue = {
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
        "isSmallBank" -> JsBoolean(true),
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
        "isWarehouseRefrigerated" -> JsBoolean(false),
        "weeklyOperatingHours" -> JsNumber(60),
        "numWorkersMainShift" -> JsNumber(roundAt(2)(0.59*building.GFA/1000)),
        "numWalkinRefrUnits" -> JsNumber(0.0),
        "percentHeated" -> JsNumber(50),
        "percentCooled" -> JsNumber(20)
      ))
      case "RefrigeratedWarehouse" => JsObject(Map (
        "isWarehouseRefrigerated" -> JsBoolean(true),
        "weeklyOperatingHours" -> JsNumber(60),
        "numWorkersMainShift" -> JsNumber(roundAt(2)(0.59*building.GFA/1000)),
        "numWalkinRefrUnits" -> JsNumber(0.0),
        "percentHeated" -> JsNumber(50),
        "percentCooled" -> JsNumber(20)
      ))
      case _ => JsObject(Map (
        "default" -> JsBoolean(false)
      ))
    }

    val buildingJson = Json.toJson(building)
    val defaultNulls = JsObject(Map(
      "energies" -> JsNull,
      "renewableEnergies" -> JsNull
    ))

    buildingJson.as[JsObject].deepMerge(defaults).deepMerge(defaultNulls)
  }


  def buildingToJson(building:GoodJsonBuilding):JsValue = {
    val buildingJson = Json.toJson(building)
    val defaultNulls = JsObject(Map(
      "energies" -> JsNull,
      "renewableEnergies" -> JsNull
    ))
    buildingJson.as[JsObject]
  }
}

case class GoodJsonBuilding(buildingName: String, state: String, postalCode:String,
                           buildingType: String, GFA:Double, areaUnits:String, reportingUnits:String,
                            baselineConstant:Int=100, country:String="USA",netMetered:Boolean=false,
                            percentBetterThanMedian:Double=20)

object GoodJsonBuilding {
  implicit val jsonBuildingWrites: Writes[GoodJsonBuilding] = Json.writes[GoodJsonBuilding]
}

