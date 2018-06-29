package models

import squants.energy._
import squants.space._

import scala.concurrent.Future
import scala.language._
import scala.math._
import play.api.libs.json._

import scala.concurrent.ExecutionContext.Implicits.global
import play.api.libs.functional.syntax._

case class BuildingProperties(parameters: JsValue) {


  def country:String = {
    parameters.asOpt[ConversionInfo] match {
      case Some(a) => {
        a.country match {
          case Some(country:String) => country
          case _ => throw new Exception("Could not retrieve Country")
        }
      }

      case _ => throw new Exception("Could not retrieve Country")
    }
  }


  def reportingUnits:String = {
    parameters.asOpt[ConversionInfo] match {
      case Some(a) => {
        a.reportingUnits match {
          case Some(reportingUnits:String) => reportingUnits
          case None => "us"
        }
      }
      case _ => throw new Exception("Could not retrieve Reporting Units")
    }
  }


  def postalCode: String = {
    parameters.asOpt[ConversionInfo] match {
      case Some(a) => {
        a.postalCode match {
          case Some(postalCode:String) => postalCode
          case _ => throw new Exception("Could not retrieve Postal Code")
        }
      }
      case _ => throw new Exception("Could not retrieve Postal Code")

    }
  }

  def state: String = {
    parameters.asOpt[ConversionInfo] match {
      case Some(a) => {
        a.state match {
          case Some(state:String) => state
          case None => throw new Exception("Could not retrieve State")
        }
      }
      case _ => throw new Exception("Could not retrieve State")
    }
  }

  def buildingName: String = {
    parameters.asOpt[ConversionInfo] match {
      case Some(a) => {
        a.buildingName match {
          case Some(name:String) => name
          case None => "Anonymous"
        }
      }
      case _ => throw new Exception("Could not retrieve Building Name")
    }
  }


  def getBaselineConstant: Future[Int] = Future{
    parameters.asOpt[BaselineConstant] match {
      case Some(a) => a.baselineConstant
      case _ => 100
      //default using ZEPI not HERS
      // case _ => throw new Exception("Could not get Baseline Constant for CBECS / HERS Conversion")
    }
  }

  def getTarget2030Value:Boolean = {
    parameters.validate[Target2030] match {
      case JsSuccess(a, _) => a.target2030
      case JsError(err) => false
      //throw new Exception("Could not determine target EUI!")
    }
  }

  def getPercentBetterThanMedia:Future[Double] = {
    for {
      targetToggle <- getTargetToggle
      baselineConstant <- getBaselineConstant
      toggleValue <- getToggleValue
      percentBetterReturn <- Future{
        targetToggle match {
          case "zeroScore" => baselineConstant - toggleValue
          case _ => toggleValue
        }
      }
    } yield percentBetterReturn
  }

  def getToggleValue:Future[Double] = Future{
    parameters.validate[PercentBetterThanMedian] match {
      case JsSuccess(a, _) => a.target
      case JsError(err) => 20.0 //default to 20
      //throw new Exception("Could not determine target EUI!")
    }
  }

  def getTargetToggle:Future[String] = Future{
    parameters.validate[TargetToggle] match {
      case JsSuccess(a, _) => a.targetToggle
      case JsError(err) => "percentReduction"
    }
  }

  def getBuilding: Future[BaseLine] = Future{

    val building: JsResult[BaseLine] = parameters.asOpt[CountryBuildingType] match {
      case Some(CountryBuildingType("USA", "Office")) => parameters.validate[Office]
      case Some(CountryBuildingType("USA", "WorshipCenter")) => parameters.validate[WorshipCenter]
      case Some(CountryBuildingType("USA", "WastewaterCenter")) => parameters.validate[WastewaterCenter]
      case Some(CountryBuildingType("USA", "Warehouse")) => parameters.validate[Warehouse]
      case Some(CountryBuildingType("USA", "RefrigeratedWarehouse")) => parameters.validate[RefrigeratedWarehouse]
      case Some(CountryBuildingType("USA", "Supermarket")) => parameters.validate[Supermarket]
      case Some(CountryBuildingType("USA", "SeniorCare")) => parameters.validate[SeniorCare]
      case Some(CountryBuildingType("USA", "Retail")) => parameters.validate[Retail]
      case Some(CountryBuildingType("USA", "ResidenceHall")) => parameters.validate[ResidenceHall]
      case Some(CountryBuildingType("USA", "MultiFamily")) => parameters.validate[MultiFamily]
      case Some(CountryBuildingType("USA", "MedicalOffice")) => parameters.validate[MedicalOffice]
      case Some(CountryBuildingType("USA", "K12School")) => parameters.validate[K12School]
      case Some(CountryBuildingType("USA", "Hotel")) => parameters.validate[Hotel]
      case Some(CountryBuildingType("USA", "Hospital")) => parameters.validate[Hospital]
      case Some(CountryBuildingType("USA", "DataCenter")) => parameters.validate[DataCenter]
      case Some(CountryBuildingType("USA", "FinancialOffice")) => parameters.validate[FinancialOffice]
      //case Some(CountryBuildingType("USA", "Parking")) => parameters.validate[Parking]
      case Some(CountryBuildingType("Canada", "Office")) => parameters.validate[CanadaOffice]
      case Some(CountryBuildingType("Canada", "Supermarket")) => parameters.validate[CanadaSupermarket]
      case Some(CountryBuildingType("Canada", "MedicalOffice")) => parameters.validate[CanadaMedicalOffice]
      case Some(CountryBuildingType("Canada", "K12School")) => parameters.validate[CanadaK12School]
      case Some(CountryBuildingType("Canada", "Hospital")) => parameters.validate[CanadaHospital]
      //case Some(CountryBuildingType("Canada", "Parking")) => parameters.validate[CanadaParking]
      case Some(_) => parameters.validate[GenericBuilding]
      case None => JsError("Could not find country or buildingType fields with JSON")
    }
    building match {
      case JsSuccess(a: BaseLine, _) => a
      case JsError(err) => throw new Exception("Building Type parameters fail validation!")
    }
  }

  def getRegion:String = {

    parameters.asOpt[StateBuildingType] match {
      case Some(StateBuildingType("WA", _)) => "West"
      case Some(StateBuildingType("OR", _)) => "West"
      case Some(StateBuildingType("CA", _)) => "West"
      case Some(StateBuildingType("MT", _)) => "West"
      case Some(StateBuildingType("ID", _)) => "West"
      case Some(StateBuildingType("NV", _)) => "West"
      case Some(StateBuildingType("WY", _)) => "West"
      case Some(StateBuildingType("UT", _)) => "West"
      case Some(StateBuildingType("CO", _)) => "West"
      case Some(StateBuildingType("AZ", _)) => "West"
      case Some(StateBuildingType("NM", _)) => "West"
      case Some(StateBuildingType("AK", _)) => "West"
      case Some(StateBuildingType("HI", _)) => "West"

      case Some(StateBuildingType("ND", _)) => "Midwest"
      case Some(StateBuildingType("SD", _)) => "Midwest"
      case Some(StateBuildingType("NE", _)) => "Midwest"
      case Some(StateBuildingType("KS", _)) => "Midwest"
      case Some(StateBuildingType("MN", _)) => "Midwest"
      case Some(StateBuildingType("IA", _)) => "Midwest"
      case Some(StateBuildingType("MO", _)) => "Midwest"
      case Some(StateBuildingType("WI", _)) => "Midwest"
      case Some(StateBuildingType("IL", _)) => "Midwest"
      case Some(StateBuildingType("MI", _)) => "Midwest"
      case Some(StateBuildingType("IN", _)) => "Midwest"
      case Some(StateBuildingType("OH", _)) => "Midwest"

      case Some(StateBuildingType("OK", _)) => "South"
      case Some(StateBuildingType("TX", _)) => "South"
      case Some(StateBuildingType("AR", _)) => "South"
      case Some(StateBuildingType("LA", _)) => "South"
      case Some(StateBuildingType("KY", _)) => "South"
      case Some(StateBuildingType("TN", _)) => "South"
      case Some(StateBuildingType("MS", _)) => "South"
      case Some(StateBuildingType("AL", _)) => "South"
      case Some(StateBuildingType("WV", _)) => "South"
      case Some(StateBuildingType("DE", _)) => "South"
      case Some(StateBuildingType("MD", _)) => "South"
      case Some(StateBuildingType("DC", _)) => "South"
      case Some(StateBuildingType("VA", _)) => "South"
      case Some(StateBuildingType("NC", _)) => "South"
      case Some(StateBuildingType("SC", _)) => "South"
      case Some(StateBuildingType("GA", _)) => "South"
      case Some(StateBuildingType("FL", _)) => "South"

      case Some(StateBuildingType("PA", _)) => "Northeast"
      case Some(StateBuildingType("NY", _)) => "Northeast"
      case Some(StateBuildingType("NJ", _)) => "Northeast"
      case Some(StateBuildingType("CT", _)) => "Northeast"
      case Some(StateBuildingType("RI", _)) => "Northeast"
      case Some(StateBuildingType("MA", _)) => "Northeast"
      case Some(StateBuildingType("VT", _)) => "Northeast"
      case Some(StateBuildingType("NH", _)) => "Northeast"
      case Some(StateBuildingType("ME", _)) => "Northeast"

      case Some(StateBuildingType(_, _)) => "Canada"
      case _ => throw new Exception("Could not find State to identify country region")
    }
  }
}

case class PosInt(value: Int)
object PosInt {
  implicit val reads: Reads[PosInt] = JsPath.read[Int](Reads.min(0)).map(new PosInt(_))
}

case class PosDouble(value: Double)
object PosDouble {
  implicit val reads: Reads[PosDouble] = JsPath.read[Double](Reads.min(0.0)).map(new PosDouble(_))
}

case class Target2030(target2030: Boolean)
object Target2030 {
  implicit val Target2030Reads:Reads[Target2030] = Json.format[Target2030]
}

case class BuildingArea(GFA:PosDouble)
object BuildingArea {
  implicit val buildingAreaReads: Reads[BuildingArea] = Json.reads[BuildingArea]
}
case class JsonEntry(ES: Int, CmPercent: Double, Ratio: Double)
object JsonEntry {
  implicit val formatFileName:Reads[JsonEntry] = Json.format[JsonEntry]
}
case class BaselineConstant(baselineConstant:Int)
object BaselineConstant {
  implicit val baselineConstantReads:Reads[BaselineConstant] = Json.format[BaselineConstant]
}

case class PercentBetterThanMedian(target:Double)
object PercentBetterThanMedian {
  implicit val percentBetterReads: Reads[PercentBetterThanMedian] = (JsPath \ "percentBetterThanMedian").read[Double](
    Reads.min(0.0) andKeep Reads.max(100.0)).map(new PercentBetterThanMedian(_))
}

case class TargetToggle(targetToggle: String)
object TargetToggle {
  implicit val targetToggleRead: Reads[TargetToggle] = Json.reads[TargetToggle]
}

case class PropParams(propType:String,propSize:Double,propPercent:Double,areaUnits:String,propTypeName:String)

case class HeatingCoolingDays(HDD: Option[Double], CDD: Option[Double])
object HeatingCoolingDays {
  implicit val heatingCoolingDayseRead: Reads[HeatingCoolingDays] = Json.reads[HeatingCoolingDays]
}

/** *
  * Base line trait, enables the reducing of equation segments and manages the lookup of energy star score values
  */
sealed trait BaseLine {

  val country: String
  val buildingType: String
  val propTypeName: Option[String]
  val GFA: PosDouble
  val areaUnits: String
  val printed: String
  val HDD: Option[Double]
  val CDD: Option[Double]

  val postalCode: String
  val degreeDays = PostalDegreeDays(postalCode)


  def regressionSegments(HDD: Double, CDD: Double): Seq[RegressionSegment]


  def expectedEnergy: Future[Double] = {
    for {
      heating <- {
        HDD match {
          case Some(a) => Future(a)
          case _ => degreeDays.lookupHDD
        }
      }
      cooling <- {
        CDD match {
          case Some(a) => Future(a)
          case _ => degreeDays.lookupCDD
        }
      }
      propSequence <- Future(regressionSegments(heating, cooling))
      predictedEnergy <- Future(propSequence.map(_.reduce).sum)
    } yield predictedEnergy

  }
  def getHDD: Future[Double] = {
    for {
      heating <- {
        HDD match {
          case Some(a) => Future(a)
          case _ => degreeDays.lookupHDD
        }
      }
    } yield heating
  }

  def getCDD: Future[Double] = {
    for {
      cooling <- {
        CDD match {
          case Some(a) => Future(a)
          case _ => degreeDays.lookupCDD
        }
      }
    } yield cooling

  }

  implicit def boolOptToInt(b: Option[Boolean]): Int = if (b.getOrElse(false)) 1 else 0
  implicit def boolToInt(b: Boolean): Int = if (b==true) 1 else 0

  def converseBoolean(i: Int): Int = {
    if (i == 0) {
      1
    } else if (i == 1) {
      0
    } else {
      0
    }
  }

  val buildingSize: Double = {
    (areaUnits, country) match {
      case ("ftSQ","USA") => SquareFeet(GFA.value).value
      case ("mSQ","USA") => SquareMeters(GFA.value) to SquareFeet
      case ("ftSQ","Canada") => SquareFeet(GFA.value) to SquareMeters
      case ("mSQ","Canada") => SquareMeters(GFA.value).value
    }
  }

  def getLog(x: Double): Double = {
    x match {
      case (0) => 0.0
      case x => log(x)
    }
  }

  def roundAt(p: Int)(n: Double): Double = {
    val s = math pow(10, p);
    (math round n * s) / s
  }

  def fillPosDoubleDefaults(building: String, parameter: String, size: Double): PosDouble = {


    building match {
      case "Office" => {
        parameter match {
          case "weeklyOperatingHours" => PosDouble(65.0)
          case "numWorkersMainShift" => PosDouble(roundAt(2)(2.3 * size / 1000))
          case "numComputers" => PosDouble(roundAt(2)(2 * size / 1000))
          case "percentHeated" => PosDouble(100.0)
          case "percentCooled" => PosDouble(100.0)
        }
      }
      case "DataCenter" => {
        parameter match {
          case "annualITEnergy" => null
        }
      }
      case "Hospital" => {
        parameter match {
          case "numStaffedBeds" => PosDouble(roundAt(2)(0.46 * size / 1000))
          case "numFTEWorkers" => PosDouble(roundAt(2)(2.6 * size / 1000))
          case "numMRIMachines" => PosDouble(1.0)
        }
      }
      case "Hotel" => {
        parameter match {
          case "numBedrooms" => PosDouble(roundAt(2)(1.95 * size / 1000))
          case "numWorkersMainShift" => PosDouble(roundAt(2)(0.32 * size / 1000))
          case "numRefrUnits" => PosDouble(roundAt(2)(0.023 * size / 1000))
          case "percentHeated" => PosDouble(100.0)
          case "percentCooled" => PosDouble(100.0)
        }
      }
      case "K12School" => {
        parameter match {
          case "numWalkinRefrUnits" => PosDouble(roundAt(2)(0.01 * size / 1000))
          case "numComputers" => PosDouble(roundAt(2)(1.75 * size / 1000))
          case "percentHeated" => PosDouble(100.0)
          case "percentCooled" => PosDouble(100.0)
        }
      }
      case "MedicalOffice" => {
        parameter match {
          case "weeklyOperatingHours" => PosDouble(65.0)
          case "numWorkersMainShift" => PosDouble(roundAt(2)(2.2 * size / 1000))
          case "percentHeated" => PosDouble(100.0)
          case "percentCooled" => PosDouble(100.0)
        }
      }
      case "MultiFamily" => {
        parameter match {
          case "numRezUnits" => PosDouble(roundAt(2)(1.2 * size / 1000))
          case "numUnitsLowRise1to4" => PosDouble(roundAt(2)(1.2 * size / 1000))
          case "numBedrooms" => PosDouble(roundAt(2)(1.4 * size / 1000))
        }
      }
      case "FinancialOffice" => {
        parameter match {
          case "weeklyOperatingHours" => PosDouble(65.0)
          case "numWorkersMainShift" => PosDouble(roundAt(2)(2.3 * size / 1000))
          case "numComputers" => PosDouble(roundAt(2)(2 * size / 1000))
          case "percentHeated" => PosDouble(100.0)
          case "percentCooled" => PosDouble(100.0)
        }
      }
      case "ResidenceHall" => {
        parameter match {
          case "numBedrooms" => PosDouble(100.0)
          case "percentHeated" => PosDouble(100.0)
          case "percentCooled" => PosDouble(100.0)
        }
      }
      case "Retail" => {
        parameter match {
          case "weeklyOperatingHours" => PosDouble(65.0)
          case "numOpenClosedRefrCases" => PosDouble(0.0)
          case "numWalkinRefrUnits" => PosDouble(0.0)
          case "numCashRegisters" => PosDouble(roundAt(2)(0.3 * size / 1000))
          case "numWorkersMainShift" => PosDouble(roundAt(2)(1.0 * size / 1000))
          case "numComputers" => PosDouble(0.2 * size / 1000)
          case "percentHeated" => PosDouble(100.0)
          case "percentCooled" => PosDouble(100.0)
        }
      }
      case "SeniorCare" => {
        parameter match {
          case "maxNumResidents" => PosDouble(roundAt(2)(2.374 * size / 1000))
          case "avgNumResidents" => PosDouble(roundAt(2)(2.075 * size / 1000))
          case "numRezUnits" => PosDouble(roundAt(2)(1.584 * size / 1000))
          case "numWorkersMainShift" => PosDouble(roundAt(2)(0.9523 * size / 1000))
          case "numComputers" => PosDouble(roundAt(2)(0.367 * size / 1000))
          case "numRezWashingMachines" => PosDouble(roundAt(2)(0.05757 * size / 1000))
          case "numCommWashingMachines" => PosDouble(roundAt(2)(0.04422 * size / 1000))
          case "numElectronicLifts" => PosDouble(roundAt(2)(0.0704 * size / 1000))
          case "numRefrUnits" => PosDouble(roundAt(2)(0.09065 * size / 1000))
          case "percentHeated" => PosDouble(100.0)
          case "percentCooled" => PosDouble(100.0)
        }
      }
      case "Supermarket" => {
        parameter match {
          case "weeklyOperatingHours" => PosDouble(105.0)
          case "numWorkersMainShift" => PosDouble(roundAt(2)(1.0 * size / 1000))
          case "numWalkinRefrUnits" => PosDouble(roundAt(2)(0.25 * size / 1000))
          case "percentHeated" => PosDouble(100.0)
          case "percentCooled" => PosDouble(100.0)
        }
      }
      case "WastewaterCenter" => {
        parameter match {
          case "wastewaterInfluentBiologicalOxygenDemand" => PosDouble(200.0)
          case "wastewaterEffluentBiologicalOxygenDemand" => PosDouble(8.0)
          case "wastewaterAvgInfluentInflow" => null
          case "wastewaterPlantDesignFlowRate" => null
          case "wastewaterLoadFactor" => null
          case "numWorkersMainShift" => PosDouble(roundAt(2)(1.0 * size / 1000))
          case "numWalkinRefrUnits" => PosDouble(roundAt(2)(0.25 * size / 1000))
          case "percentHeated" => PosDouble(100.0)
          case "percentCooled" => PosDouble(100.0)
        }
      }
      case "WorshipCenter" => {
        parameter match {
          case "weeklyOperatingHours" => PosDouble(33.0)
          case "numWalkinRefrUnits" => PosDouble(0.0)
          case "seatingCapacity" => PosDouble(roundAt(2)(40.0 * size / 1000))
          case "numRefrUnits" => PosDouble(roundAt(2)(0.018 * size / 1000))
          case "numComputers" => PosDouble(roundAt(2)(0.2 * size / 1000))
          case "percentHeated" => PosDouble(100.0)
          case "percentCooled" => PosDouble(100.0)
        }
      }
      case "Warehouse" => {
        parameter match {
          case "weeklyOperatingHours" => PosDouble(60.0)
          case "numWorkersMainShift" => PosDouble(roundAt(2)(0.59 * size / 1000))
          case "numWalkinRefrUnits" => PosDouble(0.0)
          case "percentHeated" => PosDouble(50.0)
          case "percentCooled" => PosDouble(20.0)
        }
      }

      case "RefrigeratedWarehouse" => {
        parameter match {
          case "weeklyOperatingHours" => PosDouble(60.0)
          case "numWorkersMainShift" => PosDouble(roundAt(2)(0.59 * size / 1000))
          case "numWalkinRefrUnits" => PosDouble(0.0)
          case "percentHeated" => PosDouble(50.0)
          case "percentCooled" => PosDouble(20.0)
        }
      }
      case "CanadaHospital" => {
        val adjustedSize = size * 10.7639
        parameter match {
          case "licensedBedCapacity" => PosDouble(roundAt(2)(0.69 * adjustedSize / 1000))
          case "numWorkersMainShift" => PosDouble(roundAt(2)(1.32 * adjustedSize / 1000))
          case "percentHeated" => PosDouble(100.0)
          case "percentCooled" => PosDouble(100.0)
        }
      }

      case "CanadaK12School" => {
        val adjustedSize = size * 10.7639
        parameter match {
          case "gymFloorArea" => PosDouble(0.0)
          case "numWorkersMainShift" => PosDouble(roundAt(2)(0.77 * adjustedSize / 1000))
          case "studentSeatingCapacity" => PosDouble(roundAt(2)(10.0 * adjustedSize / 1000))
          case "percentHeated" => PosDouble(100.0)
          case "percentCooled" => PosDouble(100.0)
        }
      }
      case "CanadaMedicalOffice" => {
        val adjustedSize = size * 10.7639
        parameter match {
          case "weeklyOperatingHours" => PosDouble(65.0)
          case "numWorkersMainShift" => PosDouble(roundAt(2)(2.2 * adjustedSize / 1000))
          case "percentCooled" => PosDouble(100.0)
        }
      }
      case "CanadaOffice" => {
        val adjustedSize = size * 10.7639
        parameter match {
          case "weeklyOperatingHours" => PosDouble(65.0)
          case "numWorkersMainShift" => PosDouble(roundAt(2)(2.3 * adjustedSize / 1000))
          case "numComputers" => PosDouble(roundAt(2)(2.0 * adjustedSize / 1000))
          case "numServers" => PosDouble(0.0)
          case "percentHeated" => PosDouble(100.0)
          case "percentCooled" => PosDouble(100.0)
        }
      }
      case "CanadaSupermarket" => {
        val adjustedSize = size * 10.7639
        parameter match {
          case "weeklyOperatingHours" => PosDouble(105.0)
          case "numWorkersMainShift" => PosDouble(roundAt(2)(1.0 * adjustedSize / 1000))
          case "numCashRegisters" => PosDouble(roundAt(2)(0.38 * adjustedSize / 1000))
          case "numComputers" => PosDouble(roundAt(2)(0.51 * adjustedSize / 1000))
          case "lengthRefrFoodDisplayCases" => PosDouble(roundAt(2)(2.6 * adjustedSize / 1000))
          case "percentHeated" => PosDouble(100.0)
          case "percentCooled" => PosDouble(100.0)
        }
      }
    }
  }


  def fillBooleanDefaults(building: String, parameter: String): Option[Boolean] = {
    building match {
      case "Hotel" =>
        parameter match {
          case "hasFoodPreparation" => Some(false)
        }
      case "K12School" =>
        parameter match {
          case "isHighSchool" => Some(false)
          case "isOpenWeekends" => Some(false)
          case "hasCooking" => Some(true)
        }
      case "FinancialOffice" =>
        parameter match {
          case "isSmallBank" => {
            buildingSize match {
              case a if a < 5000 => Some(true)
              case _ => Some(false)
            }
          }
        }

      case "Supermarket" =>
        parameter match {
          case "hasCooking" => Some(true)
        }
      case "WastewaterCenter" =>
        parameter match {
          case "hasCooking" => Some(true)
          case "wastewaterHasTrickleFiltration" => Some(true)
          case "wastewaterHasNutrientRemoval" => Some(true)
        }
      case "WorshipCenter" =>
        parameter match {
          case "isOpenAllWeekdays" => Some(false)
          case "hasFoodPreparation" => Some(false)
        }
      case "Warehouse" =>
        parameter match {
          case "isWarehouseRefrigerated" => Some(false)
        }
      case "RefrigeratedWarehouse" =>
        parameter match {
          case "isWarehouseRefrigerated" => Some(true)
        }
      case "CanadaHospital" =>
        parameter match {
          case "hasLaundryFacility" => Some(true)
        }
      case "CanadaK12School" =>
        parameter match {
          case "isSecondarySchool" => Some(false)
        }
    }
  }
}

/**
  *
  * @param a Coefficient
  * @param b Centering Variable
  * @param c User Input
  */

case class RegressionSegment(a:Double, b:Double, c:Double) {
  def reduce:Double = {a * (c - b)}
}

case class StateBuildingType(state: String, buildingType: String)

object StateBuildingType {
  implicit val stateBuildingTypeRead: Reads[StateBuildingType] = Json.reads[StateBuildingType]
}


/**
  * Class to manage the decomposing of Country and Building from the input JSON
  *
  * @param country country that the building resides in
  * @param buildingType type of building
  */
case class CountryBuildingType(country: String, buildingType: String)

object CountryBuildingType {
  implicit val countryBuildingTypeRead: Reads[CountryBuildingType] = Json.reads[CountryBuildingType]
}

case class GenericBuilding (GFA:PosDouble, areaUnits:String, country:String, buildingType:String,
                            propTypeName: Option[String],postalCode:String,
                            HDD: Option[Double], CDD: Option[Double]) extends BaseLine {

  def regressionSegments(HDD:Double,CDD:Double) = Seq[RegressionSegment]()

  val printed:String = {
    buildingType match {
      case "AdultEducation" => "Adult Education"
      case "College" => "College / University"
      case "PreSchool" => "Pre-school / DayCare"
      case "VocationalSchool" => "Vocational School"
      case "OtherEducation" => "Other Education"
      case "ConventionCenter" => "Convention Center"
      case "MovieTheater" => "Movie Theater"
      case "Museum" => "Museum"
      case "PerformingArts" => "Performing Arts"
      case "BowlingAlley" => "Bowling Alley"
      case "FitnessCenter" => "Fitness Center"
      case "IceRink" => "Ice / Curling Rink"
      case "RollerRink" => "Roller Rink"
      case "SwimmingPool" => "Swimming Pool"
      case "OtherRecreation" => "Other Recreation"
      case "Stadium" => "Stadium"
      case "IndoorArena" => "Indoor Arena"
      case "RaceTrack" => "Race Track"
      case "Aquarium" => "Aquarium"
      case "Bar" => "Bar"
      case "Nightclub" => "Nightclub"
      case "Casino" => "Casino"
      case "Zoo" => "Zoo"
      case "OtherEntertainment" => "Other Entertainment"
      case "GasStation" => "Convenience Store with Gas Station"
      case "ConvenienceStore" => "Convenience Store without Gas Station"
      case "FastFoodRestaurant" => "Fast Food Restaurant"
      case "Restaurant" => "Restaurant"
      case "Supermarket" => "Supermarket"
      case "WholesaleClub" => "Wholesale Club"
      case "FoodSales" => "Food Sales"
      case "FoodService" => "Food Service"
      case "AmbulatorySurgicalCenter" => "Ambulatory Surgical Center"
      case "Hospital" => "Hospital"
      case "SpecialtyHospital" => "Specialty Hospital"
      case "MedicalOffice" => "Medical Office"
      case "OutpatientCenter" => "Outpatient Rehabilitation Center"
      case "PhysicalTherapyCenter" => "Physical Therapy Center"
      case "SeniorCare" => "Senior Care Community"
      case "UrgentCareCenter" => "Urgent Care Center"
      case "Barracks" => "Barracks"
      case "Hotel" => "Hotel"
      case "MultiFamily" => "Multifamily Housing"
      case "Prison" => "Prison / Incarceration"
      case "ResidenceHall" => "Residence Hall"
      case "ResidentialLodging" => "Other Residential Lodging"
      case "MixedUse" => "Mixed Use Property"
      case "Office" => "Office"
      case "VeterinaryOffice" => "Veterinary Office"
      case "Courthouse" => "Courthouse"
      case "DrinkingWaterTreatment" => "Drinking Water Treatment Center"
      case "FireStation" => "Fire Station"
      case "Library" => "Library"
      case "PostOffice" => "Post Office"
      case "PoliceStation" => "Police Station"
      case "MeetingHall" => "Meeting Hall"
      case "TransportationTerminal" => "Transportation Terminal"
      case "WastewaterCenter" => "Wastewater Treatment Center"
      case "OtherPublicServices" => "Other Public Services"
      case "WorshipCenter" => "Worship Facility"
      case "AutoDealership" => "Automobile Dealership"
      case "EnclosedMall" => "Enclosed Mall"
      case "StripMall" => "Strip Mall"
      case "Retail" => "Retail Store"
      case "DataCenter" => "Data Center" //Data Centers behave very different and require custom script
      case "PersonalServices" => "Personal Services (Health/Beauty, Dry Cleaning, etc.)"
      case "RepairServices" => "Repair Services (Vehicle, Shoe Locksmith, etc.)"
      case "OtherServices" => "Other Services"
      case "PowerStation" => "Energy / Power Station"
      case "OtherUtility" => "Other Utility Station"
      case "SelfStorageFacility" => "Self Storage Facility"
      case "Warehouse" => "Warehouse / Distribution Center"
      case "RefrigeratedWarehouse" => "Refrigerated Warehouse"
      case "SingleFamilyDetached" => "Single Family - Detached"
      case "SingleFamilyAttached" => "Single Family - Attached"
      case "MobileHome" => "Mobile Home"
      case _ => "Other"
    }
  }



}
object GenericBuilding {
  implicit val genericBuildingTypeRead: Reads[GenericBuilding] = Json.reads[GenericBuilding]
}

case class Parking(openParkingArea: Option[PosDouble], partiallyEnclosedParkingArea: Option[PosDouble],
                   fullyEnclosedParkingArea: Option[PosDouble], hasParkingHeating: Option[Boolean],
                   areaUnits: String, country: Option[String], buildingType: String, totalParkingArea: Option[PosDouble],
                   postalCode:String) {

  val degreeDays = PostalDegreeDays(postalCode)

  implicit def boolOptToInt(b: Option[Boolean]): Int = if (b.getOrElse(false)) 1 else 0

  val areaConvert: Double = areaUnits match {
    case "ftSQ" => 1
    case "mSQ" => SquareMeters(1) to SquareFeet
  }

  val printed: String = "Parking"

}

object Parking {
  implicit val parkingReads: Reads[Parking] = Json.reads[Parking]
}

case class CanadaParking(openParkingArea: PosDouble, partiallyEnclosedParkingArea: PosDouble,
                         fullyEnclosedParkingArea: PosDouble, HDD: PosDouble, hasParkingHeating: Option[Boolean],
                         areaUnits: String, country: String, totalParkingArea: PosDouble) {

  implicit def boolOptToInt(b: Option[Boolean]): Int = if (b.getOrElse(false)) 1 else 0

  val areaConvert: Double = areaUnits match {
    case "ftSQ" => 1
    case "mSQ" => SquareMeters(1) to SquareFeet
  }
  val energyConvert: Double = (KBtus(1) to Gigajoules)

  def energyReduce: Double = regressionSegments.map(_.reduce).sum

  def expectedEnergy = energyReduce

  val printed: String = "Parking"
  val regressionSegments = Seq[RegressionSegment](
    RegressionSegment(6.128, 0, openParkingArea.value * areaConvert * energyConvert),
    RegressionSegment(18.38, 0, partiallyEnclosedParkingArea.value * areaConvert * energyConvert),
    RegressionSegment(23.28, 0, fullyEnclosedParkingArea.value * areaConvert * energyConvert),
    RegressionSegment(0.009451, 0, HDD.value * hasParkingHeating * fullyEnclosedParkingArea.value * areaConvert * energyConvert)
  )

}

object CanadaParking {
  implicit val parkingReads: Reads[CanadaParking] = Json.reads[CanadaParking]
}



/**
  *   Building Type parameters
  *
  * @param weeklyOperatingHours
  * @param numWorkersMainShift
  * @param numComputers
  * @param percentHeated
  * @param percentCooled
  * @param isSmallBank  "if is bank branch or financial office AND < 50,000 sq ft in area"
  * @param GFA
  * @param areaUnits
  */

case class Office(GFA:PosDouble, areaUnits:String, country:String, buildingType:String, postalCode:String,
                  propTypeName: Option[String],
                  HDD: Option[Double], CDD: Option[Double],
                  numComputers:Option[PosDouble],
                  weeklyOperatingHours: Option[PosDouble],
                  percentHeated:Option[PosDouble],
                  percentCooled:Option[PosDouble],
                  isSmallBank:Option[Boolean],
                  numWorkersMainShift:Option[PosDouble]) extends BaseLine {



  val printed:String = "Office"
  def regressionSegments(HDD:Double, CDD:Double):Seq[RegressionSegment] = {
    Seq[RegressionSegment] (
      RegressionSegment(186.6, 0, 1), // regression constant
      RegressionSegment(34.17, 9.535, math.min(log(buildingSize),log(200000))),
      RegressionSegment(17.28, 2.231, math.min(numComputers.getOrElse(fillPosDoubleDefaults("Office","numComputers",buildingSize)).value / buildingSize * 1000, 11.1)),
      RegressionSegment(55.96, 3.972, getLog(weeklyOperatingHours.getOrElse(fillPosDoubleDefaults("Office","weeklyOperatingHours",buildingSize)).value)),
      RegressionSegment(10.34, 0.5616, getLog(numWorkersMainShift.getOrElse(fillPosDoubleDefaults("Office","numWorkersMainShift",buildingSize)).value / buildingSize * 1000)),
      RegressionSegment(0.0077, 4411, HDD * percentHeated.getOrElse(fillPosDoubleDefaults("Office","percentHeated",buildingSize)).value / 100),
      RegressionSegment(0.0144, 1157, CDD * percentCooled.getOrElse(fillPosDoubleDefaults("Office","percentCooled",buildingSize)).value / 100),
      RegressionSegment(0, 9.535, math.min(log(buildingSize),log(200000))),
      RegressionSegment(0, .5616, getLog(numWorkersMainShift.getOrElse(fillPosDoubleDefaults("Office","numWorkersMainShift",buildingSize)).value / buildingSize * 1000)),
      RegressionSegment(0, 0, 1)
    )
  }
}
/**
  * Office companion object.  Contains built in JSON validation.
  */
object Office {
  implicit val officeReads: Reads[Office] = Json.reads[Office]
}

case class FinancialOffice(GFA:PosDouble, areaUnits:String, country:String, buildingType:String, postalCode:String,
                           propTypeName: Option[String],
                           HDD: Option[Double], CDD: Option[Double],
                           numComputers:Option[PosDouble],
                           weeklyOperatingHours: Option[PosDouble],
                           percentHeated:Option[PosDouble],
                           percentCooled:Option[PosDouble],
                           isSmallBank:Option[Boolean],
                           numWorkersMainShift:Option[PosDouble]) extends BaseLine {

  val printed:String = "Financial Office"
  def regressionSegments(HDD:Double, CDD:Double):Seq[RegressionSegment] = Seq[RegressionSegment] (
    RegressionSegment(186.6, 0, 1), // regression constant
    RegressionSegment(34.17, 9.535, math.min(log(buildingSize),log(200000))),
    RegressionSegment(17.28, 2.231, math.min(numComputers.getOrElse(fillPosDoubleDefaults("FinancialOffice","numComputers",buildingSize)).value / buildingSize * 1000, 11.1)),
    RegressionSegment(55.96, 3.972, getLog(weeklyOperatingHours.getOrElse(fillPosDoubleDefaults("FinancialOffice","weeklyOperatingHours",buildingSize)).value)),
    RegressionSegment(10.34, 0.5616, getLog(numWorkersMainShift.getOrElse(fillPosDoubleDefaults("FinancialOffice","numWorkersMainShift",buildingSize)).value / buildingSize * 1000)),
    RegressionSegment(0.0077, 4411, HDD * percentHeated.getOrElse(fillPosDoubleDefaults("FinancialOffice","percentHeated",buildingSize)).value / 100),
    RegressionSegment(0.0144, 1157, CDD * percentCooled.getOrElse(fillPosDoubleDefaults("FinancialOffice","percentCooled",buildingSize)).value / 100),
    RegressionSegment(-64.83*isSmallBank.getOrElse(fillBooleanDefaults("FinancialOffice","isSmallBank").get), 9.535, math.min(log(buildingSize),log(200000))),
    RegressionSegment(34.2*isSmallBank.getOrElse(fillBooleanDefaults("FinancialOffice","isSmallBank").get), .5616, getLog(numWorkersMainShift.getOrElse(fillPosDoubleDefaults("FinancialOffice","numWorkersMainShift",buildingSize)).value / buildingSize * 1000)),
    RegressionSegment(56.3*isSmallBank.getOrElse(fillBooleanDefaults("FinancialOffice","isSmallBank").get), 0, 1)
  )
}
/**
  * Office companion object.  Contains built in JSON validation.
  */
object FinancialOffice {
  implicit val officeReads: Reads[FinancialOffice] = Json.reads[FinancialOffice]
}

/**
  * Canada Office
  *
  * @param weeklyOperatingHours
  * @param numWorkersMainShift
  * @param numComputers
  * @param percentCooled
  * @param numServers
  * @param GFA
  * @param areaUnits
  */

case class CanadaOffice(GFA:PosDouble, areaUnits:String, country:String, buildingType:String, postalCode:String,
                        propTypeName: Option[String],
                        HDD: Option[Double], CDD: Option[Double],
                        weeklyOperatingHours:Option[PosDouble],
                        numWorkersMainShift:Option[PosDouble],
                        numComputers:Option[PosDouble],
                        percentCooled:Option[PosDouble],
                        numServers:Option[PosDouble]) extends BaseLine {



  val printed:String = "Office"
  def regressionSegments(HDD:Double, CDD:Double):Seq[RegressionSegment] = Seq[RegressionSegment] (
    RegressionSegment(1.788, 0, 1), // regression constant
    RegressionSegment(.006325, 57.95, weeklyOperatingHours.getOrElse(fillPosDoubleDefaults("CanadaOffice","weeklyOperatingHours",buildingSize)).value),
    RegressionSegment(.06546, 3.492, numWorkersMainShift.getOrElse(fillPosDoubleDefaults("CanadaOffice","numWorkersMainShift",buildingSize)).value / buildingSize * 100),
    RegressionSegment(.07455, 3.335, (numComputers.getOrElse(fillPosDoubleDefaults("CanadaOffice","numComputers",buildingSize)).value + numServers.getOrElse(fillPosDoubleDefaults("CanadaOffice","numServers",buildingSize)).value) / buildingSize * 100),
    RegressionSegment(.3643, 7.36, log(math.min(buildingSize,5000))), // buildingSize capped @ 5,000 sq meters during analysis
    RegressionSegment(-0.00002596, 2933, math.min(buildingSize,5000)), // buildingSize capped @ 5,000 sq meters during analysis
    RegressionSegment(.0002034, 4619, HDD),
    RegressionSegment(.06386, 3.703, log(CDD) * percentCooled.getOrElse(fillPosDoubleDefaults("CanadaOffice","percentCooled",buildingSize)).value / 100)
  )
}

/**
  * Canada CanadaOffice companion object.  Contains built in JSON validation.
  */
object CanadaOffice {
  implicit val canadaOfficeReads: Reads[CanadaOffice] = Json.reads[CanadaOffice]
}

/**
  *
  * @param weeklyOperatingHours
  * @param seatingCapacity
  * @param numComputers
  * @param numRefrUnits
  * @param GFA
  * @param hasFoodPreparation
  * @param isOpenAllWeekdays
  * @param areaUnits
  */

case class WorshipCenter(GFA:PosDouble, areaUnits:String, country:String, buildingType:String, postalCode:String,
                         propTypeName: Option[String],
                         HDD: Option[Double], CDD: Option[Double],
                         weeklyOperatingHours:Option[PosDouble],
                         seatingCapacity:Option[PosDouble],
                         numComputers:Option[PosDouble],
                         numRefrUnits:Option[PosDouble],
                         hasFoodPreparation:Option[Boolean],
                         isOpenAllWeekdays:Option[Boolean]) extends BaseLine {



  val printed:String = "Worship Center"
  def regressionSegments(HDD:Double, CDD:Double):Seq[RegressionSegment] = Seq[RegressionSegment] (
    RegressionSegment(73.91, 0, 1), // regression constant
    RegressionSegment(0.6532, 38.81, seatingCapacity.getOrElse(fillPosDoubleDefaults("WorshipCenter","seatingCapacity",buildingSize)).value / buildingSize * 1000),
    RegressionSegment(19.14 * isOpenAllWeekdays.getOrElse(fillBooleanDefaults("WorshipCenter","isOpenAllWeekdays").get), 0, 1),
    RegressionSegment(.2717, 33.28, weeklyOperatingHours.getOrElse(fillPosDoubleDefaults("WorshipCenter","weeklyOperatingHours",buildingSize)).value),
    RegressionSegment(26.55, 0.2036, numComputers.getOrElse(fillPosDoubleDefaults("WorshipCenter","numComputers",buildingSize)).value / buildingSize * 1000),
    RegressionSegment(15.83 * hasFoodPreparation.getOrElse(fillBooleanDefaults("WorshipCenter","hasFoodPreparation").get), 0, 1),
    RegressionSegment(113.1, 0.0183, numRefrUnits.getOrElse(fillPosDoubleDefaults("WorshipCenter","numRefrUnits",buildingSize)).value / buildingSize * 1000),
    RegressionSegment(0.0081, 4523, HDD),
    RegressionSegment(.0141, 1313, CDD)
  )
}

/**
  * WorshipCenter companion object.  Contains built in JSON validation.
  */
object WorshipCenter {
  implicit val worshipCenterReads: Reads[WorshipCenter] = Json.reads[WorshipCenter]
}

/**
  *
  * @param wastewaterAvgInfluentInflow
  * @param wastewaterInfluentBiologicalOxygenDemand
  * @param wastewaterEffluentBiologicalOxygenDemand
  * @param wastewaterPlantDesignFlowRate
  * @param wastewaterHasTrickleFiltration
  * @param wastewaterHasNutrientRemoval
  * @param wastewaterLoadFactor
  * @param GFA
  * @param areaUnits
  */
case class WastewaterCenter(GFA:PosDouble, areaUnits:String, country:String, buildingType:String, postalCode:String,
                            propTypeName: Option[String],
                            HDD: Option[Double], CDD: Option[Double],
                            wastewaterAvgInfluentInflow:Option[PosDouble],
                            wastewaterInfluentBiologicalOxygenDemand:Option[PosDouble],
                            wastewaterEffluentBiologicalOxygenDemand:Option[PosDouble],
                            wastewaterPlantDesignFlowRate:Option[PosDouble],
                            wastewaterHasTrickleFiltration:Option[Boolean],
                            wastewaterHasNutrientRemoval:Option[Boolean],
                            wastewaterLoadFactor:Option[PosDouble]) extends BaseLine {



  // for predicted EUI you do not divide by GFA, you divide by average influent flow in Gallons per Day

  val printed:String = "Wastewater Center"
  def regressionSegments(HDD:Double, CDD:Double):Seq[RegressionSegment] = Seq[RegressionSegment] (
    RegressionSegment(10.13, 0, 1), // regression constant
    RegressionSegment(-0.9421, 1.863, getLog(wastewaterAvgInfluentInflow.getOrElse(fillPosDoubleDefaults("WastewaterCenter","wastewaterAvgInfluentInflow",buildingSize)).value)),
    RegressionSegment(4.876, 5.204, getLog(wastewaterInfluentBiologicalOxygenDemand.getOrElse(fillPosDoubleDefaults("WastewaterCenter","wastewaterInfluentBiologicalOxygenDemand",buildingSize)).value)),
    RegressionSegment(-2.082, 1.656, getLog(wastewaterEffluentBiologicalOxygenDemand.getOrElse(fillPosDoubleDefaults("WastewaterCenter","wastewaterEffluentBiologicalOxygenDemand",buildingSize)).value)),
    RegressionSegment(-4.668, 4.171, getLog(wastewaterLoadFactor.getOrElse(fillPosDoubleDefaults("WastewaterCenter","wastewaterLoadFactor",buildingSize)).value)),
    RegressionSegment(-2.577,0.179, 1 * wastewaterHasTrickleFiltration.getOrElse(fillBooleanDefaults("WastewaterCenter","wastewaterHasTrickleFiltration").get)),
    RegressionSegment(1.235, 0.4591, 1 * wastewaterHasNutrientRemoval.getOrElse(fillBooleanDefaults("WastewaterCenter","wastewaterHasNutrientRemoval").get)),
    RegressionSegment(2.355,8.724,getLog(HDD)),
    RegressionSegment(1.243, 6.5, getLog(CDD))
  )
}

/**
  * WastewaterCenter companion object.  Contains built in JSON validation.
  */
object WastewaterCenter {
  implicit val wastewaterCenterReads: Reads[WastewaterCenter] = Json.reads[WastewaterCenter]
}

/**
  *
  * @param weeklyOperatingHours
  * @param numWorkersMainShift
  * @param numWalkinRefrUnits
  * @param isWarehouseRefrigerated
  * @param percentHeated
  * @param percentCooled
  * @param GFA
  * @param areaUnits
  */
case class Warehouse(GFA:PosDouble, areaUnits:String, country:String, buildingType:String, postalCode:String,
                     propTypeName: Option[String],
                     HDD: Option[Double], CDD: Option[Double],
                     weeklyOperatingHours:Option[PosDouble],
                     numWorkersMainShift:Option[PosDouble],
                     numWalkinRefrUnits:Option[PosDouble],
                     isWarehouseRefrigerated:Option[Boolean],
                     percentHeated:Option[PosDouble],
                     percentCooled:Option[PosDouble]) extends BaseLine {




  val printed:String = "Warehouse"
  def regressionSegments(HDD:Double, CDD:Double):Seq[RegressionSegment] = Seq[RegressionSegment] (
    RegressionSegment(82.18, 0, 1), // regression constant
    RegressionSegment(168.6 * isWarehouseRefrigerated.getOrElse(fillBooleanDefaults("Warehouse","isWarehouseRefrigeratedgetOrElse").get), 0, 1),
    RegressionSegment(13.63, 9.806, log(buildingSize)),
    RegressionSegment(41.84, 0.5943, numWorkersMainShift.getOrElse(fillPosDoubleDefaults("Warehouse","numWorkersMainShift",buildingSize)).value * 1000 / buildingSize),
    RegressionSegment(0.3111, 60.93, weeklyOperatingHours.getOrElse(fillPosDoubleDefaults("Warehouse","weeklyOperatingHours",buildingSize)).value),
    RegressionSegment(0.0708 * isWarehouseRefrigerated.getOrElse(fillBooleanDefaults("Warehouse","isWarehouseRefrigeratedgetOrElse").get),1570,CDD),
    RegressionSegment(0.011 * converseBoolean(isWarehouseRefrigerated),2707,HDD * percentHeated.getOrElse(fillPosDoubleDefaults("Warehouse","percentHeated",buildingSize)).value / 100),
    RegressionSegment(.0205 * converseBoolean(isWarehouseRefrigerated), 378.7, CDD * percentCooled.getOrElse(fillPosDoubleDefaults("Warehouse","percentCooled",buildingSize)).value / 100),
    RegressionSegment(262.3 * converseBoolean(isWarehouseRefrigerated), 0.0096, numWalkinRefrUnits.getOrElse(fillPosDoubleDefaults("Warehouse","numWalkinRefrUnits",buildingSize)).value * 1000 / buildingSize )
  )
}

/**
  * Warehouse companion object.  Contains built in JSON validation.
  */
object Warehouse {
  implicit val warehouseReads: Reads[Warehouse] = Json.reads[Warehouse]
}

case class RefrigeratedWarehouse(GFA:PosDouble, areaUnits:String, country:String, buildingType:String, postalCode:String,
                                 propTypeName: Option[String],
                                 HDD: Option[Double], CDD: Option[Double],
                                 weeklyOperatingHours:Option[PosDouble],
                                 numWorkersMainShift:Option[PosDouble],
                                 numWalkinRefrUnits:Option[PosDouble],
                                 isWarehouseRefrigerated:Option[Boolean],
                                 percentHeated:Option[PosDouble],
                                 percentCooled:Option[PosDouble]) extends BaseLine {

  val printed:String = "RefrigeratedWarehouse"
  def regressionSegments(HDD:Double, CDD:Double):Seq[RegressionSegment] = Seq[RegressionSegment] (
    RegressionSegment(82.18, 0, 1), // regression constant
    RegressionSegment(168.6 * isWarehouseRefrigerated.getOrElse(fillBooleanDefaults("RefrigeratedWarehouse","isWarehouseRefrigeratedgetOrElse").get), 0, 1),
    RegressionSegment(13.63, 9.806, log(buildingSize)),
    RegressionSegment(41.84, 0.5943, numWorkersMainShift.getOrElse(fillPosDoubleDefaults("RefrigeratedWarehouse","numWorkersMainShift",buildingSize)).value * 1000 / buildingSize),
    RegressionSegment(0.3111, 60.93, weeklyOperatingHours.getOrElse(fillPosDoubleDefaults("RefrigeratedWarehouse","weeklyOperatingHours",buildingSize)).value),
    RegressionSegment(0.0708 * isWarehouseRefrigerated.getOrElse(fillBooleanDefaults("RefrigeratedWarehouse","isWarehouseRefrigeratedgetOrElse").get),1570,CDD),
    RegressionSegment(0.011 * converseBoolean(isWarehouseRefrigerated),2707,HDD * percentHeated.getOrElse(fillPosDoubleDefaults("RefrigeratedWarehouse","percentHeated",buildingSize)).value / 100),
    RegressionSegment(.0205 * converseBoolean(isWarehouseRefrigerated), 378.7, CDD * percentCooled.getOrElse(fillPosDoubleDefaults("RefrigeratedWarehouse","percentCooled",buildingSize)).value / 100),
    RegressionSegment(262.3 * converseBoolean(isWarehouseRefrigerated), 0.0096, numWalkinRefrUnits.getOrElse(fillPosDoubleDefaults("RefrigeratedWarehouse","numWalkinRefrUnits",buildingSize)).value * 1000 / buildingSize )
  )
}

/**
  * Warehouse companion object.  Contains built in JSON validation.
  */
object RefrigeratedWarehouse {
  implicit val refrigeratedWarehouseReads: Reads[RefrigeratedWarehouse] = Json.reads[RefrigeratedWarehouse]
}

/**
  *
  * @param weeklyOperatingHours
  * @param numWorkersMainShift
  * @param numWalkinRefrUnits
  * @param hasCooking
  * @param percentHeated
  * @param percentCooled
  * @param GFA
  * @param areaUnits
  */
case class Supermarket(GFA:PosDouble, areaUnits:String, country:String, buildingType:String, postalCode:String,
                       propTypeName: Option[String],
                       HDD: Option[Double], CDD: Option[Double],
                       weeklyOperatingHours:Option[PosDouble],
                       numWorkersMainShift:Option[PosDouble],
                       numWalkinRefrUnits:Option[PosDouble],
                       hasCooking:Option[Boolean],
                       percentHeated:Option[PosDouble],
                       percentCooled:Option[PosDouble]) extends BaseLine {


  val printed:String = "Supermarket"
  def regressionSegments(HDD:Double, CDD:Double):Seq[RegressionSegment] = Seq[RegressionSegment] (
    RegressionSegment(581.1, 0, 1), // regression constant
    RegressionSegment(84.97, 9.679, getLog(buildingSize)),
    RegressionSegment(115.6, -0.1084, getLog(numWorkersMainShift.getOrElse(fillPosDoubleDefaults("Supermarket","numWorkersMainShift",buildingSize)).value * 1000 / buildingSize)),
    RegressionSegment(125.8, 4.657, getLog(weeklyOperatingHours.getOrElse(fillPosDoubleDefaults("Supermarket","weeklyOperatingHours",buildingSize)).value)),
    RegressionSegment(794.4, 0.2345, numWalkinRefrUnits.getOrElse(fillPosDoubleDefaults("Supermarket","numWalkinRefrUnits",buildingSize)).value * 1000 / buildingSize ),
    RegressionSegment(902.8, .0254, hasCooking.getOrElse(fillBooleanDefaults("Supermarket","hasCooking").get) * 1000 / buildingSize ),
    RegressionSegment(.0947, 1219, CDD * percentCooled.getOrElse(fillPosDoubleDefaults("Supermarket","percentCooled",buildingSize)).value / 100),
    RegressionSegment(0.0326, 3510, HDD * percentHeated.getOrElse(fillPosDoubleDefaults("Supermarket","percentHeated",buildingSize)).value / 100)
  )
}

/**
  * Supermarket companion object.  Contains built in JSON validation.
  */
object Supermarket {
  implicit val supermarketReads: Reads[Supermarket] = Json.reads[Supermarket]
}

/**
  *
  * @param weeklyOperatingHours
  * @param numWorkersMainShift
  * @param lengthRefrFoodDisplayCases
  * @param numComputers
  * @param numCashRegisters
  * @param GFA
  * @param areaUnits
  */

case class CanadaSupermarket(GFA:PosDouble, areaUnits:String, country:String, buildingType:String, postalCode:String,
                             propTypeName: Option[String],
                             HDD: Option[Double], CDD: Option[Double],
                             weeklyOperatingHours:Option[PosDouble],
                             numWorkersMainShift:Option[PosDouble],
                             lengthRefrFoodDisplayCases:Option[PosDouble],
                             numComputers:Option[PosDouble],
                             numCashRegisters:Option[PosDouble]) extends BaseLine {



  val printed:String = "Supermarket"
  def regressionSegments(HDD:Double, CDD:Double):Seq[RegressionSegment] = Seq[RegressionSegment] (
    RegressionSegment(4.828, 0, 1), // regression constant
    RegressionSegment(0.001342, 1038, math.min(buildingSize,2500)),
    RegressionSegment(1.612, 1.802, math.min(math.max(numWorkersMainShift.getOrElse(fillPosDoubleDefaults("CanadaSupermarket","numWorkersMainShift",buildingSize)).value * 100 / buildingSize, 0.4490),3.687)),
    RegressionSegment(1.35, 0.3955, numCashRegisters.getOrElse(fillPosDoubleDefaults("CanadaSupermarket","numCashRegisters",buildingSize)).value * 100 / buildingSize),
    RegressionSegment(0.698, 0.5244, numComputers.getOrElse(fillPosDoubleDefaults("CanadaSupermarket","numComputers",buildingSize)).value * 100 / buildingSize),
    RegressionSegment(0.08314, 2.827, lengthRefrFoodDisplayCases.getOrElse(fillPosDoubleDefaults("CanadaSupermarket","lengthRefrFoodDisplayCases",buildingSize)).value * 100 / buildingSize),
    RegressionSegment(0.0004642, 4798, HDD)
  )
}

/**
  * CanadaSupermarket companion object.  Contains built in JSON validation.
  */
object CanadaSupermarket {
  implicit val canadaSupermarketReads: Reads[CanadaSupermarket] = Json.reads[CanadaSupermarket]
}

/**
  *
  * @param avgNumResidents
  * @param maxNumResidents
  * @param numRezUnits
  * @param numElectronicLifts
  * @param numWorkersMainShift
  * @param numComputers
  * @param numRefrUnits
  * @param numCommWashingMachines
  * @param numRezWashingMachines
  * @param percentHeated
  * @param percentCooled
  * @param GFA
  * @param areaUnits
  */

case class SeniorCare(GFA:PosDouble, areaUnits:String, country:String, buildingType:String, postalCode:String,
                      propTypeName: Option[String],
                      HDD: Option[Double], CDD: Option[Double],
                      avgNumResidents:Option[PosDouble],
                      maxNumResidents:Option[PosDouble],
                      numRezUnits:Option[PosDouble],
                      numElectronicLifts:Option[PosDouble],
                      numWorkersMainShift:Option[PosDouble],
                      numComputers:Option[PosDouble],
                      numRefrUnits:Option[PosDouble],
                      numCommWashingMachines:Option[PosDouble],
                      numRezWashingMachines:Option[PosDouble],
                      percentHeated:Option[PosDouble],
                      percentCooled:Option[PosDouble]) extends BaseLine {



  val printed:String = "Senior Care Center"
  def regressionSegments(HDD:Double, CDD:Double):Seq[RegressionSegment] = Seq[RegressionSegment] (
    RegressionSegment(253, 0, 1), // regression constant
    RegressionSegment(24.1, 1.582, numRezUnits.getOrElse(fillPosDoubleDefaults("SeniorCare","numRezUnits",buildingSize)).value * 1000 / buildingSize),
    RegressionSegment(0.9156, 87.61, avgNumResidents.getOrElse(fillPosDoubleDefaults("SeniorCare","avgNumResidents",buildingSize)).value/maxNumResidents.getOrElse(fillPosDoubleDefaults("SeniorCare","maxNumResidents",buildingSize)).value * 100),
    RegressionSegment(256.5, 0.0692, numElectronicLifts.getOrElse(fillPosDoubleDefaults("SeniorCare","numElectronicLifts",buildingSize)).value * 1000 / buildingSize),
    RegressionSegment(35.42, 0.937, numWorkersMainShift.getOrElse(fillPosDoubleDefaults("SeniorCare","numWorkersMainShift",buildingSize)).value * 1000 / buildingSize),
    RegressionSegment(90.3, 0.3636, numComputers.getOrElse(fillPosDoubleDefaults("SeniorCare","numComputers",buildingSize)).value * 1000 / buildingSize),
    RegressionSegment(251.5, 0.0905, numRefrUnits.getOrElse(fillPosDoubleDefaults("SeniorCare","numRefrUnits",buildingSize)).value * 1000 / buildingSize),
    RegressionSegment(378.2, 0.0432, numCommWashingMachines.getOrElse(fillPosDoubleDefaults("SeniorCare","numCommWashingMachines",buildingSize)).value * 1000 / buildingSize),
    RegressionSegment(253, 0.0584, numRezWashingMachines.getOrElse(fillPosDoubleDefaults("SeniorCare","numRezWashingMachines",buildingSize)).value * 1000 / buildingSize),
    RegressionSegment(0.02004, 1184, CDD * percentCooled.getOrElse(fillPosDoubleDefaults("SeniorCare","percentCooled",buildingSize)).value/100),
    RegressionSegment(0.005879, 4524, HDD * percentHeated.getOrElse(fillPosDoubleDefaults("SeniorCare","percentHeated",buildingSize)).value/100)

  )
}

/**
  * SeniorCare companion object.  Contains built in JSON validation.
  */
object SeniorCare {
  implicit val seniorCareReads: Reads[SeniorCare] = Json.reads[SeniorCare]
}

/**
  *
  * @param weeklyOperatingHours
  * @param numOpenClosedRefrCases
  * @param numCashRegisters
  * @param numWorkersMainShift
  * @param numComputers
  * @param numWalkinRefrUnits
  * @param percentHeated
  * @param percentCooled
  * @param GFA
  * @param areaUnits
  */

case class Retail(GFA:PosDouble, areaUnits:String, country:String, buildingType:String, postalCode:String,
                  propTypeName: Option[String],
                  HDD: Option[Double], CDD: Option[Double],
                  weeklyOperatingHours:Option[PosDouble],
                  numOpenClosedRefrCases:Option[PosDouble],
                  numCashRegisters:Option[PosDouble],
                  numWorkersMainShift:Option[PosDouble],
                  numComputers:Option[PosDouble],
                  numWalkinRefrUnits:Option[PosDouble],
                  percentHeated:Option[PosDouble],
                  percentCooled:Option[PosDouble]) extends BaseLine {



  val printed:String = "Retail Space"
  def regressionSegments(HDD:Double, CDD:Double):Seq[RegressionSegment] = Seq[RegressionSegment] (
    RegressionSegment(153.1, 0, 1), // regression constant
    RegressionSegment(20.19, 9.371, getLog(buildingSize)),
    RegressionSegment(1.373, 63.74, weeklyOperatingHours.getOrElse(fillPosDoubleDefaults("Retail","weeklyOperatingHours",buildingSize)).value),
    RegressionSegment(61.76, 0.6279, numWorkersMainShift.getOrElse(fillPosDoubleDefaults("Retail","numWorkersMainShift",buildingSize)).value * 1000 / buildingSize),
    RegressionSegment(70.6, 0.3149, numComputers.getOrElse(fillPosDoubleDefaults("Retail","numComputers",buildingSize)).value * 1000 / buildingSize),
    RegressionSegment(249.1, 0.1905, numCashRegisters.getOrElse(fillPosDoubleDefaults("Retail","numCashRegisters",buildingSize)).value * 1000 / buildingSize),
    RegressionSegment(720.2, 0.0038, numWalkinRefrUnits.getOrElse(fillPosDoubleDefaults("Retail","numWalkinRefrUnits",buildingSize)).value * 1000 / buildingSize),
    RegressionSegment(81.9, 0.045, numOpenClosedRefrCases.getOrElse(fillPosDoubleDefaults("Retail","numOpenClosedRefrCases",buildingSize)).value * 1000 / buildingSize),
    RegressionSegment(0.0125, 972.1, CDD * percentCooled.getOrElse(fillPosDoubleDefaults("Retail","percentCooled",buildingSize)).value/100),
    RegressionSegment(0.0113, 3811, HDD * percentHeated.getOrElse(fillPosDoubleDefaults("Retail","percentHeated",buildingSize)).value/100)

  )
}

/**
  * Retail companion object.  Contains built in JSON validation.
  */
object Retail {
  implicit val retailReads: Reads[Retail] = Json.reads[Retail]
}

/**
  *
  * @param numBedrooms
  * @param percentHeated
  * @param percentCooled
  * @param GFA
  * @param areaUnits
  */
case class ResidenceHall(GFA:PosDouble, areaUnits:String, country:String, buildingType:String, postalCode:String,
                         propTypeName: Option[String],
                         HDD: Option[Double], CDD: Option[Double],
                         numBedrooms:Option[PosDouble],
                         percentHeated:Option[PosDouble],
                         percentCooled:Option[PosDouble]) extends BaseLine {



  val printed:String = "Residence Hall"
  def regressionSegments(HDD:Double, CDD:Double):Seq[RegressionSegment] = Seq[RegressionSegment] (
    RegressionSegment(4.99455, 0, 1), // regression constant
    RegressionSegment(0.91309, 0, getLog(buildingSize)),
    RegressionSegment(0.09455, 0, getLog(numBedrooms.getOrElse(fillPosDoubleDefaults("ResidenceHall","numBedrooms",buildingSize)).value)),
    RegressionSegment(0.00009744, 0, HDD * percentHeated.getOrElse(fillPosDoubleDefaults("ResidenceHall","percentHeated",buildingSize)).value/100),
    RegressionSegment(0.00016279, 0, CDD * percentCooled.getOrElse(fillPosDoubleDefaults("ResidenceHall","percentCooled",buildingSize)).value/100)

  )
}

/**
  * ResidenceHall companion object.  Contains built in JSON validation.
  */
object ResidenceHall {
  implicit val residenceHallReads: Reads[ResidenceHall] = Json.reads[ResidenceHall]
}

/**
  *

  * @param numRezUnits
  * @param numBedrooms
  * @param numUnitsLowRise1to4
  * @param numRezUnits
  * @param HDD
  * @param CDD
  * @param GFA
  * @param areaUnits
  */


case class MultiFamily(GFA:PosDouble, areaUnits:String, country:String, buildingType:String, postalCode:String,
                       propTypeName: Option[String],
                       HDD: Option[Double], CDD: Option[Double],
                       numRezUnits:Option[PosDouble],
                       numBedrooms:Option[PosDouble],
                       numUnitsLowRise1to4: Option[PosDouble]) extends BaseLine {


  val printed:String = "MultiFamily Building"
  def regressionSegments(HDD:Double, CDD:Double):Seq[RegressionSegment] = Seq[RegressionSegment] (
    RegressionSegment(140.8, 0, 1), // regression constant
    RegressionSegment(52.57, 1.215, numRezUnits.getOrElse(fillPosDoubleDefaults("MultiFamily","numRezUnits",buildingSize)).value * 1000 / buildingSize),
    RegressionSegment(24.45, 1.238, numBedrooms.getOrElse(fillPosDoubleDefaults("MultiFamily","numBedrooms",buildingSize)).value/numRezUnits.getOrElse(fillPosDoubleDefaults("MultiFamily","numRezUnits",buildingSize)).value),
    RegressionSegment(-18.76, 0, numUnitsLowRise1to4.getOrElse(fillPosDoubleDefaults("MultiFamily","numUnitsLowRise1to4",buildingSize)).value/numRezUnits.getOrElse(fillPosDoubleDefaults("MultiFamily","numRezUnits",buildingSize)).value),
    RegressionSegment(0.009617, 4233, HDD),
    RegressionSegment(0.01617, 1364, CDD)
  )
}
/**
  * MultiFamily companion object.  Contains built in JSON validation.
  */
object MultiFamily {
  implicit val multiFamilyReads: Reads[MultiFamily] = Json.reads[MultiFamily]
}

/**
  *
  * @param weeklyOperatingHours
  * @param numWorkersMainShift
  * @param percentCooled
  * @param GFA
  * @param areaUnits
  */
case class CanadaMedicalOffice(GFA:PosDouble, areaUnits:String, country:String, buildingType:String, postalCode:String,
                               propTypeName: Option[String],
                               HDD: Option[Double], CDD: Option[Double],
                               weeklyOperatingHours:Option[PosDouble],
                               numWorkersMainShift:Option[PosDouble],
                               percentCooled:Option[PosDouble]) extends BaseLine {


  val printed:String = "Medical Office"
  val workerDensity:Double = numWorkersMainShift.getOrElse(fillPosDoubleDefaults("CanadaMedicalOffice","numWorkersMainShift",buildingSize)).value * 100 / buildingSize

  def regressionSegments(HDD:Double, CDD:Double):Seq[RegressionSegment] = Seq[RegressionSegment] (
    RegressionSegment(1.384, 0, 1), // regression constant
    RegressionSegment(0.00004511, 1635, math.min(buildingSize,20000.0)),
    RegressionSegment(0.007505, 58.94, weeklyOperatingHours.getOrElse(fillPosDoubleDefaults("CanadaMedicalOffice","weeklyOperatingHours",buildingSize)).value),
    RegressionSegment(0.2428, 2.466, math.min(max(workerDensity,0.3),7)),
    RegressionSegment(0.001297, 100.1, CDD * percentCooled.getOrElse(fillPosDoubleDefaults("CanadaMedicalOffice","percentCooled",buildingSize)).value/100),
    RegressionSegment(0.0002015, 4808, HDD)

  )
}

/**
  * CanadaMedicalOffice companion object.  Contains built in JSON validation.
  */
object CanadaMedicalOffice {
  implicit val canadaMedicalOfficeReads: Reads[CanadaMedicalOffice] = Json.reads[CanadaMedicalOffice]
}

/**
  *
  * @param weeklyOperatingHours
  * @param numWorkersMainShift
  * @param percentCooled
  * @param percentHeated
  * @param GFA
  * @param areaUnits
  */

case class MedicalOffice(GFA:PosDouble, areaUnits:String, country:String, buildingType:String, postalCode:String,
                         propTypeName: Option[String],
                         HDD: Option[Double], CDD: Option[Double],
                         weeklyOperatingHours:Option[PosDouble],
                         numWorkersMainShift:Option[PosDouble],
                         percentCooled:Option[PosDouble],
                         percentHeated:Option[PosDouble]) extends BaseLine {



  val printed:String = "Medical Office"
  def regressionSegments(HDD:Double, CDD:Double):Seq[RegressionSegment] = Seq[RegressionSegment] (
    RegressionSegment(2.78889, 0, 1), // regression constant
    RegressionSegment(0.91433, 0, getLog(buildingSize)),
    RegressionSegment(0.46768, 0, getLog(weeklyOperatingHours.getOrElse(fillPosDoubleDefaults("MedicalOffice","weeklyOperatingHours",buildingSize)).value)),
    RegressionSegment(0.21568, 0, getLog(numWorkersMainShift.getOrElse(fillPosDoubleDefaults("MedicalOffice","numWorkersMainShift",buildingSize)).value)),
    RegressionSegment(0.00020111, 0, CDD * percentCooled.getOrElse(fillPosDoubleDefaults("MedicalOffice","percentCooled",buildingSize)).value/100),
    RegressionSegment(0.00005321, 0, HDD * percentHeated.getOrElse(fillPosDoubleDefaults("MedicalOffice","percentHeated",buildingSize)).value/100)

  )
}

/**
  * MedicalOffice companion object.  Contains built in JSON validation.
  */
object MedicalOffice {
  implicit val medicalOfficeReads: Reads[MedicalOffice] = Json.reads[MedicalOffice]
}

/**
  *

  * @param numWorkersMainShift
  * @param gymFloorArea
  * @param studentSeatingCapacity
  * @param isSecondarySchool
  * @param percentHeated
  * @param percentCooled
  * @param GFA
  * @param areaUnits
  */

case class CanadaK12School(GFA:PosDouble, areaUnits:String, country:String, buildingType:String, postalCode:String,
                           propTypeName: Option[String],
                           HDD: Option[Double], CDD: Option[Double],
                           numWorkersMainShift:Option[PosDouble],
                           gymFloorArea:Option[PosDouble],
                           studentSeatingCapacity:Option[PosDouble],
                           isSecondarySchool:Option[Boolean],
                           percentHeated:Option[PosDouble],
                           percentCooled:Option[PosDouble]) extends BaseLine {

  val printed:String = "K-12 School"
  def regressionSegments(HDD:Double, CDD:Double):Seq[RegressionSegment] = Seq[RegressionSegment] (
    RegressionSegment(1.021, 0, 1), // regression constant
    RegressionSegment(0.2308 * isSecondarySchool.getOrElse(fillBooleanDefaults("CanadaK12School","isSecondarySchool").get), 0, 1),
    RegressionSegment(0.0304, 4.983, getLog(gymFloorArea.getOrElse(fillPosDoubleDefaults("CanadaK12School","gymFloorArea",buildingSize)).value)),
    RegressionSegment(0.0004402, 418.3, studentSeatingCapacity.getOrElse(fillPosDoubleDefaults("CanadaK12School","studentSeatingCapacity",buildingSize)).value),
    RegressionSegment(0.1218, 3.175, getLog(numWorkersMainShift.getOrElse(fillPosDoubleDefaults("CanadaK12School","numWorkersMainShift",buildingSize)).value)),
    RegressionSegment(-0.3942, 8.118, getLog(buildingSize)),
    RegressionSegment(0.0005647, 47.88, CDD * percentCooled.getOrElse(fillPosDoubleDefaults("CanadaK12School","percentCooled",buildingSize)).value/100),
    RegressionSegment(0.0001635, 4584, HDD * percentHeated.getOrElse(fillPosDoubleDefaults("CanadaK12School","percentHeated",buildingSize)).value/100)
  )
}
/**
  * CanadaK12School companion object.  Contains built in JSON validation.
  */
object CanadaK12School {
  implicit val canadaK12SchoolReads: Reads[CanadaK12School] = Json.reads[CanadaK12School]
}
/**
  *
  * @param isOpenWeekends
  * @param isHighSchool
  * @param hasCooking
  * @param numComputers
  * @param numWalkinRefrUnits
  * @param percentHeated
  * @param percentCooled
  * @param GFA
  * @param areaUnits
  */
case class K12School(GFA:PosDouble, areaUnits:String, country:String, buildingType:String, postalCode:String,
                     propTypeName: Option[String],
                     HDD: Option[Double], CDD: Option[Double],
                     isOpenWeekends:Option[Boolean],
                     isHighSchool:Option[Boolean],
                     hasCooking:Option[Boolean],
                     numComputers:Option[PosDouble],
                     numWalkinRefrUnits:Option[PosDouble],
                     percentHeated:Option[PosDouble],
                     percentCooled:Option[PosDouble]) extends BaseLine {



  val printed:String = "K-12 School"
  def regressionSegments(HDD:Double, CDD:Double):Seq[RegressionSegment] = Seq[RegressionSegment] (
    RegressionSegment(131.9, 0, 1), // regression constant
    RegressionSegment(4.377 * isHighSchool.getOrElse(fillBooleanDefaults("K12School","isHighSchool").get), 0, 1),
    RegressionSegment(8.974, 7.716, getLog(HDD) * percentHeated.getOrElse(fillPosDoubleDefaults("K12School","percentHeated",buildingSize)).value / 100),
    RegressionSegment(6.389, 5.045, getLog(CDD) * percentCooled.getOrElse(fillPosDoubleDefaults("K12School","percentCooled",buildingSize)).value / 100),
    RegressionSegment(-19.26, 10.2, getLog(buildingSize)),
    RegressionSegment(18.43 * isOpenWeekends.getOrElse(fillBooleanDefaults("K12School","isOpenWeekends").get), 0, 1),
    RegressionSegment(574.7, 0.0109, numWalkinRefrUnits.getOrElse(fillPosDoubleDefaults("K12School","numWalkinRefrUnits",buildingSize)).value / buildingSize * 1000),
    RegressionSegment(24.2 * hasCooking.getOrElse(fillBooleanDefaults("K12School","hasCooking").get), 0, 1),
    RegressionSegment(9.568, 1.742, numComputers.getOrElse(fillPosDoubleDefaults("K12School","numComputers",buildingSize)).value / buildingSize * 1000),


    //if High School also include the following
    RegressionSegment(0.00021 * isHighSchool.getOrElse(fillBooleanDefaults("K12School","isHighSchool").get), 47310, buildingSize),
    RegressionSegment(0.0285 * isHighSchool.getOrElse(fillBooleanDefaults("K12School","isHighSchool").get), 1316, CDD * percentCooled.getOrElse(fillPosDoubleDefaults("K12School","percentCooled",buildingSize)).value / 100),
    RegressionSegment(-11.75 * isHighSchool.getOrElse(fillBooleanDefaults("K12School","isHighSchool").get), 5.045, getLog(CDD) * percentCooled.getOrElse(fillPosDoubleDefaults("K12School","percentCooled",buildingSize)).value / 100)

  )
}

/**
  * K12School companion object.  Contains built in JSON validation.
  */
object K12School {
  implicit val K12SchoolReads: Reads[K12School] = Json.reads[K12School]
}


/**
  *
  * @param numBedrooms
  * @param hasFoodPreparation
  * @param numWorkersMainShift
  * @param numRefrUnits
  * @param percentHeated
  * @param percentCooled
  * @param GFA
  * @param areaUnits
  */

case class Hotel(GFA:PosDouble, areaUnits:String, country:String, buildingType:String, postalCode:String,
                 propTypeName: Option[String],
                 HDD: Option[Double], CDD: Option[Double],
                 numBedrooms:Option[PosDouble],
                 hasFoodPreparation:Option[Boolean],
                 numWorkersMainShift:Option[PosDouble],
                 numRefrUnits:Option[PosDouble],
                 percentHeated:Option[PosDouble],
                 percentCooled:Option[PosDouble]) extends BaseLine {



  val printed:String = "Hotel"
  def regressionSegments(HDD:Double, CDD:Double):Seq[RegressionSegment] = Seq[RegressionSegment] (
    RegressionSegment(169.1, 0, 1), // regression constant
    RegressionSegment(33.22, 1.951, numBedrooms.getOrElse(fillPosDoubleDefaults("Hotel","numBedrooms",buildingSize)).value * 1000 / buildingSize),
    RegressionSegment(20.81, -1.395, getLog(numWorkersMainShift.getOrElse(fillPosDoubleDefaults("Hotel","numWorkersMainShift",buildingSize)).value * 1000 / buildingSize)),
    RegressionSegment(65.14 * hasFoodPreparation.getOrElse(fillBooleanDefaults("Hotel","hasFoodPreparation").get), 0, 1),
    RegressionSegment(249.8, 0.0227, numRefrUnits.getOrElse(fillPosDoubleDefaults("Hotel","numRefrUnits",buildingSize)).value * 1000 / buildingSize),
    RegressionSegment(0.0169, 1224, CDD * percentCooled.getOrElse(fillPosDoubleDefaults("Hotel","percentCooled",buildingSize)).value/100),
    RegressionSegment(0.0107, 4120, HDD * percentHeated.getOrElse(fillPosDoubleDefaults("Hotel","percentHeated",buildingSize)).value/100)

  )
}

/**
  * Hotel companion object.  Contains built in JSON validation.
  */
object Hotel {
  implicit val hotelReads: Reads[Hotel] = Json.reads[Hotel]
}

/**
  *
  * @param numFTEWorkers
  * @param numStaffedBeds
  * @param numMRIMachines
  * @param GFA
  * @param areaUnits
  */

case class Hospital(GFA:PosDouble, areaUnits:String, country:String, buildingType:String, postalCode:String,
                    propTypeName: Option[String],
                    HDD: Option[Double], CDD: Option[Double],
                    numFTEWorkers:Option[PosDouble],
                    numStaffedBeds:Option[PosDouble],
                    numMRIMachines:Option[PosDouble]) extends BaseLine {



  val printed:String = "Hospital"
  def regressionSegments(HDD:Double, CDD:Double):Seq[RegressionSegment] = Seq[RegressionSegment] (
    RegressionSegment(484.8, 0, 1), // regression constant
    RegressionSegment(26.64, 2.6, numFTEWorkers.getOrElse(fillPosDoubleDefaults("Hospital","numFTEWorkers",buildingSize)).value * 1000 / buildingSize),
    RegressionSegment(120.3, 0.4636, numStaffedBeds.getOrElse(fillPosDoubleDefaults("Hospital","numStaffedBeds",buildingSize)).value * 1000 / buildingSize),
    RegressionSegment(8961, 0.0031, numMRIMachines.getOrElse(fillPosDoubleDefaults("Hospital","numMRIMachines",buildingSize)).value * 1000 / buildingSize),
    RegressionSegment(0.0227, 1392, CDD)

  )
}

/**
  * Hospital companion object.  Contains built in JSON validation.
  */
object Hospital {
  implicit val hospitalReads: Reads[Hospital] = Json.reads[Hospital]
}

/**
  *
  * @param weeklyOperatingHours
  * @param numWorkersMainShift
  * @param licensedBedCapacity
  * @param hasLaundryFacility
  * @param percentHeated
  * @param percentCooled
  * @param GFA
  * @param areaUnits
  */

case class CanadaHospital( GFA:PosDouble, areaUnits:String, country:String, buildingType:String, postalCode:String,
                           propTypeName: Option[String],
                           HDD: Option[Double], CDD: Option[Double],
                           weeklyOperatingHours:Option[PosDouble],
                           numWorkersMainShift:Option[PosDouble],
                           licensedBedCapacity:Option[PosDouble],
                           hasLaundryFacility:Option[Boolean],
                           percentHeated:Option[PosDouble],
                           percentCooled:Option[PosDouble]) extends BaseLine {

  val printed:String = "Hospital"
  def regressionSegments(HDD:Double, CDD:Double):Seq[RegressionSegment] = Seq[RegressionSegment] (
    RegressionSegment(2.984, 0, 1), // regression constant
    RegressionSegment(0.6092, 1.417, numWorkersMainShift.getOrElse(fillPosDoubleDefaults("CanadaHospital","numWorkersMainShift",buildingSize)).value * 100 / buildingSize),
    RegressionSegment(-0.0984, 2.726, numWorkersMainShift.getOrElse(fillPosDoubleDefaults("CanadaHospital","numWorkersMainShift",buildingSize)).value/licensedBedCapacity.getOrElse(fillPosDoubleDefaults("CanadaHospital","licensedBedCapacity",buildingSize)).value),
    RegressionSegment(0.4596 * hasLaundryFacility.getOrElse(fillBooleanDefaults("CanadaHospital","hasLaundryFacility").get), 0, 1),
    RegressionSegment(9.3598e-06, 19004, min(100000,buildingSize)),
    RegressionSegment(0.2775, 4.03, getLog(CDD+50) * percentCooled.getOrElse(fillPosDoubleDefaults("CanadaHospital","percentCooled",buildingSize)).value/100),
    RegressionSegment(0.00047986, 4787, HDD * percentHeated.getOrElse(fillPosDoubleDefaults("CanadaHospital","percentHeated",buildingSize)).value/100)

  )
}

/**
  * CanadaHospital companion object.  Contains built in JSON validation.
  */
object CanadaHospital {
  implicit val canadaHospitalReads: Reads[CanadaHospital] = Json.reads[CanadaHospital]
}


// Data Centers don't follow the same rules as other buildings, to include them need to expand code with PUE based analysis
case class DataCenter(reportingUnits:String, GFA:PosDouble, areaUnits:String, country:String, buildingType:String,
                      propTypeName: Option[String],postalCode:String,
                      annualITEnergy:PosDouble, HDD: Option[Double], CDD: Option[Double]) extends BaseLine {

  val printed:String = "Data Center"
  val siteToSourceITConvert: Double = country match {
    case "USA" => 3.14
    case _ => 2.05
  }

  val annualITEnergyTBtu: Double = (KilowattHours(annualITEnergy.value) to TBtus) * siteToSourceITConvert
  val annualITEnergyKBtu: Double = (KilowattHours(annualITEnergy.value) to KBtus) * siteToSourceITConvert

  //this results in expected energy, not EUI
  def regressionSegments(HDD:Double, CDD:Double):Seq[RegressionSegment] = Seq[RegressionSegment] (
    RegressionSegment(1.924, 0, 1), // regression constant
    RegressionSegment(-0.9506, 0.2091, annualITEnergyTBtu)
  )
}

/**
  * CanadaHospital companion object.  Contains built in JSON validation.
  */
object DataCenter {
  implicit val dataCenterReads: Reads[DataCenter] = Json.reads[DataCenter]
}

