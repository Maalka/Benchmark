package models

import squants.energy.{TBtus, Gigajoules, KBtus, Energy}
import squants.space._
import scala.concurrent.Future
import scala.language._
import scala.math._
import play.api.libs.json._
import scala.concurrent.ExecutionContext.Implicits.global
import play.api.libs.functional.syntax._

case class BuildingProperties(parameters: JsValue) {

  val country:String = {
    parameters.asOpt[ConversionInfo] match {
      case Some(a) => a.country
      case _ => throw new Exception("Could not retrieve Country")
    }
  }

  val reportingUnits:String = {
    parameters.asOpt[ConversionInfo] match {
      case Some(a) => a.reportingUnits
      case _ => throw new Exception("Could not retrieve Reporting Units")
    }
  }

  val postalCode: String = {
    parameters.asOpt[ConversionInfo] match {
      case Some(a) => a.postalCode
      case _ => throw new Exception("Could not retrieve PostalCode")
    }
  }
  val state: String = {
    parameters.asOpt[ConversionInfo] match {
      case Some(a) => a.state
      case _ => throw new Exception("Could not retrieve State")
    }
  }
  def getBaselineConstant: Future[Int] = Future{
    parameters.asOpt[BaselineConstant] match {
      case Some(a) => a.baselineConstant
      case _ => throw new Exception("Could not get Baseline Constant for CBECS / HERS Conversion")
    }
  }

  def getPercentBetterThanMedia:Future[Double] = Future{
    parameters.validate[PercentBetterThanMedian] match {
      case JsSuccess(a, _) => a.target
      case JsError(err) => throw new Exception("Could not determine target EUI!")
    }
  }

  def getBuilding: Future[BaseLine] = Future{

    val building: JsResult[BaseLine] = parameters.asOpt[CountryBuildingType] match {
      case Some(CountryBuildingType("USA", "Office")) => parameters.validate[Office]
      case Some(CountryBuildingType("USA", "WorshipCenter")) => parameters.validate[WorshipCenter]
      case Some(CountryBuildingType("USA", "WastewaterCenter")) => parameters.validate[WastewaterCenter]
      case Some(CountryBuildingType("USA", "Warehouse")) => parameters.validate[Warehouse]
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
      case Some(CountryBuildingType("Canada", "Office")) => parameters.validate[CanadaOffice]
      case Some(CountryBuildingType("Canada", "Supermarket")) => parameters.validate[CanadaSupermarket]
      case Some(CountryBuildingType("Canada", "MedicalOffice")) => parameters.validate[CanadaMedicalOffice]
      case Some(CountryBuildingType("Canada", "K12School")) => parameters.validate[CanadaK12School]
      case Some(CountryBuildingType("Canada", "Hospital")) => parameters.validate[CanadaHospital]
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
      case _ => throw new Exception("Could not find Country and Building Type for Median EUI")

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

case class PropParams(propType:String,propSize:Double,propPercent:Double,areaUnits:String)

/** *
  * Base line trait, enables the reducing of equation segments and manages the lookup of energy star score values
  */
sealed trait BaseLine {
  val country:String
  val buildingType:String
  val GFA:PosDouble
  val areaUnits:String
  val regressionSegments: Seq[RegressionSegment]

  def energyReduce:Double = regressionSegments.map(_.reduce).sum
  def expectedEnergy = energyReduce

  implicit def boolOptToInt(b:Option[Boolean]):Int = if (b.getOrElse(false)) 1 else 0

  def converseBoolean(i:Int):Int = {if (i==0) {1} else if (i==1) {0} else {0}}

  val buildingSize:Double = {
    country match {
      case "USA" => (Area((GFA.value,areaUnits)).get to SquareFeet)
      case "Canada" => (Area((GFA.value,areaUnits)).get to SquareMeters)
    }
  }

  def getLog(x:Double):Double = {
    x match {
      case (0) => 0.0
      case x => log(x)
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

case class GenericBuilding (GFA:PosDouble,areaUnits:String, country:String, buildingType:String) extends BaseLine {
  val regressionSegments = Seq[RegressionSegment]()

}
object GenericBuilding {
  implicit val genericBuildingTypeRead: Reads[GenericBuilding] = Json.reads[GenericBuilding]
}



/**
  *   Building Type parameters
  *
  * @param weeklyOperatingHours
  * @param numWorkersMainShift
  * @param numComputers
  * @param percentHeated
  * @param percentCooled
  * @param HDD
  * @param CDD
  * @param isSmallBank  "if is bank branch or financial office AND < 50,000 sq ft in area"
  * @param GFA
  * @param areaUnits
  */

case class Office(GFA:PosDouble, numComputers:PosDouble, weeklyOperatingHours: PosDouble, percentHeated:PosDouble,
                  percentCooled:PosDouble, HDD:PosDouble, CDD:PosDouble, isSmallBank:Option[Boolean], numWorkersMainShift:PosDouble,
                  areaUnits:String, country:String, buildingType:String) extends BaseLine {




  val regressionSegments = Seq[RegressionSegment] (
    RegressionSegment(186.6, 0, 1), // regression constant
    RegressionSegment(34.17, 9.535, math.min(log(buildingSize),200000)),
    RegressionSegment(17.28, 2.231, math.min(numComputers.value / buildingSize * 1000, 11.1)),
    RegressionSegment(55.96, 3.972, log(weeklyOperatingHours.value)),
    RegressionSegment(10.34, 0.5616, log(numWorkersMainShift.value / buildingSize * 1000)),
    RegressionSegment(0.0077, 4411, HDD.value * percentHeated.value / 100),
    RegressionSegment(0.0144, 1157, CDD.value * percentCooled.value / 100),
    RegressionSegment(0, 9.535, log(buildingSize)),
    RegressionSegment(0, .5616, log(numWorkersMainShift.value / buildingSize * 1000)),
    RegressionSegment(0, 0, 1)
  )
}
/**
  * Office companion object.  Contains built in JSON validation.
  */
object Office {
  implicit val officeReads: Reads[Office] = Json.reads[Office]
}

case class FinancialOffice(GFA:PosDouble, numComputers:PosDouble, weeklyOperatingHours: PosDouble, percentHeated:PosDouble,
                  percentCooled:PosDouble, HDD:PosDouble, CDD:PosDouble, isSmallBank:Option[Boolean], numWorkersMainShift:PosDouble,
                  areaUnits:String, country:String, buildingType:String) extends BaseLine {


  val regressionSegments = Seq[RegressionSegment] (
    RegressionSegment(186.6, 0, 1), // regression constant
    RegressionSegment(34.17, 9.535, math.min(log(buildingSize),200000)),
    RegressionSegment(17.28, 2.231, math.min(numComputers.value / buildingSize * 1000, 11.1)),
    RegressionSegment(55.96, 3.972, log(weeklyOperatingHours.value)),
    RegressionSegment(10.34, 0.5616, log(numWorkersMainShift.value / buildingSize * 1000)),
    RegressionSegment(0.0077, 4411, HDD.value * percentHeated.value / 100),
    RegressionSegment(0.0144, 1157, CDD.value * percentCooled.value / 100),
    RegressionSegment(-64.83, 9.535, log(buildingSize)),
    RegressionSegment(34.2, .5616, log(numWorkersMainShift.value / buildingSize * 1000)),
    RegressionSegment(56.3, 0, 1)
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
  * @param HDD
  * @param CDD
  * @param numServers
  * @param GFA
  * @param areaUnits
  */

case class CanadaOffice(weeklyOperatingHours:PosDouble, numWorkersMainShift:PosDouble, numComputers:PosDouble,
                        percentCooled:PosDouble, HDD:PosDouble, CDD:PosDouble, numServers:PosDouble, GFA:PosDouble,
                        areaUnits:String, country:String, buildingType:String) extends BaseLine {




  val regressionSegments = Seq[RegressionSegment] (
    RegressionSegment(1.788, 0, 1), // regression constant
    RegressionSegment(.006325, 57.95, weeklyOperatingHours.value),
    RegressionSegment(.06546, 3.492, numWorkersMainShift.value / buildingSize * 100),
    RegressionSegment(.07455, 3.335, (numComputers.value + numServers.value) / buildingSize * 100),
    RegressionSegment(.3643, 7.36, log(math.min(buildingSize,5000))), // buildingSize capped @ 5,000 sq meters during analysis
    RegressionSegment(-0.00002596, 2933, math.min(buildingSize,5000)), // buildingSize capped @ 5,000 sq meters during analysis
    RegressionSegment(.0002034, 4619, HDD.value),
    RegressionSegment(.06386, 3.703, log(CDD.value) * percentCooled.value / 100)
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
  * @param HDD
  * @param CDD
  * @param GFA
  * @param hasFoodPreparation
  * @param isOpenAllWeekdays
  * @param areaUnits
  */

case class WorshipCenter(weeklyOperatingHours:PosDouble, seatingCapacity:PosDouble, numComputers:PosDouble,
                         numRefrUnits:PosDouble, HDD:PosDouble, CDD:PosDouble, GFA:PosDouble, hasFoodPreparation:Option[Boolean],
                         isOpenAllWeekdays:Option[Boolean], areaUnits:String, country:String, buildingType:String) extends BaseLine {




  val regressionSegments = Seq[RegressionSegment] (
    RegressionSegment(73.91, 0, 1), // regression constant
    RegressionSegment(0.6532, 38.81, seatingCapacity.value / buildingSize * 1000),
    RegressionSegment(19.14 * isOpenAllWeekdays, 0, 1),
    RegressionSegment(.2717, 33.28, weeklyOperatingHours.value),
    RegressionSegment(26.55, 0.2036, numComputers.value / buildingSize * 1000),
    RegressionSegment(15.83 * hasFoodPreparation, 0, 1),
    RegressionSegment(113.1, 0.0183, numRefrUnits.value / buildingSize * 1000),
    RegressionSegment(0.0081, 4523, HDD.value),
    RegressionSegment(.0141, 1313, CDD.value)
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
  * @param HDD
  * @param CDD
  * @param GFA
  * @param areaUnits
  */
case class WastewaterCenter(wastewaterAvgInfluentInflow:PosDouble, wastewaterInfluentBiologicalOxygenDemand:PosDouble,
                            wastewaterEffluentBiologicalOxygenDemand:PosDouble, wastewaterPlantDesignFlowRate:PosDouble,
                            wastewaterHasTrickleFiltration:Option[Boolean], wastewaterHasNutrientRemoval:Option[Boolean],
                            wastewaterLoadFactor:PosDouble, HDD:PosDouble, CDD:PosDouble, GFA:PosDouble, areaUnits:String, country:String, buildingType:String) extends BaseLine {



  // for predicted EUI you do not divide by GFA, you divide by average influent flow in Gallons per Day


  val regressionSegments = Seq[RegressionSegment] (
    RegressionSegment(10.13, 0, 1), // regression constant
    RegressionSegment(-0.9421, 1.863, log(wastewaterAvgInfluentInflow.value)),
    RegressionSegment(4.876, 5.204, log(wastewaterInfluentBiologicalOxygenDemand.value)),
    RegressionSegment(-2.082, 1.656, log(wastewaterEffluentBiologicalOxygenDemand.value)),
    RegressionSegment(-4.668, 4.171, log(wastewaterLoadFactor.value)),
    RegressionSegment(-2.577,0.179, 1 * wastewaterHasTrickleFiltration),
    RegressionSegment(1.235, 0.4591, 1 * wastewaterHasNutrientRemoval),
    RegressionSegment(2.355,8.724,log(HDD.value)),
    RegressionSegment(1.243, 6.5, log(CDD.value))
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
  * @param HDD
  * @param CDD
  * @param isWarehouseRefrigerated
  * @param percentHeated
  * @param percentCooled
  * @param GFA
  * @param areaUnits
  */
case class Warehouse(weeklyOperatingHours:PosDouble, numWorkersMainShift:PosDouble, numWalkinRefrUnits:PosDouble,
                     HDD:PosDouble, CDD:PosDouble,isWarehouseRefrigerated:Option[Boolean], percentHeated:PosDouble,
                     percentCooled:PosDouble, GFA:PosDouble, areaUnits:String, country:String, buildingType:String) extends BaseLine {





  val regressionSegments = Seq[RegressionSegment] (
    RegressionSegment(82.18, 0, 1), // regression constant
    RegressionSegment(168.6 * isWarehouseRefrigerated, 0, 1),
    RegressionSegment(13.63, 9.806, log(buildingSize)),
    RegressionSegment(41.84, 0.5943, numWorkersMainShift.value * 1000 / buildingSize),
    RegressionSegment(0.3111, 60.93, weeklyOperatingHours.value),
    RegressionSegment(0.0708 * isWarehouseRefrigerated,1570,CDD.value),
    RegressionSegment(0.011 * converseBoolean(isWarehouseRefrigerated),2707,HDD.value * percentHeated.value / 100),
    RegressionSegment(.0205 * converseBoolean(isWarehouseRefrigerated), 378.7, CDD.value * percentCooled.value / 100),
    RegressionSegment(262.3 * converseBoolean(isWarehouseRefrigerated), 0.0096, numWalkinRefrUnits.value * 1000 / buildingSize )
  )
}

/**
  * Warehouse companion object.  Contains built in JSON validation.
  */
object Warehouse {
  implicit val warehouseReads: Reads[Warehouse] = Json.reads[Warehouse]
}

/**
  *
  * @param weeklyOperatingHours
  * @param numWorkersMainShift
  * @param numWalkinRefrUnits
  * @param HDD
  * @param CDD
  * @param hasCooking
  * @param percentHeated
  * @param percentCooled
  * @param GFA
  * @param areaUnits
  */
case class Supermarket(weeklyOperatingHours:PosDouble, numWorkersMainShift:PosDouble, numWalkinRefrUnits:PosDouble,
                       HDD:PosDouble, CDD:PosDouble,hasCooking:Option[Boolean], percentHeated:PosDouble,
                       percentCooled:PosDouble, GFA:PosDouble, areaUnits:String, country:String, buildingType:String) extends BaseLine {





  val regressionSegments = Seq[RegressionSegment] (
    RegressionSegment(581.1, 0, 1), // regression constant
    RegressionSegment(84.97, 9.679, getLog(buildingSize)),
    RegressionSegment(115.6, -0.1084, getLog(numWorkersMainShift.value * 1000 / buildingSize)),
    RegressionSegment(125.8, 4.657, getLog(weeklyOperatingHours.value)),
    RegressionSegment(794.4, 0.2345, numWalkinRefrUnits.value * 1000 / buildingSize ),
    RegressionSegment(902.8, .0254, hasCooking * 1000 / buildingSize ),
    RegressionSegment(.0947, 1219, CDD.value * percentCooled.value / 100),
    RegressionSegment(0.0326, 3510, HDD.value * percentHeated.value / 100)
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
  * @param HDD
  * @param lengthRefrFoodDisplayCases
  * @param numComputers
  * @param numCashRegisters
  * @param GFA
  * @param areaUnits
  */

case class CanadaSupermarket(weeklyOperatingHours:PosDouble, numWorkersMainShift:PosDouble, HDD:PosDouble,
                             lengthRefrFoodDisplayCases:PosDouble, numComputers:PosDouble, numCashRegisters:PosDouble,
                             GFA:PosDouble, areaUnits:String, country:String, buildingType:String) extends BaseLine {




  val regressionSegments = Seq[RegressionSegment] (
    RegressionSegment(4.828, 0, 1), // regression constant
    RegressionSegment(0.001342, 1038, math.min(buildingSize,2500)),
    RegressionSegment(1.612, 1.802, math.min(math.max(numWorkersMainShift.value * 100 / buildingSize, 0.4490),3.687)),
    RegressionSegment(1.35, 0.3955, numCashRegisters.value * 100 / buildingSize),
    RegressionSegment(0.698, 0.5244, numComputers.value * 100 / buildingSize),
    RegressionSegment(0.08314, 2.827, lengthRefrFoodDisplayCases.value * 100 / buildingSize),
    RegressionSegment(0.0004642, 4798, HDD.value)
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
  * @param HDD
  * @param CDD
  * @param GFA
  * @param areaUnits
  */

case class SeniorCare(avgNumResidents:PosDouble, maxNumResidents:PosDouble,
                      numRezUnits:PosDouble, numElectronicLifts:PosDouble, numWorkersMainShift:PosDouble,
                      numComputers:PosDouble, numRefrUnits:PosDouble, numCommWashingMachines:PosDouble,
                      numRezWashingMachines:PosDouble, percentHeated:PosDouble, percentCooled:PosDouble,
                      HDD:PosDouble, CDD:PosDouble, GFA:PosDouble, areaUnits:String, country:String, buildingType:String) extends BaseLine {




  val regressionSegments = Seq[RegressionSegment] (
    RegressionSegment(253, 0, 1), // regression constant
    RegressionSegment(24.1, 1.582, numRezUnits.value * 1000 / buildingSize),
    RegressionSegment(0.9156, 87.61, avgNumResidents.value/maxNumResidents.value * 100),
    RegressionSegment(256.5, 0.0692, numElectronicLifts.value * 1000 / buildingSize),
    RegressionSegment(35.42, 0.937, numWorkersMainShift.value * 1000 / buildingSize),
    RegressionSegment(90.3, 0.3636, numComputers.value * 1000 / buildingSize),
    RegressionSegment(251.5, 0.0905, numRefrUnits.value * 1000 / buildingSize),
    RegressionSegment(378.2, 0.0432, numCommWashingMachines.value * 1000 / buildingSize),
    RegressionSegment(253, 0.0584, numRezWashingMachines.value * 1000 / buildingSize),
    RegressionSegment(0.02004, 1184, CDD.value * percentCooled.value/100),
    RegressionSegment(0.005879, 4524, HDD.value * percentHeated.value/100)

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
  * @param HDD
  * @param CDD
  * @param GFA
  * @param areaUnits
  */

case class Retail(weeklyOperatingHours:PosDouble, numOpenClosedRefrCases:PosDouble, numCashRegisters:PosDouble,
                  numWorkersMainShift:PosDouble, numComputers:PosDouble,
                  numWalkinRefrUnits:PosDouble, percentHeated:PosDouble, percentCooled:PosDouble,
                  HDD:PosDouble, CDD:PosDouble, GFA:PosDouble, areaUnits:String, country:String, buildingType:String) extends BaseLine {




  val regressionSegments = Seq[RegressionSegment] (
    RegressionSegment(153.1, 0, 1), // regression constant
    RegressionSegment(20.19, 9.371, getLog(buildingSize)),
    RegressionSegment(1.373, 63.74, weeklyOperatingHours.value),
    RegressionSegment(61.76, 0.6279, numWorkersMainShift.value * 1000 / buildingSize),
    RegressionSegment(70.6, 0.3149, numComputers.value * 1000 / buildingSize),
    RegressionSegment(249.1, 0.1905, numCashRegisters.value * 1000 / buildingSize),
    RegressionSegment(720.2, 0.0038, numWalkinRefrUnits.value * 1000 / buildingSize),
    RegressionSegment(81.9, 0.045, numOpenClosedRefrCases.value * 1000 / buildingSize),
    RegressionSegment(0.0125, 972.1, CDD.value * percentCooled.value/100),
    RegressionSegment(0.0113, 3811, HDD.value * percentHeated.value/100)

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
  * @param HDD
  * @param CDD
  * @param GFA
  * @param areaUnits
  */
case class ResidenceHall(numBedrooms:PosDouble, percentHeated:PosDouble, percentCooled:PosDouble,
                         HDD:PosDouble, CDD:PosDouble, GFA:PosDouble, areaUnits:String, country:String, buildingType:String) extends BaseLine {




  val regressionSegments = Seq[RegressionSegment] (
    RegressionSegment(4.99455, 0, 1), // regression constant
    RegressionSegment(0.91309, 0, getLog(buildingSize)),
    RegressionSegment(0.09455, 0, getLog(numBedrooms.value)),
    RegressionSegment(0.00009744, 0, HDD.value * percentHeated.value/100),
    RegressionSegment(0.00016279, 0, CDD.value * percentCooled.value/100)

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
  * @param numUnitsMidRise5to9
  * @param numUnitsHighRise10plus
  * @param HDD
  * @param CDD
  * @param GFA
  * @param areaUnits
  */

case class MultiFamily(numRezUnits:PosDouble, numBedrooms:PosDouble,
                       numUnitsLowRise1to4: PosDouble, numUnitsMidRise5to9:PosDouble, numUnitsHighRise10plus: PosDouble,
                       HDD:PosDouble, CDD:PosDouble, GFA:PosDouble, areaUnits:String, country:String, buildingType:String) extends BaseLine {




  val regressionSegments = Seq[RegressionSegment] (
    RegressionSegment(140.8, 0, 1), // regression constant
    RegressionSegment(52.57, 1.215, numRezUnits.value * 1000 / buildingSize),
    RegressionSegment(24.45, 1.238, numBedrooms.value/numRezUnits.value),
    RegressionSegment(-18.76, 0, numUnitsLowRise1to4.value/(numUnitsLowRise1to4.value + numUnitsMidRise5to9.value
      + numUnitsHighRise10plus.value)),
    RegressionSegment(0.009617, 4233, HDD.value),
    RegressionSegment(0.01617, 1364, CDD.value)

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
  * @param HDD
  * @param CDD
  * @param GFA
  * @param areaUnits
  */
case class CanadaMedicalOffice(weeklyOperatingHours:PosDouble, numWorkersMainShift:PosDouble, percentCooled:PosDouble,
                               HDD:PosDouble, CDD:PosDouble, GFA:PosDouble, areaUnits:String, country:String, buildingType:String) extends BaseLine {



  val workerDensity:Double = numWorkersMainShift.value * 100 / buildingSize

  val regressionSegments = Seq[RegressionSegment] (
    RegressionSegment(1.384, 0, 1), // regression constant
    RegressionSegment(0.00004511, 1635, min(buildingSize,20000.0)),
    RegressionSegment(0.007505, 58.94, weeklyOperatingHours.value),
    RegressionSegment(0.2428, 2.466, min(max(workerDensity,0.3),7)),
    RegressionSegment(0.001297, 100.1, CDD.value * percentCooled.value/100),
    RegressionSegment(0.0002015, 4808, HDD.value)

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
  * @param HDD
  * @param CDD
  * @param GFA
  * @param areaUnits
  */

case class MedicalOffice(weeklyOperatingHours:PosDouble, numWorkersMainShift:PosDouble, percentCooled:PosDouble,
                         percentHeated:PosDouble, HDD:PosDouble, CDD:PosDouble, GFA:PosDouble,
                         areaUnits:String, country:String, buildingType:String) extends BaseLine {




  val regressionSegments = Seq[RegressionSegment] (
    RegressionSegment(2.78889, 0, 1), // regression constant
    RegressionSegment(0.91433, 0, getLog(buildingSize)),
    RegressionSegment(0.46768, 0, getLog(weeklyOperatingHours.value)),
    RegressionSegment(0.21568, 0, getLog(numWorkersMainShift.value)),
    RegressionSegment(0.00020111, 0, CDD.value * percentCooled.value/100),
    RegressionSegment(0.00005321, 0, HDD.value * percentHeated.value/100)

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
  * @param HDD
  * @param CDD
  * @param GFA
  * @param areaUnits
  */

case class CanadaK12School(numWorkersMainShift:PosDouble,
                           gymFloorArea:PosDouble, studentSeatingCapacity:PosDouble, isSecondarySchool:Option[Boolean],
                           percentHeated:PosDouble, percentCooled:PosDouble,HDD:PosDouble, CDD:PosDouble,
                           GFA:PosDouble, areaUnits:String, country:String, buildingType:String) extends BaseLine {


  val regressionSegments = Seq[RegressionSegment] (
    RegressionSegment(1.021, 0, 1), // regression constant
    RegressionSegment(0.2308 * isSecondarySchool, 0, 1),
    RegressionSegment(0.0304, 4.983, getLog(gymFloorArea.value)),
    RegressionSegment(0.0004402, 418.3, studentSeatingCapacity.value),
    RegressionSegment(0.1218, 3.175, getLog(numWorkersMainShift.value)),
    RegressionSegment(-0.3942, 8.118, getLog(buildingSize)),
    RegressionSegment(0.0005647, 47.88, CDD.value * percentCooled.value/100),
    RegressionSegment(0.0001635, 4584, HDD.value * percentHeated.value/100)
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
  * @param HDD
  * @param CDD
  * @param GFA
  * @param areaUnits
  */
case class K12School(isOpenWeekends:Option[Boolean],
                     isHighSchool:Option[Boolean],hasCooking:Option[Boolean],numComputers:PosDouble,
                     numWalkinRefrUnits:PosDouble,percentHeated:PosDouble, percentCooled:PosDouble,HDD:PosDouble,
                     CDD:PosDouble, GFA:PosDouble, areaUnits:String, country:String, buildingType:String) extends BaseLine {




  val regressionSegments = Seq[RegressionSegment] (
    RegressionSegment(131.9, 0, 1), // regression constant
    RegressionSegment(4.377 * isHighSchool, 0, 1),
    RegressionSegment(8.974, 7.716, getLog(HDD.value) * percentHeated.value / 100),
    RegressionSegment(6.389, 5.045, getLog(CDD.value) * percentCooled.value / 100),
    RegressionSegment(-19.26, 10.2, getLog(buildingSize)),
    RegressionSegment(18.43 * isOpenWeekends, 0, 1),
    RegressionSegment(574.7, 0.0109, numWalkinRefrUnits.value / buildingSize * 1000),
    RegressionSegment(24.2 * hasCooking, 0, 1),
    RegressionSegment(9.568, 1.742, numComputers.value / buildingSize * 1000),


    //if High School also include the following
    RegressionSegment(0.00021 * isHighSchool, 47310, buildingSize),
    RegressionSegment(0.0285 * isHighSchool, 1316, CDD.value * percentCooled.value / 100),
    RegressionSegment(-11.75 * isHighSchool, 5.045, getLog(CDD.value) * percentCooled.value / 100)

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
  * @param HDD
  * @param CDD
  * @param GFA
  * @param areaUnits
  */

case class Hotel(numBedrooms:PosDouble, hasFoodPreparation:Option[Boolean], numWorkersMainShift:PosDouble,
                 numRefrUnits:PosDouble, percentHeated:PosDouble, percentCooled:PosDouble,
                 HDD:PosDouble, CDD:PosDouble, GFA:PosDouble, areaUnits:String, country:String, buildingType:String) extends BaseLine {




  val regressionSegments = Seq[RegressionSegment] (
    RegressionSegment(169.1, 0, 1), // regression constant
    RegressionSegment(33.22, 1.951, numBedrooms.value * 1000 / buildingSize),
    RegressionSegment(20.81, -1.395, getLog(numWorkersMainShift.value * 1000 / buildingSize)),
    RegressionSegment(65.14 * hasFoodPreparation, 0, 1),
    RegressionSegment(249.8, 0.0227, numRefrUnits.value * 1000 / buildingSize),
    RegressionSegment(0.0169, 1224, CDD.value * percentCooled.value/100),
    RegressionSegment(0.0107, 4120, HDD.value * percentHeated.value/100)

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
  * @param CDD
  * @param GFA
  * @param areaUnits
  */

case class Hospital(numFTEWorkers:PosDouble, numStaffedBeds:PosDouble, numMRIMachines:PosDouble,
                    CDD:PosDouble, GFA:PosDouble, areaUnits:String, country:String, buildingType:String) extends BaseLine {




  val regressionSegments = Seq[RegressionSegment] (
    RegressionSegment(484.8, 0, 1), // regression constant
    RegressionSegment(26.64, 2.6, numFTEWorkers.value * 1000 / buildingSize),
    RegressionSegment(120.3, 0.4636, numStaffedBeds.value * 1000 / buildingSize),
    RegressionSegment(8961, 0.0031, numMRIMachines.value * 1000 / buildingSize),
    RegressionSegment(0.0227, 1392, CDD.value)

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
  * @param HDD
  * @param CDD
  * @param GFA
  * @param areaUnits
  */

case class CanadaHospital(weeklyOperatingHours:PosDouble, numWorkersMainShift:PosDouble,
                          licensedBedCapacity:PosDouble, hasLaundryFacility:Option[Boolean],
                          percentHeated:PosDouble, percentCooled:PosDouble,
                          HDD:PosDouble, CDD:PosDouble, GFA:PosDouble, areaUnits:String, country:String, buildingType:String) extends BaseLine {

  val regressionSegments = Seq[RegressionSegment] (
    RegressionSegment(2.984, 0, 1), // regression constant
    RegressionSegment(0.6092, 1.417, numWorkersMainShift.value * 100 / buildingSize),
    RegressionSegment(-0.0984, 2.726, numWorkersMainShift.value/licensedBedCapacity.value),
    RegressionSegment(0.4596 * hasLaundryFacility, 0, 1),
    RegressionSegment(9.3598e-06, 19004, min(100000,buildingSize)),
    RegressionSegment(0.2775, 4.03, getLog(CDD.value+50) * percentCooled.value/100),
    RegressionSegment(0.00047986, 4787, HDD.value * percentHeated.value/100)

  )
}

/**
  * CanadaHospital companion object.  Contains built in JSON validation.
  */
object CanadaHospital {
  implicit val canadaHospitalReads: Reads[CanadaHospital] = Json.reads[CanadaHospital]
}


// Data Centers don't follow the same rules as other buildings, to include them need to expand code with PUE based analysis
case class DataCenter(annualITEnergy:PosDouble, reportingUnits:String,
                      GFA:PosDouble, areaUnits:String, country:String, buildingType:String) extends BaseLine {

  val siteToSourceITConvert: Double = country match {
    case "USA" => 3.14
    case _ => 2.05
  }

  val annualITEnergyTBtu: Double = (Energy((annualITEnergy.value, "kWh")).get to TBtus) * siteToSourceITConvert
  val annualITEnergyKBtu: Double = (Energy((annualITEnergy.value, "kWh")).get to KBtus) * siteToSourceITConvert

  val regressionSegments = Seq[RegressionSegment](
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

