
package models

import net.sf.ehcache.search.expression.And
import squants.energy.{Energy, KilowattHours}
import squants.space._
import scala.concurrent.Future
import scala.language._
import scala.math._
import play.api.libs.json._
import play.api.Play
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util._
import java.io.{InputStream, FileReader, FileNotFoundException, IOException}
import play.api.libs.functional.syntax._
import play.api.data.validation.ValidationError


case class EUIMetrics(parameters: JsValue) {

  val energyCalcs:EUICalculator = EUICalculator(parameters)
  
  val sourceEUI: Future[Double] = for {
    targetBuilding <- Future{getBuilding(parameters)}
    sourceTotalEnergy <- energyCalcs.getTotalSourceEnergy
    sourceEUI <- {
      //Console.println("Total Source Energy: " + sourceTotalEnergy)
      val f = targetBuilding match {
        case JsSuccess(a, _) => sourceTotalEnergy.value / a.floorArea
        case JsError(err) => throw new Exception("Could not determine building size for EUI Calculation!")
      }
      //Console.println("Total Actual Source EUI: " + f)
      Future(f)
    }
    } yield sourceEUI


  val ExpectedEUI:Future[Double] = {
    for {
      targetBuilding <- Future{getBuilding(parameters)}
      expectedEUI <- computeExpectedEUI(targetBuilding)
    } yield expectedEUI
  }

  val ES:Future[Int] = {

    for {
      targetBuilding <- Future {getBuilding(parameters)}
      lookupEUI <- computeLookupEUI(targetBuilding)
      sourceTotalEnergy <-  energyCalcs.getTotalSourceEnergy
      euiRatio <- getEUIratio(targetBuilding, lookupEUI, sourceTotalEnergy.value)
      lookUp <- getLookupTable(parameters)
      futureRatio <- loadLookupTable(lookUp).map {
        _.dropWhile(_.Ratio < euiRatio).headOption
      }
      checkRatio <- loadLookupTable(lookUp).map {
        _.lastOption
      }
      lastRatio <- if (futureRatio.isDefined) {
        Future(futureRatio)
      } else {
        Future(checkRatio)
      }
      computeES <- Future(lastRatio.get.ES)

    } yield computeES
  }

  val targetEUI:Future[Double] = {
    for {
      targetBuilding <- Future {getBuilding(parameters)}
      lookupEUI <- computeLookupEUI(targetBuilding)
      targetRatio <- getTargetRatio(parameters)
      targetEUI <-  getTargetEUI(targetBuilding,lookupEUI,targetRatio)
    } yield targetEUI
  }


  def computeExpectedEUI[T](targetBuilding: T): Future[Double] = {
    val f = targetBuilding match {
      case JsSuccess(a: ResidenceHall, _) => exp(a.expectedEnergy) / a.floorArea
      case JsSuccess(a: MedicalOffice, _) => exp(a.expectedEnergy) / a.floorArea
      case JsSuccess(a: BaseLine, _) => a.expectedEnergy
      case JsError(err) => throw new Exception("Actual EUI could not be computed!")
    }
    //Console.println("Predicted Source EUI: " + f)
    Future(f)
  }

  def getEUIratio[T](targetBuilding: T,lookupPredictedEUI:Double,sourceEnergy:Double):Future[Double] = {
    val f = targetBuilding match {
      case JsSuccess(a: ResidenceHall, _) => log(sourceEnergy) * 15.717 / lookupPredictedEUI
      case JsSuccess(a: MedicalOffice, _) => log(sourceEnergy) * 14.919 / lookupPredictedEUI
      case JsSuccess(a: BaseLine, _) => sourceEnergy / a.floorArea / lookupPredictedEUI
      case JsError(err) => throw new Exception("Could not calculate EUI Ratio!")
    }
    Console.println("Reference EUI Ratio: " +  f)
    Future(f)
  }

  def computeLookupEUI[T](targetBuilding: T): Future[Double] = {
    val f = targetBuilding match {
      case JsSuccess(a: BaseLine,_) => a.expectedEnergy
      case JsError(err) => throw new Exception("EUI (for lookup) could not be predicted")
    }
    Future(f)
  }

  def getTargetRatio(parameters:JsValue):Future[Double] = {
    val f = parameters.asOpt[TargetES] match {
      case Some(a) =>
        for {
          lookUp <- getLookupTable(parameters)
          targetRatioEntry <- loadLookupTable(lookUp).map {
            _.filter(_.ES == a.target).last.Ratio
          }
        } yield targetRatioEntry
      case None => throw new Exception("Could not calculate your Target EUI Ratio")
    }
    f
  }

  def getTargetEUI[T](targetBuilding: T,lookupEUI:Double,targetRatio:Double):Future[Double] = {
    val f = targetBuilding match {
      case JsSuccess(a: ResidenceHall, _) => exp(targetRatio / 15.717 * lookupEUI) / a.floorArea
      case JsSuccess(a: MedicalOffice, _) => exp(targetRatio / 14.919 * lookupEUI) / a.floorArea
      case JsSuccess(a: BaseLine, _) => targetRatio * lookupEUI
      case JsError(err) => throw new Exception("Could not calculate Target EUI!")
    }
    Console.println("Reference Expected(Source) EUI: " +  lookupEUI)
    //Console.println("Target EUI: " +  f)
    Future(f)
  }

  def loadLookupTable(filename:String): Future[Seq[JsonEntry]] = {
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
      obj <- Future {
        json.validate[Seq[JsonEntry]] match {
          case JsSuccess(a, _) => a
          case JsError(th) => throw new Exception("Cannot find this: " + th.toString())
        }
      }
    } yield obj
  }

  def getLookupTable(parameters: JsValue): Future[String] = {

    val r = parameters.asOpt[CountryBuildingType] match {
      case Some(CountryBuildingType("USA", "Office")) => Play.current.configuration.getString("baseline.office")
      case Some(CountryBuildingType("USA", "WorshipCenter")) => Play.current.configuration.getString("baseline.worshipCenter")
      case Some(CountryBuildingType("USA", "WastewaterCenter")) => Play.current.configuration.getString("baseline.wastewaterCenter")
      case Some(CountryBuildingType("USA", "Warehouse")) => Play.current.configuration.getString("baseline.warehouse")
      case Some(CountryBuildingType("USA", "Supermarket")) => Play.current.configuration.getString("baseline.supermarket")
      case Some(CountryBuildingType("USA", "SeniorCare")) => Play.current.configuration.getString("baseline.seniorCare")
      case Some(CountryBuildingType("USA", "Retail")) => Play.current.configuration.getString("baseline.retail")
      case Some(CountryBuildingType("USA", "MultiFamily")) => Play.current.configuration.getString("baseline.multiFamily")
      case Some(CountryBuildingType("USA", "ResidenceHall")) => Play.current.configuration.getString("baseline.residenceHall")
      case Some(CountryBuildingType("USA", "MedicalOffice")) => Play.current.configuration.getString("baseline.medicalOffice")
      case Some(CountryBuildingType("USA", "K12School")) => Play.current.configuration.getString("baseline.K12School")
      case Some(CountryBuildingType("USA", "Hotel")) => Play.current.configuration.getString("baseline.hotel")
      case Some(CountryBuildingType("USA", "DataCenter")) => Play.current.configuration.getString("datacenter.hotel")
      case Some(CountryBuildingType("USA", "Hospital")) => Play.current.configuration.getString("baseline.hospital")
      case Some(CountryBuildingType("Canada", "Office")) => Play.current.configuration.getString("baseline.canadaOffice")
      case Some(CountryBuildingType("Canada", "Supermarket")) => Play.current.configuration.getString("baseline.canadaSupermarket")
      case Some(CountryBuildingType("Canada", "MedicalOffice")) => Play.current.configuration.getString("baseline.canadaMedicalOffice")
      case Some(CountryBuildingType("Canada", "K12School")) => Play.current.configuration.getString("baseline.canadaK12School")
      case Some(CountryBuildingType("Canada", "Hospital")) => Play.current.configuration.getString("baseline.canadaHospital")
      case Some(_) => None
      case None => None
    }
    Future(r.getOrElse("Lookup Table Not Found"))
  }

  def getBuilding(parameters: JsValue): JsResult[BaseLine] = {
    parameters.asOpt[CountryBuildingType] match {
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
      case Some(CountryBuildingType("Canada", "Office")) => parameters.validate[CanadaOffice]
      case Some(CountryBuildingType("Canada", "Supermarket")) => parameters.validate[CanadaSupermarket]
      case Some(CountryBuildingType("Canada", "MedicalOffice")) => parameters.validate[CanadaMedicalOffice]
      case Some(CountryBuildingType("Canada", "K12School")) => parameters.validate[CanadaK12School]
      case Some(CountryBuildingType("Canada", "Hospital")) => parameters.validate[CanadaHospital]
      case Some(_) => JsError("No matching building by country and type!")
      case None => JsError("Could not find country or buildingType fields with JSON")
    }
  }
}

case class TargetES(target:Int)
object TargetES {
  implicit val TargetReads: Reads[TargetES] = (JsPath \ "targetScore").read[Int](Reads.min(0) andKeep
    Reads.max(100)).map(new TargetES(_))
}

case class JsonEntry(ES: Int, CmPercent: Double, Ratio: Double)
object JsonEntry {
  implicit val formatFileName:Reads[JsonEntry] = Json.format[JsonEntry]
}

case class PosInt(value: Int)
object PosInt {
  implicit val reads: Reads[PosInt] = JsPath.read[Int](Reads.min(0)).map(new PosInt(_))
}

case class PosDouble(value: Double)
object PosDouble {
  implicit val reads: Reads[PosDouble] = JsPath.read[Double](Reads.min(0.0)).map(new PosDouble(_))
}

/** *
  * Base line trait, enables the reducing of equation segments and manages the lookup of energy star score values
  */
sealed trait BaseLine {
  val floorArea:Double
  val regressionSegments: Seq[RegressionSegment]

  def energyReduce:Double = regressionSegments.map(_.reduce).sum
  def expectedEnergy = energyReduce
  def squareFunction(s: String):String = "Square" + s.capitalize

  implicit def boolOptToInt(b:Option[Boolean]):Int = if (b.getOrElse(false)) 1 else 0

  def converseBoolean(i:Int):Int = {if (i==0) {1} else if (i==1) {0} else {0}}
}
/**
 *
 * @param a Coefficient
 * @param b Centering Variable
 * @param c User Input
 */

case class RegressionSegment(a:Double, b:Double, c:Double) {
def reduce:Double = {
  //Console.println("a :" + a + " b: " + b + " c: " + c + " total: " + a * (c - b))
  a * (c - b) }
}
/**
* Class to manage the decomposing of Country and Building from the input JSON
* @param country country that the building resides in
* @param buildingType type of building
*/
case class CountryBuildingType(country: String, buildingType: String)

object CountryBuildingType {
implicit val countryBuildingTypeRead: Reads[CountryBuildingType] = Json.reads[CountryBuildingType]
}

/**
* Office Building Type parameters
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

case class Office(GFA:PosDouble, numComputers:PosInt, weeklyOperatingHours: PosInt, percentHeated:PosInt,
                  percentCooled:PosInt, HDD:PosInt, CDD:PosInt, isSmallBank:Option[Boolean], numWorkersMainShift:PosInt,
                  areaUnits:String) extends BaseLine {

  val floorArea:Double = (Area((GFA.value,areaUnits)).get to SquareFeet)

  val regressionSegments = Seq[RegressionSegment] (
    RegressionSegment(186.6, 0, 1), // regression constant
    RegressionSegment(34.17, 9.535, math.min(log(floorArea),200000)),
    RegressionSegment(17.28, 2.231, math.min(numComputers.value / floorArea * 1000, 11.1)),
    RegressionSegment(55.96, 3.972, log(weeklyOperatingHours.value)),
    RegressionSegment(10.34, 0.5616, log(numWorkersMainShift.value / floorArea * 1000)),
    RegressionSegment(0.0077, 4411, HDD.value * percentHeated.value),
    RegressionSegment(0.0144, 1157, CDD.value * percentCooled.value),
    RegressionSegment(-64.83 * isSmallBank, 9.535, log(floorArea)),
    RegressionSegment(34.2 * isSmallBank, .5616, log(numWorkersMainShift.value / floorArea * 1000)),
    RegressionSegment(56.3 * isSmallBank, 0, 1)
  )
}

/**
* Office companion object.  Contains built in JSON validation.
*/
object Office {
implicit val officeReads: Reads[Office] = Json.reads[Office]
}

/**
 * Canada Office
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

case class CanadaOffice(weeklyOperatingHours:PosInt, numWorkersMainShift:PosInt, numComputers:PosInt,
                percentCooled:PosInt, HDD:PosInt, CDD:PosInt, numServers:PosInt, GFA:PosDouble,
                areaUnits:String) extends BaseLine {

  val floorArea:Double = (Area((GFA.value,areaUnits)).get to SquareMeters)

  val regressionSegments = Seq[RegressionSegment] (
    RegressionSegment(1.788, 0, 1), // regression constant
    RegressionSegment(.006325, 57.95, weeklyOperatingHours.value),
    RegressionSegment(.06546, 3.492, numWorkersMainShift.value / floorArea * 100),
    RegressionSegment(.07455, 3.335, (numComputers.value + numServers.value) / floorArea * 100),
    RegressionSegment(.3643, 7.36, log(math.min(floorArea,5000))), // floorArea capped @ 5,000 sq meters during analysis
    RegressionSegment(-0.00002596, 2933, math.min(floorArea,5000)), // floorArea capped @ 5,000 sq meters during analysis
    RegressionSegment(.0002034, 4619, HDD.value),
    RegressionSegment(.06386, 3.703, log(CDD.value) * percentCooled.value)
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

case class WorshipCenter(weeklyOperatingHours:PosInt, seatingCapacity:PosInt, numComputers:PosInt,
                         numRefrUnits:PosInt, HDD:PosInt, CDD:PosInt, GFA:PosDouble, hasFoodPreparation:Option[Boolean],
                         isOpenAllWeekdays:Option[Boolean], areaUnits:String) extends BaseLine {

  val floorArea:Double = (Area((GFA.value,areaUnits)).get to SquareFeet)

  val regressionSegments = Seq[RegressionSegment] (
    RegressionSegment(73.91, 0, 1), // regression constant
    RegressionSegment(0.6532, 38.81, seatingCapacity.value / floorArea * 1000),
    RegressionSegment(19.14 * isOpenAllWeekdays, 0, 1),
    RegressionSegment(.2717, 33.28, weeklyOperatingHours.value),
    RegressionSegment(26.55, 0.2036, numComputers.value / floorArea * 1000),
    RegressionSegment(15.83 * hasFoodPreparation, 0, 1),
    RegressionSegment(113.1, 0.0183, numRefrUnits.value / floorArea * 1000),
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
                            wastewaterLoadFactor:PosDouble, HDD:PosInt, CDD:PosInt, GFA:PosDouble, areaUnits:String) extends BaseLine {

  val floorArea:Double = (Area((GFA.value,areaUnits)).get to SquareFeet)
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
case class Warehouse(weeklyOperatingHours:PosInt, numWorkersMainShift:PosInt, numWalkinRefrUnits:PosInt,
                     HDD:PosInt, CDD:PosInt,isWarehouseRefrigerated:Option[Boolean], percentHeated:PosDouble,
                     percentCooled:PosDouble, GFA:PosDouble, areaUnits:String) extends BaseLine {

  val floorArea:Double = (Area((GFA.value,areaUnits)).get to SquareFeet)


  val regressionSegments = Seq[RegressionSegment] (
    RegressionSegment(82.18, 0, 1), // regression constant
    RegressionSegment(168.6 * isWarehouseRefrigerated, 0, 1),
    RegressionSegment(13.63, 9.806, log(floorArea)),
    RegressionSegment(41.84, 0.5943, numWorkersMainShift.value * 1000 / floorArea),
    RegressionSegment(0.3111, 60.93, weeklyOperatingHours.value),
    RegressionSegment(0.0708 * isWarehouseRefrigerated,1570,CDD.value),
    RegressionSegment(0.011 * converseBoolean(isWarehouseRefrigerated),2707,HDD.value * percentHeated.value / 100),
    RegressionSegment(.0205 * converseBoolean(isWarehouseRefrigerated), 378.7, CDD.value * percentCooled.value / 100),
    RegressionSegment(262.3 * converseBoolean(isWarehouseRefrigerated), 0.0096, numWalkinRefrUnits.value * 1000 / floorArea )
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
                     percentCooled:PosDouble, GFA:PosDouble, areaUnits:String) extends BaseLine {

  val floorArea:Double = (Area((GFA.value,areaUnits)).get to SquareFeet)


  val regressionSegments = Seq[RegressionSegment] (
    RegressionSegment(581.1, 0, 1), // regression constant
    RegressionSegment(84.97, 9.679, log(floorArea)),
    RegressionSegment(115.6, -0.1084, log(numWorkersMainShift.value * 1000 / floorArea)),
    RegressionSegment(125.8, 4.657, log(weeklyOperatingHours.value)),
    RegressionSegment(794.4, 0.2345, numWalkinRefrUnits.value * 1000 / floorArea ),
    RegressionSegment(902.8, .0254, hasCooking * 1000 / floorArea ),
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
                             GFA:PosDouble, areaUnits:String) extends BaseLine {

  val floorArea:Double = (Area((GFA.value,areaUnits)).get to SquareMeters)

  val regressionSegments = Seq[RegressionSegment] (
    RegressionSegment(4.828, 0, 1), // regression constant
    RegressionSegment(0.001342, 1038, math.min(floorArea,2500)),
    RegressionSegment(1.612, 1.802, math.min(math.max(numWorkersMainShift.value * 100 / floorArea, 0.4490),3.687)),
    RegressionSegment(1.35, 0.3955, numCashRegisters.value * 100 / floorArea),
    RegressionSegment(0.698, 0.5244, numComputers.value * 100 / floorArea),
    RegressionSegment(0.08314, 2.827, lengthRefrFoodDisplayCases.value * 100 / floorArea),
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
 * @param weeklyOperatingHours
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

case class SeniorCare(weeklyOperatingHours:PosInt, avgNumResidents:PosDouble, maxNumResidents:PosDouble,
                      numRezUnits:PosDouble, numElectronicLifts:PosDouble, numWorkersMainShift:PosDouble,
                      numComputers:PosDouble, numRefrUnits:PosDouble, numCommWashingMachines:PosDouble,
                      numRezWashingMachines:PosDouble, percentHeated:PosDouble, percentCooled:PosDouble,
                      HDD:PosDouble, CDD:PosDouble, GFA:PosDouble, areaUnits:String) extends BaseLine {

  val floorArea:Double = (Area((GFA.value,areaUnits)).get to SquareFeet)

  val regressionSegments = Seq[RegressionSegment] (
    RegressionSegment(253, 0, 1), // regression constant
    RegressionSegment(24.1, 1.582, numRezUnits.value * 1000 / floorArea),
    RegressionSegment(0.9156, 87.61, avgNumResidents.value/maxNumResidents.value * 100),
    RegressionSegment(256.5, 0.0692, numElectronicLifts.value * 1000 / floorArea),
    RegressionSegment(35.42, 0.937, numWorkersMainShift.value * 1000 / floorArea),
    RegressionSegment(90.3, 0.3636, numComputers.value * 1000 / floorArea),
    RegressionSegment(251.5, 0.0905, numRefrUnits.value * 1000 / floorArea),
    RegressionSegment(378.2, 0.0432, numCommWashingMachines.value * 1000 / floorArea),
    RegressionSegment(253, 0.0584, numRezWashingMachines.value * 1000 / floorArea),
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
 * @param numRefrUnits
 * @param numWalkinRefrUnits
 * @param percentHeated
 * @param percentCooled
 * @param HDD
 * @param CDD
 * @param GFA
 * @param areaUnits
 */

case class Retail(weeklyOperatingHours:PosInt, numOpenClosedRefrCases:PosDouble, numCashRegisters:PosDouble,
                  numWorkersMainShift:PosDouble, numComputers:PosDouble, numRefrUnits:PosDouble,
                  numWalkinRefrUnits:PosDouble, percentHeated:PosDouble, percentCooled:PosDouble,
                  HDD:PosDouble, CDD:PosDouble, GFA:PosDouble, areaUnits:String) extends BaseLine {

  val floorArea:Double = (Area((GFA.value,areaUnits)).get to SquareFeet)

  val regressionSegments = Seq[RegressionSegment] (
    RegressionSegment(153.1, 0, 1), // regression constant
    RegressionSegment(20.19, 9.371, log(floorArea)),
    RegressionSegment(1.373, 63.74, weeklyOperatingHours.value),
    RegressionSegment(61.76, 0.6279, numWorkersMainShift.value * 1000 / floorArea),
    RegressionSegment(70.6, 0.3149, numComputers.value * 1000 / floorArea),
    RegressionSegment(249.1, 0.1905, numCashRegisters.value * 1000 / floorArea),
    RegressionSegment(720.2, 0.0038, numWalkinRefrUnits.value * 1000 / floorArea),
    RegressionSegment(81.9, 0.045, numOpenClosedRefrCases.value * 1000 / floorArea),
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
                      HDD:PosDouble, CDD:PosDouble, GFA:PosDouble, areaUnits:String) extends BaseLine {

  val floorArea:Double = (Area((GFA.value,areaUnits)).get to SquareFeet)

  val regressionSegments = Seq[RegressionSegment] (
    RegressionSegment(4.99455, 0, 1), // regression constant
    RegressionSegment(0.91309, 0, log(floorArea)),
    RegressionSegment(0.09455, 0, log(numBedrooms.value)),
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
 * @param weeklyOperatingHours
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

case class MultiFamily(weeklyOperatingHours:PosInt, numRezUnits:PosDouble, numBedrooms:PosDouble,
                       numUnitsLowRise1to4: PosDouble, numUnitsMidRise5to9:PosDouble, numUnitsHighRise10plus: PosDouble,
                       HDD:PosDouble, CDD:PosDouble, GFA:PosDouble, areaUnits:String) extends BaseLine {

  val floorArea:Double = (Area((GFA.value,areaUnits)).get to SquareFeet)

  val regressionSegments = Seq[RegressionSegment] (
    RegressionSegment(140.8, 0, 1), // regression constant
    RegressionSegment(52.57, 1.215, numRezUnits.value * 1000 / floorArea),
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
case class CanadaMedicalOffice(weeklyOperatingHours:PosInt, numWorkersMainShift:PosDouble, percentCooled:PosDouble,
                      HDD:PosDouble, CDD:PosDouble, GFA:PosDouble, areaUnits:String) extends BaseLine {

  val floorArea:Double = (Area((GFA.value,areaUnits)).get to SquareMeters)
  val workerDensity:Double = numWorkersMainShift.value * 100 / floorArea

  val regressionSegments = Seq[RegressionSegment] (
    RegressionSegment(1.384, 0, 1), // regression constant
    RegressionSegment(0.00004511, 1635, min(floorArea,20000.0)),
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

case class MedicalOffice(weeklyOperatingHours:PosInt, numWorkersMainShift:PosDouble, percentCooled:PosDouble,
                         percentHeated:PosDouble, HDD:PosDouble, CDD:PosDouble, GFA:PosDouble,
                         areaUnits:String) extends BaseLine {

  val floorArea:Double = (Area((GFA.value,areaUnits)).get to SquareFeet)

  val regressionSegments = Seq[RegressionSegment] (
    RegressionSegment(2.78889, 0, 1), // regression constant
    RegressionSegment(0.91433, 0, log(floorArea)),
    RegressionSegment(0.46768, 0, log(weeklyOperatingHours.value)),
    RegressionSegment(0.21568, 0, log(numWorkersMainShift.value)),
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
 * @param weeklyOperatingHours
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

case class CanadaK12School(weeklyOperatingHours:PosInt, numWorkersMainShift:PosDouble,
                           gymFloorArea:PosDouble, studentSeatingCapacity:PosDouble, isSecondarySchool:Option[Boolean],
                           percentHeated:PosDouble, percentCooled:PosDouble,HDD:PosDouble, CDD:PosDouble,
                           GFA:PosDouble, areaUnits:String) extends BaseLine {

  val floorArea:Double = (Area((GFA.value,areaUnits)).get to SquareMeters)

  val regressionSegments = Seq[RegressionSegment] (
    RegressionSegment(1.021, 0, 1), // regression constant
    RegressionSegment(0.2308 * isSecondarySchool, 0, 1),
    RegressionSegment(0.0304, 4.983, log(gymFloorArea.value)),
    RegressionSegment(0.0004402, 418.3, studentSeatingCapacity.value),
    RegressionSegment(0.1218, 3.175, log(numWorkersMainShift.value)),
    RegressionSegment(-0.3942, 8.118, log(floorArea)),
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
 * @param weeklyOperatingHours
 * @param numWorkersMainShift
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

case class K12School(weeklyOperatingHours:PosInt, numWorkersMainShift:PosDouble, isOpenWeekends:Option[Boolean],
                     isHighSchool:Option[Boolean],hasCooking:Option[Boolean],numComputers:PosDouble,
                     numWalkinRefrUnits:PosDouble,percentHeated:PosDouble, percentCooled:PosDouble,HDD:PosDouble,
                     CDD:PosDouble, GFA:PosDouble, areaUnits:String) extends BaseLine {

  val floorArea:Double = (Area((GFA.value,areaUnits)).get to SquareFeet)

  val regressionSegments = Seq[RegressionSegment] (
    RegressionSegment(131.9, 0, 1), // regression constant
    RegressionSegment(4.377 * isHighSchool, 0, 1),
    RegressionSegment(8.974, 7.716, log(HDD.value) * percentHeated.value / 100),
    RegressionSegment(6.389, 5.045, log(CDD.value) * percentCooled.value / 100),
    RegressionSegment(-19.26, 10.2, log(floorArea)),
    RegressionSegment(18.43 * isOpenWeekends, 0, 1),
    RegressionSegment(574.7, 0.0109, numWalkinRefrUnits.value / floorArea * 1000),
    RegressionSegment(24.2 * hasCooking, 0, 1),
    RegressionSegment(9.568, 1.742, numComputers.value / floorArea * 1000),


    //if High School also include the following
    RegressionSegment(0.00021 * isHighSchool, 47310, floorArea),
    RegressionSegment(0.0285 * isHighSchool, 1316, CDD.value * percentCooled.value / 100),
    RegressionSegment(-11.75 * isHighSchool, 5.045, log(CDD.value) * percentCooled.value / 100)

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
                      HDD:PosDouble, CDD:PosDouble, GFA:PosDouble, areaUnits:String) extends BaseLine {

  val floorArea:Double = (Area((GFA.value,areaUnits)).get to SquareFeet)

  val regressionSegments = Seq[RegressionSegment] (
    RegressionSegment(169.1, 0, 1), // regression constant
    RegressionSegment(33.22, 1.951, numBedrooms.value * 1000 / floorArea),
    RegressionSegment(20.81, -1.395, log(numWorkersMainShift.value * 1000 / floorArea)),
    RegressionSegment(65.14 * hasFoodPreparation, 0, 1),
    RegressionSegment(249.8, 0.0227, numRefrUnits.value * 1000 / floorArea),
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
                     CDD:PosDouble, GFA:PosDouble, areaUnits:String) extends BaseLine {

  val floorArea:Double = (Area((GFA.value,areaUnits)).get to SquareFeet)

  val regressionSegments = Seq[RegressionSegment] (
    RegressionSegment(484.8, 0, 1), // regression constant
    RegressionSegment(26.64, 2.6, numFTEWorkers.value * 1000 / floorArea),
    RegressionSegment(120.3, 0.4636, numStaffedBeds.value * 1000 / floorArea),
    RegressionSegment(8961, 0.0031, numMRIMachines.value * 1000 / floorArea),
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

case class CanadaHospital(weeklyOperatingHours:PosInt, numWorkersMainShift:PosDouble,
                    licensedBedCapacity:PosDouble, hasLaundryFacility:Option[Boolean],
                    percentHeated:PosDouble, percentCooled:PosDouble,
                    HDD:PosDouble, CDD:PosDouble, GFA:PosDouble, areaUnits:String) extends BaseLine {

  val floorArea:Double = (Area((GFA.value,areaUnits)).get to SquareMeters)

  val regressionSegments = Seq[RegressionSegment] (
    RegressionSegment(2.984, 0, 1), // regression constant
    RegressionSegment(0.6092, 1.417, numWorkersMainShift.value * 100 / floorArea),
    RegressionSegment(-0.0984, 2.726, numWorkersMainShift.value/licensedBedCapacity.value),
    RegressionSegment(0.4596 * hasLaundryFacility, 0, 1),
    RegressionSegment(9.3598e-06, 19004, min(100000,floorArea)),
    RegressionSegment(0.2775, 4.03, log(CDD.value+50) * percentCooled.value/100),
    RegressionSegment(0.00047986, 4787, HDD.value * percentHeated.value/100)

  )
}

/**
 * CanadaHospital companion object.  Contains built in JSON validation.
 */
object CanadaHospital {
  implicit val canadaHospitalReads: Reads[CanadaHospital] = Json.reads[CanadaHospital]
}



case class DataCenter(annualITEnergyKwh:PosDouble, country:String, reportingUnits:String,
                      GFA:PosDouble, areaUnits:String) extends BaseLine {

  val floorArea:Double = (Area((GFA.value,areaUnits)).get to SquareFeet)

  val regressionSegments = Seq[RegressionSegment] (
    RegressionSegment(2.984, 0, 1), // regression constant
    RegressionSegment(0.6092, 1.417, annualITEnergyKwh.value)

  )
}

/**
 * CanadaHospital companion object.  Contains built in JSON validation.
 */
object DataCenter {
  implicit val dataCenterReads: Reads[DataCenter] = Json.reads[DataCenter]
}