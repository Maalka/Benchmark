
package models

import play.api.libs.json.{JsValue, JsPath, Reads}
import squants.energy.Energy
import play.api.libs.functional.syntax._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.math._

/**
  * Created by rimukas on 5/24/16.
  */
case class ES(parameters: JsValue) {

  val result = parameters.as[List[JsValue]]

  val combinedPropMetrics:CombinedPropTypes = CombinedPropTypes(parameters)
  val energyCalcs:EUICalculator = EUICalculator(result.head)
  val buildingProps:BuildingProperties = BuildingProperties(result.head)


  def getESScore:Future[Int] = {
    for {
      majorPropType <- majorPropES
      hasGenericBuilding <- combinedPropMetrics.checkGenericBuilding
      lookUpES <- {
        majorPropType.contains(true) match {
          case true => {
            hasGenericBuilding match {
              case true => getGenericES
              case false => getPropES
            }
          }
          case false => {
            hasGenericBuilding match {
              case true => throw new Exception("Could not compute EnergyStar Score - Generic Building: No Algorithm!")
              case false => getPropES
            }
          }
        }
      }
    } yield lookUpES
  }

  def getGenericES:Future[Int] = {
    for {
      propFilter <- majorPropES
      majorProp <- combinedPropMetrics.getMajorProp(propFilter)
      targetBuilding <- BuildingProperties(majorProp).getBuilding
      propES <- {
        targetBuilding match {
          case a:GenericBuilding => throw new Exception("Could not compute EnergyStar Score - Generic Building: No Algorithm!")
          case b:BaseLine => singlePropES(majorProp)
        }
      }
    } yield propES
  }

  def majorPropES:Future[List[Boolean]] = {
    for {
      buildingSizeList <- Future.sequence(result.map(BuildingProperties(_).getBuilding).map(_.map{
        case a:BaseLine => a.buildingSize}
      ))
      buildingSizeSum:Double <- Future(buildingSizeList.sum)
      buildingSizeRatios <- Future(buildingSizeList.map{_/buildingSizeSum})
      majorPropType <-  Future(buildingSizeRatios.map{ case a => a > 0.75})
    } yield majorPropType
  }

  def getPropES:Future[Int] = {
    for {
      table <- combinedPropMetrics.getWeightedTable(result)
      predictedEnergy <- combinedPropMetrics.getTotalPredictedEnergy(result)
      actualSourceEnergy <- energyCalcs.sourceEnergynoPoolnoParking
      euiRatio <- Future(actualSourceEnergy/predictedEnergy)
      futureEntry <- Future(table.dropWhile(_.Ratio < euiRatio).headOption)
      checkEntry <- Future(table.lastOption)
    } yield
      {
        if (futureEntry.isDefined) {
          futureEntry.get.ES
        } else {
          checkEntry.get.ES
        }
      }
  }


  def singlePropES(params:JsValue):Future[Int] = {

    for {
      predictedEnergy <- combinedPropMetrics.expectedSourceEnergy(params)
      actualSourceEnergy <- energyCalcs.sourceEnergynoPoolnoParking
      buildingSize <- combinedPropMetrics.getTotalArea(result)
      targetBuilding <- BuildingProperties(params).getBuilding
      table <- combinedPropMetrics.lookupTableGet(targetBuilding)
      propTypeSize <- Future(targetBuilding.GFA)
      euiRatio <- Future{
        targetBuilding match {
          case a: ResidenceHall => log(actualSourceEnergy.value) * 15.717 / predictedEnergy.value * propTypeSize.value
          case a: MedicalOffice => log(actualSourceEnergy.value) * 14.919 / predictedEnergy.value * propTypeSize.value
          case a: DataCenter => actualSourceEnergy.value / a.annualITEnergyKBtu / predictedEnergy.value * propTypeSize.value
          case a: GenericBuilding => throw new Exception("Could not calculate EUI Ratio - Generic Building: No Algorithm!")
          case a: BaseLine => actualSourceEnergy.value / buildingSize / predictedEnergy.value * propTypeSize.value
        }
      }
      futureRatio <- Future(table.dropWhile(_.Ratio < euiRatio).headOption)
      checkRatio <- Future(table.lastOption)
    } yield
      {
        if (futureRatio.isDefined) {
          futureRatio.get.ES
        } else {
          checkRatio.get.ES
        }
      }
  }




 /* val targetSiteEnergy:Future[Double] = {
    for {
      siteRatio <- siteToSourceRatio
      sourceEnergy <- targetSourceEnergy
    } yield sourceEnergy * siteRatio
  }

  val targetSiteEUI:Future[Double] = {
    for {
      siteEnergy <- targetSiteEnergy
    } yield siteEnergy / targetBuilding.buildingSize
  }

  val targetSourceEUI:Future[Double] = {
    for {
      lookupEUI <- computeLookupEUI(targetBuilding)
      targetRatio <- getTargetRatio(parameters)
      targetEUI <-  getTargetEUI(targetBuilding,lookupEUI,targetRatio)
    } yield targetEUI
  }

  val targetSourceEnergy:Future[Double] = {
    for {
      siteRatio <- siteToSourceRatio
      sourceEnergy <- targetSourceEUI
    } yield sourceEnergy  * targetBuilding.buildingSize
  }

  val percentBetterES:Future[Int] = {
    for {

      lookupEUI <- computeLookupEUI(targetBuilding)
      sourceTotalEnergy <-  percentBetterSourceEUI
      euiRatio <- getTargetEUIratio(targetBuilding, lookupEUI, sourceTotalEnergy)
      lookUp <- getLookupTable
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


  def getTargetRatio(parameters:JsValue):Future[Double] = {
    for {
      targetES <- getTargetES
      lookUp <- getLookupTable
      targetRatioEntry <- loadLookupTable(lookUp).map {
        _.filter(_.ES == targetES).last.Ratio
      }
    } yield targetRatioEntry
  }

    def getTargetEUIratio[T](targetBuilding: T,lookupPredictedEUI:Double,sourceEUI:Double):Future[Double] = Future{
    targetBuilding match {
      case a: ResidenceHall => log(sourceEUI * a.buildingSize) * 15.717 / lookupPredictedEUI
      case a: MedicalOffice => log(sourceEUI * a.buildingSize) * 14.919 / lookupPredictedEUI
      case a: GenericBuilding => throw new Exception("Could not calculate Target EUI Ratio - Generic Building: No Algorithm!")
      case a: BaseLine => sourceEUI  / lookupPredictedEUI

    }
  }



  def getTargetES:Future[Int] = Future{
    parameters.asOpt[TargetES] match {
      case Some(a) => a.target
      case None => throw new Exception("Could not find your Target ES!")
    }
  }
*/
}

case class TargetES(target:Int)
object TargetES {
  implicit val TargetReads: Reads[TargetES] = (JsPath \ "targetScore").read[Int](Reads.min(0) andKeep
    Reads.max(100)).map(new TargetES(_))
}
