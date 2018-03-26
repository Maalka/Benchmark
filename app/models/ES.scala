
package models

import play.api.libs.json.{JsValue, JsPath, Reads}
import play.api.{Configuration, Play}
import squants.energy.Energy
import play.api.libs.functional.syntax._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.math._

/**
 * Created by rimukas on 5/24/16.
 */
case class ES(parameters: JsValue, configuration: Configuration) {

  val result = parameters.as[List[JsValue]]
  val combinedPropMetrics:CombinedPropTypes = CombinedPropTypes(parameters, configuration)
  val energyCalcs:EUICalculator = EUICalculator(result.head)
  val buildingProps:BuildingProperties = BuildingProperties(result.head)


  def getESScore:Future[Int] = {
    for {
      nonTrivialGenericBuilding <- hasNonTrivialGenericBuilding
      lookUpES <- {
        nonTrivialGenericBuilding.isEmpty match {
          case true => getPropES
          case false => throw new Exception("Could not compute EnergyStar Score - Generic Building: No Algorithm!")
        }
      }
    } yield lookUpES
  }

  def getPropES:Future[Int] = {
    for {
      buildingList <- Future.sequence(result.map(BuildingProperties(_).getBuilding))
      nonGenericPropFilter <- getNonGenericBuildings(buildingList)
      nonGenericPropTypes <- combinedPropMetrics.filterPropTypes(nonGenericPropFilter)
      actualSourceEnergy <- energyCalcs.sourceEnergynoPoolnoParking
      computedES <- {
        buildingList.length match {
          case a if a == 1 => singleES(nonGenericPropTypes.head,actualSourceEnergy)
          case _ => getWeightedES(nonGenericPropTypes,actualSourceEnergy)
        }
      }
    } yield computedES
  }


  def getWeightedES(buildingList:List[JsValue],ESEnergy:Energy):Future[Int] = {
    for {
      buildingSize <- combinedPropMetrics.getTotalArea(result)
      filteredPropSize <- combinedPropMetrics.getTotalArea(buildingList)
      predictedEnergy <- combinedPropMetrics.getTotalPredictedEnergy(buildingList)
      weightedTable <- combinedPropMetrics.getWeightedTable(buildingList)
      futureRatio <- Future(weightedTable.dropWhile(_.Ratio < ESEnergy.value).headOption)
      checkRatio <- Future(weightedTable.lastOption)
    } yield {
      if (futureRatio.isDefined) {
        futureRatio.get.ES
      } else {
        checkRatio.get.ES
      }
    }
  }

  def getTargetESScore:Future[Int] = {
    for {
      nonTrivialGenericBuilding <- hasNonTrivialGenericBuilding
      lookUpES <- {
        nonTrivialGenericBuilding.isEmpty match {
          case true => getTargetES
          case false => throw new Exception("Could not compute EnergyStar Score - Generic Building: No Algorithm!")
        }
      }
    } yield lookUpES
  }

  def hasNonTrivialGenericBuilding:Future[List[Boolean]] = {
    for {
      buildingList <- Future.sequence(result.map(BuildingProperties(_).getBuilding))
      buildingSizeList <- Future(buildingList.map{case a:BaseLine => a.buildingSize})
      buildingSizeSum:Double <- Future(buildingSizeList.sum)
      buildingSizeRatios <- Future(buildingSizeList.map{_/buildingSizeSum})
      nonTrivialPropBooleans <-  Future(buildingSizeRatios.map{ case a => a > 0.25 })
      mediumPropList <- Future {
        (nonTrivialPropBooleans, buildingList).zipped.flatMap {
          case (a, b:GenericBuilding) if a == true => Some(true)
          case (_, _) => None
        }
      }

    } yield mediumPropList
  }

  def getNonGenericBuildings(buildingList:List[BaseLine]) = Future {
    buildingList.map {
      case a if a.isInstanceOf[GenericBuilding] == true => false
      case a if a.isInstanceOf[GenericBuilding] == false => true
    }
  }

  def getTargetES:Future[Int] = {
    for {
      buildingList <- Future.sequence(result.map(BuildingProperties(_).getBuilding))
      nonGenericPropFilter <- getNonGenericBuildings(buildingList)
      nonGenericPropTypes <- combinedPropMetrics.filterPropTypes(nonGenericPropFilter)
      targetEnergy <- EUIMetrics(parameters, configuration).percentBetterSourceEnergy
      targetEnergy <- EUIMetrics(parameters, configuration).percentBetterSourceEnergy
      computedES <- {
        buildingList.length match {
          case a if a == 1 => singleES(nonGenericPropTypes.head,targetEnergy)
          case _ => getWeightedES(nonGenericPropTypes,targetEnergy)
        }
      }
    } yield computedES
  }

  def singleES(params:JsValue,sourceESEnergy:Energy):Future[Int] = {
    for {
      predictedEnergy <- combinedPropMetrics.expectedSourceEnergy(params)
      buildingSize <- combinedPropMetrics.getTotalArea(result)
      targetBuilding <- BuildingProperties(params).getBuilding
      table <- combinedPropMetrics.lookupTableGet(targetBuilding)
      propTypeSize <- Future(targetBuilding.GFA)
      euiRatio <- Future{
        targetBuilding match {
          case a: ResidenceHall => log(sourceESEnergy.value) * 15.717 / log(predictedEnergy.value)
          case a: MedicalOffice => log(sourceESEnergy.value) * 14.919 / log(predictedEnergy.value)
          case a: DataCenter => sourceESEnergy.value / predictedEnergy.value
          case a: GenericBuilding => throw new Exception("Could not calculate EUI Ratio - Generic Building: No Algorithm!")
          case a: BaseLine => sourceESEnergy.value / buildingSize / predictedEnergy.value * propTypeSize.value
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
}

case class TargetES(target:Int)
object TargetES {
  implicit val TargetReads: Reads[TargetES] = (JsPath \ "targetScore").read[Int](Reads.min(0) andKeep
    Reads.max(100)).map(new TargetES(_))
}
