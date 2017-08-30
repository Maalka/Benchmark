
package models


import squants.energy._
import squants.space._
import scala.concurrent.Future
import scala.language._
import scala.math._
import play.api.libs.json._
import play.api.Play
import scala.concurrent.ExecutionContext.Implicits.global
import java.io.{InputStream}
import scala.util.control.NonFatal


case class EUIMetrics(parameters: JsValue) {


  val result = parameters.as[List[JsValue]]


  val combinedPropMetrics:CombinedPropTypes = CombinedPropTypes(parameters)
  val es:ES = ES(parameters)

  val energyCalcs:EUICalculator = EUICalculator(result.head)
  val buildingProps:BuildingProperties = BuildingProperties(result.head)
  val buildingEmissions:Emissions = Emissions(result.head)


  def getESScore:Future[Int] = es.getESScore
  def getTargetESScore:Future[Int] = es.getTargetESScore
  def getMedianESScore:Future[Int] = Future(50)


  def zepiMedian:Future[Int] = buildingProps.getBaselineConstant


  def percentBetterMedian:Future[Double] = Future{0}
  def percentBetterTarget:Future[Double] = buildingProps.getPercentBetterThanMedia
  def getBuildingName:Future[String] = Future{buildingProps.buildingName}



  //percentBetterActual METRICS .............................................................................................//
  def percentBetterActual:Future[Double] = {
    for {
      medianSiteEUI <- medianSiteEUIConverted
      actualEUI <- siteEUIConverted
    } yield {
      actualEUI match {
        case x if x < medianSiteEUI.value => 100*math.abs((1 - actualEUI / medianSiteEUI.value))
        case x  => -100*math.abs((1 - medianSiteEUI.value / actualEUI))
      }
    }
  }
  def percentBetterActualwOnSite:Future[Double] = {
    for {
      medianSiteEUI <- medianSiteEUIConverted
      actualEUI <- siteEUIwOnSiteConverted
    } yield {
      actualEUI match {
        case x if x < medianSiteEUI.value => 100*math.abs((1 - actualEUI / medianSiteEUI.value))
        case x  => -100*math.abs((1 - medianSiteEUI.value / actualEUI))
      }
    }
  }
  def percentBetterActualwOffSite:Future[Double] = {
    for {
      medianSiteEUI <- medianSiteEUIConverted
      actualEUI <- siteEUIwOffSiteConverted
    } yield {
      actualEUI match {
        case x if x < medianSiteEUI.value => 100*math.abs((1 - actualEUI / medianSiteEUI.value))
        case x  => -100*math.abs((1 - medianSiteEUI.value / actualEUI))
      }
    }
  }
  def percentBetterActualwOnandOffSite:Future[Double] = {
    for {
      medianSiteEUI <- medianSiteEUIConverted
      actualEUI <- siteEUIwOnandOffSiteConverted
    } yield {
      actualEUI match {
        case x if x < medianSiteEUI.value => 100*math.abs((1 - actualEUI / medianSiteEUI.value))
        case x  => -100*math.abs((1 - medianSiteEUI.value / actualEUI))
      }
    }
  }




  def actualGoalReduction:Future[Double] = {
    for {
      percentBetterEUI <- percentBetterSiteEUIConverted
      actualEUI <- siteEUIwOnandOffSiteConverted
    } yield 100*math.abs((1 - percentBetterEUI.value / actualEUI))
  }


  def actualGoalBetter:Future[Double] = {
    for {
      percentBetterEUI <- percentBetterSiteEUIConverted
      actualEUI <- siteEUIwOnandOffSiteConverted
    } yield 100*math.abs((1 - actualEUI / percentBetterEUI.value))
  }

  //ZEPI METRICS .............................................................................................//
  def zepiActual:Future[Double] = {
    for {
      baselineConstant <- buildingProps.getBaselineConstant
      zepiMedianSiteEUI <- medianSiteEUI
      actualSiteEUI <- siteEUI
      zepiActual <- Future(baselineConstant*actualSiteEUI.value/zepiMedianSiteEUI.value)
    } yield zepiActual
  }
/*

  def zepiActual:Future[Double] = {
    for {
      baselineConstant <- buildingProps.getBaselineConstant
      zepiMedianSiteEUI <- medianSiteEUIConverted
      actualSiteEUI <- siteEUIConverted
      zepiActual <- Future(baselineConstant*actualSiteEUI/zepiMedianSiteEUI.value)
    } yield zepiActual
  }
  def zepiActualwOnSite:Future[Double] = {
    for {
      baselineConstant <- buildingProps.getBaselineConstant
      zepiMedianSiteEUI <- medianSiteEUIConverted
      actualSiteEUI <- siteEUIwOnSiteConverted
      zepiActual <- Future(baselineConstant*actualSiteEUI/zepiMedianSiteEUI.value)
    } yield zepiActual
  }
  def zepiActualwOffSite:Future[Double] = {
    for {
      baselineConstant <- buildingProps.getBaselineConstant
      zepiMedianSiteEUI <- medianSiteEUIConverted
      actualSiteEUI <- siteEUIwOffSiteConverted
      zepiActual <- Future(baselineConstant*actualSiteEUI/zepiMedianSiteEUI.value)
    } yield zepiActual
  }
  def zepiActualwOnandOffSite:Future[Double] = {
    for {
      baselineConstant <- buildingProps.getBaselineConstant
      zepiMedianSiteEUI <- medianSiteEUIConverted
      actualSiteEUI <- siteEUIwOnandOffSiteConverted
      zepiActual <- Future(baselineConstant*actualSiteEUI/zepiMedianSiteEUI.value)
    } yield zepiActual
  }
*/



  def zepiPercentBetter:Future[Double] = {
    for {
      baselineConstant <- buildingProps.getBaselineConstant
      percentBetter <- buildingProps.getPercentBetterThanMedia
    } yield baselineConstant*(1-percentBetter/100)
  }

  //last pass conversions to the controller
  def sourceEnergyConverted: Future[Double] =
    for {
      sourceEnergy <- energyCalcs.sourceEnergynoPoolnoParking
      convertedEnergy <- energyConversion(sourceEnergy)
    } yield convertedEnergy.value

  def onSiteRenewableTotal: Future[Double] =
    for {
      siteRenewableList <- energyCalcs.getSiteRenewableEnergyList
      convertedEnergy <- convertEnergyTuple(siteRenewableList)
      totalOnSiteRenewable <- energyCalcs.getRenewableEnergyTotalbyTypeOutput(convertedEnergy,"onSite")
    } yield totalOnSiteRenewable

  def offSitePurchasedTotal: Future[Double] =
    for {
      siteRenewableList <- energyCalcs.getSiteRenewableEnergyList
      convertedEnergy <- convertEnergyTuple(siteRenewableList)
      totalOnSiteRenewable <- energyCalcs.getRenewableEnergyTotalbyTypeOutput(convertedEnergy,"purchased")
    } yield totalOnSiteRenewable
  //this is the total site energy without accounting for renewable generation and/or purchasing
  def siteEnergyALL: Future[Double] =
    for {
      siteEnergyList <- energyCalcs.getSiteEnergyList
      convertedEnergy <- convertEnergyTuple(siteEnergyList)
      totalSiteEnergyAll <- energyCalcs.getSiteEnergyTotalbyType(convertedEnergy)
    } yield  totalSiteEnergyAll

  //this is the total site energy accounting for renewable generation and/or purchasing
  def siteEnergyConverted: Future[Double] =
    for {
      siteTotalEnergy <- energyCalcs.getTotalSiteEnergy
      convertedEnergy <- energyConversion(siteTotalEnergy)
    } yield convertedEnergy.value

  def siteEnergyListConverted: Future[List[EnergyTuple]] =
    for {
      siteEnergyList <- energyCalcs.getSiteEnergyList
      convertedEnergy <- convertEnergyTuple(siteEnergyList)
    } yield convertedEnergy

  def sourceEnergyListConverted: Future[List[EnergyTuple]] =
    for {
      sourceEnergyList <- energyCalcs.getSourceEnergy
      convertedEnergy <- convertEnergyTuple(sourceEnergyList)
    } yield convertedEnergy

  //SOURCE EUI METRICS .............................................................................................//


  def sourceEUIConverted: Future[Double] =
    for {
      sourceEnergy <- energyCalcs.sourceEnergynoPoolnoParking
      sourceRenewableEnergy <- energyCalcs.getTotalSourceRenewableEnergy
      buildingSize <- combinedPropMetrics.getTotalArea(result)
      convertedEUI <- EUIConversionConstant(sourceEnergy+sourceRenewableEnergy,buildingSize)
    } yield {
      //println("SourceEUI",convertedEUI)
      convertedEUI.value
    }

  def sourceEUIwOnSiteConverted: Future[Double] =
    for {
      sourceEnergy <- energyCalcs.sourceEnergynoPoolnoParking
      buildingSize <- combinedPropMetrics.getTotalArea(result)

      siteRenewableList <- energyCalcs.getSiteRenewableEnergyList
      convertedEnergy <- convertEnergyTuple(siteRenewableList)
      renewableBalance <- energyCalcs.getRenewableBalance(siteRenewableList)
      totalOffSite <- energyCalcs.getRenewableEnergyTotalbyType(convertedEnergy,"purchased",renewableBalance)
      convertedEUI <- EUIConversionConstant(sourceEnergy+totalOffSite,buildingSize)
    } yield {
     // println("SourceEUI w OnSite",convertedEUI)
      convertedEUI.value
    }

  def sourceEUIwOffSiteConverted: Future[Double] =
    for {
      sourceEnergy <- energyCalcs.sourceEnergynoPoolnoParking
      buildingSize <- combinedPropMetrics.getTotalArea(result)

      siteRenewableList <- energyCalcs.getSiteRenewableEnergyList
      convertedEnergy <- convertEnergyTuple(siteRenewableList)
      renewableBalance <- energyCalcs.getRenewableBalance(siteRenewableList)
      totalOnSite <- energyCalcs.getRenewableEnergyTotalbyType(convertedEnergy,"onSite",renewableBalance)
      convertedEUI <- EUIConversionConstant(sourceEnergy+totalOnSite,buildingSize)
    } yield {
      //println("SourceEUI w Offsite",convertedEUI)
      convertedEUI.value
    }

  def sourceEUIwOnandOffSiteConverted: Future[Double] =
    for {
      sourceEnergy <- energyCalcs.sourceEnergynoPoolnoParking
      buildingSize <- combinedPropMetrics.getTotalArea(result)
      convertedEUI <- EUIConversionConstant(sourceEnergy,buildingSize)
    } yield {
      //println("SourceEUI w On and Offsite",convertedEUI)
      convertedEUI.value
    }




//SITE EUI METRICS .............................................................................................//

  def siteEUIConverted: Future[Double] =
    for {
      siteTotalEnergy <- energyCalcs.getTotalSiteEnergy
      siteRenewableEnergy <- energyCalcs.getTotalSiteRenewableEnergy
      buildingSize <- combinedPropMetrics.getTotalArea(result)
      convertedEUI <- EUIConversionConstant(siteTotalEnergy+siteRenewableEnergy,buildingSize)
    } yield {
      //println("SiteEUI...",convertedEUI)
      convertedEUI.value
    }

  def siteEUIwOnSiteConverted: Future[Double] =
    for {
      siteTotalEnergy <- energyCalcs.getTotalSiteEnergy
      buildingSize <- combinedPropMetrics.getTotalArea(result)

      siteRenewableList <- energyCalcs.getSiteRenewableEnergyList
      convertedEnergy <- convertEnergyTuple(siteRenewableList)
      renewableBalance <- energyCalcs.getRenewableBalance(siteRenewableList)
      totalOffSite <- energyCalcs.getRenewableEnergyTotalbyType(convertedEnergy,"purchased",renewableBalance)
      convertedEUI <- EUIConversionConstant(siteTotalEnergy+totalOffSite,buildingSize)
    } yield {
      //println("SiteEUI w OnSite",convertedEUI)
      convertedEUI.value
    }

  def siteEUIwOffSiteConverted: Future[Double] =
    for {
      siteTotalEnergy <- energyCalcs.getTotalSiteEnergy
      buildingSize <- combinedPropMetrics.getTotalArea(result)

      siteRenewableList <- energyCalcs.getSiteRenewableEnergyList
      convertedEnergy <- convertEnergyTuple(siteRenewableList)
      renewableBalance <- energyCalcs.getRenewableBalance(siteRenewableList)
      totalOnSite <- energyCalcs.getRenewableEnergyTotalbyType(convertedEnergy,"onSite",renewableBalance)
      convertedEUI <- EUIConversionConstant(siteTotalEnergy+totalOnSite,buildingSize)
    } yield {
      //println("SiteEUI w OffSite",convertedEUI)
      convertedEUI.value
    }

  def siteEUIwOnandOffSiteConverted: Future[Double] =
    for {
      siteTotalEnergy <- energyCalcs.getTotalSiteEnergy
      buildingSize <- combinedPropMetrics.getTotalArea(result)
      convertedEUI <- EUIConversionConstant(siteTotalEnergy,buildingSize)
    } yield {
      //println("SiteEUI w OnSite and OffSite",convertedEUI)
      convertedEUI.value
    }


  def medianSiteEnergyConverted:Future[Energy] = {
    for {
      siteRatio <- siteToSourceRatio
      backupRatio <- defaultSiteToSourceRatio
      sourceEnergy <- combinedPropMetrics.getWholeBuildingSourceMedianEnergy
      convertedEnergy <- energyConversion(sourceEnergy)
    } yield {
      result.head.asOpt[CountryBuildingType] match {
        case Some(CountryBuildingType(_, "SingleFamilyDetached")) => convertedEnergy * backupRatio
        case Some(CountryBuildingType(_, "SingleFamilyAttached")) => convertedEnergy * backupRatio
        case Some(CountryBuildingType(_, "MultiFamilyLessThan5")) => convertedEnergy * backupRatio
        case Some(CountryBuildingType(_, "MultiFamilyMoreThan4")) => convertedEnergy * backupRatio
        case Some(CountryBuildingType(_, "MobileHome")) => convertedEnergy * backupRatio
        case _ => {
          if(siteRatio.isNaN) {
            convertedEnergy * backupRatio
          }else {
            //convertedEnergy * siteRatio, this line will take actual site to source ratio based on actual building fuel use
            //breakdown, rather than default by fuel mixes from a lookuptable
            convertedEnergy * backupRatio
          }
        }
      }
    }
  }

  def medianSourceEnergyConverted:Future[Energy] = {
    for {
      sourceEnergy <- combinedPropMetrics.getWholeBuildingSourceMedianEnergy
      convertedEnergy <- energyConversion(sourceEnergy)
    } yield convertedEnergy
  }

  def medianSourceEUIConverted:Future[Energy] = {
    for {
      sourceEUI <- combinedPropMetrics.getWholeBuildingSourceMedianEUI
      conversionConstant <- EUIConversionNoUnitsConstant
    } yield sourceEUI*conversionConstant
  }

  def medianSiteEUIConverted:Future[Energy] = {
    for {
      siteEnergy <- medianSiteEnergy
      buildingSize <- combinedPropMetrics.getTotalArea(result)
      convertedEUI <- EUIConversionConstant(siteEnergy,buildingSize)
    } yield convertedEUI
  }

  def percentBetterSourceEUIConverted:Future[Energy] = {
    for {
      betterTarget <- buildingProps.getPercentBetterThanMedia
      medianEUI <- combinedPropMetrics.getWholeBuildingSourceMedianEUI
      targetEUI <- Future(medianEUI * (1 - betterTarget / 100.0))
      conversionConstant <- EUIConversionNoUnitsConstant
    } yield targetEUI*conversionConstant
  }

  def percentBetterSiteEUIConverted:Future[Energy] = {
    for {
      siteEnergy <- percentBetterSiteEnergy
      buildingSize <- combinedPropMetrics.getTotalArea(result)
      convertedEUI <- EUIConversionConstant(siteEnergy,buildingSize)
    } yield convertedEUI
  }

  def percentBetterSourceEnergyConverted:Future[Energy] = {
    for {
      sourceEnergy <- percentBetterSourceEUI
      buildingSize <- combinedPropMetrics.getTotalArea(result)
      convertedEnergy <- energyConversion(sourceEnergy)
    } yield convertedEnergy * buildingSize
  }

  def percentBetterSiteEnergyConverted:Future[Energy] = {
    for {
      siteRatio <- siteToSourceRatio
      backupRatio <- defaultSiteToSourceRatio
      sourceEnergy <- percentBetterSourceEnergy
      convertedEnergy <- energyConversion(sourceEnergy)
    } yield {
      result.head.asOpt[CountryBuildingType] match {
        case Some(CountryBuildingType(_, "SingleFamilyDetached")) => convertedEnergy * backupRatio
        case Some(CountryBuildingType(_, "SingleFamilyAttached")) => convertedEnergy * backupRatio
        case Some(CountryBuildingType(_, "MultiFamilyLessThan5")) => convertedEnergy * backupRatio
        case Some(CountryBuildingType(_, "MultiFamilyMoreThan4")) => convertedEnergy * backupRatio
        case Some(CountryBuildingType(_, "MobileHome")) => convertedEnergy * backupRatio
        case _ => {
          if(siteRatio.isNaN) {
            convertedEnergy * backupRatio
          }else {
            //convertedEnergy * siteRatio, this line will take actual site to source ratio based on actual building fuel use
            //breakdown, rather than default by fuel mixes from a lookuptable
            convertedEnergy * backupRatio
          }
        }
      }
    }
  }



//--------------------------- ABOVE IS THE EXIT POINT WITH FINAL CONVERSIONS ------------------------------------- //



  def getPropOutputList:Future[List[PropParams]] = {
    for {
      propTypes <- Future.sequence(result.map(BuildingProperties(_).getBuilding))
      propGFASum <- Future(propTypes.map(_.buildingSize).sum)
      conversionConstant <- GFAConversionConstant
      outputList <- Future{
        energyCalcs.reportingUnits match {
            case "us" => propTypes.map{
              a => PropParams(a.printed, a.buildingSize * conversionConstant, a.buildingSize / propGFASum, "ftSQ")
            }
            case "metric" => propTypes.map{
              a => PropParams(a.printed, a.buildingSize * conversionConstant, a.buildingSize / propGFASum, "mSQ")
            }
          }
        }
    } yield outputList
  }

  def sourceEnergy: Future[Energy] =
    for {
      sourceEnergy <- energyCalcs.sourceEnergynoPoolnoParking
    } yield sourceEnergy

  def siteEnergy: Future[Energy] =
    for {
      siteTotalEnergy <- energyCalcs.getTotalSiteEnergy
    } yield siteTotalEnergy

  def sourceEUI: Future[Energy] =
    for {
      sourceEnergy <- energyCalcs.sourceEnergynoPoolnoParking
      buildingSize <- combinedPropMetrics.getTotalArea(result)
    } yield sourceEnergy / buildingSize

  def siteEUI: Future[Energy] =
    for {
      siteTotalEnergy <- energyCalcs.getTotalSiteEnergy
      buildingSize <- combinedPropMetrics.getTotalArea(result)
    } yield siteTotalEnergy / buildingSize

  def medianSiteEnergy:Future[Energy] = {
    for {
      siteRatio <- siteToSourceRatio
      backupRatio <- defaultSiteToSourceRatio
      sourceEnergy <- combinedPropMetrics.getWholeBuildingSourceMedianEnergy
    } yield {
      result.head.asOpt[CountryBuildingType] match {
        case Some(CountryBuildingType(_, "SingleFamilyDetached")) => sourceEnergy * backupRatio
        case Some(CountryBuildingType(_, "SingleFamilyAttached")) => sourceEnergy * backupRatio
        case Some(CountryBuildingType(_, "MultiFamilyLessThan5")) => sourceEnergy * backupRatio
        case Some(CountryBuildingType(_, "MultiFamilyMoreThan4")) => sourceEnergy * backupRatio
        case Some(CountryBuildingType(_, "MobileHome")) => sourceEnergy * backupRatio
        case _ => {
          if(siteRatio.isNaN) {
            sourceEnergy * backupRatio
          }else {
            //convertedEnergy * siteRatio, this line will take actual site to source ratio based on actual building fuel use
            //breakdown, rather than default by fuel mixes from a lookuptable
            sourceEnergy * backupRatio
          }
        }
      }
    }
  }
  def medianSourceEnergy:Future[Energy] = {
    for {
      sourceEnergy <- combinedPropMetrics.getWholeBuildingSourceMedianEnergy
    } yield sourceEnergy
  }

  def medianSourceEUI:Future[Energy] = {
    for {
      sourceEUI <- combinedPropMetrics.getWholeBuildingSourceMedianEUI
    } yield sourceEUI
  }

  def medianSiteEUI:Future[Energy] = {
    for {
      siteEnergy <- medianSiteEnergy
      buildingSize <- combinedPropMetrics.getTotalArea(result)
    } yield siteEnergy / buildingSize
  }

  def percentBetterSourceEUI:Future[Energy] = {
    for {
      betterTarget <- buildingProps.getPercentBetterThanMedia
      medianEUI <- combinedPropMetrics.getWholeBuildingSourceMedianEUI
      targetEUI <- Future(medianEUI * (1 - betterTarget / 100.0))
      } yield targetEUI
  }

  def percentBetterSiteEUI:Future[Energy] = {
    for {
      siteEnergy <- percentBetterSiteEnergy
      buildingSize <- combinedPropMetrics.getTotalArea(result)
    } yield siteEnergy / buildingSize
  }

  def percentBetterSourceEnergy:Future[Energy] = {
    for {
      sourceEUI <- percentBetterSourceEUI
      buildingSize <- combinedPropMetrics.getTotalArea(result)
    } yield sourceEUI * buildingSize
  }

  def percentBetterSiteEnergy:Future[Energy] = {
    for {
      siteRatio <- siteToSourceRatio
      backupRatio <- defaultSiteToSourceRatio
      sourceEnergy <- percentBetterSourceEnergy
    } yield {
      result.head.asOpt[CountryBuildingType] match {
        case Some(CountryBuildingType(_, "SingleFamilyDetached")) => sourceEnergy * backupRatio
        case Some(CountryBuildingType(_, "SingleFamilyAttached")) => sourceEnergy * backupRatio
        case Some(CountryBuildingType(_, "MultiFamilyLessThan5")) => sourceEnergy * backupRatio
        case Some(CountryBuildingType(_, "MultiFamilyMoreThan4")) => sourceEnergy * backupRatio
        case Some(CountryBuildingType(_, "MobileHome")) => sourceEnergy * backupRatio
        case _ => {
          if(siteRatio.isNaN) {
            sourceEnergy * backupRatio
          }else {
            //convertedEnergy * siteRatio, this line will take actual site to source ratio based on actual building fuel use
            //breakdown, rather than default by fuel mixes from a lookuptable
            sourceEnergy * backupRatio
          }
        }
      }
    }
  }


  def medianTotalEmissions:Future[Double] = {
    val local = for {
      entries <- energyCalcs.getEnergyList
      tempsourceEnergy <- sourceEnergy
      medianSourceEnergy <- combinedPropMetrics.getWholeBuildingSourceMedianEnergy
      actualMedianRatio <- Future(tempsourceEnergy.value / medianSourceEnergy.value)
      actualEmissions <- buildingEmissions.getTotalEmissions(entries)
      medianEmissions <- Future(actualEmissions / actualMedianRatio)
    } yield medianEmissions

    local.recoverWith{case NonFatal(th) => defaultMedianTotalEmissions}
  }

  def percentBetterTotalEmissions:Future[Double] = {
    val local = for {
      entries <- energyCalcs.getEnergyList
      sourceEnergy <- sourceEnergy
      percentBetterSourceEnergy <- percentBetterSourceEnergy
      actualPercentBetterRatio <- Future(sourceEnergy.value / percentBetterSourceEnergy.value)
      actualEmissions <- buildingEmissions.getTotalEmissions(entries)
      percentBetterEmissions <- Future(actualEmissions / actualPercentBetterRatio)
    } yield percentBetterEmissions

    local.recoverWith{case NonFatal(th) => defaultPercentBetterTotalEmissions}
  }

  def defaultMedianTotalEmissions: Future[Double] ={
    for {
      propFilter <- combinedPropMetrics.majorProp
      stateBuildingType <- {
        propFilter.contains(true) match {
          case a if a==true => getMajorStateBuildingType(propFilter)
          case a if a==false => Future(StateBuildingType(buildingProps.state,"Other"))
        }
      }
      statePropEnergyMix <- getMix(stateBuildingType.state,stateBuildingType.buildingType)
      medianSiteEnergyEmissions <- medianSiteEnergy
      energyList <- getDefaultEnergyTotals(statePropEnergyMix, medianSiteEnergyEmissions)
      medianEmissions <-  buildingEmissions.nonActualTotalEmissions(energyList)
    } yield medianEmissions
  }

  def defaultPercentBetterTotalEmissions: Future[Double] ={
    for {
      propFilter <- combinedPropMetrics.majorProp
      stateBuildingType <- {
        propFilter.contains(true) match {
          case a if a==true => getMajorStateBuildingType(propFilter)
          case a if a==false => Future(StateBuildingType(buildingProps.state,"Other"))
        }
      }
      statePropEnergyMix <- getMix(stateBuildingType.state,stateBuildingType.buildingType)
      medianSiteEnergyEmissions <- percentBetterSiteEnergy
      energyList <- getDefaultEnergyTotals(statePropEnergyMix, medianSiteEnergyEmissions)
      percentBetterEmissions <- buildingEmissions.nonActualTotalEmissions(energyList)

    } yield percentBetterEmissions
  }


  def getDefaultEnergyTotals(defaultMix:Double, medianSourceEnergy:Energy): Future[EnergyList] = Future{

    val energyUnit = buildingProps.country match {
      case "USA" => "KBtu"
      case _ => "GJ"
    }

    EnergyList(
      List(EnergyMetrics("grid","Electric (Grid)",energyUnit,defaultMix*medianSourceEnergy.value,null),
        EnergyMetrics("naturalGas","Natural Gas",energyUnit,(1-defaultMix)*medianSourceEnergy.value,null))
    )
  }


  def getTotalEmissions(): Future[Double] = {
    for {
      entries <- energyCalcs.getEnergyList
      totalEmissions <- buildingEmissions.getTotalEmissions(entries)
    } yield totalEmissions
  }

  def getDirectEmissionList(): Future[List[EmissionsTuple]] = {
    for {
      entries <- energyCalcs.getEnergyList
      directEmissions <- buildingEmissions.getDirectEmissionList(entries)
    } yield directEmissions
  }
  def getIndirectEmissionList(): Future[List[EmissionsTuple]] = {
    for {
      entries <- energyCalcs.getEnergyList
      indirectEmissions <- buildingEmissions.getIndirectEmissionList(entries)
    } yield indirectEmissions
  }


  def siteToSourceRatio:Future[Double] = {

    val local = for {
      siteEnergy <- energyCalcs.getTotalSiteEnergy
      sourceEnergy <- energyCalcs.getTotalSourceEnergyNoPoolNoParking
      ratio <- Future {
        siteEnergy / sourceEnergy
      }
    } yield ratio

    local.recoverWith{case NonFatal(th) => defaultSiteToSourceRatio }
  }

  def defaultSiteToSourceRatio:Future[Double] = {
    val local = for {
      propFilter <- combinedPropMetrics.majorProp
      stateBuildingType <- {
        propFilter.contains(true) match {
          case a if a==true => getMajorStateBuildingType(propFilter)
          case a if a==false => Future(StateBuildingType(buildingProps.state,"Other"))
        }
      }
      statePropEnergyMix <- getMix(stateBuildingType.state,stateBuildingType.buildingType)
      defaultRatio <- {
        stateBuildingType.buildingType match {
          case "SingleFamilyDetached" => residentialSitetoSourceRatio(result.head)
          case "SingleFamilyAttached" => residentialSitetoSourceRatio(result.head)
          case "MultiFamilyLessThan5" => residentialSitetoSourceRatio(result.head)
          case "MultiFamilyMoreThan4" => residentialSitetoSourceRatio(result.head)
          case "MobileHome" => residentialSitetoSourceRatio(result.head)
          case _ => getDefaultRatio(statePropEnergyMix)
        }
      }

    } yield defaultRatio

    local.recoverWith{case NonFatal(th) => residentialSitetoSourceRatio(result.head)}
  }

  def getMajorStateBuildingType(propFilter:List[Boolean]):Future[StateBuildingType] = {
    for {
      majorProp <- combinedPropMetrics.getMajorProp(propFilter)
      stateBuildingType <- getStateBuildingType(majorProp)
    } yield stateBuildingType
  }



  def computeLookupEUI[T](targetBuilding: T): Future[Double] = Future{
    targetBuilding match {
      case a: GenericBuilding => throw new Exception("Lookup EUI could not be computed - Generic Building: No Algorithm!")
      case a: BaseLine => a.expectedEnergy
    }
  }


  def getTargetEUI[T](targetBuilding: T,lookupEUI:Double,targetRatio:Double):Future[Double] = Future {
    targetBuilding match {
      case a: ResidenceHall => exp(targetRatio / 15.717 * lookupEUI) / a.buildingSize
      case a: MedicalOffice => exp(targetRatio / 14.919 * lookupEUI) / a.buildingSize
      case a: DataCenter => targetRatio * lookupEUI * a.annualITEnergyKBtu
      case a:GenericBuilding => throw new Exception("Could not calculate Target EUI - Generic Building: No Algorithm!!")
      case a: BaseLine => targetRatio * lookupEUI
    }
  }



  def getStateBuildingType(params: JsValue): Future[StateBuildingType] = Future{
    params.asOpt[StateBuildingType] match {
      case Some(a) => a
      case _ => throw new Exception("Cannot find State and Building Type:")
    }
  }

  def getMix(state:String,propType:String):Future[Double] = {
    val local = for {
      mixLookup <- loadEnergyMixTable.map { case a => (a \ state \ propType).toOption }
      mixValue <- mixLookup match {
        case Some(a) => Future{a.as[Double]}
        case _ => throw new Exception("Could not find State and PropType in statePropertyEnergyMix.json")
      }
    } yield mixValue

    local.recoverWith{case NonFatal(th) => getDefaultMix(state)}
  }

  def getDefaultMix(state:String):Future[Double] = {
    for {
      mixLookup <- loadEnergyMixTable.map { case a => (a \ state \ "Other").toOption }
      mixValue <- mixLookup match {
        case Some(a) => Future{a.as[Double]}
        case _ => throw new Exception("Could not find Default energyMix in statePropertyEnergyMix.json")
      }
    } yield mixValue
  }

  def loadEnergyMixTable: Future[JsValue] = {
    for {
      is <- Future(Play.current.resourceAsStream("statePropertyEnergyMix.json"))
      json <- Future {
        is match {
          case Some(is: InputStream) => {
            Json.parse(is)
          }
          case i => throw new Exception("statePropEnergyMix - Could not open file: %s".format(i))
        }
      }
    } yield json
  }

  def getDefaultRatio(gridMix:Double):Future[Double] = Future{
    1 / (gridMix*siteToSourceConversions.gridUS + (1.0-gridMix)*siteToSourceConversions.ngUS)
  }


  def residentialSitetoSourceRatio(rezParams:JsValue):Future[Double] = Future {

    val countryBuilding = rezParams.asOpt[CountryBuildingType]
    val region: String = buildingProps.getRegion

    countryBuilding match {

      case Some(CountryBuildingType("USA", "SingleFamilyDetached")) =>
      {
        region match {
          case "West" => 38.4 / 67.2
          case "Midwest" => 49.5 / 76.2
          case "South" => 41.5 / 86
          case "Northeast" => 45.7 / 67.5
        }
      }
      case Some(CountryBuildingType("USA", "SingleFamilyAttached")) =>
      {
        region match {
          case "West" => 38.8 / 63.2
          case "Midwest" => 44.8 / 66.6
          case "South" => 38.8 / 82.5
          case "Northeast" => 50.3 / 68.6
        }
      }
      case Some(CountryBuildingType("USA", "MultiFamilyLessThan5")) =>
      {
        region match {
          case "West" => 47.6 / 87.3
          case "Midwest" => 74.0 / 104.8
          case "South" => 46.9 / 113.6
          case "Northeast" => 57.8 / 78.8
        }
      }
      case Some(CountryBuildingType("USA", "MultiFamilyMoreThan4")) =>
      {
        region match {
          case "West" => 40.0 / 81.7
          case "Midwest" => 50.9 / 93.3
          case "South" => 47.9 / 122.4
          case "Northeast" => 60.7 / 98.2
        }
      }
      case Some(CountryBuildingType("USA", "MobileHome")) =>
      {
        region match {
          case "West" => 65.8 / 128.2
          case "Midwest" => 103.3 / 168.9
          case "South" => 40.0 / 162.0
          case "Northeast" => 89.3 / 145.5
        }
      }


      //Canadian Building Medians
      case Some(CountryBuildingType("Canada", "AdultEducation")) => 1.18 / 1.44
      case Some(CountryBuildingType("Canada", "College")) => 0.76 / 1.56
      case Some(CountryBuildingType("Canada", "PreSchool")) => 0.92 / 1.27
      case Some(CountryBuildingType("Canada", "VocationalSchool")) => 1.18 / 1.44
      case Some(CountryBuildingType("Canada", "OtherEducation")) => 0.92 / 1.27
      case Some(CountryBuildingType("Canada", "ConventionCenter")) => 1.74 /2.47
      case Some(CountryBuildingType("Canada", "MovieTheater")) => 0.93 / 1.63
      case Some(CountryBuildingType("Canada", "Museum")) => 1.74 / 2.47
      case Some(CountryBuildingType("Canada", "PerformingArts")) => 1.74 / 2.47
      case Some(CountryBuildingType("Canada", "BowlingAlley")) => 1.51 / 1.93
      case Some(CountryBuildingType("Canada", "FitnessCenter")) => 1.51 / 1.93
      case Some(CountryBuildingType("Canada", "IceRink")) => 1.51 / 1.93
      case Some(CountryBuildingType("Canada", "RollerRink")) => 1.51 / 1.93
      case Some(CountryBuildingType("Canada", "SwimmingPool")) => 1.51 / 1.93
      case Some(CountryBuildingType("Canada", "OtherRecreation")) => 1.11 / 1.91
      case Some(CountryBuildingType("Canada", "MeetingHall")) => 1.74 / 2.47
      case Some(CountryBuildingType("Canada", "IndoorArena")) => 1.51 / 1.93
      case Some(CountryBuildingType("Canada", "RaceTrack")) => 1.11 / 1.91
      case Some(CountryBuildingType("Canada", "Stadium")) => 1.51 / 1.93
      case Some(CountryBuildingType("Canada", "Aquarium")) => 1.74 / 2.47
      case Some(CountryBuildingType("Canada", "Bar")) => 0.93 / 1.63
      case Some(CountryBuildingType("Canada", "NightClub")) => 0.93 / 1.63
      case Some(CountryBuildingType("Canada", "Casino")) => 0.93 / 1.63
      case Some(CountryBuildingType("Canada", "Zoo")) => 1.74 / 2.47
      case Some(CountryBuildingType("Canada", "OtherEntertainment")) => 1.74 / 2.47
      case Some(CountryBuildingType("Canada", "ConvenienceStore")) => 3.14 / 5.16
      case Some(CountryBuildingType("Canada", "GasStation")) => 3.14 / 5.16
      case Some(CountryBuildingType("Canada", "FastFoodRestaurant")) => 3.05 / 4.21
      case Some(CountryBuildingType("Canada", "Restaurant")) => 3.05 / 4.21
      case Some(CountryBuildingType("Canada", "OtherDining")) => 3.05 / 4.21
      case Some(CountryBuildingType("Canada", "FoodSales")) => 3.14 / 5.16
      case Some(CountryBuildingType("Canada", "FoodService")) => 3.05 / 4.21
      case Some(CountryBuildingType("Canada", "AmbulatorySurgicalCenter")) => 1.02 / 1.5
      case Some(CountryBuildingType("Canada", "DrinkingWaterTreatment")) => 0.63 / 1.84
      case Some(CountryBuildingType("Canada", "SpecialtyHospital")) => 2.35 / 3.12
      case Some(CountryBuildingType("Canada", "OutpatientCenter")) => 1.02 / 1.5
      case Some(CountryBuildingType("Canada", "PhysicalTherapyCenter")) => 1.02 / 1.5
      case Some(CountryBuildingType("Canada", "UrgentCareCenter")) => 1.02 / 1.5
      case Some(CountryBuildingType("Canada", "Barracks")) => 1.45 / 2.05
      case Some(CountryBuildingType("Canada", "Pr n")) => 1.28 / 1.74
      case Some(CountryBuildingType("Canada", "ResidentialLodging")) => 1.12 / 1.75
      case Some(CountryBuildingType("Canada", "MixedUse")) => 0.90 / 1.23
      case Some(CountryBuildingType("Canada", "VeterinaryOffice")) => 1.02 / 1.5
      case Some(CountryBuildingType("Canada", "Courthouse")) => 1.28 / 1.74
      case Some(CountryBuildingType("Canada", "FireStation")) => 1.23 / 1.63
      case Some(CountryBuildingType("Canada", "Library")) => 1.74 / 2.47
      case Some(CountryBuildingType("Canada", "MailingCenter")) => 1.37 / 1.67
      case Some(CountryBuildingType("Canada", "PostOffice")) => 1.37 / 1.67
      case Some(CountryBuildingType("Canada", "PoliceStation")) => 1.28 / 1.74
      case Some(CountryBuildingType("Canada", "TransportationTerminal")) => 1.06 / 1.42
      case Some(CountryBuildingType("Canada", "OtherPublicServices")) => 0.90 / 1.23
      case Some(CountryBuildingType("Canada", "AutoDealership")) => 0.85 / 1.52
      case Some(CountryBuildingType("Canada", "EnclosedMall")) => 2.10 / 3.47
      case Some(CountryBuildingType("Canada", "StripMall")) => 1.38 / 2.25
      case Some(CountryBuildingType("Canada", "Laboratory")) => 0.90 / 1.23
      case Some(CountryBuildingType("Canada", "PersonalServices")) => 1.00 / 1.37
      case Some(CountryBuildingType("Canada", "RepairServices")) => 1.00 / 1.37
      case Some(CountryBuildingType("Canada", "OtherServices")) => 1.37 / 2.2
      case Some(CountryBuildingType("Canada", "PowerStation")) => 0.90 / 1.23
      case Some(CountryBuildingType("Canada", "OtherUtility")) => 0.90 / 1.23
      case Some(CountryBuildingType("Canada", "SelfStorageFacility")) => 0.75 / 0.93
      // Canadian Building Medians for Buildings with US Algorithms
      case Some(CountryBuildingType("Canada","Hotel")) => 1.12 / 1.75
      case Some(CountryBuildingType("Canada","WorshipCenter")) => 0.86 / 1.06
      case Some(CountryBuildingType("Canada","Warehouse")) => 0.75 / 0.93
      case Some(CountryBuildingType("Canada","RefrigeratedWarehouse")) => 0.90 / 1.23 // there is no median for refrigerated warehouse in Canada (it's cold there!!)
      case Some(CountryBuildingType("Canada","SeniorCare")) => 1.12 / 1.88
      case Some(CountryBuildingType("Canada","Retail")) => 0.85 / 1.52
      case Some(CountryBuildingType("Canada","ResidenceHall")) => 1.45 / 2.05
      case Some(CountryBuildingType("Canada","DataCenter")) => 1.82 / 1.82

      case Some(CountryBuildingType("Canada", _)) => 0.90 / 1.23

      case _ => throw new Exception("Could not find Default Site to Source Ratio")
    }
  }

  def residentialSiteEUI(rezParams:JsValue):Future[Double] = Future {

    val countryBuilding = rezParams.asOpt[CountryBuildingType]
    val region: String = buildingProps.getRegion

    countryBuilding match {

      case Some(CountryBuildingType("USA", "SingleFamilyDetached")) =>
      {
        region match {
          case "West" => 38.4
          case "Midwest" => 49.5
          case "South" => 41.5
          case "Northeast" => 45.7
        }
      }
      case Some(CountryBuildingType("USA", "SingleFamilyAttached")) =>
      {
        region match {
          case "West" => 38.8
          case "Midwest" => 44.8
          case "South" => 38.8
          case "Northeast" => 50.3
        }
      }
      case Some(CountryBuildingType("USA", "MultiFamilyLessThan5")) =>
      {
        region match {
          case "West" => 47.6
          case "Midwest" => 74.0
          case "South" => 46.9
          case "Northeast" => 57.8
        }
      }
      case Some(CountryBuildingType("USA", "MultiFamilyMoreThan4")) =>
      {
        region match {
          case "West" => 40.0
          case "Midwest" => 50.9
          case "South" => 47.9
          case "Northeast" => 60.7
        }
      }
      case Some(CountryBuildingType("USA", "MobileHome")) =>
      {
        region match {
          case "West" => 65.8
          case "Midwest" => 103.3
          case "South" => 40.0
          case "Northeast" => 89.3
        }
      }

      case _ => throw new Exception("Could not find Residential Default Site to Source Ratio")
    }
  }

  def convertEnergyTuple(energies: List[EnergyTuple]): Future[List[EnergyTuple]] = Future {
    (energyCalcs.country, energyCalcs.reportingUnits) match {
      case ("USA", "us") => energies.map {case a:EnergyTuple => EnergyTuple(a.energyType,a.energyName,a.energyValue in KBtus)}
      case ("USA", "metric") => energies.map {case a:EnergyTuple => EnergyTuple(a.energyType,a.energyName,a.energyValue in KilowattHours)}
      case (_, "metric") => energies.map {case a:EnergyTuple => EnergyTuple(a.energyType,a.energyName,a.energyValue in KilowattHours)}
      case (_, "us") => energies.map {case a:EnergyTuple => EnergyTuple(a.energyType,a.energyName,a.energyValue in KBtus)}
      case _ => energies

    }
  }

  def EUIConversionConstant(energyEntry:Energy,areaEntry:Double):Future[Energy] = Future{
    (energyCalcs.country, energyCalcs.reportingUnits) match {
      case ("USA", "us") => energyEntry / areaEntry
      case ("USA", "metric") => (energyEntry in KilowattHours)/(areaEntry * (SquareFeet(1) to SquareMeters))
      case (_, "metric") => (energyEntry in KilowattHours) / areaEntry
      case (_, "us") => (energyEntry in KBtus) / (areaEntry * (SquareMeters(1) to SquareFeet))
      case _ => energyEntry / areaEntry
    }
  }
  def EUIConversionNoUnitsConstant:Future[Double] = Future{
    (energyCalcs.country, energyCalcs.reportingUnits) match {
      case ("USA", "us") => 1.0
      case ("USA", "metric") => (KBtus(1) to KilowattHours)/(SquareFeet(1) to SquareMeters)
      case (_, "metric") => (Gigajoules(1) to KilowattHours)
      case (_, "us") => (Gigajoules(1) to KBtus) / (SquareMeters(1) to SquareFeet)
      case _ => 1.0
    }
  }
  def energyConversion(energyEntry:Energy):Future[Energy] = Future{
    (energyCalcs.country, energyCalcs.reportingUnits) match {
      case ("USA", "us") => energyEntry
      case ("USA", "metric") => energyEntry in KilowattHours
      case (_, "metric") => energyEntry in KilowattHours
      case (_, "us") => energyEntry in KBtus
      case _ => energyEntry
    }
  }

  def GFAConversionConstant:Future[Double] = Future{
    (energyCalcs.country, energyCalcs.reportingUnits) match {
      case ("USA", "us") => 1.0
      case ("USA", "metric") => SquareFeet(1) to SquareMeters
      case (_, "metric") => 1.0
      case (_, "us") => SquareMeters(1) to SquareFeet
      case _ => 1.0
    }
  }

}










