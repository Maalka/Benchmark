
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

  val pvSystems:SolarProperties = SolarProperties(result.head)
  def getPV = pvSystems.setPVDefaults

  val prescriptiveEUI = PrescriptiveValues(result.head)
  def getPrescriptiveEndUses = prescriptiveEUI.lookupPrescriptiveEndUSeDistribution
  def getPrescriptiveElectricity = prescriptiveEUI.lookupPrescriptiveElectricityWeighted
  def getPrescriptiveNG = prescriptiveEUI.lookupPrescriptiveNGWeighted



  val combinedPropMetrics:CombinedPropTypes = CombinedPropTypes(parameters)
  val energyCalcs:EUICalculator = EUICalculator(result.head)
  val buildingProps:BuildingProperties = BuildingProperties(result.head)
  val buildingEmissions:Emissions = Emissions(result.head)


  def getBuildingName:Future[String] = Future{buildingProps.buildingName}


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
      totalOffSite <- energyCalcs.getRenewableEnergyTotalbyType(convertedEnergy,"purchased")
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
      totalOnSite <- energyCalcs.getRenewableEnergyTotalbyType(convertedEnergy,"onSite")
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
      totalOffSite <- energyCalcs.getRenewableEnergyTotalbyType(convertedEnergy,"purchased")
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
      totalOnSite <- energyCalcs.getRenewableEnergyTotalbyType(convertedEnergy,"onSite")
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










