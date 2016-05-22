
package models


import squants.energy.{TBtus, Gigajoules, KBtus, Energy}
import squants.space._
import scala.concurrent.Future
import scala.language._
import scala.math._
import play.api.libs.json._
import play.api.Play
import scala.concurrent.ExecutionContext.Implicits.global
import java.io.{InputStream}
import play.api.libs.functional.syntax._

import scala.util.control.NonFatal


case class EUIMetrics(parameters: JsValue) {

  val energyCalcs:EUICalculator = EUICalculator(parameters)
  val countryBuilding = parameters.asOpt[CountryBuildingType]
  val targetBuilding:BaseLine = getBuilding

  val getBuildingClass: Future[String] = Future{targetBuilding.getClass.toString}


  val sourceEUI: Future[Double] =
    for {
      poolEnergy <- energyCalcs.getPoolEnergy
      parkingEnergy <- energyCalcs.getParkingEnergy
      sourceTotalEnergy <- energyCalcs.getTotalSourceEnergy
      sourceEUI <- sourceEUInoPoolnoParking(sourceTotalEnergy, poolEnergy, parkingEnergy)
    } yield sourceEUI.value / targetBuilding.buildingSize

  val siteEUI: Future[Double] =
    for {
      siteTotalEnergy <- energyCalcs.getTotalSiteEnergy
    } yield siteTotalEnergy.value / targetBuilding.buildingSize

  val buildingGFA: Future[Double] = Future{targetBuilding.buildingSize}
  val sourceSiteRatio: Future[Double] = siteToSourceRatio


  val expectedSourceEnergy:Future[Energy] = {
    for {
      expectedEUI <- computeExpectedEUI(targetBuilding)
    } yield expectedEUI
  }

  val ES:Future[Int] = {

    for {
      lookupEUI <- computeLookupEUI(targetBuilding)
      poolEnergy <- energyCalcs.getPoolEnergy
      parkingEnergy <- energyCalcs.getParkingEnergy
      sourceTotalEnergy <-  energyCalcs.getTotalSourceEnergy
      euiRatio <- getEUIratio(targetBuilding, lookupEUI, (sourceTotalEnergy - poolEnergy - parkingEnergy).value)
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

  val targetSiteEnergy:Future[Double] = {
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


  val medianSourceEUI:Future[Double] = {
    for {
      medianEUI <- {
        targetBuilding match {
          case a:GenericBuilding =>  Future(sourceMedianEUI.value)
          case b:BaseLine => for {
            lookupEUI <- computeLookupEUI(targetBuilding)
            targetRatio <- getMedianRatio(parameters)
            targetEUI <-  getTargetEUI(targetBuilding,lookupEUI,targetRatio)
          } yield targetEUI
        }
      }
    } yield medianEUI
  }

  val medianSourceEnergy:Future[Double] = {
    for {
      sourceEnergy <- medianSourceEUI
    } yield sourceEnergy * targetBuilding.buildingSize
  }

  val medianSiteEnergy:Future[Double] = {
    for {
      siteRatio <- siteToSourceRatio
      sourceEnergy <- medianSourceEnergy
    } yield sourceEnergy * siteRatio
  }

  val medianSiteEUI:Future[Double] = {
    for {
      siteEnergy <- medianSiteEnergy
    } yield siteEnergy / targetBuilding.buildingSize
  }

  val percentBetterSourceEUI:Future[Double] = {
    for {
      betterTarget <- Future {parameters.validate[PercentBetterThanMedian]}
      medianEUI <- medianSourceEUI
      targetEUI <- {
        betterTarget match {
          case JsSuccess(a, _) => Future(medianEUI * (1 - a.target / 100.0))
          case JsError(err) => throw new Exception("Could not determine target EUI!")
        }
      }
    } yield targetEUI
  }


  val percentBetterSourceEnergy:Future[Double] = {
    for {
      sourceEnergy <- percentBetterSourceEUI
    } yield sourceEnergy * targetBuilding.buildingSize
  }

  val percentBetterSiteEnergy:Future[Double] = {
    for {
      siteRatio <- siteToSourceRatio
      sourceEnergy <- percentBetterSourceEnergy
    } yield sourceEnergy * siteRatio
  }

  val percentBetterSiteEUI:Future[Double] = {
    for {
      siteEnergy <- percentBetterSiteEnergy
    } yield siteEnergy / targetBuilding.buildingSize
  }

  val percentBetterES:Future[Int] = {
    for {
    //targetBuilding <- getBuilding(parameters)
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
      stateBuildingType <- getStateBuildingType(parameters)
      statePropEnergyMix <- EnergyMix.getMix(stateBuildingType.state,stateBuildingType.buildingType)
      defaultRatio <- EnergyMix.getDefaultRatio(statePropEnergyMix)
    } yield defaultRatio

    local.recoverWith{case NonFatal(th) => residentialSitetoSourceRatio}
  }


  def sourceEUInoPoolnoParking(sourceTotalEnergy:Energy, poolEnergy:Energy,
                               parkingEnergy:Energy):Future[Energy] = Future(sourceTotalEnergy - poolEnergy - parkingEnergy)


  def computeExpectedEUI[T](targetBuilding: T): Future[Energy] = Future{
    val unitlessEnergy = targetBuilding match {
      case a: ResidenceHall => exp(a.expectedEnergy)
      case a: MedicalOffice => exp(a.expectedEnergy)
      case a: GenericBuilding => throw new Exception("Cannot compute Expected Energy - Generic Building: No Algorithm!")
      case a: BaseLine => a.expectedEnergy * a.buildingSize
    }
    countryBuilding match {
      case Some(CountryBuildingType("USA", _)) => KBtus(unitlessEnergy)
      case Some(CountryBuildingType("Canada", _)) => Gigajoules(unitlessEnergy)
      case _ => throw new Exception("Cannot compute Expected Energy - Generic Building: No Algorithm!")
    }
  }

  def getEUIratio[T](targetBuilding: T,lookupPredictedEUI:Double,sourceEnergy:Double):Future[Double] = Future{

    targetBuilding match {
      case a: ResidenceHall => log(sourceEnergy) * 15.717 / lookupPredictedEUI
      case a: MedicalOffice => log(sourceEnergy) * 14.919 / lookupPredictedEUI
      case a: DataCenter => sourceEnergy / a.annualITEnergyKBtu / lookupPredictedEUI
      case a: GenericBuilding => throw new Exception("Could not calculate EUI Ratio - Generic Building: No Algorithm!")
      case a: BaseLine => sourceEnergy / a.buildingSize / lookupPredictedEUI
    }
  }


  def getTargetEUIratio[T](targetBuilding: T,lookupPredictedEUI:Double,sourceEUI:Double):Future[Double] = Future{
    targetBuilding match {
      case a: ResidenceHall => log(sourceEUI * a.buildingSize) * 15.717 / lookupPredictedEUI
      case a: MedicalOffice => log(sourceEUI * a.buildingSize) * 14.919 / lookupPredictedEUI
      case a: GenericBuilding => throw new Exception("Could not calculate Target EUI Ratio - Generic Building: No Algorithm!")
      case a: BaseLine => sourceEUI  / lookupPredictedEUI

    }
  }

  def computeLookupEUI[T](targetBuilding: T): Future[Double] = Future{
    targetBuilding match {
      case a: GenericBuilding => throw new Exception("Lookup EUI could not be computed - Generic Building: No Algorithm!")
      case a: BaseLine => a.expectedEnergy
    }
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


  def getMedianRatio(parameters:JsValue):Future[Double] = {
    for {
      lookUp <- getLookupTable
      targetRatioEntry <- loadLookupTable(lookUp).map {
        _.filter(_.ES == 50).last.Ratio
      }
    } yield targetRatioEntry
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

  def getStateBuildingType(parameters: JsValue): Future[StateBuildingType] = Future{
    parameters.asOpt[StateBuildingType] match {
      case Some(a) => a
      case _ => throw new Exception("Cannot find State and Building Type:")
    }
  }

  def getLookupTable(): Future[String] = {

    val r = countryBuilding match {
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
      case Some(CountryBuildingType("USA", "DataCenter")) => Play.current.configuration.getString("baseline.datacenter")
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

  def getBuilding(): BaseLine = {
    val building:JsResult[BaseLine] = countryBuilding match {
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
      case Some(_) => parameters.validate[GenericBuilding]
      case None => JsError("Could not find country or buildingType fields with JSON")
    }
    building match {
      case JsSuccess(a: BaseLine, _) => a
      case JsError(err) => throw new Exception("Building Type parameters fail validation!")
    }
  }


  def sourceMedianEUI():Energy = {

    val region:String = getRegion(parameters)

    val sourceMedian:Double = {
      countryBuilding match {
        case Some(CountryBuildingType("USA","AdultEducation")) => 141.4
        case Some(CountryBuildingType("USA","College")) => 262.6
        case Some(CountryBuildingType("USA","PreSchool")) => 145.7
        case Some(CountryBuildingType("USA","DataCenter")) => 1.821 //this = Total Energy / IT Energy
        case Some(CountryBuildingType("USA","VocationalSchool")) => 141.4
        case Some(CountryBuildingType("USA","OtherEducation")) => 141.4
        case Some(CountryBuildingType("USA","ConventionCenter")) => 69.8
        case Some(CountryBuildingType("USA","MovieTheater")) => 85.1
        case Some(CountryBuildingType("USA","Museum")) => 85.1
        case Some(CountryBuildingType("USA","PerformingArts")) => 85.1
        case Some(CountryBuildingType("USA","BowlingAlley")) => 96.8
        case Some(CountryBuildingType("USA","FitnessCenter")) => 96.8
        case Some(CountryBuildingType("USA","IceRink")) => 96.8
        case Some(CountryBuildingType("USA","RollerRink")) => 96.8
        case Some(CountryBuildingType("USA","SwimmingPool")) => 96.8
        case Some(CountryBuildingType("USA","OtherRecreation")) => 96.8
        case Some(CountryBuildingType("USA","MeetingHall")) => 69.8
        case Some(CountryBuildingType("USA","IndoorArena")) => 85.1
        case Some(CountryBuildingType("USA","RaceTrack")) => 85.1
        case Some(CountryBuildingType("USA","Stadium")) => 85.1
        case Some(CountryBuildingType("USA","Aquarium")) => 85.1
        case Some(CountryBuildingType("USA","Bar")) => 85.1
        case Some(CountryBuildingType("USA","NightClub")) => 85.1
        case Some(CountryBuildingType("USA","Casino")) => 85.1
        case Some(CountryBuildingType("USA","Zoo")) => 85.1
        case Some(CountryBuildingType("USA","OtherEntertainment")) => 85.1
        case Some(CountryBuildingType("USA","ConvenienceStore")) => 536.3
        case Some(CountryBuildingType("USA","GasStation")) => 536.3
        case Some(CountryBuildingType("USA","FastFoodRestaurant")) => 1015.3
        case Some(CountryBuildingType("USA","Restaurant")) => 432.0
        case Some(CountryBuildingType("USA","OtherDining")) => 432.0
        case Some(CountryBuildingType("USA","FoodSales")) => 536.3
        case Some(CountryBuildingType("USA","FoodService")) => 543.2
        case Some(CountryBuildingType("USA","AmbulatorySurgicalCenter")) => 155.2
        case Some(CountryBuildingType("USA","DrinkingWaterTreatment")) => 6.61
        case Some(CountryBuildingType("USA","SpecialtyHospital")) => 389.8
        case Some(CountryBuildingType("USA","OutpatientCenter")) => 155.2
        case Some(CountryBuildingType("USA","PhysicalTherapyCenter")) => 155.2
        case Some(CountryBuildingType("USA","UrgentCareCenter")) => 182.7
        case Some(CountryBuildingType("USA","Barracks")) => 114.9
        case Some(CountryBuildingType("USA","Prison")) => 169.9
        case Some(CountryBuildingType("USA","ResidentialLodging")) => 155.5
        case Some(CountryBuildingType("USA","MixedUse")) => 123.1
        case Some(CountryBuildingType("USA","VeterinaryOffice")) => 182.7
        case Some(CountryBuildingType("USA","Courthouse")) => 169.9
        case Some(CountryBuildingType("USA","FireStation")) => 154.4
        case Some(CountryBuildingType("USA","Library")) => 235.6
        case Some(CountryBuildingType("USA","MailingCenter")) => 100.4
        case Some(CountryBuildingType("USA","PostOffice")) => 100.4
        case Some(CountryBuildingType("USA","PoliceStation")) => 154.4
        case Some(CountryBuildingType("USA","TransportationTerminal")) => 85.1
        case Some(CountryBuildingType("USA","OtherPublicServices")) => 123.1
        case Some(CountryBuildingType("USA","AutoDealership")) => 130.1
        case Some(CountryBuildingType("USA","EnclosedMall")) => 235.6
        case Some(CountryBuildingType("USA","StripMall")) => 237.6
        case Some(CountryBuildingType("USA","Laboratory")) => 123.1
        case Some(CountryBuildingType("USA","PersonalServices")) => 100.4
        case Some(CountryBuildingType("USA","RepairServices")) => 100.4
        case Some(CountryBuildingType("USA","OtherServices")) => 100.4
        case Some(CountryBuildingType("USA","PowerStation")) => 123.1
        case Some(CountryBuildingType("USA","OtherUtility")) => 123.1
        case Some(CountryBuildingType("USA","SelfStorageFacility")) => 47.6

        case Some(CountryBuildingType("USA","SingleFamilyDetached")) => {
          region match {
            case "West" => 67.2
            case "Midwest" => 76.2
            case "South" => 86
            case "Northeast" => 67.5
          }
        }
        case Some(CountryBuildingType("USA","SingleFamilyAttached")) => {
            region match {
              case "West" => 63.2
              case "Midwest" => 66.6
              case "South" => 82.5
              case "Northeast" => 68.6
            }
          }
        case Some(CountryBuildingType("USA","MultiFamilyLessThan5")) => {
              region match {
                case "West" => 87.3
                case "Midwest" => 104.8
                case "South" => 113.6
                case "Northeast" => 78.8
              }
            }
        case Some(CountryBuildingType("USA","MultiFamilyMoreThan4")) => {
              region match {
                case "West" => 81.7
                case "Midwest" => 93.3
                case "South" => 122.4
                case "Northeast" => 98.2
              }
            }
        case Some(CountryBuildingType("USA","MobileHome")) => {
          region match {
            case "West" => 128.2
            case "Midwest" => 168.9
            case "South" => 162.0
            case "Northeast" => 145.5
          }
        }
        case Some(CountryBuildingType("USA",_)) => 123.1

        //Canadian Building Medians
        case Some(CountryBuildingType("Canada","AdultEducation")) => 1.44
        case Some(CountryBuildingType("Canada","College")) => 1.56
        case Some(CountryBuildingType("Canada","PreSchool")) => 1.27
        case Some(CountryBuildingType("Canada","VocationalSchool")) => 1.44
        case Some(CountryBuildingType("Canada","OtherEducation")) => 1.27
        case Some(CountryBuildingType("Canada","ConventionCenter")) => 2.47
        case Some(CountryBuildingType("Canada","MovieTheater")) => 1.63
        case Some(CountryBuildingType("Canada","Museum")) => 2.47
        case Some(CountryBuildingType("Canada","PerformingArts")) => 2.47
        case Some(CountryBuildingType("Canada","BowlingAlley")) => 1.93
        case Some(CountryBuildingType("Canada","FitnessCenter")) => 1.93
        case Some(CountryBuildingType("Canada","IceRink")) => 1.93
        case Some(CountryBuildingType("Canada","RollerRink")) => 1.93
        case Some(CountryBuildingType("Canada","SwimmingPool")) => 1.93
        case Some(CountryBuildingType("Canada","OtherRecreation")) => 1.91
        case Some(CountryBuildingType("Canada","MeetingHall")) => 2.47
        case Some(CountryBuildingType("Canada","IndoorArena")) => 1.93
        case Some(CountryBuildingType("Canada","RaceTrack")) => 1.91
        case Some(CountryBuildingType("Canada","Stadium")) => 1.93
        case Some(CountryBuildingType("Canada","Aquarium")) => 2.47
        case Some(CountryBuildingType("Canada","Bar")) => 1.63
        case Some(CountryBuildingType("Canada","NightClub")) => 1.63
        case Some(CountryBuildingType("Canada","Casino")) => 1.63
        case Some(CountryBuildingType("Canada","Zoo")) => 2.47
        case Some(CountryBuildingType("Canada","OtherEntertainment")) => 2.47
        case Some(CountryBuildingType("Canada","ConvenienceStore")) => 5.16
        case Some(CountryBuildingType("Canada","GasStation")) => 5.16
        case Some(CountryBuildingType("Canada","FastFoodRestaurant")) => 4.21
        case Some(CountryBuildingType("Canada","Restaurant")) => 4.21
        case Some(CountryBuildingType("Canada","OtherDining")) => 4.21
        case Some(CountryBuildingType("Canada","FoodSales")) => 5.16
        case Some(CountryBuildingType("Canada","FoodService")) => 4.21
        case Some(CountryBuildingType("Canada","AmbulatorySurgicalCenter")) => 1.5
        case Some(CountryBuildingType("Canada","DrinkingWaterTreatment")) => 1.84
        case Some(CountryBuildingType("Canada","SpecialtyHospital")) => 3.12
        case Some(CountryBuildingType("Canada","OutpatientCenter")) => 1.5
        case Some(CountryBuildingType("Canada","PhysicalTherapyCenter")) => 1.5
        case Some(CountryBuildingType("Canada","UrgentCareCenter")) => 1.5
        case Some(CountryBuildingType("Canada","Barracks")) => 2.05
        case Some(CountryBuildingType("Canada","Prison")) => 1.74
        case Some(CountryBuildingType("Canada","ResidentialLodging")) => 1.75
        case Some(CountryBuildingType("Canada","MixedUse")) => 1.23
        case Some(CountryBuildingType("Canada","VeterinaryOffice")) => 1.5
        case Some(CountryBuildingType("Canada","Courthouse")) => 1.74
        case Some(CountryBuildingType("Canada","FireStation")) => 1.63
        case Some(CountryBuildingType("Canada","Library")) => 2.47
        case Some(CountryBuildingType("Canada","MailingCenter")) => 1.67
        case Some(CountryBuildingType("Canada","PostOffice")) => 1.67
        case Some(CountryBuildingType("Canada","PoliceStation")) => 1.74
        case Some(CountryBuildingType("Canada","TransportationTerminal")) => 1.42
        case Some(CountryBuildingType("Canada","OtherPublicServices")) => 1.23
        case Some(CountryBuildingType("Canada","AutoDealership")) => 1.52
        case Some(CountryBuildingType("Canada","EnclosedMall")) => 3.47
        case Some(CountryBuildingType("Canada","StripMall")) => 2.25
        case Some(CountryBuildingType("Canada","Laboratory")) => 1.23
        case Some(CountryBuildingType("Canada","PersonalServices")) => 1.37
        case Some(CountryBuildingType("Canada","RepairServices")) => 1.37
        case Some(CountryBuildingType("Canada","OtherServices")) => 2.20
        case Some(CountryBuildingType("Canada","PowerStation")) => 1.23
        case Some(CountryBuildingType("Canada","OtherUtility")) => 1.23
        case Some(CountryBuildingType("Canada","SelfStorageFacility")) => 0.93
        // Canadian Building Medians for Buildings with US Algorithms
        case Some(CountryBuildingType("Canada","Hotel")) => 1.75
        case Some(CountryBuildingType("Canada","WorshipCenter")) => 1.06
        case Some(CountryBuildingType("Canada","Warehouse")) => 0.93
        case Some(CountryBuildingType("Canada","SeniorCare")) => 1.88
        case Some(CountryBuildingType("Canada","Retail")) => 1.52
        case Some(CountryBuildingType("Canada","ResidenceHall")) => 2.05
        case Some(CountryBuildingType("Canada","DataCenter")) => 1.82 //this = Total Energy / IT Energy

        case Some(CountryBuildingType("Canada",_)) => 1.23

        case Some(_) => throw new Exception("Could not find Country and Building Type for Median EUI")
        case None => throw new Exception("Could not find Country and Building Type for Median EUI")
      }
    }

   countryBuilding match {
      case Some(CountryBuildingType("USA", _)) => KBtus(sourceMedian)
      case Some(CountryBuildingType("Canada", _)) => Gigajoules(sourceMedian)
      case Some(CountryBuildingType(_, _)) => throw new Exception("Could not find Country and Building Type for Median EUI")
      case None => throw new Exception("Could not find Country and Building Type for Median EUI")
    }
  }

  def siteMedianEUI():Energy = {

    val region:String = getRegion(parameters)

    val siteMedian: Double = {
      countryBuilding match {
        case Some(CountryBuildingType("USA", "AdultEducation")) => 59.6
        case Some(CountryBuildingType("USA", "College")) => 130.7
        case Some(CountryBuildingType("USA", "PreSchool")) => 70.9
        case Some(CountryBuildingType("USA", "DataCenter")) => 1.821
        case Some(CountryBuildingType("USA", "VocationalSchool")) => 59.6
        case Some(CountryBuildingType("USA", "OtherEducation")) => 59.6
        case Some(CountryBuildingType("USA", "ConventionCenter")) => 45.3
        case Some(CountryBuildingType("USA", "MovieTheater")) => 45.3
        case Some(CountryBuildingType("USA", "Museum")) => 45.3
        case Some(CountryBuildingType("USA", "PerformingArts")) => 45.3
        case Some(CountryBuildingType("USA", "BowlingAlley")) => 41.2
        case Some(CountryBuildingType("USA", "FitnessCenter")) => 41.2
        case Some(CountryBuildingType("USA", "IceRink")) => 41.2
        case Some(CountryBuildingType("USA", "RollerRink")) => 41.2
        case Some(CountryBuildingType("USA", "SwimmingPool")) => 41.2
        case Some(CountryBuildingType("USA", "OtherRecreation")) => 41.2
        case Some(CountryBuildingType("USA", "MeetingHall")) => 45.3
        case Some(CountryBuildingType("USA", "IndoorArena")) => 45.3
        case Some(CountryBuildingType("USA", "RaceTrack")) => 45.3
        case Some(CountryBuildingType("USA", "Stadium")) => 45.3
        case Some(CountryBuildingType("USA", "Aquarium")) => 45.3
        case Some(CountryBuildingType("USA", "Bar")) => 45.3
        case Some(CountryBuildingType("USA", "NightClub")) => 45.3
        case Some(CountryBuildingType("USA", "Casino")) => 45.3
        case Some(CountryBuildingType("USA", "Zoo")) => 45.3
        case Some(CountryBuildingType("USA", "OtherEntertainment")) => 45.3
        case Some(CountryBuildingType("USA", "ConvenienceStore")) => 192.9
        case Some(CountryBuildingType("USA", "GasStation")) => 192.9
        case Some(CountryBuildingType("USA", "FastFoodRestaurant")) => 384.0
        case Some(CountryBuildingType("USA", "Restaurant")) => 223.8
        case Some(CountryBuildingType("USA", "OtherDining")) => 223.8
        case Some(CountryBuildingType("USA", "FoodSales")) => 192.9
        case Some(CountryBuildingType("USA", "FoodService")) => 266.8
        case Some(CountryBuildingType("USA", "AmbulatorySurgicalCenter")) => 63.0
        case Some(CountryBuildingType("USA", "DrinkingWaterTreatment")) => 2.27
        case Some(CountryBuildingType("USA", "SpecialtyHospital")) => 196.9
        case Some(CountryBuildingType("USA", "OutpatientCenter")) => 63.0
        case Some(CountryBuildingType("USA", "PhysicalTherapyCenter")) => 63.0
        case Some(CountryBuildingType("USA", "UrgentCareCenter")) => 66.8
        case Some(CountryBuildingType("USA", "Barracks")) => 73.9
        case Some(CountryBuildingType("USA", "Prison")) => 93.2
        case Some(CountryBuildingType("USA", "ResidentialLodging")) => 73.4
        case Some(CountryBuildingType("USA", "MixedUse")) => 78.8
        case Some(CountryBuildingType("USA", "VeterinaryOffice")) => 66.8
        case Some(CountryBuildingType("USA", "Courthouse")) => 93.2
        case Some(CountryBuildingType("USA", "FireStation")) => 88.3
        case Some(CountryBuildingType("USA", "Library")) => 91.6
        case Some(CountryBuildingType("USA", "MailingCenter")) => 49.6
        case Some(CountryBuildingType("USA", "PostOffice")) => 49.6
        case Some(CountryBuildingType("USA", "PoliceStation")) => 88.3
        case Some(CountryBuildingType("USA", "TransportationTerminal")) => 45.3
        case Some(CountryBuildingType("USA", "OtherPublicServices")) => 78.8
        case Some(CountryBuildingType("USA", "AutoDealership")) => 52.5
        case Some(CountryBuildingType("USA", "EnclosedMall")) => 93.7
        case Some(CountryBuildingType("USA", "StripMall")) => 94.2
        case Some(CountryBuildingType("USA", "Laboratory")) => 78.8
        case Some(CountryBuildingType("USA", "PersonalServices")) => 49.6
        case Some(CountryBuildingType("USA", "RepairServices")) => 49.6
        case Some(CountryBuildingType("USA", "OtherServices")) => 49.6
        case Some(CountryBuildingType("USA", "PowerStation")) => 78.8
        case Some(CountryBuildingType("USA", "OtherUtility")) => 78.8
        case Some(CountryBuildingType("USA", "SelfStorageFacility")) => 19.8
        case Some(CountryBuildingType("USA","SingleFamilyDetached")) => {
          region match {
            case "West" => 38.4
            case "Midwest" => 49.5
            case "South" => 41.5
            case "Northeast" => 45.7
          }
        }
        case Some(CountryBuildingType("USA","SingleFamilyAttached")) => {
          region match {
            case "West" => 38.8
            case "Midwest" => 44.8
            case "South" => 38.8
            case "Northeast" => 50.3
          }
        }
        case Some(CountryBuildingType("USA","MultiFamilyLessThan5")) => {
          region match {
            case "West" => 47.6
            case "Midwest" => 74.0
            case "South" => 46.9
            case "Northeast" => 57.8
          }
        }
        case Some(CountryBuildingType("USA","MultiFamilyMoreThan4")) => {
          region match {
            case "West" => 40.0
            case "Midwest" => 50.9
            case "South" => 47.9
            case "Northeast" => 60.7
          }
        }
        case Some(CountryBuildingType("USA","MobileHome")) => {
          region match {
            case "West" => 65.8
            case "Midwest" => 103.3
            case "South" => 40.0
            case "Northeast" => 89.3
          }
        }
        case Some(CountryBuildingType("USA", _)) => 78.8


          //Canadian Building Medians
        case Some(CountryBuildingType("Canada", "AdultEducation")) => 1.18
        case Some(CountryBuildingType("Canada", "College")) => 0.76
        case Some(CountryBuildingType("Canada", "PreSchool")) => 0.92
        case Some(CountryBuildingType("Canada", "VocationalSchool")) => 1.18
        case Some(CountryBuildingType("Canada", "OtherEducation")) => 0.92
        case Some(CountryBuildingType("Canada", "ConventionCenter")) => 1.74
        case Some(CountryBuildingType("Canada", "MovieTheater")) => 0.93
        case Some(CountryBuildingType("Canada", "Museum")) => 1.74
        case Some(CountryBuildingType("Canada", "PerformingArts")) => 1.74
        case Some(CountryBuildingType("Canada", "BowlingAlley")) => 1.51
        case Some(CountryBuildingType("Canada", "FitnessCenter")) => 1.51
        case Some(CountryBuildingType("Canada", "IceRink")) => 1.51
        case Some(CountryBuildingType("Canada", "RollerRink")) => 1.51
        case Some(CountryBuildingType("Canada", "SwimmingPool")) => 1.51
        case Some(CountryBuildingType("Canada", "OtherRecreation")) => 1.11
        case Some(CountryBuildingType("Canada", "MeetingHall")) => 1.74
        case Some(CountryBuildingType("Canada", "IndoorArena")) => 1.51
        case Some(CountryBuildingType("Canada", "RaceTrack")) => 1.11
        case Some(CountryBuildingType("Canada", "Stadium")) => 1.51
        case Some(CountryBuildingType("Canada", "Aquarium")) => 1.74
        case Some(CountryBuildingType("Canada", "Bar")) => 0.93
        case Some(CountryBuildingType("Canada", "NightClub")) => 0.93
        case Some(CountryBuildingType("Canada", "Casino")) => 0.93
        case Some(CountryBuildingType("Canada", "Zoo")) => 1.74
        case Some(CountryBuildingType("Canada", "OtherEntertainment")) => 1.74
        case Some(CountryBuildingType("Canada", "ConvenienceStore")) => 3.14
        case Some(CountryBuildingType("Canada", "GasStation")) => 3.14
        case Some(CountryBuildingType("Canada", "FastFoodRestaurant")) => 3.05
        case Some(CountryBuildingType("Canada", "Restaurant")) => 3.05
        case Some(CountryBuildingType("Canada", "OtherDining")) => 3.05
        case Some(CountryBuildingType("Canada", "FoodSales")) => 3.14
        case Some(CountryBuildingType("Canada", "FoodService")) => 3.05
        case Some(CountryBuildingType("Canada", "AmbulatorySurgicalCenter")) => 1.02
        case Some(CountryBuildingType("Canada", "DrinkingWaterTreatment")) => 0.63
        case Some(CountryBuildingType("Canada", "SpecialtyHospital")) => 2.35
        case Some(CountryBuildingType("Canada", "OutpatientCenter")) => 1.02
        case Some(CountryBuildingType("Canada", "PhysicalTherapyCenter")) => 1.02
        case Some(CountryBuildingType("Canada", "UrgentCareCenter")) => 1.02
        case Some(CountryBuildingType("Canada", "Barracks")) => 1.45
        case Some(CountryBuildingType("Canada", "Prison")) => 1.28
        case Some(CountryBuildingType("Canada", "ResidentialLodging")) => 1.12
        case Some(CountryBuildingType("Canada", "MixedUse")) => 0.90
        case Some(CountryBuildingType("Canada", "VeterinaryOffice")) => 1.02
        case Some(CountryBuildingType("Canada", "Courthouse")) => 1.28
        case Some(CountryBuildingType("Canada", "FireStation")) => 1.23
        case Some(CountryBuildingType("Canada", "Library")) => 1.74
        case Some(CountryBuildingType("Canada", "MailingCenter")) => 1.37
        case Some(CountryBuildingType("Canada", "PostOffice")) => 1.37
        case Some(CountryBuildingType("Canada", "PoliceStation")) => 1.28
        case Some(CountryBuildingType("Canada", "TransportationTerminal")) => 1.06
        case Some(CountryBuildingType("Canada", "OtherPublicServices")) => 0.90
        case Some(CountryBuildingType("Canada", "AutoDealership")) => 0.85
        case Some(CountryBuildingType("Canada", "EnclosedMall")) => 2.10
        case Some(CountryBuildingType("Canada", "StripMall")) => 1.38
        case Some(CountryBuildingType("Canada", "Laboratory")) => 0.90
        case Some(CountryBuildingType("Canada", "PersonalServices")) => 1.00
        case Some(CountryBuildingType("Canada", "RepairServices")) => 1.00
        case Some(CountryBuildingType("Canada", "OtherServices")) => 1.37
        case Some(CountryBuildingType("Canada", "PowerStation")) => 0.90
        case Some(CountryBuildingType("Canada", "OtherUtility")) => 0.90
        case Some(CountryBuildingType("Canada", "SelfStorageFacility")) => 0.75
        // Canadian Building Medians for Buildings with US Algorithms
        case Some(CountryBuildingType("Canada","Hotel")) => 1.12
        case Some(CountryBuildingType("Canada","WorshipCenter")) => 0.86
        case Some(CountryBuildingType("Canada","Warehouse")) => 0.75
        case Some(CountryBuildingType("Canada","SeniorCare")) => 1.12
        case Some(CountryBuildingType("Canada","Retail")) => 0.85
        case Some(CountryBuildingType("Canada","ResidenceHall")) => 1.45
        case Some(CountryBuildingType("Canada","DataCenter")) => 1.82

        case Some(CountryBuildingType("Canada", _)) => 0.90

        case Some(_) => throw new Exception("Could not find Country and Building Type for Median EUI")
        case None => throw new Exception("Could not find Country and Building Type for Median EUI")
      }
    }

    countryBuilding match {
      case Some(CountryBuildingType("USA", _)) => KBtus(siteMedian)
      case Some(CountryBuildingType("Canada", _)) => Gigajoules(siteMedian)
      case Some(CountryBuildingType(_, _)) => throw new Exception("Could not find Country and Building Type for Median EUI")
      case None => throw new Exception("Could not find Country and Building Type for Median EUI")
    }
  }

  def residentialSitetoSourceRatio():Future[Double] = Future {

    val region: String = getRegion(parameters)

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
      case Some(CountryBuildingType("Canada", "Prison")) => 1.28 / 1.74
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
      case Some(CountryBuildingType("Canada","SeniorCare")) => 1.12 / 1.88
      case Some(CountryBuildingType("Canada","Retail")) => 0.85 / 1.52
      case Some(CountryBuildingType("Canada","ResidenceHall")) => 1.45 / 2.05
      case Some(CountryBuildingType("Canada","DataCenter")) => 1.82 / 1.82

      case Some(CountryBuildingType("Canada", _)) => 0.90 / 1.23

      case _ => throw new Exception("Could not find Default Site to Source Ratio")
    }
  }


  def getRegion(parameters:JsValue):String = {

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

  def getTargetES:Future[Int] = Future{
    parameters.asOpt[TargetES] match {
      case Some(a) => a.target
      case None => throw new Exception("Could not find your Target ES!")
    }
  }
}


case class BuildingArea(GFA:PosDouble)
object BuildingArea {
  implicit val buildingAreaReads: Reads[BuildingArea] = Json.reads[BuildingArea]
}

case class TargetES(target:Int)
object TargetES {
  implicit val TargetReads: Reads[TargetES] = (JsPath \ "targetScore").read[Int](Reads.min(0) andKeep
    Reads.max(100)).map(new TargetES(_))
}

case class PercentBetterThanMedian(target:Double)
object PercentBetterThanMedian {
  implicit val percentBetterReads: Reads[PercentBetterThanMedian] = (JsPath \ "percentBetterThanMedian").read[Double](
    Reads.min(0.0) andKeep Reads.max(100.0)).map(new PercentBetterThanMedian(_))
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
  val country:String
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

case class GenericBuilding (GFA:PosDouble,areaUnits:String, country:String) extends BaseLine {
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
                  areaUnits:String, country:String) extends BaseLine {




  val regressionSegments = Seq[RegressionSegment] (
    RegressionSegment(186.6, 0, 1), // regression constant
    RegressionSegment(34.17, 9.535, math.min(log(buildingSize),200000)),
    RegressionSegment(17.28, 2.231, math.min(numComputers.value / buildingSize * 1000, 11.1)),
    RegressionSegment(55.96, 3.972, log(weeklyOperatingHours.value)),
    RegressionSegment(10.34, 0.5616, log(numWorkersMainShift.value / buildingSize * 1000)),
    RegressionSegment(0.0077, 4411, HDD.value * percentHeated.value / 100),
    RegressionSegment(0.0144, 1157, CDD.value * percentCooled.value / 100),
    RegressionSegment(-64.83 * isSmallBank, 9.535, log(buildingSize)),
    RegressionSegment(34.2 * isSmallBank, .5616, log(numWorkersMainShift.value / buildingSize * 1000)),
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
                        areaUnits:String, country:String) extends BaseLine {




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
                         isOpenAllWeekdays:Option[Boolean], areaUnits:String, country:String) extends BaseLine {




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
                            wastewaterLoadFactor:PosDouble, HDD:PosDouble, CDD:PosDouble, GFA:PosDouble, areaUnits:String, country:String) extends BaseLine {



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
                     percentCooled:PosDouble, GFA:PosDouble, areaUnits:String, country:String) extends BaseLine {





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
                       percentCooled:PosDouble, GFA:PosDouble, areaUnits:String, country:String) extends BaseLine {





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
                             GFA:PosDouble, areaUnits:String, country:String) extends BaseLine {




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
                      HDD:PosDouble, CDD:PosDouble, GFA:PosDouble, areaUnits:String, country:String) extends BaseLine {




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
                  HDD:PosDouble, CDD:PosDouble, GFA:PosDouble, areaUnits:String, country:String) extends BaseLine {




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
                         HDD:PosDouble, CDD:PosDouble, GFA:PosDouble, areaUnits:String, country:String) extends BaseLine {




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
                       HDD:PosDouble, CDD:PosDouble, GFA:PosDouble, areaUnits:String, country:String) extends BaseLine {




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
                               HDD:PosDouble, CDD:PosDouble, GFA:PosDouble, areaUnits:String, country:String) extends BaseLine {



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
                         areaUnits:String, country:String) extends BaseLine {




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
                           GFA:PosDouble, areaUnits:String, country:String) extends BaseLine {


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
                     CDD:PosDouble, GFA:PosDouble, areaUnits:String, country:String) extends BaseLine {




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
                 HDD:PosDouble, CDD:PosDouble, GFA:PosDouble, areaUnits:String, country:String) extends BaseLine {




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
                    CDD:PosDouble, GFA:PosDouble, areaUnits:String, country:String) extends BaseLine {




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
                          HDD:PosDouble, CDD:PosDouble, GFA:PosDouble, areaUnits:String, country:String) extends BaseLine {

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
                      GFA:PosDouble, areaUnits:String, country:String) extends BaseLine {

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

