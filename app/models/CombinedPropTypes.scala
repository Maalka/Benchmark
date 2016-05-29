package models


import squants.energy._
import squants.energy.EnergyConversions.EnergyNumeric
import scala.concurrent.Future
import scala.language._
import scala.math._
import play.api.libs.json._
import play.api.Play
import scala.concurrent.ExecutionContext.Implicits.global
import java.io.{InputStream}


case class CombinedPropTypes(params: JsValue) {

  val result = params.as[List[JsValue]]
  val buildingProps:BuildingProperties = BuildingProperties(result.head)

  def getWholeBuildingSourceMedianEnergy:Future[Energy] = {
    for {
      wholeBuildingSourceMedianEUI <- getWholeBuildingSourceMedianEUI
      totalArea <- getTotalArea
    } yield wholeBuildingSourceMedianEUI*totalArea
  }

  def getWholeBuildingSourceMedianEUI:Future[Energy] = {
    for {
      majorPropType <- majorProp
      hasGenericBuilding <- checkGenericBuilding
      lookUpSourceMedianEUI <- {
        majorPropType.contains(true) match {
          case true => {
            hasGenericBuilding match {
              case true => getGenericSourceEUI
              case false => getMedianSourceEUI
            }
          }
          case false => {
            getDefaultSourceMedianEUI(result.head)
            }
          }
        }
      } yield lookUpSourceMedianEUI
    }

  def getMedianSourceEUI:Future[Energy] = {
    for {
      totalArea <- getTotalArea
      baselineSourceEnergy <- getMedianSourceEnergy
    }yield baselineSourceEnergy/totalArea
  }

  def getMedianSourceEnergy:Future[Energy] = {
    for {
      weightedTable <- getWeightedTable
      medianRatio <- getMedianRatio(weightedTable)
      totalEnergy <- getTotalPredictedEnergy
    }yield totalEnergy*medianRatio
  }

  def getGenericSourceEUI:Future[Energy] = {
    for {
      propFilter <- majorProp
      mediumPropFilter <- mediumSizeProps
      majorProp <- getMajorProp(propFilter)
      majorPropType <- BuildingProperties(majorProp).getBuilding
      sourceEUI <- {
        mediumPropFilter.contains(true) match {
          case a if a==true => {
            println(majorPropType)
            majorPropType match {
              case c: GenericBuilding => singlePropMedianSourceEUI(majorProp)
              case d: BaseLine => Future {
                buildingProps.country match {
                  case "USA" => KBtus(148.1)
                  case "Canada" => Gigajoules(1.68)
                }
              }
            }
          }
          case a if a==false => singlePropMedianSourceEUI(majorProp)
        }

      }
    } yield sourceEUI
  }

  def getMajorProp(propFilter:List[Boolean]):Future[JsValue] = Future{
    (propFilter,result).zipped.collect{case (a,b) if a == true => b }.head
  }

  def singlePropMedianSourceEUI(params:JsValue):Future[Energy] = {
    for {
      targetBuilding <- BuildingProperties(params).getBuilding
      medianEUI <- {
        targetBuilding match {
          case a:GenericBuilding => sourceMedianEUI(params)
          case b:BaseLine => {
            for {
              lookupEUI <- computeLookupEUI(targetBuilding)
              lookupTable <- lookupTableGet(params)
              targetRatio <- getMedianRatio(lookupTable)
              targetEUI <- getTargetEUI(targetBuilding, lookupEUI, targetRatio)
            } yield targetEUI
          }
        }
      }
    } yield medianEUI
  }

  def sumEnergy(energies:List[Energy]):Future[Energy] = Future{
    buildingProps.country match {
      case "USA" => energies.sum in KBtus
      case "Canada" => energies.sum in Gigajoules
    }
  }

  def getMedianRatio(lookUp:Seq[TableEntry]):Future[Double] = {
    for {
      targetRatioEntry <- Future(lookUp.filter(_.ES == 50).last.Ratio)
    } yield targetRatioEntry
  }

  def getTargetEUI[T](targetBuilding: T,lookupEUI:Energy,targetRatio:Double):Future[Energy] = Future {
    targetBuilding match {
      case a: ResidenceHall => {
        a.country match {
          case "USA" => KBtus(exp(targetRatio / 15.717 * lookupEUI.value) / a.buildingSize)
          case "Canada" => Gigajoules(exp(targetRatio / 15.717 * lookupEUI.value) / a.buildingSize)
        }
      }
      case a: MedicalOffice => {
        a.country match {
          case "USA" => KBtus(exp(targetRatio / 14.919 * lookupEUI.value) / a.buildingSize)
          case "Canada" => Gigajoules(exp(targetRatio / 14.919 * lookupEUI.value) / a.buildingSize)
        }
      }
      case a: DataCenter => targetRatio * lookupEUI * a.annualITEnergyKBtu
      case a:GenericBuilding => throw new Exception("Could not calculate Target EUI - Generic Building: No Algorithm!!")
      case a: BaseLine => targetRatio * lookupEUI
    }
  }

  def checkGenericBuilding:Future[Boolean] = {
    for {
      genericCheck: List[Boolean] <- Future.sequence(result.map(BuildingProperties(_).getBuilding).map(_.map {
        case a: GenericBuilding => true
        case _ => false
      }))
      testGeneric <- {
        genericCheck.contains(true) match {
          case true => Future(true)
          case false => Future(false)
        }
      }
    } yield testGeneric
  }

  def majorProp:Future[List[Boolean]] = {
    for {
      buildingSizeList <- Future.sequence(result.map(BuildingProperties(_).getBuilding).map(_.map{
        case a:BaseLine => a.buildingSize}
      ))
      buildingSizeSum:Double <- Future(buildingSizeList.sum)
      buildingSizeRatios <- Future(buildingSizeList.map{_/buildingSizeSum})
      majorPropType <-  Future(buildingSizeRatios.map{ case a => a > 0.5 })
    } yield majorPropType
  }


  def mediumSizeProps:Future[List[Boolean]] = {
    for {
      buildingSizeList <- Future.sequence(result.map(BuildingProperties(_).getBuilding).map(_.map{
        case a:BaseLine => a.buildingSize}
      ))
      buildingSizeSum:Double <- Future(buildingSizeList.sum)
      buildingSizeRatios <- Future(buildingSizeList.map{_/buildingSizeSum})
      mediumSizePropTypes <-  Future(buildingSizeRatios.map{ case a => 0.25 < a && a < 0.5 })
    } yield mediumSizePropTypes
  }

  def getEnergyWeights: Future[List[Double]] = {
    for {
      propEnergies <- Future.sequence(result.map(expectedSourceEnergy))
      propEnergiesSum <- getTotalPredictedEnergy
      energyWeights <- Future(propEnergies.map(_.value/propEnergiesSum.value))
    } yield energyWeights
  }

  def getTotalPredictedEnergy: Future[Energy] = {
    for {
      propEnergies <- Future.sequence(result.map(expectedSourceEnergy))
      propEnergiesSum <- sumEnergy(propEnergies)
    } yield propEnergiesSum
  }

  def getTotalArea: Future[Double] = {
    for {
      propTypes <- Future.sequence(result.map(BuildingProperties(_).getBuilding))
      propGFASum <- Future(propTypes.map(_.GFA.value).sum)
    } yield propGFASum
  }

  def getWeightedTable: Future[Seq[TableEntry]] = {
    val ESList:List[Int] = List.range(1,101,1).sorted(Ordering[Int].reverse)
    val cmPercentList:List[Double] = List.range(0,100,1).map(_/100.0)

    for {
      tableList <- Future.sequence(result.map(lookupTableGet).map(_.map(_.map(_.Ratio))))
      energyWeights <- getEnergyWeights
      weightedTable <- makeWeightedTable(tableList,energyWeights)
      zippedTable <- Future((ESList,cmPercentList,weightedTable).zipped.toList)
      formattedTable <- convertTabletoEntries(zippedTable)
      //jsonTable <- Future(convertEntriesToJson(formattedTable))
    } yield formattedTable
  }

  def makeWeightedTable(tableList:List[Seq[Double]],propWeights:List[Double]):Future[List[Double]] = Future {
    tableList.transpose.map(_.zip(propWeights).map{ case (a, b) => a * b }.sum)
  }

  def convertTabletoEntries(table: List[(Int,Double,Double)]): Future[Seq[TableEntry]] = Future{
    table.map { case (a,b,c) =>  TableEntry(a,b,c) }
  }

  def convertEntriesToJson(entries: Seq[TableEntry]): Seq[JsValue] = {
    entries.map(Json.toJson(_))
  }



  def expectedSourceEnergy(parameters:JsValue):Future[Energy] = {
    for {
      targetBuilding <- BuildingProperties(parameters).getBuilding
      expectedEnergy <- computeExpectedEnergy(targetBuilding,parameters.asOpt[CountryBuildingType])
    } yield expectedEnergy
  }

  def computeExpectedEnergy[T](targetBuilding: T,countryBuilding:Option[CountryBuildingType]): Future[Energy] = Future{
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




  def lookupTableGet(parameters: JsValue): Future[Seq[TableEntry]] = {
    for {
      lookUp <- getLookupTable(parameters)
      futureTable <- loadLookupTable(lookUp)
    } yield futureTable
  }

  def loadLookupTable(filename:String): Future[Seq[TableEntry]] = {
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
        json.validate[Seq[TableEntry]] match {
          case JsSuccess(a, _) => a
          case JsError(th) => throw new Exception("Cannot find this: " + th.toString())
        }
      }
    } yield obj
  }

  def getLookupTable(parameters:JsValue): Future[String] = {

    val countryBuilding = parameters.asOpt[CountryBuildingType]

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


  def computeLookupEUI[T](targetBuilding: T): Future[Energy] = Future{
    targetBuilding match {
      case a: GenericBuilding => throw new Exception("Lookup EUI could not be computed - Generic Building: No Algorithm!")
      case a: BaseLine => {
        a.country match {
          case "USA" => KBtus(a.expectedEnergy)
          case "Canada" => Gigajoules(a.expectedEnergy)
        }
      }
    }
  }

  def getDefaultSourceMedianEUI(parameters:JsValue):Future[Energy] = Future{
    parameters.asOpt[CountryBuildingType] match {
      case Some(CountryBuildingType("USA", _)) => KBtus(123.1)
      case Some(CountryBuildingType("Canada", _)) => Gigajoules(1.23)
      case Some(_) => throw new Exception("Could not find Country and Building Type for Median EUI")
      case None => throw new Exception("Could not find Country and Building Type for Median EUI")
    }
  }

  def sourceMedianEUI(parameters:JsValue):Future[Energy] = Future{

    val region:String = getRegion(parameters)
    val countryBuilding = parameters.asOpt[CountryBuildingType]

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
}

case class TableEntry(ES: Int, CmPercent: Double, Ratio: Double)
object TableEntry {
  implicit val tableEntryWrites: Writes[TableEntry] = Json.writes[TableEntry]
  implicit val tableEntryReads: Reads[TableEntry] = Json.reads[TableEntry]
}


