package models

import java.io.InputStream

import play.{Environment, api}
import play.api.{Environment, Play}
import play.api.libs.json._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import play.api.libs.functional.syntax._
import play.api.libs.functional.syntax._
import play.api.libs.json.Reads._
import play.api.libs.json._
import squants.Energy
import squants.energy.{Energy, KBtus}
import squants.space.{Area, SquareFeet, SquareMeters}



case class PrescriptiveValues(parameters:JsValue) {

  val finalConversion:MetricConversion = MetricConversion(parameters)


  def lookupPrescriptiveTotalMetricIntensity(metric:Option[String]): Future[Energy] = {
    for {
      validatedPropList <- getValidatedPropList
      building_size <- Future(validatedPropList.map(_.floor_area).sum)
      weightedEndUseDistList <- lookupPrescriptiveEndUses(metric)
      totalEUI <- getPrescriptiveTotalEUI(weightedEndUseDistList)
    } yield KBtus(totalEUI)
  }

  def lookupPrescriptiveTotalMetric(metric:Option[String]): Future[Energy] = {
    for {
      validatedPropList <- getValidatedPropList
      building_size <- Future(validatedPropList.map(_.floor_area).sum)
      totalEUI <- lookupPrescriptiveTotalMetricIntensity(metric)
      endUsePercents <- Future(totalEUI*building_size)
    } yield endUsePercents
  }

  def lookupPrescriptiveEndUsePercents(metric:Option[String]): Future[EndUseDistribution] = {
    for {
      weightedEndUseDistList <- lookupPrescriptiveEndUses(metric)
      endUsePercents <- getEndUseDistPercents(weightedEndUseDistList)
    } yield endUsePercents
  }

  def lookupPrescriptiveEndUses(metric:Option[String]): Future[EndUseDistribution] = {
    for {
      electric <- lookupPrescriptiveElectricityWeighted(metric)
      ng <- lookupPrescriptiveNGWeighted(metric)
      weightedEndUseDistList <- getWeightedEndUSeDistList(electric,ng)
    } yield weightedEndUseDistList
  }

  def lookupPrescriptiveElectricityWeighted(metric:Option[String]): Future[ElectricityDistribution] = {
    for {
      lookupParams <- getPrescriptiveParams
      lookupTableName <- chooseLookupTable(lookupParams)
      validatedPropList <- getValidatedPropList
      areaWeights <- getAreaWeights(validatedPropList)
      elecDistList:List[ElectricityDistribution] <- Future.sequence(validatedPropList.map(lookupPrescriptiveElectricity(_)))
      weightedElecDistList <- getWeightedElecDistList(areaWeights,elecDistList)
      conversionMetrics <- finalConversion.getConversionMetrics(metric)
      electric_converted <- finalConversion.convertMetrics(weightedElecDistList,None,conversionMetrics,metric)
    } yield electric_converted
  }


  def lookupPrescriptiveNGWeighted(metric:Option[String]): Future[NaturalGasDistribution] = {
    for {
      lookupParams <- getPrescriptiveParams
      lookupTableName <- chooseLookupTable(lookupParams)
      validatedPropList <- getValidatedPropList
      areaWeights <- getAreaWeights(validatedPropList)
      ngDistList: List[NaturalGasDistribution] <- Future.sequence(validatedPropList.map(lookupPrescriptiveNG(_)))
      weightedNGDistList <- getWeightedNGDistList(areaWeights,ngDistList)
      conversionMetrics <- finalConversion.getConversionMetrics(metric)
      ng_converted <- finalConversion.convertMetrics(weightedNGDistList,None,conversionMetrics,metric)
    } yield ng_converted
  }


  def lookupPrescriptiveElectricity(propDesc:ValidatedPropTypes): Future[ElectricityDistribution] = {
    for {
      lookupParams <- getPrescriptiveParams
      lookupTableName <- chooseLookupTable(lookupParams)
      prescriptiveEUITable <- loadLookupTable(lookupTableName)
      euiDist <-
        Future {
          (prescriptiveEUITable \ propDesc.building_type \ lookupParams.climate_zone).toOption match {
            case Some(a) => a.head.validate[ElectricityDistribution] match {
              case JsSuccess(b: ElectricityDistribution, _) => b
              case JsError(err) => throw new Exception(JsError.toJson(err).value.toString())
            }
            case _ => throw new Exception("Could not retrieve Prescriptive EUI (Electric) data!")
          }
        }
    } yield euiDist
  }
  def lookupPrescriptiveNG(propDesc:ValidatedPropTypes): Future[NaturalGasDistribution] = {
    for {
      lookupParams <- getPrescriptiveParams
      lookupTableName <- chooseLookupTable(lookupParams)
      prescriptiveEUITable <- loadLookupTable(lookupTableName)
      euiDist <-
        Future {
          (prescriptiveEUITable \ propDesc.building_type \ lookupParams.climate_zone).toOption match {
            case Some(a) => a.head.validate[NaturalGasDistribution] match {
              case JsSuccess(b: NaturalGasDistribution, _) => b
              case JsError(err) => throw new Exception(JsError.toJson(err).value.toString())
            }
            case _ => throw new Exception("Could not retrieve Prescriptive EUI (NG) data!")
          }
        }
    } yield euiDist
  }



  def getPrescriptiveTotalEUI(EndUses:EndUseDistribution):Future[Double] = Future {
// End Uses are in KBtu and building size is in Square Feet

          EndUses.htg +
          EndUses.clg +
          EndUses.intLgt +
          EndUses.extLgt +
          EndUses.intEqp +
          EndUses.extEqp +
          EndUses.fans +
          EndUses.pumps +
          EndUses.heatRej +
          EndUses.humid +
          EndUses.heatRec +
          EndUses.swh +
          EndUses.refrg +
          EndUses.gentor

    }

  def getEndUseDistPercents(EndUses:EndUseDistribution):Future[EndUseDistribution] = Future {

        val sum = {
          EndUses.htg +
          EndUses.clg +
          EndUses.intLgt +
          EndUses.extLgt +
          EndUses.intEqp +
          EndUses.extEqp +
          EndUses.fans +
          EndUses.pumps +
          EndUses.heatRej +
          EndUses.humid +
          EndUses.heatRec +
          EndUses.swh +
          EndUses.refrg +
          EndUses.gentor
        }

       EndUseDistribution(
          EndUses.htg/sum,
          EndUses.clg/sum,
          EndUses.intLgt/sum,
          EndUses.extLgt/sum,
          EndUses.intEqp/sum,
          EndUses.extEqp/sum,
          EndUses.fans/sum,
          EndUses.pumps/sum,
          EndUses.heatRej/sum,
          EndUses.humid/sum,
          EndUses.heatRec/sum,
          EndUses.swh/sum,
          EndUses.refrg/sum,
          EndUses.gentor/sum,
          EndUses.net/sum
        )
    }

  def getWeightedEndUSeDistList(elec:ElectricityDistribution, ng:NaturalGasDistribution):Future[EndUseDistribution] = Future {

        EndUseDistribution(
          elec.elec_htg + ng.ng_htg,
          elec.elec_clg + ng.ng_clg,
          elec.elec_intLgt + ng.ng_intLgt,
          elec.elec_extLgt + ng.ng_extLgt,
          elec.elec_intEqp + ng.ng_intEqp,
          elec.elec_extEqp + ng.ng_extEqp,
          elec.elec_fans + ng.ng_fans,
          elec.elec_pumps + ng.ng_pumps,
          elec.elec_heatRej + ng.ng_heatRej,
          elec.elec_humid + ng.ng_humid,
          elec.elec_heatRec + ng.ng_heatRec,
          elec.elec_swh + ng.ng_swh,
          elec.elec_refrg + ng.ng_refrg,
          elec.elec_gentor + ng.ng_gentor,
          elec.elec_net + ng.ng_net
        )
    }


  def getWeightedElecDistList(areaWeights:List[Double], distList:List[ElectricityDistribution]):Future[ElectricityDistribution] = Future {

    val weightedList = (distList, areaWeights).zipped.map {
      case (b: ElectricityDistribution, c: Double) => {
        ElectricityDistribution(
          b.elec_htg * c,
          b.elec_clg * c,
          b.elec_intLgt * c,
          b.elec_extLgt * c,
          b.elec_intEqp * c,
          b.elec_extEqp * c,
          b.elec_fans * c,
          b.elec_pumps * c,
          b.elec_heatRej * c,
          b.elec_humid * c,
          b.elec_heatRec * c,
          b.elec_swh * c,
          b.elec_refrg * c,
          b.elec_gentor * c,
          b.elec_net * c
        )
      }
    }

    val elec_htg = weightedList.map(_.elec_htg).sum
    val elec_clg = weightedList.map(_.elec_clg).sum
    val elec_intLgt = weightedList.map(_.elec_intLgt).sum
    val elec_extLgt = weightedList.map(_.elec_extLgt).sum
    val elec_intEqp = weightedList.map(_.elec_intEqp).sum
    val elec_extEqp = weightedList.map(_.elec_extEqp).sum
    val elec_fans = weightedList.map(_.elec_fans).sum
    val elec_pumps = weightedList.map(_.elec_pumps).sum
    val elec_heatRej = weightedList.map(_.elec_heatRej).sum
    val elec_humid = weightedList.map(_.elec_humid).sum
    val elec_heatRec = weightedList.map(_.elec_heatRec).sum
    val elec_swh = weightedList.map(_.elec_swh).sum
    val elec_refrg = weightedList.map(_.elec_refrg).sum
    val elec_gentor = weightedList.map(_.elec_gentor).sum
    val elec_net = weightedList.map(_.elec_net).sum

    ElectricityDistribution(elec_htg, elec_clg,elec_intLgt, elec_extLgt, elec_intEqp, elec_extEqp, elec_fans, elec_pumps, elec_heatRej, elec_humid, elec_heatRec, elec_swh, elec_refrg, elec_gentor, elec_net)

    }


  def getWeightedNGDistList(areaWeights:List[Double], distList:List[NaturalGasDistribution]):Future[NaturalGasDistribution] = Future {

    val weightedList = (distList, areaWeights).zipped.map {
      case (b: NaturalGasDistribution, c: Double) => {
        NaturalGasDistribution(
          b.ng_htg * c,
          b.ng_clg * c,
          b.ng_intLgt * c,
          b.ng_extLgt * c,
          b.ng_intEqp * c,
          b.ng_extEqp * c,
          b.ng_fans * c,
          b.ng_pumps * c,
          b.ng_heatRej * c,
          b.ng_humid * c,
          b.ng_heatRec * c,
          b.ng_swh * c,
          b.ng_refrg * c,
          b.ng_gentor * c,
          b.ng_net * c
        )
      }
    }

    val ng_htg = weightedList.map(_.ng_htg).sum
    val ng_clg = weightedList.map(_.ng_clg).sum
    val ng_intLgt = weightedList.map(_.ng_intLgt).sum
    val ng_extLgt = weightedList.map(_.ng_extLgt).sum
    val ng_intEqp = weightedList.map(_.ng_intEqp).sum
    val ng_extEqp = weightedList.map(_.ng_extEqp).sum
    val ng_fans = weightedList.map(_.ng_fans).sum
    val ng_pumps = weightedList.map(_.ng_pumps).sum
    val ng_heatRej = weightedList.map(_.ng_heatRej).sum
    val ng_humid = weightedList.map(_.ng_humid).sum
    val ng_heatRec = weightedList.map(_.ng_heatRec).sum
    val ng_swh = weightedList.map(_.ng_swh).sum
    val ng_refrg = weightedList.map(_.ng_refrg).sum
    val ng_gentor = weightedList.map(_.ng_gentor).sum
    val ng_net = weightedList.map(_.ng_net).sum

    NaturalGasDistribution(ng_htg, ng_clg,ng_intLgt, ng_extLgt, ng_intEqp,ng_extEqp, ng_fans, ng_pumps, ng_heatRej, ng_humid, ng_heatRec, ng_swh, ng_refrg, ng_gentor, ng_net)

  }


  def getAreaWeights(validatedPropList: List[ValidatedPropTypes]):Future[List[Double]] = {
    for {
      propSizeList <- Future(validatedPropList.map(_.floor_area))
      propsizeSum:Double <- Future(propSizeList.sum)
      propSizeRatios <- Future(propSizeList.map{_/propsizeSum})
    } yield propSizeRatios
  }

  def chooseLookupTable(validatedPrescriptiveParams: ValidatedPrescriptiveParams): Future[String] =  Future{
    validatedPrescriptiveParams.prescriptive_resource match {
          case 0 => "prescriptive_0.json"
          case _ => throw new Exception("Cannot Identify Appropriate Lookup Table: Check prescriptive_resource value!")
        }
  }

  def loadLookupTable(filename:String): Future[JsValue] = {
    for {
      is <- Future(play.api.Environment.simple().resourceAsStream(filename))
      json <- Future {
        is match {
          case Some(is: InputStream) => {
            Json.parse(is)
          }
          case i => throw new Exception("Prescriptive EUI Lookup - Could not open file: %s".format(i))
        }
      }
    } yield json
  }

  def validatePrescriptiveParams(buildingParams: PrescriptiveParams): Future[ValidatedPrescriptiveParams] = Future {
    val climateZone = buildingParams.climate_zone match {
      case Some(b: String) => b
      case _ => throw new Exception("Could not Identify Climate Zone")
    }
    val prescriptive_resource = buildingParams.prescriptive_resource match {
      case Some(a: Int) => a
      case _ => 0
    }
    ValidatedPrescriptiveParams(climateZone, prescriptive_resource)
  }

  def getPrescriptiveParams: Future[ValidatedPrescriptiveParams] = {
    parameters.validate[PrescriptiveParams] match {
      case JsSuccess(a: PrescriptiveParams, _) => validatePrescriptiveParams(a)
      case _ => throw new Exception("Prescriptive Resource or Climate Zone Incorrect!")
    }
  }



  def getValidatedPropParams(propDesc: PropDesc): Future[ValidatedPropTypes] = Future {
    val propType:String = propDesc.building_type match {
      case Some(b: String) => b
      case _ => throw new Exception("Not a Proper Building Type")
    }
    val units:String = propDesc.floor_area_units match {
      case Some("mSQ") => "mSQ"
      case Some("ftSQ") => "ftSQ"
      case _ => throw new Exception("Floor Area Units must be either ftSQ or mSQ")
    }

    val floorArea:Double = propDesc.floor_area match {
      case Some(a: Double) => {
        units match {
          case "mSQ" => (Area((a, "mSQ")).get to SquareFeet)
          case "ftSQ" => (Area((a, "ftSQ")).get to SquareFeet)
          case _ => throw new Exception("Floor Area Units Must be mSQ or ftSQ! ")
        }
      }
      case _ => throw new Exception("No Floor Area Found! ")
    }
    ValidatedPropTypes(propType,floorArea,"ftSQ")
  }


  def getValidatedPropList: Future[List[ValidatedPropTypes]] =  {

    for {
      props:PropList <-
        Future {
          parameters.validate[PropList] match {
              case JsSuccess(b: PropList, _) => b
              case JsError(err) => throw new Exception("Problem with submitted Prop List!")
          }
        }
      validatedProps <- {
        props.prop_types.isEmpty match {
          case true => throw new Exception("Prop List is Empty!")
          case _ => Future.sequence(props.prop_types.map(getValidatedPropParams(_)))
        }
      }
    } yield validatedProps
  }
}




case class EndUseDistribution(htg:Double,clg:Double,intLgt:Double = 0.0,extLgt:Double = 0.0,intEqp:Double = 0.0,
                                   extEqp:Double = 0.0, fans:Double = 0.0,pumps:Double = 0.0,heatRej:Double = 0.0,
                                   humid:Double = 0.0, heatRec:Double = 0.0,swh:Double = 0.0,refrg:Double = 0.0,
                                   gentor:Double = 0.0,net:Double = 0.0)


case class ElectricityDistribution(elec_htg:Double,elec_clg:Double,elec_intLgt:Double = 0.0,elec_extLgt:Double = 0.0,elec_intEqp:Double = 0.0,
                                   elec_extEqp:Double = 0.0, elec_fans:Double = 0.0,elec_pumps:Double = 0.0,elec_heatRej:Double = 0.0,
                                   elec_humid:Double = 0.0, elec_heatRec:Double = 0.0,elec_swh:Double = 0.0,elec_refrg:Double = 0.0,
                                   elec_gentor:Double = 0.0,elec_net:Double = 0.0)

object ElectricityDistribution {
  implicit val ElectricityDistributionReads: Reads[ElectricityDistribution] = Json.reads[ElectricityDistribution]
}
case class NaturalGasDistribution(ng_htg:Double = 0.0,ng_clg:Double = 0.0,ng_intLgt:Double = 0.0,ng_extLgt:Double = 0.0,
                                  ng_intEqp:Double = 0.0,ng_extEqp:Double = 0.0,ng_fans:Double = 0.0,ng_pumps:Double = 0.0,
                                  ng_heatRej:Double = 0.0,ng_humid:Double = 0.0,ng_heatRec:Double = 0.0, ng_swh:Double = 0.0,
                                  ng_refrg:Double = 0.0,ng_gentor:Double = 0.0,ng_net:Double = 0.0)

object NaturalGasDistribution {
  implicit val NaturalGasDistributionReads: Reads[NaturalGasDistribution] = Json.reads[NaturalGasDistribution]
}




case class PrescriptiveParams(climate_zone: Option[String], prescriptive_resource: Option[Int])

object PrescriptiveParams {
  implicit val PrescriptiveParamsReads: Reads[PrescriptiveParams] = Json.reads[PrescriptiveParams]
}

case class ValidatedPrescriptiveParams(climate_zone: String, prescriptive_resource:Int)

object ValidatedPrescriptiveParams {
  implicit val ValidatedPrescriptiveParamsReads: Reads[ValidatedPrescriptiveParams] = Json.reads[ValidatedPrescriptiveParams]
}





case class ValidatedPropList(prop_types: List[ValidatedPropTypes])

object ValidatedPropList {
  implicit val ValidatedPropListReads: Reads[ValidatedPropList] = Json.reads[ValidatedPropList]
}

case class ValidatedPropTypes(building_type: String,floor_area: Double, floor_area_units: String)

object ValidatedPropTypes {
  implicit val ValidatedPropTypesReads: Reads[ValidatedPropTypes] = Json.reads[ValidatedPropTypes]
}




case class PropList(prop_types: List[PropDesc])

object PropList {
  implicit val PropListReads: Reads[PropList] = Json.reads[PropList]
}

case class PropDesc(building_type: Option[String],floor_area: Option[Double], floor_area_units: Option[String])

object PropDesc {
  implicit val PropDescReads: Reads[PropDesc] = Json.reads[PropDesc]
}