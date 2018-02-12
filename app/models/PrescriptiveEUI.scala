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



case class PrescriptiveValues(parameters:JsValue) {


  def lookupPrescriptiveElectricity: Future[ElectricityDistribution] = {
    for {
      lookupParams <- getPrescriptiveParams
      lookupTableName <- chooseLookupTable(lookupParams)
      prescriptiveEUITable <- loadLookupTable(lookupTableName)
      euiDist <-
        Future {
          (prescriptiveEUITable \ lookupParams.building_type \ lookupParams.climate_zone).toOption match {
            case Some(a) => a.head.validate[ElectricityDistribution] match {
              case JsSuccess(b: ElectricityDistribution, _) => b
              case JsError(err) => throw new Exception(JsError.toJson(err).value.toString())
            }
            case _ => throw new Exception("Could not retrieve Prescriptive EUI (Electric) data!")
          }
        }
    } yield euiDist
  }
  def lookupPrescriptiveNG: Future[NaturalGasDistribution] = {
    for {
      lookupParams <- getPrescriptiveParams
      lookupTableName <- chooseLookupTable(lookupParams)
      prescriptiveEUITable <- loadLookupTable(lookupTableName)
      euiDist <-
        Future {
          (prescriptiveEUITable \ lookupParams.building_type \ lookupParams.climate_zone).toOption match {
            case Some(a) => a.head.validate[NaturalGasDistribution] match {
              case JsSuccess(b: NaturalGasDistribution, _) => b
              case JsError(err) => throw new Exception(JsError.toJson(err).value.toString())
            }
            case _ => throw new Exception("Could not retrieve Prescriptive EUI (NG) data!")
          }
        }
    } yield euiDist
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

  def validateBuildingParams(buildingParams: PrescriptiveParams): Future[ValidatedPrescriptiveParams] = Future {
    val buildingType = buildingParams.building_type match {
      case Some(b: String) => b
      case _ => throw new Exception("Not a Proper Building Type")
    }
    val climateZone = buildingParams.climate_zone match {
      case Some(b: String) => b
      case _ => throw new Exception("Could not Identify Climate Zone")
    }
    val prescriptive_resource = buildingParams.prescriptive_resource match {
      case Some(a: Int) => a
      case _ => 0
    }
    ValidatedPrescriptiveParams(climateZone, buildingType, prescriptive_resource)
  }

  def getPrescriptiveParams: Future[ValidatedPrescriptiveParams] =  {
    parameters.validate[PrescriptiveParams] match {
      case JsSuccess(a: PrescriptiveParams, _) => {
        a.building_type match {
          case Some(b:String) => validateBuildingParams(a)
          case _ => throw new Exception("Not a Proper Building Type")
        }
      }
      case JsError(err) => throw new Exception(JsError.toJson(err).value.toString())
    }
  }
}


case class ElectricityDistribution(elec_htg:Double,elec_clg:Double,elec_intLgt:Double = 0.0,elec_extLgt:Double = 0.0,elec_intEqp:Double = 0.0,elec_extEqp:Double = 0.0,
elec_fans:Double = 0.0,elec_pumps:Double = 0.0,elec_heatRej:Double = 0.0,elec_humid:Double = 0.0,elec_heatRec:Double = 0.0,elec_swh:Double = 0.0,elec_refrg:Double = 0.0,elec_gentor:Double = 0.0,elec_net:Double = 0.0, site_EUI:Double = 0.0)

object ElectricityDistribution {
  implicit val ElectricityDistributionReads: Reads[ElectricityDistribution] = Json.reads[ElectricityDistribution]
}
case class NaturalGasDistribution(ng_htg:Double = 0.0,ng_clg:Double = 0.0,ng_intLgt:Double = 0.0,ng_extLgt:Double = 0.0,ng_intEqp:Double = 0.0,ng_extEqp:Double = 0.0,ng_fans:Double = 0.0,ng_pumps:Double = 0.0,ng_heatRej:Double = 0.0,ng_humid:Double = 0.0,ng_heatRec:Double = 0.0,
ng_swh:Double = 0.0,ng_refrg:Double = 0.0,ng_gentor:Double = 0.0,ng_net:Double = 0.0,site_EUI:Double = 0.0)

object NaturalGasDistribution {
  implicit val NaturalGasDistributionReads: Reads[NaturalGasDistribution] = Json.reads[NaturalGasDistribution]
}


case class PrescriptiveParams(climate_zone: Option[String],building_type: Option[String], prescriptive_resource: Option[Int])

object PrescriptiveParams {
  implicit val PrescriptiveParamsReads: Reads[PrescriptiveParams] = Json.reads[PrescriptiveParams]
}

case class ValidatedPrescriptiveParams(climate_zone: String,building_type: String, prescriptive_resource:Int)

object ValidatedPrescriptiveParams {
  implicit val ValidatedPrescriptiveParamsReads: Reads[ValidatedPrescriptiveParams] = Json.reads[ValidatedPrescriptiveParams]
}
