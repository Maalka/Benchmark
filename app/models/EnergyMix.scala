package models

import java.io.InputStream

import play.api.Play
import play.api.data.validation.ValidationError
import play.api.libs.json.{Json, JsValue}

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

/**
 * Created by rimukas on 12/10/15.
 */
object EnergyMix {

  val energyMixTable = loadEnergyMixTable

  def loadEnergyMixTable: Future[JsValue] = {
    for {
      is <- Future(Play.current.resourceAsStream("statePropertyEnergyMix.json"))
      json <- Future {
        is match {
          case Some(is: InputStream) => {
            Json.parse(is)
          }
          case _ => throw new Exception("Could not open file")
        }
      }
    } yield json
  }


  def getMix(state:String,propType:String):Future[Double] = {
    val mixTable = energyMixTable.map { case a => (a \ state \ propType).toOption}
    mixTable.map{
      case Some(a) => a.as[Double]
      case _ => throw new Exception("Could not find State and PropType in statePropertyEnergyMix.json")
    }
  }

  def getDefaultRatio(gridMix:Double):Future[Double] = Future{
    1 / (gridMix*siteToSourceConversions.gridUS + (1.0-gridMix)*siteToSourceConversions.ngUS)
  }

}