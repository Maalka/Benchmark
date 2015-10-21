package models

/**
 * Created by rimukas on 10/20/15.
 */
/** *
  * Base line trait, enables the reducing of equation segments and manages the lookup of energy star score values
  */

import play.api.data.validation.ValidationError
import play.api.libs.json._
import play.api.libs.json.Reads._
import scala.language.implicitConversions
import play.api.libs.functional.syntax._
import scala.math._



/**
* Maps the json to the correct BaseLine type.
*/
object EUI {
  def getEUI(parameters: JsValue):JsResult[Double] = {

    val r = parameters.validate[energyMetrics]

    r.map {case (a: BaseLine) => a.expectedEnergy}

  }
}


case class energyMetrics(country:String,energyType:String,energyUnits:String,energyUse:Double,energyRate:Double){

  def convertUnits(country:String, energyType:String,energyUnits:String,energyUse:Double):Double = {
      val conversion:Double = country match {
        case "USA" => 3.0
        case _=> 3.0
      }
  conversion
  }
}

object energyMetrics {
  implicit val readEnergyMetrics: Reads[energyMetrics] = Json.reads[energyMetrics]

}