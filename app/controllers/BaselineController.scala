
package controllers
import com.github.tototoshi.csv.CSVReader
import models._
import com.google.inject.Inject
import play.api.cache.CacheApi
import play.api.mvc._
import com.maalka.WeatherNormalization

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.language.implicitConversions


trait BaselineActions {
  this: Controller =>

  def normalize() = Action.async(parse.json) { implicit request =>

    val filename = "/tmp/upload/" + "aaa.csv"
    val reader = CSVReader.open(filename)
    val csvOutput: CSVcompute = CSVcompute(reader.all())

    val filtered = NormalizedWeather(request.body, csvOutput).filteredTempEnergy

    val (temperature, energy) = filtered.unzip

    val output = WeatherNormalization.calculateOutput(temperature.toArray, energy.toArray)

    output._1.map{ println(_)}
    output._2.map{ println(_)}

    val output_just_segmented = WeatherNormalization.segmentedRegression(temperature.toArray, energy.toArray)

    output_just_segmented.map {o =>  println("breakpoint1: " + o.psi(0).est) }
    output_just_segmented.map {o =>  println("residuals: " + o.residuals) }
    output_just_segmented.map {o =>  println("coefficients: " + o.coefficients) }

    Future{Ok("File has been uploaded")}


  }
}
class BaselineController @Inject() (val cache: CacheApi) extends Controller with Security with Logging with BaselineActions