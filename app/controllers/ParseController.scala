
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


trait ParseActions {
  this: Controller =>



  def normalize() = Action.async(parse.json) { implicit request =>

    println(request.body)

    val filename = "/tmp/upload/" + "weather_norm_sample_hyphen.csv"
    val reader = CSVReader.open(filename)
    val csvOutput: CSVcompute = CSVcompute(reader.all())

    Future{Ok("File has been uploaded")}


  }
}
class ParseController @Inject() (val cache: CacheApi) extends Controller with Security with Logging with ParseActions