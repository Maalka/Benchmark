/**
 * Created by rimukas on 10/12/15.
 */


package controllers

import models._
import com.google.inject.Inject
import play.api.cache.AsyncCacheApi
import play.api.libs.json._
import play.api.mvc._

import scala.concurrent.Future
import scala.util.control.NonFatal
import scala.concurrent.ExecutionContext.Implicits.global
import scala.language.implicitConversions
import akka.actor.ActorSystem
import com.typesafe.scalalogging.LazyLogging
import services.AzureMapsService



class DegreeDaysController @Inject() (
                                       val cache: AsyncCacheApi,
                                       cc: ControllerComponents,
                                      azureMapsService: AzureMapsService
                                     )(implicit val actorSystem: ActorSystem) extends AbstractController(cc) with LazyLogging {
  this: AbstractController =>

  implicit def doubleToJSValue(d: Double): JsValue = Json.toJson(d)

  def roundAt(p: Int)(n: Double): Double = {
    val s = math pow(10, p); (math round n * s) / s
  }

  def apiRecover(throwable: Throwable): Either[String, JsValue] = {
    throwable match {
      case NonFatal(th) => Left(th.getMessage)
    }
  }

  def api[T](response: T): Either[String, JsValue] = {
    response match {
      case v: Double => Right(v)
      case v: Int => Right(Json.toJson(v))
      case v: String => Right(Json.toJson(v))
      case None => Left("Could not recognize input type")
    }
  }

  def getDDPostcodeMetrics(DD: DegreeDays): Future[(Either[String, JsValue], Either[String, JsValue], Either[String, JsValue])] = {

    val weatherStationFuture = DD.lookupWeatherStation.map(api).recover { case NonFatal(th) => apiRecover(th) }
    val hddFuture = DD.lookupHDD.map(api).recover { case NonFatal(th) => apiRecover(th) }
    val cddFuture = DD.lookupCDD.map(api).recover { case NonFatal(th) => apiRecover(th) }

    for {
      weatherStation <- weatherStationFuture
      hdd <- hddFuture
      cdd <- cddFuture
    } yield (weatherStation, hdd, cdd)
  }

  def getDDAzureMetrics(DD: DegreeDays): Future[Seq[Either[String, JsValue]]] = {
    val weatherStationFuture = DD.lookupWeatherStation
      .map(api)
      .recover { case NonFatal(th) => apiRecover(th) }

    val hddAndCddFuture = DD.lookupHddAndCddFromAzure
      .map {
        case Some((hdd, cdd)) => Seq(Right(Json.toJson(hdd)), Right(Json.toJson(cdd)))
        case None => Seq(Left("HDD and CDD data not found"), Left("HDD and CDD data not found"))
      }.recover {
        case NonFatal(th) => Seq(apiRecover(th), apiRecover(th))
      }

    for {
      weatherStation <- weatherStationFuture
      hddAndCdd <- hddAndCddFuture
    } yield weatherStation +: hddAndCdd
  }

  // Introduce constant for field names
  private val FieldNames: Seq[String] = Seq(
    "weatherStation",
    "HDD",
    "CDD"
  )

  def getDDMetrics() = Action.async(parse.json) { implicit request =>
    val DD = DegreeDays(request.body, azureMapsService)

    val postcodeMetrics = getDDPostcodeMetrics(DD)

    // Rename variable for better clarity
    val processedMetricsFutures = postcodeMetrics.flatMap {
      case (Right(w), Right(hdd), Right(cdd)) => Future.successful(Seq(Right(w), Right(hdd), Right(cdd)))
      case _ => getDDAzureMetrics(DD)
    }

    // Extract result processing logic to a method
    processedMetricsFutures.map(FieldNames.zip(_)).map(processMetrics)
  }

  // Extract function to process metrics into errors and results
  private def processMetrics(fieldResults: Seq[(String, Either[String, JsValue])]): Result = {
    val errors = fieldResults.collect {
      case (name, Left(error)) => Json.obj(name -> error)
    }
    val results = fieldResults.collect {
      case (name, Right(value)) => Json.obj(name -> value)
    }
    Ok(Json.obj(
      "values" -> results,
      "errors" -> errors
    ))
  }
}

