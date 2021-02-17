package filters

import akka.actor.ActorSystem
import akka.event.Logging
import akka.stream.Materializer
import com.typesafe.scalalogging.LazyLogging

import javax.inject.{Inject, Singleton}
import org.joda.time.DateTime
import play.api.mvc.{Filter, RequestHeader, Result}

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class LoggingFilter @Inject() (
                                actorSystem: ActorSystem,
                                implicit val executionContext: ExecutionContext,
                                implicit val mat: Materializer, ec: ExecutionContext) extends Filter with LazyLogging {

  def apply(nextFilter: RequestHeader => Future[Result])
           (requestHeader: RequestHeader): Future[Result] = {

    val startTime = System.currentTimeMillis

    nextFilter(requestHeader).map { result =>

      val endTime = System.currentTimeMillis
      val requestTime = endTime - startTime


      //"%h %l %u %t \"%r\" %>s %b"

      logger.info(s"""${requestHeader.host} - - ${requestTime} ${DateTime.now()} \"{}\" ${result.header.status} ${result.body.contentLength.getOrElse(0)}""", requestHeader.uri)

      result.withHeaders("Request-Time" -> requestTime.toString)
    }
  }
}

