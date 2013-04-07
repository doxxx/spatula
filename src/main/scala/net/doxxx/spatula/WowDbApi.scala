package net.doxxx.spatula

import akka.actor._
import akka.contrib.throttle.TimerBasedThrottler
import akka.contrib.throttle.Throttler.SetTarget
import akka.pattern._
import spray.can.client.HttpClient
import spray.client.HttpConduit
import spray.http._
import spray.io._
import spray.json._
import spray.caching._
import scala.concurrent._
import ExecutionContext.Implicits.global

class WowDbApi(settings: Settings) extends Actor with ActorLogging {
  private val ioBridge = IOExtension(context.system).ioBridge()
  private val httpClient = context.system.actorOf(Props(new HttpClient(ioBridge)))

  private val conduit = context.system.actorOf(
    props = Props(new HttpConduit(httpClient, "www.wowdb.com", 80)),
    name = "http-conduit"
  )

  log.info("Request rate: {}", settings.requestRate)
  private val throttler = context.system.actorOf(Props(new TimerBasedThrottler(settings.requestRate)), "http-conduit-throttler")
  throttler ! SetTarget(Some(conduit))

  private val itemCache: Cache[JsObject] = LruCache(maxCapacity = 1000)
  private val spellCache: Cache[JsObject] = LruCache(maxCapacity = 1000)

  import HttpConduit._

  private val pipeline: HttpRequest => Future[HttpResponse] = sendReceive(throttler)

  private def toJson(r: HttpResponse): JsValue = {
    val text = r.entity.asString
    // wowdb.com returns JSON surrounded with parentheses, which must be stripped
    val trimmed = text.substring(1, text.length - 1)
    JsonParser(trimmed)
  }

  private def fetchItem(id: Int): Future[JsObject] = {
    itemCache.fromFuture(id) {
      log.debug("Fetching item {}", id)
      pipeline(Get("/api/item/%d?cookieTest=1".format(id))).map(toJson).map(_.asJsObject)
    }
  }

  private def fetchSpell(id: Int): Future[JsObject] = {
    spellCache.fromFuture(id) {
      log.debug("Fetching spell {}", id)
      pipeline(Get("/api/spell/%d?cookieTest=1".format(id))).map(toJson).map(_.asJsObject)
    }
  }

  import WowDbApi._

  def receive = {
    case FetchItem(id) => fetchItem(id) pipeTo sender
    case FetchSpell(id) => fetchSpell(id) pipeTo sender
  }
}

object WowDbApi {
  case class FetchItem(id: Int)
  case class FetchSpell(id: Int)
}
