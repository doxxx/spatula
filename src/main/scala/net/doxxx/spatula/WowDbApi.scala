package net.doxxx.spatula

import akka.actor._
import spray.can.client.HttpClient
import spray.client.HttpConduit
import spray.http._
import spray.io._
import spray.json._
import spray.caching._
import scala.concurrent._
import ExecutionContext.Implicits.global

class WowDbApi(implicit val system: ActorSystem) {
  val ioBridge = IOExtension(system).ioBridge()
  val httpClient = system.actorOf(Props(new HttpClient(ioBridge)))

  val conduit = system.actorOf(
    props = Props(new HttpConduit(httpClient, "www.wowdb.com", 80)),
    name = "http-conduit"
  )

  val itemCache: Cache[JsObject] = LruCache(maxCapacity = 1000)
  val spellCache: Cache[JsObject] = LruCache(maxCapacity = 1000)

  import HttpConduit._

  val pipeline: HttpRequest => Future[HttpResponse] = sendReceive(conduit)

  def toJson(r: HttpResponse): JsValue = {
    val text = r.entity.asString
    // wowdb.com returns JSON surrounded with parentheses, which must be stripped
    val trimmed = text.substring(1, text.length - 1)
    JsonParser(trimmed)
  }

  def fetchItem(id: Int): Future[JsObject] = {
    itemCache.fromFuture(id) {
      pipeline(Get("/api/item/%d?cookieTest=1".format(id))).map(toJson).map(_.asJsObject)
    }
  }

  def fetchSpell(id: Int): Future[JsObject] = {
    spellCache.fromFuture(id) {
      pipeline(Get("/api/spell/%d?cookieTest=1".format(id))).map(toJson).map(_.asJsObject)
    }
  }
}
