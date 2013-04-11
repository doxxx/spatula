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
  import DefaultJsonProtocol._
  import WowDbApi._

  private val ioBridge = IOExtension(context.system).ioBridge()
  private val httpClient = context.system.actorOf(Props(new HttpClient(ioBridge)))

  private val conduit = context.system.actorOf(
    props = Props(new HttpConduit(httpClient, "www.wowdb.com", 80)),
    name = "http-conduit"
  )

  log.info("Request rate: {}", settings.requestRate)
  private val throttler = context.system.actorOf(Props(new TimerBasedThrottler(settings.requestRate)), "http-conduit-throttler")
  throttler ! SetTarget(Some(conduit))

  private val itemCache: Cache[Item] = LruCache(maxCapacity = 1000)
  private val spellCache: Cache[Spell] = LruCache(maxCapacity = 1000)

  import HttpConduit._

  private val pipeline: HttpRequest => Future[HttpResponse] = sendReceive(throttler)

  private def toJson(r: HttpResponse): JsObject = {
    val text = r.entity.asString
    // wowdb.com returns JSON surrounded with parentheses, which must be stripped
    val trimmed = text.substring(1, text.length - 1)
    JsonParser(trimmed).asJsObject
  }

  private def fetchItem(id: Int): Future[JsObject] = {
    log.debug("Fetching item {}", id)
    pipeline(Get("/api/item/%d?cookieTest=1".format(id))).map(toJson)
  }

  private def fetchSpell(id: Int): Future[JsObject] = {
    log.debug("Fetching spell {}", id)
    pipeline(Get("/api/spell/%d?cookieTest=1".format(id))).map(toJson)
  }

  val feastRE = "Set out a (.+?) feast".r
  val restoresRE = "Restores ([0-9,\\.]+) (health|mana)( and ([0-9,\\.]+) mana)?".r
  val comboRE = "Restores ([0-9,\\.]+) health and ([0-9,\\.]+) mana".r
  val healthRE = "Restores ([0-9,\\.]+) health".r
  val manaRE = "Restores ([0-9,\\.]+) mana".r
  val commaRE = ",".r

  val buffRE = "If you spend at least ([0-9]+) seconds eating you will become well fed and gain (.+) for ([0-9]+) (.+).".r

  def parseNumber(s: String): Int = {
    // some numbers have decimals so toDouble to parse and then toInt to round down
    commaRE.replaceAllIn(s, "").toDouble.toInt
  }

  def buildSpell(id: Int): Future[Spell] = {
    spellCache.fromFuture(id) {
      fetchSpell(id).map { spellObj =>
        val desc = spellObj.fields("AuraDescriptionParsed").convertTo[String]
        val effects: Seq[SpellEffect] = {
          if (feastRE.findFirstIn(desc).isDefined) {
            // ignore feasts
            Seq.empty
          }
          else {
            val restore: Seq[SpellEffect] = restoresRE.findFirstIn(desc) match {
              case Some(comboRE(health, mana)) => Seq(HealthAndMana(parseNumber(health), parseNumber(mana)))
              case Some(healthRE(health)) => Seq(Health(parseNumber(health)))
              case Some(manaRE(mana)) => Seq(Mana(parseNumber(mana)))
              case None => Seq.empty
            }
            val buff: Seq[SpellEffect] = buffRE.findFirstIn(desc) match {
              case Some(buffDesc) => Seq(Buff(buffDesc))
              case None => Seq.empty
            }
            restore ++ buff
          }
        }
        log.debug("Fetched spell {}: '{}' => {}", id, desc, effects)
        Spell(id, effects)
      }
    }
  }

  def buildItem(itemObj: JsObject): Future[Item] = {
    val id = itemObj.fields("ID").convertTo[Int]
    val name = itemObj.fields("Name").convertTo[String]
    val flags1 = itemObj.fields("Flags1").convertTo[Int]
    val conjured = (flags1 & 0x2) == 0x2
    val spellObjs = itemObj.fields("Spells").convertTo[Seq[Map[String,Int]]]
    val spells = spellObjs.map(obj => buildSpell(obj("SpellID")))
    Future.sequence(spells).map { spells =>
      Item(id, name, conjured, spells.map(_.effects).flatten)
    }
  }

  def receive = {
    case FetchItem(id) => itemCache.fromFuture(id) {
      fetchItem(id).flatMap(buildItem) pipeTo sender
    }

  }
}

object WowDbApi {
  case class FetchItem(id: Int)

  sealed trait SpellEffect
  sealed trait Refreshment extends SpellEffect
  case class Health(amount: Int) extends Refreshment
  case class Mana(amount: Int) extends Refreshment
  case class HealthAndMana(health: Int, mana: Int) extends Refreshment
  case class Buff(desc: String) extends SpellEffect
  case class Spell(id: Int, effects: Seq[SpellEffect])
  case class Item(id: Int, name: String, conjured: Boolean, effects: Seq[SpellEffect])
}
