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
import scalax.file.Path

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

  private val itemDiskCache = new DiskCache(Path("cache") / "item")
  private val spellDiskCache = new DiskCache(Path("cache") / "spell")
  private val itemCache: Cache[Item] = LruCache(maxCapacity = 1000)
  private val spellCache: Cache[Spell] = LruCache(maxCapacity = 1000)

  import HttpConduit._

  private val pipeline: HttpRequest => Future[HttpResponse] = sendReceive(throttler)

  private def toJson(s: String): JsObject = {
    // wowdb.com returns JSON surrounded with parentheses, which must be stripped
    val trimmed = s.substring(1, s.length - 1)
    JsonParser(trimmed).asJsObject
  }

  private def fetchItemJson(id: Int): Future[JsObject] = {
    log.debug("Fetching item {}", id)
    itemDiskCache.fromFuture(id) {
      pipeline(Get("/api/item/%d?cookieTest=1".format(id))).map(r => r.entity.asString)
    }.map(toJson).filter(!_.fields.contains("Error"))
  }

  private def fetchSpellJson(id: Int): Future[JsObject] = {
    log.debug("Fetching spell {}", id)
    spellDiskCache.fromFuture(id) {
      pipeline(Get("/api/spell/%d?cookieTest=1".format(id))).map(r => r.entity.asString)
    }.map(toJson)
  }

  private val feastRE = "Set out a .+? to feed".r
  private val restoresRE = "Restores ([0-9,\\.]+) (health|mana)( and (([0-9,\\.]+) )?mana)?".r
  private val comboRE = "Restores ([0-9,\\.]+) health and ([0-9,\\.]+) mana".r
  private val combo2RE = "Restores ([0-9,\\.]+) health and mana".r
  private val healthRE = "Restores ([0-9,\\.]+) health".r
  private val manaRE = "Restores ([0-9,\\.]+) mana".r
  private val commaRE = ",".r

  private val buffRE = "If you spend at least [0-9]+ seconds eating you will become well fed.+".r

  private def parseNumber(s: String): Int = {
    // some numbers have decimals so toDouble to parse and then toInt to round down
    commaRE.replaceAllIn(s, "").toDouble.toInt
  }

  private def buildSpell(spellJson: JsObject): Future[Spell] = future {
    val id = spellJson.fields("ID").convertTo[Int]
    val desc = spellJson.fields("AuraDescriptionParsed").convertTo[String]
    val effects: Seq[SpellEffect] = {
      if (feastRE.findFirstIn(desc).isDefined) {
        // ignore feasts
        Seq.empty
      }
      else {
        val restore: Seq[SpellEffect] = restoresRE.findFirstIn(desc) match {
          case Some(comboRE(health, mana)) => Seq(HealthAndMana(parseNumber(health), parseNumber(mana)))
          case Some(combo2RE(amount)) => Seq(HealthAndMana(parseNumber(amount), parseNumber(amount)))
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

  private def getSpell(id: Int): Future[Spell] = {
    spellCache.fromFuture(id) {
      for (spellJson <- fetchSpellJson(id); spell <- buildSpell(spellJson)) yield spell
    }
  }

  private def buildItem(itemJson: JsObject): Future[Item] = {
    val id = itemJson.fields("ID").convertTo[Int]
    val name = itemJson.fields("Name").convertTo[String]
    val flags1 = itemJson.fields("Flags1").convertTo[Int]
    val conjured = (flags1 & 0x2) == 0x2
    val spellInfos = itemJson.fields("Spells").convertTo[Seq[Map[String, JsValue]]]
    val spells = Future.sequence(spellInfos.map(info => info("SpellID") match {
      case id:JsNumber => Some(getSpell(id.value.toInt))
      case _ => None
    }).flatten)
    spells.map { spells =>
      Item(id, name, conjured, spells.map(_.effects).flatten)
    }
  }

  private def getItem(id: Int): Future[Item] = {
    itemCache.fromFuture(id) {
      for (itemJson <- fetchItemJson(id); item <- buildItem(itemJson)) yield item
    }
  }

  def receive = {
    case GetItem(id) => getItem(id) pipeTo sender
  }
}

object WowDbApi {
  case class GetItem(id: Int)
}
