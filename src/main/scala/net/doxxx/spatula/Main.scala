package net.doxxx.spatula

import scala.concurrent.ExecutionContext.Implicits.global
import akka.actor.ActorSystem
import scala.util.{Failure, Success}
import scala.concurrent._
import scala.concurrent.duration._
import spray.json.{JsObject, DefaultJsonProtocol}
import java.io.{BufferedWriter, FileWriter, File}
import scala.io.Source

object Main {
  import DefaultJsonProtocol._

  implicit val system = ActorSystem()

  val api = new WowDbApi

  def main(args: Array[String]) {
    if (args.length != 2) {
      println("Syntax: spatula <item ids file> <output lua file>")
      sys.exit(-1)
    }

    val inFile = new File(args(0))
    val outFile = new File(args(1))

//    val itemIdsString = "58257,58259,58261,58263,58265,62679,65499,68140,70924,70925,70926,70927,74636,74641,74642,74643,74644,74645,74646,74647,74648,74649,74650,74651,74652,74653,74654,74655,74656,74919,74921,75016,75026,75037,75038,79320,80313,80610,80618,81175,81400,81401,81402,81403,81404,81405,81406,81407,81408,81409,81410,81411,81412,81413,81414,81415,81916,81918,81920,81921,81923,81924,82344,82449,82450,82451,83094,85501,85504,86026,86057,86069,86070,86073,86074,86432,86508,87226,87228,87230,87232,87234,87236,87238,87240,87242,87244,87246,87248,87253,87264,88379,88382,88530,88531,88578,88586,89683,90135,90735,94535"
//    val itemIds = itemIdsString.split(",").toSeq.map(_.toInt)
    val itemIds = Source.fromFile(inFile).getLines().map(_.split(',')).flatten.map(_.toInt).toSeq

    val fs = for (id <- itemIds) yield {
      val f = api.fetchItem(id)
      for (
        obj <- f;
        item <- buildItem(obj)
      ) yield item
    }

    val f = Future.sequence(fs)

    f.onComplete {
      case Success(items) => {

        val healthItems = for (item <- items) yield categorizeHealthItem(item)
        val manaItems = for (item <- items) yield categorizeManaItem(item)

        val categorizedItems = healthItems ++ manaItems

        val lua = buildLua(categorizedItems)
        val w = new BufferedWriter(new FileWriter(outFile))
        lua.foreach { line =>
          w.write(line)
          w.write('\n')
        }
        w.close()
      }
      case Failure(t) => t.printStackTrace()
    }

    Await.ready(f, Duration.Inf)

    system.shutdown()
  }

  def buildLua(items: Seq[Option[CategorizedItem]]): Seq[String] = {
    val filtered = items.filter(_.isDefined).map(_.get)
    val grouped = filtered.groupBy(_.category)
    val mapped = grouped.mapValues(_.toSet)

    val lua = mapped.map {
      case (category, catItems) => {
        val itemIds = catItems.map(item => "%d:%d".format(item.id, item.value)).mkString(",")
        """PT:AddData("%s","%s")""".format(category, itemIds)
      }
    }

    lua.toSeq
  }

  def categorizeHealthItem(item: Item): Option[CategorizedItem] = {
    item match {
      // health food
      case Item(id, _, true, Seq(HealthAndMana(value, _))) =>           Some(CategorizedItem("MMM.Consumable.Food.Combo.Conjured", id, value))
      case Item(id, _, true, Seq(Health(value))) =>                     Some(CategorizedItem("MMM.Consumable.Food.Basic.Conjured", id, value))
      case Item(id, _, true, Seq(HealthAndMana(value, _), Buff(_))) =>  Some(CategorizedItem("MMM.Consumable.Food.Buff.Combo.Conjured", id, value))
      case Item(id, _, true, Seq(Health(value), Buff(_))) =>            Some(CategorizedItem("MMM.Consumable.Food.Buff.Basic.Conjured", id, value))
      case Item(id, _, false, Seq(HealthAndMana(value, _))) =>          Some(CategorizedItem("MMM.Consumable.Food.Combo.Non-Conjured", id, value))
      case Item(id, _, false, Seq(Health(value))) =>                    Some(CategorizedItem("MMM.Consumable.Food.Basic.Non-Conjured", id, value))
      case Item(id, _, false, Seq(HealthAndMana(value, _), Buff(_))) => Some(CategorizedItem("MMM.Consumable.Food.Buff.Combo.Non-Conjured", id, value))
      case Item(id, _, false, Seq(Health(value), Buff(_))) =>           Some(CategorizedItem("MMM.Consumable.Food.Buff.Basic.Non-Conjured", id, value))
      case _ => None
    }
  }

  def categorizeManaItem(item: Item): Option[CategorizedItem] = {
    item match {
      case Item(id, _, true, Seq(HealthAndMana(_, value))) =>           Some(CategorizedItem("MMM.Consumable.Food.Combo.Conjured.Mana", id, value))
      case Item(id, _, true, Seq(Mana(value))) =>                       Some(CategorizedItem("MMM.Consumable.Food.Basic.Conjured.Mana", id, value))
      case Item(id, _, true, Seq(HealthAndMana(_, value), Buff(_))) =>  Some(CategorizedItem("MMM.Consumable.Food.Buff.Combo.Conjured.Mana", id, value))
      case Item(id, _, true, Seq(Mana(value), Buff(_))) =>              Some(CategorizedItem("MMM.Consumable.Food.Buff.Basic.Conjured.Mana", id, value))
      case Item(id, _, false, Seq(HealthAndMana(_, value))) =>          Some(CategorizedItem("MMM.Consumable.Food.Combo.Non-Conjured.Mana", id, value))
      case Item(id, _, false, Seq(Mana(value))) =>                      Some(CategorizedItem("MMM.Consumable.Food.Basic.Non-Conjured.Mana", id, value))
      case Item(id, _, false, Seq(HealthAndMana(_, value), Buff(_))) => Some(CategorizedItem("MMM.Consumable.Food.Buff.Combo.Non-Conjured.Mana", id, value))
      case Item(id, _, false, Seq(Mana(value), Buff(_))) =>             Some(CategorizedItem("MMM.Consumable.Food.Buff.Basic.Non-Conjured.Mana", id, value))
      // uncategorized
      case _ => None
    }
  }

  case class CategorizedItem(category: String, id: Int, value: Int)

  val restoresRE = "Restores ([0-9,]+) (health|mana)( and ([0-9,]+) mana)?".r
  val comboRE = "Restores ([0-9,]+) health and ([0-9,]+) mana".r
  val healthRE = "Restores ([0-9,]+) health".r
  val manaRE = "Restores ([0-9,]+) mana".r
  val commaRE = ",".r

  val buffRE = "If you spend at least ([0-9]+) seconds eating you will become well fed and gain ([0-9]+) ([^ ]+) for ([0-9]+) (.+).".r

  def parseNumber(s: String): Int = {
    commaRE.replaceAllIn(s, "").toInt
  }

  def buildSpellEffects(spellId: Int): Future[Seq[SpellEffect]] = {
    api.fetchSpell(spellId).map { spellObj =>
      val desc = spellObj.fields("AuraDescriptionParsed").convertTo[String]
//      println(desc)
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

  def buildItem(itemObj: JsObject): Future[Item] = {
    val id = itemObj.fields("ID").convertTo[Int]
    val name = itemObj.fields("Name").convertTo[String]
    val flags1 = itemObj.fields("Flags1").convertTo[Int]
    val conjured = (flags1 & 0x2) == 0x2
    val spells = itemObj.fields("Spells").convertTo[Seq[Map[String,Int]]]
    val spellEffects = spells.map { spellObj =>
      val spellId = spellObj("SpellID")
      buildSpellEffects(spellId)
    }
    Future.sequence(spellEffects).map { effects =>
      Item(id, name, conjured, effects.flatten)
    }
  }

  sealed trait SpellEffect
  sealed trait Refreshment extends SpellEffect
  case class Health(amount: Int) extends Refreshment
  case class Mana(amount: Int) extends Refreshment
  case class HealthAndMana(health: Int, mana: Int) extends Refreshment
  case class Buff(desc: String) extends SpellEffect
  case class Item(id: Int, name: String, conjured: Boolean, effects: Seq[SpellEffect])

}
