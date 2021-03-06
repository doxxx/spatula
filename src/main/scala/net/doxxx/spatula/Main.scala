package net.doxxx.spatula

import akka.actor._
import akka.pattern._
import akka.util.Timeout
import akka.event.Logging
import com.typesafe.config.ConfigFactory
import scala.util.{Failure, Success}
import scala.concurrent._
import scala.concurrent.duration._
import ExecutionContext.Implicits.global
import scalax.file.Path

object Main {
  import WowDbApi._

  val config = ConfigFactory.load()
  implicit val system = ActorSystem("spatula", config)
  val log = Logging(system, "spatula")

  val settings = new Settings(config)
  implicit val timeout = Timeout(5.minutes)

  val api = system.actorOf(Props(new WowDbApi(settings)), "wowdb-api")

  def main(args: Array[String]) {
    if (args.length != 2) {
      println("Syntax: spatula <item ids file> <output lua file>")
      sys.exit(-1)
    }

    val inFile = Path(args(0))
    val outFile = Path(args(1))

    log.info("Reading item IDs from {}", inFile.path)
    val itemIds = inFile.lines().map(_.split(',')).flatten.map(_.toInt).toSet

    val fs = for (id <- itemIds) yield {
      val f = (api ? GetItem(id)).mapTo[Item].map(Some(_)).recover({
        case _ => {
          log.error("Failed to get item #{}", id)
          None
        }
      })

      f.onSuccess {
        case Some(i: Item) => log.info("Fetched item: {} -> {}", i.id, i.name)
      }

      f
    }

    val f = Future.sequence(fs).map(_.flatten).map { items =>
      val healthItems = for (item <- items) yield categorizeHealthItem(item)
      val manaItems = for (item <- items) yield categorizeManaItem(item)

      val categorizedItems = (healthItems ++ manaItems).flatten

      buildLua(categorizedItems)
    }.andThen {
      case Success(lua) => {
        try {
          log.info("Writing LUA to {}", outFile.path)
          outFile.writeStrings(lua, sys.props("line.separator"))
        }
        catch {
          case t: Throwable => log.error(t, "Could not write output file {}", outFile)
        }
      }
      case Failure(t) => log.error(t, "Error")
    }

    Await.ready(f, Duration.Inf)

    system.shutdown()
  }

  def buildLua(items: Iterable[CategorizedItem]): Seq[String] = {
    val grouped = items.groupBy(_.category)
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


}
