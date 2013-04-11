package net.doxxx.spatula

sealed trait SpellEffect
sealed trait Refreshment extends SpellEffect
case class Health(amount: Int) extends Refreshment
case class Mana(amount: Int) extends Refreshment
case class HealthAndMana(health: Int, mana: Int) extends Refreshment
case class Buff(desc: String) extends SpellEffect
case class Spell(id: Int, effects: Seq[SpellEffect])
case class Item(id: Int, name: String, conjured: Boolean, effects: Seq[SpellEffect])
