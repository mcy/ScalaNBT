package com.xorinc.scalanbt.example

import com.xorinc.scalanbt.tags._

object NBTExample extends App {

  // Create an empty TagCompound
  val map = TagCompound()

  // add some entries
  map.position = List(55D, 0D, -10.5)
  map.aName = "A thing"
  map.bytes = "jasdhfoaiweuhf".getBytes
  map.ident = -1
  map.boolean = true
  map.char = 'a'
  map.sym = 'symbol
  map.ident = 5

  // create a TagString
  val str = "foobar".nbt

  // create an empty TagList and add/remove elements
  val list = TagList() + "foo" + str + "baz" - "foo".nbt

  // print things
  println(str)
  println(list)
  println(map)

  // extract a TagList from `map` and print it
  println(map.position[TagList])

  // convert `map` into mojangson and print it
  println(map.mojangson)

  // create a TagList and pattern match on it
  val x = TagList("foo", "bar", "baz") match {
    case TagList(_, TagString(s @ "bar"), _) => s
    case _ => "nada"
  }

  // loop over a list
  for(TagString(s) <- list)
    println(s.toUpperCase)

  // loop over a compound
  for((_, TagString(s)) <- map)
    println(s)

  println(x)
}
