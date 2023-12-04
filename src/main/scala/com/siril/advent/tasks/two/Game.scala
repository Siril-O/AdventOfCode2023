package com.siril.advent.tasks.two

case class Game(id: Int, sets: List[GameSet])

case class GameSet(cubes: List[(Int, Color)])

sealed trait Color

object Colors {

  object Red extends Color

  object Green extends Color

  object Blue extends Color

  def unapply(input: String): Option[Color] = input.trim match {
    case "blue" => Some(Colors.Blue)
    case "red" => Some(Colors.Red)
    case "green" => Some(Colors.Green)
    case other => None
  }

}