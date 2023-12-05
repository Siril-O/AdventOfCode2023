package com.siril.advent.tasks.four

case class Card(id: String, winning: Array[Int], submitted: Array[Int]){
  val intersect: Set[Int] = winning.toSet.intersect(submitted.toSet)
}
