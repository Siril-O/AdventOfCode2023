package com.siril.advent.tasks.five

import com.siril.advent.tasks.Task

import scala.collection.mutable.ArrayBuffer

object AlmanacProcessor extends Task[Almanac, Long] {
  override def solve(almanac: Almanac): Long =
    almanac.seeds.map { seed =>
      var lastId = seed
      almanac.mappings.foreach(map => lastId = map.findCorrespondingDest(lastId))
      lastId
    }.min

  override def solveAdvanced(almanac: Almanac): Long = {
    val seedRanges = almanac.seeds.grouped(2).map(a => a.head -> (a.head + a(1))).toArray
    val transformed = seedRanges.flatMap { range =>
      var lastRanges = Array(range)
      almanac.mappings.foreach(mapping => lastRanges = lastRanges.flatMap(mapping.findCorrespondingDestRange))
      lastRanges
    }
    transformed.foldLeft(Long.MaxValue)((acc, range) => Math.min(range._1, acc))
  }


  private val seedsReg = "seeds\\s*:(.+)".r
  private val attributeReg = "([a-z]+)-to-([a-z]+) map:".r

  override def parseInput(raw: List[String]): Almanac =
    Almanac(
      seeds = raw.head match {
        case seedsReg(ids) => ids.trim.split(" ").map(_.toLong)
        case other => throw new IllegalArgumentException(s"Can not parse seeds: $other")
      },
      mappings = splitLinesToMaps(raw).map {
        case head :: rest => head match {
          case attributeReg(Attributes(src), Attributes(desc)) => Mapping(src, desc, rest.map(ValuesRange(_)).toArray)
        }
      }.toArray
    )

  private def splitLinesToMaps(raw: List[String]) = {
    val acc = ArrayBuffer(new ArrayBuffer[String]())
    for (item <- raw.drop(2).iterator) {
      if (item.isBlank) acc += new ArrayBuffer[String]() else acc.last += item
    }
    acc.toList.map(_.toList)
  }

  override def subDir: String = "five"
}


sealed trait Attribute

object Attributes {
  object Seed extends Attribute

  object Soil extends Attribute

  object Fertilizer extends Attribute

  object Water extends Attribute

  object Light extends Attribute

  object Temperature extends Attribute

  object Humidity extends Attribute

  object Location extends Attribute

  def unapply(input: String): Option[Attribute] = input.trim match {
    case "seed" => Some(Seed)
    case "soil" => Some(Soil)
    case "fertilizer" => Some(Fertilizer)
    case "water" => Some(Water)
    case "light" => Some(Light)
    case "temperature" => Some(Temperature)
    case "humidity" => Some(Humidity)
    case "location" => Some(Location)
    case _ => None
  }
}