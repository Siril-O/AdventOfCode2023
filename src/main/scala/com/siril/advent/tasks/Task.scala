package com.siril.advent.tasks

trait Task[T, R] {
  def solve(almanac: T): R

  def solveAdvanced(t: T): R

  def parseInput(raw: List[String]): T

  def subDir: String
}
