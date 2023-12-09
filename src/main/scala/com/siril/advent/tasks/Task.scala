package com.siril.advent.tasks

trait Task[T, R] {
  def solve(input: T): R

  def solveAdvanced(t: T): R

  def parseInput(raw: List[String]): T

  def subDir: String
}
