package com.siril.advent.tasks.seven

import com.siril.advent.tasks.Task
import com.siril.advent.tasks.seven.SevenF.{Input, Output}

object Seven extends Task[Input, Output] {
  override def solve(v1: Input): Output = ???

  override def parseInput(raw: List[String]): Input = ???

  override def subDir: String = "seven"

  override def solveAdvanced(t: Input): Output = ???
}


object SevenF {
  type Input = Unit
  type Output = Unit
}