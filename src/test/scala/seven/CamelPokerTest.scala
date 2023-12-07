package seven

import com.siril.advent.tasks.TestCtx
import com.siril.advent.tasks.seven.Seven
import com.siril.advent.tasks.seven.SevenF.{Input, Output}
import org.specs2.mutable.Specification

class CamelPokerTest extends Specification {

  "CamelPoker" should {
    "solve test input" in new Ctx {
      instance.solve(testInput) must beEqualTo(6440)
    }
    "solve task input" in new Ctx {
      instance.solve(taskInput) must beEqualTo(250474325)
    }
    "solve advanced test input" in new Ctx {
      instance.solveAdvanced(testInput) must beEqualTo(5905)
    }
    "solve advanced task input" in new Ctx {
      instance.solveAdvanced(taskInput) must beEqualTo(248909434)
    }
  }

  trait Ctx extends TestCtx[Input, Output] {
    override def instance: Seven.type = Seven
  }
}
