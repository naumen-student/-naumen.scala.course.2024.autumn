import Exercises.Vector2D
import utest._

object Test extends TestSuite {

  val tests = Tests {
    'test_divBy3Or7 - {
      assert(Exercises.divBy3Or7(1, 3) == Seq(3))
      assert(Exercises.divBy3Or7(5, 9) == Seq(6, 7, 9))
      assert(Exercises.divBy3Or7(0, 100) == Seq(0, 3, 6, 7, 9, 12, 14, 15, 18, 21, 24, 27, 28, 30, 33, 35, 36, 39, 42, 45, 48, 49, 51, 54, 56, 57, 60, 63, 66, 69, 70, 72, 75, 77, 78, 81, 84, 87, 90, 91, 93, 96, 98, 99))
    }

    'test_sumOfDivBy3Or5 - {
      assert(Exercises.sumOfDivBy3Or5(1, 10) == 33)
      assert(Exercises.sumOfDivBy3Or5(10, 30) == 202)
      assert(Exercises.sumOfDivBy3Or5(0, 50) == 593)
    }

    'test_primeFactor - {
      assert(Exercises.primeFactor(8) == Seq(2))
      assert(Exercises.primeFactor(80) == Seq(2, 5))
      assert(Exercises.primeFactor(98) == Seq(2, 7))
    }

    'test_sumScalars - {
      assert(Exercises.sumScalars(Vector2D(3, 3), Vector2D(3, 3), Vector2D(2, 2), Vector2D(2, 2)) == 26)
      assert(Exercises.sumScalars(Vector2D(1, 1), Vector2D(1, 1), Vector2D(1, 1), Vector2D(1, 1)) == 4)
    }
    'test_sumCosines - {
      assert(Exercises.sumCosines(Vector2D(1, 0), Vector2D(0, 1), Vector2D(1, 0), Vector2D(1, 0)) == 1)
      assert(Exercises.sumCosines(Vector2D(1, 0), Vector2D(1, 0), Vector2D(1, 0), Vector2D(1, 0)) == 2)
    }
    'test_sortByHeavyweight - {
      assert(Exercises.sortByHeavyweight(
        Map(
          "Aluminum" -> (12, 2.6889), "Graphite" -> (1, 2.1),
          "Magnesium" -> (14, 1.738)
        )
      ) == Seq("Graphite", "Aluminum", "Magnesium"))
      assert(Exercises.sortByHeavyweight(Map.empty[String, (Int, Double)]) == Seq.empty[String])
    }
  }
}
