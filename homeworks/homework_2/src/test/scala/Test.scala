import utest._

object Test extends TestSuite {

  val tests = Tests {
    'test_divBy3Or7 - {
      assert(Exercises.divBy3Or7(1, 3) == Seq(3))
      assert(Exercises.divBy3Or7(5, 9) == Seq(6, 7, 9))
      assert(Exercises.divBy3Or7(0, 100) == Seq(0, 3, 6, 7, 9, 12, 14, 15, 18, 21, 24, 27, 28, 30, 33, 35, 36, 39, 42, 45, 48, 49, 51, 54, 56, 57, 60, 63, 66, 69, 70, 72, 75, 77, 78, 81, 84, 87, 90, 91, 93, 96, 98, 99))
    }
    'test_sumOfDivBy3Or5 - {
      assert(Exercises.sumOfDivBy3Or5(1, 3) == 3)
      assert(Exercises.sumOfDivBy3Or5(5, 9) == 20)
      assert(Exercises.sumOfDivBy3Or5(0, 20) == 98)
    }
    'test_primeFactor - {
      assert(Exercises.primeFactor(80) == Seq(2, 5))
      assert(Exercises.primeFactor(98) == Seq(2, 7))
      assert(Exercises.primeFactor(4) == Seq(2))
      assert(Exercises.primeFactor(2) == Seq(2))
      assert(Exercises.primeFactor(1) == Seq())
      assert(Exercises.primeFactor(0) == Seq())
    }
    'test_sumScalars - {
      assert(Exercises.sumScalars(Exercises.Vector2D(0, 0),
        Exercises.Vector2D(0, 0),
        Exercises.Vector2D(0, 0),
        Exercises.Vector2D(0, 0)) == 0)
      assert(Exercises.sumScalars(Exercises.Vector2D(1, 2),
        Exercises.Vector2D(3, 4),
        Exercises.Vector2D(5, 6),
        Exercises.Vector2D(7, 8)) == 94)
      assert(Exercises.sumScalars(Exercises.Vector2D(1, 2),
        Exercises.Vector2D(3, -4),
        Exercises.Vector2D(-5, 6),
        Exercises.Vector2D(-7, -8)) == -18)
    }
    'test_sumCosines - {
      assert(Exercises.sumCosines(Exercises.Vector2D(1, 0),
        Exercises.Vector2D(0, 1),
        Exercises.Vector2D(6, 6),
        Exercises.Vector2D(1, 1)) == 1)
      assert(Exercises.sumCosines(Exercises.Vector2D(0, -3),
        Exercises.Vector2D(3, -4),
        Exercises.Vector2D(3, -4),
        Exercises.Vector2D(3, 0)) == 1.4)
    }
    'test_sort - {
      assert(Exercises.sortByHeavyweight() == Seq("Tin", "Platinum", "Nickel", "Aluminum", "Titanium", "Lead", "Sodium", "Uranium", "Gold", "Tungsten", "Zirconium", "Chrome", "Iron", "Copper", "Silver", "Plutonium", "Cobalt", "Cesium", "Calcium", "Lithium", "Magnesium", "Potassium", "Graphite"))
    }
  }
}
