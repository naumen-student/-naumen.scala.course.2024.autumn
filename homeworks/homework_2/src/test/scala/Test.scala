import utest._

object Test extends TestSuite{

    val tests = Tests{
        'test_divBy3Or7 - {
            assert(Exercises.divBy3Or7(1, 3) == Seq(3))
            assert(Exercises.divBy3Or7(5, 9) == Seq(6, 7, 9))
            assert(Exercises.divBy3Or7(0, 100) == Seq(0, 3, 6, 7, 9, 12, 14, 15, 18, 21, 24, 27, 28, 30, 33, 35, 36, 39, 42, 45, 48, 49, 51, 54, 56, 57, 60, 63, 66, 69, 70, 72, 75, 77, 78, 81, 84, 87, 90, 91, 93, 96, 98, 99))
        }
        'test_sumOfDivBy3Or5 - {
            assert(Exercises.sumOfDivBy3Or5(3, 7) == 14)
            assert(Exercises.sumOfDivBy3Or5(4, 7) == 11)
            assert(Exercises.sumOfDivBy3Or5(0, 0) == 0)
            assert(Exercises.sumOfDivBy3Or5(-5, 0) == -8)
        }
        'test_primeFactor - {
            assert(Exercises.primeFactor(80) == Seq(2,5))
            assert(Exercises.primeFactor(333) == Seq(3,37))
            assert(Exercises.primeFactor(0) == Seq())
        }
        'test_sumScalars - {
            assert(Exercises.sumScalars(Vector2D(0, 0), Vector2D(0, 0), Vector2D(0, 0), Vector2D(0, 0)) == 0)
            assert(Exercises.sumScalars(Vector2D(1, 0), Vector2D(1, 0), Vector2D(0, 1), Vector2D(-1, 0)) == 1)
            assert(Exercises.sumScalars(Vector2D(1, 2), Vector2D(3, 4), Vector2D(5, 6), Vector2D(7, 8)) == 94)
        }
        'test_sumCosines - {
            assert(Exercises.sumCosines(Vector2D(0, 1), Vector2D(1, 0), Vector2D(0, -1), Vector2D(-1, 0)) == 0)
            assert(Exercises.sumCosines(Vector2D(1, 0), Vector2D(1, 0), Vector2D(0, 1), Vector2D(-1, 0)) == 1)
            assert(Exercises.sumCosines(Vector2D(1, 0), Vector2D(-1, 0), Vector2D(-1, 0), Vector2D(1, 0)) == -2)
        }
        'test_sortByHeavyweight - {
            assert(Exercises.sortByHeavyweight(Map("Car" -> (1500, 20), "Feather" -> (10, 0.05))) == Seq("Feather", "Car"))
            assert(Exercises.sortByHeavyweight(Map.empty) == Seq())
        }
    }
}
