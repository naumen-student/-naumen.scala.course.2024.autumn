import utest._
import Exercises.Vector2D
import java.{util => ju}

object Test extends TestSuite{

    val tests = Tests{
        'test_divBy3Or7 - {
            assert(Exercises.divBy3Or7(1, 3) == Seq(3))
            assert(Exercises.divBy3Or7(5, 9) == Seq(6, 7, 9))
            assert(Exercises.divBy3Or7(0, 100) == Seq(0, 3, 6, 7, 9, 12, 14, 15, 18, 21, 24, 27, 28, 30, 33, 35, 36, 39, 42, 45, 48, 49, 51, 54, 56, 57, 60, 63, 66, 69, 70, 72, 75, 77, 78, 81, 84, 87, 90, 91, 93, 96, 98, 99))
        }
        'test_sumOfDivBy3Or5 - {
            assert(Exercises.sumOfDivBy3Or5(0, 1) == 0)
            assert(Exercises.sumOfDivBy3Or5(0, 10) == 33)
            assert(Exercises.sumOfDivBy3Or5(14, 16) == 15)
        }
        'test_primeFactor - {
            assert(Exercises.primeFactor(1) == Seq.empty)
            assert(Exercises.primeFactor(80) == Seq(2, 5))
            assert(Exercises.primeFactor(9) == Seq(3))
        }
        'test_sumScalars - {
            assert(Exercises.sumScalars(Vector2D(1, 1), Vector2D(5, 10), Vector2D(3, 4), Vector2D(2, 3)) == 33)
            assert(Exercises.sumScalars(Vector2D(0, 0), Vector2D(5123, 112310), Vector2D(0, 0), Vector2D(2123, 89125)) == 0)
            assert(Exercises.sumScalars(Vector2D(2, 2), Vector2D(3, 3), Vector2D(4, 4), Vector2D(5, 5)) == 52)
        }
        'test_sumCosines - {
            assert(Exercises.sumCosines(Vector2D(3, 4), Vector2D(6, 8), Vector2D(3, 4), Vector2D(6, 8)) == 2)
            assert(Exercises.sumCosines(Vector2D(1, 0), Vector2D(1, 0), Vector2D(1, 0), Vector2D(1, 0)) == 2)
            assert(Exercises.sumCosines(Vector2D(9, 12), Vector2D(3, 4), Vector2D(1, 0), Vector2D(1, 0)) == 2)
        }
        'test_sortByHeavyweight - {
            assert(Exercises.sortByHeavyweight(Map.empty) == Seq.empty)
            assert(Exercises.sortByHeavyweight(Map("Stonks" -> (100, 100), "NoStonks" -> (100, 99))) == Seq("NoStonks", "Stonks"))
            assert(Exercises.sortByHeavyweight(Map("A" -> (0, 100000), "B" -> (1, 0.000000001))) == Seq("A", "B"))
        }
    }
}
