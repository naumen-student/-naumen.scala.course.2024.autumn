import utest._
import Exercises.Vector2D
object Test extends TestSuite{

    val tests = Tests{
        'test_divBy3Or7 - {
            assert(Exercises.divBy3Or7(1, 3) == Seq(3))
            assert(Exercises.divBy3Or7(5, 9) == Seq(6, 7, 9))
            assert(Exercises.divBy3Or7(0, 100) == Seq(0, 3, 6, 7, 9, 12, 14, 15, 18, 21, 24, 27, 28, 30, 33, 35, 36, 39, 42, 45, 48, 49, 51, 54, 56, 57, 60, 63, 66, 69, 70, 72, 75, 77, 78, 81, 84, 87, 90, 91, 93, 96, 98, 99))
            assert(Exercises.divBy3Or7(-100,0)==Exercises.divBy3Or7(0,100).map(-_).reverse)
        }
        'test_sumOfDivBy3Or5-{
            assert(Exercises.sumOfDivBy3Or5(1,3)==3)
            assert(Exercises.sumOfDivBy3Or5(1,5)==8)
            assert(Exercises.sumOfDivBy3Or5(-100,100)==0)
            assert(Exercises.sumOfDivBy3Or5(0,100)==Seq(3,5,6,9,10,12,15,18,20,21,24,25,27,30,33,35,36,39,40,42,45,48,50,51,54,55,57,60,63,65,66,69,70,72,75,78,80,81,84,85,87,90,93,95,96,99,100).sum)
        }
        'test_PrimeFactor-{
            assert(Exercises.primeFactor(0)==Seq())
            assert(Exercises.primeFactor(1)==Seq(1))
            assert(Exercises.primeFactor(7)==Seq(7))
            assert(Exercises.primeFactor(8)==Seq(2))
            assert(Exercises.primeFactor(30)==Seq(2,3,5))
            assert(Exercises.primeFactor(100)==Seq(2,5))
            assert(Exercises.primeFactor(120)==Seq(2,3,5))
        }
       'test_vectorMath-{
           assert(Exercises.sumScalars(Vector2D(1,1),Vector2D(5,4),Vector2D(1,1),Vector2D(7,6))==22)
           assert(Exercises.sumScalars(Vector2D(0,0),Vector2D(5,100),Vector2D(0,0),Vector2D(100,7))==0)
           assert(Exercises.sumScalars(Vector2D(5,6),Vector2D(5.5,84),Vector2D(17.5,21),Vector2D(1.5,3))==Exercises.sumScalars(Vector2D(17.5,21),Vector2D(1.5,3),Vector2D(5,6),Vector2D(5.5,84)))
           assert(Exercises.sumScalars(Vector2D(5,6),Vector2D(5.5,84),Vector2D(17.5,21),Vector2D(1.5,3))==Exercises.sumScalars(Vector2D(5.5,84),Vector2D(5,6),Vector2D(1.5,3),Vector2D(17.5,21)))
           assert(Exercises.sumCosines(Vector2D(4,5),Vector2D(-5,4),Vector2D(3,4),Vector2D(-4,3))==0)
           assert(Exercises.sumCosines(Vector2D(4,5),Vector2D(-5,4),Vector2D(3,4),Vector2D(18,24))==1)
           assert(Exercises.sumCosines(Vector2D(4,5),Vector2D(-20,-25),Vector2D(3,4),Vector2D(18,24))==0)
       }
        'test_sortByHeavyweight-{
            assert(Exercises.sortByHeavyweight(Map("1"->(4,1.0),"2"->(2,1.0),"3"->(3,1.0)))==Seq("2","3","1"))
            assert(Exercises.sortByHeavyweight(Map("1"->(1,5.0),"2"->(1,7.0),"3"->(1,1.5)))==Seq("3","1","2"))
            assert(Exercises.sortByHeavyweight(Map("1"->(3,3.0),"2"->(1,7.0),"3"->(2,6.5),"4"->(5,0.01)))==Seq("4","2","3","1"))
            assert(Exercises.sortByHeavyweight() ==Seq("Tin","Platinum","Nickel","Aluminum","Titanium","Lead","Sodium","Uranium","Gold","Tungsten","Zirconium","Chrome","Iron","Copper","Silver","Plutonium","Cobalt","Cesium","Calcium","Lithium","Magnesium","Potassium","Graphite"))
        }
    }
}
