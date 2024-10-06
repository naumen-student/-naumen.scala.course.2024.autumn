import utest._

object Test extends TestSuite {
    val tests = Tests {
        'test_createTable - {
            val table = Table(3, 3)
            for (i <- 0 until 9) {
                assert(table.getCell(i / 3, i % 3).map(_.toString).contains("empty"))
            }
            assert(table.getCell(0, -1).map(_.toString).isEmpty)
            assert(table.getCell(-1, 0).map(_.toString).isEmpty)
            assert(table.getCell(9, 8).map(_.toString).isEmpty)
            assert(table.getCell(8, 9).map(_.toString).isEmpty)
        }
        'test_numberCell - {
            val table = Table(2, 2)
            val cellInt00 = NumberCell(5)
            val cellInt11 = NumberCell(2147483647)
            table.setCell(0, 0, cellInt00)
            table.setCell(1, 1, cellInt11)
            assert(table.getCell(0, 0).map(_.toString).contains("5"))
            assert(table.getCell(0, 1).map(_.toString).contains("empty"))
            assert(table.getCell(1, 0).map(_.toString).contains("empty"))
            assert(table.getCell(1, 1).map(_.toString).contains("2147483647"))
        }
        'test_stringCell - {
            val table = Table(2, 2)
            val cellStr01 = StringCell("01")
            val cellStr10 = StringCell("10")
            table.setCell(0, 1, cellStr01)
            table.setCell(1, 0, cellStr10)
            assert(table.getCell(0, 0).map(_.toString).contains("empty"))
            assert(table.getCell(0, 1).map(_.toString).contains("01"))
            assert(table.getCell(1, 0).map(_.toString).contains("10"))
            assert(table.getCell(1, 1).map(_.toString).contains("empty"))
        }
        'test_referenceCell - {
            val table = Table(3, 3)
            /*ix = 0*/
            val cellStr00 = StringCell("00")
            val cellRef01 = ReferenceCell(0, 2, table)
            val cellRef02 = ReferenceCell(0, 1, table)
            /*ix = 1*/
            val cellInt10 = NumberCell(10)
            val cellInt11 = NumberCell(11)
            val cellRef12 = ReferenceCell(0, 0, table)
            /*ix = 2*/
            val cellEmpty20 = new EmptyCell
            val cellRef21 = ReferenceCell(1, 1, table)
            val cellRef22 = ReferenceCell(2, 1, table)
            table.setCell(0, 0, cellStr00)
            table.setCell(0, 1, cellRef01)
            table.setCell(0, 2, cellRef02)
            table.setCell(1, 0, cellInt10)
            table.setCell(1, 1, cellInt11)
            table.setCell(1, 2, cellRef12)
            table.setCell(2, 0, cellEmpty20)
            table.setCell(2, 1, cellRef21)
            table.setCell(2, 2, cellRef22)
            for (i <- 0 until 9) {
                val value = table.getCell(i / 3, i % 3).get.toString
                i match {
                    case 0 => assert(value == "00")
                    case 1 => assert(value == "cyclic")
                    case 2 => assert(value == "cyclic")
                    case 3 => assert(value == "10")
                    case 4 => assert(value == "11")
                    case 5 => assert(value == "00")
                    case 6 => assert(value == "empty")
                    case 7 => assert(value == "11")
                    case 8 => assert(value == "11")
                    case _ => assert(false)
                }
            }
        }
    }
}
