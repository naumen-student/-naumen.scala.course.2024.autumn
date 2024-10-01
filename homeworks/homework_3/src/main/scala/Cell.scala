import scala.annotation.tailrec

trait Cell {
    def toString : String
}

class EmptyCell extends Cell {
    override def toString: String = "empty"
}

class NumberCell(number: Int) extends Cell {
    override def toString: String = number.toString
}

class StringCell(string: String) extends Cell {
    override def toString: String = string
}

class ReferenceCell(ix: Int, iy: Int, table: Table) extends Cell {
    val x: Int = ix
    val y: Int = iy

    override def toString(): String = {
        if (x < 0 || x >= table.w || y < 0 || y >= table.h) {
            "outOfRange"
        } else {
            val visitedCells = collection.mutable.Set[Cell]()

            @tailrec
            def checkCyclicReference(nextCell: ReferenceCell): Option[Boolean] = {
                if (visitedCells.contains(nextCell)) {
                    Some(true)
                } else {
                    visitedCells.add(nextCell)
                    table.getCell(nextCell.x, nextCell.y) match {
                        case Some(cell: ReferenceCell) => checkCyclicReference(cell)
                        case None => None
                        case _ => Some(false)
                    }
                }
            }

            @tailrec
            def getCellRecursive(cell: ReferenceCell) : String = {
                table.getCell(cell.x, cell.y) match {
                    case Some(cell: ReferenceCell) => getCellRecursive(cell)
                    case Some(cell) => cell.toString
                    case None => "outOfRange"
                }
            }

            var isCyclic = checkCyclicReference(this)
            if (isCyclic == None) {
                "outOfRange"
            } else if (isCyclic == Some(true)) {
                "cyclic"
            } else {
                getCellRecursive(this)
            }
        }
    }
}
