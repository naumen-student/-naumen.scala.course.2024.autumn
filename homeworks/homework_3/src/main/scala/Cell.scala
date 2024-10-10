import scala.annotation.tailrec

trait Cell;

class EmptyCell extends Cell {
    override def toString: String = "empty"
}

class NumberCell(val number: Int) extends Cell {
    override def toString: String = number.toString
}

class StringCell(val text: String) extends Cell {
    override def toString: String = text
}

class ReferenceCell(val ix: Int, val iy: Int, val table: Table) extends Cell {
    override def toString: String = {
        toString(Set.empty)
    }

    @tailrec
    private def toString(visited: Set[(Int, Int)]): String = {
        if (visited.contains((ix, iy))) {
            "cyclic"
        } else {
            table.getCell(ix, iy) match {
                case Some(cell: ReferenceCell) => cell.toString(visited + ((ix, iy)))
                case Some(cell) => cell.toString
                case None => "outOfRange"
            }
        }
    }
}