import scala.collection.mutable

sealed trait Cell {
  def toString: String
}

class EmptyCell extends Cell {
  override def toString: String = "empty"
}

class NumberCell(val number: Int) extends Cell {
  override def toString: String = number.toString
}

class StringCell(val text: String) extends Cell {
  override def toString: String = text
}

class ReferenceCell(ix: Int, iy: Int, table: Table) extends Cell {
  override def toString: String = resolveReference()

  private def resolveReference(visited: Set[(Int, Int)] = Set.empty): String = {
    if (visited.contains((ix, iy))) {
      "cyclic"
    } else {
      val newVisited = visited + ((ix, iy))
      table.getCell(ix, iy) match {
        case Some(refCell: ReferenceCell) =>
          refCell.resolveReference(newVisited)
        case Some(cell) =>
          cell.toString
        case None =>
          "outOfRange"
      }
    }
  }
}