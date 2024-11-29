import scala.annotation.tailrec

trait Cell {
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

  @tailrec
  private def resolveCycle(visited: Set[Cell] = Set.empty): String = {
    table.getCell(ix, iy) match {
      case Some(ref: ReferenceCell) =>
        if (visited.contains(ref)) {
          "cyclic"
        } else {
          ref.resolveCycle(visited + ref)
        }
      case Some(cell) => cell.toString
      case None => "outOfRange"
    }
  }

  override def toString: String = resolveCycle()
}