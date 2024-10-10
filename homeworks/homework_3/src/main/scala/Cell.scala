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

class ReferenceCell(cellX: Int, cellY: Int, table: Table) extends Cell {
  private def next: Option[Cell] = table.getCell(cellX, cellY)
  override def toString: String = {
    this.next match {
      case None => "outOfRange"
      case Some(cell) =>
        cell match
          case cell1: ReferenceCell if cell1.next.contains(this) => "cyclic"
          case _ => cell.toString
    }
  }
}