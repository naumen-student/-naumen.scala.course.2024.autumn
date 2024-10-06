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

class ReferenceCell(val ix: Int, val iy: Int, val table: Table) extends Cell {

  override def toString: String = {
    table.getCell(ix, iy) match {
      case Some(cell: ReferenceCell) =>
        table.getCell(cell.ix, cell.iy) match {
          case Some(nextCell) =>
            if (this != nextCell) nextCell.toString
            else "cyclic"
          case None => "outOfRange"
        }

      case Some(cell) => cell.toString
      case None => "outOfRange"
    }
  }
}
