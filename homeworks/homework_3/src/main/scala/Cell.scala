sealed trait Cell {
  def toString: String
}

case class EmptyCell() extends Cell {
  override def toString: String = "empty"
}

case class NumberCell(number: Int) extends Cell {
  override def toString: String = number.toString
}

case class StringCell(text: String) extends Cell {
  override def toString: String = text
}

case class ReferenceCell(ix: Int, iy: Int, table: Table) extends Cell {
  private def referencedCell: Option[Cell] = table.getCell(ix, iy)

  override def toString: String = {
    referencedCell match {
      case Some(refCell: ReferenceCell) =>
        refCell.referencedCell match {
          case Some(nextRefCell: ReferenceCell) if nextRefCell.ix == ix && nextRefCell.iy == iy => "cyclic"
          case Some(nextCell) => nextCell.toString
          case None => "outOfRange"
        }
      case Some(cell) => cell.toString
      case None => "outOfRange"
    }
  }
}