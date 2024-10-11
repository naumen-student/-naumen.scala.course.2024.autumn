trait Cell;

case class EmptyCell() extends Cell {
  override def toString: String = "empty"
}

case class NumberCell(private var value: Int) extends Cell {
  override def toString: String = value.toString
}

case class StringCell(private var value: String) extends Cell {
  override def toString: String = value
}

case class ReferenceCell(ix: Int, iy: Int, table: Table) extends Cell {
  override def toString: String = toStringInternal()

  private def toStringInternal(visitedCells: Set[Cell] = Set.empty): String = table.getCell(ix, iy).map{
    case referenceCell: ReferenceCell =>
      if (!visitedCells.contains(this)){
        referenceCell.toStringInternal(visitedCells ++ Set(this))
      } else "cyclic"
    case cell: Cell => cell.toString
  }.getOrElse("outOfRange")
}