trait Cell {

}

class EmptyCell extends Cell {
  override def toString: String = "empty"
}

class NumberCell(num: Int) extends Cell {
  override def toString: String = num.toString
}

class StringCell(str: String) extends Cell {
  override def toString: String = str
}

class ReferenceCell(ix: Int, iy: Int, table: Table) extends Cell {
  private def ref: Option[Cell] = table.getCell(ix, iy)

  override def toString: String = ref.map {
    case refCell: ReferenceCell => if (refCell.ref == Option(this)) "cyclic" else refCell.toString;
    case cell: Cell => cell.toString
  }.getOrElse("outOfRange")
}
