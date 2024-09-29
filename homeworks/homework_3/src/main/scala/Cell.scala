import scala.annotation.tailrec

sealed trait Cell

case class NumberCell(number: Int) extends Cell {
  override def toString: String = number.toString
}
case class StringCell(string: String) extends Cell {
  override def toString: String = string
}
case class ReferenceCell(x: Int, y: Int, table: Table) extends Cell {
  override def toString: String = getRefValue()

  @tailrec
  private def getRefValue(acc: Seq[ReferenceCell] = Seq()): String = {
    table.getCell(x, y) match {
      case Some(cell: ReferenceCell) =>
        if (acc.contains(cell)) "cyclic"
        else cell.getRefValue(acc :+ cell)
      case Some(value) => value.toString
      case None => "outOfRange"
    }
  }
}


case class EmptyCell() extends Cell {
  override def toString: String = "empty"
}