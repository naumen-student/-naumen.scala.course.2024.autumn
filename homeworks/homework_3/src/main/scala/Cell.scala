import scala.collection.mutable

trait Cell {

}

class EmptyCell extends Cell {
  override def toString: String = {
    "empty"
  }
}

class NumberCell(number: Int) extends Cell {
  override def toString: String = this.number.toString
}

class StringCell(string: String) extends Cell {
  override def toString: String = string
}

class ReferenceCell(val ix: Int, val iy: Int, val table: Table) extends Cell {
  override def toString: String = {
    var currentCellOption = table.getCell(ix, iy)

    val path = mutable.HashSet[Cell]()
    while (currentCellOption.isDefined && currentCellOption.get.isInstanceOf[ReferenceCell]) {
      val currentCell = currentCellOption.get
      if (!path.add(currentCell)) return "cyclic"

      val cell = currentCell.asInstanceOf[ReferenceCell]
      currentCellOption = cell.table.getCell(cell.ix, cell.iy)
    }

    currentCellOption.getOrElse("outOfRange").toString
  }
}