import scala.collection.mutable

trait Cell

class NumberCell(number: Int) extends Cell {
  override def toString: String = number.toString
}

class EmptyCell extends Cell {
  override def toString: String = "empty"
}

class ReferenceCell(ix: Int, iy: Int, table: Table) extends Cell {
  val path: mutable.HashSet[Cell] = mutable.HashSet[Cell]()

  override def toString: String = {
    val cell = table.getCell(ix, iy)
    if (cell.isEmpty) {
      "outOfRange"
    }
    else if (cell.get.isInstanceOf[ReferenceCell]) {
      if (path.contains(cell.get)) {
        "cyclic"
      }
      else {
        path.add(cell.get)
        cell.get.toString
      }
    }
    else {
      cell.get.toString
    }
  }
}

class StringCell(str: String) extends Cell {
  override def toString: String = str
}