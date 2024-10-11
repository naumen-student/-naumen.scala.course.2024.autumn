trait Cell

class EmptyCell extends Cell {

  override def toString: String = "empty"
}

class StringCell(str: String) extends Cell {

  override def toString: String = str
}

class NumberCell(num: Int) extends Cell {

  override def toString: String = num.toString
}

class ReferenceCell(ix: Int, iy: Int, table: Table) extends Cell {



  override def toString: String = toStringImpl()

  private def toStringImpl(cellHistory: Set[ReferenceCell] = Set.empty): String = {
    table.getCell(ix, iy).map{
      case referenceCell: ReferenceCell =>
        if (!cellHistory, contains(this)) referenceCell.toStringImpl(cellHistory ++ Set(this))
        else "cyclic"
      case cell: Cell => cell.toString
    }.
    }getOrElse("outOfRange")
  }
}


























