trait Cell {
    def toString: String
}

class EmptyCell extends Cell {
    override def toString: String = "empty"
}

class NumberCell(number: Int) extends Cell {
    override def toString: String = number.toString
}

class StringCell(text: String) extends Cell {
    override def toString: String = text
}

class ReferenceCell(ix: Int, iy: Int, table: Table) extends Cell {
    override def toString: String = {
    table.getCell(ix, iy) match {
        case Some(cell) =>
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
