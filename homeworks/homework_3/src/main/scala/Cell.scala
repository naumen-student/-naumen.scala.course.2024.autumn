trait Cell {
    override def toString(): String
}

case class EmptyCell() extends Cell {
    override def toString(): String = "empty"
}
case class NumberCell(number: Int) extends Cell {
    override def toString(): String = number.toString()
}
    
case class StringCell(string: String) extends Cell {
    override def toString(): String = string
}
case class ReferenceCell(ix: Int, iy: Int, table: Table) extends Cell {
    private def getNextCell(): Option[Cell] = table.getCell(ix, iy)
    
    override def toString(): String = {
        val lastRefCell = getLastRefCell(getNextCell())
        lastRefCell match {
            case None => "outOfRange"
            case Some(value: ReferenceCell) => "cyclic"
            case _ => lastRefCell.get.toString()
        } 
    }   

    private def getLastRefCell(cell: Option[Cell]): Option[Cell] =
        cell match {
            case Some(value: ReferenceCell) => if (value == this) Option(this) else getLastRefCell(value.getNextCell())
            case _ => cell
        }
}