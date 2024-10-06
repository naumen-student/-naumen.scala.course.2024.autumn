class Table(val width: Int, val height: Int) {
  private val cells = Array.fill[Cell](width * height)(new EmptyCell)

  def getCell(ix: Int, iy: Int): Option[Cell] = {
    if (!inRange(ix, iy)) {
      None
    }
    else {
      Option(cells(getIndex(ix, iy)))
    }
  }

  def setCell(ix: Int, iy: Int, cell: Cell): Unit = {
    if (inRange(ix, iy)) {
      cells(getIndex(ix, iy)) = cell
    }
  }

  private def inRange(ix: Int, iy: Int):Boolean = {
    if ((ix < width || iy < height) && ix >= 0 && iy >= 0)
      true
    else
      false
  }

  private def getIndex(ix: Int, iy: Int):Int = {
    ix + width * iy
  }
}