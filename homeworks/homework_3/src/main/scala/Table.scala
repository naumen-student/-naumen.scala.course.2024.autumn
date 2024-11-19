class Table(val width: Int, val height: Int) {

  private val cells = Array.fill[Cell](width * height)(new EmptyCell)

  private def toIndex(ix: Int, iy: Int): Int = ix + iy * width

  private def isWithinBounds(ix: Int, iy: Int): Boolean = ix >= 0 && ix < width && iy >= 0 && iy < height

  def getCell(ix: Int, iy: Int): Option[Cell] = {
    if (isWithinBounds(ix, iy)) Some(cells(toIndex(ix, iy))) else None
  }

  def setCell(ix: Int, iy: Int, cell: Cell): Unit = {
    if (isWithinBounds(ix, iy)) {
      cells(toIndex(ix, iy)) = cell
    }
  }
}