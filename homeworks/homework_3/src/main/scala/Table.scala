class Table(width: Int, height: Int) {

  private val cells = Array.fill[Cell](width * height)(new EmptyCell)

  def getCell(ix: Int, iy: Int): Option[Cell] = if (ix > -1 && ix < width && iy > -1 && iy < height) {
    Some(cells(ix + iy * width))
  } else None

  def setCell(ix: Int, iy: Int, cell: Cell): Unit = if (ix > -1 && ix < width && iy > -1 && iy < height) {
    cells(ix + iy * width) = cell
  }
}