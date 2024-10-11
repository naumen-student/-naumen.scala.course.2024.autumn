class Table(width: Int, height: Int) {

  private val cellsArr: Array[Any] = Array.fill[Cell](width * height)(new EmptyCell)

  private def inRange(ix: Int, iy: Int): Option[Boolean] = Some(ix > -1 && ix < width && iy > -1 && iy < height).filter(identity => identity)

  private def cellIdx(ix: Int, iy: Int): Int = ix + iy * width

  def getCell(ix: Int, iy: Int): Option[Cell] =
    inRange(ix, iy).map(_ => cellsArr(cellIdx(ix, iy)))

  def setCell(ix: Int, iy: Int, cell: Cell): Unit =
    inRange(ix, iy).foreach(_ => cellsArr(cellIdx(ix, iy)) = cell)

}