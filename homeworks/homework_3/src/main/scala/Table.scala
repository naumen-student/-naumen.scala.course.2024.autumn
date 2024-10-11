case class Table(height: Int, width: Int) {
  private val cells = Array.fill[Cell](height * width)(new EmptyCell)

  private def indexInRange(ix: Int, iy: Int): Option[Boolean] = Some(ix >= 0 && ix < width && iy >= 0 && iy < height).filter(x => x)

  private def getIndex(ix: Int, iy: Int): Int = ix + iy * height

  def getCell(ix: Int, iy: Int): Option[Cell] = indexInRange(ix, iy).map(_ => cells(getIndex(ix, iy)))

  def setCell(ix: Int, iy: Int, value: Cell): Unit = indexInRange(ix, iy).foreach(_ => cells(getIndex(ix, iy)) = value)
}
