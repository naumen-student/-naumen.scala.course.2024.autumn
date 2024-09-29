import scala.collection.mutable

class Table(val width: Int, val height: Int) {
  private val cells: mutable.ArrayBuffer[Cell] = mutable.ArrayBuffer.fill(width * height)(new EmptyCell())

  private def index(ix: Int, iy: Int): Int = ix + iy * width

  def getCell(ix: Int, iy: Int): Option[Cell] = {
    if (ix >= 0 && ix < width && iy >= 0 && iy < height) {
      Some(cells(index(ix, iy)))
    } else {
      None
    }
  }

  def setCell(ix: Int, iy: Int, cell: Cell): Unit = {
    if (ix >= 0 && ix < width && iy >= 0 && iy < height) {
      cells(index(ix, iy)) = cell
    }
  }
}