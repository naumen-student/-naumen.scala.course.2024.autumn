import scala.collection.mutable
import scala.util.Try

case class Table(width: Int, height: Int) {
  private val cells: mutable.ArrayBuffer[Cell] = mutable.ArrayBuffer.fill(width * height)(EmptyCell())

  def getCell(ix: Int, iy: Int): Option[Cell] =
    Try(cells(ix + iy * width)).toOption

  def setCell(ix: Int, iy: Int, cell: Cell): Unit =
    Try(cells(ix + iy * width) = cell).toOption
}