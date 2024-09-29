import scala.collection.mutable

case class Table(width: Int, length: Int) {

  private val cells: mutable.Seq[Cell] =
    collection.mutable.Seq.fill(width * length)(EmptyCell())

  def getCell(ix: Int, iy: Int): Option[Cell] =
    if(!(ix < 0 || ix >= length || iy < 0 || iy >= width))
      Some(cells(ix + iy * width))
     else None

  def setCell(ix: Int, iy: Int, cell: Cell): Unit =
    if(!(ix < 0 || ix >= length || iy < 0 || iy >= width)) cells(ix + iy * width) = cell



}