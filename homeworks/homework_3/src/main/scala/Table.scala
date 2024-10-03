class Table(val width: Int, val height: Int) {
  private var items: Array[Cell] = new Array[Cell](height * width)

  def setCell(ix: Int, iy: Int, cell: Cell): Unit = {
    if (!inBounds(ix, iy)) {
      return
    }

    items(evaluateFlattenIndex(ix, iy)) = cell
  }

  def getCell(ix: Int, iy: Int): Option[Cell] = {
    if (!inBounds(ix, iy)) {
      return None
    }

    Some(getCellInner(ix, iy))
  }

  private def getCellInner(ix: Int, iy: Int): Cell = {
    val index = evaluateFlattenIndex(ix, iy)
    var item = items(index)
    if (item == null) {
      item = new EmptyCell()
      items(index) = item
    }
    item
  }

  private def evaluateFlattenIndex(ix: Int, iy: Int) = iy * width + ix

  private def inBounds(ix: Int, iy: Int): Boolean = ix >= 0 && ix < width && iy >= 0 && iy < height
}