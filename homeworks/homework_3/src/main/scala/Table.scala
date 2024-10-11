class Table(width: Int, height: Int) {
    private val cells : Array[Cell] = Array.fill(width * height)(new EmptyCell)

    def getCell(ix: Int, iy: Int) = {
        if (ix >= 0 && ix < width && iy >= 0 && iy < height) Some(cells(ix + iy * width))
        else None
    }

    def setCell(ix: Int, iy: Int, cell: Cell) = {
        if (ix >= 0 && ix < width && iy >= 0 && iy < height) {
            cells(ix + iy * width) = cell
        }
    }
}