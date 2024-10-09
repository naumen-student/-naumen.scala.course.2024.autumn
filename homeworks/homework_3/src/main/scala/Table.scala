class Table(width:Int,height:Int) {
  val arr = Array.fill[Cell](height * width)(new EmptyCell)
  def getCell (ix:Int,iy:Int):Option[Cell]={
    if(ix>=0&&ix<width&&iy>=0&&iy<height)
        Some(arr(ix+iy*width))
    else
        None
  }
   def setCell(ix:Int,iy:Int,cell:Cell):Boolean={
      if(ix>=0&&ix<width&&iy>=0&&iy<height){
         arr(ix+iy*width)=cell
         true
      }
      else
         false
   }
}
