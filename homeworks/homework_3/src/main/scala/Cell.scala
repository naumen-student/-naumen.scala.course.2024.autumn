trait Cell{
  def toString:String
}

class EmptyCell extends Cell{
    override def toString:String = "empty"
}

case class NumberCell(value:Int) extends Cell{
    override def toString: String = value.toString
}

case class StringCell(value:String)extends Cell{
    override def toString: String = value
}

case class ReferenceCell(ix:Int,iy:Int,table:Table) extends Cell{
    override def toString: String= {
        var cur :Option[Cell]= Some(this)
        val refs = collection.mutable.Set[ReferenceCell]()
       while(!cur.isEmpty&&cur.get.isInstanceOf[ReferenceCell]&& !refs.contains((cur).get.asInstanceOf[ReferenceCell])){
         val rCur = cur.get.asInstanceOf[ReferenceCell]
         refs += rCur
         cur = table.getCell(rCur.ix, rCur.iy)
       }
      cur match {
        case None=>"outOfRange"
        case _ =>cur.get match {
            case ReferenceCell(_, _, _)=>"cyclic"
            case other =>other.toString
        }
      }
    }
}


