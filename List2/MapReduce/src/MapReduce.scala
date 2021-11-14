import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class MapReduce {
  var Graph:List[(Int,List[Int])]=List(
    (1,List(2,3)),
    (3,List(1,5)),
    (2,List(5)),
    (5,List())
  )

  def MapAndreduce(): Unit =
  {
    Reduce(myMap())
  }

  def myMap():mutable.Buffer[Map[Int,Int]]={
    var InvertedEdges=mutable.Buffer[Map[Int,Int]]()
    for (edges<-Graph)
      {
        for (edge<-edges._2) {
          var inverted = Map(edge -> edges._1)
          InvertedEdges=InvertedEdges+=inverted
        }
      }
      return InvertedEdges
  }

  def Reduce(map:mutable.Buffer[Map[Int,Int]]) =
  {
    var mapped=myMap()
    var list=ListBuffer[(Int,List[Int])]()
    for (chunk<-mapped)
      {
        var edgeStart=chunk.keys.toList(0)
        if (!list.exists(x=>x._1==edgeStart)) {
        list+=((edgeStart,chunk.values.toList))}
        else list(list.indexWhere(x=>x._1==edgeStart))=(edgeStart,list(list.indexWhere(x=>x._1==edgeStart))._2:+chunk.values.toList(0))
      }
      list.toList
  }
}
