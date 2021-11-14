import java.io.{File, FileWriter}
import java.nio.file.{Files, Paths}
import scala.:+
import scala.collection.immutable.ListMap
import scala.collection.mutable.ListBuffer
import scala.io.Source
import scala.io.StdIn.readLine
import scala.math.log10

class WordCloudGenerator {

  private var WordCountPairs:Seq[(String,Int)]=Seq.empty
  private var WordCountPairsSeparated:ListBuffer[Tuple3[String,Seq[(String,Int)],Int]]=ListBuffer.empty


  val ListOfBooks=List("The_Last_Dutchess_Of_Belgrade.txt", "The_Kingmakers.txt")
  def ReadFromFile(filename:String): Unit = {
    var fileContents=""
    if (filename=="")
      {
         // fileContents = Source.fromFile("The_Last_Dutchess_Of_Belgrade.txt").mkString.toLowerCase
      }
    else
      {
        if (Files.exists(Paths.get(filename)))
        fileContents = Source.fromFile(filename).mkString.toLowerCase
        else
          println("File doesn't exist")
      }
    Process(fileContents,filename)
  }

  def ReadFromConsole(): Unit = {
    val text =readLine()
    Process(text,"ConsoleInput")
  }

  def SaveToCSV(filename:String):Unit={
    WordCountPairs=WordCountPairs.sortWith(_._2 > _._2)
    val fileWriter = new FileWriter(new File("csv/"+filename+".csv"))
    for(pair<-WordCountPairs.take(20)) {
      fileWriter.write(pair._2+","+pair._1+"\n")
    }
    fileWriter.close()
  }

  def PrintToConsole():Unit= {
    WordCountPairs=WordCountPairs.sortWith(_._2 > _._2)
    for (pair <- WordCountPairs.take(20)) {
      println(pair._1 + "," + pair._2 )
    }
  }

  def PrintToConsoleSeparated(n:Int):Unit= {
    for (document<-WordCountPairsSeparated) {
      println("Most frequent for:"+document._1+" with total N.o. words="+document._3)
      for (pair <- document._2.take(n)) {
        println(pair._1 + "," + pair._2 )
      }
    }
  }

  private def  Process(str:String, filename:String) : Unit={
    var fileContentsRemovedPunctuation=str.replaceAll("""[\p{Punct}]|“|”""","")
    val allWordsArray=fileContentsRemovedPunctuation.split("\\s+")
    val stopWordsArray = Source.fromFile("stop_words_english.txt").getLines().toArray;
    var allWordsList= allWordsArray.toBuffer
    var NotStopWords=allWordsList.filter(!stopWordsArray.contains(_))
    var counters=NotStopWords.groupBy(l => l).map(t => (t._1, t._2.length))
    var sortedCounters=counters.toSeq.sortWith(_._2 > _._2)
    WordCountPairs=WordCountPairs++sortedCounters
    WordCountPairs=WordCountPairs.groupBy(l => l._1).map(t => (t._1, t._2.foldLeft(0)(_+_._2))).toSeq
    WordCountPairsSeparated=WordCountPairsSeparated+=(Tuple3(filename,sortedCounters,sortedCounters(0)._2))
  }

  def tfIdf(document:Tuple3[String,Seq[(String,Int)],Int]):List[(String,Double)]={
    var wordsWithTfIdf=List[(String,Double)]()
    var words=document._2
    for (word<-words)
      {
        var cnt=document._3
        var tf=word._2.toDouble/cnt
        var wordAppearsInCount=WordCountPairsSeparated.count(x=>x._2.toMap.keys.count(_==word._1)>0)
        var log2 = (x: Double) => log10(x)/log10(2.0)
        var idf=log2(ListOfBooks.length/wordAppearsInCount)
        var tfidf=tf*idf
        wordsWithTfIdf=wordsWithTfIdf:+(word._1,tfidf)
      }
      return wordsWithTfIdf
  }

  def tfIdfAllDocuments(n:Int): Unit =
  {
    var result=ListMap[String,Map[String,Double]]()
    for (document<-WordCountPairsSeparated)
      {
        var singleResult=tfIdf(document).sortWith(_._2>_._2)
        result=result+(document._1->ListMap(singleResult.take(n):_*))
      }
      //Printing results
      for (res<-result) {
        println("DocumentID:"+res._1+" top "+n+" TFIDF scores:")
        for (word<-res._2)
          println(word._1+", "+(math rint word._2 * 1000) / 1000)
      }

  }
}
