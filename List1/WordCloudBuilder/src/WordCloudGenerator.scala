import java.io.{File, FileWriter}
import java.nio.file.{Files, Paths}
import scala.io.Source
import scala.io.StdIn.readLine

class WordCloudGenerator {

  private var WordCountPairs:Seq[(String,Int)]=Seq.empty

  def ReadFromFile(filename:String): Unit = {
    var fileContents=""
    if (filename=="")
      {
         fileContents = Source.fromFile("The_Last_Dutchess_Of_Belgrade.txt").mkString.toLowerCase
      }
    else
      {
        if (Files.exists(Paths.get(filename)))
        fileContents = Source.fromFile(filename).mkString.toLowerCase
        else
          println("File doesn't exist")
      }
    ProcessToWordCountPairs(fileContents)
  }

  def ReadFromConsole(): Unit = {
    val text =readLine()
    ProcessToWordCountPairs(text)
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
      println(pair._1 + "," + pair._2 + "\n")
    }
  }

  private def  ProcessToWordCountPairs(str:String) : Unit={
    var fileContentsRemovedPunctuation=str.replaceAll("""[\p{Punct}]""", "")
    val allWordsArray=fileContentsRemovedPunctuation.split("\\s+")
    val stopWordsArray = Source.fromFile("stop_words_english.txt").getLines().toArray;
    var allWordsList= allWordsArray.toBuffer
    var NotStopWords=allWordsList.filter(!stopWordsArray.contains(_))
    var counters=NotStopWords.groupBy(l => l).map(t => (t._1, t._2.length))
    var sortedCounters=counters.toSeq.sortWith(_._2 > _._2)
    WordCountPairs=WordCountPairs++sortedCounters
  }
}
