import scala.io.Source
import scala.io.StdIn.readLine
import java.nio.file.{Paths, Files}

object Main {
  def main(args: Array[String]): Unit = {
    var generator= new WordCloudGenerator()
    var option=""
    while (option!="q")
      {
        println("Select option (rf-read from file, rc-read string from console, cout-print to console fout-save to file, q-quit," +
          "\n scout-print separated to console, r-read all the files hardcoded, tfidf-tfidf analysis on all documents)")
        option=readLine()
        option match {
          case "q" => //do nothing and end
            println("Quit!")
          case "rf"=>println("Enter name of the file")
            var filename=readLine()
            generator.ReadFromFile(filename)
          case "r"=>
            for (file<-generator.ListOfBooks)
              {
                generator.ReadFromFile(file)
              }
          case "rc"=>
            var str=readLine()
            generator.ReadFromConsole()
          case "fout"=> println("Enter name of the file")
            var name=readLine()
            generator.SaveToCSV(name)
            println("saved to: "+name+".csv")
          case "cout"=> generator.PrintToConsole()
          case "scout"=>generator.PrintToConsoleSeparated(20)
          case "tfidf"=>generator.tfIdfAllDocuments(20)
          case _ => println("Unrecognised option" )
        }
      }
  }


}
