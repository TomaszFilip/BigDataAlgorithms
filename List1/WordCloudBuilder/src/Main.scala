import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
    var generator= new WordCloudGenerator()

    generator.ReadFromFile("")
    generator.ReadFromConsole()
    generator.SaveToCSV("test")
    generator.PrintToConsole()
  }


}
