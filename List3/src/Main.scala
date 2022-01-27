
object Main {
  def main(args: Array[String]): Unit = {
    var books=Array("The_Last_Dutchess_Of_Belgrade.txt","The_Kingmakers.txt","Macbeth.txt")
    var footballBooks=Array("FOOTBALL_Behind_the_Line.txt","FOOTBALL_Football_Days_Memories.txt","FOOTBALL_Scottish_Football.txt")
    var s = new Shingles();
    println(footballBooks(0)+" vs "+footballBooks(2))
    val ns=Array(10,100,250,500)
    for(n<-ns) {
      for (k <- 4 to 13) {
        val filecontents = s.TextToArray(s.ReadFromFile(footballBooks(0)))
        val filecontents2 = s.TextToArray(s.ReadFromFile(footballBooks(2)))
        val shingles = s.TextArrayToShingles(filecontents, k)
        val shingles2 = s.TextArrayToShingles(filecontents2, k)
        print(k+", "+n+", "+s.JaccardFull(shingles, shingles2)+", ")
        val hashfunctions = s.GenerateHashFunctions(shingles.length, n)
        val signatures = s.Signatures(Array(shingles.toSet, shingles2.toSet), hashfunctions)
        println(s.JaccardSignatures(signatures(0), signatures(1)))
      }
    }
  }
}
