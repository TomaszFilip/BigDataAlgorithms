import java.nio.file.{Files, Paths}
import scala.collection.mutable.ListBuffer
import scala.io.Source
import scala.util.Random

class Shingles {
  def ReadFromFile(filename:String): String = {
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
    return fileContents

  }

    def TextToArray(str:String): Array[String] ={
    val textArray=str.split("")
    return textArray
  }

   def TextArrayToShingles(arr:Array[String],k:Int):Array[String]={
    var index=0
    var kshingles=ListBuffer[String]()
    while(index<arr.length-k+1)
      {
        var shingle="".toString()
        for(kk<-0 to k-1) {
          shingle = shingle + arr(index + kk)
        }
        kshingles.append(shingle)
        index+=1
      }
      return kshingles.groupBy(l => l).map(t=>(t._1)).toArray
  }

   def Intersection(set1:Array[String], set2:Array[String]):Int={
    var intersectionCount=0
    for(item<-set1)
      {
        if (set2.contains(item))
          {
            intersectionCount+=1
          }
      }
      return intersectionCount
  }

   def JaccardFull(bookAShingles:Array[String],bookBshingles:Array[String]): Float =
  {
    val intersectionN=List(bookAShingles,bookBshingles).reduce((a, b) => a intersect b).length
    val unionN=bookAShingles.length+bookBshingles.length-intersectionN
    val sim = intersectionN.toFloat/unionN
    return sim
  }

  def GenerateHashArrays(k:Int,n:Int): Array[Array[Int]] =
  {
    var funarray=Array.ofDim[Int](n,k)
    for(nn<-0 to n-1) {
      val kArray = (0 to k - 1).toArray
      val randomKArray = Random.shuffle(kArray).toArray
      funarray(nn)=randomKArray
    }
    return  funarray
  }

  def GenerateHashFunctions(k:Int,n:Int): Array[Int=>Int] =
  {
    var ret=Array.ofDim[Int=>Int](n)
    val hashArrays=GenerateHashArrays(k,n);
    for(nn<-0 to n-1) {
      val f: (Int) => Int = (hashArrays(nn))(_)
      ret(nn)=f
    }
    ret
  }

  def Signatures(shingleSets:Array[Set[String]],hashFunctions:Array[(Int)=>Int])
  : Array[Array[Int]]= {
    val unionn=shingleSets.reduce((a, b) => a union b).toList
    var ret=Array.ofDim[Int](shingleSets.length,hashFunctions.length)
    var index=0
    for (shingleSet <- shingleSets)
      {
        var shingleSetArr=shingleSet.toArray
        var index2=0
        var signature=Array.ofDim[Int](hashFunctions.length)
        for(hashFunction<-hashFunctions)
          {

            var firstOne=unionn.length
            var end=false
            var index3=0
            while(end!=true)
              {
                var deb=unionn(hashFunction(index3))
                if(shingleSetArr.contains(unionn(hashFunction(index3))))
                  {
                    firstOne=index3;

                    end=true
                  }
                index3+=1
              }
              signature(index2)=firstOne
              index2+=1
          }
        ret(index)=signature
        index+=1
      }
      ret
    }

  def JaccardSignatures(SignatureA:Array[Int], SignatureB:Array[Int]):Float={
    var sameCount=0
    for (i<-0 to SignatureA.length-1 ){
      if(SignatureA(i)==SignatureB(i))
        {
          sameCount+=1
        }
      }
    return sameCount.toFloat/SignatureA.length
    }

}





