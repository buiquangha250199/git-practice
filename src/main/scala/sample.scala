import java.util.Scanner
import scala.collection.mutable.ListBuffer
import scala.io.Source
import scala.util.Random
object WordChain {
  def main(args: Array[String]): Unit = {
    val dic = loadFile
    val listChar = List("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z")
    val usedUserWords = new ListBuffer[String]
    val usedComputerWords = new ListBuffer[String]
    val usedWords = new ListBuffer[String]
    isUser match {
      case 0 =>
        val randomChar = dic (Random.nextInt (dic.length) )
        println (randomChar)
        addToUsedList (randomChar, usedWords)
        addToUsedList (randomChar, usedComputerWords)
        start ()
      case _ =>
        val ranChar = randomChar (listChar)
        println (ranChar)
        addToUsedList (ranChar, usedWords)
        addToUsedList (ranChar, usedComputerWords)
        start()
    }
    def start(): Unit = {
      val input = inputWord
      val lastWord = usedWords.last
      val search = searchWord(input.last.toString, dic)
      val message = validateInput (input)
      println(message)
      if (message.contains("non-alphabets") || message.contains("start with") || message.contains("dictionary")) {
        start()
      }
      if (message.equals("")) {
        val random = randomWord(search)
        comAnswer(random)
        addToUsedList(input, usedUserWords)
        addToUsedList(random, usedComputerWords)
        addToUsedList(input, usedWords)
        addToUsedList(random, usedWords)
        start()
      }
    }

    def validateInput (input: String) : String = {
      val lastWord = usedWords.last
      val message = input match {
        case input if (usedWords.length == dic.length) => "you win. " + dic.length + " words have used in this game."
        case input if (!input.startsWith(lastWord.last.toString)) =>"word must start with " + lastWord.last.toString
        case input if (!isNonChar(input)) => input + " contains non-alphabets character"
        case input if (isUsed(input, usedUserWords)) => "you lose. The word that you inputted has used by you at turn " +
          (usedUserWords.indexOf(input) + 1) + ". " +
          usedWords.length + " words have used in this game."
        case input if (isUsed(input, usedComputerWords)) =>"you lose. The word that you inputted has used by me at turn " +
          (usedComputerWords.indexOf(input) + 1) + ". " +
          usedWords.length + " words have used in this game."
        case input if (!isExistInDic(input, dic)) =>input + " is not in the dictionary"
        case _ => ""
      }
      message
    }

  }
  def inputWord : String = {
    val scanner = new Scanner(System.in)
    println("Enter word: ")
    val word = scanner.next()
    word
  }
  def searchWord(letter: String, dic: List[String]): List[String] = {
    var listWordSearched=ListBuffer[String]()
    for(word <- dic){
      if(word.startsWith(letter)){
        listWordSearched +=word
      }
    }
    listWordSearched.toList
  }
  def randomWord(listWords: List[String]): String = {
    listWords(Random.nextInt(listWords.length))
  }
  def comAnswer(word: String): Unit ={
    println(word)
  }
  def randomChar(listChar: List[String]): String = {
    listChar(Random.nextInt(listChar.length))
  }
  def isUser :Int= {
    Random.nextInt(2)
  }
  def isNonChar(word: String) :Boolean = {
    word.forall(_.isLetter)
  }
  def isUsed(word: String, usedList: ListBuffer[String]): Boolean = {
    usedList.contains(word)
  }
  def isExistInDic(word: String, dic: List[String]): Boolean = {
    dic.contains(word)
  }
  def addToUsedList(word: String, usedList: ListBuffer[String]): Unit = {
    usedList += word
  }
  def loadFile: List[String] = {
    var dic = new ListBuffer[String]
    val fileName = "words_alpha.txt"
    val source = Source.fromFile(fileName)
    for(line <- source.getLines()){
      dic +=line
    }
    dic.toList
  }
}