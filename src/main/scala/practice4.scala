import scala.collection.mutable.ListBuffer
import scala.util.Random

object WordChainGame {
  // read file
  def readDictionaryFile(): String = {
    val source = scala.io.Source.fromFile("dictionary.txt")
    val lines = try source.mkString finally source.close()
    lines
  }
  // random character
  def random(START: Int, END: Int): Int = {
    val random_number = new Random()
    START + random_number.nextInt(( END - START) + 1)
  }
  def randomChar: String = {
    val aNumber = 97
    val zNumber = 122
    random( aNumber, zNumber).toChar.toString
  }
  def isUserGoFirst: Int = random(0, 1)
  def userInput: String = {
    println("Please input: ")
    val scanner = new java.util.Scanner(System.in)
    val line = scanner.nextLine().toLowerCase()
    line
  }
  def computerRandomWord(wordListBuffer: ListBuffer[String]): String = {
    wordListBuffer(Random.nextInt(wordListBuffer.length))
  }
  def computerTurn(str: String): Unit = {
    println(str)
  }
  def findWord(string: String, listBuffer: ListBuffer[String]):String = {
    val results = listBuffer.filter(_(0).toString() == string)
    var res = ""
    if(results.length != 0)
      res = results.head
    res
  }
  def existInDictionary(string: String,listBuffer: ListBuffer[String]): Boolean = {
    var exist = false
    if(listBuffer.contains(string))
      exist = true
    exist
  }
  def game() {
    // get all words in file and split to listBuffer
    val wordString = readDictionaryFile().toLowerCase()
    val wordList: List[String] = wordString.split(", ").map(_.trim).toList
    var wordListBuffer = ListBuffer.empty ++= wordList
    // set a map all input: turn -> "[player] [word]" to count turn and amount of words
    val inputtedWord = scala.collection.mutable.Map[Int, String]()
    var turn = 1
    val lengthOfWordListAtStart = wordListBuffer.length
    var wordPresent = ""
    def validLogicGame(str: String): Boolean = {
      str.head.toString.toLowerCase() != wordPresent.last.toString.toLowerCase()
    }
    def userLose(str: String): Boolean = {
      inputtedWord.exists(_._2 == ("user " + str)) || inputtedWord.exists(_._2 == ("computer " + str))
    }
    def checkout(str: String): Unit = {
      var result = scala.collection.mutable.Map[Int, String]()
      if(inputtedWord.exists(_._2 == ("user " + str))) result = inputtedWord.filter(x => x._2 == ("user " + str))
      else result = inputtedWord.filter(x => x._2 == ("user " + str))
      println("Your word was used at turn " + result.keys.head)
    }
    def checkInput(str: String): String = {
      var message = ""
      if(validLogicGame(str)) {
        message = "invalid"
        println("Wrong logic game")
      }
      if(str.forall(!_.isLetter)) {
        message = "non-characters"
        println("Contain non-characters")
      }
      if(!existInDictionary(str, wordListBuffer)) {
        message = "Not exist"
        println("Not exist in dictionary")
      }
      if(userLose(str)) {
        message = "lose"
        checkout(str)
      }
      message
    }
    def gameLoop(): Unit = {
      // user turn
      val userWord = userInput
      val check = checkInput(userWord)
      check match {
        case "invalid" => gameLoop()
        case "non-characters" => gameLoop()
        case "Not exist" => gameLoop()
        case "lose" => println("You lose")
        case _ => println("Computer turn:")
      }
      wordListBuffer -= userWord
      inputtedWord(turn) = "user " + userWord
      turn += 1
      // computer turn
      val computerWord = findWord(userWord.last.toString, wordListBuffer)
      println(computerWord)
      if (computerWord != "") {
        wordPresent = computerWord
        wordListBuffer -= computerWord
        inputtedWord(turn) = "computer " + computerWord
        turn += 1
        gameLoop()
      } else {
        if(check != "lose") {
          val amountOfWords = inputtedWord.filter(x => x._2.contains("user"))
          println("you win, the numbers of word was used is " + amountOfWords.size)
        }
      }
    }
    isUserGoFirst match {
      case 1 =>
        println("User go first:")
        val ran = randomChar
        println(ran)
        wordPresent = ran
        gameLoop()
      case _ =>
        val randomWord = computerRandomWord(wordListBuffer)
        wordListBuffer -= randomWord
        inputtedWord(turn) = randomWord
        println("Computer go first")
        wordPresent = randomWord
        computerTurn(randomWord)
        gameLoop()
    }
  }
  def main(args: Array[String]) {
    game()
  }
}