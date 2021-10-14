// Test values
val nums = List(1, 2, 3, 6, 1, 1, 1, 4,4)
val abc = List("a", "x", "p", "d", "r")
val abcdup = List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e')


//Exercice 0
def listLength[A](l: List[A]): Int = l match {
  case Nil       => 0
  case x :: tail => 1 + listLength(tail)
}

//Exercise 1 : Find the last element of a list.
def lastElement[A](l: List[A]): A = l match {
  case Nil       => throw new Exception
  case x :: Nil  => x
  case x :: next => lastElement(next)
}

// Exercise 2 : Find the Kth element of a list.
def nth[A](k:Int, l:List[A]):A = (k,l) match {
    case (0, h::_) => h
    case (k, _::tail) if k > 0 => nth(k - 1, tail)
    case _ => throw new NoSuchElementException
}

// Exercise 3 : Reverse a list
def reverseList[A](l : List[A]) : List[A] = l match {
  case Nil => List()
  case (x :: Nil) => List(x)
  case (x :: xs) => reverseList(xs) ::: List(x)
}

// Exercise 4 : Eliminate consecutive duplicates of list elements
def removeDuplicates[A](l: List[A]):List[A] = l match {
    case Nil => Nil
    case head::Nil => List(head)
    case head::tail if (head == tail.head) => removeDuplicates(tail)
    case head::tail => head::removeDuplicates(tail)
}

// Exercise 5 : Run-length encoding of a list.
def runLength[T](l:List[T]):List[(Int, T)] =
    if(l.isEmpty) Nil
    else {
      val (pack,rest) = l.span(_ == l.head)
      (pack.length, l.head) :: runLength(rest)
    }

// Exercise 6 : Modified run-length encoding.
def runLengthModified[T](l:List[T]):List[(Any)] =
    if(l.isEmpty) Nil
    else {
      val (pack,rest) = l.span(_ == l.head)
      if (pack.length == 1) l.head :: runLengthModified(rest)
      else {
        (pack.length, l.head) :: runLengthModified(rest)
      }
    }

// Exercice 7 : Decode a run-length encoded list.
def decodeRunLength[T](l: List[(Int, T)]): List[T] = l match {
  case Nil => List()
  case ((num, elem) :: Nil) => List.fill(num)(elem)
  case ((num, elem) :: x) => List.fill(num)(elem) ::: decodeRunLength2(x)
}

// Exercise 7 (not recursive but used flatMap)
def decodeRunLength2[T](l:List[(Int, T)]):List[T]= {
  l flatMap {
      case (num, elem) => List.fill(num)(elem)
      case null => throw new IllegalArgumentException
  }
}

// Test
println(listLength(nums))
println(lastElement(abc))
println(nth(2, abc))
println(reverseList(nums))
println(removeDuplicates(abcdup))
println(runLength(abcdup))
println(runLengthModified(abcdup))
val test = runLength(nums)
println(decodeRunLength(test))

//var x = { println("x"); 1 }
//lazy val y = { println("y"); 2 }
//def z = { println("z"); 3 }

// La réponse est la C

//La différence entre eux est que le val est exécuté quand il est défini
//tandis que le lazy val est exécuté quand on y accède la première fois.
//Contrairement à une méthode, le lazy val est exécuté une fois, puis plus jamais.

class NotEmptyString(val s : String){}

object NotEmptyString {
  def apply(x: String) : Option[NotEmptyString] = x match {
      case "" => None
      case null => None
      case full => Some(new NotEmptyString(full))
   }
}

NotEmptyString("")
