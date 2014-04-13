/**
 * solving the problems from http://aperiodic.net/phil/scala/s-99/
 */

/**
 * Lists
 */
val myList = List(1, 1, 2, 3, 5, 8)
val palList = List(1, 5, 3, 5, 1)
val nestedList = List(1, List(2, 3), 4, List(5, List(6, 7), 8))
val dupList = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
/**
 * p1 - Find the last element of a list.
 */
def last(aList: List[Int]): Int = aList.last
last(myList)
/**
 * p2 - Find the last but one element of a list.
 */
def penultimate(aList: List[Int]): Int = aList.init.last
penultimate(myList)
/**
 * p3 - Find the Kth element of a list.
 */
def nth(index: Int, aList: List[Int]): Int = {
  if (index < 0 || index > aList.size) throw new NoSuchElementException
  else aList(index)
}
def nthRec(index: Int, aList: List[Int]): Int = (index, aList) match {
  case (_, Nil) => throw new NoSuchElementException
  case (0, h :: _) => h
  case (n, _ :: tail) => nthRec(n - 1, tail)
}
nth(5, myList)
nthRec(5, myList)
/**
 * p4 - Find the number of elements of a list.
 */
def length(aList: List[Int]): Int = aList.size
def lengthTailRec(aList: List[Int]): Int = {
  def lengthR(result: Int, cur: List[Int]): Int = cur match {
    case Nil => result
    case _ :: tail => lengthR(result + 1, tail)
  }
  lengthR(0, aList)
}
def lengthFold(aList: List[Int]): Int = aList.foldLeft(0) { (count, _) => count + 1 }
length(myList)
lengthTailRec(myList)
lengthFold(myList)
/**
 * p5 - Reverse a list.
 */
def reverse(aList: List[Int]): List[Int] = aList.reverse
def reverseTailRec(aList: List[Int]): List[Int] = {
  def reverseR(result: List[Int], cur: List[Int]): List[Int] = cur match {
    case Nil => result
    case h :: tail => reverseR(h :: result, tail)
  }
  reverseR(List.empty, aList)
}
def reverseFold(aList: List[Int]): List[Int] = aList.foldLeft(List[Int]()) {
  case (newList, cur) => cur :: newList
}
reverse(myList)
reverseTailRec(myList)
reverseFold(myList)
/**
 * p6 - Find out whether a list is a palindrome.
 */
def isPalindrome(aList: List[Int]): Boolean = aList == aList.reverse
isPalindrome(palList)
isPalindrome(myList)
/**
 * p7 - Flatten a nested list structure.
 */
def flatten(aList: List[Any]): List[Any] = aList.flatMap {
  case l: List[_] => flatten(l)
  case elm => List(elm)
}
flatten(nestedList)
/**
 * p8 - Eliminate consecutive duplicates of list elements.
 */
def compress[A](aList: List[A]): List[A] = aList.foldRight(List[A]()) {
  (elm, newList) =>
    if (newList.isEmpty || newList.head != elm) elm :: newList
    else newList
}
def compressTailRec[A](aList: List[A]): List[A] = {
  def compressR(res: List[A], cur: List[A]): List[A] = cur match {
    case Nil => res.reverse
    case h :: tail => if (tail.isEmpty || h != tail.head) compressR(h :: res, tail) else compressR(res, tail)
//    case h :: tail => compressR(h :: res, tail.dropWhile(_ == h))
  }
  compressR(List[A](), aList)
}
compress(dupList)
compressTailRec(dupList)
