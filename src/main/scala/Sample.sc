import scala.collection.JavaConverters._
val a = List(1, 2, 3, 4, 5,6,7)
val b = List(6)
val c = List()
val str1 = List('a', 'a', 'b', 'c', 'c', 'd', 'd', 'a', 'b', 'b')
val str2 = List('b')
val str3 = List()
//Prob 1
def getLast(xs: List[Int]): Any = {
  xs match {
    case List() => Nil
    case a:: Nil => a
    case _ => getLast(xs.tail)
  }
}
getLast(a)
getLast(b)
getLast(c)

//Prob 2
def getPenultimate(xs: List[Int]): Any = {
  xs match {
    case Nil    => Nil
    case a::Nil => Nil
    case a::b::Nil => a
    case _ => getPenultimate(xs.tail)

  }
}
getPenultimate(a)
getPenultimate(b)
getPenultimate(c)

//Prob 3

def reverseList(reversed: List[Int], xs: List[Int]): List[Int] = {

  xs match {
    case Nil => reversed
    case _ => reverseList(xs.head::reversed, xs.tail)
  }
}


def reverseListSimple(xs: List[Int]): List[Int] = {
  xs match {
    case Nil => Nil
    case a :: b => {
      reverseListSimple(b) ++ List(a)
    }
  }
}

reverseList(Nil, a)
reverseListSimple(a)

//Prob 4

def compress(xs: List[Char]): List[Char] = {
  xs match {
    case Nil => Nil
    case a::Nil => a :: Nil
    case _ if xs.head != xs.tail.head => xs.head::compress(xs.tail)
    case _ => compress(xs.tail)
  }
}

compress(str1)
compress(str2)
compress(str3)


//Prob 5

def duplicateN(n: Int, xs: List[Int], result: List[Int]): List[Int] = {

  def duplicateElem(n: Int, x: Int, res: List[Int]): List[Int] = {
    if(n == 0)
      res
    else
      duplicateElem(n-1, x, x::res)
  }

  if (xs.isEmpty)
      result
  else
    duplicateN(n, xs.tail, result:::duplicateElem(n, xs.head, Nil))


}

duplicateN(4,a,Nil)

//Prob 6

def drop(n: Int, xs:List[Int]): List[Int] = {

  def dropNth(n: Int, x: Int, xs: List[Int], res: List[Int]): List[Int] = {
    if(xs.isEmpty)
        res
    else
    {
      if (x == 1)
        dropNth(n, n, xs.tail, res)

      else
         dropNth(n, x - 1, xs.tail, xs.head::res)
    }
  }

  dropNth(n, n, xs, Nil)
}

drop(3, a)

//Prob 7
def slice(st: Int, end: Int, xs: List[Int]): List[Int] = {

  def getSlice(n: Int, xs: List[Int]): List[Int] = {
    if(n == 0)
      Nil
    else
      xs.head :: getSlice(n-1, xs.tail)
  }

  if(st == 0)
    getSlice(end, xs)
  else
    slice(st-1, end -1, xs.tail)
}

slice(1, 5, a)



//Prob 9 :
def findSum(a: Int, b: Int, end: Int) = {

  def findMulpSum(n: Int, end: Int): Int = {

    def  greatestDivisorLT(n: Int ,end: Int): Int = {
      end/n
    }
    val x: Int = greatestDivisorLT(n ,end)

    n * (x* (x+1)/2)
  }

  findMulpSum(a, end) + findMulpSum(b, end) - findMulpSum( a * b, end)
}

findSum(3, 5, 15)

//Prob 11

//n*2 C 2 = 40 c 2
