/* Lazy Evaluation */

// 1) print stream
 val str = 1 #:: 2 #:: 3 #:: Stream.empty
 str.foreach(x => println(x))

// 2) fibonacci
def fib(a: Int, b: Int): Stream[Int] =
	a #:: fib(b, a + b)

val fibs = fib(1, 1).take(7)
fibs.foreach(x => println(x))

// 3) triangular
def trian(a: Int, b: Int): Stream[Int] = 	
	(b) #:: trian(a+1,a+b)

val tr = trian(2,1).take(10)
tr.foreach(x=>println(x))


// 4) is pentagonal
import scala.math.sqrt
var count = 1

def streamrange(lo: Int, hi: Int): Stream[Int] =
  if (lo >= hi) Stream.empty
  else Stream.cons(lo, streamrange(lo + 1, hi))

def ispentagonal(N: Int ): Boolean = { 
  var n = (sqrt((24*N)+1)+1)/6
  if(n==count){
    count = count+1
    return true
  }
  false
}
(streamrange(1, 1000) filter ispentagonal)(5)

// 5) cuadrados mágicos


/* Type Classes */
// 1) multiply
def multiply(value: Int)(implicit by: Int) = value*by
implicit val m = 4
multiply(4)


// 2)
def tprint[T](list: Array[T]): Unit = {
  var j = 0 
  while (j < list.length) {
    print(list(j))
    print(" ")
    j = j+1
  }
  println()
}

def selectionsort[T](list: Array[T])(implicit ord: Ordering[T]): Unit = {
  def swap[T](list: Array[T], i: Int, j: Int) {
    var tmp = list(i)
    list(i) = list(j)
    list(j) = tmp
  }
  
  var i = 0
  while(i < (list.length - 1)) {
    var min = i
    var j = i + 1
    
    while (j < list.length) {
      if(ord.lt(list(j),list(min))) {
        min = j
      }
      j += 1
    }
      
    swap(list,i,min)    
    i += 1
  }
    
}

var it = Array(1,2,3,6,4,3,7,12)
selectionsort(it)
tprint(it)

var dt = Array(5.1,3.2,1.3,0.7,2.3)
selectionsort(dt)
tprint(dt)

var st = Array("z","y","h","a","b","c","d","e")
selectionsort(st)
tprint(st)

// 3)
def binarysearch[T](list: Array[T], target: T)(implicit ord: Ordering[T]): Int = {
  var left = 0
  var right = list.length-1
  while (left<=right) {
    val mid = left + (right-left)/2
    if (list(mid)==target)
      return mid
    else if (ord.gt(list(mid),target))
      right = mid-1
    else
      left = mid+1
    }
  -1
}

var it = Array(1,2,4,5,6,7,8,9,10,11,12,14)
binarysearch(it,11)

var dt = Array(1.1,1.2,1.3,1.7,2.3)
binarysearch(dt,1.2)

var ds = Array("a","b","c","d","e")
binarysearch(ds,"e")

// 4) 
abstract class TPlus[A] {
  def add(x: A, y: A): A
  def unit: A
}

implicit val StrTPlus: TPlus[String] = new TPlus[String] {
  def add(x: String, y: String): String = x concat y
  def unit: String = ""
}

implicit val IntTPlus: TPlus[Int] = new TPlus[Int] {
  def add(x: Int, y: Int): Int = x + y
  def unit: Int = 0
}

def sum[A](xs: List[A])(implicit m: TPlus[A]): A =
  if (xs.isEmpty) m.unit
  else m.add(xs.head, sum(xs.tail))
  
println(sum(List(1, 2, 3)))
println(sum(List("a", "b", "c")))