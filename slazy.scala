object slazy{

	def streamrange(lo: Int, hi: Int): Stream[Int] = {
		if (lo >= hi) Stream.empty
		else Stream.cons(lo, streamrange(lo + 1, hi))
	}

	def listrange(lo: Int, hi: Int): List[Int] = {
		if (lo >= hi) Nil
		else lo :: listrange(lo + 1, hi)
	}

	def isprime(n: Int): Boolean = ! ((2 until n-1) exists (n % _ == 0))



	def something() = {
		println("calling something")
		1
	}
	def callByValue(x: Int) = {
		println("x1=" + x)
		println("x2=" + x)
	}

	def callByName(x: => Int) = {
		println("x1=" + x)
		println("x2=" + x)
	}

	def main(args: Array[String]): Unit = {
		lazy val x = { println ("foo") ; 10 }
		println ("bar")
		println (x)
		println (x)

		println(streamrange(1,10))
		println(listrange(1,10))

		println((streamrange(1000, 10000) filter isprime)(1))

		callByValue(something())
		callByName(something())
	}
}