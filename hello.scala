object hello{

	def factorial(n: Int): Int = {
		if (n == 0) 1
		else n * factorial(n - 1)
	}


	def improve(guess: Double) = {
	    (guess + x / guess) / 2
	}

	def isGoodEnough(guess: Double) = {
	    abs(square(guess) - x) < 0.001
	}

	def sqrtIter(guess: Double): Double = {
	    if (isGoodEnough(guess)) guess
	    else sqrtIter(improve(guess))	  
	}

	def sqrt(x: Double) = {	  
	  sqrtIter(1.0)
	}	

	def calc(a: Int, op: String): Double = {

		val rs = op match {
		    case "fact" => factorial(a)
		    case "sqrt" => sqrt(a)
		    case _  => 0
		}

		rs
	}

	def main(args: Array[String]): Unit = {
		val tmp = "Hello, world!"
		println(calc(16))
	}
}