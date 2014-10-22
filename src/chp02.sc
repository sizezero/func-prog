object chp2 {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet

	// ex 2.1
  
  def fib(n: Int): Int = {
  	def loop(i: Int, prev1: Int, prev2: Int): Int =
  		if (n==i) prev1+prev2
  		else if (i==1) loop(2, 1, 0)
  		else loop(i+1, prev1+prev2, prev1)
  	loop(1, 0, 0)
  }                                               //> fib: (n: Int)Int
  
  1 to 7 map { fib(_) }                           //> res0: scala.collection.immutable.IndexedSeq[Int] = Vector(0, 1, 2, 3, 5, 8, 
                                                  //| 13)

	// ex 2.2
                                                  
  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
  	def loop(i: Int): Boolean = {
  		if (i >= as.length-1) true
  		else if (ordered(as(i), as(i+1))) loop(i+1)
  		else false
  	}
  	if (as.length == 0 || as.length == 1) true
  	else loop(0)
  }                                               //> isSorted: [A](as: Array[A], ordered: (A, A) => Boolean)Boolean
  
  isSorted(1 to 5 toArray, (a: Int, b: Int) => { a<=b } )
                                                  //> res1: Boolean = true
  isSorted(5 to 1 by -1 toArray, (a: Int, b: Int) => { a<=b } )
                                                  //> res2: Boolean = false

	Array.fill(5)(1)                          //> res3: Array[Int] = Array(1, 1, 1, 1, 1)
  isSorted(Array.fill(5)(1), (a: Int, b: Int) => { a<=b } )
                                                  //> res4: Boolean = true
  
  // ex 2.3
  
  def curry[A,B,C](f: (A, B) => C): A => (B => C) = {
  	def g(a: A): B => C = {
  		def h(b: B): C = f(a,b)
  		h
  	}
  	g
  }                                               //> curry: [A, B, C](f: (A, B) => C)A => (B => C)
  
  // ex 2.4
  
  def uncurry[A,B,C](f: A => B => C): (A, B) => C = {
  	def g(a: A, b: B): C = {
  		f(a)(b)
  	}
  	g
  }                                               //> uncurry: [A, B, C](f: A => (B => C))(A, B) => C

  // ex 2.5
  
  def compose[A,B,C](f: B => C, g: A => B): A => C = {
  	def h(a: A): C = {
  		f(g(a))
  	}
  	h
  }                                               //> compose: [A, B, C](f: B => C, g: A => B)A => C
}