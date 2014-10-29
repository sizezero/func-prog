object chp06 {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  trait RNG {
  	def nextInt: (Int, RNG)
  }
  
  case class SimpleRNG(seed: Long) extends RNG {
  	def nextInt: (Int, RNG) = {
  		val newSeed = (seed * 0x5DDEECE66DL + 0XBL) & 0XFFFFFFFFFFFFL
  		val nextRNG = SimpleRNG(newSeed)
  		val n = (newSeed >>> 16).toInt
  		(n, nextRNG)
  	}
  }
  
  val rng = SimpleRNG(42)                         //> rng  : chp06.SimpleRNG = SimpleRNG(42)
  val (n1, rng2) = rng.nextInt                    //> n1  : Int = 258380509
                                                  //| rng2  : chp06.RNG = SimpleRNG(16933225090541)
  val (n2, rng3)= rng2.nextInt                    //> n2  : Int = 1120283631
                                                  //| rng3  : chp06.RNG = SimpleRNG(73418908081140)
 
 	// ex 6.1
 	
 	def nonNegativeInt(rng: RNG): (Int, RNG) = {
 		val (i, rng2) = rng.nextInt
 		val j = if (i < 0) -(i + 1) else i
 		(j, rng2)
 	}                                         //> nonNegativeInt: (rng: chp06.RNG)(Int, chp06.RNG)
 	nonNegativeInt(rng)                       //> res0: (Int, chp06.RNG) = (258380509,SimpleRNG(16933225090541))
 	
 	// ex 6.2
 	
 	def double(rng: RNG): (Double, RNG) = {
 		val (i, rng2) = nonNegativeInt(rng)
 		val d: Double = i.toDouble / Int.MaxValue
 		(d, rng2)
 	}                                         //> double: (rng: chp06.RNG)(Double, chp06.RNG)
 	double(rng)                               //> res1: (Double, chp06.RNG) = (0.12031780049219624,SimpleRNG(16933225090541))
 	
 	// 6.3
 	
 	def intDouble(rng: RNG): ((Int, Double), RNG) = {
 		val (i, rng2) = rng.nextInt
 		val (d, rng3) = double(rng2)
 		((i, d), rng3)
 	}                                         //> intDouble: (rng: chp06.RNG)((Int, Double), chp06.RNG)
 	
 	def doubleInt(rng: RNG): ((Double, Int), RNG) = {
 		val (d, rng2) = double(rng)
 		val (i, rng3) = rng2.nextInt
 		((d, i), rng3)
 	}                                         //> doubleInt: (rng: chp06.RNG)((Double, Int), chp06.RNG)
 	
 	def double3(rng: RNG): ((Double, Double, Double), RNG) = {
 		val (d1, rng2) = double(rng)
 		val (d2, rng3) = double(rng2)
 		val (d3, rng4) = double(rng3)
 		((d1, d2, d3), rng4)
 	}                                         //> double3: (rng: chp06.RNG)((Double, Double, Double), chp06.RNG)
 	
 	// ex 6.4
 	
 	def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
 		if (count==0) (Nil, rng)
 		else {
 			val (h, rng2) = rng.nextInt
 			val (t, rng3) = ints(count-1)(rng2)
 			(h :: t, rng3)
 		}
 	}                                         //> ints: (count: Int)(rng: chp06.RNG)(List[Int], chp06.RNG)
 	ints(5)(rng)                              //> res2: (List[Int], chp06.RNG) = (List(258380509, 1120283631, 1805604882, 264
                                                  //| 896780, -377482330),SimpleRNG(256736294759617))

	// 6.5
	
	type Rand[+A] = RNG => (A, RNG)

	// a function that can take a rng
	val int: Rand[Int] = _.nextInt            //> int  : chp06.Rand[Int] = <function1>
	
	def unit[A](a: A): Rand[A] =
		rng => (a, rng)                   //> unit: [A](a: A)chp06.Rand[A]
	def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
		rng => {
			val (a, rng2) = s(rng)
			(f(a), rng2)
			}                         //> map: [A, B](s: chp06.Rand[A])(f: A => B)chp06.Rand[B]
			
	def double_2: Rand[Double] =
		map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))
                                                  //> double_2: => chp06.Rand[Double]
	double_2(rng)                             //> res3: (Double, chp06.RNG) = (0.12031780043616891,SimpleRNG(16933225090541))
                                                  //| 
	
	// ex 6.6
	
	def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
		rng1 => {
			val (a, rng2) = ra(rng1)
			val (b, rng3) = rb(rng2)
			(f(a,b), rng3)
		}
	}                                         //> map2: [A, B, C](ra: chp06.Rand[A], rb: chp06.Rand[B])(f: (A, B) => C)chp06.
                                                  //| Rand[C]
  // 6.7
 
  // return value of sequence Rand[List[A]] doesn't make sense
 
  // 6.8
 
  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
 		rng => {
 			val (a, rng2) = f(rng)
 			g(a)(rng2)
 		}                                 //> flatMap: [A, B](f: chp06.Rand[A])(g: A => chp06.Rand[B])chp06.Rand[B]
 		
 	// got from answer sheet; looks awful
  def nonNegativeLessThan(n: Int): Rand[Int] =
  	flatMap(nonNegativeInt) { i =>
  		val mod = i % n
  		if (i + (n-1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
  	}                                         //> nonNegativeLessThan: (n: Int)chp06.Rand[Int]
  	
  // I'm not quite understanding this chapter. The complex return values seem to serve no other purpose than to
  // attempt to hide a function argument with syntax.  e.g. the following functions are more or less the same:
  
  def f1[A,B,C](a: A, b: B): C = ???              //> f1: [A, B, C](a: A, b: B)C
  def f2[A,B,C](a: A)(b: B): C = ???              //> f2: [A, B, C](a: A)(b: B)C
  def f3[A,B,C](a: A): B => C = ???               //> f3: [A, B, C](a: A)B => C
  
  // 6.9
  
 	def map_[A,B](s: Rand[A])(f: A => B): Rand[B] =
 		flatMap(s)(a => unit(f(a)))       //> map_ : [A, B](s: chp06.Rand[A])(f: A => B)chp06.Rand[B]
 		
 	def map2_[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
 		flatMap(ra)( a =>
 			flatMap(rb)( b =>
 				unit(f(a,b))
 			)
 		)
 	}                                         //> map2_ : [A, B, C](ra: chp06.Rand[A], rb: chp06.Rand[B])(f: (A, B) => C)chp0
                                                  //| 6.Rand[C]
  // 6.10

	// The book likes to call this class State but that's a terrible name since it makes
	// this object sound like a noun.  In reality it's a verb: the function that generates
	// a new value (A) and modifies a state (S).  A better name would be Generator

	case class Generator[S,+A](run: S => (A,S)) {
	
		// Runs the function f after the normal Generator this
		// Acts like post processing of the generated value
		// Not sure why this deserves to be called flatMap besides the signature.
		// What makes this a monad and deserving of map and flatMap?
	  def flatMap[B](f: A => Generator[S,B]): Generator[S,B] =
 			Generator(s => {
 				val (a, s2) = run(s)
 				f(a).run(s2)
 			})

		// Same comments as flatMap
 		def map[B](f: A => B): Generator[S,B] = Generator( s => {
 			val (a, s2) = run(s)
 			(f(a), s2)
 		})

 		def map2[B,C](gb: Generator[S,B])(f: (A, B) => C): Generator[S,C] =
 			flatMap( a =>
 				gb.flatMap( b =>
 					Generator.unit[S,C](f(a,b))
 				)
 			)
 			
 		// passing on sequence
		
	}
	
	object Generator {
	
		def unit[S,A](a: A): Generator[S,A] =
			Generator(s => (a, s))
			
	}
	
	// 6.11 pass...
}