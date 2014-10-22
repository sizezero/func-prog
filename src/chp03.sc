object chp03 {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet

	sealed trait List[+A]
	case object Nil extends List[Nothing]
	case class Cons[+A](head: A, tail: List[A]) extends List[A]
	
	object List {
	
		def sum(ints: List[Int]): Int = ints match {
			case Nil => 0
			case Cons(x,xs) => x + sum(xs)
		}
		
		def product(ds: List[Double]): Double = ds match {
			case Nil => 1.0
			case Cons(0.0, _) => 0.0
			case Cons(x,xs) => x * product(xs)
		}
		
		def apply[A](as: A*): List[A] =
			if (as.isEmpty) Nil
			else Cons(as.head, apply(as.tail: _*))

	}
  
  // ex 3.1
  
  // 3
  
  val x = List(1,2,3,4,5) match {
  	case Cons(x, Cons(2, Cons(4, _))) => x
  	case Nil => 42
  	case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
  	case Cons(h, t) => h + List.sum(t)
  	case _ => 101
  }                                               //> x  : Int = 3
  
  // ex 3.2
  
  def tail[A](l: List[A]): List[A] = l match {
  	case Nil => Nil
  	case Cons(_, t) => t
  }                                               //> tail: [A](l: chp03.List[A])chp03.List[A]
  
  // ex 3.3
  
  def setHead[A](l: List[A], h: A): List[A] = l match {
  	case Nil => List(h)
  	case Cons(_, t) => Cons(h, t)
  }                                               //> setHead: [A](l: chp03.List[A], h: A)chp03.List[A]
  setHead(List(1,2,3),42)                         //> res0: chp03.List[Int] = Cons(42,Cons(2,Cons(3,Nil)))
  
  // ex 3.4
  
  def drop[A](l: List[A], n: Int): List[A] =
  	if (n==0) l
  	else l match {
  		case Nil => Nil
  		case Cons(_, t) => drop(t, n-1)
  	}                                         //> drop: [A](l: chp03.List[A], n: Int)chp03.List[A]
  	
  drop(List(1,2,3),2)                             //> res1: chp03.List[Int] = Cons(3,Nil)
  drop(List(1,2,3),4)                             //> res2: chp03.List[Int] = Nil
  
  // ex 3.5
  
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
  		case Nil => Nil
  		case Cons(h, t) => if (!f(h)) l else dropWhile(t, f)
  	}                                         //> dropWhile: [A](l: chp03.List[A], f: A => Boolean)chp03.List[A]
 
 	def lte2(n: Int): Boolean = n<=2          //> lte2: (n: Int)Boolean
  dropWhile(List(1,2,3), (x: Int) => { x <= 2 } ) //> res3: chp03.List[Int] = Cons(3,Nil)
 
 	// ex 3.6
 	
 	def init[A](l: List[A]): List[A] = l match {
 		case Nil => Nil
 		case Cons(h, Nil) => Nil
 		case Cons(h, t) => Cons(h, init(t))
 	}                                         //> init: [A](l: chp03.List[A])chp03.List[A]
 	
 	init(List(1,2,3))                         //> res4: chp03.List[Int] = Cons(1,Cons(2,Nil))
 	init(List(1,2))                           //> res5: chp03.List[Int] = Cons(1,Nil)
 	init(List(1))                             //> res6: chp03.List[Int] = Nil
 	init(Nil)                                 //> res7: chp03.List[Nothing] = Nil
 	
 	// ex 3.7
 	
 	// product needs a short circuit HOF in order to implement product and halt on 0
 	
 	// ex 3.8
 	
 	def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = as match {
 		case Nil => z
 		case Cons(x, xs) => f(x, foldRight(xs, z)(f))
 	}                                         //> foldRight: [A, B](as: chp03.List[A], z: B)(f: (A, B) => B)B
 	
 	foldRight(List(1,2,3), Nil: List[Int])(Cons(_,_))
                                                  //> res8: chp03.List[Int] = Cons(1,Cons(2,Cons(3,Nil)))
       
 	// foldRight() and List() are inverse functions?
 	
 	// ex 3.9
 	
 	def length[A](as: List[A]): Int = foldRight(as, 0)((_,b) => b+1)
                                                  //> length: [A](as: chp03.List[A])Int
 	length(List(1,2,3))                       //> res9: Int = 3
 	length(List(1,2))                         //> res10: Int = 2
 	length(List(1))                           //> res11: Int = 1
 	length(Nil)                               //> res12: Int = 0
 	
 	// ex 3.10
 	
 	def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = as match {
 		case Nil => z
 		case Cons(x, xs) => foldLeft(xs, f(z,x))(f)
 	}                                         //> foldLeft: [A, B](as: chp03.List[A], z: B)(f: (B, A) => B)B
 	foldLeft(List(1,3,5), 1)(_ - _)           //> res13: Int = -8
 	foldRight(List(1,3,5), 1)(_ - _)          //> res14: Int = 2
 	
 	// ex 3.11
 	
 	def sum(l: List[Int]) = foldLeft(l, 0)(_ + _)
                                                  //> sum: (l: chp03.List[Int])Int
 	sum(List(1,2,3))                          //> res15: Int = 6
 	
 	def product(l: List[Int]) = foldLeft(l, 1)(_ * _)
                                                  //> product: (l: chp03.List[Int])Int
 	product(List(1,2,3))                      //> res16: Int = 6
 	
 	def len(l: List[Int]) = foldLeft(l, 0)((b,a) => b + 1)
                                                  //> len: (l: chp03.List[Int])Int
 	len(List(1,2,3))                          //> res17: Int = 3
 
 	// ex 3.12
 	
 	def reverse[A](l: List[A]): List[A] = foldLeft(l, Nil: List[A])( (b,a) => Cons(a, b))
                                                  //> reverse: [A](l: chp03.List[A])chp03.List[A]
 	reverse(List(1,2,3))                      //> res18: chp03.List[Int] = Cons(3,Cons(2,Cons(1,Nil)))
 	
 	// 3.13
 	
 	// ???
 	
 	// 3.14
 	
 	def append[A](a1: List[A], a2: List[A]): List[A] = foldRight(a1, a2)(Cons(_, _))
                                                  //> append: [A](a1: chp03.List[A], a2: chp03.List[A])chp03.List[A]
  append(List(1,2,3), List(4,5,6))                //> res19: chp03.List[Int] = Cons(1,Cons(2,Cons(3,Cons(4,Cons(5,Cons(6,Nil)))))
                                                  //| )
                                                  
  // 3.15
  
  def concat[A](ll: List[List[A]]): List[A] = foldRight(ll, Nil:List[A])((a,b) => append(a,b))
                                                  //> concat: [A](ll: chp03.List[chp03.List[A]])chp03.List[A]
  concat(List(List(1,2,3),List(4,5,6),List(7,8,9)))
                                                  //> res20: chp03.List[Int] = Cons(1,Cons(2,Cons(3,Cons(4,Cons(5,Cons(6,Cons(7,C
                                                  //| ons(8,Cons(9,Nil)))))))))
                                                  
	// 3.16
	
	def addOne(l: List[Int]): List[Int] = l match {
		case Nil => Nil
		case Cons(h, t) => Cons(h+1, addOne(t))
	}                                         //> addOne: (l: chp03.List[Int])chp03.List[Int]
	
	addOne(List(1,2,3))                       //> res21: chp03.List[Int] = Cons(2,Cons(3,Cons(4,Nil)))
	
	// 3.17
	
	def doubleToString(l: List[Double]): List[String] = l match {
		case Nil => Nil
		case Cons(h, t) => Cons(h.toString, doubleToString(t))
	}                                         //> doubleToString: (l: chp03.List[Double])chp03.List[String]
	doubleToString(List(1.0,2.0,3.0))         //> res22: chp03.List[String] = Cons(1.0,Cons(2.0,Cons(3.0,Nil)))
	
	// 3.18
	
	def map[A,B](as: List[A])(f: A => B): List[B] = as match {
		case Nil => Nil
		case Cons(h, t) => Cons(f(h), map(t)(f))
	}                                         //> map: [A, B](as: chp03.List[A])(f: A => B)chp03.List[B]
	
	map(List(1,2,3))(_ + 1)                   //> res23: chp03.List[Int] = Cons(2,Cons(3,Cons(4,Nil)))
	map(List(1.0,2.0,3.0))(_.toString)        //> res24: chp03.List[String] = Cons(1.0,Cons(2.0,Cons(3.0,Nil)))
	
	// 3.19
	
	def filter[A](as: List[A])(f: A => Boolean): List[A] = as match {
		case Nil => Nil
		case Cons(h, t) => if (f(h)) Cons(h, filter(t)(f)) else filter(t)(f)
	}                                         //> filter: [A](as: chp03.List[A])(f: A => Boolean)chp03.List[A]
	
	filter(List(1,2,3,4,5,6,7,8))(_ % 2 == 0) //> res25: chp03.List[Int] = Cons(2,Cons(4,Cons(6,Cons(8,Nil))))
	
	// 3.20
	
	def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = as match {
		case Nil => Nil
		case Cons(h, t) => append(f(h), flatMap(t)(f))
	}                                         //> flatMap: [A, B](as: chp03.List[A])(f: A => chp03.List[B])chp03.List[B]
	
	flatMap(List(1,2,3))(i => List(i,i))      //> res26: chp03.List[Int] = Cons(1,Cons(1,Cons(2,Cons(2,Cons(3,Cons(3,Nil)))))
                                                  //| )
	
  // 3.21
  
	def filter2[A](as: List[A])(f: A => Boolean): List[A] = flatMap(as)(a => if (f(a)) List(a) else Nil)
                                                  //> filter2: [A](as: chp03.List[A])(f: A => Boolean)chp03.List[A]
	
	filter2(List(1,2,3,4,5,6,7,8))(_ % 2 == 0)//> res27: chp03.List[Int] = Cons(2,Cons(4,Cons(6,Cons(8,Nil))))
	
	// 3.22
	
	def addPairs(as1: List[Int], as2: List[Int]): List[Int] = as1 match {
		case Nil => Nil
		case Cons(a1, t1) => as2 match {
			case Nil => Nil
			case Cons(a2, t2) => Cons(a1+a2, addPairs(t1, t2))
		}
	}                                         //> addPairs: (as1: chp03.List[Int], as2: chp03.List[Int])chp03.List[Int]
	
	addPairs(List(1,2,3),List(4,5,6))         //> res28: chp03.List[Int] = Cons(5,Cons(7,Cons(9,Nil)))
	
	// 3.23

	def zipWith[A](as1: List[A], as2: List[A])(f: (A, A) => A): List[A] = as1 match {
		case Nil => Nil
		case Cons(a1, t1) => as2 match {
			case Nil => Nil
			case Cons(a2, t2) => Cons(f(a1,a2), zipWith(t1, t2)(f))
		}
	}                                         //> zipWith: [A](as1: chp03.List[A], as2: chp03.List[A])(f: (A, A) => A)chp03.L
                                                  //| ist[A]
	
	zipWith(List(1,2,3),List(4,5,6))(_ + _)   //> res29: chp03.List[Int] = Cons(5,Cons(7,Cons(9,Nil)))
	
	
	// 3.24
	
	def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
		def isPrefix[A](l: List[A], pre: List[A]): Boolean = l match {
			case Nil => pre == Nil
			case Cons(lh, lt) => pre match {
				case Nil => true
				case Cons(ph, pt) => if (lh==ph) isPrefix(lt,pt) else false
			}
		}
		sup match {
			case Nil => isPrefix(Nil, sub)
			case Cons(h, t) => if (isPrefix(sup, sub)) true else hasSubsequence(t, sub)
		}
	}                                         //> hasSubsequence: [A](sup: chp03.List[A], sub: chp03.List[A])Boolean
	
	hasSubsequence(List(1,2,3,4),List(2,3))   //> res30: Boolean = true
	hasSubsequence(List(1,2,3,4),List(3,2))   //> res31: Boolean = false
	hasSubsequence(List(1,2,3,4),List(1,2))   //> res32: Boolean = true
	hasSubsequence(List(1,2,3,4),List(4))     //> res33: Boolean = true
	hasSubsequence(List(1,2,3,4),List(1,2,3,4))
                                                  //> res34: Boolean = true



	sealed trait Tree[+A]
	case class Leaf[A](value: A) extends Tree[A]
	case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
	
	val tree1 = Branch(Branch(Leaf("a"),Leaf("b")),Branch(Leaf("c"),Leaf("d")))
                                                  //> tree1  : chp03.Branch[String] = Branch(Branch(Leaf(a),Leaf(b)),Branch(Leaf(
                                                  //| c),Leaf(d)))
	// 3.25
	
	def count[A](t: Tree[A]): Int = t match {
		case Leaf(_) => 1
		case Branch(left, right) => count(left) + count(right) + 1
	}                                         //> count: [A](t: chp03.Tree[A])Int
	count(tree1)                              //> res35: Int = 7
	
	// 3.26
	
	def maximum(t: Tree[Int]): Int = t match {
		case Leaf(n) => n
		case Branch(left, right) => maximum(left).max(maximum(right))
	}                                         //> maximum: (t: chp03.Tree[Int])Int

	val tree2 = Branch(Branch(Leaf(1),Leaf(2)),Branch(Leaf(3),Leaf(4)))
                                                  //> tree2  : chp03.Branch[Int] = Branch(Branch(Leaf(1),Leaf(2)),Branch(Leaf(3),
                                                  //| Leaf(4)))
	maximum(tree2)                            //> res36: Int = 4
	
	// 3.27
	
  def depth[A](t: Tree[A]): Int = t match {
		case Leaf(_) => 1
		case Branch(left, right) => depth(left).max(depth(right)) + 1
	}                                         //> depth: [A](t: chp03.Tree[A])Int
	depth(tree1)                              //> res37: Int = 3
	depth(tree2)                              //> res38: Int = 3
	
	// 3.28
	
  def mapt[A](t: Tree[A])(f: A => A): Tree[A] = t match {
		case Leaf(a) => Leaf(f(a))
		case Branch(left, right) => {
			// why does mapt need an explicit [A] cast?
			val l: Tree[A] = mapt[A](left)(f)
			val r: Tree[A] = mapt[A](right)(f)
			Branch(l, r)
		}
	}                                         //> mapt: [A](t: chp03.Tree[A])(f: A => A)chp03.Tree[A]
	mapt(tree2)(_ + 1)                        //> res39: chp03.Tree[Int] = Branch(Branch(Leaf(2),Leaf(3)),Branch(Leaf(4),Leaf
                                                  //| (5)))
	
	// 3.29
	
	// is this more of a reduce?
	def fold[A,B](t: Tree[A])(fl: A => B)(fb: (B,B) => B): B = t match {
		case Leaf(a) => fl(a)
		case Branch(left, right) => fb(fold(left)(fl)(fb), fold(right)(fl)(fb))
	}                                         //> fold: [A, B](t: chp03.Tree[A])(fl: A => B)(fb: (B, B) => B)B
	
	
	def count2[A](t: Tree[A]): Int = fold(t)(a => 1)((l, r) => l+r+1)
                                                  //> count2: [A](t: chp03.Tree[A])Int
	count2(tree1)                             //> res40: Int = 7
	
	def maximum2(t: Tree[Int]): Int = fold(t)(n => n)((l,r) => l.max(r))
                                                  //> maximum2: (t: chp03.Tree[Int])Int

	maximum2(tree2)                           //> res41: Int = 4
	
	
	def depth2[A](t: Tree[A]): Int = fold(t)(n => 1)((l,r) => l.max(r) + 1)
                                                  //> depth2: [A](t: chp03.Tree[A])Int
	depth2(tree1)                             //> res42: Int = 3
	depth2(tree2)                             //> res43: Int = 3
	
	def mapt2[A](t: Tree[A])(f: A => A): Tree[A] = fold[A,Tree[A]](t)( a => Leaf(f(a)) )( (l,r) => Branch(l,r) )
                                                  //> mapt2: [A](t: chp03.Tree[A])(f: A => A)chp03.Tree[A]
  mapt2(tree2)(_ + 1)                             //> res44: chp03.Tree[Int] = Branch(Branch(Leaf(2),Leaf(3)),Branch(Leaf(4),Leaf
                                                  //| (5)))
	
}