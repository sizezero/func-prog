
// this has to be a scala app.  The code breaks in a 
// worksheet due to compiler bugs


trait Stream[+A] {
  
    def toList: List[A] = this match {
    	case Empty => Nil
    	case Cons(h, t) => h() :: t().toList
    }
  
  	def take(n: Int): Stream[A] = 
  		if (n==0) Empty
  		else this match {
  		  case Empty => Empty
  		  case Cons(h, t) => Stream.cons(h(), t().take(n-1))
  		}
  	
  	def drop(n: Int): Stream[A] = 
  	  if (n==0) this
  	  else this match {
  	    case Empty => Empty
  	    case Cons(h, t) => t().drop(n-1)
  	  }

  	def takeWhile(p: A => Boolean): Stream[A] = this match {
  	  case Empty => Empty
  	  case Cons(h, t) => 
  	    if (p(h())) Stream.cons(h(), t().takeWhile(p))
  	    else Empty
  	}
  	  
  	def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
  	  case Cons(h, t) => f(h(), t().foldRight(z)(f))
  	  case _ => z
  	}
      
  	
  	def forAll(p: A => Boolean): Boolean = this match {
  	  case Empty => true
  	  case Cons(h, t) => p(h()) && t().forAll(p)
  	}
  	
  	// got this from the answers. seems a bit obscure
  	def forAll2(p: A => Boolean): Boolean = 
  	  foldRight(true)((a,b) => p(a) && b)
  	  
  	def takeWhile2(p: A => Boolean): Stream[A] = 
  	  foldRight[Stream[A]](Empty)((h, t) => if (p(h)) Stream.cons(h,t) else Empty)
  	
  	def map[B](p: A => B): Stream[B] =
  	  foldRight[Stream[B]](Empty)((h, t) => Stream.cons(p(h), t))
  	  
  	def filter(p: A => Boolean): Stream[A] =
  	  foldRight[Stream[A]](Empty)((h, t) => if (p(h)) Stream.cons(h, t) else t)
  	  
  	def append[B >: A](a: => Stream[B]): Stream[B] =
  	  foldRight[Stream[B]](a)((h, t) => Stream.cons(h,t))
  	  
  	def flatMap[B](f: A => Stream[B]): Stream[B] =
  	  foldRight[Stream[B]](Empty)((h, t) => f(h).append(t))
  }
  case object Empty extends Stream[Nothing]
  case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]
  
  object Stream {
  	def cons[A](hd: => A, t1: => Stream[A]): Stream[A] = {
  		lazy val head = hd
  		lazy val tail = t1
  		Cons(() => head, () => tail)
  	}
  	
  	def empty[A]: Stream[A] = Empty
  	
  	def apply[A](as: A*): Stream[A] =
  		if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
  		
  }
  
object Lazy extends App {
  def p(o: Object) { println(o.toString) }

  // ex 5.2
  
  p(Stream(1,2,3).take(1).toList)
  p(Stream(1,2,3).take(2).toList)
  p(Stream(1,2,3).drop(1).toList)
  p(Stream(1,2,3).drop(2).toList)
  
  // ex 5.3
  
  p( Stream(1,2,3,4,5).takeWhile(i => { println("e "+i); i < 4 }).toList )
  
  // ex 5.4
  
  // ex 5.5
  p( Stream(1,2,3,4,5).takeWhile2(i => { println("e "+i); i < 4 }).toList )
  
  // ex 5.6 not sure what headOption is
  
  // ex 5.7
  p( Stream(1,2,3,4,5).take(3).map{i => {println("map "+i); i + 10}}.toList )
  p( Stream(1,2,3,4,5).take(3).filter{i => {println("filter "+i); i%2==1}}.toList )
  
  // ex 5.8
  
  def constant[A](a: A): Stream[A] = Stream.cons(a, constant(a))
  p(constant(9).take(3).toList)
  
  // ex 5.9
  
  def from(n: Int): Stream[Int] = Stream.cons(n, from(n+1))
  p(from(9).take(3).toList)
  
  // ex 5.10
  
  def fibs: Stream[Int] = { 
    def fibn(prev: Int, last:Int): Stream[Int] = {
	  val next = prev + last
      Stream.cons(next, fibn(last, next))
    }
    Stream.cons(0, Stream.cons(1, fibn(0,1)))
  }
  p(fibs.take(7).toList)
  
  // ex 5.11
  
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case None => Empty
      case Some((a, s)) => Stream.cons(a, unfold(s)(f)) 
    }

  // ex 5.12
  
  def fibs2: Stream[Int] = {
    def produce(s: (Int,Int)): Option[(Int,(Int,Int))] = {
      val (last, prev) = s
      val next = last + prev
      Some(next,(prev,next))
    }
    Stream.cons(0, Stream.cons(1, unfold((0,1))(produce)))      
  }
  p(fibs2.take(7).toList)

  def from2(n: Int): Stream[Int] = unfold(n)(s => Some((n, n+1)))
  p(from2(9).take(3).toList)
  
  def constant2[A](a: A): Stream[A] = unfold(a)(s => Some((s,s)))
  
  def ones2: Stream[Int] = unfold(1)(s => Some(1,1))
  p(ones2.take(3).toList)

  // ex 5.13
  
  def map[A, B](as: Stream[A])(f: A => B): Stream[B] = unfold(as)(as => as match {
    case Empty => None
    case Cons(h, t) => Some( f(h()), t() )
  })
  p(map(Stream(1,2,3,4,5))(_+10).take(3).toList)
  
  def take[A](as: Stream[A])(n: Int) = unfold((as, n))(s => {
    val (as2, n2) = s
    as2 match {
      case Cons(h, t) if (n2>0) => Some( (h(), (t(), n2-1)) )
      case _ => None
    } 
  })
  p(take(Stream(1,2,3,4,5))(3).toList)
  
  // sick of this...
}
