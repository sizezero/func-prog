

// this has to be a scala app.  The code breaks in a 
// worksheet due to compiler bugs

trait Option[+A] {
  
  def map[B](f: A => B): Option[B] = this match {
    case Some(a) => Some(f(a))
 	case None => None
  }
 		
  def flatMap[B](f: A => Option[B]): Option[B] = this match {
  	case Some(a) => f(a)
  	case None => None
  }

  def getOrElse[B >: A](default: => B): B = this match {
	case Some(a) => a
	case None => default
  }
		
  def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
  	case Some(_) => this
	case None => ob
  }
		
  def filter(f: A => Boolean): Option[A] = this match {
  	case Some(a) => if (f(a)) this else None
	case None => None
  }
}
  
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Main extends App {
  def p(o: Object) { println(o.toString) }

  p("Yo")
  p(None)
  p(None.map{(i: Int) => i + 1})
  p(Some(1).map{(i: Int) => i + 1})
  
  // ex 4.2
  
  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)
  
  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs).flatMap{ m =>
      mean(xs.map{(x: Double) => math.pow(x - m, 2)})
    }
  }
  
  p(variance(Seq(1.0, 2.0, 3.0)))
  
  // ex 4.3
  
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a.flatMap{ a2 =>
      b.flatMap{ b2 =>
      	Some(f(a2,b2))
      }
    }
  
  p(map2(Some(1), Some(2))(_ + _))
}
