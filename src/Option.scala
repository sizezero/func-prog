

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
  
  // ex 4.4
  
  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    if (a.exists(_ == None)) None
    else Some(a.map{ o => o match { case Some(x) => x } })
  }
  p(sequence(List(Some(1),Some(2))))
  p(sequence(List(Some(1),None)))
  
  // ex 4.5
  
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
    case Nil => Some(Nil)
    case ha :: ta => traverse(ta)(f) match {
      case None => None
      case Some(tb) => f(ha) match {
        case None => None
        case Some(b) => Some(b :: tb)
      }
    }
  }
  
  def Try[A](a: => A): Option[A] =
    try Some(a)
    catch { case e: Exception => None}
  
  p( traverse(List("1", "2", "3"))(s => Try(s.toInt)) )
  p( traverse(List("1", "foo", "3"))(s => Try(s.toInt)) )
  
  def sequence2[A](a: List[Option[A]]): Option[List[A]] = traverse(a)(x => x)
  p(sequence2(List(Some(1),Some(2))))
  p(sequence2(List(Some(1),None)))
  
  // ex 4.6
  
  trait Either[+E, +A] {
    
    def map[B](f: A => B): Either[E, B] = this match {
      case l: Left[E] => l
      case Right(a) => Right(f(a))
    }
    
    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
      case l: Left[EE] => l
      case Right(a) => f(a)
    }
    
    def orElse[EE >: E, B >: A](b: Either[EE, B]): Either[EE, B] = this match {
      case l: Left[EE] => b
      case r: Right[B] => r
    }
    
    def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
      for {
        x <- this
        y <- b
      } yield f(x,y)
      
  }
  case class Left[+E](value: E) extends Either[E, Nothing]
  case class Right[+A](value: A) extends Either[Nothing, A]

  p( Right(1).map2(Right(2))(_ + _) )
  p( Left("error").map2(Right(2))((x: Int, y:Int) => x + y) )
  p( Right(1).map2(Left("error"))((x: Int, y:Int) => x + y) )
  
  // ex 4.7
  
  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = es match {
    case Nil => Right(Nil)
    case ha :: tas => ha match {
      case l: Left[E] => l
      case Right(a) => sequence(tas) match {
        case l: Left[E] => l
        case Right(as) => Right(a :: as)
      }
    }
  }
  
  p(sequence(List(Right(1),Right(2))))
  p(sequence(List(Left("error"),Right(2))))
  p(sequence(List(Right(1),Left("error"))))
  p(sequence(List(Left("error1"),Left("error2"))))
  
  def traverse2[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = as match {
    case Nil => Right(Nil)
    case ha :: tas => f(ha) match {
      case l: Left[E] => l
      case Right(b) => traverse2(tas)(f) match {
        case l: Left[E] => l
        case Right(tb) => Right(b :: tb)
      }
    }
  }
  
  
}
