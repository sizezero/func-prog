object chp05 {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  // let's see if the scala compiler allows us to write code in a worksheet.  Nope, scala compiler sucks.
  
  // ex 5.1
  
  trait Stream[+A] {
  
    def toList: List[A] = this match {
    	case Empty => Nil
    	case Cons(h, t) => h() :: t().toList
    }
  
  	def take(n: Int): Stream[A] = this match {
  		case Empty => Empty
  		case Cons(h, t) => Stream.cons(h(), t().take(n-1))
  	}
  	
  	//def drop(n: Int): Stream[A] =
  		  	
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
  
  // ex 5.2
  
  Stream(1,2,3).take(1).toList                    //> java.lang.ClassFormatError: Duplicate field name&signature in class file chp
                                                  //| 05$$anonfun$main$1$Cons$3
                                                  //| 	at java.lang.ClassLoader.defineClass1(Native Method)
                                                  //| 	at java.lang.ClassLoader.defineClass(ClassLoader.java:800)
                                                  //| 	at java.security.SecureClassLoader.defineClass(SecureClassLoader.java:14
                                                  //| 2)
                                                  //| 	at java.net.URLClassLoader.defineClass(URLClassLoader.java:449)
                                                  //| 	at java.net.URLClassLoader.access$100(URLClassLoader.java:71)
                                                  //| 	at java.net.URLClassLoader$1.run(URLClassLoader.java:361)
                                                  //| 	at java.net.URLClassLoader$1.run(URLClassLoader.java:355)
                                                  //| 	at java.security.AccessController.doPrivileged(Native Method)
                                                  //| 	at java.net.URLClassLoader.findClass(URLClassLoader.java:354)
                                                  //| 	at java.lang.ClassLoader.loadClass(ClassLoader.java:425)
                                                  //| 	at sun.misc.Launcher$AppClassLoader.loadClass(Launcher.java:308)
                                                  //| 	at java.lang.ClassLoader.loadClass(ClassLoader.java:358)
                                                  //| 	at chp05$$anonfun$main$1$Cons$4$.apply(chp05.scala:24)
                                                  //| 	at chp05$$anonfun$main$1$St
                                                  //| Output exceeds cutoff limit.
}