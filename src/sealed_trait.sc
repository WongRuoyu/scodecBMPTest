
sealed trait A

sealed trait A1 extends A

sealed trait A11 extends A1

case class B1(x:Int) extends A1
case class B2(x:Int) extends A1

sealed trait A2 extends A