//Immutable stack using List
//Naar https://gist.github.com/smac89/94b2c347f95a76d0f2214af46fd81ddd
//Versimpeld, ook gebruik ik head als basis van de stack, en peek i.p.v. top,
//Option i.p.v. Either, en alleen maar Lists

import scala.annotation._
final class pure extends Annotation

@pure
case class Stack[A] (items: List[A] = List.empty[A]){
	def push(item:A):Stack[A] = Stack(item::items)
	def pushAll(moreItems:List[A]):Stack[A] = Stack(moreItems++items)
	def pop:Option[ (A,Stack[A]) ] = {
		if(isEmpty) None
		else Some(items.head, Stack(items.tail))
	}
	def peek:Option[A] = {
		if(isEmpty) None
		else Some(items.head)}
	def size: Int = items.length
	def isEmpty : Boolean = items.isEmpty
	override def toString = "Stack(" + items.mkString(", ") + ")"
/*
	//Met Either kun je de fout specificeren. Hier niet nodig
	
	def popE:Either[ String, (A,Stack[A]) ] = {
		if(isEmpty) Left("Empty stack is not poppable")
		else Right(items.head, Stack(items.tail))
	}
	def peekE:Either[String, A] = {
		if(isEmpty) Left("Empty stack is not peekable")
		else Right(items.head)}
*/
}

import scala.reflect.ClassTag
def objectType(a:Any):String =  ClassTag(a.getClass).toString()

val leeg = Stack[Int]()
//println("leeg: " + leeg)
assert(leeg.toString  == "Stack()")
assert(leeg.isEmpty)

val rijtje = Stack(List(1,2,3,4,5))
assert( Stack(List(1,2,3,4,5)).pop.get._2 == Stack(List(2, 3, 4, 5)) )

val rijtje2 = rijtje.push(0)
//println("rijtje: " + rijtje)
//println("rijtje2: " + rijtje2)
assert( rijtje2 == Stack(List(0, 1, 2, 3, 4, 5)) )

val een = leeg.push(2)
//println("een: " + een)
//println("Peek-leeg:" + leeg.peek)
//println("Peek-rijtje:" + rijtje.peek)
//println("Peek-rijtje2:" + rijtje2.peek)
assert(een.toString == "Stack(2)")
assert(leeg.peek == None)
assert(rijtje.peek == Some(1))
assert(rijtje2.peek == Some(0))

val gepoptLeeg = leeg.pop.getOrElse("Oeps")
//println("GepoptLeeg:" + gepoptLeeg)
assert(gepoptLeeg == "Oeps")

val gepopt2 = rijtje2.pop.getOrElse("Oeps")
//assert(gepopt2._1 == 0 )
//assert(gepopt2._2 ==  Stack( List(1, 2, 3, 4, 5)) )
                               //De Tuple2._1 syntax werkt vreemd genoeg niet!!!!!
//println("Gepopt2:" + gepopt2)
//println("Type gepopt2:" + objectType(gepopt2))
assert(gepopt2 == (0, Stack( List(1, 2, 3, 4, 5))) )
assert(gepopt2.toString == "(0,Stack(1, 2, 3, 4, 5))")

val (result, rest) = gepopt2  // <<----DEZE SYNTAX GEBRUIKEN!!!!
//println("Gepopt2 resultaat: " + result) 
//println("Gepopt2 rest:      " + rest)
assert(result == 0)
assert(rest == Stack(List(1,2,3,4,5)))

val langerRijtje = rijtje.pushAll(List(20,30,40))
//println("langerRijtje: " + langerRijtje)
assert(langerRijtje == Stack( List(20,30,40, 1, 2, 3, 4, 5)) )
assert(langerRijtje.size == 8 )



