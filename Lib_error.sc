class ProgrammingError(msg:String, location:String)extends Error {
  println("ProgrammingError: "+msg + "\nLocation: "+ location)
}
case class ParserException (message:String) extends Exception(message)
