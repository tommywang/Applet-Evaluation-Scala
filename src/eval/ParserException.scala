package eval

class ParserException(msg:String) extends Exception {
  
  def displayException():String =
  {
	  return "Expression mal formee : " + msg;
  }
}