package Server

object ParserCombinator {
  type Parser[A] = String => List[(A, String)]
  
  private def result[A](a: A): Parser[A] = inp => List((a, inp))
  
  // Failure parser -> always fails
  private def fail[A]: Parser[A] = _ => Nil
  
  // Parses the next character in the inp
  private def item: Parser[Char] = inp =>
    if (inp.isEmpty) Nil
    else List((inp.head, inp.tail))
    
  // True parser combinator. 
  def or[A](p1: Parser[A])(p2: Parser[A]): Parser[A] = inp =>
    p1(inp) ++ p2(inp)
    
  // Attempts to match the first char from the input string.
  def sat(p: Char => Boolean): Parser[Char] = inp => for {
    (x, rest) <- item(inp)
    if p(x)
  } yield (x, rest)
}
