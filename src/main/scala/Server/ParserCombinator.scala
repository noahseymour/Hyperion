package Server

object ParserCombinator {
  // TODO: parameterise output context? Or change Option => Either?
  type Parser[a] = String => Option[(a, String)]

  // Monad instance for Parser[A]
  extension [a](p: Parser[a]) {
    // Scala's "fmap"
    def map[b](f: a => b): Parser[b] = p(_).map((x, rest) => (f(x), rest))

    // Scala's "bind" - monadic sequencing of parsers
    def flatMap[b](f: a => Parser[b]): Parser[b] = str => for {
      (a, rest) <- p(str)
      (b, rest1) <- f(a)(rest)
    } yield (b, rest1)

    // No sensible definition
    def foreach(f: a => Any): Unit = ()

    // Applies a predicate to a parser.
    def withFilter(pred: a => Boolean): Parser[a] = str => p(str).filter((x, _) => pred(x))

    // Left-biased alternative
    def +(q: Parser[a]): Parser[a] = str => p(str).orElse(q(str))
  }

  // Ignores the string given
  def ignore[a](a: a): Parser[a] = str => Some((a, str))

  // Failure parser
  def fail[a]: Parser[a] = _ => None

  // Parses the first character of the input.
  def char: Parser[Char] = str =>
    if (str.isEmpty) None
    else Some((str.head, str.tail))

  // Returns a parser that matches on the predicate
  def sat(p: Char => Boolean): Parser[Char] = for {
    q <- char
    if p(q)
  } yield q
  
  // Attempts to apply P. If it cannot match P it will do nothing.
  def optional[a](p: Parser[a])(default: a): Parser[a] = p + ignore(default)

  // Applies parser p zero or more times. Note this NEVER fails
  def many[a](p: Parser[a]): Parser[List[a]] = optional(many1(p))(Nil)

  // Applies parser p one or more times.
  def many1[a](p: Parser[a]): Parser[List[a]] = for {
    x <- p
    xs <- many(p)
  } yield x :: xs

  // Applies SEP in between Ps, beginning and ending with P zero-or-more times, extracting P.
  def sepBy[a, b](p: Parser[a])(sep: Parser[b]): Parser[List[a]] = optional(sepBy1(p)(sep))(Nil)

  // Applies SEP in between Ps, beginning and ending with P one-or-more times, extracting P.
  def sepBy1[a, b](p: Parser[a])(sep: Parser[b]): Parser[List[a]] = for {
    x <- p
    xs <- many(for {
      _ <- sep
      y <- p
    } yield y)
  } yield x :: xs

  // Applies P in between SEPs, beginning with SEP and ending with P zero-or-more times, extracting P.
  def startSepBy[a, b](p: Parser[a])(sep: Parser[b]): Parser[List[a]] = optional(startSepBy1(p)(sep))(Nil)

  // Applies P in between SEPs, beginning with SEP and ending with P one-or-more times, extracting P.
  def startSepBy1[a, b](p: Parser[a])(sep: Parser[b]): Parser[List[a]] = for {
    _ <- sep
    xs <- sepBy1(p)(sep)
  } yield xs
  
  // Matches ID at the start of the input.
  def identifier[a](id: String): Parser[String] = id match {
    case ""  => ignore("")
    case _    => for {
      x <- sat(_ == id.head)
      xs <- identifier(id.tail)
    } yield x.toString + xs
  }
  
  // Parses the whole of the input. 
  def consume: Parser[String] = str => Some((str, ""))
  
  /* Examples. */
  val letter: Parser[Char] = sat(_.isLetter)
  val word: Parser[List[Char]] = many1(letter)

  val digit: Parser[Char] = sat(_.isDigit)
  val twoDigit: Parser[String] = for {
    x <- digit
    y <- digit
  } yield x.toString + y.toString
}
