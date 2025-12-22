package Server

object ParserCombinator {
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

  // Failure parser -> always fails
  private def fail[a]: Parser[a] = _ => None

  // Parses the next character in the input
  private def item: Parser[Char] = str =>
    if (str.isEmpty) None
    else Some((str.head, str.tail))

  // Returns a parser that matches on the predicate
  def sat(p: Char => Boolean): Parser[Char] = for {
    q <- item
    if p(q)
  } yield q
  
  def optional[a](p: Parser[a])(default: a): Parser[a] = p + ignore(default)

  // Applies parser p zero or more times.
  def many[a](p: Parser[a]): Parser[List[a]] = optional(many1(p))(Nil)

  // Applies parser p one or more times.
  def many1[a](p: Parser[a]): Parser[List[a]] = for {
    x <- p
    xs <- many(p)
  } yield x :: xs

  // Zero-or-more applications of P, where Ps are separated by SEP
  def sepBy[a, b](p: Parser[a])(sep: Parser[b]): Parser[List[a]] = optional(sepBy1(p)(sep))(Nil)

  // One-or-more applications of P, where P's pattern is separated by SEP pattern
  def sepBy1[a, b](p: Parser[a])(sep: Parser[b]): Parser[List[a]] = for {
    x <- p
    xs <- many(for {
      _ <- sep
      y <- p
    } yield y)
  } yield x :: xs

  // Zero-or-more applications of P, where Ps are separated by SEP, beginning with a separator.
  def startSepBy[a, b](p: Parser[a])(sep: Parser[b]): Parser[List[a]] = optional(startSepBy1(p)(sep))(Nil)

  // One-or-more applications of P, where P's pattern is separated by SEP pattern
  def startSepBy1[a, b](p: Parser[a])(sep: Parser[b]): Parser[List[a]] = for {
    _ <- sep
    xs <- sepBy1(p)(sep)
  } yield xs
  
  def identifier[a](id: String): Parser[String] = id match {
    case ""  => ignore("")
    case _    => for {
      x <- sat(_ == id.head)
      xs <- identifier(id.tail)
    } yield x.toString + xs
  }
  
  /* Examples. */
  val letter: Parser[Char] = sat(_.isLetter)
  val word: Parser[List[Char]] = many1(letter)

  val digit: Parser[Char] = sat(_.isDigit)
  val twoDigit: Parser[String] = for {
    x <- digit
    y <- digit
  } yield x.toString + y.toString
}
