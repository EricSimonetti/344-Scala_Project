import scala.util.parsing.combinator._

abstract class Tree
case class Alternation(l: Tree, r: Tree) extends Tree
case class Concatenation(l: Tree, r: Tree) extends Tree
case class Option(o: Tree) extends Tree
case class Str(n: String) extends Tree

class Combinators extends JavaTokenParsers{
  override def skipWhitespace() = false
  // S  -: E$
  // E  -: T '|' E | T
  // T  -: F T | F
  // F  -: A '?' | A
  // A  -: C | '(' E ')'
  def e: Parser[Tree] = t ~ "|" ~ e ^^ { case l ~ _ ~ r => Alternation(l, r) } | t
  def t: Parser[Tree] = f ~ t ^^ { case l ~ r => Concatenation(l, r) } | f
  def f: Parser[Tree] = a ~ "?" ^^ { case o ~ "?" => Option(o) } | a
  def a: Parser[Tree] = "(" ~ e ~ ")" ^^ {case "(" ~ p ~ ")" => p} | c
  def c: Parser[Tree] =  "[A-Za-z0-9/.\\s]".r ^^ { str => Str(str) }
}

object Main extends Combinators {

  def eval(t: Tree, s: String): String = t match {
    case Alternation(l,r) =>
      if(eval(l,s).contains("+"))
        eval(r,s)
      else
        eval(l,s)

    case Concatenation(l, r) =>
      if (eval(l, s).contains("+"))
        "+"
      else
        eval(r, eval(l, s))

    case Option(o) =>
      if(eval(o, s).contains("+"))
        s
      else
        eval(o,s)

    case Str(n) if s.startsWith(n)|n.equals(".") => s.substring(1)
    case Str(n) if !s.startsWith(n) => "+"
  }

  def main(args: Array[String]){
    val input = scala.io.StdIn.readLine("Pattern?")
    val exp:Tree = parseAll(e, input).get
    println(exp)
    while(!input.equals("q")){
      val input = scala.io.StdIn.readLine("String?")
      println(
        if(eval(exp, input).equals("")) "Match"
        else "No match"
      )
    }
  }
}
