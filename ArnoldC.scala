import scala.collection.mutable
import sext._

class ArnoldC {
  // Arguments to most functions are either shorts or variables
  abstract sealed class ArnoldCValue
  case class Var(s: Symbol) extends ArnoldCValue
  case class Num(i: Short) extends ArnoldCValue
  implicit def symbolToArnoldCValue(s: Symbol): ArnoldCValue = Var(s)
  implicit def shortToArnoldCValue(i: Short): ArnoldCValue = Num(i)
  implicit def intToArnoldCValue(i: Int): ArnoldCValue = Num(i.toShort)
  def getVal(c: ArnoldCValue): Short = c match {
    case Var(s) => binds.get(s) match {
      case Some(i) => i
      case None => throw new Exception("Variable " + s + " undefined.")
    }
    case Num(i) => i
  }

  type Printable = Either[ArnoldCValue, String]
  implicit def symbolToPrintable(s: Symbol) = Left(Var(s))
  implicit def shortToPrintable(s: Short) = Left(Num(s))
  implicit def stringToPrintable(s: String) = Right(s)

  // Basic AST structure
  abstract sealed class Line
  type StatementSequence = mutable.Queue[Line]

  case class Print(s: Printable) extends Line
  case class DeclareVar(variable: Symbol, initialValue: Short) extends Line
  case class Assignment(variable: Symbol, init: ArnoldCValue, ops: StatementSequence) extends Line

  case class If(a: ArnoldCValue, thn: StatementSequence, els: StatementSequence) extends Line
  case class While(a: ArnoldCValue, seq: StatementSequence) extends Line

  abstract case class Operation(a: ArnoldCValue, operator: Any) extends Line {
    def op(s: Short): Short
  }
  abstract class BinaryOperation(a: ArnoldCValue, operator: (Short, Short) => Int) extends Operation(a, operator) {
    def op(s: Short): Short = {
      operator(getVal(a), s).toShort
    }
  }
  class Plus(a: ArnoldCValue) extends BinaryOperation(a, (_ + _))
  class Minus(a: ArnoldCValue) extends BinaryOperation(a, (_ - _))
  class Mult(a: ArnoldCValue) extends BinaryOperation(a, (_ * _))
  class Div(a: ArnoldCValue) extends BinaryOperation(a, (_ / _))
  abstract class BinaryRelation(a: ArnoldCValue, operator: (Short, Short) => Boolean) extends Operation(a, operator) {
    def op(s: Short): Short = {
      val ret = if (operator(s, getVal(a))) 1 else 0
      return ret.toShort
    }
  }
  class Equals(a: ArnoldCValue) extends BinaryRelation(a, (_ == _))
  // TODO: make sure right order for >
  class Greater(a: ArnoldCValue) extends BinaryRelation(a, (_ > _))
  abstract class BooleanRelation(a: ArnoldCValue, operator: (Boolean, Boolean) => Boolean) extends Operation(a, operator) {
    def op(s: Short): Short = {
      val ret = if (operator(getVal(a) != 0, s != 0)) 1 else 0
      return ret.toShort
    }
  }
  class Or(a: ArnoldCValue) extends BooleanRelation(a, (_ || _))
  class And(a: ArnoldCValue) extends BooleanRelation(a, (_ && _))

  // Runtime variables and functions
  class Bindings[T] {
    val vals = mutable.HashMap[Symbol, T]()
    def set(k: Symbol, v: T) = vals(k) = v
    def get(k: Symbol) = vals.get(k)
  }

  val binds = new Bindings[Short] // ArnoldC only supports 16bit signed integers
  val stack = new mutable.Stack[Short]

  def runLines(lines: StatementSequence) {
    val _lines = lines.clone()
    while (!_lines.isEmpty) {
      runLine(_lines.dequeue())
    }
  }

  def runLine(line: Line) {
    line match {
      case Print(p) => {
        p match {
          case Left(c) => println(getVal(c))
          case Right(s) => println(s)
        }
      }
      case DeclareVar(s, v) => binds.set(s, v)
      case Assignment(s, init, ops) => {
        stack.push(getVal(init))
        runLines(ops)
        binds.set(s, stack.pop())
      }
      case o @ Operation(_, _) => stack.push(o.op(stack.pop()))
      case If(c, thn, els) => {
        if (getVal(c) == 0) {
          runLines(thn)
        } else {
          runLines(els)
        }
      }
      case While(c, seq) => {
        while (getVal(c) != 0) {
          runLines(seq)
        }
      }
    }
  }

  // Parsing functions and variables
  val mainFunction: StatementSequence = new StatementSequence
  val pushDest = new mutable.Stack[StatementSequence]
  def pushLine(l: Line) = pushDest.head.enqueue(l)

  // Declaration patterns TODO: require these to follow each other

  def pStartMain = pushDest.push(mainFunction)
  def pEndMain() {
    println("Running...")
    println(mainFunction.treeString)
    pushDest.pop()
    runLines(mainFunction) // Run in reverse order
  }
  def pPrint(p: Printable) = pushLine(Print(p))

  var toAssign: Symbol = 'null
  def pStartAssignment(s: Symbol) = toAssign = s
  def pInitAssignment(a: ArnoldCValue) {
    val seq = new StatementSequence
    pushLine(Assignment(toAssign, a, seq))
    pushDest.push(seq)
  }
  def pEndAssignment = pushDest.pop()

  var declareTemp: Symbol = 'null // TODO: check for null
  def pStartDeclare(s: Symbol) = declareTemp = s
  def pEndDeclare(i: Short) = pushLine(DeclareVar(declareTemp, i))

  var ifStack = new mutable.Stack[If]
  def pStartIf(a: ArnoldCValue) {
    val thn = new StatementSequence
    val els = new StatementSequence
    val line = If(a, thn, els)
    ifStack.push(line)
    pushLine(line)
    pushDest.push(thn)
  }
  def pElse {
    val If(_, _, els) = ifStack.top
    pushDest.pop()
    pushDest.push(els)
  }
  def pEndIf {
    pushDest.pop()
    ifStack.pop()
  }

  def pStartWhile(a: ArnoldCValue) {
    val seq = new StatementSequence
    pushLine(While(a, seq))
    pushDest.push(seq)
  }
  def pEndWhile = pushDest.pop()

  def pAdd(a: ArnoldCValue) = pushLine(new Plus(a))
  def pMin(a: ArnoldCValue) = pushLine(new Minus(a))
  def pMul(a: ArnoldCValue) = pushLine(new Mult(a))
  def pDiv(a: ArnoldCValue) = pushLine(new Div(a))
  def pEql(a: ArnoldCValue) = pushLine(new Equals(a))
  def pGrt(a: ArnoldCValue) = pushLine(new Greater(a))
  def pOr(a: ArnoldCValue) = pushLine(new Or(a))
  def pAnd(a: ArnoldCValue) = pushLine(new And(a))

  // All of the tokens are here; don't look at them unless you want to go insane
  class Token

  object ITS extends Token {
    def SHOWTIME = pStartMain
  }
  object YOU extends Token {
    def HAVE(t: Token) = BEEN
    def SET(t: Token) = US
    def ARE(t: Token) = NOT
  }
  object BEEN extends Token {
    def TERMINATED = pEndMain
    // TODO: move this to NO
    def RESPECT(t: Token) = FOR
  }
  object TALK extends Token {
    def TO(t: Token) = THE
  }
  object THE extends Token {
    def HAND = pPrint _
    def CHOPPER = pStartAssignment _
  }
  object HEY extends Token {
    // TODO: make this two different objects somehow
    def CHRISTMASTREE = pStartDeclare _
  }
  object US extends Token {
    def UP = pEndDeclare _
  }
  object GET extends Token {
    def UP = pAdd _
    def DOWN = pMin _
    def TO(t: Token) = THE
  }
  object YOURE extends Token { // TODO: apostrophe
    def FIRED = pMul _
    def ME = pEql _
  }
  object HE extends Token {
    def HAD(t: Token) = TO // TODO: throw error otherwise
  }
  object TO extends Token {
    def SPLIT = pDiv _
  }
  object BECAUSE extends Token {
    def IM(t: Token) = GOING // TODO: error check, maybe by creating subclass
  }
  object GOING extends Token {
    def TO(t: Token) = SAY
  }
  object SAY extends Token {
    def PLEASE = pStartIf _
  }
  def BULLSHIT = pElse
  object NO extends Token
  object FOR extends Token {
    def LOGIC = pEndIf
  }
  object STICK extends Token {
    def AROUND = pStartWhile _
  }
  def CHILL = pEndWhile
  object HERE extends Token {
    def IS(t: Token) = MY
  }
  object MY extends Token {
    def INVITATION = pInitAssignment _
  }
  object ENOUGH extends Token {
    def TALK = pEndAssignment
  }
  object NOT extends Token {
    def YOU(t: Token) = YOURE
  }
  object LETOFF extends Token {
    def SOME(t: Token) = STEAM
  }
  object STEAM extends Token {
    def BENNET = pGrt _
  }
  object CONSIDER extends Token {
    def THAT(t: Token) = A
  }
  object A extends Token {
    def DIVORCE = pOr _
  }
  object KNOCK extends Token {
    def KNOCK = pAnd _
  }
}

object Test extends ArnoldC {
  def main(args: Array[String]): Unit = {
    ITS SHOWTIME;
    HEY CHRISTMASTREE 'limit
    YOU SET US UP 10
    HEY CHRISTMASTREE 'index
    YOU SET US UP 1
    HEY CHRISTMASTREE 'squared
    YOU SET US UP 1
    HEY CHRISTMASTREE 'loop
    YOU SET US UP 1
    STICK AROUND 'loop
    GET TO THE CHOPPER 'squared
    HERE IS MY INVITATION 'index
    YOURE FIRED 'index
    ENOUGH TALK;
    TALK TO THE HAND 'squared
    GET TO THE CHOPPER 'loop
    HERE IS MY INVITATION 'limit
    LETOFF SOME STEAM BENNET 'index
    ENOUGH TALK;
    GET TO THE CHOPPER 'index
    HERE IS MY INVITATION 'index
    GET UP 1
    ENOUGH TALK;
    CHILL;
    YOU HAVE BEEN TERMINATED;
  }
}
