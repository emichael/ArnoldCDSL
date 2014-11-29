package com.ellismichael.arnoldc

import scala.collection.mutable
import scala.Left
import scala.Right
import scala.io.StdIn

class ArnoldC {
  // Arguments to most functions are either shorts or variables
  abstract sealed class ArnoldCValue
  case class Var(s: Symbol) extends ArnoldCValue
  case class Num(i: Short) extends ArnoldCValue
  implicit def symbolToArnoldCValue(s: Symbol): ArnoldCValue = Var(s)
  implicit def shortToArnoldCValue(i: Short): ArnoldCValue = Num(i)
  implicit def intToArnoldCValue(i: Int): ArnoldCValue = Num(i.toShort)
  def getVal(c: ArnoldCValue): Short = c match {
    case Var(s) => variables.get(s) match {
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

  case class FunctionCall(name: Symbol, args: List[ArnoldCValue], retVar: Symbol) extends Line
  case class Return(a: ArnoldCValue) extends Line

  case class ReadShort(s: Symbol) extends Line

  abstract case class Operation(a: ArnoldCValue, operator: Any) extends Line {
    def op(s: Short): Short
  }
  abstract class BinaryOperation(a: ArnoldCValue, operator: (Short, Short) => Int) extends Operation(a, operator) {
    def op(s: Short): Short = {
      // This is the correct order to make - and / work appropriately
      operator(s, getVal(a)).toShort
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
  class Greater(a: ArnoldCValue) extends BinaryRelation(a, (_ > _))

  abstract class BooleanRelation(a: ArnoldCValue, operator: (Boolean, Boolean) => Boolean) extends Operation(a, operator) {
    def op(s: Short): Short = {
      val ret = if (operator(getVal(a) != 0, s != 0)) 1 else 0
      return ret.toShort
    }
  }
  class Or(a: ArnoldCValue) extends BooleanRelation(a, (_ || _))
  class And(a: ArnoldCValue) extends BooleanRelation(a, (_ && _))

  // Runtime support classes
  class Bindings[T] {
    val vals = mutable.HashMap[Symbol, T]()
    def set(k: Symbol, v: T) = vals(k) = v
    def get(k: Symbol): Option[T] = vals.get(k)
  }

  class Function(var args: List[Symbol], val body: StatementSequence)

  class Scope[T] {
    val binds = mutable.Stack[Bindings[T]]()
    increaseScope()

    def increaseScope() = binds.push(new Bindings[T]())
    def decreaseScope() = binds.pop()
    def declare(s: Symbol, v: T) = binds.top.set(s, v)
    def set(s: Symbol, v: T): Unit = {
      for (stack <- binds) {
        stack.get(s) match {
          case Some(_) => {
            stack.set(s, v)
            return
          }
          case None => ()
        }
      }
      throw new Exception(s + " not yet declared.")
    }
    def get(s: Symbol): Option[T] = {
      for (stack <- binds) {
        stack.get(s) match {
          case value @ Some(v) => return value
          case None => ()
        }
      }
      return None
    }
  }

  // Runtime variables and functions
  val variables = new Scope[Short] // ArnoldC only supports 16bit signed integers
  val functions = new Bindings[Function] // all functions are in global scope
  val stack = new mutable.Stack[Short] // runtime stack
  var hasReturned = false

  def runLines(lines: StatementSequence) {
    val _lines = lines.clone()
    while (!_lines.isEmpty && !hasReturned) {
      runLine(_lines.dequeue())
    }
    // reset the returned flag to stop skipping lines
    hasReturned = false
  }

  def runLine(line: Line) {
    line match {
      case Print(p) => {
        p match {
          case Left(c) => println(getVal(c))
          case Right(s) => println(s)
        }
      }

      case DeclareVar(s, v) => {
        variables.declare(s, v)
      }

      case Assignment(s, init, ops) => {
        getVal(s) // Get to make sure not null
        stack.push(getVal(init))
        runLines(ops)
        variables.set(s, stack.pop())
      }

      case o @ Operation(_, _) => stack.push(o.op(stack.pop()))

      case If(c, thn, els) => {
        if (getVal(c) != 0) {
          runLines(thn)
        } else {
          runLines(els)
        }
      }

      case ReadShort(s) => {
        val v = StdIn.readShort()
        variables.set(s, v)
      }

      case While(c, seq) => {
        while (getVal(c) != 0) {
          runLines(seq)
        }
      }

      case FunctionCall(name, args, retSymbol) => {
        // First, get the function
        var func: Function = null
        functions.get(name) match {
          case Some(f) => func = f
          case None => throw new Exception("Function " + name + " undefined.")
        }
        // Then, setup the new bindings in a higher scope
        val newBindings: List[(Symbol, Short)] = func.args.reverse.zip(args.map(getVal))
        variables.increaseScope()
        for ((s, v) <- newBindings) {
          variables.declare(s, v)
        }
        // Run the function
        runLines(func.body)
        variables.decreaseScope()
        // If there is a return symbol, set it
        if (retSymbol != null) {
          variables.set(retSymbol, stack.pop())
        }
      }

      case Return(a: ArnoldCValue) => {
        stack.push(getVal(a))
        hasReturned = true
      }
    }
  }

  // Used to partially ensure correct syntax; this is a little bit messy
  abstract sealed class Keyword
  case class StartFunction() extends Keyword
  case class DeclareArgument() extends Keyword
  case class EndFunctionHeader() extends Keyword

  case class StartAssignment() extends Keyword
  case class InitAssignment() extends Keyword

  case class DeclareVariable() extends Keyword
  case class InitVariable() extends Keyword

  var expectedNextType: List[Keyword] = null
  var previousType: Keyword = null
  def logKeyword(k: Keyword) {
    if (expectedNextType != null && !expectedNextType.contains(k)) {
      throw new Exception("Expecting " + expectedNextType + " but got " + k);
    }

    var expectedPrevious: List[Keyword] = null
    expectedNextType = null

    k match {
      case DeclareVariable() => {
        expectedNextType = List(InitVariable())
      }
      case InitVariable() => {
        expectedPrevious = List(DeclareVariable())
      }
      case StartAssignment() => {
        expectedNextType = List(InitAssignment())
      }
      case InitAssignment() => {
        expectedPrevious = List(StartAssignment())
      }
      case DeclareArgument() => {
        expectedPrevious = List(StartFunction(), DeclareArgument())
        expectedNextType = List(DeclareArgument(), EndFunctionHeader())
      }
      case EndFunctionHeader() => {
        expectedPrevious = List(DeclareArgument())
      }
      case StartFunction() => {}
    }

    if (expectedPrevious != null && !expectedPrevious.contains(previousType)) {
      throw new Exception(k + " should follow " + expectedPrevious + " not " + previousType);
    }

    previousType = k
  }

  // Parsing functions and variables
  val pushDest = new mutable.Stack[StatementSequence]()
  def pushLine(l: Line) = pushDest.head.enqueue(l)

  val mainFunction: StatementSequence = new StatementSequence()
  def pStartMain = pushDest.push(mainFunction)
  def pEndMain() {
    pushDest.pop()
    runLines(mainFunction)
  }

  var currentFunction: Function = null
  def pStartFunctionDeclaraction(s: Symbol) {
    logKeyword(StartFunction())
    val body: StatementSequence = new StatementSequence()
    currentFunction = new Function(List(), body)
    pushDest.push(body)
    functions.set(s, currentFunction)
  }
  def pDeclareArgument(s: Symbol) {
    logKeyword(DeclareArgument())
    currentFunction.args = s :: currentFunction.args
  }
  def pEndFunctionHeader() {
    logKeyword(EndFunctionHeader())
  }
  def pEndFunctionDeclaration() {
    currentFunction = null
    pushDest.pop()
  }

  var returnSymbol: Symbol = null
  def pSetReturnVar(s: Symbol) {
    returnSymbol = s
  }
  def pFunctionCall(name: Symbol, args: Seq[ArnoldCValue]) {
    pushLine(new FunctionCall(name, args.toList, returnSymbol))
    returnSymbol = null
  }

  def pReturn(a: ArnoldCValue) = pushLine(new Return(a))

  var toAssign: Symbol = null
  def pStartAssignment(s: Symbol) = {
    logKeyword(StartAssignment())
    toAssign = s
  }
  def pInitAssignment(a: ArnoldCValue) {
    logKeyword(InitAssignment())
    val seq = new StatementSequence
    pushLine(Assignment(toAssign, a, seq))
    pushDest.push(seq)
  }
  def pEndAssignment = pushDest.pop()

  def pReadShort(s: Symbol) = pushLine(new ReadShort(s))

  var declareTemp: Symbol = null
  def pStartDeclare(s: Symbol) {
    logKeyword(DeclareVariable())
    declareTemp = s
  }
  def pEndDeclare(i: Short) = {
    logKeyword(InitVariable())
    pushLine(DeclareVar(declareTemp, i))
  }

  var ifStack = new mutable.Stack[If]
  def pStartIf(a: ArnoldCValue) {
    val thn = new StatementSequence()
    val els = new StatementSequence()
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

  def pPrint(p: Printable) = pushLine(Print(p))

  // All of the tokens are here; don't look at them unless you want to go insane
  class Token
  class BeenToken extends Token
  class UsToken extends Token
  class NotToken extends Token
  class ForToken extends Token
  class ToToken extends Token
  class TheToken extends Token
  class GoingToken extends Token
  class SayToken extends Token
  class MyToken extends Token
  class YourToken extends Token
  class AndToken extends Token
  class MotorToken extends Token
  class NoToken extends Token
  class SteamToken extends Token
  class AToken extends Token
  class AssToken extends Token
  class BackToken extends Token
  class PeopleToken extends Token
  class MeToken extends Token
  class VistaToken extends Token
  class AnsweredToken extends Token
  class WantToken extends Token
  class HaveToken extends Token
  class YouToken extends Token
  class YoureToken extends Token

  object ITS extends Token {
    def SHOWTIME = pStartMain
  }
  object YOU extends YouToken {
    def HAVE(t: BeenToken) = BEEN
    def HAVE(t: NoToken) = NO
    def SET(t: UsToken) = US
    def ARE(t: NotToken) = NOT
    def QUESTIONS(t: AndToken) = AND
  }
  object BEEN extends BeenToken {
    def TERMINATED = pEndMain
  }
  object TALK extends Token {
    def TO(t: TheToken) = THE
  }
  object THE extends TheToken {
    def HAND = pPrint _
    def CHOPPER = pStartAssignment _
  }
  object HEY extends Token {
    def CHRISTMASTREE = pStartDeclare _
  }
  object US extends UsToken {
    def UP = pEndDeclare _
  }
  object GET extends Token {
    def UP = pAdd _
    def DOWN = pMin _
    def TO(t: TheToken) = THE
    def YOUR(t: AssToken) = ASS
  }
  object YOURE extends YoureToken {
    def FIRED = pMul _
    def ME = pEql _
  }
  object HE extends Token {
    def HAD(t: ToToken) = TO
  }
  object TO extends ToToken {
    def SPLIT = pDiv _
    def ASK(t: YouToken) = YOU
  }
  object BECAUSE extends Token {
    def IM(t: GoingToken) = GOING
  }
  object GOING extends GoingToken {
    def TO(t: SayToken) = SAY
  }
  object SAY extends SayToken {
    def PLEASE = pStartIf _
  }
  def BULLSHIT = pElse
  object NO extends NoToken {
    def RESPECT(t: ForToken) = FOR
    def PROBLEMO = 1.toShort
  }
  object FOR extends ForToken {
    def LOGIC = pEndIf
  }
  object STICK extends Token {
    def AROUND = pStartWhile _
  }
  def CHILL = pEndWhile
  object HERE extends Token {
    def IS(t: MyToken) = MY
  }
  object MY extends MyToken {
    def INVITATION = pInitAssignment _
  }
  object ENOUGH extends Token {
    def TALK = pEndAssignment
  }
  object NOT extends NotToken {
    def YOU(t: YoureToken) = YOURE
  }
  object LETOFF extends Token {
    def SOME(t: SteamToken) = STEAM
  }
  object STEAM extends SteamToken {
    def BENNET = pGrt _
  }
  object CONSIDER extends Token {
    def THAT(t: AToken) = A
  }
  object A extends AToken {
    def DIVORCE = pOr _
  }
  object KNOCK extends Token {
    def KNOCK = pAnd _
  }
  object LISTEN extends Token {
    def TO(t: MeToken) = ME
  }
  object ME extends MeToken {
    def CAREFULLY = pStartFunctionDeclaraction _
  }
  object I extends Token {
    def NEED(t: YourToken) = YOUR
    def LIED = 0.toShort
    def WANT(t: ToToken) = TO
  }
  object YOUR extends YourToken {
    def CLOTHES(t: YourToken) = YOUR
    def BOOTS(t: AndToken) = AND
  }
  object AND extends AndToken {
    def YOUR(t: MotorToken) = MOTOR
    def I(t: WantToken) = WANT
  }
  object MOTOR extends MotorToken {
    def CYCLE = pDeclareArgument _
  }
  object GIVE extends Token {
    def THESE(t: PeopleToken) = PEOPLE
  }
  object PEOPLE extends PeopleToken {
    def AIR = pEndFunctionHeader
  }
  object ILL extends Token {
    def BEBACK = pReturn _
  }
  object HASTA extends Token {
    def LA(t: VistaToken) = VISTA
  }
  object VISTA extends VistaToken {
    def BABY = pEndFunctionDeclaration
  }
  object ASS extends AssToken {
    def TOMARS = pSetReturnVar _
  }
  object DOIT extends Token {
    def NOW(s: Symbol, vals: ArnoldCValue*) = pFunctionCall(s, vals)
  }
  object WANT extends WantToken {
    def TO(t: HaveToken) = HAVE
  }
  object HAVE extends HaveToken {
    def THEM(t: AnsweredToken) = ANSWERED
  }
  object ANSWERED extends AnsweredToken {
    def IMMEDIATELY = pReadShort _
  }
}
