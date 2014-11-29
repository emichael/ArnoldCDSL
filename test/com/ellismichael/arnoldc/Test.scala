package com.ellismichael.arnoldc

object Test extends ArnoldC {
  def main(args: Array[String]): Unit = {
    // List all test classes to run here
    ModuloTest.runTest()
    DivisibleTest.runTest()
    GreaterTest.runTest()
    //    ReadingTest.runTest()
    FizzBuzzTest.runTest()
    FibTest.runTest()
  }
}

abstract class TestArnoldProgram extends ArnoldC {
  val expectedOutput: String
  def test(): Unit
  def runTest() {
    println("---Running " + this.getClass.getSimpleName + "---")
    println("Expected output:\n>\t" + expectedOutput.replaceAll("\n", "\n>\t"))
    test()
    println("---End " + this.getClass.getSimpleName + "---")
  }
}

object ModuloTest extends TestArnoldProgram {
  val expectedOutput = "1\n10\n0\n1"
  def test() {
    LISTEN TO ME CAREFULLY 'modulo
    I NEED YOUR CLOTHES YOUR BOOTS AND YOUR MOTOR CYCLE 'dividend
    I NEED YOUR CLOTHES YOUR BOOTS AND YOUR MOTOR CYCLE 'divisor
    GIVE THESE PEOPLE AIR;
    HEY CHRISTMASTREE 'quotient
    YOU SET US UP (I LIED)
    HEY CHRISTMASTREE 'remainder
    YOU SET US UP (I LIED)
    HEY CHRISTMASTREE 'product
    YOU SET US UP (I LIED)
    GET TO THE CHOPPER 'quotient
    HERE IS MY INVITATION 'dividend
    HE HAD TO SPLIT 'divisor
    ENOUGH TALK;
    GET TO THE CHOPPER 'product
    HERE IS MY INVITATION 'divisor
    YOURE FIRED 'quotient
    ENOUGH TALK;
    GET TO THE CHOPPER 'remainder
    HERE IS MY INVITATION 'dividend
    GET DOWN 'product
    ENOUGH TALK;
    ILL BEBACK 'remainder
    HASTA LA VISTA BABY;

    ITS SHOWTIME;
    HEY CHRISTMASTREE 'result1
    YOU SET US UP (I LIED)
    HEY CHRISTMASTREE 'result2
    YOU SET US UP (I LIED)
    HEY CHRISTMASTREE 'result3
    YOU SET US UP (I LIED)
    HEY CHRISTMASTREE 'result4
    YOU SET US UP (I LIED)
    GET YOUR ASS TOMARS 'result1
    DOIT NOW ('modulo, 9, 4)
    TALK TO THE HAND 'result1
    GET YOUR ASS TOMARS 'result2
    DOIT NOW ('modulo, 4795, 87)
    TALK TO THE HAND 'result2
    GET YOUR ASS TOMARS 'result3
    DOIT NOW ('modulo, 3978, 221)
    TALK TO THE HAND 'result3
    GET YOUR ASS TOMARS 'result4
    DOIT NOW ('modulo, 15, 2)
    TALK TO THE HAND 'result4
    YOU HAVE BEEN TERMINATED;
  }
}

object DivisibleTest extends TestArnoldProgram {
  val expectedOutput = "1\n1\n0\n0"

  def test() {
    LISTEN TO ME CAREFULLY 'modulo
    I NEED YOUR CLOTHES YOUR BOOTS AND YOUR MOTOR CYCLE 'dividend
    I NEED YOUR CLOTHES YOUR BOOTS AND YOUR MOTOR CYCLE 'divisor
    GIVE THESE PEOPLE AIR;
    HEY CHRISTMASTREE 'quotient
    YOU SET US UP (I LIED)
    HEY CHRISTMASTREE 'remainder
    YOU SET US UP (I LIED)
    HEY CHRISTMASTREE 'product
    YOU SET US UP (I LIED)
    GET TO THE CHOPPER 'quotient
    HERE IS MY INVITATION 'dividend
    HE HAD TO SPLIT 'divisor
    ENOUGH TALK;
    GET TO THE CHOPPER 'product
    HERE IS MY INVITATION 'divisor
    YOURE FIRED 'quotient
    ENOUGH TALK;
    GET TO THE CHOPPER 'remainder
    HERE IS MY INVITATION 'dividend
    GET DOWN 'product
    ENOUGH TALK;
    ILL BEBACK 'remainder
    HASTA LA VISTA BABY;

    LISTEN TO ME CAREFULLY 'divisible
    I NEED YOUR CLOTHES YOUR BOOTS AND YOUR MOTOR CYCLE 'dividend
    I NEED YOUR CLOTHES YOUR BOOTS AND YOUR MOTOR CYCLE 'divisor
    GIVE THESE PEOPLE AIR;

    HEY CHRISTMASTREE 'result
    YOU SET US UP 0
    GET YOUR ASS TOMARS 'result
    DOIT NOW ('modulo, 'dividend, 'divisor)

    HEY CHRISTMASTREE 'isZero
    YOU SET US UP 0
    GET TO THE CHOPPER 'isZero
    HERE IS MY INVITATION 'result
    YOU ARE NOT YOU YOURE ME 0
    ENOUGH TALK;

    ILL BEBACK 'isZero
    HASTA LA VISTA BABY;

    ITS SHOWTIME;
    HEY CHRISTMASTREE 'result1
    YOU SET US UP (I LIED)
    HEY CHRISTMASTREE 'result2
    YOU SET US UP (I LIED)
    HEY CHRISTMASTREE 'result3
    YOU SET US UP (I LIED)
    HEY CHRISTMASTREE 'result4
    YOU SET US UP (I LIED)
    GET YOUR ASS TOMARS 'result1
    DOIT NOW ('divisible, 5, 1)
    TALK TO THE HAND 'result1
    GET YOUR ASS TOMARS 'result2
    DOIT NOW ('divisible, 49, 7)
    TALK TO THE HAND 'result2
    GET YOUR ASS TOMARS 'result3
    DOIT NOW ('divisible, 15, 2)
    TALK TO THE HAND 'result3
    GET YOUR ASS TOMARS 'result4
    DOIT NOW ('divisible, 20, 19)
    TALK TO THE HAND 'result4
    YOU HAVE BEEN TERMINATED;

  }
}

object GreaterTest extends TestArnoldProgram {
  val expectedOutput = "0"
  def test() {
    ITS SHOWTIME;
    HEY CHRISTMASTREE 'test
    YOU SET US UP (NO PROBLEMO)
    GET TO THE CHOPPER 'test
    HERE IS MY INVITATION 'test
    LETOFF SOME STEAM BENNET 2
    ENOUGH TALK;
    TALK TO THE HAND 'test
    YOU HAVE BEEN TERMINATED;
  }
}

object ReadingTest extends TestArnoldProgram {
  val expectedOutput = "Whatever you input"
  def test() {
    ITS SHOWTIME;
    HEY CHRISTMASTREE 'test
    YOU SET US UP (NO PROBLEMO)
    I WANT TO ASK YOU QUESTIONS AND I WANT TO HAVE THEM ANSWERED IMMEDIATELY 'test
    TALK TO THE HAND 'test
    YOU HAVE BEEN TERMINATED;
  }
}

object FizzBuzzTest extends TestArnoldProgram {
  val expectedOutput = "1\n2\nFizz\n4\nBuzz\nFizz\n7\n8\nFizz\nBuzz"
  def test() {
    LISTEN TO ME CAREFULLY 'modulo
    I NEED YOUR CLOTHES YOUR BOOTS AND YOUR MOTOR CYCLE 'dividend
    I NEED YOUR CLOTHES YOUR BOOTS AND YOUR MOTOR CYCLE 'divisor
    GIVE THESE PEOPLE AIR;
    HEY CHRISTMASTREE 'quotient
    YOU SET US UP (I LIED)
    HEY CHRISTMASTREE 'remainder
    YOU SET US UP (I LIED)
    HEY CHRISTMASTREE 'product
    YOU SET US UP (I LIED)
    GET TO THE CHOPPER 'quotient
    HERE IS MY INVITATION 'dividend
    HE HAD TO SPLIT 'divisor
    ENOUGH TALK;
    GET TO THE CHOPPER 'product
    HERE IS MY INVITATION 'divisor
    YOURE FIRED 'quotient
    ENOUGH TALK;
    GET TO THE CHOPPER 'remainder
    HERE IS MY INVITATION 'dividend
    GET DOWN 'product
    ENOUGH TALK;
    ILL BEBACK 'remainder
    HASTA LA VISTA BABY;

    LISTEN TO ME CAREFULLY 'divisible
    I NEED YOUR CLOTHES YOUR BOOTS AND YOUR MOTOR CYCLE 'dividend
    I NEED YOUR CLOTHES YOUR BOOTS AND YOUR MOTOR CYCLE 'divisor
    GIVE THESE PEOPLE AIR;
    HEY CHRISTMASTREE 'result
    YOU SET US UP 0
    GET YOUR ASS TOMARS 'result
    DOIT NOW ('modulo, 'dividend, 'divisor)
    HEY CHRISTMASTREE 'isZero
    YOU SET US UP 0
    GET TO THE CHOPPER 'isZero
    HERE IS MY INVITATION 'result
    YOU ARE NOT YOU YOURE ME 0
    ENOUGH TALK;
    ILL BEBACK 'isZero
    HASTA LA VISTA BABY;

    ITS SHOWTIME;

    HEY CHRISTMASTREE 'isLessThan10
    YOU SET US UP (NO PROBLEMO)
    HEY CHRISTMASTREE 'n
    YOU SET US UP 0
    HEY CHRISTMASTREE 'multiple
    YOU SET US UP (NO PROBLEMO)

    STICK AROUND 'isLessThan10
    GET TO THE CHOPPER 'n
    HERE IS MY INVITATION 'n
    GET UP 1
    ENOUGH TALK

    GET TO THE CHOPPER 'isLessThan10
    HERE IS MY INVITATION 10
    LETOFF SOME STEAM BENNET 'n
    ENOUGH TALK

    GET YOUR ASS TOMARS 'multiple
    DOIT NOW ('divisible, 'n, 15)
    BECAUSE IM GOING TO SAY PLEASE 'multiple
    TALK TO THE HAND "FizzBuzz"
    BULLSHIT;
    GET YOUR ASS TOMARS 'multiple
    DOIT NOW ('divisible, 'n, 3)
    BECAUSE IM GOING TO SAY PLEASE 'multiple
    TALK TO THE HAND "Fizz"
    BULLSHIT;
    GET YOUR ASS TOMARS 'multiple
    DOIT NOW ('divisible, 'n, 5)
    BECAUSE IM GOING TO SAY PLEASE 'multiple
    TALK TO THE HAND "Buzz"
    BULLSHIT
    TALK TO THE HAND 'n
    YOU HAVE NO RESPECT FOR LOGIC;
    YOU HAVE NO RESPECT FOR LOGIC;
    YOU HAVE NO RESPECT FOR LOGIC;
    CHILL;

    YOU HAVE BEEN TERMINATED;

  }
}

object FibTest extends TestArnoldProgram {
  val expectedOutput = "1\n1\n\n2\n3\n5\n8\n13\n21\n34\n55\n"
  def test() {
    LISTEN TO ME CAREFULLY 'fib
    I NEED YOUR CLOTHES YOUR BOOTS AND YOUR MOTOR CYCLE 'n
    GIVE THESE PEOPLE AIR;

    HEY CHRISTMASTREE 'equals1
    YOU SET US UP (I LIED)
    GET TO THE CHOPPER 'equals1
    HERE IS MY INVITATION 1
    YOU ARE NOT YOU YOURE ME 'n
    ENOUGH TALK;

    HEY CHRISTMASTREE 'equals1or2
    YOU SET US UP (I LIED)
    GET TO THE CHOPPER 'equals1or2
    HERE IS MY INVITATION 2
    YOU ARE NOT YOU YOURE ME 'n
    CONSIDER THAT A DIVORCE 'equals1
    ENOUGH TALK;

    BECAUSE IM GOING TO SAY PLEASE 'equals1or2
    ILL BEBACK 1
    BULLSHIT;

    HEY CHRISTMASTREE 'nm1
    YOU SET US UP (I LIED)
    GET TO THE CHOPPER 'nm1
    HERE IS MY INVITATION 'n
    GET DOWN 1
    ENOUGH TALK;

    HEY CHRISTMASTREE 'nm2
    YOU SET US UP (I LIED)
    GET TO THE CHOPPER 'nm2
    HERE IS MY INVITATION 'nm1
    GET DOWN 1
    ENOUGH TALK;

    HEY CHRISTMASTREE 'fib1
    YOU SET US UP (I LIED)
    GET YOUR ASS TOMARS 'fib1
    DOIT NOW ('fib, 'nm1)

    HEY CHRISTMASTREE 'fib2
    YOU SET US UP (I LIED)
    GET YOUR ASS TOMARS 'fib2
    DOIT NOW ('fib, 'nm2)

    HEY CHRISTMASTREE 'sum
    YOU SET US UP (I LIED)
    GET TO THE CHOPPER 'sum
    HERE IS MY INVITATION 'fib1
    GET UP 'fib2
    ENOUGH TALK;

    ILL BEBACK 'sum

    YOU HAVE NO RESPECT FOR LOGIC;
    HASTA LA VISTA BABY;

    ITS SHOWTIME;

    HEY CHRISTMASTREE 'isLessThan10
    YOU SET US UP (NO PROBLEMO)
    HEY CHRISTMASTREE 'n
    YOU SET US UP (I LIED)
    HEY CHRISTMASTREE 'value
    YOU SET US UP (I LIED)

    STICK AROUND 'isLessThan10
    GET TO THE CHOPPER 'n
    HERE IS MY INVITATION 'n
    GET UP 1
    ENOUGH TALK

    GET TO THE CHOPPER 'isLessThan10
    HERE IS MY INVITATION 10
    LETOFF SOME STEAM BENNET 'n
    ENOUGH TALK

    GET YOUR ASS TOMARS 'value
    DOIT NOW ('fib, 'n)
    TALK TO THE HAND 'value
    CHILL;

    YOU HAVE BEEN TERMINATED;
  }
}
