package com.ellismichael.arnoldc

abstract class Test extends ArnoldC {
  val expectedOutput: String
  def test(): Unit
  def runTest() {
    println("---Running " + this.getClass.getSimpleName + "---")
    println("Expected output:\n>\t" + expectedOutput.replaceAll("\n", "\n>\t"))
    test()
    println("---End " + this.getClass.getSimpleName + "---")
  }

  def main(args: Array[String]): Unit = {
    // List all test classes to run here
    ModuloTest.runTest()
  }

}
// TODO: create run function

object ModuloTest extends Test {
  val expectedOutput = "1\n10\n0\n1"
  def test() {
    LISTEN TO ME CAREFULLY 'modulo
    I NEED YOUR CLOTHES YOUR BOOTS AND YOUR MOTOR CYCLE 'dividend
    I NEED YOUR CLOTHES YOUR BOOTS AND YOUR MOTOR CYCLE 'divisor
    GIVE THESE PEOPLE AIR;
    HEY CHRISTMASTREE 'quotient
    YOU SET US UP 0
    HEY CHRISTMASTREE 'remainder
    YOU SET US UP 0
    HEY CHRISTMASTREE 'product
    YOU SET US UP 0
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
    YOU SET US UP 0
    HEY CHRISTMASTREE 'result2
    YOU SET US UP 0
    HEY CHRISTMASTREE 'result3
    YOU SET US UP 0
    HEY CHRISTMASTREE 'result4
    YOU SET US UP 0
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