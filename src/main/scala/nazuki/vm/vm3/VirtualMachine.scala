package nazuki.vm
package vm3

import scala.collection.mutable

import nazuki.util._

class VirtualMachine
    extends Alpha
    with Beta
    with binary32.Stack
    with binary32.IO
    with binary32.Bitwise
    with binary32.Arithmetic
    with binary32.Comparison
    with binary32.Control {

  case class Instruction(id: Int)

  private var nextInstruction = 0

  private val instructionMap = mutable.Map.empty[Instruction, () => Unit]

  private var stackWidth = 33
  private var counterWidth = 1
  private var codeWidth = 1

  def createInstruction(): Instruction = {
    val instruction = Instruction(nextInstruction)
    nextInstruction += 1
    instruction
  }

  def register(operation: => Unit): Instruction = {
    val instruction = createInstruction()
    instructionMap.put(instruction, () => operation)
    instruction
  }

  def generate(instructions: List[Instruction]): String = {
    def getWidth(n: Int): Int = {
      var w = 0
      while (n > (1 << w)) w += 1
      w
    }

    counterWidth = getWidth(instructions.size) + 1

    codeWidth = getWidth(instructionMap.size) + 1

    val opcodeMap = Map.from(
      for (instruction <- instructionMap.keys)
        yield (instruction.id, instruction)
    )

    val t0 = alloc(0)
    val counter = alloc(1 until counterWidth)
    val t1 = alloc(counterWidth)
    val code = alloc(counterWidth + 1 until counterWidth + codeWidth)

    t0 += 1
    t0 {
      t0 -= 1
      def lookupCounter(i: Int, bits: Int = 0)(f: Int => Unit): Unit = {
        if (i >= 0) {
          branch(counter(i), t0) {
            lookupCounter(i - 1, bits | (1 << i))(f)
          } {
            lookupCounter(i - 1, bits)(f)
          }
        } else {
          f(bits)
        }
      }
      lookupCounter(counterWidth - 2) { bits =>
        for (instruction <- instructions.lift(bits)) {
          for (j <- 0 until (counterWidth - 1)) {
            if (instruction.id.testBit(j)) {
              code(j) += 1
            }
          }
        }
      }
      def lookupCode(i: Int, bits: Int = 0)(f: Int => Unit): Unit = {
        if (i >= 0) {
          branchOnce(code(i), t1) {
            lookupCode(i - 1, bits | (1 << i))(f)
          } {
            lookupCode(i - 1, bits)(f)
          }
        } else {
          f(bits)
        }
      }
      lookupCode(codeWidth - 2) { bits =>
        for (
          instruction <- opcodeMap.get(bits);
          operation <- instructionMap.get(instruction)
        ) {
          counterToStack()
          operation()
          stackToCounter()
        }
      }
      at(counter(0)) {
        raw("[>]+<[-<]>")
      }
      t1 {
        t1 -= 1
        t0 -= 1
      }
      t0 += 1
    }

    result
  }

  def counterToStack(): Unit = {
    forward(counterWidth)
    forward(codeWidth)
    forward(stackWidth)
    bfOpn()
    forward(stackWidth)
    bfCls()
  }

  def stackToCounter(): Unit = {
    backward(stackWidth)
    bfOpn()
    backward(stackWidth)
    bfCls()
    backward(codeWidth)
    backward(counterWidth)
  }

  def jump(address: Int): Unit = {
    stackToCounter()
    val counter = alloc(1 until counterWidth)
    for (i <- 0 until counterWidth - 1) {
      counter(i) := (address >> i) & 1
    }
    counterToStack()
  }

  def exit(): Unit = {
    stackToCounter()
    val counter = alloc(1 until counterWidth)
    for (i <- 0 until counterWidth - 1) {
      counter(i) := 1
    }
    counterToStack()
  }
}
