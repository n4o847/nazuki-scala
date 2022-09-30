package nazuki.vm
package vm1

import scala.collection.mutable
import scala.math

import nazuki.util._

/** 使い方:
  * ```
  * val vm = new VirtualMachine
  * val scan = vm.register { vm.doScan(signed = true) }
  * val add = vm.register { vm.doAdd() }
  * val print = vm.register { vm.doPrint(signed = true) }
  * val code = vm.generate(List(scan, scan, add, print))
  * ```
  */
class VirtualMachine
    extends Alpha
    with Beta
    with binary32.Stack
    with binary32.IO
    with binary32.Arithmetic {

  case class Instruction(id: Int)

  private var nextInstruction = 0

  private val instructionMap = mutable.Map.empty[Instruction, () => Unit]

  private var stackWidth = 33
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
    codeWidth = getWidth(instructions.size) + 1

    val opcodeMap = Map.from(
      for (instruction <- instructionMap.keys)
        yield (instruction.id, instruction)
    )

    val tmp = alloc(0)
    val cmd = alloc(1 until codeWidth)
    bfDec()
    forward(codeWidth)
    for (instruction <- instructions.reverse) {
      for (i <- 0 until codeWidth - 1) {
        if (instruction.id.testBit(i)) {
          cmd(i) += 1
        }
      }
      forward(codeWidth)
    }
    backward(codeWidth)
    bfInc()
    tmp {
      tmp -= 1
      def seeBit(i: Int, bits: Int): Unit = {
        if (i >= 0) {
          branchMut(cmd(i), tmp) {
            seeBit(i - 1, bits | (1 << i))
          } {
            seeBit(i - 1, bits)
          }
        } else {
          for (
            instruction <- opcodeMap.get(bits);
            operation <- instructionMap.get(instruction)
          ) {
            codeToStack()
            operation()
            stackToCode()
          }
        }
      }
      seeBit(codeWidth - 2, 0)
      tmp += 1
      backward(codeWidth)
      tmp += 1
    }

    result
  }

  def codeToStack(): Unit = {
    forward(codeWidth)
    bfOpn()
    forward(codeWidth)
    bfCls()
    forward(stackWidth)
    bfOpn()
    forward(stackWidth)
    bfCls()
  }

  def stackToCode(): Unit = {
    backward(stackWidth)
    bfOpn()
    backward(stackWidth)
    bfCls()
    backward(codeWidth)
    bfOpn()
    backward(codeWidth)
    bfCls()
  }
}
