package nazuki.assembly

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

enum Labeled[A] {
  case L0(item: A)
  case L1(item: Int => A, label: String)
  case Label(label: String)
}

enum ResolutionType {
  case Relative
  case Absolute
}

def resolveLabels(
    labeledInstructions: Seq[Labeled[Instruction]],
    resolutionType: ResolutionType
): Tuple2[List[Instruction], Map[String, Int]] = {
  import Labeled._
  var index = 0
  val labelToIndex = mutable.Map.empty[String, Int]
  var instructions = ArrayBuffer.empty[Instruction]
  for (instruction <- labeledInstructions) {
    instruction match {
      case Label(label) =>
        labelToIndex.put(label, index)
      case _ =>
        index += 1
    }
  }
  resolutionType match {
    case ResolutionType.Relative =>
      for ((instruction, index) <- labeledInstructions.zipWithIndex) {
        instruction match {
          case L0(instruction) =>
            instructions += instruction
          case L1(instruction, label) =>
            instructions += instruction(labelToIndex(label) - index - 1)
          case Label(_) =>
        }
      }
    case ResolutionType.Absolute =>
      for (instruction <- labeledInstructions) {
        instruction match {
          case L0(instruction) =>
            instructions += instruction
          case L1(instruction, label) =>
            instructions += instruction(labelToIndex(label))
          case Label(_) =>
        }
      }
  }
  (instructions.toList, labelToIndex.toMap)
}
