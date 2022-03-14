package nazuki.assembly

enum Instruction {
  case Const(value: Int)
  case Dup
  case Get(index: Int)
  case Set(index: Int)
  case Drop
  case Not
  case And
  case Or
  case Xor
  case Shl
  case ShrU
  case ShrS
  case Inc
  case Add
  case Sub
  case Mul10
  case Mul
  case Eqz
  case Nez
  case Eq
  case LtS
  case LeS
  case LtU
  case LeU
  case GtS
  case GeS
  case GtU
  case GeU
  case Scan
  case Print
  case Getc
  case Putc
  case Write(text: String)
  case Jump(offset: Int)
  case Jz(offset: Int)
  case Jnz(offset: Int)
  case Jeq(offset: Int)
}
