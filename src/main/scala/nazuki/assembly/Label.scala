package nazuki.assembly

enum Labeled[A] {
  case L0(item: A)
  case L1(item: Int => A, label: String)
  case Label(label: String)
}
