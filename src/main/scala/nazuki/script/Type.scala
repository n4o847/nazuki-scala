package nazuki.script

enum Type {
  case Unit
  case Int
  case Tuple(items: List[Type])
}
