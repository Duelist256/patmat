val list = List('a', 'b', 'a')
def times(list: List[Char]): List[(Char, Int)] = {
  def _times(list: List[Char], result: Map[Char, Int]): Map[Char, Int] = {
    list match {
      case x :: xs =>
        if (result.contains(x))
          _times(xs, result + (x -> (result(x) + 1)))
        else _times(xs, result + (x -> 1))
      case Nil => result
    }
  }
  _times(list, Map()).toList
}

times(list)

case class A(int: Int)