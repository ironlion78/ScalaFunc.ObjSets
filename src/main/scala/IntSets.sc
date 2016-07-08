val j = 1+2
val s = new NonEmpty(3, Empty, Empty)
val t = new NonEmpty(4, Empty, Empty)
t.incl(3).union(Empty)
Empty.union(Empty).incl(5)
Empty.incl(3).union(Empty)
t.union(s.incl(5).incl(6))
s.union(t)

abstract class IntSet {
  def incl(x: Int): IntSet
  def contains(x: Int): Boolean
  def union(other: IntSet): IntSet
  def toString: String
}

object Empty extends IntSet {
  def incl(x: Int): IntSet = new NonEmpty(x, Empty, Empty)
  def contains(x: Int): Boolean = false
  def union(other: IntSet): IntSet = other
  override def toString = "."
}

class NonEmpty (elem: Int, left: IntSet, right: IntSet) extends IntSet {
  def incl(x: Int): IntSet = {
    if (x < elem) new NonEmpty(elem, left.incl(x), right)
    else if (x > elem) new NonEmpty(elem, left, right.incl(x))
    else this
  }
  def contains(x: Int): Boolean = {
    if (x < elem) left.contains(x)
    else if (x > elem) right.contains(x)
    else true
  }
  def union(other: IntSet): IntSet = {
    other.union(left).union(right).incl(elem)
  }
  override def toString = "{" + left + elem + right + "}"
}

def nth[T] (n: Int, list: List[T]): T = {
  if (list.isEmpty) throw new IndexOutOfBoundsException
  else if (n == 0) list.head
  else nth(n - 1, list.tail)
}