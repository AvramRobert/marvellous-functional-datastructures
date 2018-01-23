import scala.annotation.tailrec

object Vector {

  def apply[A](as: A*): Vec[A] = as.foldLeft(empty[A])(_ append _)
  def empty[A]: Vec[A] = Vec(new Array[AnyRef](32), 0, 0)

  // It's faster to keep the tree as an Array[AnyRef] and cast, instead of defining an ADT
  // You can also just keep track of the `shift`, instead of keeping the height and always multiplying it with 5
  case class Vec[A](tree: Array[AnyRef], size: Int, height: Int) {

    def makeNode: Array[AnyRef] = new Array[AnyRef](32)
    def asA(a: AnyRef): A = a.asInstanceOf[A]
    def asRef(a: A): AnyRef = a.asInstanceOf[AnyRef]
    def asArray(a: AnyRef): Array[AnyRef] = a.asInstanceOf[Array[AnyRef]]

    private def path(a: A, lheight: Int = height): Array[AnyRef] = {
      val newNode = makeNode
      if (lheight == 0) newNode.update(0, asRef(a))
      else newNode.update(0, path(a, lheight - 1))
      newNode
    }

    private def push(a: A, idx: Int, node: Array[AnyRef] = tree, lheight: Int = height): Array[AnyRef] = {
      val i = (idx >>> (lheight * 5)) & 31
      if (node == null) path(a, lheight)
      else {
        val newNode = node.clone()
        if (lheight == 0) newNode.update(i, asRef(a))
        else newNode.update(i, push(a, idx, asArray(node(i)), lheight - 1))
        newNode
      }
    }

    @tailrec
    private def lookup(idx: Int, node: Array[AnyRef] = tree, lheight: Int = height): A = {
      val i = (idx >>> (lheight * 5)) & 31
      if (lheight == 0) asA(node(i))
      else lookup(idx, asArray(node(i)), lheight - 1)
    }

    private def isDense: Boolean = Math.pow(32, height+1) == size // optimisation: (size >>> 5) >= (1 << (height * 5))

    def append(a: A): Vec[A] = if (isDense) {
      val newTree = makeNode
      newTree.update(0, tree)
      newTree.update(1, path(a))
      copy(tree = newTree, size = size + 1, height = height + 1)
    } else {
      copy(tree = push(a, size), size = size + 1)
    }

    def apply(i: Int): Option[A] = {
      if (i >= size || i < 0) None
      else Some(lookup(i))
    }

    // Current implementation is O(nlog32n). Can be made O(n)
    def foldLeft[B](b: B)(f: (B, A) => B): B = (0 until size).foldLeft(b) { (nb, i) => f(nb, lookup(i)) }

    override def toString: String =
      if (size == 0) "Vector()"
      else foldLeft("Vector(") { (x, y) => s"$x$y, " }.dropRight(2) + ")"
  }
}