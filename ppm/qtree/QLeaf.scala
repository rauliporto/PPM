package qtree

case class QLeaf[A, B](value: B) extends QTree[A]
