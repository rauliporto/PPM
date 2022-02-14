package qtree

trait RandomWithState {
  def nextInt(value: Int): (Int, RandomWithState)
}
