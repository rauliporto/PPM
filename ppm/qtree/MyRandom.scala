package qtree

case class MyRandom(seed: Long) extends RandomWithState {

  def nextInt(value: Int): (Int, RandomWithState) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRandom = MyRandom(newSeed)
    val n = (newSeed >>> 16).toInt
    if (value > 0)
      (1, nextRandom)
    else
      (0, nextRandom)
  }

}
