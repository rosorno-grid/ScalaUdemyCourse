package lectures.part1as

object DarkSugar {
  //Methods w/single param
  def singleArgMethod(x: Int): String = s"$x little ducks"

  val descrption = singleArgMethod {
    val x = 5
    val y = 6
    y + x
  }
  println(descrption)


}
