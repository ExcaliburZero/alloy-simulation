package alloysimulation

import java.io.DataOutputStream
import java.io.FileOutputStream

object RGBBitmap {
  val NUM_COLORS = 3
}

class RGBBitmap(width: Int, height: Int) {
  private val colors = Array.ofDim[Int](width, height, RGBBitmap.NUM_COLORS)

  def updateColor(w: Int, h: Int, red: Int, blue: Int, green: Int): Unit = {
    val entry = colors(w)(h)
    entry.update(0, red)
    entry.update(1, blue)
    entry.update(2, green)
  }

  def writeToPPM(filepath: String): Unit = {
    val out = new DataOutputStream(new FileOutputStream(filepath))

    out.writeBytes("P6\u000a%d %d\u000a%d\u000a".format(width, height, 255))

    for(
      x <- 0 until width;
      y <- 0 until height;
      c = colors(x)(y)
    ){
       out.writeByte(c(0))
       out.writeByte(c(1))
       out.writeByte(c(2))
    }
  }
}
