package alloysimulation

import java.awt.Color
import java.awt.image.BufferedImage

import java.io.DataOutputStream
import java.io.File
import java.io.FileOutputStream

import javax.imageio.ImageIO

object RGBBitmap {
  val NUM_COLORS = 3

  def writeToPNG(alloy: Alloy, filepath: String): Unit = {
    val image = new BufferedImage(alloy.getWidth(), alloy.getHeight(),
      BufferedImage.TYPE_INT_RGB)

    for(
      x <- 0 until alloy.getWidth();
      y <- 0 until alloy.getHeight();
      c = Math.min(alloy(x, y, 0).toInt, 255)
    ){
      image.setRGB(x, y, (new Color(c, 0, 0)).getRGB())
    }

    ImageIO.write(image, "png", new File(filepath))
  }

  def writeToPPM(alloy: Alloy, filepath: String): Unit = {
    val out = new DataOutputStream(new FileOutputStream(filepath))

    out.writeBytes("P6\u000a%d %d\u000a%d\u000a".format(
      alloy.getWidth(), alloy.getHeight(), 255))

    for(
      x <- 0 until alloy.getWidth();
      y <- 0 until alloy.getHeight();
      c = Math.min(alloy(x, y, 0).toInt, 255)
    ){
       out.writeByte(c)
       out.writeByte(0)
       out.writeByte(0)
    }
  }
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
