package alloysimulation

import java.awt.Color
import java.awt.image.BufferedImage

import java.io.DataOutputStream
import java.io.File
import java.io.FileOutputStream

import javax.imageio.ImageIO

object RGBBitmap {
  val NUM_COLORS = 3

  def writeToPNG(alloy: Alloy, filepath: String, maxTemp: Double): Unit = {
    val image = new BufferedImage(alloy.getWidth(), alloy.getHeight(),
      BufferedImage.TYPE_INT_RGB)

    for(
      x <- 0 until alloy.getWidth();
      y <- 0 until alloy.getHeight();
      c = Math.min((alloy(x, y, 0) / maxTemp * 255).toInt, 255)
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
