package code.model.BMP

import code.model.BMP.BitCount.BitCount
import code.model.BMP.Compression.Compression
import scodec.Codec
import scodec.codecs.{uint32, _}


case class BMPInfoHeader(biSize: Long,
                         biWidth: Long,
                         biHeight: Long,
                         biPlanes: Int,
                         biBitCount: BitCount,
                         biCompression: Compression,
                         biSizeImage: Long,
                         biXPelsPerMeter: Long,
                         biYPelsPerMeter: Long,
                         biClrUsed: Long,
                         biClrImportant: Long) {

  override val toString: String = s"BMPInfoHeader(biSize:%d,%nbiWidth:%d %nbiHeight:%d,%nbiBitCount:%s,%nbiCompression:%s,%nbiSizeImage:%d)".
    format(this.biSize,this.biWidth,this.biHeight,this.biBitCount.toString,this.biCompression.toString,this.biSizeImage)

  def hasColorTable: Boolean = this.biBitCount.id == 1 || this.biBitCount.id == 4 || this.biBitCount.id == 8

  def hasMask: Boolean = this.biCompression ==  Compression.MASK

  def numColorPalette: Int = this.biBitCount match {
    case BitCount._1Color => 2
    case BitCount._16Color => 16
    case BitCount._256Color => 256
    case _ => 0
  }
}

object BMPInfoHeader {

  implicit val codec: Codec[BMPInfoHeader] = {
    ("biSize" | uint32L) ::
      ("biWidth" | uint32L) ::
      ("biHeight" | uint32L) ::
      ("biPlanes" | uint16L) ::
      ("biBitCounts" | BitCount.codec) ::
      ("biCompression" | Compression.codec) ::
      ("biSizeImage" | uint32L) ::
      ("biXPelsPerMeter" | uint32L) ::
      ("biYPelsPerMeter" | uint32L) ::
      ("biClrUsed" | uint32L) ::
      ("biClrImportant" | uint32L)
  }.as[BMPInfoHeader]

}
