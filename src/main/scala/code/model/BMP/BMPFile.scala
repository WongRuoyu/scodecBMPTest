package code.model.BMP

import scodec.bits.BitVector
import scodec.codecs.{conditional, limitedSizeBytes, list}
import scodec.{Attempt, Codec, DecodeResult, SizeBound}
import shapeless.HNil

case class BMPFile(header: BMPFileHeader,
                   infoHeader: BMPInfoHeader,
                   colorPalette: Option[List[ColorPalette]],
                   bitFieldMask: Option[BitFieldMask],
                   data: List[BMPDataItem]) {
  def hasColorTable: Boolean = this.infoHeader.biBitCount == BitCount._1Color || this.infoHeader.biBitCount == BitCount._16Color || this.infoHeader.biBitCount == BitCount._256Color

  //  override def toString: String = this.header.toString ++ "\\n" ++ this.infoHeader.toString ++ "\\n" ++ this.colorPalette.map(_.flatMap(_.toString)).toString ++ "\\n"
}

object BMPFile {

  //the bytes each ColorTable contains
  lazy val numBytesPerColorPalette: Int = 4

  private lazy val headerCodec = BMPFileHeader.codec
  private lazy val infoHeaderCodec = BMPInfoHeader.codec
  private lazy val colorPaletteCodec = list(ColorPalette.codec)
  private lazy val bitFieldMaskCodec = BitFieldMask.codec
  private lazy val dataCodec = list(BMPDataItem.codec)

  implicit val codec: Codec[BMPFile] = new Codec[BMPFile] {

    // the file header and info header together is 54 bytes at least
    override def sizeBound: SizeBound = SizeBound.atLeast(54 * 8)

    override def encode(file: BMPFile): Attempt[BitVector] = {

      val header = headerCodec.encode(file.header).require
      val infoHeader = infoHeaderCodec.encode(file.infoHeader).require
      val colorPalette: BitVector = file.colorPalette.map { list => colorPaletteCodec.encode(list).require }.getOrElse(BitVector.empty)
      val bitFieldMask: BitVector = file.bitFieldMask.map(bitFieldMaskCodec.encode(_).require).getOrElse(BitVector.empty)
      val data: BitVector = dataCodec.encode(file.data).require


      val bits = header ++ infoHeader ++ colorPalette ++ bitFieldMask ++ data
      Attempt.successful(bits)
    }

    override def decode(bits: BitVector): Attempt[DecodeResult[BMPFile]] = {

      case class Headers(header: BMPFileHeader,
                         infoHeader: BMPInfoHeader,
                         colorPalette: Option[List[ColorPalette]],
                         bitFieldMask: Option[BitFieldMask])

      val headingCodec = (headerCodec ~ infoHeaderCodec).flatZip {
        case (_, infoHeader: BMPInfoHeader) => conditional(infoHeader.hasColorTable, limitedSizeBytes(infoHeader.numColorPalette * numBytesPerColorPalette, list(ColorPalette.codec)))
      }.flatZip {
        case ((_, infoHeader), _) =>  conditional(infoHeader.hasMask, bitFieldMaskCodec)
      }.flattenLeftPairs.as[Headers]

      val headers: Headers = headingCodec.decode(bits).require.value

      val res: List[BMPDataItem] = headingCodec.decode(bits).map {
        case DecodeResult(Headers(_, infoHeader, _, _), remainder) if infoHeader.biBitCount == BitCount._1Color && infoHeader.biCompression == Compression.NoCompression => list(MonoItem.codec).decode(remainder)
        case DecodeResult(Headers(_, infoHeader, _, _), remainder) if infoHeader.biBitCount == BitCount._16Color && infoHeader.biCompression == Compression.NoCompression => list(RGBCode16.codec).decode(remainder)
        case DecodeResult(Headers(_, infoHeader, _, _), remainder) if infoHeader.biBitCount == BitCount._16Color && infoHeader.biCompression == Compression.RLE8 => list(RLECode.codec).decode(remainder)
        case DecodeResult(Headers(_, infoHeader, _, _), remainder) if infoHeader.biBitCount == BitCount._256Color && infoHeader.biCompression == Compression.NoCompression => list(EightBitsItem.codec).decode(remainder)
        case DecodeResult(Headers(_, infoHeader, _, _), remainder) if infoHeader.biBitCount == BitCount._256Color && infoHeader.biCompression == Compression.RLE4 => list(RLECode.codec).decode(remainder)
          // 16-bit data is always MASKed.
        case DecodeResult(Headers(_, infoHeader, _, _), remainder) if infoHeader.biBitCount == BitCount._16TrueColor => list(RGBCode16.codec).decode(remainder)
        // 24-bit data is always stored as (R,G,B)
        case DecodeResult(Headers(_, infoHeader,_,_), remainder) if infoHeader.biBitCount == BitCount._24TrueColor => list(TwentiesBitsColoredItem.codec).decode(remainder)
        case DecodeResult(Headers(_, infoHeader,_,_), remainder) if infoHeader.biBitCount == BitCount._32TrueColor && infoHeader.biCompression == Compression.NoCompression => list(ThirtiesBitsColoredItem.codec).decode(remainder)
        case DecodeResult(Headers(_, infoHeader,_,_), remainder) if infoHeader.biBitCount == BitCount._32TrueColor && infoHeader.biCompression == Compression.MASK => list(RGBCode32.codec).decode(remainder)
      }.require.require.value

      val file = BMPFile(headers.header,headers.infoHeader,headers.colorPalette,headers.bitFieldMask,res)
      Attempt.successful(DecodeResult(file,BitVector.empty))
   }
  }
}