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
  def hasColorTable: Boolean = this.infoHeader.biBitCount == 1 || this.infoHeader.biBitCount == 4 || this.infoHeader.biBitCount == 8

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
        case ((_, infoHeader), _) => { conditional(infoHeader.hasMask, bitFieldMaskCodec)}
      }.flattenLeftPairs.as[Headers]

      val headers: Headers = headingCodec.decode(bits).require.value

      val res: List[BMPDataItem] = headingCodec.decode(bits).map {
        case DecodeResult(Headers(_, infoHeader, _, _), remainder) if infoHeader.biBitCount == BitCount._1Color => {println(infoHeader);list(MonoItem.codec).decode(remainder)}
        case DecodeResult(Headers(_, infoHeader, _, _), remainder) if infoHeader.biBitCount == BitCount._16Color && infoHeader.biCompression == Compression.NoCompression => list(SixteenBitsItem.codec).decode(remainder)
        case DecodeResult(Headers(_, infoHeader, _, _), remainder) if infoHeader.biBitCount == BitCount._16Color && infoHeader.biCompression == Compression.RLE8 => list(RLECode.codec).decode(remainder)
        case DecodeResult(Headers(_, infoHeader, _, _), remainder) if infoHeader.biBitCount == BitCount._256Color && infoHeader.biCompression == Compression.NoCompression => list(EightBitsItem.codec).decode(remainder)
        case DecodeResult(Headers(_, infoHeader, _, _), remainder) if infoHeader.biBitCount == BitCount._256Color && infoHeader.biCompression == Compression.RLE4 => list(EightBitsItem.codec).decode(remainder)
        case DecodeResult(Headers(_, infoHeader, _, _), remainder) if infoHeader.biBitCount == BitCount._16TrueColor => list(SixteenBitsItem.codec).decode(remainder)
        case DecodeResult(Headers(_, infoHeader, _, _), remainder) if infoHeader.biBitCount == BitCount._24TrueColor => list(TwentyBitsItem.codec).decode(remainder)
        case DecodeResult(Headers(_, infoHeader, colorPalette, bitFieldMask), remainder) if infoHeader.biBitCount == BitCount._32TrueColor => {println(infoHeader);println(colorPalette);println(bitFieldMask);list(ThirtyBitsItem.codec).decode(remainder)}
      }.require.require.value


/*      val re: List[BitVector] = for {
        (headers,remainder) <- headingCodec.decode(bits)
      } yield headers*/

      val file = BMPFile(headers.header,headers.infoHeader,headers.colorPalette,headers.bitFieldMask,res)
      Attempt.successful(DecodeResult(file,BitVector.empty))
      //      val res = headingCodec.decode(bits)

      /*      val fileCodec: Codec[BMPFile] = headingCodec.flatZip {
              case (((_, infoHeader), _), _) if infoHeader.biBitCount == BitCount._1Color => list(MonoItem.codec)
              case (((_, infoHeader), _), _) if infoHeader.biBitCount == BitCount._16Color && infoHeader.biCompression == Compression.NoCompression => list(SixteenBitsItem.codec).as[List[BMPDataItem]]
              case (((_, infoHeader), _), _) if infoHeader.biBitCount == BitCount._16Color && infoHeader.biCompression == Compression.RLE8 => list(RLECode.codec)
              case (((_, infoHeader), _), _) if infoHeader.biBitCount == BitCount._256Color && infoHeader.biCompression == Compression.NoCompression => list(EightBitsItem.codec)
              case (((_, infoHeader), _), _) if infoHeader.biBitCount == BitCount._256Color && infoHeader.biCompression == Compression.RLE4 => list(EightBitsItem.codec)
              case (((_, infoHeader), _), _) if infoHeader.biBitCount == BitCount._16TrueColor => list(SixteenBitsItem.codec)
              case (((_, infoHeader), _), _) if infoHeader.biBitCount == BitCount._24TrueColor => list(TwentyBitsItem.codec)
              case (((_, infoHeader), _), _) if infoHeader.biBitCount == BitCount._32TrueColor => list(ThirtyBitsItem.codec)
            }.flattenLeftPairs.as[BMPFile]
            fileCodec.decode(bits)*/
    }
  }
}