import code.model.BMP.{BMPFile, BitCount, ColorPalette}
import scodec.bits.{BitVector, HexStringSyntax}

import java.io.{File, FileInputStream}


(9218000-2)%(48*4)
val bc = BitCount._16TrueColor
print(s"the toString() method is:%s".format(bc.toString()))

val colorTable = ColorPalette(10,20,30)
println(colorTable.toString)

val tt = hex"1234"

//val file = new File("/run/media/ruoyu/Doc/itellJworks/scodecBMPTest/src/assets/LAND2.BMP")
val file = new File("/run/media/ruoyu/Doc/itellJworks/scodecBMPTest/src/assets/LAND3.BMP")
//val file = new File("/run/media/ruoyu/Doc/itellJworks/scodecBMPTest/src/assets/BLK.BMP")
//val file = new File("/run/media/ruoyu/Doc/itellJworks/scodecBMPTest/src/assets/BLU.BMP")
//val file = new File("/run/media/ruoyu/Doc/itellJworks/scodecBMPTest/src/assets/GRN.BMP")
//val file = new File("/run/media/ruoyu/Doc/itellJworks/scodecBMPTest/src/assets/land.bmp")
//val file = new File("/run/media/ruoyu/Doc/itellJworks/scodecBMPTest/src/flag_b24.bmp")
//val file = new File("/run/media/ruoyu/Doc/itellJworks/scodecBMPTest/src/test3.bmp")
//val file = new File("D:\\itellJworks\\ScodecTest\\src\\test1.bmp")
println(s"the length of the file is:%d".format(file.length()))
val in = new FileInputStream(file)
val bytes = new Array[Byte](file.length().toInt)
in.read(bytes)
in.close()
val bitVector = BitVector(bytes)
//println(bitVector.take(112).toHex)
//println(s"the bitVector read from the bytesArray is:%d".format(bytes.take(10)))
//val bitVector = BitVector.fromInputStream(in,256)
val codec = BMPFile.codec
val decoded = codec.decode(bitVector).require
//println(decoded.value.header)
val fileHeader = decoded.value.header
//println(decoded.value.infoHeader)
//println(s"the offset of data in this BMPFileHeader is:%d".format(fileHeader.bfOffBits))
val infoHeader= decoded.value.infoHeader

val colorPalette = decoded.value.colorPalette
//println(s"the num of ColorPalette is:%d".format(colorPalette.get.length))
//colorPalette.foreach(_.foreach(println _))

val bitFieldMask = decoded.value.bitFieldMask
bitFieldMask.foreach(println _)

val data = decoded.value.data
println(s"the length of data is:%d".format(data.length))
data.grouped(infoHeader.biWidth.toInt).map(_.mkString("-")).foreach(println _)
//data.foreach(println _)
