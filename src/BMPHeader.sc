import code.model.BMP.{BMPFileHeader, BMPInfoHeader}
import scodec.bits.BitVector

import java.io.{File, FileInputStream}

val file = new File("/run/media/ruoyu/Doc/itellJworks/scodecBMPTest/src/test1.bmp")
//val file = new File("D:\\itellJworks\\ScodecTest\\src\\test1.bmp")
println(s"the length of the file is:%d".format(file.length()))
val in = new FileInputStream(file)
val bytes = new Array[Byte](file.length().toInt)
in.read(bytes)
in.close()
val bitVector = BitVector(bytes)

val headerCodec = BMPFileHeader.codec

val header = headerCodec.decode(bitVector).require
println(header.value)



// info header
val infoHeaderCodec = BMPInfoHeader.codec
val infoHeader = infoHeaderCodec.decode(header.remainder).require
println(infoHeader.value)

// colorPalatte

//val colorPalette = Co