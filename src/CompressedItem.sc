import code.model.BMP.{CompressedItem, EOL, Encoded}
import scodec.codecs.list

val eof = EOL()
val codec = CompressedItem.codec

val coded = codec.encode(eof).require
println(coded.toHex)

val codec2 = EOL.codec
val coded2 = codec2.encode(eof).require
println(coded2.toHex)

val decoded  = codec.decode(coded).require.value
//val decoded_2 = codec.decode(coded2).require.value

val decoded2 = codec2.decode(coded2).require.value


// list codec -------------------------------------------------------------

val listCodec = list(codec)

val encodedItem = Encoded(2,3)
val lst1 = List(eof,encodedItem)
val lst1_coded = listCodec.encode(lst1).require
val lst1_decoded = listCodec.decode(lst1_coded).require.value
lst1_decoded.foreach(println(_))
//------------------------------------