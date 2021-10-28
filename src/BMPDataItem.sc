import code.model.BMP.{BMPDataItem, EOF, EightBitsItem, MonoItem, Skipped, Uncoded}
import scodec.codecs.list

val eof = EOF()
println(s"the eof is encoded by EOF.codec as        :%s".format(EOF.codec.encode(eof).require.toHex))
println(s"the eof is encoded by BMPDataItem.codec as:%s".format(BMPDataItem.codec.encode(eof).require.toHex))
val uncoded = Uncoded(List(2,3,4))
print(s"the uncoded is encoded as:%s".format(Uncoded.codec.encode(uncoded).require.toHex))
val monoItem = MonoItem(1)
val eightBitsItem = EightBitsItem(20)

val codec = list(BMPDataItem.codec)

val dataItem = List(eof,uncoded,monoItem,eightBitsItem)
val dataItem_coded = codec.encode(dataItem).require
println(dataItem_coded.toHex)

val dataItem_decoded = codec.decode(dataItem_coded).require.value
dataItem_decoded.foreach(println)