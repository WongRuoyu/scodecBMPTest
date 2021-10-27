package code.bmp

import code.model.BMP._
import org.specs2.mutable._
import scodec.bits.{BitVector, HexStringSyntax}

class BmpTest extends Specification {
  "BMPFileHeaderTest should" >> {
    val fileHeader = BMPFileHeader("BM", 20, 40)
    val codec = BMPFileHeader.codec
    val encoded = codec.encode(fileHeader).require
    val decoded = codec.decode(encoded).require
    "be able to encode BMPFileHeader correctly" >> {
      encoded.length mustNotEqual (0)
    }
    "be able to decode BMPFileHeader correctly" >> {
      decoded.value mustEqual (fileHeader)
    }
    "be able to decode raw bits correctly" >> {
      val bits = BitVector.fromHex("424d760500000000000036040000")
      val decoded2 = bits.map(codec.decode _)
      println(decoded2.get.require.value)
      decoded2.isEmpty.mustEqual(false)
    }
  }
  "BMPInfoHeaderTest should" >> {
    val infoHeader = BMPInfoHeader(20L, 20L, 20L, 10, BitCount._1Color, Compression.NoCompression, 256L, 100L, 150L, 0L, 10L)
    val codec = BMPInfoHeader.codec
    val encoded = codec.encode(infoHeader).require
    val decoded = codec.decode(encoded).require
    "be able to encode BMPInfoHeader correctly" >> {
      encoded.length.mustNotEqual(0)
    }
    "be able to decode BMPInfoHeader correctly" >> {
      decoded.value.must_===(infoHeader)
    }
    "be able to decode raw bitVector correctly" >> {
      val bits = BitVector.fromHex("28000000110000001000000001000800000000004001000000000000000000000000000000000000")
      val decoded2 = bits.map(codec.decode(_))
      val infoHeader = BMPInfoHeader(40, 17, 16, 1, BitCount._256Color, Compression.NoCompression, 320, 0, 0, 0, 0)
      decoded2.get.require.value.must_===(infoHeader)
    }
  }
  "BitCountTest should" >> {
    val bitCount = BitCount._16TrueColor
    val codec = BitCount.codec
    val encoded = codec.encode(bitCount).require
    val decoded = codec.decode(encoded).require
    "be able print the string correctly" >> {
      decoded.value.must_===(bitCount)
      bitCount.toString.must_===("_16TrueColor")
    }
    "be able to encode BitCount value correctly" >> {
      encoded.toHex.mustEqual("1000")
    }
    "be able to decode BitCount value correctly" >> {
      decoded.value.must_===(bitCount)
    }
  }
  "BitCountTest should" >> {
    val bitCount = BitCount._16TrueColor
    val codec = BitCount.codec
    val encoded = codec.encode(bitCount).require
    val decoded = codec.decode(encoded).require
    "be able to encode BitCount value correctly" >> {
      encoded.toHex.mustEqual("1000")
    }
    "be able to decode BitCount value correctly" >> {
      decoded.value.must_===(bitCount)
    }
  }
  "CompressionTest should" >> {
    val compression = Compression.RLE4
    val codec = Compression.codec
    val encoded = codec.encode(compression).require
    val decoded = codec.decode(encoded).require
    "be able to encode Compression correctly" >> {
      encoded.toHex.mustEqual("02000000")
    }
    "be able to decode Compression correctly" >> {
      decoded.value.mustEqual(compression)
    }
  }
  "ColorTableTest should" >> {
    val table = ColorPalette(10, 20, 30)
    val codec = ColorPalette.codec
    val encoded = codec.encode(table).require
    val decoded = codec.decode(encoded).require.value
    "be able to encode ColorTable correctly" >> {
      encoded.length.mustNotEqual(0)
    }
    "be able to decode ColorTable correctly" >> {
      decoded.mustEqual(table)
    }
  }
  "RLETest should" >> {
    "the Encoded case class Test" >> {
      "the codec should" >> {
        val encoded = Encoded(2, 1)
        val codec = Encoded.codec
        val encoded_encoded = codec.encode(encoded).require
        "encode the Encoded entity correctly" >> {
          encoded_encoded.toHex.must_===("0201")
        }
        "decode the Encoded entity correctly" >> {
          codec.decode(hex"020101".bits).require.value.must_===(encoded)
        }
      }
    }
    "the Uncoded case class Test" >> {
      "the codec should" >> {
        val unCoded = Uncoded(List(1, 2, 3))
        val codec = Uncoded.codec
        val unCoded_encoded = codec.encode(unCoded).require
        "encode the entity correctly" >> {
          unCoded_encoded.toHex.must_===("000301020300")
        }
        "decode the entity correctly" >> {
          codec.decode(hex"000301020300".bits).require.value.must_===(unCoded)
        }
      }
    }
    "the Skipped case class Test" >> {
      "the codec should" >> {
        val skipped = Skipped(5, 1)
        val codec = Skipped.codec
        val encoded = codec.encode(skipped).require
        val expected = hex"00020501".bits
        "encode the entity correctly" >> {
          codec.encode(skipped).require.must_===(expected)
        }
        "decode the entity correctly" >> {
          codec.decode(expected).require.value.must_===(skipped)
        }
      }
    }
    "the EOL case class Test" >> {
      "the codec should" >> {
        val eol = EOL()
        val codec = EOL.codec
        val expected = hex"0000".bits
        "encode the entity corrrectly" >> {
          codec.encode(eol).require.must_===(expected)
        }
        "decode the entity corrrectly" >> {
          codec.decode(expected).require.value.must_===(eol)
        }
      }
    }
    "the EOF case class Test" >> {
      "the codec should" >> {
        val eof = EOF()
        val codec = EOF.codec
        val expected = hex"0001".bits
        "encode the entity corrrectly" >> {
          codec.encode(eof).require.must_===(expected)
        }
        "decode the entity corrrectly" >> {
          codec.decode(expected).require.value.must_===(eof)
        }
      }
    }
  }
}
