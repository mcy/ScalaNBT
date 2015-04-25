package com.xorinc.scalanbt

import java.io.{Console => _, _}
import tags._
import java.util.zip.{GZIPOutputStream, GZIPInputStream}

package object io {

  // TODO: support compression

  val UTF_8 = java.nio.charset.Charset.forName("UTF-8")

  def readNBT(in: DataInput): (String, TagCompound) = {
    import in._
    def readStr(): String = {
      val len = readShort().toInt
      val bytes = Array.ofDim[Byte](len)
      readFully(bytes)
      new String(bytes, UTF_8)
    }

    def readByteArray(): TagByteArray = {
      val len = readInt()
      val bytes = Array.ofDim[Byte](len)
      readFully(bytes)
      TagByteArray(bytes)
    }

    def readIntArray(): TagIntArray = {
      val len = readInt()
      val ints = Array.ofDim[Int](len)
      for(i <- 0 until len) ints(i) = readInt()
      TagIntArray(ints)
    }

    def parseCompound(): TagCompound = {
      val comp = TagCompound()
      var id: Byte = 0
      do {
        id = readByte()
        if(id != 0) {
          val name = readStr()
          val value =
            id match {
              case 1 => readByte().nbt
              case 2 => readShort().nbt
              case 3 => readInt().nbt
              case 4 => readLong().nbt
              case 5 => readFloat().nbt
              case 6 => readDouble().nbt
              case 7 => readByteArray()
              case 8 => readStr().nbt
              case 9 => parseList()
              case 10 => parseCompound()
              case 11 => readIntArray()
            }
          comp(name) = value
        }
      } while (id != 0)
      comp
    }
    def parseList(): TagList = {
      val id = readByte()
        val parseFunc: () => Tag =
          id match {
            case 0 => () => null
            case 1 => () => readByte().nbt
            case 2 => () => readShort().nbt
            case 3 => () => readInt().nbt
            case 4 => () => readLong().nbt
            case 5 => () => readFloat().nbt
            case 6 => () => readDouble().nbt
            case 7 => () => readByteArray()
            case 8 => () => readStr().nbt
            case 9 => () => parseList()
            case 10 => () => parseCompound()
            case 11 => () => readIntArray()
          }
        val len = readInt()
        val list = TagList((for(_ <- 0 until len) yield parseFunc()): _*)
        if(id == 0) TagList() else list
    }
    readByte()
    (readStr(), parseCompound())
  }

  def writeNBT(out: DataOutput)(tag: (String, TagCompound)) = {
    import out._

    def writeTag(s: String, t: Tag): Unit = {
      if(s ne null) {
        writeByte(t.id.toByte)
        writeStr(s)
      }
      t.asInstanceOf[Object] match {
        case TagByte(b) => writeByte(b)
        case TagShort(s) => writeShort(s)
        case TagInt(i) => writeInt(i)
        case TagLong(l) => writeLong(l)
        case TagFloat(f) => writeFloat(f)
        case TagDouble(d) => writeDouble(d)
        case TagString(s) => writeStr(s)
        case TagByteArray(a) => writeByteArray(a)
        case TagIntArray(a) => writeIntArray(a)
        case c: TagCompound => writeCompound(c)
        case l: TagList => writeList(l)
      }
    }

    def writeStr(s: String): Unit = {
      val len = s.length.toShort
      writeShort(len)
      val chars = s.getBytes(UTF_8)
      out.write(chars)

    }

    def writeByteArray(b: Array[Byte]): Unit = {
      val len = b.length
      writeInt(len)
      out.write(b)
    }

    def writeIntArray(b: Array[Int]): Unit = {
      val len = b.length
      writeInt(len)
      for(i <- b) writeInt(i)
    }

    def writeCompound(c: TagCompound): Unit = {
      for((s: String, t: Tag) <- c.toMap){
        writeTag(s, t)
      }
      writeByte(0)
    }
    def writeList(l: TagList): Unit = {
      val len = l.length
      if(len == 0) {
        writeByte(0)
        writeInt(len)
      } else {
        writeByte(l(0).id.toByte)
        writeInt(len)
        for(t <- l)
          writeTag(null, t)
      }
    }

    writeByte(10)
    writeStr(tag._1)
    writeCompound(tag._2)
  }

  def readNBT(in: InputStream, deflate: Boolean = false): (String, TagCompound) =
    readNBT(new DataInputStream(if (deflate) new GZIPInputStream(in) else in).asInstanceOf[DataInput])
  def writeNBT(out: OutputStream, inflate: Boolean = false)(tag: (String, TagCompound)): Unit =
    writeNBT(new DataOutputStream(if (inflate) new GZIPOutputStream(out) else out).asInstanceOf[DataOutput])(tag)

  implicit class __bytearray(val a: Array[Byte]) extends AnyVal {
    def parseNBT: (String, TagCompound) = {
      val in = new ByteArrayInputStream(a)
      readNBT(in)
    }

    def toCompound: TagCompound = parseNBT._2
  }

  case class MojangsonParseException(message: String) extends RuntimeException(message)

}
