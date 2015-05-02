package com.xorinc.scalanbt

import java.io.ByteArrayOutputStream

import scala.reflect.{ClassTag, classTag}
import collection.mutable.{ ArrayBuffer, HashMap => MHMap }
import Numeric.{ FloatAsIfIntegral, DoubleAsIfIntegral }

package object tags {

  // this exists because the implicit resolver is dumb
  implicit val __fint: Integral[Float] = FloatAsIfIntegral
  implicit val __dint: Integral[Double] = DoubleAsIfIntegral

  import scala.collection.GenTraversableOnce

  sealed trait Tag {
    type This <: Tag

    def companion: TagCompanion
    def name: String = companion.name
    def id: Int = companion.id
    def mojangson: String
  }

  // this tag doesn't actually exist; Compound and List will discard it
  sealed trait TagEndLike extends Tag
  object TagEnd extends TagCompanion("TAG_END", 0) with TagEndLike {
    override def companion = this
    override val name: String = super.name
    override val id: Int = super.id
    override def mojangson = "null"
  }

  object Tag {

    def apply(x: Any): Tag = x match {
      case t: Tag => t
      case z: Boolean => TagByte(z)
      case b: Byte => TagByte(b)
      case h: Short => TagShort(h)
      case c: Char => TagString(c)
      case i: Int => TagInt(i)
      case l: Long => TagLong(l)
      case f: Float => TagFloat(f)
      case d: Double => TagDouble(d)
      case s: String => TagString(s)
      case s: Symbol => TagString(s)
      case a: Array[Byte] => TagByteArray(a)
      case a: Array[Int] => TagIntArray(a)
      case map: Map[String, Any] => new TagCompound(map.map { case (s, y) => (s, Tag(y)) }.toSeq)
      case tr: TraversableOnce[Any] => new TagList(tr.map(Tag(_)))
      case _ => TagEnd
    }
  }

  sealed abstract class TagCompanion(val name: String, val id: Int)

  implicit case object TagCompound extends TagCompanion("TAG_COMPOUND", 10) {
    def apply(xs: (String, Any)*): TagCompound = new TagCompound(xs.map { case (s, y) => (s, Tag(y)) })
    def unapplySeq(xs: TagCompound): Option[Seq[(String, Tag)]] = Some(xs.toSeq)
  }

  import language.dynamics
  final class TagCompound private (xs: MHMap[String, Tag]) extends Tag with Dynamic {

    type This = TagCompound
    
    def this(map: Seq[(String, Tag)]) = this(MHMap[String, Tag](map: _*))

    private val theMap = xs.filter(_._2 ne TagEnd)

    def toSeq: Seq[(String, Tag)] = theMap.toSeq
    def toMap: Map[String, Tag] = theMap.toMap

    def apply(s: String): Tag = theMap.getOrElse(s, TagEnd)
    def update(s: String, x: Any): Unit = {
      val t = Tag(x)
      if(t ne TagEnd) theMap(s) = t
    }

    def selectDynamic[T <: Tag : ClassTag](s: String): T = {
      val ct = classTag[T]
      ct.runtimeClass.cast(this(s)).asInstanceOf[T]
    }

    def updateDynamic(s: String)(x: Any): Unit = this(s) = x

    def foreach(f: ((String, Tag)) => Unit): Unit = theMap.foreach[Unit](f)

    def mapValues(f: Tag => Any): TagCompound = new TagCompound(theMap.mapValues[Tag](f.andThen(Tag(_))).toSeq)

    def filter(f: ((String, Tag)) => Boolean): TagCompound = new TagCompound(theMap.filter(f).toSeq)

    def filterKeys(f: String => Boolean): TagCompound = new TagCompound(theMap.filterKeys(f).toSeq)

    override def equals(o: Any): Boolean = o match {
      case null => false
      case that: TagCompound => this.theMap == that.theMap
      case _ => false
    }

    override def companion: TagCompanion = TagCompound

    override def toString: String = theMap.toSeq.map(t => s"${t._1}: ${t._2}").mkString("TagCompound(", ", ", ")")

    def toBytes: Array[Byte] = {
      val out = new ByteArrayOutputStream()
      io.writeNBT(out)(("", this))
      out.toByteArray
    }

    override def mojangson: String = theMap.toSeq.map(t => s"${t._1}:${t._2.mojangson}").mkString("{", ",", ",}")
  }

  implicit case object TagList extends TagCompanion("TAG_LIST", 9) {
    def apply(xs: Any*) = new TagList(xs.map(Tag(_)))
    def unapplySeq(xs: TagList): Option[Seq[Tag]] = Some(xs.toSeq)
  }

  // TagList is !not! covariant._
  final class TagList private (xs: ArrayBuffer[Tag]) extends Tag {

    type This = TagList

    def this(xs: TraversableOnce[Tag]) = this(ArrayBuffer[Tag](xs.toSeq: _*))

    private val theList = xs.filter(_ ne TagEnd)

    if(theList.length > 1){
      val clazz = theList(0).getClass
      if(!theList.forall(_.getClass eq clazz))
        throw new IllegalArgumentException("TagList can contain only one type of tag!")
    }

    def toSeq: Seq[Tag] = theList.toSeq
    def toArray(implicit ev: ClassTag[Tag]): Array[Tag] = theList.toArray

    def apply(i: Int): Tag = theList(i)
    def update(i: Int, x: Any): Unit = {
      val t = Tag(x)
      if(t ne TagEnd) theList(i) = t
    }

    def add(x: Any): this.type = {
      val t = Tag(x)
      if(t eq TagEnd) return this
      if(theList.length > 0){
        val clazz = theList(0).getClass
        if(clazz ne t.getClass)
          throw new IllegalArgumentException("TagList can contain only one type of tag!")
      }
      theList += t
      this
    }
    def + (x: Any): this.type = this.add(x)

    def remove(x: Any): this.type = {
      theList -= Tag(x)
      this
    }
    def - (x: Any): this.type = this.remove(x)

    def length: Int = theList.length

    def foreach(f: Tag => Unit): Unit = theList.foreach(f)

    def map(f: Tag => Any): TagList = new TagList(theList.map(f.andThen(Tag(_))).asInstanceOf[TraversableOnce[Tag]])

    def flatMap(f: Tag => GenTraversableOnce[Any]): TagList =
      new TagList(theList.flatMap(f).map(Tag(_)).asInstanceOf[TraversableOnce[Tag]])

    def filter(f: Tag => Boolean): TagList = new TagList(theList.filter(f))

    override def companion = TagList

    override def toString: String = theList.mkString("TagList(", ", ", ")")

    override def mojangson: String = theList.map(_.mojangson).mkString("[", ",", ",]")

    override def equals(o: Any): Boolean = o match {
      case null => false
      case that: TagList => this.theList == that.theList
      case _ => false
    }
  }

  sealed abstract class ValTag[A, Self <: ValTag[A, Self]](private val x: A) extends Tag with Serializable with Cloneable {
    self: Self =>

    type This = Self

    def get: A = x

    override def clone(): Self = companion(get)

    override def companion: ValTagCompanion[A, Self]

    override def toString: String = s"${getClass.getSimpleName}($x)"

    override def equals(o: Any): Boolean = o match {
      case null => false
      case that: ValTag[_, _] => this.x == that.x
      case _ => false
    }
  }

  sealed abstract class ValTagCompanion[A, Self <: ValTag[A, Self]](name: String, id: Int)
    extends TagCompanion(name, id) {

    def apply(x: A): Self

    def unapply(tag: Self): Option[A] = Some(tag.get)
  }

  sealed abstract class NumericValTag[A, Self <: NumericValTag[A, Self]](x: A)
    extends ValTag[A, Self](x) with Ordered[Self] { self: Self =>

    def +(y: Self): Self = companion.plus(this, y)
    def -(y: Self): Self = companion.minus(this, y)
    def *(y: Self): Self = companion.times(this, y)
    def /(y: Self): Self = companion.quot(this, y)
    def %(y: Self): Self = companion.rem(this, y)

    def unary_- : Self = companion.negate(this)
    def unary_+ : Self = this

    def toByte:    Byte    = companion.toInt(this).toByte
    def toShort:   Short   = companion.toInt(this).toShort
    def toChar:    Char    = companion.toInt(this).toChar
    def toInt:     Int     = companion.toInt(this)
    def toLong:    Long    = companion.toLong(this)
    def toFloat:   Float   = companion.toFloat(this)
    def toDouble:  Double  = companion.toDouble(this)
    def toBoolean: Boolean = companion.toInt(this) != 0

    def nbtByte:   TagByte   = TagByte(toByte)
    def nbtShort:  TagShort  = TagShort(toShort)
    def nbtInt:    TagInt    = TagInt(toInt)
    def nbtLong:   TagLong   = TagLong(toLong)
    def nbtFloat:  TagFloat  = TagFloat(toFloat)
    def nbtDouble: TagDouble = TagDouble(toDouble)

    def abs: Self = companion.abs(this)

    override def compare(that: Self): Int = companion.compare(this, that)

    override def companion: NumericValTagCompanion[A, Self]

  }

  sealed abstract class NumericValTagCompanion[A : Integral, Self <: NumericValTag[A, Self]](name: String, id: Int)
    extends ValTagCompanion[A, Self](name, id) with Integral[Self] {
    val ops: Integral[A] = implicitly[Integral[A]]

    def plus (x: Self, y: Self): Self = this(ops.plus (x.get, y.get))
    def minus(x: Self, y: Self): Self = this(ops.minus(x.get, y.get))
    def times(x: Self, y: Self): Self = this(ops.times(x.get, y.get))
    def quot (x: Self, y: Self): Self = this(ops.quot (x.get, y.get))
    def rem  (x: Self, y: Self): Self = this(ops.rem  (x.get, y.get))

    def negate(x: Self): Self = this(ops.negate(x.get))

    def toByte   (x: Self):    Byte   = ops.toInt(x.get).toByte
    def toShort  (x: Self):   Short   = ops.toInt(x.get).toShort
    def toInt    (x: Self):     Int   = ops.toInt(x.get)
    def toLong   (x: Self):    Long   = ops.toLong(x.get)
    def toFloat  (x: Self):   Float   = ops.toFloat(x.get)
    def toDouble (x: Self):  Double   = ops.toDouble(x.get)

    override def compare(x: Self, y: Self): Int = ops.compare(x.get, y.get)

    override def zero: Self = this(ops.zero)
    override def one: Self = this(ops.one)

    override def abs(x: Self): Self = this(ops.abs(x.get))
    override def signum(x: Self): Int = ops.signum(x.get)
  }

  sealed abstract class BitwiseValTag[A, Self <: BitwiseValTag[A, Self]](x: A)
    extends NumericValTag[A, Self](x) with Ordered[Self] { self: Self =>

    def &(y: Self): Self = companion.and(this, y)
    def |(y: Self): Self = companion.or (this, y)
    def ^(y: Self): Self = companion.xor(this, y)
    
    def unary_~ : Self = companion.not(this)
    
    def >> (i: Int): Self = companion.r2(this, i)
    def << (i: Int): Self = companion.l2(this, i)
    def >>>(i: Int): Self = companion.r3(this, i)

    override def companion: BitwiseValTagCompanion[A, Self]
  }

  sealed abstract class BitwiseValTagCompanion[A : Integral, Self <: NumericValTag[A, Self]](name: String, id: Int)
    extends NumericValTagCompanion[A, Self](name, id) {

    def and(x: Self, y: Self): Self
    def or (x: Self, y: Self): Self
    def xor(x: Self, y: Self): Self

    def not(x: Self): Self

    def r2(x: Self, i: Int): Self
    def l2(x: Self, i: Int): Self
    def r3(x: Self, i: Int): Self
  }

  sealed trait ArrayValTag[A, Self <: ValTag[Array[A], Self]] extends ValTag[Array[A], Self] {
    self: Self =>

    abstract override val get: Array[A] = super.get.clone()

    def apply(i: Int): A = super.get.apply(i)

    def length: Int = super.get.length

    def foreach(f: A => Unit): Unit = super.get.foreach(f)

    def map(f: A => A)(implicit ev: ClassTag[A]): Self = this.companion(super.get.map(f).toArray)

    def flatMap(f: A => GenTraversableOnce[A])(implicit ev: ClassTag[A]): Self = this.companion(super.get.flatMap(f).toArray)

    def filter(f: A => Boolean)(implicit ev: ClassTag[A]): Self = this.companion(super.get.filter(f).toArray)
  }

  sealed trait ArrayValTagCompanion[A, Self <: ValTag[Array[A], Self]] extends ValTagCompanion[Array[A], Self] {
    
    abstract override def apply(x: Array[A]): Self = super.apply(x.clone())
  }

  implicit class __toNBT[A](val x: A) extends AnyVal {
    def nbt[T <: ValTag[A, T]](implicit ev: ValTagCompanion[A, T]): T = ev(x)
  }

  implicit class __bool2NBT(val b: Boolean) extends AnyVal {
    def nbt: TagByte = TagByte(b)
  }

  implicit class __char2NBT(val c: Char) extends AnyVal {
    def nbt: TagString = TagString(c)
  }

  implicit class __sym2NBT(val s: Symbol) extends AnyVal {
    def nbt: TagString = TagString(s)
  }

  implicit class __toNBTList(val x: TraversableOnce[Any]) extends AnyVal {
    def nbt: TagList = Tag(x).asInstanceOf[TagList]
  }

  implicit case object TagByte extends BitwiseValTagCompanion[Byte, TagByte]("TAG_BYTE", 1) {
    def apply(x: Byte): TagByte = new TagByte(x)
    def apply(b: Boolean): TagByte = new TagByte(if(b) 1 else 0)

    def and(x: TagByte, y: TagByte): TagByte = this((x.get & y.get).toByte)
    def or (x: TagByte, y: TagByte): TagByte = this((x.get | y.get).toByte)
    def xor(x: TagByte, y: TagByte): TagByte = this((x.get ^ y.get).toByte)

    def not(x: TagByte): TagByte = this(x.get.unary_~.toByte)

    def r2(x: TagByte, i: Int): TagByte = this((x.get >> i).toByte)
    def l2(x: TagByte, i: Int): TagByte = this((x.get << i).toByte)
    def r3(x: TagByte, i: Int): TagByte = this((x.get >>> i).toByte)

    def fromInt(x: Int): TagByte = this(x.toByte)
  }

  final class TagByte(x: Byte)
    extends BitwiseValTag[Byte, TagByte](x) {
    override def companion = TagByte
    override def mojangson = x + "b"
  }

  implicit case object TagShort extends BitwiseValTagCompanion[Short, TagShort]("TAG_SHORT", 2) {
    def apply(x: Short): TagShort = new TagShort(x)

    def and(x: TagShort, y: TagShort): TagShort = this((x.get & y.get).toShort)
    def or (x: TagShort, y: TagShort): TagShort = this((x.get | y.get).toShort)
    def xor(x: TagShort, y: TagShort): TagShort = this((x.get ^ y.get).toShort)

    def not(x: TagShort): TagShort = this(x.get.unary_~.toShort)

    def r2(x: TagShort, i: Int): TagShort = this((x.get >> i).toShort)
    def l2(x: TagShort, i: Int): TagShort = this((x.get << i).toShort)
    def r3(x: TagShort, i: Int): TagShort = this((x.get >>> i).toShort)

    def fromInt(x: Int): TagShort = this(x.toShort)
  }

  final class TagShort(x: Short) extends BitwiseValTag[Short, TagShort](x) {
    override def companion = TagShort
    override def mojangson = x + "s"
  }

  implicit case object TagInt extends BitwiseValTagCompanion[Int, TagInt]("TAG_INT", 3) {
    def apply(x: Int): TagInt = new TagInt(x)

    def and(x: TagInt, y: TagInt): TagInt = this(x.get & y.get)
    def or (x: TagInt, y: TagInt): TagInt = this(x.get | y.get)
    def xor(x: TagInt, y: TagInt): TagInt = this(x.get ^ y.get)

    def not(x: TagInt): TagInt = this(x.get.unary_~)

    def r2(x: TagInt, i: Int): TagInt = this(x.get >> i)
    def l2(x: TagInt, i: Int): TagInt = this(x.get << i)
    def r3(x: TagInt, i: Int): TagInt = this(x.get >>> i)

    def fromInt(x: Int): TagInt = this(x)
  }

  final class TagInt(x: Int) extends BitwiseValTag[Int, TagInt](x) {
    override def companion = TagInt
    override def mojangson = x.toString
  }

  implicit case object TagLong extends BitwiseValTagCompanion[Long, TagLong]("TAG_LONG", 4) {
    def apply(x: Long): TagLong = new TagLong(x)

    def and(x: TagLong, y: TagLong): TagLong = this(x.get & y.get)
    def or (x: TagLong, y: TagLong): TagLong = this(x.get | y.get)
    def xor(x: TagLong, y: TagLong): TagLong = this(x.get ^ y.get)

    def not(x: TagLong): TagLong = this(x.get.unary_~)

    def r2(x: TagLong, i: Int): TagLong = this(x.get >> i)
    def l2(x: TagLong, i: Int): TagLong = this(x.get << i)
    def r3(x: TagLong, i: Int): TagLong = this(x.get >>> i)

    def fromInt(x: Int): TagLong = this(x)
  }

  final class TagLong(x: Long) extends BitwiseValTag[Long, TagLong](x) {
    override def companion = TagLong
    override def mojangson = x + "L"
  }

  implicit case object TagFloat extends NumericValTagCompanion[Float, TagFloat]("TAG_FLOAT", 5) {
    def apply(x: Float): TagFloat = new TagFloat(x)

    def fromInt(x: Int): TagFloat = this(x)
  }

  final class TagFloat(x: Float) extends NumericValTag[Float, TagFloat](x) {
    override def companion = TagFloat
    override def mojangson = x + "f"
  }

  implicit case object TagDouble extends NumericValTagCompanion[Double, TagDouble]("TAG_DOUBLE", 6) {
    def apply(x: Double): TagDouble = new TagDouble(x)

    def fromInt(x: Int): TagDouble = this(x)
  }

  final class TagDouble(x: Double) extends NumericValTag[Double, TagDouble](x) {
    override def companion = TagDouble
    override def mojangson = x + "d"
  }

  implicit case object TagString extends ValTagCompanion[String, TagString]("TAG_SRING", 8) {
    implicit def extractTagString(s: TagString): String = s.get
    def apply(x: String): TagString = new TagString(x)
    def apply(x: Char): TagString = new TagString(x.toString)
    def apply(x: Symbol): TagString = new TagString(x.toString)
  }

  final class TagString(x: String) extends ValTag[String, TagString](x) {
    override def companion = TagString
    override def mojangson = "\"" + x.replace("\"","\\\"") + "\""
  }

  implicit case object TagByteArray extends ValTagCompanion[Array[Byte], TagByteArray]("TAG_BYTE_ARRAY", 7) {
    def apply(x: Array[Byte]): TagByteArray = new TagByteArray(x.clone())
  }

  final class TagByteArray(x: Array[Byte]) extends ValTag[Array[Byte], TagByteArray](x)
    with ArrayValTag[Byte, TagByteArray] {
    override def companion = TagByteArray
    override def mojangson = s"[${x.length} bytes]"
  }

  implicit case object TagIntArray extends ValTagCompanion[Array[Int], TagIntArray]("TAG_INT_ARRAY", 11) {
    def apply(x: Array[Int]): TagIntArray = new TagIntArray(x.clone())
  }

  final class TagIntArray(x: Array[Int]) extends ValTag[Array[Int], TagIntArray](x)
    with ArrayValTag[Int, TagIntArray] {
    override def companion = TagIntArray
    override def mojangson = x.mkString("[", ",", ",]")
  }
}