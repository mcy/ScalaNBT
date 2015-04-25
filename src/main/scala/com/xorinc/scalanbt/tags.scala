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

    private val ops: Integral[A] = companion.numeric

    def +(y: Self): Self = companion(ops.plus(x, y.get))
    def -(y: Self): Self = companion(ops.minus(x, y.get))
    def *(y: Self): Self = companion(ops.times(x, y.get))
    def /(y: Self): Self = companion(ops.quot(x, y.get))
    def %(y: Self): Self = companion(ops.rem(x, y.get))

    def unary_- : Self = companion(ops.negate(x))
    def unary_+ : Self = this

    def toByte:   Byte   = ops.toInt(x).toByte
    def toShort:  Short  = ops.toInt(x).toShort
    def toChar:   Char   = ops.toInt(x).toChar
    def toInt:    Int    = ops.toInt(x)
    def toLong:   Long   = ops.toLong(x)
    def toFloat:  Float  = ops.toFloat(x)
    def toDouble: Double = ops.toDouble(x)

    def nbtByte:   TagByte   = TagByte(ops.toInt(x).toByte)
    def nbtShort:  TagShort  = TagShort(ops.toInt(x).toShort)
    def nbtInt:    TagInt    = TagInt(ops.toInt(x))
    def nbtLong:   TagLong   = TagLong(ops.toLong(x))
    def nbtFloat:  TagFloat  = TagFloat(ops.toFloat(x))
    def nbtDouble: TagDouble = TagDouble(ops.toDouble(x))

    def abs: Self = companion(ops.abs(x))

    override def compare(that: Self): Int = ops.compare(x, that.get)

    override def companion: NumericValTagCompanion[A, Self]

  }

  sealed abstract class NumericValTagCompanion[A : Integral, Self <: NumericValTag[A, Self]](name: String, id: Int)
    extends ValTagCompanion[A, Self](name, id){
    val numeric: Integral[A] = implicitly[Integral[A]]
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

  implicit case object TagByte extends NumericValTagCompanion[Byte, TagByte]("TAG_BYTE", 1) {
    def apply(x: Byte): TagByte = new TagByte(x)
    def apply(b: Boolean): TagByte = new TagByte(if(b) 1 else 0)
  }

  final class TagByte(x: Byte)
    extends NumericValTag[Byte, TagByte](x) {
    override def companion = TagByte
    override def mojangson = x + "b"
  }

  implicit case object TagShort extends NumericValTagCompanion[Short, TagShort]("TAG_SHORT", 2) {
    def apply(x: Short): TagShort = new TagShort(x)
  }

  final class TagShort(x: Short) extends NumericValTag[Short, TagShort](x) {
    override def companion = TagShort
    override def mojangson = x + "s"
  }

  implicit case object TagInt extends NumericValTagCompanion[Int, TagInt]("TAG_INT", 3) {
    def apply(x: Int): TagInt = new TagInt(x)
  }

  final class TagInt(x: Int) extends NumericValTag[Int, TagInt](x) {
    override def companion = TagInt
    override def mojangson = x.toString
  }

  implicit case object TagLong extends NumericValTagCompanion[Long, TagLong]("TAG_LONG", 4) {
    def apply(x: Long): TagLong = new TagLong(x)
  }

  final class TagLong(x: Long) extends NumericValTag[Long, TagLong](x) {
    override def companion = TagLong
    override def mojangson = x + "L"
  }

  implicit case object TagFloat extends NumericValTagCompanion[Float, TagFloat]("TAG_FLOAT", 5) {
    def apply(x: Float): TagFloat = new TagFloat(x)
  }

  final class TagFloat(x: Float) extends NumericValTag[Float, TagFloat](x) {
    override def companion = TagFloat
    override def mojangson = x + "f"
  }

  implicit case object TagDouble extends NumericValTagCompanion[Double, TagDouble]("TAG_DOUBLE", 6) {
    def apply(x: Double): TagDouble = new TagDouble(x)
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