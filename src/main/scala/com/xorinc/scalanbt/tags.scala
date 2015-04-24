package com.xorinc.scalanbt

import java.io.ByteArrayOutputStream

import scala.reflect.{ClassTag, classTag}
import collection.mutable.{ ArrayBuffer, HashMap => MHMap }
import Numeric.{ FloatAsIfIntegral, DoubleAsIfIntegral }

package object tags {

  implicit val __fint: Integral[Float] = FloatAsIfIntegral
  implicit val __dint: Integral[Double] = DoubleAsIfIntegral

  import scala.collection.GenTraversableOnce

  sealed trait Tag[Self <: Tag[Self]] {
    self: Self =>

    def companion: TagCompanion

    def name: String = companion.name

    def id: Int = companion.id

    def mojangson: String
  }

  sealed abstract class TagCompanion(val name: String, val id: Int)

  implicit case object TagCompound extends TagCompanion("TAG_COMPOUND", 10) {
    def apply(xs: (String, Tag[_])*): TagCompound = new TagCompound(xs)
    def unapplySeq(xs: TagCompound): Option[Seq[(String, Tag[_])]] = Some(xs.toSeq)
  }

  import language.dynamics
  final class TagCompound private (xs: MHMap[String, Tag[_]]) extends Tag[TagCompound] with Dynamic {

    def this(map: Seq[(String, Tag[_])]) = this(MHMap[String, Tag[_]](map: _*))

    private val theMap = xs

    def toSeq: Seq[(String, Tag[_])] = theMap.toSeq
    def toMap: Map[String, Tag[_]] = theMap.toMap

    def apply(s: String): Option[Tag[_]] = theMap.get(s)
    def update(s: String, t: Tag[_]): Unit = theMap(s) = t

    def applyDynamic[T <: Tag[T] : ClassTag](s: String): T = {
      val ct = classTag[T]
      ct.runtimeClass.cast(theMap(s)).asInstanceOf[T]
    }

    def updateDynamic(s: String)(t: Tag[_]): Unit = update(s, t)

    def foreach(f: ((String, Tag[_])) => Unit): Unit = theMap.foreach[Unit](f)

    def mapValues(f: Tag[_] => Tag[_]): TagCompound = new TagCompound(theMap.mapValues[Tag[_]](f).toSeq)

    def filter(f: ((String, Tag[_])) => Boolean): TagCompound = new TagCompound(theMap.filter(f).toSeq)

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
    def apply(xs: Tag[_]*) = new TagList(xs)
    def unapplySeq(xs: TagList): Option[Seq[Tag[_]]] = Some(xs.toSeq)
  }

  // TagList is !not! covariant._
  final class TagList private (xs: ArrayBuffer[Tag[_]]) extends Tag[TagList] {
    self: TagList =>

    def this(xs: Traversable[Tag[_]]) = this(ArrayBuffer[Tag[_]](xs.toSeq: _*))

    private val theList = xs

    if(theList.length > 1){
      val clazz = theList(0).getClass
      if(!theList.forall(_.getClass eq clazz))
        throw new IllegalArgumentException("TagList can contain only one type of tag!")
    }

    def toSeq: Seq[Tag[_]] = theList.toSeq
    def toArray(implicit ev: ClassTag[Tag[_]]): Array[Tag[_]] = theList.toArray

    def apply(i: Int): Tag[_] = theList(i)
    def update(i: Int, x: Tag[_]): Unit = theList(i) = x

    def add(x: Tag[_]): this.type = {
      if(theList.length > 0){
        val clazz = theList(0).getClass
        if(clazz ne x.getClass)
          throw new IllegalArgumentException("TagList can contain only one type of tag!")
      }
      theList += x
      this
    }
    def + (x: Tag[_]): this.type = this.add(x)

    def remove(x: Tag[_]): this.type = {
      theList - x
      this
    }
    def - (x: Tag[_]): this.type = this.remove(x)

    def length: Int = theList.length

    def foreach(f: Tag[_] => Unit): Unit = theList.foreach(f)

    def map[B <: Tag[_]](f: Tag[_] => B): TagList = new TagList(theList.map(f))

    def flatMap[B <: Tag[_]](f: Tag[_] => GenTraversableOnce[B]): TagList = new TagList(theList.flatMap(f))

    def filter(f: Tag[_] => Boolean): TagList = new TagList(theList.filter(f))

    override def companion = TagList

    override def toString: String = theList.mkString("TagList(", ", ", ")")

    override def mojangson: String = theList.map(_.mojangson).mkString("[", ",", ",]")

    override def equals(o: Any): Boolean = o match {
      case null => false
      case that: TagList => this.theList == that.theList
      case _ => false
    }
  }

  sealed abstract class ValTag[A, Self <: ValTag[A, Self]](private val x: A) extends Tag[Self] with Serializable with Cloneable {
    self: Self =>

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

  implicit class __toNBTList[A <: Tag[_]](val x: Traversable[A]) extends AnyVal {
    def nbt: TagList = TagList(x.toSeq: _*)
  }

  implicit case object TagByte extends NumericValTagCompanion[Byte, TagByte]("TAG_BYTE", 1) {
    def apply(x: Byte): TagByte = new TagByte(x)
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
  }

  final class TagString(x: String) extends ValTag[String, TagString](x) {
    override def companion = TagString
    override def mojangson = x
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