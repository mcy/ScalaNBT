# ScalaNBT

Scala library for NBT io with some Mojangson support.

## Usage

There are ten data tags: 
* Tags for each of the six Java number primitives, `byte, short, int, long, float, double`, as well as `String`.
* A list tag that can hold exactly one type of tag.
* A compound tag (a map) that maps strings to arbitrary tags.
* An end tag that exists only to denote a missing tag, and does not exist

General tags can be created with `Tag()`. If no Tag type exists that can hold the argument TagEnd will be returned.


-----

Primitive tags can be created in one of two ways:

`TagString("foo")` `TagInt(1)`

`"foo".nbt` `1.nbt`

These are immutable.

-----

`TagList`s can be manipulated like `Seq`s, but types are validated at runtime rather than at compile time.

`foo = list(1)`

`list(2) = 5`

TagList will use Tag() to convert non-nbt values into tags.

You can also create `TagList`s from `Traversable`s:
 
`List(1, 2).nbt`

-----

`TagCompound`s can be manipulated like `Map`s, but you can use dynamic notation too:

`compound.key = "value"`

TagCompound will use Tag() to convert non-nbt values into tags.

You can also create them similarly to `Map`s:

`TagCompound("one" -> 1, "2" -> "two")`

-----

`TagCompound`s can be converted to and from binary form via the `io.readNBT` 
and `io.writeNBT` functions, or the `.toBytes` method.

All tags can be converted to mojangson via the `.mojangson` method.
