## zio-xml-parser

A highly simplified xml-parser, that it may not solve whole of RFC, but just works for you if you 
consider xml as your config.

The libraty is mainly made to avoid external library dependencies
in zio-config. 

The parser will be designed
in a way to work seamlessly with zio-config's
language assumptions. The parser will be moved to zio-parser library.

In a nutshell, a xml parsed object is as simple as

```scala
import zio.Chunk

sealed trait XmlObject

object XmlObject {
  type Attribute = (String, String)

  final case class Text(value: String) extends XmlObject

  final case class TagElement(
    name: String,
    attributes: Chunk[Attribute],
    children: Chunk[XmlObject]
  ) extends XmlObject
}

```

We use zio-parser that has already dealt with recursive logic and stack safety.
