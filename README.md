## zio-xml-parser

A highly simplified xml-parser,
to avoid external library dependencies
in zio-config. The parser will be designed
in a way to work seamlessly with zio-config's
language assumptions.

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
    children: XmlObject
  ) extends XmlObject
}

```

We use zio-parser that has already dealt with recursive logic and stack safety.
