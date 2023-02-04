package com.thaj.xmlparser

import com.thaj.xmlparser.XmlObject.TagElement
import com.thaj.xmlparser.XmlParser.{attributeParser, xmlParser}
import zio.Chunk
import zio.parser.Parser

object XmlParser extends Parsers {

  lazy val tagContent: Parser[String, Char, Option[Chunk[XmlObject]]] =
    xmlParser.repeat
      .orElseEither(textParser.zip(ws).map(Chunk(_)))
      .map(_.merge)
      .optional

  lazy val xmlParser: Parser[
    String,
    Char,
    XmlObject
  ] =
    ws.zip(openAngular)
      .zip(ws)
      .zip(tagIdentifier)
      .zip(ws)
      .zip(attributeParser.zip(ws).repeat0)
      .zip(ws)
      .zip(closedAngular)
      .zip(ws)
      .zip(tagContent)
      .zip(ws)
      .zip(closedTag)
      .zip(ws)
      .transformEither { case (name, attributes, xmlObject, (_, _, closedTag)) =>
        if (name == closedTag) {
          Right(
            TagElement(
              name,
              attributes,
              Chunk.fromIterable(xmlObject).flatten
            )
          )
        } else {
          Left(s"Closed tag $closedTag is not the same as open tag $name")
        }
      }

  def parse(string: String): Either[Parser.ParserError[String], XmlObject] =
    xmlParser.parseString(string)

}

object TestApp extends App {

//  println(
//    attributeParser
//      .parseString("key=\"value\"")
//  )
//
//  val str =
//    s"""
//       |
//       |<head key=\"value\" key=\"value2\"> hello </head>
//       |
//       |""".stripMargin
//
//  println(
//    xmlParser
//      .parseString(str)
//  )
//
//  val nextTest =
//    s"""
//       | <simpletag a = "a">
//       |   <sub1>
//       |     text1
//       |   </sub1>
//       |   <sub2 a = "b">
//       |      text2
//       |   </sub2>
//       |   <sub3 a = "c">
//       |     text3
//       |   </sub3>
//       | </simpletag>
//       |
//       |
//       |
//       |""".stripMargin
//
//  println(xmlParser.parseString(nextTest).map(_.flattened))

//  Right(
//    HashMap(
//      Chunk(KeyName(simpletag), KeyName(sub2)) -> text2,
//      Chunk(KeyName(simpletag), KeyName(a)) -> a,
//      Chunk(KeyName(simpletag), KeyName(sub2), KeyName(a)) -> b,
//      Chunk(KeyName(simpletag), KeyName(sub3), KeyName(a)) -> c,
//      Chunk(KeyName(simpletag), KeyName(sub1)) -> text1,
//      Chunk(KeyName(simpletag), KeyName(sub3)) -> text3
//    )
//  )

//  val html =
//    s"""
//       |<aa\\<aa
//       |aaa
//       |="aaa"
//       |
//       |
//       |></aa\\<aa
//       |>
//       |
//       |""".stripMargin
//
  val html_ = {

  s"""
     |        < 洎穐鮜咰錩◈扆᫦蔴輑蓶뚈ၐ껬뎺ᶓ콻ㆌꎈ謶죪嘁失べ搑諠펴f    
     |QbLoDEpSWeEyWn44ptAQBKRi8WJ7CS=
     |"
     |fGT6HrsVJRcJPAFunRIimoCEctRng7"tthSmRuvIs1vV2tZsUtqfaOHB30PBi="dA6tL12xWxTihJe6FChWLRnKxmldMv"    5SJ2eIpmqR46QQyyyVNx7ybGRKYXf4    ="OxDE99jisNZJxkqJ9x325Ld6aiA916    "           fZfmrmltmKPjDqVtoAdpvIV1bTz9bx        =    "    A6jqFxFV1FZc9CIr8l51f7N89LBuwW        "             
     |
     |
     |
     |
     |
     |
     |
     |
     |AnNEPw97UALUFeTJC29pHjXarRaWZi        =
     |
     |
     |
     |
     |
     |
     |
     |
     |"
     |
     |
     |
     |
     |
     |
     |
     |
     |洎穐鮜咰錩◈扆᫦蔴輑蓶뚈ၐ껬뎺ᶓ콻ㆌꎈ謶죪嘁失べ搑諠펴f           >
     |
     |hdoBvl8ebtOicLMxgwmTccGy9LsTKg      "                   wxD54JS3ONqydQxm8ALmVPxPUfaEBP​=   "   ubDhA3soWvErm8QLQqm9bFspusJEsf​"​  FODebCjlB6jM2HZoflasa4it2UYRgO ="4fYrcUiPvQDv8WsYeEvLHbtjYTQycK "             wuIlOC5uBlCS66N5GE5WgUYERvhv98   =   "   iFOEu7ZoKdIGt9m1tKm32lIBX3wvzh   "               9scGoPRnO1yJB6DTrvBhTfdYWkRpTk=      "      42ZCcWKKGdScYj5KfZk5CKgbB1H414"        wBGYd7CDVFhSqT65iyAYbpJn16kNX8    ="sFBHpGh8WbjIlJbJVnt6AYw6NcheoE    "                 oo3fqGH95chzHkiQk1wiqc340edhC2   =   "   X50rXIGGG5GzEZdK0qPcbBEE2HLCGT   "        ​​​​​​​​BQGXCxwwVNn6njNLqCb6XAcXR5Hbwg       =​​​​​​​​"​​​​​​​​vnjyOgt4YwGgKWXhQRBogN4uRNicLe       "       nFprqhPYV6CK2UdfXX1ywS0frtAraJ          ="2O82alzxpiZhiGLeAQucAGfrwk7eig          "          							>                              ​​​​​​​​</            콸⧔飹뛏搔?启툹멿蝋愰Ů⦷ꏿ緁臡祉흘듨鞷掟埀뭃譗눴䉥퇢筩꒹   >        
     |        <         韭롯홍氮陫⸞訳똕匙ㅦ䃸嬍駶??閿췟Ԅ強젿ꇟ㎜얙Қا훪㐁ꛠ		Pil56dzKxxnVibg77NjaOEgEJtBYhb     ="DxHV1imArW2IrM7wZHINmMSbwBolZH     "                xGJ3EXUentRp3JQDTzmJpZwdOstkzE      =  "  ILqvS3MEFfwJwXGrql00rA5rOpg0WW      "      
     |
     |
     |
     |</         韭롯홍氮陫⸞訳똕匙ㅦ䃸嬍駶??閿췟Ԅ強젿ꇟ㎜얙Қا훪㐁ꛠ		　　　　>        
     |  <
     |
     |
     |
     |
     |
     |
     |
     |
     |         >                 </​​​​​ ༳㙌㌄뚿㐭?﹝䎐ऐ췁㙟屈풽侐แ屝ु뮸吱Ⱊ챲猕​畿긁躴ᮂ疬        >    
     |
     |""".stripMargin

  }

  println(html_)
  println(xmlParser.parseString(html_))

  println(Parser.charNotIn('"').repeat0.parseString("\rsghbn"))

}
