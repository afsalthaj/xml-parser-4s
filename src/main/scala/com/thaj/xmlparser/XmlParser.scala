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

  println(
    attributeParser
      .parseString("key=\"value\"")
  )

  val str =
    s"""
       |
       |<head key=\"value\" key=\"value2\"> hello </head>
       |
       |""".stripMargin

  println(
    xmlParser
      .parseString(str)
  )

  val nextTest =
    s"""
       | <simpletag a = "a">
       |   <sub1>
       |     text1
       |   </sub1>
       |   <sub2 a = "b">
       |      text2
       |   </sub2>
       |   <sub3 a = "c">
       |     text3
       |   </sub3>
       | </simpletag>
       |
       |
       |
       |""".stripMargin

  println(xmlParser.parseString(nextTest).map(_.flattened))

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

  val html =
    s"""
       |<aa\\<aa
       |aaa
       |="aaa"
       |
       |
       |></aa\\<aa
       |>
       |
       |""".stripMargin

  val html_ =
    s"""
       |< ⎱㶩饥鳆⢢癝瞖ഫ?輗뽁?⟍ꬒ옺쯉﮻ɡ깔䲊媦﷨ດﺽ忪経㮂쮌㴅业
       |
       |ꅉ瘧?㎴逾╱窱朆쳷㵌靄?団틗䯛篫㏒ḳ鍒搬⌆軚￾忉捱䰙嶹="
       |㺥釣퉄쉾栶멵㚴郘뱭肞욷閪熷郫脏쫧〽䨗网籄Ἠ筈뀻귢榑ɔ軽뀻 "
       | 稄ᘨบ䠬䢈ꍪ쏀쨍哚饱ᯯ㺴ᚮ꿄徰偮쭂䱦苣꛴멱焀䄫尣⑝艘ࡪ뒞饆="
       | 㔦铮열ㆿ縝挌⊚嵶ꗣ᳜ဇ㷶ꇀෞ괆➎뢄᪏䬍圭蜩蛏荓㷞땐龬ᢺ翲?쇥"
       |
       |  ?蜄罪킃簔螀ử띱꿐쪬प衶㼨頶啐ᛔ崏᫞쐶᧖濁彞ꩯ?⭇="  㰚荓唟鿡ꑭ薵嶧騖㓡ꂾ귷絑ফﲀݣౢ黿䑕押ᡨ鹙郠䳓雲⥣⥉ꪪ敲Ե"
       |
       |쳽ퟛዦ嶐뽐뎶䷥荂?믊墬뻎৽蚖亮뮭䨂ₒ큕뒽窢瑿턓虺ꑿ﷾中휖돚="᭻忾랶螺₊‧契ቓ߸왆㫂싀狔죫ތ巬ꪩ董쇛陧伺棶븟炔䕏䞙"  ᆾ쯞倿呱튚⠐㫤甫䍦훽Ꞥ쓟傭㪴뮽໷造੢ឹἶ镘ꂧ뭢 =" ❪̊๺䁽师ο鷥銫ꐵ紆莙ﳎ俴쉺ỡ䂧炑ു拾吟엗䶚趛榴獩å鸗 "
       |텺ꠏ⢳앀萪咥旎줨癶㞺ἂて벪䥆?腎잠勪ᕴ欇⣤ࣩ轻킻䈏﷋灢ᕧ="떼㱲?鐗㻧뼪⠠ῌ攃犞ᜆ共ഺը樂?휫逺リ鵯Ѝ㩯ぶ賓돖鈗뼲"
       | 퉟吝旖撒痿졣Ϳ䌊饳쵚粋Ƈ쌠⳦簾᭏鷩종７掴檵騨許羜놂鐓䆏=" 擲娔횷䴾榦傘䭅젬򥪎愒಩뙫萅稟컌␠슺ఌ毶惠Ὢ唇젻ⷋ鿦"  ㏶鶮갹뺂ﷺ䌷陞佯霶끺஍኶㼚藐姃鉕꿍嬁䆧쉚ṳ轀?ꍄ괘龷鶞
       |=" 甼➔⦴굨쁧鐢Љ㞷᭩୕杻Ꭸ㆏溍枤혡㣳ꡖ쾃毴ᡳﱋ볫歂᩷秣澷
       |"
       |
       |
       |噞〯褍ᰏ絀외뱴ⷹ濧꾘?潖ꊿ閈㖐䵶齰뭅㝪ﾎ喁䚅肧쮷孏燅  ="
       |
       |ᢒ镘口఻잋앵䎞ᆤ㬵帥巉愧錡播麥瞕鏷뜌큡ﰌ媚숅楤―㥃ꮴ  " 潕騗墰毳焻匐ᾊ免㮲⽽쇟꽽浮㲐㯶Ꮬ柬䛩져硔꒰ꑩꨤ᎔퇟
       |="⢗軲ॠꅊ?䨄냼啹烞雂?ौ浯㛁ᢁ氱夵怶섚1䅊ﻕ攌ꆌ뚪蕟ↁ秲㨮
       |"  >
       |
       |</⎱㶩饥鳆⢢癝瞖ഫ?輗뽁?⟍ꬒ옺쯉﮻ɡ깔䲊媦﷨ດﺽ忪経㮂쮌㴅业
       |>
       |""".stripMargin

  println(xmlParser.parseString(html_))

}
