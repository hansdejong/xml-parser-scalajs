import $file.P2_xmllexer
import $file.P3_xmlparser_token
import $file.P4_xmlparser_flatnode, P4_xmlparser_flatnode._
import $file.P5_xmltreeparser, P5_xmltreeparser._
import $file.P6_XML_voorbeelden

P1_Opstart.main(Array(""))

object P1_Opstart{
    def main(args: Array[String]):Unit = {
    val xmlString = P6_XML_voorbeelden.Voorbeelden.lijntjesSchema
//  val xmlString = scala.io.Source.fromFile("DariusBook.xml").mkString

    println( "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX" )
    println( "______________________________________________________________________" )
//  println( xmlString )
    val tokenstringList: List[String] = P2_xmllexer.lexxml(xmlString)
    println( "I. ==>\tEerst de tokenstringList (in Lexer):\n\n" +  tokenstringList.mkString)
    
    println( "______________________________________________________________________" )
    println( "II. ==>\tDan walktokens(tokenstringList) (in Lexer):\n")
    P2_xmllexer.walktokens(tokenstringList)
    
    println( "______________________________________________________________________" )
    val tokenList = P3_xmlparser_token.Parser_token.makeTokenList(tokenstringList)//roept ook xmlpreparser.buildToken() aan
    println("III. ==>\tNu de TokenList: (in Parser_token):\n\n" + tokenList.mkString)
    
    println( "______________________________________________________________________" )
    val flatnodeList = P4_xmlparser_flatnode.Parser_flatnode.makeFlatnodeList(tokenList)
    val uitlegIV = "\t(Deze komt praktisch overeen met de originele invoerlisting.)\n\n"
    println("IV. ==>\tEn de NodeList (in Parser_node):\n" + uitlegIV + flatnodeList.mkString) 
    
    println( "_________________________P5_____________________________________________" )
    val root:P5_xmltreeparser.StartElement = P5_xmltreeparser.TreeParser.makeElementTree(flatnodeList)
    val uitlegV = "\tDit wordt één element, de root, dat de andere elementen bevat.\n" +
                  "\tElementen bevatten hiërarchisch weer een lijst elementen en een lijst attributen.\n" +
                  "\tAfsluittags zijn waarschijnlijk niet meer nodig.\n"
    println("V. ==>\tEn tenslotte wordt dit de elementTree (in xmltreeparser):\n" + uitlegV + "\nRoot:\n" + root)
    
    println( "______________________________________________________________________" )
    val uitlegVI = "Nu nog een aantal voorbeelden van het gebruik.\n\n"
    println("VI. ==>\t" + uitlegVI) 
    println("De naam van de root: " + root.name)
    val children = for(child <- root.childNodes) yield child.description
    val childElem = for(child <- root.childStartElements) yield child.description
    println("De kinderen van de root:\n" + children.mkString("\n") + "\n")
    println("Het tweede element (base nul): " + root.childNodes(2).description)
    println("Alleen de topelementen:\n" + childElem.mkString("\n") + "\n")
	val schema = root.childStartElements(0)
	println( "De childNodes van schema:\n" + (for(node<-schema.childNodes)yield node.description).mkString("\n") )
	val emptyElem = schema.childNodes(2)
	println( "EmptyElem: "+ emptyElem.description + ",\nEmptyElem's parent: " + emptyElem.parent.description +
	 ",\nEmptyElem's attributes: "  + emptyElem.asInstanceOf[P5_xmltreeparser.Element].attributes)
	 
	 
	println( "______________________________________________________________________" )
    val uitlegVII = "PreorderTraversal.\n\n"
    println("VII. ==>\t" + uitlegVII) 
    println("NodeLijst:\n "+ root.preorder.mkString) 
     
	 
  }//einde main()
  
  
  
}



