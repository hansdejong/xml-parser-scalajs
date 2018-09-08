import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Map

import $file.P3_xmlparser_token, P3_xmlparser_token._

var gLevel = 0 

object Parser_flatnode{

	def makeFlatnodeList(theList: List[Token]): List[Flatnode] = { //AFBLIJVEN. DOET HET!
		val resultList = new ListBuffer[Flatnode]
		for(theToken <- theList) if(tokenToFlatnode(theToken)!=None)resultList += tokenToFlatnode(theToken).get
		resultList.toList
	}

private def tokenToFlatnode(theToken:Token):Option[Flatnode]={
	theToken.token match {
		case TokenType.Comment => {
			//Inclusief commenttekens <!-- ... -->
			val commentText = theToken.fullText
			return Some(Comment_Flatnode(commentText))	}

		case TokenType.PI => {
			//Had ik hier eerst overgeslagen
			val targetText = theToken.fullText  //TODO, split up
			val contentText = theToken.fullText //TODO, split up
			return Some(PI_Flatnode(targetText, contentText))	}

		case TokenType.CDATA => {
			//Inclusief cdatatekens <![CDATA[ ... ]]>
			val cdataText = theToken.fullText
			return Some(CDATA_Flatnode(cdataText))	}

		case TokenType.XmlDecl => {
			//Inclusief <?xml ...name/values... ?>
			val xmlDeclContent = theToken.fullText
			return Some(XmlDeclaration_Flatnode(xmlDeclContent)) }

		case TokenType.Decl1 => None
			//Merge met volgende token

		case TokenType.Decl2 =>{
			//Merge met vorige token
			val DeclContent = theToken.fullText
			return Some(Declaration_Flatnode(DeclContent)) }

		case TokenType.TText =>{
			//Pure text
			val text = theToken.fullText
			return Some(Text_Flatnode(text)) }

		case TokenType.ElemEnd =>{
			val rNaam = """^</?([\w|:]+)""".r
			//The tagname (only)
			var theName=""
			(rNaam findAllIn theToken.fullText).matchData foreach{
				nm => theName = nm.group(1)
			}
			val endTag = EndTag_Flatnode(theName)
			return Some(endTag) }

		case TokenType.ElemStart =>{
			val rNaam = """^</?([\w|:]+)""".r
			//val rAttr = """\s\w+=\"\w+\"\W""".r
			val rAttr="""\s([^"]*)=\"([^"]*)\"""".r
			//The tagname
			var theName=""
			(rNaam findAllIn theToken.fullText).matchData foreach{
				nm => theName = nm.group(1)
			}
			val startTag = BeginTag_Flatnode(theName)
			//todo attributes
			val allAtts = rAttr findAllIn theToken.fullText
			for(att <- allAtts){
					(rAttr findAllIn att).matchData foreach{
					nv => startTag.attributes(nv.group(1)) = nv.group(2) //Setting map-entry
				}
			}
			//todo (fulltext, niet meer nodig), (Tokenlist, nee nog niet)
			return Some(startTag) }

		case TokenType.ElemEmpty =>{
			val rNaam = """^</?([\w|:]+)""".r
			//val rAttr = """\s\w+=\"\w+\"\W""".r
			val rAttr="""\s([^"]*)=\"([^"]*)\"""".r
			var theName=""
			(rNaam findAllIn theToken.fullText).matchData foreach{
				nm => theName = nm.group(1)
			}
			val emptyTag = EmptyTag_Flatnode(theName)
			//todo attributes
			val allAtts = rAttr findAllIn theToken.fullText
			//Hij lijkt er steeds een te weinig te geven
			for(att <- allAtts){
				(rAttr findAllIn att).matchData foreach{
					nv => emptyTag.attributes(nv.group(1)) = nv.group(2) //Setting map-entry
				}
			}
			//todo (fulltext, niet meer nodig), (Tokenlist, nee nog niet)
			return Some(emptyTag) }
		case _ => {
			println("Fout in stap IV, tokenToFlatnode. Unknown Token " + theToken.token)
			return None
		}
	}

}
	
}//Einde Parser_node

//=======================================================================================

trait Flatnode

case class Text_Flatnode(text: String) extends Flatnode{
	val level = gLevel
	override def toString() = { (" " * level) + text + "\n"	}}
case class PI_Flatnode( target:String, content:String ) extends Flatnode{
	val level = gLevel
	override def toString() = { (" " * level) + content + "\n"	}}
case class CDATA_Flatnode (content:String) extends Flatnode{
	val level = gLevel
	override def toString() = { (" " * level) + content + "\n"	}}
case class Comment_Flatnode	(content:String) extends Flatnode{
	val level = gLevel
	override def toString() = { (" " * level) + content + /*"(" + level + ")" +*/ "\n"	}}
case class XmlDeclaration_Flatnode (content:String) extends Flatnode{
	val level = gLevel
	override def toString() = { (" " * level) + content + "\n"	}}
case class Declaration_Flatnode	(content:String) extends Flatnode{
	val level = gLevel
	override def toString() = { (" " * level) + content + "\n"	}}
	

case class EmptyTag_Flatnode(val name:String)extends Flatnode{ //replaced by Element
	val level = gLevel// + 1
//	val level = gLevel; gLevel += 1
	var attributes = Map.empty[String,String]
	override def toString () = {
		" " * level + "<"+ name + attributeString(attributes) +"/>"+"\n"
	}
}
case class BeginTag_Flatnode(val name:String/*, parent:Option[Element]*/ ) extends Flatnode {//replaced
	val level = gLevel; gLevel += 1 //zou level een ref zijn naar gLevel i.p.v. een echte variabele
	var attributes = Map.empty[String, String]
	override def toString() = {
		" " * level + "<"+ name + attributeString(attributes) +">"+"\n"
	}
}
case class EndTag_Flatnode(val name:String)extends Flatnode { //removed
	val level = gLevel; gLevel -= 1
	override def toString() = { (" " * level) + "</" + name +  ">" + "\n"}}



//Gebruikt door de "ruwe" element-classes (Nog serieel i.t.t. hiërarchisch)
def attributeString(theMap:Map[String,String]):String ={
	//var result:List[String]=List()
	var result=""
	if (theMap.size==0) result = "(Geen attributen)"
	else{ 
//		for((k,v)<-theMap) result += " " + k + "=\"" + v + "\" "
		val results = for( (k,v)<-theMap) yield ( k + "=\"" + v + "\"")
		result = results.mkString(", ")
	}
	" " + result
}










