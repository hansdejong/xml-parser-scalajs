import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Map
import scala.util.matching.Regex //Doet het ook zonder!

object TokenType extends Enumeration{
  type TokenType = Value
  val ElemStart, ElemEnd, ElemEmpty, XmlDecl, Decl1, Decl2, CDATA, PI, TText, Comment = Value
}

object Parser_token{
	//Structuur van walktokens overnemen, inclusief foutcorrectie.
	def makeTokenList( tokens:List[String] ): List[Token] = {
		val listBuf = new ListBuffer[Token]
		for (token <- tokens){
			if (token.startsWith("<")){

				if (token.startsWith("<!--") && token.endsWith("-->") ){
					val nwToken = Token(TokenType.Comment)
					nwToken.fullText = token
					listBuf += nwToken
				}
				if (token.startsWith("<![CDATA[")){
					val nwToken = Token(TokenType.CDATA)
					nwToken.fullText = token
					listBuf += nwToken
				}
				if (token.startsWith("<!")){
					val nwToken = Token(TokenType.Decl1)
					nwToken.fullText = token
					listBuf += nwToken
				}
				else if (token.startsWith("<?xml")){
					val nwToken = Token(TokenType.XmlDecl)
					nwToken.fullText = token
					listBuf += nwToken
				}
				else if (token.startsWith("<?")){
					val nwToken = Token(TokenType.PI)
					nwToken.fullText = token
					listBuf += nwToken
				}
				else if (token.startsWith("</")){
					val nwToken = Token(TokenType.ElemEnd)
					nwToken.fullText = token
					listBuf += nwToken
				}
				else if (token.endsWith("/>")){
					val nwToken = Token(TokenType.ElemEmpty)
					nwToken.fullText = token
					listBuf += nwToken
				}
				else if (token.endsWith(">")){
					val nwToken = Token(TokenType.ElemStart)
					nwToken.fullText = token
					listBuf += nwToken
				}
				else
					println ( "error: " + token) //Voorlopig
			}
			else if (token.toLowerCase.startsWith("doctype")){
				val nwToken = Token(TokenType.Decl2) //Kan ik het vorige teken aanvullen?
				nwToken.fullText = token
				listBuf += nwToken
			}
			else if(token.trim !=""){
				val nwToken = Token(TokenType.TText)
				nwToken.fullText = token
				listBuf += nwToken
			}
		}
		val tokenList:List[Token]=listBuf.toList
		tokenList.foreach(buildToken(_))
		tokenList
	}

	//Zorgt voor eventuele naam, en attributen
	private def buildToken(item:Token) ={
		val rName ="""^</?([\w|:]+)""".r
		//val rAttr = """\s\w+=\"\w+\"\W""".r
		val rAttr="""\s([^"]*)=\"([^"]*)\"""".r //Nakijken. Snap ik niet
		val namedTokens = List (TokenType.ElemStart, TokenType.ElemEnd)
		if( namedTokens.contains( item.token )  ){
			item.tagname = (rName findFirstIn item.fullText) getOrElse ("")
		}
		val atts = rAttr findAllIn item.fullText
		for(all <- atts) item.attributes += all
	}

}//Einde Parser_token

//=====================Externe routines===================================================================

case class Token( token: TokenType.TokenType){
	var tagname = ""
	var fullText = ""
	val attributes: ListBuffer[String]  = ListBuffer.empty
	override def toString () =  token.toString + ": " + fullText+"\n" +
		"\t==> "+ attributes + "\n" //Er weer uit
}




