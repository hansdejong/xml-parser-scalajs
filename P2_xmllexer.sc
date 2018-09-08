//http://code.activestate.com/recipes/65125-xml-lexing-shallow-parsing/
//http://www.cs.sfu.ca/~cameron/REX.html (61-88)

import scala.util.matching.Regex
import scala.collection.mutable.ListBuffer


def makeRegularExpression( onlyMarkup: Boolean = false )/*: String*/ = {
	val TextSE 		=  "[^<]+"
	val UntilHyphen 	=  "[^-]*-"
	val Until2Hyphens 	=  UntilHyphen + "(?:[^-]" + UntilHyphen + ")*-"
	val CommentCE 	=  Until2Hyphens + ">?"
	val UntilRSBs 		=  "[^\\]]*](?:[^\\]]+])*]+"
	val CDATA_CE 		=  UntilRSBs + "(?:[^\\]>]" + UntilRSBs + ")*>" 
	val S 			=  "[ \\n\\t\\r]+"
	val NameStrt 		=  "[A-Za-z_:]|[^\\x00-\\x7F]"
	val NameChar 		=  "[A-Za-z0-9_:.-]|[^\\x00-\\x7F]"
	val Name 		=  "(?:" + NameStrt + ")(?:" + NameChar + ")*"
	val QuoteSE 		=  "\"[^\"]*\"|'[^']*'"
	val DT_IdentSE	= S + Name + "(?:" + S + "(?:" + Name + "|" + QuoteSE + "))*" 
	val MarkupDeclCE	= "(?:[^\\]\"'><]+|" + QuoteSE + ")*>" 
	val S1	 		=  "[\\n\\r\\t ]"
	val UntilQMs 		=  "[^?]*\\?+"
	val PI_Tail  		=  "\\?>|" + S1 + UntilQMs + "(?:[^>?]" + UntilQMs + ")*>" 
	val DT_ItemSE   	=  "<(?:!(?:--" + Until2Hyphens + ">|[^-]" + MarkupDeclCE + ")|\\?" + Name + "(?:$PI_Tail))|%%" + Name + ";|" + S	
	val DocTypeCE	 	=  DT_IdentSE + "(?:$S)?(?:\\[(?:" + DT_ItemSE + ")*](?:" + S + ")?)?>?" 
	val DeclCE	 	=  "--(?:" + CommentCE + ")?|\\[CDATA\\[(?:" + CDATA_CE + ")?|DOCTYPE(?:" + DocTypeCE + ")?"
	val PI_CE 			=  Name + "(?:" + PI_Tail + ")?"
	val EndTagCE	 	=  Name + "(?:" + S + ")?>?"
	val AttValSE 		=  "\"[^<\"]*\"|'[^<']*'"
	val ElemTagCE	 	=  Name + "(?:" + S + Name + "(?:"+ S + ")?=(?:" + S + ")?(?:" + AttValSE +"))*(?:" + S + ")?/?>?"
	val MarkupSPE	 	=  "<(?:!(?:" + DeclCE + ")?|\\?(?:" + PI_CE + ")?|/(?:" + EndTagCE + ")?|(?:" + ElemTagCE + ")?)"
	val XML_SPE 		= TextSE + "|"+ MarkupSPE
	val XML_MARKUP_ONLY_SPE = MarkupSPE

	if ( onlyMarkup)
	  XML_MARKUP_ONLY_SPE
	else
	  XML_SPE
}

	def lexxml(data:String, markuponly:Boolean = false ): List[String] = {
	  val reg=makeRegularExpression(markuponly).r
	  val listBuf = new ListBuffer[String]
	  val iter: Regex.MatchIterator = reg findAllIn data
	  while(iter.hasNext){
		  listBuf += iter.next
	  }
	  listBuf.toList
	}

	def assertlex(data:String , numtokens:Int, markuponly:Boolean = false) ={
		val tokens:List[String] = lexxml(data, markuponly)
		if (tokens.length != numtokens){
			val fullParse = lexxml(data).length
		    assert (fullParse ==numtokens , s"data = $data, numtokens is not $numtokens but $fullParse" )
		}
		if (!markuponly)
		    assert ( tokens.mkString == data )
		walktokens(tokens)
    }

def walktokens( tokens:List[String] ):Unit = {
    println
    for (token <- tokens){
        if (token.startsWith("<")){
            if (token.startsWith("<!--") && token.endsWith("-->") )
                println ("comment: "+ token)//Comment
			else if (token.startsWith("<![CDATA["))
				println ("CDATA: "+ token)//CDATA
			else if (token.startsWith("<!"))
				println ("declaration: "+ token)//Decl1
            else if (token.startsWith("<?xml"))
                println ("xml declaration: " + token)//XmlDecl
            else if (token.startsWith("<?"))
                println ("processing instruction: " + token)//PI
            else if (token.startsWith("</"))
                println ( "end-tag: " + token)//ElemEnd
            else if (token.endsWith("/>"))
                println ( "empty-tag: " + token)//ElemEmpty
            else if (token.endsWith(">"))
                println ( "start-tag: " + token)//ElemStart
            else
                println ( "error: " + token)//Error
        }
        else if (token.toLowerCase.startsWith("doctype"))
	        println ("decl-rest:   " + token)//Decl2. Small problem. Better not correct the lexer itself.
        else
            println ("text:  " +  token)//TText
   }
}

def testlexer():Unit= {
	println("Dit is xmllexer.testlexer():\n")
    assertlex("<!doctype html public \"-//w3c//dtd html 4.0/en\" \"http://www.w3.org/tr/rec-html40/strict.dtd\">", 2)//oeps
    assertlex("<abc/>", 1)
    assertlex("<abc/><!--Dit is commentaar-->", 2)
    assertlex("<abc number=\"34\"/>", 1)
	  assertlex("<abc/><![CDATA[<sender>John Smith</sender>]]><abc/>", 3)
    assertlex("<abc><def/></abc>", 3)
    assertlex("<abc color=\"paars\">Blah</abc>", 3)
    assertlex("<abc>Blah</abc>", 2, markuponly=true)
    assertlex("<?xml version='1.0'?><abc>Blah</abc>", 3, markuponly=true)
    assertlex("<?Hans wat doe je nou ?>", 1)
    assertlex("<abc>Blah&foo;Blah</abc>", 3)
    assertlex("<abc>Blah&foo;Blah</abc>", 2, markuponly=true)
    assertlex("<abc></abcde>", 2)
    assertlex("</abc></abc>", 2)
    assertlex("<abc></def></abc>", 3)
}


