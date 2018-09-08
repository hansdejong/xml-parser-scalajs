import $file.Lib_error, Lib_error._
import $file.Lib_stack, Lib_stack._
import $file.P3_xmlparser_token, P3_xmlparser_token._
import $file.P4_xmlparser_flatnode, P4_xmlparser_flatnode._

import scala.collection.mutable.ListBuffer

object TreeParser{
	def makeElementTree(flatnodeList: List[Flatnode]): TopElement = {
		//Een rootelement toegevoegd
		val parent:Option[TopElement] = None
		val attributes = scala.collection.mutable.Map.empty[String,String] //stub,emptyMap. Blijft waarchijnlijk leeg voor de root
		val root: TopElement = TopElement(0, "root", attributes, makeNodeList( flatnodeList))
		updateParents(root)
		root
	}

	def makeNodeList( children: List[Flatnode]):List[Node] = {
		val startlevel=0
		val ownChildren:Either[String,List[Node]] = makeNodeListOrFail(startlevel, children)
		ownChildren match{
			case Left(msg)      => throw new ParserException( msg )
			case Right(theList) => theList
		}
	}

    def makeNodeListOrFail(level:Int, children: List[Flatnode]):Either[String,List[Node]] = {
        var insideSub = false
        val ownChildren = new ListBuffer[Node]
        val currentSubList = new ListBuffer[Flatnode]
        var stack = new Stack[BeginTag_Flatnode]
        for((child, index) <- children.zipWithIndex){
            child match{
                case tag:BeginTag_Flatnode => stack = stack.push(tag)
                                              if (insideSub){
                                                  currentSubList += tag
                                              }
                                              else{
                                                  insideSub = true
                                                  //verder niets. Er komen tot de matchende eindTag alleen elementen op de subList
                                                  //Die eindtag maakt bij match zowel begin als eindtag aan
                                              }
                case tag:EndTag_Flatnode   => {
                                                 //println("\n>>>EindTag<<<")
                                                 val popped = stack.pop
                                                 if(popped == None) return Left("Lege stack (endtag zonder begintag)")
                                                 else{// Begin: er is een node op de stack
                                                     val(poppedTag, theRest) = popped.get
                                                     stack = theRest
                                                     if(insideSub) {
///*
                                                         //Controle toegevoegd: Tagname moet sowieso gelijk zijn aan de naam van de gepopte tag
                                                         //Misschien beter weg en pas afhandelen bij verwerking substring
                                                         if(poppedTag.name != tag.name) 
                                                             return Left("Niet matchende endtag (" + poppedTag.name +", index in subList: " +
                                                             index+ ")\n" + "ownChildren:\n" + ownChildren.toList)
//*/
                                                         if(stack.isEmpty && poppedTag.name == tag.name){//namen gelijk en stack leeg
                                                       	     //println("---insideSub. Gelijk: "+ tag.name + " en " + poppedTag.name)
                                                             insideSub = false
                                                             val sublist =  currentSubList.toList
                                                             currentSubList.clear
                                                             val xxxNodeList = makeNodeListOrFail(level+1, /*Some(dummyElement),*/ sublist)   
                                                             val theSubList = xxxNodeList match{
																case Left(msg)      => return Left( msg )
																case Right(theList) => theList
															 } 
                                                             ownChildren += TopElement(level,  
                                                                                       poppedTag.name,
                                                                                       poppedTag.attributes,
                                                                                       theSubList//children, recursief
                                                                                      ) 
                                                             ownChildren += flatnodeToNode(level, tag) //endtag ook toevoegen
                                                         }
                                                         
                                                         else{//namen verschillend of stack nog niet leeg
                                                             //println("---insideSub. Ongelijk: "+ tag.name + " en " + poppedTag.name, of stack niet leeg)
                                                             if(!stack.isEmpty)
	                                                             currentSubList += tag //geneste endtag 
	                                                         else
	                                                             return Left("Niet matchende endtag (" + poppedTag.name +", index in subList: " +
                                                                     index+ ")\n" + "ownChildren:\n" + ownChildren.toList)
                                                         }

  												   }else{//outside sub. Mag niet voorkomen.
                                                       return Left("Endtag zonder begintag (" + poppedTag.name +", index in subList: "+
                                                       index+ ")\n" + "ownChildren:\n" + ownChildren.toList) 
												   }//Eind: outside sub
											   }// Eind: er is een node op de stack
                                            }//Eind Endtag-blok
                case tag:EmptyTag_Flatnode       => if(insideSub) currentSubList += tag else ownChildren += flatnodeToNode(level, tag)
                case tag:Text_Flatnode           => if(insideSub) currentSubList += tag else ownChildren += flatnodeToNode(level, tag)
                case tag:Comment_Flatnode        => if(insideSub) currentSubList += tag else ownChildren += flatnodeToNode(level, tag)
                case tag:CDATA_Flatnode          => if(insideSub) currentSubList += tag else ownChildren += flatnodeToNode(level, tag)
                case tag:XmlDeclaration_Flatnode => if(insideSub) currentSubList += tag else ownChildren += flatnodeToNode(level, tag)
                case tag:Declaration_Flatnode    => if(insideSub) currentSubList += tag else ownChildren += flatnodeToNode(level, tag)
                case tag:PI_Flatnode             => if(insideSub) currentSubList += tag else ownChildren += flatnodeToNode(level, tag)
                case _ => throw new Lib_error.ProgrammingError("Unknown Flatnode " + child, "makeElementsOrFail")
            }//Einde match
        }//Einde for
		Right(ownChildren.toList)
    }// EindemakeElementsOrFail
			
    def dummy() = {}	
}//Einde TreeParser

val dummyTopElement:TopElement = TopElement(0, "Dummy", scala.collection.mutable.Map(""->""), Nil) 	

trait Element{
    val description:String
    val attributes: scala.collection.mutable.Map[String,String]
    val numChildTopElements:Int //inclusief empty-elements
    val childTopElements:List[Element]
    val childNodes:List[Node]
}
trait Node{	
    var parent:TopElement/*option?*/
    val description:String
    def indent(level:Int):String= ("  " * level)
}
//trait Element

case class Text_Node (level:Int, text: String) extends Node{
	var parent = dummyTopElement
	val description         = indent(level) + "Text"
	override def toString() = indent(level) + text + "\n"
}
case class Comment_Node	(level:Int, content:String) extends Node{
	var parent = dummyTopElement
	val description         = indent(level) + "Comment"
	override def toString() = indent(level) + content + /*"(" + level + ")" +*/ "\n"
}
case class CDATA_Node (level:Int, content:String) extends Node{
	var parent = dummyTopElement
	val description         = indent(level) + "CDATA"
	override def toString() = indent(level) + content + "\n"	
}
case class XmlDeclaration_Node (level:Int, content:String) extends Node{
	var parent = dummyTopElement
	val description         = indent(level) + "XmlDecl"
	override def toString() = indent(level) + content + "\n"
}
case class Declaration_Node	(level:Int, content:String) extends Node{
	var parent = dummyTopElement
	val description         = indent(level) + "Decl"
	override def toString() = indent(level) + content + "\n"
}
case class PI_Node ( level:Int, target:String, content:String ) extends Node{
	var parent = dummyTopElement
	val description         = indent(level) + "PI"
	override def toString() = indent(level) + content + "\n"
}

case class TopElement (val level:Int,
					val name:String,
					//val parent:Option[Element],
					val attributes: scala.collection.mutable.Map[String,String],
					val childNodes: List[Node]
				   ) extends Node with Element{ 
	var parent = dummyTopElement
	val description = indent(level) + "TopElem <" +name+">"
	override def toString () = {
		var result = indent(level) + "<"+ name + ", Attributen:" + attributeString(attributes)+">"+"\n"
		for(childNode <- childNodes){
			result += childNode.toString()
 		}
 		result
	}

	val numChildTopElements = {
		var result:Int = 0
		for(el <- childNodes){
			el match{
				case el:TopElement => result += 1
				case el:EmptyElement => result += 1
				case _ =>
			}
		}
		result
	}
	val childTopElements:List[Element] = {
		var buf = new ListBuffer[Element]
		for(el <- childNodes){
			el match{
				case el:TopElement => buf += el
				case el:EmptyElement => buf += el
				case _ =>
			}
		}
		buf.toList
	}
}

case class EmptyElement (val level:Int,
				         val name:String,
					     val attributes: scala.collection.mutable.Map[String,String]
				        ) extends Node with Element{
 	var parent = dummyTopElement
	val description          = indent(level) + "EmptyElem <" + name + "/>"
	override def toString () = indent(level) + "<"+ name + ", Attributen:" + attributeString(attributes)+"/>"+"\n"
	val numChildTopElements = 0
    val	childTopElements = Nil
    val childNodes = Nil
}

case class EndElement (val level:Int, val name:String) extends Node{ 
 	var parent = dummyTopElement
	val description          = indent(level) + "EndElem </" + name + ">"
	override def toString () = indent(level) + "</"+ name + ">"+"\n"
}

private def flatnodeToNode(level:Int, flatnode:Flatnode):Node={
	flatnode match {
	  //BeginTag niet hier, wijkt af. Niet alles kopieren, maar children recursief met een sublist
		case end:EndTag_Flatnode             => val endName = end.name
		                                        EndElement(level, endName) //Geen attributes en children
		case empty:EmptyTag_Flatnode         => val emptyName = empty.name
		                                        val emptyAttr = empty.attributes
		                                        EmptyElement(level, emptyName, emptyAttr)
		case textnode:Text_Flatnode          => val textContent = textnode.text //Pure text
                                                Text_Node(level, textContent)
		case comment:Comment_Flatnode        => val commentContent = comment.content//Inclusief commenttekens <!-- ... -->
                                                Comment_Node(level, commentContent)
		case cdata:CDATA_Flatnode            => val cdataContent = cdata.content//Inclusief cdatatekens <![CDATA[ ... ]]>
                                                CDATA_Node(level, cdataContent)
		case xmlDecl:XmlDeclaration_Flatnode => val xmldeclContent = xmlDecl.content//Inclusief <?xml ...name/values... ?>
                                                XmlDeclaration_Node(level, xmldeclContent)
		case decl:Declaration_Flatnode       => val declContent = decl.content
                                                Declaration_Node(level, declContent)
		case pi:PI_Flatnode                  => val piTarget = pi.target
                                                val piContent = pi.content
                                                PI_Node(level, piTarget, piContent)
		case _                               => throw new Lib_error.ProgrammingError("Unknown Flatnode " + flatnode, "flatnodeToNode")
	}
}

private def updateParents(top:TopElement):Unit = {
    for(child <- top.childNodes){
	    child match{
		    case subTop:TopElement => subTop.parent = top
	                                  updateParents(subTop)
            case _          => child.parent = top
        }
    }
}




