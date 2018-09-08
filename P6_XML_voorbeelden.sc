object Voorbeelden{
  val exp1 = "7>8" 			//check
  val exp2 = "3+7" 			//check
  val exp3 = "3^7" 			//check
  val test1 = "aap noot mies"
  val test2 = "<een></een>"
  val test3 = """<a foo="" foo=""/>"""
  val test4 = """<a foo="" />""" //Deze doet het!!!!!
  val test6 = """<a foo=""></a>""" //Definition .block missing. Opgelost met"{}" Nu verwacht-ie een NameStart
  val test7 = """<Ruit foo=""></Ruist>""" //Definition .block missing. Nu verwacht-ie een NameStart. Bleek de spatie voor Ruist.
  //Nu OK. Kennelijk wordt verschil tussen de tags niet gedetecteerd.

  val lijntjesSchema: String =
  """<?xml version="1.0"?>
<!--Hans-->
<xsd:schema xmlns:xsd="http://www.w3.org/2001/XMLSchema">
	<xsd:annotation>
		<xsd:documentation> Test lijntjes-schema.</xsd:documentation>
	</xsd:annotation>
	<xsd:element name="kluwen" type="kluwenType"/>
	
	<xsd:complexType name="kluwenType">
		<xsd:sequence>
			<!--En zomaar nog een commentaar-->
			<xsd:element name="lijntje" maxOccurs="unbounded" type="lijntjesType" />
		</xsd:sequence>
	</xsd:complexType>
	
	<xsd:complexType name="lijntjesType">
		<xsd:sequence>
			<xsd:element name="x" type="coordinateType" use="required"/>
			<xsd:element name="y" type="coordinateType" use="required"/>
		</xsd:sequence>
	</xsd:complexType>
	
	<xsd:complexType name="coordinateType">
		<xsd:complexContent>
			<xsd:restriction base="xsdAnyType">
			 	<xsd:attribute name="value" type="xsd:decimal" use="required"/>
			 	<xsd:attribute name="min" type="xsd:decimal" use="required" fixed="-1"/>
			 	<xsd:attribute name="max" type="xsd:decimal" use="required" fixed="1"/>
			</xsd:restriction>
		</xsd:complexContent> 
	</xsd:complexType>
</xsd:schema>
"""

val lijntjesVoorbeeld: String   = """<?xml version="1.0" encoding="UTF-8"?>
<kluwen xsi:noNamespaceSchemaLocation="XLijntjesSchema.xsd" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
	<lijntje x="3" y="10"/>
	<lijntje x="13" y="7"/>
	<lijntje x="16" y="8"/>
	<lijntje x="2" y="17"/>
</kluwen>
"""

val lijntjesVoorbeeld2: String   = "<kluwen></ kluwen>"//"""<kluwen></ kluwen>"""  
}

