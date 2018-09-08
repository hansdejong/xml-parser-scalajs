import re

class recollector:
    def __init__(self):
        self.res={}
    def add(self, name, reg ):
        re.compile(reg) # check that it is valid

        self.res[name] = reg % self.res
        
collector = recollector()
a = collector.add

a("TextSE" , "<aap")

a("MarkupSPE" , "noot")
a("XML_SPE" , "%(TextSE)s|%(MarkupSPE)s")
a("XML_MARKUP_ONLY_SPE" , "%(MarkupSPE)s")


def lexxml(data, markuponly=0):
    if markuponly:
        reg = "XML_MARKUP_ONLY_SPE"
    else:
        reg = "XML_SPE"
    regex = re.compile(collector.res[reg])
    return regex.findall(data)

def assertlex(data, numtokens, markuponly=0):
    tokens = lexxml(data, markuponly)
#    if len(tokens)!=numtokens:
#        assert len(lexxml(data))==numtokens,            "data = '%s', numtokens = '%s'" %(data, numtokens)
#    if not markuponly:
#        assert "".join(tokens)==data
    walktokens(tokens)

def walktokens(tokens):
    print
    for token in tokens:
        if token.startswith("<"):
            if token.startswith("<!"):
                print "declaration:", token
            elif token.startswith("<?xml"):
                print "xml declaration:", token
            elif token.startswith("<?"):
                print "processing instruction:", token
            elif token.startswith("</"):
                print "end-tag:", token
            elif token.endswith("/>"):
                print "empty-tag:", token
            elif token.endswith(">"):
                print "start-tag:", token
            else:
                print "error:", token
        else:
            print "text:", token

def testlexer():
    # this test suite could be larger!
    assertlex("<abc/>", 1)
    assertlex("<abc><def/></abc>", 3)
    assertlex("<abc>Blah</abc>", 4)
    assertlex("<abc>Blah</abc>", 2, markuponly=1)
    assertlex("<?xml version='1.0'?><abc>Blah</abc>", 3, markuponly=1)
    assertlex("<abc>Blah&foo;Blah</abc>", 3)
    assertlex("<abc>Blah&foo;Blah</abc>", 2, markuponly=1)
    assertlex("<abc><abc>", 2)
    assertlex("</abc></abc>", 2)
    assertlex("<abc></def></abc>", 3)

if __name__=="__main__":
    testlexer()
