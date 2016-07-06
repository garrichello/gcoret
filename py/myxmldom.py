from xml.dom import minidom
import os.path

NodeLists = {'foo' : 'bar'}

def load(filename):
    global Document
    global Nodes
    Document = minidom.parse(filename)
    Nodes = {os.path.basename(filename) : Document}
    return Document.nodeType

def getChildNodes(nodeName):
    global NodeLists
    global Nodes
    newNodeList = Nodes[nodeName].childNodes
    NodeLists[nodeName] = newNodeList
    return newNodeList.length

def getItem(nodeListName, nodeIdx):
    global NodeLists
    global Nodes
    item = NodeLists[nodeListName].item(nodeIdx)
    Nodes[nodeListName+'-'+str(nodeIdx)] = item
    return item.nodeType

def getAttribute(nodeName, attrName):
    global Nodes
#    print 'Node '+nodeName+' attribute '+attrName+' has value '+Nodes[nodeName].getAttribute(attrName)
    attrVal = Nodes[nodeName].getAttribute(attrName)
    return attrVal.encode('ascii', 'backslashreplace')

def getElementsByTagName(nodeName, tagName):
    global Nodes
    global NodeLists
    newNodeList = Nodes[nodeName].getElementsByTagName(tagName)
    NodeLists[nodeName] = newNodeList
    return newNodeList.length

def getFirstChild(nodeName):
    global Nodes
    fclName = nodeName+'_fc'
    firstChild = Nodes[nodeName].firstChild
    Nodes[fclName] = firstChild
    return firstChild.nodeType

def getNodeType(nodeName):
    global Nodes
    return Nodes[nodeName].nodeType

def getNodeValue(nodeName):
    global Nodes
    return Nodes[nodeName].nodeValue.encode('ascii', 'backslashreplace')

