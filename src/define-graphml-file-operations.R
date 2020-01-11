# TODO: Add comment
# 
# Author: nejat
###############################################################################


library(package="igraph", lib.loc=LIBRARY.LOC.PATH)
library(package="XML", lib.loc=LIBRARY.LOC.PATH)









########################################################################
#
########################################################################
removeAttrDefinitionNode = function(doc, id){

	keys = getNodeSet(doc, "//*[name()='key']")
	nodeToBeRemoved = NA

	for(key in keys){
		if(xmlAttrs(key)["id"] == id){
			nodeToBeRemoved = key
			break
		}
	}

	print(nodeToBeRemoved)

	if(!is.na(nodeToBeRemoved)){
		removeNodes(nodeToBeRemoved)
	} else{
		print("error: remove nodes")
	}
}



########################################################################
# return the content of the updated graphml file
########################################################################
addAttrDefinitionNode = function(doc, id, for.name, attr.name, attr.type){

	x = xmlRoot(doc, skip = FALSE)
	s = newXMLNode("key", parent=x)
	addAttributes(s, "id"=id)
	addAttributes(s, "for"=for.name)
	addAttributes(s, "attr.name"=attr.name)
	addAttributes(s, "attr.type"=attr.type)

	# the last child of the root node has the graph content
	#  (nodes def, edges def) which has many many lines.
	# To keep this graph content at the last XML node and place first
	#  the atrribute definition nodes (even if we add a new attr def node),
	#  do the following:
	n=xmlSize(x)
	last.before.node = x[[n-1]]
	x[[n-1]] = x[[n]]
	x[[n]] = last.before.node

}


########################################################################
# return the content of the updated graphml file
########################################################################
#addPartitionInfoIntoNodes = function(file.path, attr.name, algo.name, algo.output.file)
addPartitionInfoIntoNodes = function(file.path, attr.name, mems)
{
	doc <- xmlParse(file.path)

#	mems = get.membership.from.file(algo.name, algo.output.file)
#	if(algo.name == KMBS)
#		# replace -1 by an integer numbr for plot => -1 means node exclusion
#		mems = update.kmbs.membership.for.plot(mems)


	id = paste("v", attr.name, sep="_")
	addAttrDefinitionNode(
			doc, id=id, for.name="node", attr.name=attr.name, attr.type="double"
	)

	nodes = getNodeSet(doc, "//*[name()='node']")
	for(i in 1:length(nodes)){
		node = nodes[i]
		node.mem.info = mems[i]
		ils.partition = newXMLNode("data", parent=node)
		addAttributes(ils.partition, "key"=id)
		xmlValue(ils.partition) = node.mem.info
	}

	return(doc)
}



########################################################################
# return the content of the updated graphml file
########################################################################
addSignAttrIntoGraphmlFiles = function(file.path){
	doc <- xmlParse(file.path)

	addAttrDefinitionNode(
			doc, id="e_sign", for.name="edge", attr.name="sign", attr.type="double"
	)

	edges = getNodeSet(doc, "//*[name()='edge']")
	for(edge in edges){
		e = xmlChildren(edge)
		e.data = e[[1]] # first child
#		print(e.data)
		attr.name = xmlAttrs(e.data)["key"]
		if(attr.name == "e_weight" || attr.name == "weight" ){
			val = as.numeric( xmlValue(e.data) )
			xmlValue(e.data) = abs(val)
			s = newXMLNode("data", parent=edge)
			addAttributes(s, "key"="e_sign")
			if(val<0){
				xmlValue(s) = -1
			} else{
				xmlValue(s) = 1
			}
		}
	}

	return(doc)
}


########################################################################
# return the content of the updated graphml file
########################################################################
removeSignAttrFromGraphmlFile = function(file.path){
	doc <- xmlParse(file.path)

	removeAttrDefinitionNode(doc, "e_sign")

	edges = getNodeSet(doc, "//*[name()='edge']")
	for(edge in edges){
		e = xmlChildren(edge)
		e.weight = e[[1]] # first child
		e.sign = e[[2]] # second child
		val = as.numeric( xmlValue(e.weight) )
		if(as.numeric(xmlValue(e.sign)) == -1){
			xmlValue(e.weight) = val * -1
		}

		removeNodes(e.sign)

	}


	return(doc)
}





########################################################################
# order node ids in ascending order
# gephi randomly orders the nodes which is bad for node coloring from mems file
########################################################################
orderNodesById = function(file.path){
	doc <- xmlParse(file.path)

	nodes = getNodeSet(doc, "//*[name()='node']")
	node.ordered.list = list()

	for(node in nodes){
		id.val = xmlGetAttr(node, "id")

		# id start at 0 and R does not support 0 for indices
		id = as.numeric(gsub("n", "", id.val)) + 1
		#print(length(doc["//*[name()='node']"]))
		#print(node)
		node.ordered.list[[id]] = node
	}

	# TODO: Anlamadim bu yaptigim niye calisiyo??
	# normalde replacesNode(old, new) ile denedim ama orda
	#  length(doc["//*[name()='node']"]) yaptigimda habire node eksiliyo halbuki
	#  eksilmemesi lazim cunku replace yapiyorum

	x = getNodeSet(doc, "//*[name()='graph']")
	#print(x[[1]])
	addChildren(x[[1]], kids = node.ordered.list, at = 0, cdata = FALSE)


	return(doc)
}


########################################################################
# order node ids in ascending order
# gephi randomly orders the nodes which is bad for node coloring from mems file
########################################################################
retreiveAttrByKey = function(attr.list, key){

	for(attr in attr.list){

		attr.key = xmlAttrs(attr)["key"]
		if(attr.key == key){
			return(attr)
		}
	}

	return(-1) # otherwise
}


########################################################################
# order node ids in ascending order
# gephi randomly orders the nodes which is bad for node coloring from mems file
########################################################################
CorrectMEPIds = function(network.path, parititon.path){
	doc <- xmlParse(parititon.path)

	# check.names ile colon isimlerinde bosluk varsa o silinmiyo, ayni kaliyo
	MEP.DETAILS =
			read.table(
					"../../../out/_overall/mep-details.csv",
					header=1,
					sep=";",
					check.names= FALSE
			)

	############################################################################
	# Remove mep data if it was not available during a specific period
	g <- suppressWarnings(read.graph(file=network.path, format="graphml"))

	#iso.nodes.indx = which(degree(g) == 0) # to eliminate isolated nodes
	#nodes.indx = which(degree(g) != 0) # to eliminate isolated nodes
	#g = delete_vertices(g, iso.nodes.indx)

	current.meps = as.numeric(vertex.attributes(g)$MEPid)
	index.current.meps = which(MEP.DETAILS[, "MEP Id"] %in% current.meps)
	current.mep.details = MEP.DETAILS[index.current.meps,]
	############################################################################



	nodes = getNodeSet(doc, "//*[name()='node']")

	for(node in nodes){
		id.val = xmlGetAttr(node, "id")

		# id start at 0 and R does not support 0 for indices
		id = as.numeric(gsub("n", "", id.val)) + 1




		mepid = current.mep.details[id, "MEP Id"]
		firstname = current.mep.details[id, "Firstname"]
		lastname = current.mep.details[id, "Lastname"]
		group = current.mep.details[id, "Group"]


		children = xmlChildren(node)
		attr.lastname = retreiveAttrByKey(children, "v_lastname")
		xmlValue(attr.lastname) = lastname

		attr.firstname = retreiveAttrByKey(children, "v_firstname")
		xmlValue(attr.firstname) = firstname

		attr.mepid = retreiveAttrByKey(children, "v_mepid")
		xmlValue(attr.mepid) = mepid

		attr.group = retreiveAttrByKey(children, "v_group")
		xmlValue(attr.group) = group

	}


	return(doc)
}



# ==============================================================================

# EXAMPLE.GRAPHML.CONTENT = '<?xml version="1.0" encoding="UTF-8"?>
# <graphml xmlns="http://graphml.graphdrawing.org/xmlns"
#      xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
#      xsi:schemaLocation="http://graphml.graphdrawing.org/xmlns
#     http://graphml.graphdrawing.org/xmlns/1.0/graphml.xsd"> 
#   <key id="e_weight" for="edge" attr.name="weight" attr.type="double"/>
#   <graph id="G" edgedefault="undirected">
#     <node id="n0">
#     </node>
#     <node id="n1">
#     </node>
#     <node id="n2">
#     </node>
#     <node id="n3">
#     </node>
#     <node id="n4">
#     </node>
#     <edge source="n0" target="n1">
#       <data key="e_weight">1</data>
#     </edge>
#     <edge source="n0" target="n2">
#       <data key="e_weight">1</data>
#     </edge>
#     <edge source="n0" target="n3">
#       <data key="e_weight">1</data>
#     </edge>
#     <edge source="n0" target="n4">
#       <data key="e_weight">-1</data>
#     </edge>
#     <edge source="n1" target="n2">
#       <data key="e_weight">1</data>
#     </edge>
#     <edge source="n1" target="n3">
#       <data key="e_weight">1</data>
#     </edge>
#     <edge source="n1" target="n4">
#       <data key="e_weight">1</data>
#     </edge>
#     <edge source="n2" target="n3">
#       <data key="e_weight">1</data>
#     </edge>
#     <edge source="n2" target="n4">
#       <data key="e_weight">-1</data>
#     </edge>
#     <edge source="n3" target="n4">
#       <data key="e_weight">1</data>
#     </edge>
#   </graph>
# </graphml>
# '
# 
# # ==========
# # Scenario 1 
# # ==========
# 
# file.path = "temp-graphml-ops.graphml"
# write(x=EXAMPLE.GRAPHML.CONTENT, file=file.path)
# 
# doc = addSignAttrIntoGraphmlFile(file.path)
# print(doc)
# 
# 
# # ------
# 
# # ==========
# # Scenario 2
# # ==========
# 
# file.path = "temp-graphml-ops.graphml"
# write(x=EXAMPLE.GRAPHML.CONTENT, file=file.path)
# 
# nodes = getNodeSet(doc, "//*[name()='node']")
# 
# addAttrDefinitionNode(
#     doc, id="e_sign", for.name="edge", attr.name="sign", attr.type="double"
# )
# 
# keys = getNodeSet(doc, "//*[name()='key']")
# for(key in keys)
#     print(xmlAttrs(key)["id"])
# 
# print(doc)
# removeAttrDefinitionNode(doc, "e_sign")
# print(doc)



