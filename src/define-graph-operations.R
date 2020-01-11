# TODO: Add comment
# 
# Author: nejat
###############################################################################






########################################################################
# 
########################################################################
get.nb.cluster.from.membership = function(membership){
	indx = which(membership != CLU.NO.FOR.ALL.ISOLATED.NODES)
	nb.cluster = length(unique(membership[indx]))

	#nb.cluster = length(unique(membership))
	return(nb.cluster)
}


############################################################################
#
############################################################################
read.graph.graphml = function(network.path){
	g = suppressWarnings(read_graph(file=network.path, format=c("graphml")))
	return(g)
}


############################################################################
#
############################################################################
read.graph.ils.file.as.df = function(network.path){
	# skip the first line bc it does not contain graph info
	df = read.table(
			file=network.path, 
			header=FALSE, 
			sep="\t", 
			skip=1, 
			check.names=FALSE
	)
	# df$V1: vertex1
	# df$V2: vertex2
	# df$V3: weight
	return(df)
}



############################################################################
# to handle isolated nodes, first we had to find the max vertex id.
#  Then, indicate explicitely vertices ids in graph.data.frame()
############################################################################
read.graph.ils = function(network.path){
	df = read.graph.ils.file.as.df(network.path)
	
	edg.list = df[,c(1, 2)]
	max.v.id = max(unique(c(edg.list[,1], edg.list[,2])))
	
	g <- graph.data.frame(edg.list, vertices=seq(0,max.v.id), directed=FALSE)
	E(g)$weight = df[, 3]
	
	return(g)
}



########################################################################
# 
########################################################################
convert.weight.into.unweight.ils.input.graph = 
		function(network.path, unweighted.network.path)
{
	network = file(network.path, 'r')  # connection for reading 
	first.line = readLines(network, n = 1) 
	t <- read.table(network, header = FALSE, skip=1) # skip first line
	close(network) 
	
	weights = t$V3
	converted.weights = sapply(weights, 
			function(w){
				if(w > 0)
					return(1)
				else if(w < 0)
					return(-1)
			}
	)
	t$V3 = converted.weights
	
	write(first.line, unweighted.network.path)
	write.table(
			t, 
			unweighted.network.path, 
			sep="\t", 
			append=TRUE, 
			row.names=FALSE, 
			col.names=FALSE
	)
	
	return(t)
}


############################################################################
# Get nb node in the network
# 
############################################################################
get.network.size = function(network.path){
	
	g <- suppressWarnings(read.graph(file=network.path, format="graphml"))
	return(vcount(g))
}


############################################################################
#
############################################################################
has.any.isolated.nodes = function(network.path){
	
	g <- suppressWarnings(read.graph(file=network.path, format="graphml"))
	
	if(any(degree(g) == 0)){ # if not an isolated node(s) (2 ve ya 3 tane isolated node ayni clusterda da bulunabilir)			
		return(TRUE)
	}
	
	return(FALSE)
}


############################################################################
# First use the "has.any.isolated.nodes" method: If TRUE returns, use this method
# 
############################################################################
get.isolated.nodes.indx = function(network.path){
	
	g <- suppressWarnings(read.graph(file=network.path, format="graphml"))
	
	iso.index = which(degree(g) == 0)
	return(iso.index)
}


############################################################################
# First use the "has.any.isolated.nodes" method: If TRUE returns, use this method
# this method will allow us know how many nodes are isolated. Hence, we omit them in order to calculate "#cluster"
#
# TODO I need to check: every isolated node is also a singleton-cluster in output of CC/RCC methods ??
############################################################################
get.nb.isolated.nodes = function(network.path){
	
	iso.index = get.isolated.nodes.indx(network.path)
	return(length(iso.index))
}


########################################################################
# network.path is needed for a graph object
#
# put every isolated node into separate cluster
########################################################################
post.proc.membership.for.isolated.nodes = function(network.path, membership){
	
	iso.index = get.isolated.nodes.indx(network.path)
	#membership[iso.index] = CLU.NO.FOR.ALL.ISOLATED.NODES
	#nb.clu = length(unique(membership))-1 # exclude 0
	
	for(i in iso.index){
	    old.clu = membership[i]
	    membership[i] = CLU.NO.FOR.ALL.ISOLATED.NODES
        j = which(membership > old.clu)
        if(length(j) > 0)
            membership[j] = membership[j] - 1
	}
	
	return(membership)
}



############################################################################
#
############################################################################
get.graph.density = function(network.path){
	g <- suppressWarnings(read.graph(file=network.path, format="graphml"))					
	density = edge_density(g, loops=FALSE)
	return(density)
}



# ==================================================================
# Graph links and weights stats
#
# network.path: graphml graph file path
# ==================================================================
extract.graph.links.and.weights.stats = function(network.path){
	
	g <- suppressWarnings(read.graph(file=network.path, format="graphml"))
	neg.links <- E(g)$weight<0
	pos.links <- E(g)$weight>0
	
	neg.edges.total.weights = sum(abs(E(g)$weight[neg.links]))
	pos.edges.total.weights = sum(abs(E(g)$weight[pos.links]))
	neg.edges.total.links = sum(neg.links)
	pos.edges.total.links = sum(pos.links)
	
	df = data.frame(
			neg.edges.total.weights,
			pos.edges.total.weights,
			neg.edges.total.links,
			pos.edges.total.links		
	)
	
	return(df)
	
}


# ==================================================================
# Connected component analysis
# ==================================================================
do.connected.component.analysis = function(network.path){
	g <- suppressWarnings(read.graph(file=network.path, format="graphml"))	
	
	comp = clusters(g)
	component.membership = paste(comp$membership, collapse=" ")
	nb.compontent = comp$no
	component.size = paste(comp$csize, collapse=" ")
	
	df = data.frame(
			nb.compontent,
			component.size,
			component.membership
	)
	return(df)
}



############################################################################
#
############################################################################
# algo.output.file = membership file
# output.type = {"value", "percentage"}
compute.imbalance.from.membership = function(network.path, membership, output.type = "value"){
	g <- suppressWarnings(read.graph(file=network.path, format="graphml"))
	
	membership = as.integer(membership)
	edge.mat <- get.edgelist(g)
	
	clus.mat <- cbind(membership[as.integer(edge.mat[,1])], membership[as.integer(edge.mat[,2])])
	
	same.clus <- clus.mat[,1]==clus.mat[,2]
	
	#compare link signs and positions 
	neg.links <- E(g)$weight<0
	pos.links <- E(g)$weight>0
	neg.misplaced <- same.clus & neg.links
	pos.misplaced <- !same.clus & pos.links
	all.misplaced <- neg.misplaced | pos.misplaced
	
	imb.value = sum(abs(E(g)$weight[all.misplaced]))
	
	
	if(output.type == "value"){
		return(format(round(imb.value, 5), nsmall = 5)) # 5 decimal floating
	}
	else{ # if(output.type == "percentage")
		perc = (imb.value/ sum(abs(E(g)$weight)))*100
		return(format(round(perc, 5), nsmall = 5))
	}
	
}



############################################################################
#
############################################################################
# algo.output.file = membership file
compute.relaxed.imbalance.from.membership = function(network.path, membership, output.type = "value"){
	
	g <- suppressWarnings(read.graph(file=network.path, format="graphml"))
	
	edge.mat <- get.edgelist(g)
	clus.mat <- cbind(membership[edge.mat[,1]], membership[edge.mat[,2]])
	
	#compare link signs and positions 
	neg.links <- E(g)$weight<0
	pos.links <- E(g)$weight>0
	
	
	# compute the imbalance (i.e. cost) of intra-edges
	
	nb.clu = length(unique(membership))
	
	cost = 0
	for(clu in seq_len(nb.clu)){
		in.edges = (clus.mat[, 1] == clu & clus.mat[, 2] == clu)
		
		pos.misplaced = which((pos.links & in.edges) == TRUE)
		neg.misplaced = which((neg.links & in.edges) == TRUE)
		pos.cost = sum(abs(E(g)$weight[pos.misplaced]))
		neg.cost = sum(abs(E(g)$weight[neg.misplaced]))
		
		min.cost = ifelse(pos.cost > neg.cost, neg.cost, pos.cost)
		cost = cost + min.cost
	}
	
	
	# compute the imbalance (i.e. cost) of inter-edges if nb.clu > 1
	if(nb.clu > 1){
		pair.list = combn(x=nb.clu, m=2) # x'in m'li combinasyonu
		nb.pair = length(pair.list)/2
		
		for(i in seq_len(nb.pair)){
			clu1 = pair.list[1, i]
			clu2 = pair.list[2, i]
			
			betw.edges = (clus.mat[, 1] == clu1 & clus.mat[, 2] == clu2) | (clus.mat[, 1] == clu2 & clus.mat[, 2] == clu1)
			
			pos.misplaced = which((pos.links & betw.edges) == TRUE)
			neg.misplaced = which((neg.links & betw.edges) == TRUE)
			pos.cost = sum(abs(E(g)$weight[pos.misplaced]))
			neg.cost = sum(abs(E(g)$weight[neg.misplaced]))
			
			min.cost = ifelse(pos.cost > neg.cost, neg.cost, pos.cost)
			cost = cost + min.cost
		}
	}
	
	if(output.type == "value"){
		return(format(round(cost, 5), nsmall = 5)) # 5 decimal floating
	}
	else{ # if(output.type == "percentage")
		perc = (cost/ sum(abs(E(g)$weight)))*100
		return(format(round(perc, 5), nsmall = 5))
	}
}

############################################################################
#
############################################################################
compute.neg.pos.edge.percentage = function(g){
	
	neg.links <- E(g)$weight<0
	pos.links <- E(g)$weight>0
	
	neg.edges.total.weight = sum(abs(E(g)$weight[neg.links]))
	pos.edges.total.weight = sum(abs(E(g)$weight[pos.links]))
	
	result = list()
	result$pos.perc = (pos.edges.total.weight/ (pos.edges.total.weight + neg.edges.total.weight))*100
	result$neg.perc = (neg.edges.total.weight/ (pos.edges.total.weight + neg.edges.total.weight))*100
	
	return(result)
}



########################################################################
# 
########################################################################
perform.stats.after.partitioning = function(network.path, membership){
	
	cc.imb.value =  compute.imbalance.from.membership(network.path, membership, "value")
	cc.imb.perc =   compute.imbalance.from.membership(network.path, membership, "percentage")
	rcc.imb.value = compute.relaxed.imbalance.from.membership(network.path, membership, "value")
	rcc.imb.perc =  compute.relaxed.imbalance.from.membership(network.path, membership, "percentage")
	
	# ========================================================================
	nb.isolated.nodes = 0
	if(has.any.isolated.nodes(network.path))
		nb.isolated.nodes = get.nb.isolated.nodes(network.path)
	# ========================================================================
	
	nb.cluster = length(unique(membership)) - nb.isolated.nodes
	
#	imb.result = 
#			retreive.pils.imbalance.analysis.result(
#					algo.output.file, 
#					nb.partition, 
#					vcount(g) # nb node
#			)
#	in.edges.contr = imb.result["in edges contribution"][[1]]
#	out.edges.contr = imb.result["out edges contribution"][[1]]
#	neg.imb.contributions = 
#			in.edges.contr[,"negative.sum"] + out.edges.contr[,"negative.sum"]
#	pos.imb.contributions = 
#			in.edges.contr[,"positive.sum"] + out.edges.contr[,"positive.sum"]
#	
	
	stats = list(cc.imb.value=cc.imb.value, cc.imb.perc=cc.imb.perc,
			rcc.imb.value=rcc.imb.value, rcc.imb.perc=rcc.imb.perc, nb.cluster=nb.cluster) # TODO in.edges.contr ve out.edges.contr eklenebilir
	return(stats)
}



############################################################################
# In order to test wheter this function works correctly, create a test graph
# This graph is not randomized, so every time we will get the same graph
############################################################################
create.test.graph = function(){

	el = matrix(0, 10, 3)
	el[1,] = c(2, 4, +1)
	el[2,] = c(2, 5, +1)
	el[3,] = c(4, 5, -1)
	el[4,] = c(2, 7, -1)
	el[5,] = c(3, 7, +1)
	el[6,] = c(5, 7, +1)
	el[7,] = c(1, 7, +1)
	el[8,] = c(5, 6, +1)
	el[9,] = c(3, 6, +1)
	el[10,] = c(3, 5, -1)
	
	# membership
	mems = c(2, 1, 3, 1, 3, 3, 2)
	
	g=graph.edgelist(el[,1:2], directed=FALSE)
	E(g)$weight=as.numeric(el[,3])
	
	palette <- rainbow(length(unique(mems)))
	V(g)$color = palette[mems]
	
	return(g)
}
