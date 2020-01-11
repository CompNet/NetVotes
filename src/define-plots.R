# TODO: Add comment
# 
# Author: nejat
###############################################################################



########################################################################
# Prepares the node shapes info for plot and legend. 
# "shape label ids" allows to put the available shape symbols in legend
# "shape membership" allows to draw node shapes
#
# target.type: "Group" or "State"
# mep.details: MEPs (available MEPs in the concerned graph) details such as country, MEP id etc.
#
# returns: shape info which contains shape label ids" and "shape membership"
########################################################################
prepare.shape.info = function(target.type, mep.details){
	shape.info = list(label.ids = vector(), mems = vector())
	
	
	if(target.type == "State"){ # use node shapes for political group info

		# ============================================================
		# create a list which contains political group names and their associated ids
		POLITICAL.GROUP.LIST.BY.ID = as.numeric(as.factor(POLITICAL.GROUP.LIST))
		political.group.shape.ass = lapply(POLITICAL.GROUP.LIST.BY.ID, function(id) id)
		names(political.group.shape.ass) = POLITICAL.GROUP.LIST
#		"political.group.shape.ass":
#		$ALDE		$ECR		$EFD		$EPP		$Greens		$GUENGL		$NI		$SD
#		[1] 1		[1] 2		[1] 3		[1] 4		[1] 5		[1] 6		[1] 7	[1] 8
		# ============================================================
		
		# in "shape.membership" each node is associated with a political group id
		shape.info$mems = # shape.membership
				sapply(mep.details[,"Group"], 
						function(pol.group.name) 
							political.group.shape.ass[[pol.group.name]] # return id
				) 
		shape.info$label.ids = sort(unique(shape.info$mems)) # later, shape ids will give us shape names
		
	} else if(target.type == "Group"){ # use node shapes for country info
		# Also, we did  not create as many vertex shape as the number of countries 
		# TODO If you can, create many many node shapes (approx 26-27). 
		# But, you should be able to show these node shapes in legend which is hard as well
		
		# ============================================================
		# create a list which contains region names and their associated ids
		REGION.LIST.BY.ID = as.numeric(as.factor(REGION.LIST))
		region.shape.ass = lapply(REGION.LIST.BY.ID, function(id) id)
		names(region.shape.ass) = REGION.LIST
#		An example of "region.shape.ass":
#		$Central		$Northern		$Southeastern		$Southern		$Western
#		[1] 1			[1] 2			[1] 3				[1] 4			[1] 5
		# ============================================================
	
		# in "shape.membership" each node is associated with a region id
		shape.info$mems = # shape.membership
				sapply(mep.details[,"Region"], 
						function(region.name) 
							region.shape.ass[[region.name]] # return id
				) 

		shape.info$label.ids = sort(unique(shape.info$mems)) # later, shape ids will give us shape names
	}
	
	return(shape.info)
}



########################################################################
# Prepares plot title which basically contains Imbalance and cluster information.
#
# output.dir.desc: Concatenation of target,domain and period information
# output.dir.basename: MEPs (available MEPs in the concerned graph) details such as country, MEP id etc.
# stats.vec: imbalance and cluster info
#
# returns: plot tile
########################################################################
make.plot.title = function(output.dir.desc, output.dir.basename, stats.vec)
{
	plot.title = 
			paste(
					paste(output.dir.desc, "  |.|  ", output.dir.basename, sep=""),
					"\nI(P)=", stats.vec$cc.imb.value,
					"-%I(P)=", stats.vec$cc.imb.perc,
					", RI(P)=", stats.vec$rcc.imb.value,
					"-%RI(P)=", stats.vec$rcc.imb.perc,
					", #cluster: ", stats.vec$nb.cluster,
					"\n", # treemap'deki plot.title'da tam gozukmedigi icin bunu ekledim
					sep=""
			)
	
	return(plot.title)
}



########################################################################
# Prepares plot filename which depends on 4 options
# - circ.layout.enabled, - node.shape.enabled, 
# - node.id.label.enabled, - imbalance.edge.contribution.enabled
#
# desc: a description related to the name of used partitioning algorithm
# node.shape.enabled: if enabled, node shape is drawn (with the use of legend)
# node.id.label.enabled: if enabled, node ids are shown (inside node circles) on the plot
# imbalance.edge.contribution.enabled: if enabled, node circles which contributes to
#	imbalance will be marked by "O" shape on the plot
#
# returns: plot filename
########################################################################
make.plot.filename = function(desc, circ.layout.enabled=FALSE, 
		node.shape.enabled=TRUE, imbalance.edge.contribution.enabled=FALSE, node.id.label.enabled=FALSE){
	
	filename = paste("graph-", desc, sep="")
	
	if(circ.layout.enabled == TRUE)
		filename = paste(filename, "-","circ-lyt", sep="")
	if(node.shape.enabled == TRUE)
		filename = paste(filename, "-","node-shape", sep="")
	if(node.id.label.enabled == TRUE)
		filename = paste(filename,"-","node-ids",sep="")
	if(imbalance.edge.contribution.enabled == TRUE)
		filename = paste(filename,"-","imb.edge",sep="")
	
	return(filename)
}



########################################################################
# Adds legend which describes the shapes used in the plot.
# src: https://stackoverflow.com/questions/10389967/common-legend-for-multiple-plots-in-r
#
# target.type: "Group" or "State"
# shape.info: contains "shape label ids" and "shape membership"
# 
# returns: -
########################################################################
add.shape.legend.into.plot = function(target.type, shape.info){
	
	shape.label.ids = shape.info[["label.ids"]]
	
	# ======================================================
	# we can think that these 2 variables are constant.
	# we'll choose the corresponding shape type and info with "shape.label.ids"
	SHAPE.SIZES = c(0.8, 0.8, 1.4, 1, 1, 1, 1, 1.4)
	SHAPE.TYPES = c(0, 1, 0, 2, 6, 5, 8, 1) #pch = c(15, 17, 25, 23, 8, 20)) 
	
	shape.list = NA
	if(target.type == "State")
		shape.list = POLITICAL.GROUP.LIST
	else # if(target.type == "Group")
		shape.list = REGION.LIST
	# =======================================================
	
	plot(1, type = "n", axes=FALSE, xlab="", ylab="")
	
	# 0: square, 0: square, 2:triangleUp, 
	# 6:triangleDown, 5:diamond, 8:star, 1:circle, 1:circle
	legend(x = "top",inset = 0,
			legend = shape.list[shape.label.ids], 
			cex=0.5, 
#			xpd = TRUE, 
			horiz = TRUE, 
			pt.cex=SHAPE.SIZES[shape.label.ids], 
			
			# pt.cex allows to change shape size
			pch = SHAPE.TYPES[shape.label.ids] 
	) 
}


########################################################################
# Plot (mono) graph with legend if needed
# src: https://stackoverflow.com/questions/10389967/common-legend-for-multiple-plots-in-r
#
#
# network.path: network path (signed graphml file)
# plot.inputs: the necessary info for plot
#	- output.full.dir.name, - membership, - plot.title, - desc
# target.type: "Group" or "State"
# output.directory: the full path in which results and pdf files are generated
# output.dir.desc: Concatenation of target,domain and period information
# current.MEP.details: MEPs (available MEPs in the concerned graph) details such as country, MEP id etc.
# node.shape.enabled: if enabled, node shape is drawn (with the use of legend)
# node.id.label.enabled: if enabled, node ids are shown (inside node circles) on the plot
# imbalance.edge.contribution.enabled: if enabled, node circles which contributes to
#	imbalance will be marked by "O" shape on the plot
#
# returns: -

# 
########################################################################
plot.graph = function(network.path, plot.inputs, target.type, output.directory, output.dir.desc, current.MEP.details,
		circ.layout.enabled=FALSE, node.shape.enabled=FALSE, imbalance.edge.contribution.enabled=FALSE, node.id.label.enabled=FALSE){
	

    
	plot.path = plot.inputs$output.full.dir.name
	filename = make.plot.filename(plot.inputs$desc, circ.layout.enabled, node.shape.enabled,
			imbalance.edge.contribution.enabled, node.id.label.enabled)
	
	
	pdf(file=paste(plot.path,"/",filename,".pdf", sep=""), bg="white",compress=FALSE)
	
	m = matrix(c(1,2),nrow = 2,ncol = 1,byrow = TRUE)
	heights = c(0.8,0.2)
	layout(mat = m,heights = heights)
	
	# ==========================================================================
	# PLOT
	par(mar = c(2,2,2,1))
	
	shape.info = call.plot(target.type, network.path, plot.inputs$membership, plot.inputs$plot.title, current.MEP.details,
			circ.layout.enabled, node.shape.enabled, imbalance.edge.contribution.enabled, node.id.label.enabled)
	# ==========================================================================
	
	if(node.shape.enabled == TRUE){
		add.shape.legend.into.plot(target.type, shape.info)
	}
	
	dev.off()
}



########################################################################
# Plot multi graph with legend if needed. This function can also plot mono graph
# src: https://stackoverflow.com/questions/10389967/common-legend-for-multiple-plots-in-r
#
#
# network.path: network path (signed graphml file)
# plots.inputs: a list of "plot.inputs". "plot.inputs" is the necessary info for plot 
#	- output.full.dir.name, - membership, - plot.title, - desc
# target.type: "Group" or "State"
# output.directory: the full path in which results and pdf files are generated
# output.dir.desc: Concatenation of target,domain and period information
# current.MEP.details: MEPs (available MEPs in the concerned graph) details such as country, MEP id etc.
# node.shape.enabled: if enabled, node shape is drawn (with the use of legend)
# node.id.label.enabled: if enabled, node ids are shown (inside node circles) on the plot
# imbalance.edge.contribution.enabled: if enabled, node circles which contributes to
#	imbalance will be marked by "O" shape on the plot
#
# returns: -
########################################################################
plot.multigraph = function(desc, network.path, plots.inputs, target.type, output.directory, output.dir.desc, current.MEP.details,
		circ.layout.enabled=FALSE, node.shape.enabled=FALSE, imbalance.edge.contribution.enabled=FALSE, node.id.label.enabled=FALSE){
	
	plot.path = output.directory
	filename = make.plot.filename(desc, circ.layout.enabled, node.shape.enabled,
			imbalance.edge.contribution.enabled, node.id.label.enabled)
	
	############################################################
	# Plot display config 
	n = length(plots.inputs)
	cat("there are",n," plot inputs \n")
	
	m=NA
	heights=NA
	
	nb.row=0
	nb.col=0
	if(n == 1){
		nb.row=1
		nb.col=1
		
		m = matrix(c(1,2),nrow = nb.row+1,ncol = nb.col,byrow = TRUE)
		heights = c(0.7,0.3)
	} else if(n == 2){
		nb.row=1
		nb.col=2
		
		m = matrix(c(1,2,3,3),nrow = nb.row+1,ncol = nb.col,byrow = TRUE)
		heights = c(0.8,0.2)
	} else if(n == 3){
		nb.row=2
		nb.col=2
		
		m = matrix(c(1,2,3,3,4,4),nrow = nb.row+1,ncol = nb.col,byrow = TRUE)
		heights = c(0.4,0.4,0.2)
	} else if(n == 4){
		nb.row=2
		nb.col=2
		
		m = matrix(c(1,2,3,4,5,5),nrow = nb.row+1,ncol = nb.col,byrow = TRUE)
		heights = c(0.4,0.4,0.2)
	} else if(n == 5){
		nb.row=3
		nb.col=2
		
		m = matrix(c(1,2,3,4,5,5,6,6),nrow = nb.row+1,ncol = nb.col,byrow = TRUE)
		heights = c(0.3,0.3,0.3,0.1)
	} else if(n == 6){
		nb.row=3
		nb.col=2
		
		m = matrix(c(1,2,3,4,5,6,7,7),nrow = nb.row+1,ncol = nb.col,byrow = TRUE)
		heights = c(0.3,0.3,0.3,0.1)
	} else {
		print(cat("WARNING: there are",n," plot inputs -- EXIT"))
		return()
	}
	############################################################
	
	
	pdf(file=paste(plot.path,"/",filename,".pdf", sep=""), bg="white",compress=FALSE)
	layout(mat = m,heights = heights)

	# ==========================================================================
	# PLOTS
	shape.info = call.plots(target.type, network.path, plots.inputs, current.MEP.details,
			circ.layout.enabled, node.shape.enabled, imbalance.edge.contribution.enabled, node.id.label.enabled)
	# ==========================================================================
	
	if(node.shape.enabled == TRUE){
		add.shape.legend.into.plot(target.type, shape.info)
	}
	dev.off()
}



########################################################################
# plot a graph. And performs some additional operations which depend on user options
# such as circ.layout.enabled, imbalance.edge.contribution.enabled, etc.
# src: https://stackoverflow.com/questions/10389967/common-legend-for-multiple-plots-in-r
#
#
# target.type: "Group" or "State"
# network.path: network path (signed graphml file)
# membership: mebership file which contains cluster information for each node in the graph
# plot.title: plot title
# current.MEP.details: MEPs (available MEPs in the concerned graph) details such as country, MEP id etc.
# circ.layout.enabled: enable circular layout
# node.shape.enabled: if enabled, node shape is drawn (with the use of legend)
# node.id.label.enabled: if enabled, node ids are shown (inside node circles) on the plot
# imbalance.edge.contribution.enabled: if enabled, node circles which contributes to
#	imbalance will be marked by "O" shape on the plot
#
# returns: -
########################################################################
call.plot = 
		function(target.type, network.path, membership, plot.title, current.MEP.details,
				circ.layout.enabled, node.shape.enabled, imbalance.edge.contribution.enabled, node.id.label.enabled)
{	
	shape.info = NA # output
	
	
	
	############################################################################
	g <- suppressWarnings(read.graph(file=network.path, format="graphml"))
	############################################################################
	
	############################################################################
	if(circ.layout.enabled){ # for circular layout
		if(target.type == "State")
			lyt = layout_in_circle(g, order=order(V(g)$Group))
		else # if(target.type == "Group")
			lyt = layout_in_circle(g, order=order(V(g)$Country))
		
		V(g)$x <- lyt[,1]
		V(g)$y <- lyt[,2]
	}
	############################################################################
	

	shape.membership = NA
	if(node.shape.enabled == TRUE){
		shape.info = prepare.shape.info(target.type, current.MEP.details)
		shape.membership = shape.info[["mems"]]
		shape.label.ids = shape.info[["label.ids"]]
	}
	
	

	neg.imb.contributions=NA
	pos.imb.contributions=NA
	if(imbalance.edge.contribution.enabled == TRUE){
		# TODO I get this info from the algo output file that Mario generates (for ILS)
		# However, Mario does not seem to compute correctly this info. CHECK IT OUT AGAIN
		
#		imb.result = 
#				retreive.pils.imbalance.analysis.result(
#						algo.output.file, 
#						nb.cluster, 
#						vcount(g) # nb node
#				)
#		in.edges.contr = imb.result["in edges contribution"][[1]]
#		out.edges.contr = imb.result["out edges contribution"][[1]]
#		neg.imb.contributions = 
#				in.edges.contr[,"negative.sum"] + out.edges.contr[,"negative.sum"]
#		pos.imb.contributions = 
#				in.edges.contr[,"positive.sum"] + out.edges.contr[,"positive.sum"]
	}
	
	# ==========================================================
	# ==========================================================
	plot.network(g, membership, plot.title, shape.membership, 
			neg.imb.contributions, pos.imb.contributions, node.id.label.enabled)
	# ==========================================================
	# ==========================================================
	
	title(plot.title, cex.main=0.6)
	box(col = 'black')

	return(shape.info)
}



########################################################################
# plot signed graphs based on "plots.inputs" which contains one or more plot inputs.
# It is used in the "plot.multigraph" method
# src: https://stackoverflow.com/questions/10389967/common-legend-for-multiple-plots-in-r
#
#
# target.type: "Group" or "State"
# network.path: network path (signed graphml file)
# plots.inputs: a list of "plot.inputs". "plot.inputs" is the necessary info for plot 
#	- output.full.dir.name, - membership, - plot.title, - desc
# output.directory: the full path in which results and pdf files are generated
# output.dir.desc: Concatenation of target,domain and period information
# current.MEP.details: MEPs (available MEPs in the concerned graph) details such as country, MEP id etc.
# node.shape.enabled: if enabled, node shape is drawn (with the use of legend)
# node.id.label.enabled: if enabled, node ids are shown (inside node circles) on the plot
# imbalance.edge.contribution.enabled: if enabled, node circles which contributes to
#	imbalance will be marked by "O" shape on the plot
#
# returns: -
########################################################################
call.plots = 
		function(target.type, network.path, plots.inputs, current.MEP.details,
				circ.layout.enabled, node.shape.enabled, imbalance.edge.contribution.enabled, node.id.label.enabled)
{	
	# we suppose that shape labels used in all the plots are the same.
	# use the last shape label info among plots inputs
	shape.info = NA
	for(plot.inputs in plots.inputs){
		
		par(mar = c(2,2,2,1))
		
		# FOR loop'ta 4 tane bile plot.inputs olsa, shape label'lar hepsinde ayni olcak
		shape.info = call.plot(target.type, network.path, plot.inputs$membership, plot.inputs$plot.title, current.MEP.details,
				circ.layout.enabled, node.shape.enabled, imbalance.edge.contribution.enabled, node.id.label.enabled)
	}

	return(shape.info)
}


###############################################################################
###############################################################################
# =============================================================================
###############################################################################
###############################################################################


############################################################################
# create more compact graph based on cluster and network information in which each node is a cluster
# It aims at showing easily the connections between clusters and connection features
#   such as negative links, positive links, etc.
# 
# 1) compute edge.mat and clus.mat
# 2) handle isolated nodes (not all the algorithms perform the same procedure for iso nodes)
# 3) for each cluster node, 
#						 3.1) prepare pie proportions for positive, negative and empty (for isolated nodes)
# 						 3.2) prepare node descriptions
# 						 3.3) compute the necessary info (pos, neg, etc.)
#						 3.4) put 2 links between each cluster node pairs (if available):
# 								1st for pos connections and 2nd for negative conn
# 
# network.path: network path (signed graphml file)
# membership: mebership file which contains cluster information for each node in the graph
# current.MEP.details: MEPs (available MEPs in the concerned graph) details such as country, MEP id etc.
# output.full.path: the full path and full name of the pdf file to be generated
#
#
# returns: -
############################################################################
create.cluster.graph = function(target.type, network.path, membership, current.MEP.details, output.full.path){
	# init
	g.compact = NA
	clu.size = NA 
	groups.info = NA
	edge.colors = NA
	weights = NA
	weights.perc = NA
	#edge.widths = NA
	pie.portions = list()
	
	
	g <- suppressWarnings(read.graph(file=network.path, format="graphml"))
	total.abs.weight = sum(abs(E(g)$weight))
	edge.mat <- get.edgelist(g)
	clus.mat <- cbind(membership[edge.mat[,1]], membership[edge.mat[,2]])
	
	nb.clu = get.nb.cluster.from.membership(membership)

	# ==========================================================================
	targets.info = list() # prepare for each cluster
    print(membership)
	for(clu in seq(1, nb.clu)){
	    cat("clu: ", clu, "\n")
		nodes.index.in.clu = which(membership == clu)
		
		# ==========================================================
		# compute pie portions for each cluster node	 
		pos.perc = 0
		neg.perc = 0
		neg.weight= 0
		pos.weight=0
		isolated.node = 0
		
		g.inner = induced.subgraph(g, nodes.index.in.clu)			
		if(!all(degree(g.inner) == 0)){ # if not an isolated node(s) (2 ve ya 3 tane isolated node ayni clusterda da bulunabilir)
			weight.total=sum(abs(E(g.inner)$weight))
			
			pos.edges.indx = which(E(g.inner)$weight > 0)
			pos.weight = sum(E(g.inner)$weight[pos.edges.indx])
			
			neg.edges.indx = which(E(g.inner)$weight < 0)
			neg.weight = sum(E(g.inner)$weight[neg.edges.indx])
			
			pos.perc = pos.weight/weight.total
			neg.perc = 1 - pos.perc
		} else{
			# if isolate.node = 1, and pos.perc = 0, neg.perc = 0 
			# there will be no colored pie for node (i.e. will be only one white whole node circle)
			isolated.node = 1
		}
		pie.portions[[clu]] = c(pos.perc, neg.perc , isolated.node) 
		# ==========================================================
		
		
	
		if(target.type == "State")
			opp.target.col.desc = "Group"
		else # if(target.type == "Group")
			opp.target.col.desc = "State"
		

		target.info.in.clu = current.MEP.details[nodes.index.in.clu, opp.target.col.desc]
		res.table = table(target.info.in.clu)



		
		# ==================
		# prepare description for each node cluster
		s=""
		for(i in seq(1, length(res.table))){ # for each group, retreive its nb member
			if(as.vector(res.table)[i] != 0) # if there is at least 1 available member
				s = paste(s, names(res.table)[i], " (", as.vector(res.table)[i], ")\n", sep="")
		}
		
		
		inner.total.weight = pos.weight + abs(neg.weight)
		s = paste(s, "pos weight: ",roundNumber(pos.weight, 4),
				"( of ", 100*roundNumber(pos.weight/inner.total.weight), "%)", "\n", sep="")
		s = paste(s, "global pos %: ", 100*roundNumber(pos.weight/total.abs.weight, 4),
				"\n", sep="")
		s = paste(s, "neg weight: ", roundNumber(neg.weight, 4), 
				"( of ", 100*roundNumber(abs(neg.weight)/inner.total.weight), "%)", "\n", sep="")
		s = paste(s, "global neg %: ", 100*roundNumber(abs(neg.weight)/total.abs.weight, 4),
				"\n", sep="")
		targets.info[clu] = s
		# ==================

	}

	if(nb.clu > 1){
	    
		pair.list = combn(x=nb.clu, m=2) # Generate all combinations of the elements of ‘x’ taken ‘m’ at a time.
		nb.pair = length(pair.list)/2
		
		# if clu.size = [2, 11], that means:  1.cluster contains 2, 2. cluster contains 11 nodes
		#clu.size = as.vector(table(membership)) 
		
		edgelist.matrix = matrix(NA, 2*nb.pair, 2) # 2*nb.pair for 2 links/edges between 2 nodes( 1st link for neg, 2nd link for pos)
		for(i in seq_len(nb.pair)){
			j = (i-1)*2 + 1 # jump 2 by 2 in order to handle the fact that 2 cluster might have 2 links between them (pos or/and neg)
			clu1 = pair.list[1, i]
			clu2 = pair.list[2, i]
			
			betw.edges = (clus.mat[, 1] == clu1 & clus.mat[, 2] == clu2) | (clus.mat[, 1] == clu2 & clus.mat[, 2] == clu1)
			
			betw.edgelist = E(g)$weight[betw.edges]
			pos.weight.indx = which(betw.edgelist > 0)
			pos.weight = sum(betw.edgelist[pos.weight.indx])
			neg.weight.indx = which(betw.edgelist < 0)
			neg.weight = sum(betw.edgelist[neg.weight.indx])
			
			
			pos.perc = pos.weight/total.abs.weight
			pos.perc = ifelse(is.na(pos.perc), 0, 100*pos.perc)
			neg.perc = abs(neg.weight)/total.abs.weight
			neg.perc = ifelse(is.na(neg.perc), 0, 100*neg.perc)
			
			
			# extra
			if(pos.weight == 0) pos.weight = ""
			else pos.weight = roundNumber(pos.weight, 4) # 4 decimal floating
			
			if(neg.weight == 0) neg.weight = "" 
			else neg.weight = roundNumber(neg.weight, 4) # 4 decimal floating
			
			# extra
			if(pos.perc == 0) pos.perc = ""
			else pos.perc = paste("%", roundNumber(pos.perc, 4)) # 4 decimal floating
			
			if(neg.perc == 0) neg.perc = "" 
			else neg.perc = paste("%", roundNumber(neg.perc, 4)) # 4 decimal floating
			
			
			edgelist.matrix[j, 1:2] = c(clu1, clu2)
			edgelist.matrix[j+1, 1:2] = c(clu1, clu2)
			weights[j] = pos.weight
			weights[j+1] = neg.weight
			
			weights.perc[j] = pos.perc
			weights.perc[j+1] = neg.perc
		}
		
		g.compact = graph.edgelist(edgelist.matrix, directed=FALSE)
		E(g.compact)$weight=as.numeric(weights)
		
		edge.colors <- rep(NA, ecount(g.compact))
		edge.colors[E(g.compact)$weight<0] <- adjustcolor("RED", alpha.f=0.5)
		edge.colors[E(g.compact)$weight>0] <- adjustcolor("GREEN", alpha.f=0.5)
		#edge.widths <- abs(E(g.compact)$weight)*10
		
		
	} else{ # if(nb.clu == 1){
		
		## I think it is the easist way to create a graph with 1 isolated node
		g.compact = random.graph.game(n=1, p.or.m=0) 
		pdf(file=output.full.path, bg="white",compress=FALSE)
		plot(
				g.compact, 	
				vertex.shape="pie",
				vertex.pie=pie.portions, 
				vertex.pie.color= list(c("green", "red", "white")),
				vertex.size=10, 
				#vertex.size=(clu.size*2), 
				vertex.label=targets.info, 
				vertex.label.cex=0.7
		#vertex.color=vertex.colors, 
		#vertex.label.color=vertex.label.colors, 
		#vertex.label.cex=vertex.label.cexs
		)
		dev.off()
		return()

	}
	

	# ================================================================
	# Plot
	# ================================================================
	
	# circular spatialization in order to put some space between the nodes 
	coords = layout_in_circle(g.compact)
	print(output.full.path)
	pdf(file=output.full.path, bg="white",compress=FALSE)
	
	plot(
			g.compact, 	
			layout =  coords,
			vertex.shape="pie",
			vertex.pie=pie.portions, 
			vertex.pie.color= list(c("green", "red", "white")),
			vertex.size=10, 
			#vertex.size=(clu.size*2), 
			vertex.label=targets.info, 
			vertex.label.cex=0.7,
			#vertex.color=vertex.colors, 
			#vertex.label.color=vertex.label.colors, 
			#vertex.label.cex=vertex.label.cexs,
			edge.color=edge.colors,
			edge.label=paste(weights, weights.perc, sep="\n"), # alt alta yazdir
			edge.label.cex=0.6,
#			edge.curved=TRUE
			edge.curved=rep(c(0.4, 0.7), ecount(g.compact)/2)
#			edge.width=edge.widths, 
	)
	
# legend("bottom",
# 		legend = shape.list[shape.label.ids],
# 		cex=0.8,
# 		xpd = TRUE,
# 		horiz = TRUE,
# 		pt.cex=SHAPE.SIZES[shape.label.ids],
# 
# 		# pt.cex allows to change shape size
# 		pch = SHAPE.TYPES[shape.label.ids]
# )
	
	dev.off()
	
}

