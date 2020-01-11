# TODO: Add comment
# 
# Author: nejat
###############################################################################

if(LOG.ENABLED)
	library(package="treemap", lib.loc=LIBRARY.LOC.PATH)


plot.treemap = 
		function(network.path, plot.inputs, target.type, target.name)
{	
	#################################################################################
	# filter mep data. Remove mep data if it was not available during a specific period
	g.ml <- suppressWarnings(read.graph(file=network.path, format="graphml"))
	current.mep = as.numeric(vertex.attributes(g.ml)$MEPid)
	index.current.meps = which(MEP.DETAILS[, "MEP Id"] %in% current.mep)
	current.mep.details = MEP.DETAILS[index.current.meps,]
	#print(network.path)
	print(current.mep)
	print(index.current.meps)
	
	mep.indx = which(current.mep.details[,target.type] %in% target.name)
	current.mep.target.details = current.mep.details[mep.indx,]
	#################################################################################

	col.name = NA
	if(target.type == "State"){
		col.name = "Group"
	} else if(target.type == "Group"){
		col.name = "State"
	}
	
	my.df = current.mep.target.details[, c("MEP Id", col.name)]
	my.df$partitions = plot.inputs$membership
	my.df$partition.labels = paste(my.df[ ,col.name], my.df$partitions, sep="-")
	
	treemap.input = 
			aggregate(
					rep(1, length(my.df$partitions)), 
					by=list(my.df$partitions, my.df[ ,col.name]), 
					sum
			)
	
	colnames(treemap.input) = c("partition.label", "target", "member.size")
	
	# ==========================================================================
	# TODO maybe, you can put 'white' color for the rectangle related to isolated nodes
	# workaround in order to visualize isolated nodes info
	indx = which(treemap.input$partition.label == CLU.NO.FOR.ALL.ISOLATED.NODES)
	treemap.input$partition.label[indx] = "_isolated"
	# =========================================================================
	
	treemap.input$partition.label = 
			paste("C", treemap.input$partition.label, sep="")
	
	treemap.input$desc = 
			paste(
					treemap.input$target, 
					paste("(", treemap.input$member.size, ")", sep=""), 
					paste("\n", treemap.input$partition.label, sep=""), 
					sep=" "
			)

	# =======================================================

	filename = paste("treemap-", plot.inputs$desc, sep="")
	
	pdf(
		file=paste(plot.inputs$output.full.dir.name,"/",filename,".pdf", sep=""), 
		bg="white",
		compress=FALSE
	)
	

	treemap(
		treemap.input, 
		index=c("partition.label", "desc"), 
		vSize="member.size", 
		vColor="partition.label", 
		type="categorical", 
		format.legend = list(scientific = FALSE, big.mark = " "), 
		palette="RdYlBu", 
		title=plot.inputs$plot.title, 
		fontsize.title=8, 
		title.legend="Cluster ID", 
		fontsize.labels=10
	)
	dev.off()
}
