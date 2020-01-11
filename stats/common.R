# TODO: Add comment
# 
# Author: nejat
###############################################################################

source("stats/define-consts.R")



###############################################################################
#
###############################################################################
prepare.curr.row.desc.part.for.real.instances = function(target, domain, period, filtering.threshold){
	
	b.network.filtering = ifelse(filtering.threshold == "NA", 1, 0)
	curr.row.desc.part = data.frame(target, domain, period, b.network.filtering)
	
	return(curr.row.desc.part)
}


###############################################################################
#
###############################################################################
prepare.curr.row.desc.part.for.random.instances = function(graph.size){
	
	curr.row.desc.part = data.frame(graph.size)
	return(curr.row.desc.part)
}



###############################################################################
#
###############################################################################
insert.curr.row.into.graph.stats.csv = function(curr.row.desc.part, dir.path, filename, network.path.graphml, filtering.threshold=NA){

	s.network.filtering = ""
	if(!is.na(filtering.threshold))
		s.network.filtering = ifelse(filtering.threshold == "NA", "filtered-", "original-")
	
	links.weights = extract.graph.links.and.weights.stats(network.path.graphml)
	conn.comp = do.connected.component.analysis(network.path.graphml)		
	density = get.graph.density(network.path.graphml)
	iso.nodes.indx = get.isolated.nodes.indx(network.path.graphml)
	graph.size = get.network.size(network.path.graphml)
		
	curr.row.part = data.frame(
			links.weights,
			conn.comp,
			density,
			s.iso.nodes.indx = paste(iso.nodes.indx, collapse=" "),
			graph.size
	)
	curr.row = data.frame(curr.row.desc.part, curr.row.part)
	
	filename = paste(s.network.filtering, filename, sep="")
	filepath = paste(dir.path, filename, sep="/")
	if(!file.exists(filepath))
		write.table(x=curr.row, file=filepath, sep=",", append = FALSE, row.names=FALSE)
	else
		write.table(x=curr.row, file=filepath, sep=",", append = TRUE, row.names=FALSE, col.names=FALSE)
}



###############################################################################
#
###############################################################################
insert.curr.row.into.partitioning.stats.csv = function(curr.row.desc.part, dir.path, filename, result, filtering.threshold=NA){
	
	s.network.filtering = ""
	if(!is.na(filtering.threshold))
		s.network.filtering = ifelse(filtering.threshold == "NA", "filtered-", "original-")
	
	
	exec.time = result$exec.time
	stats.vec = result$stats.vec
	plot.inputs = result$plot.inputs
	
	curr.row.part = data.frame(
			exec.time = exec.time,
			imbalance.value = stats.vec$cc.imb.value,
			imbalance.percentage = stats.vec$cc.imb.perc,
			sPartition = paste(plot.inputs$membership, collapse=" ")
	)
	curr.row = data.frame(curr.row.desc.part, curr.row.part)
	
	filename = paste(s.network.filtering, filename, sep="")
	filepath = paste(dir.path, filename, sep="/")
	if(!file.exists(filepath))
		write.table(x=curr.row, file=filepath, sep=",", append = FALSE, row.names=FALSE)
	else
		write.table(x=curr.row, file=filepath, sep=",", append = TRUE, row.names=FALSE, col.names=FALSE)
}
