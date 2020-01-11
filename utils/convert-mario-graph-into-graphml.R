# TODO: Add comment
# 
# Author: nejat
###############################################################################


#!/usr/bin/Rscript
library(package="igraph")

#sPartition = df[, "sPartition"]
#partition = lapply(sPartition, function(x) unlist( strsplit(as.character(x), " ") ))

g.sizes = seq(from=10, to=850, by=10)
graph.files.dir = "generated-networks-original"

for(graph.size in g.sizes){
	
	# ===================================================================================
	filename = paste("stats/", graph.files.dir, "/signed", graph.size, ".G", sep="")
	con <- file(filename, "r")
	lines <- readLines(con)
	close(con)
	
	filename2=paste("stats/", graph.files.dir, "/signed", graph.size,".ncol", sep="")
	write(lines[2:length(lines)], file = filename2 , append = FALSE)
	
	g=read.graph(filename2, format="ncol")
	filename3=paste("stats/", graph.files.dir, "/signed", graph.size,".graphml", sep="")
	
	write.graph(g, filename3, format="graphml")
	
	# remove "*.ncol" files ==> unnecessary, there were just intermediate files
	unlink(filename2, recursive=FALSE)
	# ===================================================================================
	
	
	# compute.imbalance.from.membership(network.path, membership, output.type = "value")
}
