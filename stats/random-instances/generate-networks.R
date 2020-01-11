# TODO: Add comment
# 
# Author: nejat
###############################################################################

library(package="igraph")
source("stats/define-consts.R")

dir.create(DIR.RANDOM.INSTANCES, showWarnings = TRUE)
dir.create(DIR.RANDOM.GENERATED.NETWORKS, showWarnings = TRUE)

# to remove isolated nodes:
# g <- delete.vertices(g, V(g)[ degree(g)==0 ])

output.path = DIR.RANDOM.GENERATED.NETWORKS
tmp.graph.filename = paste(DIR.RANDOM.INSTANCES, "/temp.txt", sep="")

g = suppressWarnings(read.graph(file=BASE.GRAPH.FILENAME, format="graphml"))

for(graph.size in seq(from=STEP, to=MAX.G.SIZE, by=STEP)){
	random.node.ids = sample(x=MAX.G.SIZE, size=graph.size)
	subgraph = induced.subgraph(g, random.node.ids)
	
	# ====================================================
	# write graph in graphml format
	filename = paste(output.path,"/signed", graph.size, ".graphml", sep="")
	write.graph(graph=subgraph,file=filename,format="graphml")
	# ====================================================
	
	# =====================================================
	# write graph in .G format (i.e. Mario's graph format)
	first.line = paste(vcount(subgraph), ecount(subgraph),sep="\t")
	write.graph(subgraph, tmp.graph.filename, format="ncol")
	lines = readLines(con = tmp.graph.filename)
	lines2=sapply(lines, function(x) gsub(" ", "\t", x, fixed = TRUE))
	#delete temp file
	unlink(tmp.graph.filename)
	
	filename = paste(output.path,"/signed", graph.size, ".G", sep="")
	write.table(first.line, filename,row.names=F, col.names=F, quote=F)
	write.table(lines2, filename,row.names=F, col.names=F, append=T, quote=F)
	# =====================================================
}
