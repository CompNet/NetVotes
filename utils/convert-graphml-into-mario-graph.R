# TODO: Add comment
# 
# Author: nejat
###############################################################################

##############################################################################
# convert "graphml" graph file into ".G" graph file.
# ".G" graph file is used by Mario
###############################################################################
convert.graphml.into.mario.graph = function(g, mario.graph.filename){
	first.line = paste(vcount(g), ecount(g),sep="\t")
	write.graph(g, mario.graph.filename, format="ncol")
	lines = readLines(con = mario.graph.filename)
	lines2=sapply(lines, function(x) gsub(" ", "\t", x, fixed = TRUE))
	write.table(first.line, mario.graph.filename,row.names=F, col.names=F, quote=F)
	write.table(lines2, mario.graph.filename,row.names=F, col.names=F, append=T, quote=F)
}