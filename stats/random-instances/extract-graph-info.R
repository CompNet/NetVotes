# TODO: Add comment
# 
# Author: nejat
###############################################################################
source("stats/define-consts.R")
source("src/define-graph-operations.R")

output.path = DIR.RANDOM.INSTANCES.CSV
dir.create(output.path, showWarnings=TRUE)

g = suppressWarnings(read.graph(file=BASE.GRAPH.FILENAME, format="graphml"))

for(graph.size in seq(from=STEP, to=MAX.G.SIZE, by=STEP)){
	# read graph
	network.path.graphml = paste(DIR.RANDOM.GENERATED.NETWORKS,"/signed", graph.size, ".graphml", sep="")
	
	# ========================================================================
	# extract.partitioning.method.stats
	desc.part = prepare.curr.row.desc.part.for.random.instances(graph.size)
	filename = GRAPH.STR.STATS.CSV.FILENAME
	dir.path = DIR.RANDOM.INSTANCES.CSV
	insert.curr.row.into.graph.stats.csv(desc.part, dir.path, filename, network.path.graphml)
	# ========================================================================
}