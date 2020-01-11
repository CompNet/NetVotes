# TODO: Add comment
# 
# Author: nejat
###############################################################################
source("stats/define-consts.R")
source("stats/common.R")


source("src/define-consts.R")
source("src/define-ils.R")
source("src/define-ExCC.R")
source("src/define-parameter-set.R")

output.path = DIR.RANDOM.INSTANCES.CSV
dir.create(output.path, showWarnings=TRUE)

g = suppressWarnings(read.graph(file=BASE.GRAPH.FILENAME, format="graphml"))


for(graph.size in seq(from=STEP, to=MAX.G.SIZE, by=STEP)){
	# read graph
	network.path.G = paste(DIR.RANDOM.GENERATED.NETWORKS,"/signed", graph.size, ".G", sep="")
	network.path.graphml = paste(DIR.RANDOM.GENERATED.NETWORKS,"/signed", graph.size, ".graphml", sep="")
	
				
	####################################################################
	# EXCC
	####################################################################

	desc = ExCC
	result = perform.ExCC(worker.id = NA, desc = desc,
			output.directory = DIR.RANDOM.INSTANCES, output.dir.desc = "",
			target.name = NA, network.path.G = network.path.G,
			network.path.graphml = network.path.graphml, current.MEP.details = NA,
			LOG.ENABLED = FALSE,
			RUNNING.PARTITIONING.ALGOS.ENABLED = TRUE,
			UPDATING.GRAPHML.CONTENT.ENABLED = FALSE,
			PLOTTING.ENABLED = FALSE)

	exec.time = result$exec.time
	stats.vec = result$stats.vec
	plot.inputs = result$plot.inputs
	
	# ========================================================================
	# extract.partitioning.method.stats
	desc.part = prepare.curr.row.desc.part.for.random.instances(graph.size)
	filename = EXCC.STATS.CSV.FILENAME
	dir.path = DIR.RANDOM.INSTANCES.CSV
	insert.curr.row.into.partitioning.stats.csv(desc.part, dir.path, filename, result)
	# ========================================================================
	
	# ==============================================================
	# delete the temporary dir where the result file created
	unlink(plot.inputs$output.full.dir.name, recursive = TRUE)
	# ==============================================================
	
	
	###################################################################
	# ILS
	###################################################################	
	
	# ==============================================================
	# to handle the method 'keep.only.necessary.ils.output.files()':
	# move 'signedXX.G' and 'signed.XX.graphml' into DIR.RANDOM.INSTANCES by removing 'XX' 
	network.path.G.temp = paste(DIR.RANDOM.INSTANCES,SIGNED.GRAPH.FILENAME,sep="/")
	network.path.graphml.temp = paste(DIR.RANDOM.INSTANCES,GRAPHML.NETWORK.FILENAME,sep="/")
	file.copy(network.path.G, network.path.G.temp, 
			overwrite=TRUE, recursive=FALSE, copy.mode=TRUE, copy.date=FALSE)
	file.copy(network.path.graphml, network.path.graphml.temp, 
			overwrite=TRUE, recursive=FALSE, copy.mode=TRUE, copy.date=FALSE)
	# ======================================================================

	desc = ILSCC
	result = perform.ils.cc(worker.id = NA, desc = desc,
			output.directory = DIR.RANDOM.INSTANCES, output.dir.desc = "",
			target.name = NA, network.path.G = network.path.G.temp,
			network.path.graphml = network.path.graphml.temp, network.size = graph.size, 
			current.MEP.details = NA,
			LOG.ENABLED = FALSE,
			RUNNING.PARTITIONING.ALGOS.ENABLED = TRUE,
			UPDATING.GRAPHML.CONTENT.ENABLED = FALSE,
			PLOTTING.ENABLED = FALSE)
	
	exec.time = result$exec.time
	stats.vec = result$stats.vec
	plot.inputs = result$plot.inputs
		
	
	# ========================================================================
	# extract.partitioning.method.stats
	desc.part = prepare.curr.row.desc.part.for.random.instances(graph.size)
	filename = ILSCC.STATS.CSV.FILENAME
	dir.path = DIR.RANDOM.INSTANCES.CSV
	insert.curr.row.into.partitioning.stats.csv(desc.part, dir.path, filename, result)
	# ========================================================================
	
	
	# ==============================================================
	# delete the temporary dir where the result file created
	unlink(plot.inputs$output.full.dir.name, recursive = TRUE)
	unlink(network.path.G.temp, recursive = FALSE)
	unlink(network.path.graphml.temp, recursive = FALSE)
	# ==============================================================
	
}
