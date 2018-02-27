#############################################################################################
# setwd("~/eclipse/workspaces/Networks/NetVotes")
# source("src/gafes.R")
#############################################################################################
library("igraph")

source("src/plot-tools/plot-networks.R")


# cst
data.path <- "~/Downloads"
#file.name <- "full_simila"
#file.name <- "cormat"
#file.name <- "similarities_trans"
#file.name <- "similarities_avignon"
file.name <- "similarities_all"


# load correlation matrix
mat <- data.matrix(read.csv2(file.path(data.path,paste0(file.name,".csv")),header=TRUE,as.is=TRUE)[-1])
#mat <- data.matrix(read.csv(file.path(data.path,paste0(file.name,".csv")),header=TRUE,as.is=TRUE)[-1])
diag(mat) <- 0

# build graph
g <- graph_from_adjacency_matrix(adjmatrix=mat, mode="undirected", weighted=TRUE, add.colnames=TRUE)
plot.network(g, membership=NA, plot.file=NA, format=NA)

# record graphml graph
g2 <- g
E(g2)$sign <- sign(E(g2)$weight)
E(g2)$weight <- abs(E(g2)$weight)
write.graph(g2,file.path(data.path,paste0(file.name,".graphml")),format="graphml")

# record .G graph
t <- get.edgelist(graph=g) - 1	# start numbering nodes at zero
t <- cbind(t,E(g)$weight)		# add weights as the third column
write.table(data.frame(vcount(g),ecount(g)), file=file.path(data.path,paste0(file.name,".G")), append=FALSE, sep="\t", row.names=FALSE, col.names=FALSE)	# write header
write.table(t, file.path(data.path,paste0(file.name,".G")), append=TRUE, sep="\t", row.names=FALSE, col.names=FALSE)										# write proper graph

# apply the partitioning algo
cmd <- paste0("mpirun -n 1 /home/vlabatut/eclipse/workspaces/Networks/NetVotes/lib/mestrado/grasp/build/graspcc --alpha 1 --iterations 500 --neighborhood_size 1 --rcc 0 --k 7 --time-limit 1800 --input-file ",
		"\"/home/vlabatut/Downloads/",file.name,".G\" --output-folder ",
		"\"/home/vlabatut/Downloads/output\" --gain-function-type 0 --strategy GRASP --perturbationLevelMax 30")
system(cmd)
