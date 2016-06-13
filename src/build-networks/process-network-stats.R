#############################################################################################
# Process various stats on the signed networks extracted from the vote data. All stats are
# recorded in text files, some of them are also plotted.
# 
# 07/2015 Israel Mendonça (v1)
# 11/2015 Vincent Labatut (v2)
#############################################################################################
library("igraph")



#############################################################################################
# Constants use to define table column names
#############################################################################################
COL.NAMES <- c()
COL.ORIGINAL.GRAPH <- "Original"
COL.NAMES <- c(COL.NAMES,COL.ORIGINAL.GRAPH)
COL.POSITIVE.GRAPH <- "Positive"
COL.NAMES <- c(COL.NAMES,COL.POSITIVE.GRAPH)
COL.NEGATIVE.GRAPH <- "Negative"
COL.NAMES <- c(COL.NAMES,COL.NEGATIVE.GRAPH)


#############################################################################################
# Constants use to define table row names
#############################################################################################
ROW.NAMES <- c()
# nodes
	ROW.NODE.COUNT <- "Number of nodes"
	ROW.NAMES <- c(ROW.NAMES,ROW.NODE.COUNT)
	ROW.ISOLATE.COUNT <- "Number of isolates"
	ROW.NAMES <- c(ROW.NAMES,ROW.ISOLATE.COUNT)
	ROW.ISOLATE.PROP <- "Proportion of isolates"
	ROW.NAMES <- c(ROW.NAMES,ROW.ISOLATE.PROP)
# links
	ROW.LINK.COUNT <- "Number of links"
	ROW.NAMES <- c(ROW.NAMES,ROW.LINK.COUNT)
	ROW.LINK.PROP <- "Proportion of links"
	ROW.NAMES <- c(ROW.NAMES,ROW.LINK.PROP)
# densities
	ROW.DENSITY <- "Density"
	ROW.NAMES <- c(ROW.NAMES,ROW.DENSITY)
	ROW.TRANSITIVITY.GLOBAL <- "Global transitivity"
	ROW.NAMES <- c(ROW.NAMES,ROW.TRANSITIVITY.GLOBAL)
	ROW.TRANSITIVITY.LOCAL <- "Average local transitivity"
	ROW.NAMES <- c(ROW.NAMES,ROW.TRANSITIVITY.LOCAL)
# components
	ROW.COMPONENT.COUNT <- "Number of components"
	ROW.NAMES <- c(ROW.NAMES,ROW.COMPONENT.COUNT)
	ROW.COMPONENT.SIZE <- "Giant component size"
	ROW.NAMES <- c(ROW.NAMES,ROW.COMPONENT.SIZE)
	ROW.COMPONENT.PROP <- "Giant component proportion"
	ROW.NAMES <- c(ROW.NAMES,ROW.COMPONENT.PROP)
# connectivity
	ROW.CONNECTIVITY.LINK <- "Link-connectivity"
	ROW.NAMES <- c(ROW.NAMES,ROW.CONNECTIVITY.LINK)
	ROW.CONNECTIVITY.NODE <- "Node-connectivity"
	ROW.NAMES <- c(ROW.NAMES,ROW.CONNECTIVITY.NODE)
# degree
	ROW.DEGREE.AVERAGE <- "Average degree"
	ROW.NAMES <- c(ROW.NAMES,ROW.DEGREE.AVERAGE)
	ROW.DEGREE.STDEV <- "Degree stdev"
	ROW.NAMES <- c(ROW.NAMES,ROW.DEGREE.STDEV)
	ROW.DEGREE.MIN <- "Minimal degree"
	ROW.NAMES <- c(ROW.NAMES,ROW.DEGREE.MIN)
	ROW.DEGREE.MAX <- "Maximal degree"
	ROW.NAMES <- c(ROW.NAMES,ROW.DEGREE.MAX)
	ROW.DEGREE.CORRELATION <- "Degree correlation"
	ROW.NAMES <- c(ROW.NAMES,ROW.DEGREE.CORRELATION)
# strength
	ROW.STRENGTH.AVERAGE <- "Average strength"
	ROW.NAMES <- c(ROW.NAMES,ROW.STRENGTH.AVERAGE)
	ROW.STRENGTH.STDEV <- "Strength stdev"
	ROW.NAMES <- c(ROW.NAMES,ROW.STRENGTH.STDEV)
	ROW.STRENGTH.MIN <- "Minimal strength"
	ROW.NAMES <- c(ROW.NAMES,ROW.STRENGTH.MIN)
	ROW.STRENGTH.MAX <- "Maximal strength"
	ROW.NAMES <- c(ROW.NAMES,ROW.STRENGTH.MAX)
	ROW.STRENGTH.CORRELATION <- "Strength correlation"
	ROW.NAMES <- c(ROW.NAMES,ROW.STRENGTH.CORRELATION)
# distances


#############################################################################################
# Processes global measures describing the specified graph.
#
# g: the graph to process (not necessarily the signed graph).
# g.ref: reference graph (the signed graph).
# result: the table which will be completed by this function.
# col.name: name of the column corresponding to g, in the result table.
#
# returns: the modified table.
#############################################################################################
process.global.measures <- function(g, g.ref, result, col.name)
{	if(vcount(g)==0 | ecount(g)==0)
		tlog("..........WARNING: empty graph (",col.name,") >> cannot process any topological measure.")
	else
	{	# common processing
		deg <- degree(g)
		g.clean <- delete.vertices(graph=g, v=which(deg==0))
		deg.clean <- degree(g.clean)
		str.clean <- graph.strength(g.clean)
		
		# numbers of nodes
		tlog("..........Processing number of nodes")
		result[ROW.NODE.COUNT,col.name] <- vcount(g)
		result[ROW.ISOLATE.COUNT,col.name] <- length(which(deg==0))
		result[ROW.ISOLATE.PROP,col.name] <- length(which(deg==0))/vcount(g)
		
		# numbers of links
		tlog("..........Processing number of links")
		result[ROW.LINK.COUNT,col.name] <- ecount(g)
		result[ROW.LINK.PROP,col.name] <- ecount(g)/ecount(g.ref)
			
		### from now on, we ignore isolates
		
		# densities
		tlog("..........Processing densities")
		result[ROW.DENSITY,col.name] <- graph.density(g.clean)
		result[ROW.TRANSITIVITY.GLOBAL,col.name] <- transitivity(g.clean, type="globalundirected")
		result[ROW.TRANSITIVITY.LOCAL,col.name] <- transitivity(g.clean, type="localaverageundirected")
		
		# components
		tlog("..........Processing components")
		tmp <- clusters(g.clean)
		#print(g.clean)		
		#print(tmp)		
		result[ROW.COMPONENT.COUNT,col.name] <- tmp$no
		result[ROW.COMPONENT.SIZE,col.name] <- max(tmp$csize)
		result[ROW.COMPONENT.PROP,col.name] <- max(tmp$csize) / vcount(g.clean)
		
#		# connectivity
#		tlog("..........Processing connectivity")
#		result[ROW.CONNECTIVITY.LINK,col.name] <- graph.adhesion(g.clean,checks=FALSE)
#		result[ROW.CONNECTIVITY.NODE,col.name] <- graph.cohesion(g.clean,checks=FALSE)
		
		# degree
		tlog("..........Processing degree")
		result[ROW.DEGREE.AVERAGE,col.name] <- mean(deg.clean)
		result[ROW.DEGREE.STDEV,col.name] <- sd(deg.clean)
		result[ROW.DEGREE.MIN,col.name] <- min(deg.clean)
		result[ROW.DEGREE.MAX,col.name] <- max(deg.clean)
		result[ROW.DEGREE.CORRELATION,col.name] <- assortativity.degree(g.clean)
		
		# strength
		tlog("..........Processing strength")
		result[ROW.STRENGTH.AVERAGE,col.name] <- mean(str.clean)
		result[ROW.STRENGTH.STDEV,col.name] <- sd(str.clean)
		result[ROW.STRENGTH.MIN,col.name] <- min(str.clean)
		result[ROW.STRENGTH.MAX,col.name] <- max(str.clean)
		result[ROW.STRENGTH.CORRELATION,col.name] <- assortativity (g.clean, types1=str.clean, types2=NULL, directed=FALSE)
		
		# distances // eccentricity
		# TODO could add other measures here
	}

	return(result)
}


#############################################################################################
# Process a set of topological measures for the signed graph, as well as the positive and
# negative ones. The resulting values are recorded in a table.
#
# g: the graphe to process.
# folder: folder in which to place the produced files.
#
# returns: the property table.
#############################################################################################
process.network.stats <- function(g, folder)
{	# init result
	result <- matrix(NA,nrow=length(ROW.NAMES),ncol=length(COL.NAMES))
	rownames(result) <- ROW.NAMES
	colnames(result) <- COL.NAMES
	
	# process original graph
	result <- process.global.measures(g, g, result, col.name=COL.ORIGINAL.GRAPH)
	
	# process positive graph
	pos.g <- subgraph.edges(graph=g, eids=which(E(g)$weight>0), delete.vertices=FALSE)
	result <- process.global.measures(pos.g, g, result, col.name=COL.POSITIVE.GRAPH)
	
	# process negative graph
	neg.g <- subgraph.edges(graph=g, eids=which(E(g)$weight<0), delete.vertices=FALSE)
	result <- process.global.measures(neg.g, g, result, col.name=COL.NEGATIVE.GRAPH)
	
	# TODO degree distribution (maybe add a special function to handle distributions (in general) and produce plots)
	
	# record result
	table.file <- file.path(folder,"properties.csv")
	write.csv2(result, file=table.file, row.names=TRUE)	
	
	return(result)
}
