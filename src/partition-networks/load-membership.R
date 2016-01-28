#############################################################################################
# Loads membership vectors processed using external tools.
# 
# 01/2016 Vincent Labatut (v2)
#############################################################################################
source("src/define-constants.R")



#############################################################################################
# Loads the partition estimated by the external tools (their file format needs a specific
# conversion processing).
#
# file.name: the path and name of the file to load.
# algo.name: code representing the concerned (external) partitioning algorithm.
# returns: the corresponding partition as a membership vector.
#############################################################################################
load.external.partition <- function(file.name, algo.name)
{	if(algo.name==CORCLU.ALGO.PILS)
		result <- load.pils.partition(file.name)
	else
	{	# TODO treat other external tools here
	}
	
	return(result)
}



#############################################################################################
# Loads the partition estimated by the pILS tool.
# 
# file.name: the path and name of the file to load.
# returns: the corresponding partition as a membership vector.
#############################################################################################
load.pils.partition <- function(file.name, algo.name)
{	# open and read the file
	con <- file(file.name, "r")
	lines <- readLines(con)
	close(con)

	# process the file content
	i <- 4
	line <- lines[i]
	res <- list()
	while(line!="")
	{  # process current line
		#print(line)
		line <- strsplit(x=line, "[ ", fixed=TRUE)[[1]][2]
		line <- strsplit(x=line, " ]", fixed=TRUE)[[1]][1]
		nodes <- as.integer(strsplit(x=line," ", fixed=TRUE)[[1]]) + 1 # plus one because C++ starts counting from 0
		res[[length(res)+1]] <- nodes
		
		# process next line
		i <- i + 1
		line <- lines[i]      
	}
	
	# build the membership vector
	mx <- max(unlist(res))
	membership <- rep(NA,mx)
	for(i in 1:length(res))
	{  nodes <- res[[i]]
		membership[nodes] <- i 
	}
	
#	# record the partition using the internal format
#	write.table(x=membership, file=partition.file, row.names=FALSE, col.names=FALSE)
	
	return(membership)
}
