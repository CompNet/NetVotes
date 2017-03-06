#############################################################################################
# These functions build the normalized names of the algorithms handled in NetVotes.
# 
# 03/2017 Vincent Labatut
#############################################################################################


#############################################################################################
# Unsigned networks partitioning algorithms info
#############################################################################################
COMDET.ALGO.VALUES <- c()
COMDET.ALGO.NAMES <- c()
COMDET.ALGO.EDGEBETW <- "EB"
	# this implementation uses the weights and directions, if present
	COMDET.ALGO.VALUES <- c(COMDET.ALGO.VALUES, COMDET.ALGO.EDGEBETW)
	COMDET.ALGO.NAMES[COMDET.ALGO.EDGEBETW] <- "EdgeBetweenness"
COMDET.ALGO.INFOMAP <- "IM"
	# this implementation uses the weights and directions, if present
	COMDET.ALGO.VALUES <- c(COMDET.ALGO.VALUES, COMDET.ALGO.INFOMAP)
	COMDET.ALGO.NAMES[COMDET.ALGO.INFOMAP] <- "InfoMap"
COMDET.ALGO.LABELPROP <- "LP"
	# this implementation uses the weights and directions, if present
	COMDET.ALGO.VALUES <- c(COMDET.ALGO.VALUES, COMDET.ALGO.LABELPROP)
	COMDET.ALGO.NAMES[COMDET.ALGO.LABELPROP] <- "LabelPropagation"
COMDET.ALGO.LOUVAIN <- "LV"
	# this implementation uses the weights, if present, but cannot use directions
	COMDET.ALGO.VALUES <- c(COMDET.ALGO.VALUES, COMDET.ALGO.LOUVAIN)
	COMDET.ALGO.NAMES[COMDET.ALGO.LOUVAIN] <- "Louvain"
COMDET.ALGO.WALKTRAP <- "WT"
	# this implementation uses the weights, if present, and simply ignores directions
	COMDET.ALGO.VALUES <- c(COMDET.ALGO.VALUES, COMDET.ALGO.WALKTRAP)
	COMDET.ALGO.NAMES[COMDET.ALGO.WALKTRAP] <- "WalkTrap"
comdet.algo.ncg.value <- function(value) paste("NCG",value,sep="-")	# returns the negative complementary value (i.e. short code) associated to the specified (positive) value
for(value in COMDET.ALGO.VALUES) COMDET.ALGO.NAMES[comdet.algo.ncg.value(value)] <- paste("NCG",COMDET.ALGO.NAMES[value])
#TODO maybe we should allow parameters there too? (ie for regular community detection methods)


#############################################################################################
# Returns the code (short name) for the Iterated Local Search (ILS) partioning method. See 
# the algorithm documentation for more details.
#
# rcc: whether to solve the correlation clustering (FALSE) or relaxed CC problem (TRUE).
# l: neighborhood size (during the local search).
# k: number of clusters (max number for RCC)
# alpha: randomness factor.
# gain: 0 for min imbalance, 
#       1 for max modularity gain function, 
#       2 for max negative modularity gain function, 
#       3 for max positive-negative modularity gain function, 
#       4 for max pos-neg mod gain function II, 
#       5 for max pos-neg mod gain function III
# perturbation: maximal level of perturbation.
#
# returns: the short name corresponding to the ILS method with the specified parameters.
#############################################################################################
get.ils.code <- function(l, k, alpha, rcc, gain, perturbation)
{	result <- "ILS"
	
	if(rcc)
		result <- paste0(result,"-RCC")
	else
		result <- paste0(result,"-CC")
	
	result <- paste0(result,"_l",l)
	result <- paste0(result,"_k",k)
	result <- paste0(result,"_a",alpha)
	result <- paste0(result,"_g",gain)
	result <- paste0(result,"_p",perturbation)
	
	return(result)
}


#############################################################################################
# Returns the inline command for the Iterated Local Search (ILS) partioning method. See 
# the algorithm documentation for more details.
#
# algo.name: short code associated to the algorithm.
# input.file: complete path to the targeted graph file.
# output.folder: complete path to the folder in which the produced files will be placed.
# time.limit: maximum duration of the processing.
# iter.nbr: maximum number of iterations of the processing.
#
# returns: the command allowing to invoke the program externally.
#############################################################################################
get.ils.command <- function(algo.name, input.file, output.folder, time.limit=1800, iter.nbr=500)
{	# init
	command.folder <- file.path(LIB.FOLDER,"mestrado","grasp","build")
	result <- file.path(command.folder, "graspcc")
	params <- c()
	
	# break down the specified code (short name)
	tmp <- strsplit(x=algo.name, split="_", fixed=TRUE)[[1]]
	params <- sapply(tmp[2:length(tmp)], function(s) 
		{	res <- as.numeric(substr(s,2,nchar(s)))
			names(res) <- substr(s,1,1)
			return(res)
		})
			
	# rcc flag
	rcc.flag <- strsplit(x=tmp[1], split="-", fixed=TRUE)[[1]][2]
	if(rcc.flag=="RCC")
		params["rcc"] <- 1
	else
		params["rcc"] <- 0
	
	# build command
	result <- paste0("--alpha ",params["a"])
	result <- paste0("--iterations ",iter.nbr)
	result <- paste0("--neighborhood_size ",params["l"])
	result <- paste0("--rcc ",params["rcc"])
	result <- paste0("-k ",params["k"])
	result <- paste0("--time-limit ",time.limit)
	result <- paste0("--input-file",input.file)
	result <- paste0("--output-folder",output.folder)
	result <- paste0("--gain-function-type ",params["g"])
	result <- paste0("--strategy ","ILS") 
	result <- paste0("--perturbationLevelMax ",params["p"])
		
	return(result)
}


#############################################################################################
# Returns the code (short name) for the Grasp partioning method. See the algorithm documentation
# for more details.
#
# rcc: whether to solve the correlation clustering (FALSE) or relaxed CC problem (TRUE).
# l: neighborhood size (during the local search).
# k: number of clusters (max number for RCC)
# alpha: randomness factor.
# gain: 0 for min imbalance, 
#       1 for max modularity gain function, 
#       2 for max negative modularity gain function, 
#       3 for max positive-negative modularity gain function, 
#       4 for max pos-neg mod gain function II, 
#       5 for max pos-neg mod gain function III
# perturbation: maximal level of perturbation.
#
# returns: the short name corresponding to the Grasp method with the specified parameters.
#############################################################################################
get.grasp.code <- function(rcc, l, k, alpha, gain, perturbation)
{	result <- "Grasp"
	
	if(rcc)
		result <- paste0(result,"-RCC")
	else
		result <- paste0(result,"-CC")
	
	result <- paste0(result,"_l",l)
	result <- paste0(result,"_k",k)
	result <- paste0(result,"_a",alpha)
	result <- paste0(result,"_g",gain)
	result <- paste0(result,"_p",perturbation)
	
	return(result)
}


#############################################################################################
# Returns the inline command for the Greedy Randomized Adaptive Search Procedure (Grasp) partioning 
# method. See the algorithm documentation for more details.
#
# algo.name: short code associated to the algorithm.
# input.file: complete path to the targeted graph file.
# output.folder: complete path to the folder in which the produced files will be placed.
# time.limit: maximum duration of the processing.
# iter.nbr: maximum number of iterations of the processing.
#
# returns: the command allowing to invoke the program externally.
#############################################################################################
get.grasp.command <- function(algo.name, input.file, output.folder, time.limit=1800, iter.nbr=500)
{	# init
	command.folder <- file.path(LIB.FOLDER,"mestrado","grasp","build")
	result <- file.path(command.folder, "graspcc")
	params <- c()
	
	# break down the specified code (short name)
	tmp <- strsplit(x=algo.name, split="_", fixed=TRUE)[[1]]
	params <- sapply(tmp[2:length(tmp)], function(s) 
			{	res <- as.numeric(substr(s,2,nchar(s)))
				names(res) <- substr(s,1,1)
				return(res)
			})
	
	# rcc flag
	rcc.flag <- strsplit(x=tmp[1], split="-", fixed=TRUE)[[1]][2]
	if(rcc.flag=="RCC")
		params["rcc"] <- 1
	else
		params["rcc"] <- 0
	
	# build command
	result <- paste0("--alpha ",params["a"])
	result <- paste0("--iterations ",iter.nbr)
	result <- paste0("--neighborhood_size ",params["l"])
	result <- paste0("--rcc ",params["rcc"])
	result <- paste0("-k ",params["k"])
	result <- paste0("--time-limit ",time.limit)
	result <- paste0("--input-file",input.file)
	result <- paste0("--output-folder",output.folder)
	result <- paste0("--gain-function-type ",params["g"])
	result <- paste0("--strategy ","GRASP") 
	result <- paste0("--perturbationLevelMax ",params["p"])
	
	return(result)
}

#TODO pILS ?


#############################################################################################
# Returns the full name based on the normalized (short) name. Note that for parameterized 
# algorithms, this will just return a clean version of the short name, since it contains 
# the parameter values.
#
# algo.names: short names of the considered algorithms.
#
# returns: the corresponding full names, to be used in plots for instance.
#############################################################################################
get.algo.names <- function(algo.names)
{	result <- c()
	
	for(algo.name in algo.names)
	{	# no parameters
		if(algo.name %in% names(COMDET.ALGO.NAMES))
			result <- c(result, COMDET.ALGO.NAMES[algo.name])
		# parameters
		else
			result <- c(result, gsub(pattern="_", replacement=" ", x=algo.name, fixed=TRUE))
	}
	
	return(result)
}


#############################################################################################
# Returns the inline command for the specified algorithm. The "..." parameters are fetched
# to the algorithm-specific function.
#
# algo.name: short code associated to the algorithm, including its parameter values.
#
# returns: the command allowing to invoke the program externally.
#############################################################################################
get.algo.commands <- function(algo.names, ...)
{	result <- c()
	
	for(algo.name in algo.names)
	{	if(startsWith(algo.name,"ILS"))
			result <- c(result, get.ils.command(algo.name, ...))
		else if(startsWith(algo.name,"Grasp"))
			result <- c(result, get.grasp.command(algo.name, ...))
	}
	
	return(result)
}
