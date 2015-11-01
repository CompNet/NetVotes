#############################################################################################
# Generate graph objects and files using the agreement index values processed separately. 
# 
# 07/2015 Israel Mendonça (v1)
# 11/2015 Vincent Labatut (v2)
#############################################################################################
library("igraph")

source("src/define-constants.R")
source("src/prepare-data/filter-data.R")



#############################################################################################
# Generates the graph files for the specified data and parameters. The fonction record the
# graph using the Graphml format, as well as a format compatible with the pILS algorithm.
# 
# agreement: agreement indices for the considered MEPs.
# mep.details: MEP details, only for the considered MEPs.
# neg.thresh: negative agreement values above this threshold are set to zero (i.e. ignored).
# pos.thresh: positive agreement values below this threshold are set to zero (i.e. ignored).
# folder: folder (and beginning of the filename) for the produced graph files.
# graph.name: name of the graph in the graphml file.
# returns: the generate graph, as an igraph object.
#############################################################################################
extract.network <- function(agreement, mep.details, neg.thresh=NA, pos.thresh=NA, folder, graph.name)
{	# replace NAs by zeros
	agreement[is.na(agreement)] <- 0
	
	# possibly apply thresholds
	if(!is.na(neg.thresh))
		agreement[agreement<0 & agreement>neg.thresh] <- 0
	if(!is.na(pos.thresh))
		agreement[agreement>0 & agreement<pos.thresh] <- 0
	
	# build network using igraph
	result <- graph.adjacency(adjmatrix=agreement, 
		mode="undirected", weighted=TRUE, diag=FALSE,
		add.colnames=NA, add.rownames="MEPid")
	result$name <- graph.name
	
	# add MEP attributes
	V(result)$Firstname <- mep.details[,COL.FIRSTNAME]
	V(result)$Lastname <- mep.details[,COL.LASTNAME]
	V(result)$Country <- mep.details[,COL.STATE]
	V(result)$Group <- mep.details[,COL.GROUP]
	
	graph.base <- paste(folder,"network",sep="")
	
	# export the graph under the graphml format
	graph.file <- paste(graph.base,".graphml",sep="")
	write.graph(graph=result, file=graph.file, format="graphml")
	
	# export using a format compatible with pils
	t <- get.edgelist(graph=result) - 1	# start numbering nodes at zero
	t <- cbind(t,E(result)$weight)		# add weights as the third column
	graph.file <- paste(graph.base,".G",sep="")
	write.table(data.frame(length(mep.details),nrow(t)), file=graph.file, append=FALSE, sep="\t", row.names=FALSE, col.names=FALSE)	# write header
	write.table(t, file=graph.file, append=TRUE, sep="\t", row.names=FALSE, col.names=FALSE)								# write proper graph
	
	#TODO plot graph
	
	return(result)
}


#############################################################################################
# Generate all possible networks for all time periods and domains, for the specified thresholds 
# and agreement scores. 
#
# mep.details: description of each MEP.
# neg.thresh: negative agreement values above this threshold are set to zero (i.e. ignored).
# pos.thresh: positive agreement values below this threshold are set to zero (i.e. ignored).
# score.file: files describing the scores to use when processing the inter-MEP agreement
#			  (without the .txt extension).
# subfolder: subfolder used to store the generated files.
# mode: indicates whether we are processing only a subpart of the original MEPs (used in the 
#		plot titles).
#############################################################################################
extract.networks <- function(mep.details, neg.thresh=NA, pos.thresh=NA, score.file, subfolder, mode)
{	# setup graph title
	if(is.na(mode))
		mode.str <- ""
	else
		mode.str <- paste(" - mode=",mode,sep="")
	base.graph.name <- paste("MEP agreement - score=",score.file,mode.str,sep="")
	
	# consider each domain individually (including all domains at once)
	for(dom in c(DOM.ALL,DOMAIN.VALUES))
	{	# setup agreement folder
		agr.folder <- paste(AGREEMENT.FOLDER,"/",subfolder,"/",score.file,"/",dom,"/",sep="")
		
		# consider each time period (each individual year as well as the whole term)
		for(date in c(DATE.T7.ALL,DATE.T7.YEARS))
		{	cat("Extracting network data for domain ",dom," and period ",DATE.STR.T7[date],"\n",sep="")
			
			# setup graph title
			graph.name <- paste(base.graph.name," - domain=",dom," - period=",DATE.STR.T7[date],
					" - neg.tresh=",neg.thresh," - pos.tresh=",pos.thresh,sep="")
			# setup graph folder
			folder <- paste(NETWORKS.FOLDER,"/",subfolder,"/",score.file,"/",dom,"/",DATE.STR.T7[date],
					"/","negtr=",neg.thresh,"-postr=",pos.thresh,"/",sep="")
			dir.create(folder, recursive=TRUE, showWarnings=FALSE)
			
			# load agreement index file
			table.file <- paste(agr.folder,DATE.STR.T7[date],"-agreement",sep="")
			if(!file.exists(table.file))
				cat("Agreement file ",table.file," not found >> not necessarily an error: maybe not enough data to process agreement","\n",sep="")
			else
			{	# retrieve agreement
				agreement <- as.matrix(read.csv(file=table.file, row.names=1))
				# build and record network
				g <- extract.network(agreement, mep.details, neg.thresh, pos.thresh, folder, graph.name)
				
				#TODO process network stats
				
			}
		}
	}
}


#############################################################################################
# Generate all networks for the whole dataset, by country and by political group, for the 
# specified thresholds and agreement scores. 
#
# mep.details: description of each MEP.
# neg.thresh: negative agreement values above this threshold are set to zero (i.e. ignored).
# pos.thresh: positive agreement values below this threshold are set to zero (i.e. ignored).
#############################################################################################
extract.all.networks <- function(mep.details, neg.thresh=NA, pos.thresh=NA, score.file)
{	# extract networks for all data
	cat("Extract networks for all data","\n",sep="")
	folder <- "everything"
	extract.networks(mep.details, neg.thresh=NA, pos.thresh=NA, score.file, folder, mode=NA)
	
	# networks by political group
	cat("Extract networks by group","\n",sep="")
	folder <- "bygroup"
	for(group in GROUP.NAMES)
	{	cat("Process stats for group ",group,"\n",sep="")
		
		# select data
		filtered.mep.ids <- filter.meps.by.group(mep.details,group)
		idx <- match(filtered.mep.ids,mep.details[,COL.MEPID])
		grp.meps <- mep.details[idx,]
		
		# setup folder
		grp.folder <- paste(folder,"/",group,sep="")
		
		# extract networks
		extract.networks(grp.meps, neg.thresh, pos.thresh, score.file, grp.folder, mode=group)
	}
	
	# networks by home country
	cat("Extract networks by country","\n",sep="")
	folder <- "bycountry"
	for(country in COUNTRY.VALUES)
	{	cat("Process stats for country ",country,"\n",sep="")
		
		# select data
		filtered.mep.ids <- filter.meps.by.country(mep.details,country)
		idx <- match(filtered.mep.ids,mep.details[,COL.MEPID])
		cntr.meps <- mep.details[idx,]
		
		# setup folder
		cntr.folder <- paste(folder,"/",country,sep="")
		
		# extract networks
		extract.networks(cntr.meps, neg.thresh, pos.thresh, score.file, cntr.folder, mode=country)
	}
}
