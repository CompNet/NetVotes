#############################################################################################
# Generate graph objects and files using the agreement index values processed separately. 
# 
# 07/2015 Israel Mendonça (v1)
# 11/2015 Vincent Labatut (v2)
#############################################################################################
library("igraph")

source("src/define-constants.R")
source("src/plot-tools/plot-networks.R")
source("src/prepare-data/filter-data.R")
source("src/build-networks/process-network-stats.R")



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
# plot.formats: formats of the plot files.
#############################################################################################
extract.network <- function(agreement, mep.details, neg.thresh=NA, pos.thresh=NA, folder, graph.name, plot.formats)
{	cat("Building network folder='",folder,"'\n",sep="")
	
	# replace NAs by zeros
	agreement[is.na(agreement)] <- 0
	
	# possibly apply thresholds
	if(!is.na(neg.thresh))
		agreement[agreement<0 & agreement>neg.thresh] <- 0
	if(!is.na(pos.thresh))
		agreement[agreement>0 & agreement<pos.thresh] <- 0
	
	# build network using igraph
	result <- graph.adjacency(adjmatrix=agreement, 		# use the agreement as the adjacency matrix
		mode="undirected", weighted=TRUE, diag=FALSE,	# ignore the diagonal of the agreement matrix
		add.colnames=NA, add.rownames="MEPid")			# use the id as node names
	result$name <- graph.name
	
	# add MEP attributes
	V(result)$Firstname <- mep.details[,COL.FIRSTNAME]
	V(result)$Lastname <- mep.details[,COL.LASTNAME]
	V(result)$Country <- mep.details[,COL.STATE]
	V(result)$Group <- mep.details[,COL.GROUP]
	
	# plot graph and get spatial positions as nodal attributes
	cat("Plotting network...\n")
	graph.base <- file.path(folder,SIGNED.FILE)
	g <- plot.network(g=result, plot.file=graph.base, format=plot.formats)
	
	# export the graph under the graphml format
	graph.file <- paste(graph.base,".graphml",sep="")
	write.graph(graph=result, file=graph.file, format="graphml")
	
	# also export the positive and complementary negative graphs, as unsigned graphs
	gp <- subgraph.edges(graph=result, eids=which(E(result)$weight>0), delete.vertices=FALSE)
	graph.file <- file.path(folder,paste(POSITIVE.FILE,".graphml",sep=""))
	write.graph(graph=gp, file=graph.file, format="graphml")
	gn <- subgraph.edges(graph=result, eids=which(E(result)$weight<0), delete.vertices=FALSE)
	gn <- graph.complementer(graph=gn, loops=FALSE)
	graph.file <- file.path(folder,paste(COMP.NEGATIVE.FILE,".graphml",sep=""))
	write.graph(graph=gn, file=graph.file, format="graphml")
	
	# export using a format compatible with pILS
	t <- get.edgelist(graph=result) - 1	# start numbering nodes at zero
	t <- cbind(t,E(result)$weight)		# add weights as the third column
	graph.file <- paste(graph.base,".G",sep="")
	write.table(data.frame(nrow(mep.details),nrow(t)), file=graph.file, append=FALSE, sep="\t", row.names=FALSE, col.names=FALSE)	# write header
	write.table(t, file=graph.file, append=TRUE, sep="\t", row.names=FALSE, col.names=FALSE)								# write proper graph
	
	# process network stats
	cat("Processing network stats...\n")
	process.network.stats(result, folder)
	
	return(result)
}


#############################################################################################
# Generates all possible networks for all time periods and domains, for the specified thresholds 
# and agreement scores. 
#
# mep.details: description of each MEP.
# neg.thresh: negative agreement values above this threshold are set to zero (i.e. ignored).
# pos.thresh: positive agreement values below this threshold are set to zero (i.e. ignored).
# score.file: files describing the scores to use when processing the inter-MEP agreement
#			  (without the .txt extension).
# domains: political domains to consider when processing the data.
# dates: time periods to consider when processing the data.
# country: state member currently processed (or NA if none in particular).
# group: political gorup currently processed (or NA if none in particular).
# plot.formats: formats of the plot files.
#############################################################################################
extract.networks <- function(mep.details, neg.thresh=NA, pos.thresh=NA, score.file, domains, dates, country, group, plot.formats)
{	# setup graph title
	if(is.na(country))
		if(is.na(group))
			mode.str <- ""
		else
			mode.str <- paste(" - group=",group,sep="")
	else
		mode.str <- paste(" - country=",country,sep="")
	base.graph.name <- paste("MEP agreement - score=",score.file,mode.str,sep="")
	
	# consider each domain individually (including all domains at once)
	for(dom in domains)
	{	# setup agreement folder
		#agr.folder <- paste(AGREEMENT.FOLDER,"/",subfolder,"/",score.file,"/",dom,"/",sep="")
		agr.folder <- get.agreement.path(score=score.file,country,group,domain=dom)
		
		# consider each time period (each individual year as well as the whole term)
		for(date in dates)
		{	cat("Extracting network for domain ",dom," and period ",DATE.STR.T7[date],"\n",sep="")
			
			# setup graph title
			graph.name <- paste(base.graph.name," - domain=",dom," - period=",DATE.STR.T7[date],
					" - neg.tresh=",neg.thresh," - pos.tresh=",pos.thresh,sep="")
			# setup graph folder
			#folder <- paste(NETWORKS.FOLDER,"/",subfolder,"/",score.file,
			#		"/","negtr=",neg.thresh,"-postr=",pos.thresh,
			#		"/",dom,"/",DATE.STR.T7[date],"/",sep="")
			folder <- get.networks.path(score=score.file,neg.thresh,pos.thresh,country,group,domain=dom,period=date)
			dir.create(folder, recursive=TRUE, showWarnings=FALSE)
			
			# load agreement index file
			table.file <- file.path(agr.folder,paste(DATE.STR.T7[date],"-agreement.csv",sep=""))
			if(!file.exists(table.file))
				cat("WARNING: Agreement file ",table.file," not found >> not necessarily an error: maybe not enough data to process agreement","\n",sep="")
			else
			{	# retrieve agreement
				agreement <- as.matrix(read.csv2(file=table.file, row.names=1))
				# build and record network
				g <- extract.network(agreement, mep.details, neg.thresh, pos.thresh, folder, graph.name, plot.formats)
			}
		}
	}
}


#############################################################################################
# Generates all networks for the whole dataset, by country and by political group, for the 
# specified thresholds and agreement scores. 
#
# mep.details: description of each MEP.
# neg.thresh: negative agreement values above this threshold are set to zero (i.e. ignored).
# pos.thresh: positive agreement values below this threshold are set to zero (i.e. ignored).
# domains: political domains to consider when processing the data.
# dates: time periods to consider when processing the data.
# everything: whether to process all data without distinction of country or political group.
# countries: member states to consider separately when processing the data.
# groups: political groups to consider separately when processing the data.
# plot.formats: formats of the plot files.
#############################################################################################
extract.all.networks <- function(mep.details, neg.thresh=NA, pos.thresh=NA, score.file, domains, dates, everything, countries, groups, plot.formats)
{	# extract networks for all data
	if(everything)
	{	cat("Extract networks for all data","\n",sep="")
		extract.networks(mep.details, neg.thresh, pos.thresh, score.file, domains, dates, country=NA, group=NA, plot.formats)
	}
	
	# networks by political group
	cat("Extract networks by group","\n",sep="")
	for(group in groups)
	{	cat("Extract networks for group ",group,"\n",sep="")
		
		# select data
		filtered.mep.ids <- filter.meps.by.group(mep.details,group)
		idx <- match(filtered.mep.ids,mep.details[,COL.MEPID])
		grp.meps <- mep.details[idx,]
		
		# extract networks
		extract.networks(grp.meps, neg.thresh, pos.thresh, score.file, domains, dates, country=NA, group, plot.formats)
	}
	
	# networks by home country
	cat("Extract networks by country","\n",sep="")
	for(country in countries)
	{	cat("Extract networks for country ",country,"\n",sep="")
		
		# select data
		filtered.mep.ids <- filter.meps.by.country(mep.details,country)
		idx <- match(filtered.mep.ids,mep.details[,COL.MEPID])
		cntr.meps <- mep.details[idx,]
		
		# extract networks
		extract.networks(cntr.meps, neg.thresh, pos.thresh, score.file, domains, dates, country, group=NA, plot.formats)
	}
}
