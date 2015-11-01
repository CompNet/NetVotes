#############################################################################################
# Processes the agreement between MEPs, and generate plots and statistics.
# These scripts directly make use of the "score matrices" stored in in/score as lists. 
# 
# 07/2015 Israel Mendonça (v1)
# 10/2015 Vincent Labatut (v2)
#############################################################################################


#############################################################################################
# Loads the specified score matrix, and returns, well, this matrix.
#
# file.name: name of the file containing the scores (without the .txt extension).
# returns: the loaded matrix.
#############################################################################################
load.agreement.matrix <- function(file.name)
{	# load the file content
	file.name <- paste(file.name,".txt",sep="")
	file <- file.path(SCORE.FOLDER,file.name)
	df <- read.table(file, header=FALSE, as.is=TRUE, check.names=FALSE)
	# possibly replace NA by equivalent strings
	df[is.na(df[,1]),1] <- "NA"
	df[is.na(df[,2]),2] <- "NA"
	
	# init the result matrix
	result <- matrix(NA, nrow=length(VOTE.VALUES)+1, ncol=length(VOTE.VALUES)+1)
	rownames(result) <- c(VOTE.VALUES, "NA")
	colnames(result) <- c(VOTE.VALUES, "NA")
	
	# fill the matrix
	for(i in 1:nrow(df))
	{	result[df[i,1],df[i,2]] <- df[i,3]
		result[df[i,2],df[i,1]] <- df[i,3]
	}
	
	return(result)
}


#############################################################################################
# Processes the agreement scores between all pairs of MEPs, for the specified votes and
# agreement matrix. Note the parameter 'votes' is a vector containing all votes for all MEPs
# for a *single* document. The returned matrix uses the same MEP orders for its rows and columns.
#
# NA values indicate the score is undefined for the considered pair of MEPs, and should
# therefore be ignored in subsequent processings, e.g. when averaging to get the agreement index.
# 
# votes: vector of all MEP votes for a given document.
# agreement.matrix: matrix containing the reference scores, loaded with load.aggreement.matrix.
# returns: a square matrix whose size is the number of MEPs, and containing all agreement scores
#  		   for the considered document.
#############################################################################################
process.agreement.score <- function(votes, agreement.matrix)
{	# possibly replace NA by equivalent strings
	votes[is.na(votes)] <- "NA"
	
	# init matrix
	result <- matrix(NA,nrow=length(votes), ncol=length(votes))
	
	# fill matrix
	for(i in 1:length(votes))
	{	for(j in 1:length(votes))
		{	#cat(i,":",j,"\n")
			result[i,j] <- agreement.matrix[votes[i],votes[j]]
		}
	}
	
	return(result)
}


#############################################################################################
# Processes the agreement indices between all pairs of MEPs, for the specified votes and
# agreement matrix. Note the parameter 'votes' is a matrix containing all votes for all MEPs
# (on the rows) for a series of documents (columns). The returned matrix uses the same MEP orders 
# for its row and columns.
#
# NA values indicate the agreement index is undefined for the considered pair of MEPs, and should
# therefore be ignored in subsequent processings, e.g. when extracting a network or generating
# plots.
# 
# votes: matrix of all MEP (rows) votes for several documents (columns).
# agreement.matrix: matrix containing the reference scores, loaded with load.aggreement.matrix.
# returns: a square matrix whose size is the number of MEPs, and containing all agreement indices
#  		   for the considered documents.
#############################################################################################
process.agreement.index <- function(votes, agreement.matrix)
{	# init data structures
	counts <- matrix(0,nrow=nrow(votes),ncol=nrow(votes))
	sums <- matrix(0,nrow=nrow(votes),ncol=nrow(votes))
		
	# process each document in the vote matrix
	for(i in 1:ncol(votes))
	{	cat("Processing document",i,"\n")
		# get scores
		scores <- process.agreement.score(votes[,i], agreement.matrix)
		
		# update counts
		increments <- matrix(0,nrow=nrow(scores),ncol=ncol(scores))
		increments[!is.na(scores)] <- 1
		counts <- counts + increments

		# update sums
		scores[is.na(scores)] <- 0
		sums <- sums + scores
	}
	
	# normalize the sums to get the agreement indices
	#print(sums)
	#print(counts)
	result <- sums / counts
	result[is.nan(result)] <- NA
	return(result)
}


#############################################################################################
# Processes the agreement for all domains and time periods, for the specified raw votes.
#
# all.votes: raw vote data, including how each MEP voted.
# doc.details: description of each voted document.
# score.file: files describing the scores to use when processing the inter-MEP agreement.
#			  (without the .txt extension).
# subfolder: subfolder used to store the generated files.
# mode: indicates whether we are processing only a subpart of the original MEPs (used in the 
#		plot titles).
#############################################################################################
process.agreement.stats <- function(all.votes, doc.details, score.file, subfolder, mode)
{	object <- "Agreement index"
	x.label <- paste("Agreement index - score=",score.file,sep="")
			
	# setup title prefix
	if(is.na(mode))
		plot.prefix <- ""
	else
		plot.prefix <- paste("[",mode,"] ",sep="")
	
	# load the agreement scores
	agreement.matrix <- load.agreement.matrix(score.file)
	
	# consider each domain individually (including all domains at once)
	for(dom in c(DOM.ALL,DOMAIN.VALUES))
	{	# setup folder
		folder <- paste(AGREEMENT.FOLDER,"/",subfolder,"/",score.file,"/",dom,"/",sep="")
		dir.create(folder, recursive=TRUE, showWarnings=FALSE)
		
		# consider each time period (each individual year as well as the whole term)
		for(date in c(DATE.T7.ALL,DATE.T7.YEARS))
		{	cat("Processing agreement data for domain ",dom," and period ",DATE.STR.T7[date],"\n",sep="")
			
			# retain only the documents related to the selected topic and dates
			if(dom==DOM.ALL)
				domval <- NA
			else
				domval <- dom
			docids <- filter.docs.by.date.and.domain(doc.details, 
				start.date=DATE.START.T7[[date]], end.date=DATE.END.T7[[date]], 
				domains=domval)
			# check if there's enough data remaining
			if(length(docids)>1)
			{	# format data
				cols <- match(docids, colnames(all.votes))
				votes <- all.votes[,cols]
				agreement <- process.agreement.index(votes, agreement.matrix)
				
				# record raw agreement index values
				colnames(agreement) <- all.votes[,COL.MEPID]
				rownames(agreement) <- all.votes[,COL.MEPID]
				table.file <- paste(folder,DATE.STR.T7[date],"-agreement",sep="")
				write.csv(agreement,file=table.file, row.names=TRUE)
				
				# keep only the triangular part of the matrix (w/o the diagonal)
				#print(agreement)				
				agr.vals <- agreement[upper.tri(agreement,diag=FALSE)]
				
				# check there are enough agreement values
				if(all(is.na(agr.vals)))
					cat("WARNING: All agreement values are NAs >> not processing these data\n",sep="")
				else
				{	# plot absolute counts as bars
					title <- paste(plot.prefix,"Distribution of ",object," - domain=",dom,", - period=",DATE.STR.T7[date],sep="")
					plot.file <- paste(folder,DATE.STR.T7[date],"-counts",sep="")
					data <- plot.histo(plot.file, values=agr.vals,
						x.label, 
						proportions=FALSE, x.lim=c(-1,1), y.max=NA, break.nbr=NA, 
						plot.title=title, format=c("PDF","PNG",NA))
					# record as a table
					data <- data[,c("y","xmin","xmax")]
					table.file <- paste(plot.file,".csv",sep="")
					write.csv(data,file=table.file, row.names=FALSE)
					
					# plot proportions as bars
					title <- paste(plot.prefix,"Distribution of ",object," - domain=",dom,", - period=",DATE.STR.T7[date],sep="")
					plot.file <- paste(folder,DATE.STR.T7[date],"-proportions",sep="")
					data <- plot.histo(plot.file, values=agr.vals,
						x.label, 
						proportions=TRUE, x.lim=c(-1,1), y.max=0.5, break.nbr=NA, 
						plot.title=title, format=c("PDF","PNG",NA))
					# record as a table
					data <- data[,c("y","xmin","xmax")]
					table.file <- paste(plot.file,".csv",sep="")
					write.csv(data,file=table.file, row.names=FALSE)
				}
			}
			else
				cat("WARNING: Only ",length(docids)," documents remaining after filtering >> not processing these data\n",sep="")
		}
	}
}


#############################################################################################
# Main function of this script, generating all agreement-related tables and plots.
#
# all.votes: individual vote data, i.e. how each MEP voted.
# doc.details: description of each voted document.
# mep.details: description of each MEP.
# score.file: files describing the scores to use when processing the inter-MEP agreement
#			  (without the .txt extension).
#############################################################################################
process.agreement <- function(all.votes, doc.details, mep.details, score.file)
{	# process agreement for all data
	cat("Process agreement for all data","\n",sep="")
	folder <- "everything"
	process.agreement.stats(all.votes, doc.details, score.file, folder, mode=NA)
	
	# stats by political group
	cat("Process stats by group","\n",sep="")
	folder <- "bygroup"
	for(group in GROUP.NAMES)
	{	cat("Process stats for group ",group,"\n",sep="")
		
		# select data
		mepids <- filter.meps.by.group(mep.details,group)
		idx <- match(mepids,all.votes[,COL.MEPID])
		group.votes <- all.votes[idx,]
		
		# setup folder
		grp.folder <- paste(folder,"/",group,sep="")
		# process agreement
		process.agreement.stats(group.votes, doc.details, score.file, grp.folder, mode=group)
	}
	
	# stats by home country
	cat("Process stats by country","\n",sep="")
	folder <- "bycountry"
	for(country in COUNTRY.VALUES)
	{	cat("Process stats for country ",country,"\n",sep="")
		
		# select data
		mepids <- filter.meps.by.country(mep.details,country)
		idx <- match(mepids,all.votes[,COL.MEPID])
		country.votes <- all.votes[idx,]
		
		# setup folder
		cntr.folder <- paste(folder,"/",country,sep="")
		# process agreement
		process.agreement.stats(country.votes, doc.details, score.file, cntr.folder, mode=country)
	}
}


#############################################################################################
# Tests
#############################################################################################
#agreement.matrix <- load.agreement.matrix("m3.txt")
#################################################
#votes <- c("For",NA,NA,NA,"For","Against","Abstention")
#scores <- process.agreement.score(votes, agreement.matrix)
#print(scores)
#################################################
#votes <- matrix(c(
#		"For", NA, NA,	  NA, "For",		"Against", "Abstention",
#		"For", NA, "For", NA, "Abstention", "For",	   "Abstention",
#		"For", NA, NA,	  NA, "For",		"Against", "Abstention"),
#		ncol=3)
#colnames(votes) <- 1:3
#indices <- process.agreement.index(votes, agreement.matrix)
#print(indices)
#################################################
process.agreement(all.votes, doc.details, mep.details, score.file="m3")
#################################################
