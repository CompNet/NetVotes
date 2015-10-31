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
# file.name: name of the file containing the scores.
# returns: the loaded matrix.
#############################################################################################
load.agreement.matrix <- function(file.name)
{	# load the file content
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
		{	result[i,j] <- agreement.matrix[votes[i],votes[j]]
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
	{	# get scores
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
#############################################################################################
process.all.agreement <- function(all.votes, score.file)
{	# load the agreement scores
	agreement.matrix <- load.agreement.matrix(score.file)
	
	# consider each domain individually (including all domains at once)
	for(dom in c(DOM.ALL,DOMAIN.VALUES))
	{	cat("Processing agreement data for domain ",dom,"\n",sep="")
		
		# setup folder
		folder <- paste(main.folder,dom,"/averaged/",sep="")
		dir.create(folder, recursive=TRUE, showWarnings=FALSE)
		
		# consider each time period (each individual year as well as the whole term)
		agr <- list()
		agr.spe <- list()
		for(date in c(DATE.T7.ALL,DATE.T7.YEARS))
		{	cat("Processing period ",DATE.STR.T7[date],"\n",sep="")
			
			# retain only the documents related to the selected topic and dates
			if(dom==DOM.ALL)
				domval <- NA
			else
				domval <- dom
			docids <- filter.docs.by.date.and.domain(doc.details, 
				start.date=DATE.START.T7[[date]], end.date=DATE.END.T7[[date]], 
				domains=domval)
			if(length(docids)>1)
			{	# format data
				cols <- match(docids, colnames(all.votes))
				votes <- all.votes[,cols]
				aggreement <- process.agreement.index(votes, agreement.matrix)
				agr <- apply(numerized, 2, mean)
				
				# plot absolute counts as bars
				title <- paste("Distribution of ",object," for domain ",dom,", for period ",DATE.STR.T7[date],sep="")
				plot.file <- paste(folder,prefix,DATE.STR.T7[date],"-counts",sep="")
				data <- plot.histo(plot.file, values=agr[[date]],
					x.label=object, 
					proportions=FALSE, x.lim=c(0,1), y.max=NA, break.nbr=NA, 
					plot.title=title, format=c("PDF","PNG",NA))
				# record as a table
				data <- data[,c("y","xmin","xmax")]
				table.file <- paste(plot.file,".csv",sep="")
				write.csv(data,file=table.file, row.names=FALSE)
				
				# plot proportions as bars
				title <- paste("Distribution of ",object," for domain ",dom,", for period ",DATE.STR.T7[date],sep="")
				plot.file <- paste(folder,prefix,DATE.STR.T7[date],"-proportions",sep="")
				data <- plot.histo(plot.file, values=agr[[date]],
					x.label=object, 
					proportions=TRUE, x.lim=c(0,1), y.max=0.5, break.nbr=NA, 
					plot.title=title, format=c("PDF","PNG",NA))
				# record as a table
				data <- data[,c("y","xmin","xmax")]
				table.file <- paste(plot.file,".csv",sep="")
				write.csv(data,file=table.file, row.names=FALSE)
			}
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

