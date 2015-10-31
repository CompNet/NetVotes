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
# for a *single* document. The returned matrix uses the same MEP orders for its row and columns.
#
# NA values indicate the score is undefined for the considered pair of MEPs, and should
# therefore be ignored in subsequent processings, e.g. when averaging to get the agreement index.
# 
# votes: vectors of all MEP votes for a given document.
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
	rownames(sums) <- votes[,COL.MEPID]
	colnames(sums) <- votes[,COL.MEPID]
	
	# process each document in the vote matrix
	for(i in 2:ncol(votes))
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
# Tests
#############################################################################################
agreement.matrix <- load.agreement.matrix("m3.txt")
#################################################
votes <- c("For",NA,NA,NA,"For","Against","Abstention")
scores <- process.agreement.score(votes, agreement.matrix)
print(scores)
#################################################
votes <- matrix(c(
		1:7,
		"For", NA, NA,	  NA, "For",		"Against", "Abstention",
		"For", NA, "For", NA, "Abstention", "For",	   "Abstention",
		"For", NA, NA,	  NA, "For",		"Against", "Abstention"),
		ncol=4)
colnames(votes) <- c(COL.MEPID,1:3)
indices <- process.agreement.index(votes, agreement.matrix)
print(indices)
#################################################



#ConvertMatrix <- function(matrix) {
#	
#	loyalty.values <- list("Absent"=NA,"Rebel"=1,"Loyal"=0,"Didn't vote"=NA,"Documented Absence"=NA,"NA"=NA,"Independent"=NA)
#	
#	matrix[sapply(matrix,is.null)] <- NA
#	reply <- mapply(function(s) loyalty.values[[s]],matrix)
#	reply[sapply(reply, is.null)] <- NA
#	reply <- unlist(reply)
#	
#	reply <- matrix(data = reply, nrow=nrow(matrix), ncol=ncol(matrix))
#	
#	return(reply)
#}
#
#AgreementPlot <- function(agreementMatrix) {
#	temp <- agreementMatrix
#	temp[lower.tri(temp,diag=TRUE)] <- NA
#	temp <- round(temp,2)
#	
#	pdf(file=file.path(paste0(output.agreement.dir,"/",dir.title,"/",file.title,"_agreement_distribution.pdf")))
#	barplot(table(temp),ylim=c(0,30000), xpd=FALSE, xlab="Agreement",ylab="Frequency",col=1:20,main="Agreement Distribution")
#	dev.off()
#	
#	pdf(file=file.path(paste0(output.agreement.dir,"/",dir.title,"/",file.title,"_agreement_histogram.pdf")))
#	hist(temp, xlab="Agreement",ylab="Frequency",col=1:20,main="Agreement Distribution")
#	dev.off()
#}
#
## Function to generate the agreement between each MeP
#CalculateAgreement <- function(vetor) {
#	temp <- matrix(0,nrow=nrow(vetor),ncol=nrow(vetor))
#	rownames(temp) <- rownames(vetor)
#	agreement <- 0
#	agr <- 0
#	points <- vector(length=ncol(vetor))
#	col.size = ncol(vetor)
#	
#	if(selected.table==2) {
#		for(i in 1:(nrow(vetor)-1)) {
#			for(j in (i+1):nrow(vetor)) {
#				agr <- paste(vetor[i,],vetor[j,],sep="")      # Compare Two-by-Two
#				points <- sapply(agr,function(s) table2[[s]]) # Apply the Function to substitue the values
#				agreement <- sum(unlist(points))              # Sum the values
#				agreement <- agreement/col.size               # Normalize by the number of documents
#				temp[i, j] <- agreement                       # Put the value in the adjacency matrix
#				temp[j, i] <- agreement                       # Put the value in the adjacency matrix
#			}
#		}
#	} else {
#		for(i in 1:(nrow(vetor)-1)) {
#			for(j in (i+1):nrow(vetor)) {
#				agr <- paste(vetor[i,],vetor[j,],sep="")      # Compare Two-by-Two
#				points <- sapply(agr,function(s) table1[[s]]) # Apply the Function to substitue the values
#				agreement <- sum(unlist(points))              # Sum the values
#				agreement <- agreement/col.size               # Normalize by the number of documents
#				temp[i, j] <- agreement                       # Put the value in the adjacency matrix
#				temp[j, i] <- agreement                       #Put the value in the adjacency matrix
#			}
#		}
#	}
#	
#	return(temp)
#	
#}
