#############################################################################################
# This script processes the raw data exported from the VoteWatch website, in order to extract
# some tables the main script can then use. It is *not* called by the main script: the data
# given with this project already contain the files resulting of this preprocessing. So, this
# script does not need to be executed before the main one.
# 
# Possible vote values:
# - For: the MEP voted in favor of the text.
# - Abstain: the MEP was neither in favor or not in favor of the text.
# - Against: the MEP was not in favor of the text.
# - Didn't vote: the MEP was present but decided not to vote (or couldn't vote).
# - Absent: the MEP was not present, and did not justify his absence.
# - Documented Absence: the MEP was not present and justified his absence.
# - NA: the MEP was not holding a mandate when voting the considered text.
# 
# 07/2015 Israel Mendonça (v1)
# 09/2015 Vincent Labatut (v2)
#############################################################################################


#############################################################################################
# Folder names
#############################################################################################
# VoteWatch data
VW.FOLDER <- file.path(IN.FOLDER,"votewatch")
	# outputs of the CC method (processed independently)
	CC.FOLDER <- file.path(VW.FOLDER,"pils")
	# raw data (i.e. tables)
	RAW.FOLDER <- file.path(VW.FOLDER,"raw")
		# aggregated files
		AGG.FOLDER <- file.path(RAW.FOLDER,"aggregated")
		# original files
		ORIG.FOLDER <- file.path(RAW.FOLDER,"original")


#############################################################################################
# File names
#############################################################################################
ALL.VOTES.FILE		<- file.path(AGG.FOLDER,"all-votes.csv")
MEP.DETAILS.FILE	<- file.path(AGG.FOLDER,"mep-details.csv")
MEP.LOYALTY.FILE	<- file.path(AGG.FOLDER,"mep-loyalty.csv")
DOMAIN.FREQ.FILE	<- file.path(AGG.FOLDER,"domain-freq.csv")
DOC.DETAILS.FILE	<- file.path(AGG.FOLDER,"document-details.csv")


#############################################################################################
# Column names
#############################################################################################
# individual votes
	COL.NAME	<- "Name"
	COL.STATE	<- "Member State"
	COL.LOYALTY <- "Loyal / Rebel to political group"
	COL.VOTE	<- "Vote"
	COL.GROUP	<- "Group"
# document details
	COL.DATE		<- "Date"
	COL.DOCNAME		<- "Name of document"
	COL.RESULT		<- "Result of vote"
	COL.INSTITUTION	<- "Parliament or council"
	COL.DOMAIN		<- "Policy area"
# created tables
	COL.MEPID		<- "MEP Id"
	COL.LASTNAME	<- "Lastname"
	COL.FIRSTNAME	<- "Firstname"
	COL.DOMID		<- "Domain Id"
	COL.DOCFREQ		<- "Domain Frequency"
	
	
#############################################################################################
# Just loads the file containing the document details.
#
# returns: a table containing the document details.
#############################################################################################
load.doc.details <- function()
{	result <- as.matrix(read.csv2(DOC.DETAILS.FILE,check.names=FALSE))
	return(result)
}


#############################################################################################
# Extracts the list of policy domains and their frequency in terms of voted documents.
#
# doc.details: table describing the voted documents.
# returns: a table containing the policy domains and their frequencies.
#############################################################################################
extract.domains <- function(doc.details)
{	cat("Retrieving the policy domains\n",sep="")
	
	# if the file already exists, just load it
	if(file.exists(DOMAIN.FREQ.FILE))
		result <- as.matrix(read.csv(DOMAIN.FREQ.FILE,check.names=FALSE))
	
	# otherwise, build the table and record it
	else
	{	# count the domains
		counts <- table(doc.details[,COL.DOMAIN])
		
		# build the table
		domains <- names(counts)
		result <- cbind(1:length(domains),domains,counts[domains])
		colnames(result) <- c(COL.DOMID,COL.DOMAIN,COL.DOCFREQ)
		
		# record the table
		write.csv(result,file=DOMAIN.FREQ.FILE,row.names=FALSE)
	}
	
	return(result)
}


#############################################################################################
# Parses the collection of files describing the individual votes of each document,
# and extract the MEPs' details.
#
# returns: a table containing the MEPs and their details.
#############################################################################################
extract.mep.details <- function()
{	cat("Retrieving the MEPs' details\n",sep="")
	
	# if the file already exists, just load it
	if(file.exists(MEP.DETAILS.FILE))
		result <- as.matrix(read.csv(MEP.DETAILS.FILE,check.names=FALSE))
	
	# otherwise, build the table and record it
	else
	{	# init the table
		result <- NULL
		
		# get the list of document-wise vote files
		file.list <- list.files(ORIG.FOLDER, no..=TRUE)
		
		# process each one of them
		f <- 1
		for(file in file.list)
		{	cat("Processing file ", file, " (",f,"/",length(file.list),")\n",sep="")
			# read the file
			path <- file.path(ORIG.FOLDER,file)
			data <- as.matrix(read.csv(path,check.names=FALSE))
			#tmp <- colnames(data)
			f <- f + 1
			
			# init the table
			if(length(result)==0)
			{	result <- rbind(result, cbind(matrix(NA,nrow=nrow(data),ncol=2), data[,c(COL.NAME,COL.STATE,COL.GROUP)]))
				colnames(result) <- c(COL.LASTNAME,COL.FIRSTNAME,COL.NAME,COL.STATE,COL.GROUP)
			}
			# or add to the table
			else
			{	idx <- which(is.na(match(data[,COL.NAME], result[,COL.NAME])))
				if(length(idx)==1)
					result <- rbind(result, c(rep(NA,2), data[idx,c(COL.NAME,COL.STATE,COL.GROUP)]))
				else
					result <- rbind(result, cbind(matrix(NA,nrow=length(idx),ncol=2), data[idx,c(COL.NAME,COL.STATE,COL.GROUP)]))
			}
		}
		
		# split the names
		names <- sapply(result[,COL.NAME], split.name)
		result[,COL.LASTNAME] <- names[1,]
		result[,COL.FIRSTNAME] <- names[2,]
		
		# sort the names alphabetically
		names <- paste(result[,COL.LASTNAME],result[,COL.FIRSTNAME])
		idx <- match(sort(names),names)
		result <- result[idx,]
		
		# add unique ids
		result <- cbind(1:nrow(result),result)
		colnames(result)[1] <- COL.MEPID
		
		# record the table
		write.csv(result,file=MEP.DETAILS.FILE,row.names=FALSE)
	}
	
	return(result)
}


#############################################################################################
# Splits the name of a MEP as it appears in the original vote files, in order to separate
# the firstname(s) and lastname(s).
# 
# name: a string containing both firstname(s) and lastname(s).
# returns: a vector whose first element is the firstname(s) and second is the lastname(s).
#############################################################################################
split.name <- function(name)
{	firstnames <- NA
	lastnames <- NA
	
	names <- strsplit(name, " ")[[1]]
	for(n in names)
	{	if(n==toupper(n) & (nchar(n)>2 | substring(n,2,2)!="."))
		{	if(is.na(lastnames))
				lastnames <- n
			else
				lastnames <- paste(lastnames,n)
		}
		else
		{	if(is.na(firstnames))
				firstnames <- n
			else
				firstnames <- paste(firstnames,n)
		}
	}
	
	result <- c(lastnames, firstnames)
	return(result)
}


#############################################################################################
# Concatenate the votes data contained in the individual documents, in order to get
# a single, more convenient matrix.
#
# mep.details: details describing the MEPs, as loaded by the function extract.mep.details.
# returns: the complete vote matrix.
#############################################################################################
concatenate.votes <- function(mep.details)
{	cat("Concatenating all the MEPs' votes\n",sep="")
	
	# if the file already exists, just load it
	if(file.exists(ALL.VOTES.FILE))
		result <- as.matrix(read.csv(ALL.VOTES.FILE,check.names=FALSE))
	
	# otherwise, build the table and record it
	else
	{	# init the table
		result <- cbind(NULL,mep.details[,COL.MEPID])
		colnames(result)[1] <- COL.MEPID
		
		# get the list of document-wise vote files
		file.list <- list.files(ORIG.FOLDER, no..=TRUE)
		filename.list <- as.integer(sapply(file.list, function(n) substring(n,1,nchar(n)-4)))
		idx <- match(sort(filename.list),filename.list)
		file.list <- file.list[idx]
		
		# process each one of them
		f <- 1
		for(file in file.list)
		{	cat("Processing file ", file, " (",f,"/",length(file.list),")\n",sep="")
			# read the file
			path <- file.path(ORIG.FOLDER,file)
			data <- as.matrix(read.csv(path,check.names=FALSE))
			f <- f + 1
			
			# add a new column to the table
			result <- cbind(result, rep(NA,nrow(mep.details)))
			colnames(result)[ncol(result)] <- substring(file,1,nchar(file)-4)
			
			# complete this new column
			idx <- match(data[,COL.NAME],mep.details[,COL.NAME])
			result[idx,ncol(result)] <- data[,COL.VOTE]
		}
		
		# record the table
		write.csv(result,file=ALL.VOTES.FILE,row.names=FALSE)
	}
	
	return(result)
}


#############################################################################################
# Concatenate the loyalty data contained in the individual documents, in order to get
# a single, more convenient matrix.
#
# mep.details: details describing the MEPs, as loaded by the function extract.mep.details.
# returns: the complete loyalty matrix.
#############################################################################################
concatenate.loyalties <- function(mep.details)
{	cat("Concatenating all the MEPs' loyalty values\n",sep="")
	
	# if the file already exists, just load it
	if(file.exists(MEP.LOYALTY.FILE))
		result <- as.matrix(read.csv(MEP.LOYALTY.FILE,check.names=FALSE))
	
	# otherwise, build the table and record it
	else
	{	# init the table
		result <- cbind(NULL,mep.details[,COL.MEPID])
		colnames(result)[1] <- COL.MEPID
		
		# get the list of document-wise vote files
		file.list <- list.files(ORIG.FOLDER, no..=TRUE)
		filename.list <- as.integer(sapply(file.list, function(n) substring(n,1,nchar(n)-4)))
		idx <- match(sort(filename.list),filename.list)
		file.list <- file.list[idx]
		
		# process each one of them
		f <- 1
		for(file in file.list)
		{	cat("Processing file ", file, " (",f,"/",length(file.list),")\n",sep="")
			# read the file
			path <- file.path(ORIG.FOLDER,file)
			data <- as.matrix(read.csv(path,check.names=FALSE))
			f <- f + 1
			
			# add a new column to the table
			result <- cbind(result, rep(NA,nrow(mep.details)))
			colnames(result)[ncol(result)] <- substring(file,1,nchar(file)-4)
			
			# complete this new column
			idx <- match(data[,COL.NAME],mep.details[,COL.NAME])
			result[idx,ncol(result)] <- data[,COL.LOYALTY]
		}
		
		# record the table
		write.csv(result,file=MEP.LOYALTY.FILE,row.names=FALSE)
	}
	
	return(result)
}


#############################################################################################
# Load all the tables and returns them as a list.
#
# returns: a list containing all the loaded tables.
#############################################################################################
load.raw.data <- function()
{	result <- list()
	
	# document-related
	result$doc.details <- load.doc.details()
	result$domain.details <- extract.domains(doc.details)
	
	# MEP-related
	result$mep.details <- extract.mep.details()
	result$all.votes <- concatenate.votes(mep.details)
	result$loyalty.values <- concatenate.loyalties(mep.details)
	
	return(result)
}
