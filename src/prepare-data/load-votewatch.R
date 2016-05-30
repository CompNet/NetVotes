#############################################################################################
# This script processes the raw data exported from the VoteWatch website, in order to extract
# some tables the main script can then use.
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
source("src/prepare-data/load-common.R")


#############################################################################################
# Folder names
#############################################################################################
# VoteWatch data
VW.FOLDER <- file.path(IN.FOLDER,"votewatch")
#VW.FOLDER <- file.path(IN.FOLDER,"test") # just for debug
	# raw data (i.e. tables)
	VW.RAW.FOLDER <- file.path(VW.FOLDER,"raw")


#############################################################################################
# Input file names
#############################################################################################
DOC.DETAILS.RAW.FILE	<- file.path(VW.FOLDER,"list.csv")


#############################################################################################
# Column names
#############################################################################################
# individual votes
	VW.COL.NAME		<- "Name"
	VW.COL.STATE	<- "Member State"
	VW.COL.BEHAVIOR	<- "Loyal / Rebel to political group"
	VW.COL.VOTE		<- "Vote"
	VW.COL.GROUP	<- "Group"
# document details
	VW.COL.DOCID		<- "Doc Id"
	VW.COL.DATE			<- "Date"
	VW.COL.DOCNAME		<- "Name of document"
	VW.COL.RESULT		<- "Result of vote"
	VW.COL.INSTITUTION	<- "Parliament or council"
	VW.COL.DOMAIN		<- "Policy area"
	

#############################################################################################
# Domain mapping
#############################################################################################
# map used to convert official domain names into VoteWatch ones
DOMAIN.VW2SYMB <- c()
DOMAIN.VW2SYMB["Constitutional and inter-institutional affairs"] <- DOMAIN.AFCO
DOMAIN.VW2SYMB["Foreign & security policy"] <- DOMAIN.AFET
DOMAIN.VW2SYMB["Agriculture"] <- DOMAIN.AGRI
DOMAIN.VW2SYMB["Budget"] <- DOMAIN.BUDG
DOMAIN.VW2SYMB["Budgetary control"] <- DOMAIN.CONT
DOMAIN.VW2SYMB["Culture & education"] <- DOMAIN.CULT
DOMAIN.VW2SYMB["Development"] <- DOMAIN.DEVE
DOMAIN.VW2SYMB["Gender equality"] <- DOMAIN.FEMM
DOMAIN.VW2SYMB["Economic & monetary affairs"] <- DOMAIN.ECON
DOMAIN.VW2SYMB["Employment & social affairs"] <- DOMAIN.EMPL
DOMAIN.VW2SYMB["Environment & public health"] <- DOMAIN.ENVI
DOMAIN.VW2SYMB["Internal market & consumer protection"] <- DOMAIN.IMCO
DOMAIN.VW2SYMB["International trade"] <- DOMAIN.INTA
DOMAIN.VW2SYMB["Industry, research & energy"] <- DOMAIN.ITRE
DOMAIN.VW2SYMB["Legal affairs"] <- DOMAIN.JURI
DOMAIN.VW2SYMB["Civil liberties, justice & home affairs"] <- DOMAIN.LIBE
DOMAIN.VW2SYMB["Fisheries"] <- DOMAIN.PECH
DOMAIN.VW2SYMB["Petitions"] <- DOMAIN.PETI
DOMAIN.VW2SYMB["Regional development"] <- DOMAIN.REGI
DOMAIN.VW2SYMB["Internal regulations of the EP"] <- DOMAIN.RIPE
DOMAIN.VW2SYMB["Transport & tourism"] <- DOMAIN.TRAN


#############################################################################################
# Vote mapping
#############################################################################################
VOTE.VW2SYMB <- c()
VOTE.VW2SYMB["For"] <- VOTE.FOR
VOTE.VW2SYMB["Abstain"] <- VOTE.ABST
VOTE.VW2SYMB["Against"] <- VOTE.AGST
VOTE.VW2SYMB["Didn't vote"] <- VOTE.NONE
VOTE.VW2SYMB["Absent"] <- VOTE.ABSENT
VOTE.VW2SYMB["Documented Absence"] <- VOTE.DOCABSENT


#############################################################################################
# Group mapping
#############################################################################################
GROUP.VW2SYMB <- c()
GROUP.VW2SYMB["ALDE/ADLE"] <- GROUP.ALDE
GROUP.VW2SYMB["ECR"] <- GROUP.ECR
GROUP.VW2SYMB["EFD"] <- GROUP.EFD
GROUP.VW2SYMB["EPP"] <- GROUP.EPP
GROUP.VW2SYMB["Greens/EFA"] <- GROUP.GREENS
GROUP.VW2SYMB["GUE-NGL"] <- GROUP.GUENGL
GROUP.VW2SYMB["NI"] <- GROUP.NI
GROUP.VW2SYMB["S&D"] <- GROUP.SD


#############################################################################################
# Loads and cleans the file containing the document details.
#
# returns: a table containing the document details.
#############################################################################################
vw.clean.doc.details <- function()
{	tlog("..Retrieving and cleaning the document details")
	dir.create(OVERALL.FOLDER, recursive=TRUE, showWarnings=FALSE)
	
	# if the file already exists, just load it
	if(file.exists(DOC.DETAILS.FILE))
	{	result <- as.matrix(read.csv2(DOC.DETAILS.FILE,check.names=FALSE))
		result[,COL.DOCID] <- as.integer(result[,COL.DOCID])
	}
	
	# otherwise, build the table and record it
	else
	{	# init the cleaned table
		result <- NULL
		
		# load the original table
		data <- as.matrix(read.csv2(DOC.DETAILS.RAW.FILE,check.names=FALSE))
		
		# build the table
		result <- cbind(result, as.integer(data[,VW.COL.DOCID]), data[,c(VW.COL.DATE,VW.COL.DOCNAME)])
		# clean the result values
		doc.res <- data[,VW.COL.RESULT]
		doc.res[doc.res=="+"] <- VOTE.FOR
		doc.res[doc.res=="-"] <- VOTE.AGST
		result <- cbind(result, doc.res)
		# clean the domain names
		dom.ids <- DOMAIN.VW2SYMB[data[,VW.COL.DOMAIN]]
		result <- cbind(result, dom.ids)
		# add the column names
		colnames(result) <- c(COL.DOCID,COL.DATE,COL.TITLE,COL.RESULT,COL.DOMID)
		
		# record the table
		write.csv2(result,file=DOC.DETAILS.FILE,row.names=FALSE)
	}
	
	return(result)
}


#############################################################################################
# Parses the collection of files describing the individual votes of each document,
# and extract the MEPs' details.
#
# returns: a table containing the MEPs and their details.
#############################################################################################
vw.extract.mep.details <- function()
{	tlog("..Retrieving the MEPs' details")
	
	# if the file already exists, just load it
	if(file.exists(MEP.DETAILS.FILE))
	{	result <- as.matrix(read.csv2(MEP.DETAILS.FILE,check.names=FALSE))
		result[,COL.MEPID] <- as.integer(result[,COL.MEPID])
	}
	
	# otherwise, build the table and record it
	else
	{	# init the table
		result <- NULL
		
		# get the list of document-wise vote files
		file.list <- list.files(VW.RAW.FOLDER, no..=TRUE)
		
		# process each one of them
		f <- 1
		for(file in file.list)
		{	tlog("....Processing file ", file, " (",f,"/",length(file.list),")")
			# read the file
			path <- file.path(VW.RAW.FOLDER,file)
			data <- as.matrix(read.csv(path,check.names=FALSE))
			#tmp <- colnames(data)
			f <- f + 1
			
			# init the table
			if(length(result)==0)
			{	result <- rbind(result, cbind(matrix(NA,nrow=nrow(data),ncol=2), data[,c(VW.COL.NAME,VW.COL.STATE,VW.COL.GROUP)]))
				colnames(result) <- c(COL.LASTNAME,COL.FIRSTNAME,COL.FULLNAME,COL.STATE,COL.GROUP)
			}
			# or add to the table
			else
			{	idx <- which(is.na(match(data[,VW.COL.NAME], result[,COL.FULLNAME])))
				if(length(idx)==1)
					result <- rbind(result, c(rep(NA,2), data[idx,c(VW.COL.NAME,VW.COL.STATE,VW.COL.GROUP)]))
				else
					result <- rbind(result, cbind(matrix(NA,nrow=length(idx),ncol=2), data[idx,c(VW.COL.NAME,VW.COL.STATE,VW.COL.GROUP)]))
			}
		}
		
		# clean group names
		result[,COL.GROUP] <- GROUP.VW2SYMB[result[,COL.GROUP]]
		
		# split the names
		names <- sapply(result[,COL.FULLNAME], vw.split.name)
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
		write.csv2(result,file=MEP.DETAILS.FILE,row.names=FALSE)
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
vw.split.name <- function(name)
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
vw.concatenate.votes <- function(mep.details)
{	tlog("..Concatenating all the MEPs' votes")
	dir.create(OVERALL.FOLDER, recursive=TRUE, showWarnings=FALSE)
	
	# if the file already exists, just load it
	if(file.exists(ALL.VOTES.FILE))
	{	result <- as.matrix(read.csv2(ALL.VOTES.FILE,check.names=FALSE))
		result[,COL.MEPID] <- as.integer(result[,COL.MEPID])
	}
	
	# otherwise, build the table and record it
	else
	{	# init the table
		result <- cbind(NULL,as.integer(mep.details[,COL.MEPID]))
		colnames(result)[1] <- COL.MEPID
		
		# get the list of document-wise vote files
		file.list <- list.files(VW.RAW.FOLDER, no..=TRUE)
		filename.list <- as.integer(sapply(file.list, function(n) substring(n,1,nchar(n)-4)))
		idx <- match(sort(filename.list),filename.list)
		file.list <- file.list[idx]
		
		# process each one of them
		f <- 1
		for(file in file.list)
		{	tlog("....Processing file ", file, " (",f,"/",length(file.list),")")
			# read the file
			path <- file.path(VW.RAW.FOLDER,file)
			data <- as.matrix(read.csv(path,check.names=FALSE))
			f <- f + 1
			
			# add a new column to the table
			result <- cbind(result, rep(NA,nrow(mep.details)))
			colnames(result)[ncol(result)] <- substring(file,1,nchar(file)-4)
			
			# complete this new column
			idx <- match(data[,VW.COL.NAME],mep.details[,COL.FULLNAME])
			result[idx,ncol(result)] <- VOTE.VW2SYMB[data[,VW.COL.VOTE]]
		}
		
		# record the table
		write.csv2(result,file=ALL.VOTES.FILE,row.names=FALSE)
	}
	
	return(result)
}


#############################################################################################
# Concatenate the behavior data contained in the individual documents, in order to get
# a single, more convenient matrix.
#
# mep.details: details describing the MEPs, as loaded by the function extract.mep.details.
# returns: the complete behavior matrix.
#############################################################################################
#concatenate.behavior.values <- function(mep.details)
#{	tlog("Concatenating all the MEPs' behavior values")
#	
#	# if the file already exists, just load it
#	if(file.exists(MEP.BEHAVIOR.FILE))
#		result <- as.matrix(read.csv2(MEP.BEHAVIOR.FILE,check.names=FALSE))
#	
#	# otherwise, build the table and record it
#	else
#	{	# init the table
#		result <- cbind(NULL,as.integer(mep.details[,COL.MEPID]))
#		colnames(result)[1] <- COL.MEPID
#		
#		# get the list of document-wise vote files
#		file.list <- list.files(VW.RAW.FOLDER, no..=TRUE)
#		filename.list <- as.integer(sapply(file.list, function(n) substring(n,1,nchar(n)-4)))
#		idx <- match(sort(filename.list),filename.list)
#		file.list <- file.list[idx]
#		
#		# process each one of them
#		f <- 1
#		for(file in file.list)
#		{	tlog("Processing file ", file, " (",f,"/",length(file.list),")")
#			# read the file
#			path <- file.path(VW.RAW.FOLDER,file)
#			data <- as.matrix(read.csv2(path,check.names=FALSE))
#			f <- f + 1
#			
#			# add a new column to the table
#			result <- cbind(result, rep(NA,nrow(mep.details)))
#			colnames(result)[ncol(result)] <- substring(file,1,nchar(file)-4)
#			
#			# complete this new column
#			idx <- match(data[,VW.COL.NAME],mep.details[,COL.FULLNAME])
#			result[idx,ncol(result)] <- data[,VW.COL.BEHAVIOR]
#		}
#		
#		# record the table
#		write.csv2(result,file=MEP.BEHAVIOR.FILE,row.names=FALSE)
#	}
#	
#	return(result)
#}


#############################################################################################
# Load all the tables and returns them as a list.
#
# returns: a list containing all the loaded tables.
#############################################################################################
load.votewatch.data <- function()
{	tlog("***************************************************")
	tlog("****** LOAD VW DATA")
	tlog("***************************************************")
	
	result <- list()
	
	result$doc.details <- vw.clean.doc.details()
	result$mep.details <- vw.extract.mep.details()
	result$all.votes <- vw.concatenate.votes(result$mep.details)
	result$group.lines <- extract.group.lines(result$all.votes, result$mep.details)
	result$behavior.values <- process.behavior.values(result$all.votes, result$mep.details, result$group.lines)
		
	return(result)
}


#############################################################################################
# Test
#############################################################################################
#l <- load.raw.data()
#doc.details <- l$doc.details
#mep.details <- l$mep.details
#all.votes <- l$all.votes
#behavior.values <- l$behavior.values
