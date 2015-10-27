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
	# raw data (i.e. tables)
	RAW.FOLDER <- file.path(VW.FOLDER,"raw")


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
	VW.COL.LOYALTY	<- "Loyal / Rebel to political group"
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
DOM.SYMB2CUSTOM <- c()
DOM.SYMB2CUSTOM[DOM.AFCO] <- "Constitutional and inter-institutional affairs"
DOM.SYMB2CUSTOM[DOM.AFET] <- "Foreign & security policy"
DOM.SYMB2CUSTOM[DOM.AGRI] <- "Agriculture"
DOM.SYMB2CUSTOM[DOM.BUDG] <- "Budget"
DOM.SYMB2CUSTOM[DOM.CONT] <- "Budgetary control"
DOM.SYMB2CUSTOM[DOM.CULT] <- "Culture & education"
DOM.SYMB2CUSTOM[DOM.DEVE] <- "Development"
DOM.SYMB2CUSTOM[DOM.FEMM] <- "Gender equality"
DOM.SYMB2CUSTOM[DOM.ECON] <- "Economic & monetary affairs"
DOM.SYMB2CUSTOM[DOM.EMPL] <- "Employment & social affairs"
DOM.SYMB2CUSTOM[DOM.ENVI] <- "Environment & public health"
DOM.SYMB2CUSTOM[DOM.IMCO] <- "Internal market & consumer protection"
DOM.SYMB2CUSTOM[DOM.INTA] <- "International trade"
DOM.SYMB2CUSTOM[DOM.ITRE] <- "Industry, research & energy"
DOM.SYMB2CUSTOM[DOM.JURI] <- "Legal affairs"
DOM.SYMB2CUSTOM[DOM.LIBE] <- "Civil liberties, justice & home affairs"
DOM.SYMB2CUSTOM[DOM.PECH] <- "Fisheries"
DOM.SYMB2CUSTOM[DOM.PETI] <- "Petitions"
DOM.SYMB2CUSTOM[DOM.REGI] <- "Regional development"
DOM.SYMB2CUSTOM[DOM.RIPE] <- "Internal regulations of the EP"
DOM.SYMB2CUSTOM[DOM.TRAN] <- "Transport & tourism"
# opposite map, used to convert VoteWatch domain names into official ones
DOM.CUSTOM2SYMB <- names(DOMAIN.FULLNAMES)
names(DOM.CUSTOM2SYMB) <- DOM.SYMB2CUSTOM
# the list of policy domain symbols
DOMAIN.VALUES <- sort(names(DOMAIN.FULLNAMES))
		

#############################################################################################
# Loads and cleans the file containing the document details.
#
# returns: a table containing the document details.
#############################################################################################
clean.doc.details <- function()
{	cat("Retrieving and cleaning the document details\n",sep="")
	
	# if the file already exists, just load it
	if(file.exists(DOMAIN.FREQ.FILE))
		result <- as.matrix(read.csv(DOC.DETAILS.FILE,check.names=FALSE))
	
	# otherwise, build the table and record it
	else
	{	# init the cleaned table
		result <- NULL
		
		# load the original table
		data <- as.matrix(read.csv2(DOC.DETAILS.RAW.FILE,check.names=FALSE))
		
		# build the table
		result <- cbind(result, as.integer(data[,VW.COL.DOCID]), data[,c(VW.COL.DATE,VW.COL.DOCNAME,VW.COL.RESULT)])
		# clean the domain names
		dom.ids <- DOM.CUSTOM2SYMB[data[,VW.COL.DOMAIN]]
		result <- cbind(result, dom.ids)
		# add the column names
		colnames(result) <- c(COL.DOCID,COL.DATE,COL.TITLE,COL.RESULT,COL.DOMID)
		
		# record the table
		write.csv(result,file=DOC.DETAILS.FILE,row.names=FALSE)
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
		file.list <- list.files(RAW.FOLDER, no..=TRUE)
		
		# process each one of them
		f <- 1
		for(file in file.list)
		{	cat("Processing file ", file, " (",f,"/",length(file.list),")\n",sep="")
			# read the file
			path <- file.path(RAW.FOLDER,file)
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
		
		# split the names
		names <- sapply(result[,COL.FULLNAME], split.name)
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
		result <- cbind(NULL,as.integer(mep.details[,COL.MEPID]))
		colnames(result)[1] <- COL.MEPID
		
		# get the list of document-wise vote files
		file.list <- list.files(RAW.FOLDER, no..=TRUE)
		filename.list <- as.integer(sapply(file.list, function(n) substring(n,1,nchar(n)-4)))
		idx <- match(sort(filename.list),filename.list)
		file.list <- file.list[idx]
		
		# process each one of them
		f <- 1
		for(file in file.list)
		{	cat("Processing file ", file, " (",f,"/",length(file.list),")\n",sep="")
			# read the file
			path <- file.path(RAW.FOLDER,file)
			data <- as.matrix(read.csv(path,check.names=FALSE))
			f <- f + 1
			
			# add a new column to the table
			result <- cbind(result, rep(NA,nrow(mep.details)))
			colnames(result)[ncol(result)] <- substring(file,1,nchar(file)-4)
			
			# complete this new column
			idx <- match(data[,VW.COL.NAME],mep.details[,COL.FULLNAME])
			result[idx,ncol(result)] <- data[,VW.COL.VOTE]
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
		result <- cbind(NULL,as.integer(mep.details[,COL.MEPID]))
		colnames(result)[1] <- COL.MEPID
		
		# get the list of document-wise vote files
		file.list <- list.files(RAW.FOLDER, no..=TRUE)
		filename.list <- as.integer(sapply(file.list, function(n) substring(n,1,nchar(n)-4)))
		idx <- match(sort(filename.list),filename.list)
		file.list <- file.list[idx]
		
		# process each one of them
		f <- 1
		for(file in file.list)
		{	cat("Processing file ", file, " (",f,"/",length(file.list),")\n",sep="")
			# read the file
			path <- file.path(RAW.FOLDER,file)
			data <- as.matrix(read.csv(path,check.names=FALSE))
			f <- f + 1
			
			# add a new column to the table
			result <- cbind(result, rep(NA,nrow(mep.details)))
			colnames(result)[ncol(result)] <- substring(file,1,nchar(file)-4)
			
			# complete this new column
			idx <- match(data[,VW.COL.NAME],mep.details[,COL.FULLNAME])
			result[idx,ncol(result)] <- data[,VW.COL.LOYALTY]
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
#load.raw.data <- function()
#{	result <- list()
#	
#	# document-related
#	result$doc.details <- clean.doc.details()
#	result$domain.details <- extract.domains(result$doc.details)
#	
#	# MEP-related
#	result$mep.details <- extract.mep.details()
#	result$all.votes <- concatenate.votes(result$mep.details)
#	result$loyalty.values <- concatenate.loyalties(result$mep.details)
#	
#	return(result)
#}
doc.details <- clean.doc.details()
mep.details <- extract.mep.details()
all.votes <- concatenate.votes(mep.details)
loyalty.values <- concatenate.loyalties(mep.details)


#############################################################################################
# Test
#############################################################################################
#l <- load.raw.data()
#doc.details <- l$doc.details
#mep.details <- l$mep.details
#all.votes <- l$all.votes
#loyalty.values <- l$loyalty.values
