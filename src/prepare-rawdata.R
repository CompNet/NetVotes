#############################################################################################
# This script processes the raw data exported from the VoteWatch website, in order to extract
# some tables the main script can then use. It is *not* called by the main script: the data
# given with this project already contain the files resulting of this preprocessing. So, this
# script does not need to be executed before the main one.
# 
# 07/2015 Israel Mendonça (v1)
# 09/2015 Vincent Labatut (v2)
#############################################################################################


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
	COL.POLICY		<- "Policy area"
# created tables
	COL.MEPID		<- "MEP Id"
	COL.LASTNAME	<- "Lastname"
	COL.FIRSTNAME	<- "Firstname"
	
#############################################################################################
# Parses the collection of files describing the individual votes of each document,
# and extract the MEPs' details.
#
# returns: a table containing the MEPs and their details.
#############################################################################################
extract.mep.details <- function()
{	# if the file already exists, just load it
#	if(file.exists(MEP.DETAILS.FILE))
#		result <- read.csv(MEP.DETAILS.FILE)
#	
#	# otherwise, build the table and record it
#	else
	{	# init the table
		result <- NULL
		
		# get the list of document-wise vote files
		file.list <- list.files(ORIG.FOLDER, no..=TRUE)
		
		# process each one of them
		f <- 1
		for(file in file.list)
		{	cat("Processing file ", file, "(",f,"/",length(file.list),")\n",sep="")
			# read the file
			path <- file.path(ORIG.FOLDER,file)
			data <- as.matrix(read.csv(path,check.names=FALSE))
			tmp <- colnames(data)
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
# Concatenate the votes data contained in the individual document, in order to get
# a single, more convenient matrix.
#
# returns: the complete vote matrix.
#############################################################################################
concatenate.votes <- function()
{	# if the file already exists, just load it
	if(file.exists(ALL.VOTES.FILE))
		result <- read.csv(ALL.VOTES.FILE)
	
	# otherwise, build the table and record it
	else
	{	# get the list of document-wise vote files
		file.list <- list.files(ORIG.FOLDER, no..=TRUE)
		
		# process each one of them
		for(file in file.list)
		{	# read the file
			path <- file.path(ORIG.FOLDER,file)
			data <- read.csv(path)
			
			# add to the table
			
		}
	}
	
	return(result)
}




##############################################################################################
##Pre-Processing of all the files
##Function to read all the files from the folder and return a list containing all the MPS
#createMPsList <- function(data.dir,votes.files,columns.names.MPs){
#  
#  temp <- read.csv(file.path(data.dir,votes.files[1]))
#  MPs <- temp[ columns.names.MPs ]
#  names(MPs) <- c("names","country","political_group")
#  warning <- ""
#  
#  for( i in  2:length(votes.files) ){
#    file_name <- file.path(data.dir,votes.files[i])
#    temp <- read.csv(file_name)
#    MPs.temp <- temp[ columns.names.MPs ]
#    names(MPs.temp) <- c("names","country","political_group")
#    
#    MPs <- merge(MPs,MPs.temp,by.x="names",by.y="names",all=TRUE)
#    
#    new.MPs <- which(is.na( MPs[,2] )) 				 
#    if(length(new.MPs)) MPs[new.MPs, c(2,3) ] <- MPs[new.MPs, c(4,5)]	 
#    MPs <- MPs[1:3]									
#    
#    names(MPs) <- names(MPs) <- c("names","country","political_group")
#    
#  }
#  MPs <- cbind(paste0("m_",rownames(MPs)),MPs)
#  names(MPs) <- c("id","names","country","political_group")
#  
#  write.csv(MPs, file=file.path(generation.dir,MPs.filename),row.names = FALSE)
#  
#  print(warning)
#  
#  return(MPs)
#}
#
##Read all the votes documents and return a data frame containing (all the MPs X Votes in Documents)
#create.allvotes.table <- function(data.dir,votes.files,columns.names.MPs,columns.names.votes,MPs){
#  
#  big.table <- data.frame(MP=MPs)
#  big.table$ind <- 1:nrow(big.table)
#  names(big.table) <- c("mp_id","names","country","political_group","ind")
#  
#  for( i in  1:length(votes.files) ){
#    
#    smaller.big.table <- big.table[c("ind","names")]
#    
#    file_name <- file.path(data.dir,votes.files[i])
#    temp <- read.csv(file_name)[c(columns.names.votes,columns.names.MPs[1])]
#    names(temp) <- c(columns.names.votes,"names")
#    
#    merged <- merge(temp,smaller.big.table,by.x="names",by.y="names")
#    
#    ncol <- ncol(big.table)
#    big.table[as.integer(merged$ind),ncol+1] <- merged[columns.names.votes]
#    names(big.table)[ncol+1] <- votes.files[i]
#  }
#  
#  write.csv(big.table, file=file.path(generation.dir,allvotes.filename),row.names = FALSE)
#  
#  return(big.table)
#}
#
#CreateLoyaltyTable <- function(data.dir,votes.files,columns.names.MPs,columns.names.loyalty,MPs) {
#  
#  loyalty.table <- data.frame(MP=MPs)
#  loyalty.table$ind <- 1:nrow(loyalty.table)
#  names(loyalty.table) <- c("mp_id","names","country","political_group","ind")
#  print(columns.names.loyalty)
#  
#  for( i in  1:length(votes.files) ){
#    
#    smaller.loyalty.table <- loyalty.table[c("ind","names")]
#    
#    file_name <- file.path(data.dir,votes.files[i])
#    temp <- read.csv(file_name)[c(columns.names.loyalty,columns.names.MPs[1])]
#    names(temp) <- c(columns.names.loyalty,"names")
#    
#    merged <- merge(temp,smaller.loyalty.table,by.x="names",by.y="names")
#    
#    ncol <- ncol(loyalty.table)
#    loyalty.table[as.integer(merged$ind),ncol+1] <- merged[columns.names.loyalty]
#    names(loyalty.table)[ncol+1] <- votes.files[i]
#  }
#  write.csv(loyalty.table, file=file.path(generation.dir,loyalty.filename),row.names = FALSE)
#  
#  return(loyalty.table)
#  
#}
#
#
#if(length(columns.names.MPs)!=3)
#  stop(paste0("In file config_Files&Directories.R , variable columns.names.MPs must indicate exactly 3 colum names. Please modify it regarding the .csv files contained in directory ",data.dir))
#
#if(length(columns.names.votes)!=1)		
#  stop(paste0("In file config_Files&Directories.R , variable columns.names.vote must indicate exactly 3 colum names. Please modify it regarding the .csv files contained in directory ",data.dir))
#
##Generate a list of files containing all the vote documents
#votes.files <- list.files(data.dir)
#
##Create a folder "generation.dir" if doesn't exist
#dir.create(generation.dir, showWarnings = FALSE) 
#
##Store all the MPs in a variable
#if (!MPs.filename %in% list.files(generation.dir)){ 					
#  # If the file was not created before, creates the file with all the MPs
#  MPs <- createMPsList(data.dir,votes.files,columns.names.MPs) 		
#}else{
#  #If the file already exists, read it
#  filename <- file.path(generation.dir,MPs.filename)
#  MPs <- read.csv(filename)
#}
## Renaming the colums of the table "MPs"
#names(MPs) <- c("id","names","country","political_group") 		
## Edits the row names of MPs with the MPs ids
#rownames(MPs) <- MPs$id 								
#
##File listing the names of all the vote sessions
#filename <- file.path(data.dir2,docs.filename)
#docs <- read.csv(filename)
#
## TODO: change because it's a bit dirty
#set <- unlist(strsplit(votes.files,"\\."))[2*(1:length(votes.files))-1]	#dirty
#votes.files <- votes.files[order(as.integer(set))] 						#dirty
#docs$doc_id <- votes.files 												#dirty
#
#
##Create the Big Table, the one containing all the votes and MPs
## Check if the file has been created already
#if (!allvotes.filename %in% list.files(generation.dir)){
#  # Creates the file and puts the result in the variable "big.table"  
#  big.table <- create.allvotes.table(data.dir,votes.files,columns.names.MPs,columns.names.votes,MPs) 	
#}else{
#  # Reads the file and puts the result in the variable "big.table"  			
#  filename <- file.path(generation.dir,allvotes.filename)
#  big.table <- read.csv(filename,check.names=FALSE)
#}
#rownames(big.table) <- big.table$mp_id 	# edits the row names of big.table with the MPs ids
#
## Check if the file has been created already
#if (!loyalty.filename %in% list.files(generation.dir)){
#  # Creates the file and puts the result in the variable "big.table"  
#  loyalty.table <- CreateLoyaltyTable(data.dir,votes.files,columns.names.MPs,columns.names.loyalty,MPs)   
#}else{
#  # Reads the file and puts the result in the variable "big.table"  			
#  filename <- file.path(generation.dir,loyalty.filename)
#  loyalty.table <- read.csv(filename,check.names=FALSE)
#}
#rownames(loyalty.table) <- loyalty.table$mp_id   # edits the row names of big.table with the MPs ids
