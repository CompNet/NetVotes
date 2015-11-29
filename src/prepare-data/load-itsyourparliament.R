#############################################################################################
# This script processes the raw data exported from the Itsyourparliament website, in order to 
# extract some tables the main script can then use.
# 
# 11/2015 Vincent Labatut
#############################################################################################
library("XML")
library("stringr")

source("src/prepare-data/load-common.R")



#############################################################################################
# Folder names
#############################################################################################
# "It's your parliament" data
IYP.FOLDER <- file.path(IN.FOLDER,"itsyourparliament")
	# MEP data
	IYP.MEPS.FOLDER <- file.path(IYP.FOLDER,"meps")
	# Vote data
	IYP.VOTES.FOLDER <- file.path(IYP.FOLDER,"votes")
	# Policy areas data
	IYP.DOMAINS.FOLDER <- file.path(IYP.FOLDER,"domains")


#############################################################################################
# Folder names
#############################################################################################
# list of domains
IYP.DOMAINS.LIST.FILE <- file.path(IYP.DOMAINS.FOLDER,"_domains.xml")


#############################################################################################
# XML elements and attributes
#############################################################################################
# mep details
	IYP.ELT.MEPID		<- "mepid"
	IYP.ELT.MEPNAME		<- "mepname"
	IYP.ELT.FULLNAME	<- "fullname"
	IYP.ELT.COUNTRY		<- "country"
	IYP.ELT.TITLE		<- "title"
	IYP.ELT.PARTY		<- "party"
	IYP.ELT.BIRTHDATE	<- "birth"
	IYP.ELT.BIRTHPLACE	<- "birthplace"
	IYP.ELT.EP_ID		<- "europarlid"
	IYP.ELT.GROUP		<- "group"
# vote details
	IYP.ELT.VOTE.TITLE	<- "votetitle"
	IYP.ELT.FULL.TITLE	<- "fulltitle"
	IYP.ELT.POLICY.AREA	<- "policyarea"
	IYP.ELT.DOC.REF		<- "docref"
	IYP.ELT.EP.REF		<- "epref"
	IYP.ELT.REPORTER.ID	<- "reporterid"
	IYP.ELT.VOTE.DATE	<- "date"
	IYP.ELT.MEP.VOTE	<- "mepvote"
	IYP.ELT.VOTES		<- "votes"
# domain details
	IYP.ELT.COMMITTEE	<- "committee"
	IYP.ELT.ID			<- "id"
	IYP.ELT.POLICY.NAME	<- "policyarea_name"
	IYP.ELT.VOTEID		<- "voteid"
	
	


#############################################################################################
# Read the XML file corresponding to the specified MEP id, and returns the corresponding
# vector of strings.
#
# mep.id: ID of the MEP (in IYP).
# returns: string vector representing the MEP details.
#############################################################################################
iyp.extract.mep <- function(mep.id)
{	result <- c()
	
	# retrieve XML document
	file <- paste(IYP.MEPS.FOLDER,"/",mep.id,".xml",sep="")
	doc <- readLines(file)
	xml.data <- xmlParse(doc)
	xml <- xmlToList(xml.data)
	
	# id
	result[COL.MEPID] <- mep.id
	
	# names
	fullname <- str_trim(xml[[IYP.ELT.FULLNAME]])
	lastname <- str_trim(xml[[IYP.ELT.MEPNAME]])
	idx <- str_locate(fullname,fixed(lastname,ignore_case=TRUE))[1]-2
	firstname <- substr(fullname,1,idx)
	result[COL.LASTNAME] <- lastname
	result[COL.FIRSTNAME] <- firstname
	result[COL.FULLNAME] <- fullname
	
	# other details
	result[COL.STATE] <- str_trim(xml[[IYP.ELT.COUNTRY]])
	result[COL.GROUP] <- str_trim(xml[[IYP.ELT.GROUP]])
	result[COL.TITLE] <- str_trim(xml[[IYP.ELT.TITLE]])
	result[COL.PARTY] <- str_trim(xml[[IYP.ELT.PARTY]])
	result[COL.BIRTHDATE] <- str_trim(xml[[IYP.ELT.BIRTHDATE]])
	result[COL.BIRTHPLACE] <- str_trim(xml[[IYP.ELT.BIRTHPLACE]])
	result[COL.EP.ID] <- str_trim(xml[[IYP.ELT.EP_ID]])
	
	return(result)
}



#############################################################################################
# Read the XML files corresponding to the specified MEP ids, and returns the corresponding
# string table.
#
# mep.ids: IDs of the MEP (in IYP).
# returns: string array representing the MEP details.
#############################################################################################
iyp.extract.meps <- function(mep.ids)
{	# build matrix
	cols <- c(COL.MEPID, COL.LASTNAME, COL.FIRSTNAME,
		COL.FULLNAME, COL.STATE, COL.GROUP, COL.TITLE,
		COL.PARTY, COL.BIRTHDATE, COL.BIRTHPLACE, COL.EP.ID)
	result <- matrix(NA,nrow=length(mep.ids),ncol=length(cols))
	colnames(result) <- cols
	for(i in 1:length(mep.ids))
	{	data <- iyp.extract.mep(mep.ids[i])
		result[i,cols] <- data[cols]
	}
	
	# record matrix
	write.csv(result,file=MEP.DETAILS.FILE,row.names=FALSE)
	
	return(result)
}



#############################################################################################
# Read the XML file corresponding to the specified domain id, and returns the corresponding
# vector vote ids.
#
# domain.id: ID of the domain (in IYP).
# returns: vector of vote ids.
#############################################################################################
iyp.extract.domain <- function(domain.id)
{	# retrieve XML document
	file <- paste(IYP.DOMAINS.FOLDER,"/",domain.id,".xml",sep="")
	doc <- readLines(file)
	xml.data <- xmlParse(doc)
	xml <- xmlToList(xml.data)
	
	# process each listed vote
	result <- rep(NA,length(xml))
	for(i in 1:length(xml))
	{	vote <- xml[[i]]
		vote.id <- str_trim(vote[[IYP.ELT.VOTEID]])
		result[i] <- vote.id
	}
	
	return(result)
}



#############################################################################################
# Read the XML file listing the domains, then process each domain, and returns a table representing
# the domains as well as a list of vectors of vote ids (one for each domain).
#
# returns: a list containing the table (details) and the lists (votes).
#############################################################################################
iyp.extract.domains <- function()
{	# retrieve the main XML document
	doc <- readLines(IYP.DOMAINS.LIST.FILE)
	xml.data <- xmlParse(doc)
	xml <- xmlToList(xml.data)
	
	# init table and vote list
	cols <- c(IYP.ELT.ID, IYP.ELT.COMMITTEE, IYP.ELT.POLICY.NAME)
	domains <- matrix(NA,nrow=length(xml),ncol=length(cols))
	colnames(domains) <- cols
	votes <- list()
	
	# process each domain
	for(i in 1:length(xml))
	{	# update table
		domain <- xml[[i]]
		domains[i,IYP.ELT.ID] <- str_trim(domain[[IYP.ELT.ID]])
		domains[i,IYP.ELT.COMMITTEE] <- str_trim(domain[[IYP.ELT.COMMITTEE]])
		domains[i,IYP.ELT.POLICY.NAME] <- str_trim(domain[[IYP.ELT.POLICY.NAME]])
		
		# update list
		vts <- iyp.extract.domain(domains[i,IYP.ELT.ID])
		votes[[domains[i,IYP.ELT.ID]]] <- vts
	}
	
	# record table (debug only)
	file <- paste(IYP.DOMAINS.FOLDER,"/","_domains.csv",sep="")
	write.csv(domains,file=file,row.names=FALSE)
	
	result <- list(details=domains, votes=votes)
	return(result)
}



#############################################################################################
#############################################################################################
iyp.extract.vote <- function(vote.id)
{	# retrieve XML document
	file <- paste(IYP.VOTES.FOLDER,"/",vote.id,".xml",sep="")
	doc <- readLines(file)
	xml.data <- xmlParse(doc)
	xml <- xmlToList(xml.data)
	
	# extract vote information
	details <- c()
	details[IYP.ELT.VOTE.TITLE] <- str_trim(xml[[IYP.ELT.VOTE.TITLE]])
	details[IYP.ELT.FULL.TITLE] <- str_trim(xml[[IYP.ELT.FULL.TITLE]])
	details[IYP.ELT.POLICY.AREA] <- str_trim(xml[[IYP.ELT.POLICY.AREA]])
	details[IYP.ELT.DOC.REF] <- str_trim(xml[[IYP.ELT.DOC.REF]])
	details[IYP.ELT.EP.REF] <- str_trim(xml[[IYP.ELT.EP.REF]])
	details[IYP.ELT.REPORTER.ID] <- str_trim(xml[[IYP.ELT.REPORTER.ID]])
	details[IYP.ELT.VOTE.DATE] <- str_trim(xml[[IYP.ELT.VOTE.DATE]])
	
	# extract vote values
	votes <- c()
	for(i in 1:length(xml[[IYP.ELT.VOTES]]))
	{	v <- xml[[IYP.ELT.VOTES]][[i]]
		mep.id <- str_trim(v[[IYP.ELT.MEPID]])
		vote.value <- str_trim(v[[IYP.ELT.MEP.VOTE]])
		votes[mep.id] <- vote.value
	}
	
	result <- list(details=details, votes=votes)	
	return(result)
}



#############################################################################################
#############################################################################################
iyp.extract.votes <- function(vote.ids)
{	# build details matrix
	details.cols <- c(IYP.ELT.VOTE.TITLE, IYP.ELT.FULL.TITLE, IYP.ELT.POLICY.AREA,
		IYP.ELT.DOC.REF, IYP.ELT.EP.REF, IYP.ELT.REPORTER.ID, IYP.ELT.VOTE.DATE)
	details.mat <- matrix(NA,nrow=length(vote.ids),ncol=length(details.cols))
	colnames(details.mat) <- cols
	
	# build vote values matrix
#	votes.mat <- matrix(NA,nrow=)
	
	for(i in 1:length(mep.ids))
	{	data <- iyp.extract.mep(mep.ids[i])
		result[i,cols] <- data[cols]
	}
	result <- cbind(mep.ids,result)
	colnames(result)[1] <- IYP.ELT.MEPID
	
	# record matrices
	write.csv(result,file=IYP.MEPS.FILE,row.names=FALSE)
	
	return(result)
}



# TODO
# - charger chaque vote et constituer les tables all-votes et doc-details
# - calculer la ligne de groupe et constituer la table group-lines
# - calculer la loyauté et constituer la table mep-behavior
# (confronter aux fonctions communes dispo)
