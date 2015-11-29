#############################################################################################
# Extracts raw data from the itsyourparliament Website.
# 
# 11/2015 Vincent Labatut
#############################################################################################
library("XML")
library("stringr")

source("src/define-constants.R")



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
# File names
#############################################################################################
IYP.MEPS.FILE			<- file.path(IYP.FOLDER,"mep-details.csv")
IYP.DOMAINS.LIST.FILE	<- file.path(IYP.DOMAINS.FOLDER,"_domains.xml")

#############################################################################################
# XML elements
#############################################################################################
IYP.ELT.VOTES	<- "votes"

#############################################################################################
# Column names
#############################################################################################
# mep details
	IYP.COL.MEPID		<- "mepid"
	IYP.COL.MEPNAME		<- "mepname"
	IYP.COL.FULLNAME	<- "fullname"
	IYP.COL.COUNTRY		<- "country"
	IYP.COL.TITLE		<- "title"
	IYP.COL.PARTY		<- "party"
	IYP.COL.BIRTHDATE	<- "birth"
	IYP.COL.BIRTHPLACE	<- "birthplace"
	IYP.COL.EP_ID		<- "europarlid"
	IYP.COL.GROUP		<- "group"
# vote details
	IYP.COL.VOTE.TITLE	<- "votetitle"
	IYP.COL.FULL.TITLE	<- "fulltitle"
	IYP.COL.POLICY.AREA	<- "policyarea"
	IYP.COL.DOC.REF		<- "docref"
	IYP.COL.EP.REF		<- "epref"
	IYP.COL.REPORTER.ID	<- "reporterid"
	IYP.COL.VOTE.DATE	<- "date"
	IYP.COL.MEP.VOTE	<- "mepvote"
	

iyp.extract.mep <- function(mep.id)
{	result <- c()
	
	# retrieve XML document
	url <- paste("http://itsyourparliament.eu/api/mep.php?id=",mep.id,sep="")
	page <- readLines(url)
	xml.data <- xmlParse(page)
	xml <- xmlToList(xml.data)
	
	# extract relevant XML elements
	result[IYP.COL.MEPNAME] <- str_trim(xml[[IYP.COL.MEPNAME]])
	result[IYP.COL.FULLNAME] <- str_trim(xml[[IYP.COL.FULLNAME]])
	result[IYP.COL.COUNTRY] <- str_trim(xml[[IYP.COL.COUNTRY]])
	result[IYP.COL.TITLE] <- str_trim(xml[[IYP.COL.TITLE]])
	result[IYP.COL.PARTY] <- str_trim(xml[[IYP.COL.PARTY]])
	result[IYP.COL.BIRTHDATE] <- str_trim(xml[[IYP.COL.BIRTHDATE]])
	result[IYP.COL.BIRTHPLACE] <- str_trim(xml[[IYP.COL.BIRTHPLACE]])
	result[IYP.COL.EP_ID] <- str_trim(xml[[IYP.COL.EP_ID]])
	result[IYP.COL.GROUP] <- str_trim(xml[[IYP.COL.GROUP]])
	
	# names
#	fullname <- str_trim(xml$fullname)
#	lastname <- str_trim(xml$mepname)
#	idx <- str_locate(fullname,fixed(lastname,ignore_case=TRUE))[1]-2
#	firstname <- substr(fullname,1,idx)
#	result <- c(result, lastname)
#	result <- c(result, firstname)
#	result <- c(result, fullname)
	
	return(result)
}


iyp.extract.meps <- function(mep.ids)
{	# build matrix
	cols <- c(IYP.COL.MEPNAME,IYP.COL.FULLNAME,IYP.COL.COUNTRY,IYP.COL.TITLE,IYP.COL.PARTY,IYP.COL.BIRTHDATE,IYP.COL.BIRTHPLACE,IYP.COL.EP_ID,IYP.COL.GROUP)
	result <- matrix(NA,nrow=length(mep.ids),ncol=length(cols))
	colnames(result) <- cols
	for(i in 1:length(mep.ids))
	{	data <- iyp.extract.mep(mep.ids[i])
		result[i,cols] <- data[cols]
	}
	result <- cbind(mep.ids,result)
	colnames(result)[1] <- IYP.COL.MEPID
	
	# record matrix
	write.csv(result,file=IYP.MEPS.FILE,row.names=FALSE)
	
	return(result)
}

iyp.extract.vote <- function(vote.id)
{	# retrieve XML document
	url <- paste("http://www.itsyourparliament.eu/api/vote.php?id=",vote.id,sep="")
	page <- readLines(url)
	xml.data <- xmlParse(page)
	xml <- xmlToList(xml.data)
	
	# extract vote information
	details <- c()
	details[IYP.COL.VOTE.TITLE] <- str_trim(xml[[IYP.COL.VOTE.TITLE]])
	details[IYP.COL.FULL.TITLE] <- str_trim(xml[[IYP.COL.FULL.TITLE]])
	details[IYP.COL.POLICY.AREA] <- str_trim(xml[[IYP.COL.POLICY.AREA]])
	details[IYP.COL.DOC.REF] <- str_trim(xml[[IYP.COL.DOC.REF]])
	details[IYP.COL.EP.REF] <- str_trim(xml[[IYP.COL.EP.REF]])
	details[IYP.COL.REPORTER.ID] <- str_trim(xml[[IYP.COL.REPORTER.ID]])
	details[IYP.COL.VOTE.DATE] <- str_trim(xml[[IYP.COL.VOTE.DATE]])
	
	# extract vote values
	votes <- c()
	for(i in 1:length(xml[[IYP.ELT.VOTES]]))
	{	v <- xml[[IYP.ELT.VOTES]][[i]]
		mep.id <- str_trim(v[[IYP.COL.MEPID]])
		vote.value <- str_trim(v[[IYP.COL.MEP.VOTE]])
		votes[mep.id] <- vote.value
	}
	
	result <- list(details=details, votes=votes)	
	return(result)
}

iyp.extract.votes <- function(vote.ids)
{	# build details matrix
	details.cols <- c(IYP.COL.VOTE.TITLE, IYP.COL.FULL.TITLE, IYP.COL.POLICY.AREA,
		IYP.COL.DOC.REF, IYP.COL.EP.REF, IYP.COL.REPORTER.ID, IYP.COL.VOTE.DATE)
	details.mat <- matrix(NA,nrow=length(vote.ids),ncol=length(details.cols))
	colnames(details.mat) <- cols
	
	# build vote values matrix
#	votes.mat <- matrix(NA,nrow=)
	
	for(i in 1:length(mep.ids))
	{	data <- iyp.extract.mep(mep.ids[i])
		result[i,cols] <- data[cols]
	}
	result <- cbind(mep.ids,result)
	colnames(result)[1] <- IYP.COL.MEPID
	
	# record matrices
	write.csv(result,file=IYP.MEPS.FILE,row.names=FALSE)
	
	return(result)
}


#x <- iyp.extract.mep(mep.id=136)
#x <- iyp.extract.meps(mep.ids=136:138)
#x <- iyp.extract.vote(vote.id=7000)
	

# TODO fonction qui récup tous les votes et en profite pour lister les MEP ids nécessaires (et toute autre info)
# TODO dans extraction raw : prendre tous les XML. puis, extraire simplement ce dont on a besoin dans le script de loading



#############################################################################################
# Retrieve the list of all MEPs from the website from www.itsyourparliament.eu, and record
# them as XML files for later use.
#############################################################################################
iyp.load.meps <- function()
{	max.id <- 870
	
	for(mep.id in 1:max.id)
	{	cat("Retrieving XML file for MEP id ",mep.id," (",mep.id,"/",max.id,")\n",sep="")
		url <- paste("http://itsyourparliament.eu/api/mep.php?id=",mep.id,sep="")
		page <- readLines(url)
		file <- paste(IYP.MEP.INFO.FOLDER,"/",mep.id,".xml",sep="")
		writeLines(page,file)
	}
}



#############################################################################################
# Retrieve the list of all votes from the website from www.itsyourparliament.eu, and record
# them as XML files for later use.
#############################################################################################
iyp.load.votes <- function()
{	max.id <- 7513
	
	for(vote.id in 1:max.id)
	{	cat("Retrieving XML file for vote id ",vote.id," (",vote.id,"/",max.id,")\n",sep="")
		url <- paste("http://www.itsyourparliament.eu/api/vote.php?id=",vote.id,sep="")
		page <- readLines(url)
		file <- paste(IYP.VOTES.FOLDER,"/",vote.id,".xml",sep="")
		writeLines(page,file)
	}
}



#############################################################################################
# Retrieve the list of all policy domains from the website from www.itsyourparliament.eu, and record
# them as XML files for later use.
#############################################################################################
iyp.load.domains <- function()
{	# get the list of domains
	cat("Retrieving XML file for domain list\n",sep="")
	url <- "http://itsyourparliament.eu/api/policyareas.php"
	page <- readLines(url)
	writeLines(page,IYP.DOMAINS.LIST.FILE)
	
	# get the list of votes for each domain
	dom.ids <- 26:58[-c(32,45,49,50,52)]
	for(d in 1:length(dom.ids))
	{	dom.id <- dom.ids[d]
		cat("Retrieving XML file for domain id ",dom.id," (",d,"/",length(dom.ids),")\n",sep="")
		url <- paste("http://www.itsyourparliament.eu/api/policyarea.php?id=",dom.id,sep="")
		page <- readLines(url)
		file <- paste(IYP.DOMAINS.FOLDER,"/",dom.id,".xml",sep="")
		writeLines(page,file)
	}
}
