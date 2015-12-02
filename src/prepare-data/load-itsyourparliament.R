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
# File names
#############################################################################################
# XML list of domains
IYP.DOMAIN.LIST.FILE <- file.path(IYP.DOMAINS.FOLDER,"_domains.xml")


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
# Domain mapping
#############################################################################################
# map used to convert official domain names into VoteWatch ones
DOMAIN.IYP2SYMB <- c()
DOMAIN.IYP2SYMB["41"] <- DOMAIN.AFCO
DOMAIN.IYP2SYMB["34"] <- DOMAIN.AFET
DOMAIN.IYP2SYMB["37"] <- DOMAIN.AGRI
DOMAIN.IYP2SYMB["44"] <- DOMAIN.BUDG
DOMAIN.IYP2SYMB["42"] <- DOMAIN.CONT
DOMAIN.IYP2SYMB["40"] <- DOMAIN.CULT
DOMAIN.IYP2SYMB["48"] <- DOMAIN.DEVE
DOMAIN.IYP2SYMB["32"] <- DOMAIN.FEMM
DOMAIN.IYP2SYMB["53"] <- DOMAIN.FEMM
DOMAIN.IYP2SYMB["28"] <- DOMAIN.ECON
DOMAIN.IYP2SYMB["31"] <- DOMAIN.EMPL
DOMAIN.IYP2SYMB["38"] <- DOMAIN.ENVI
DOMAIN.IYP2SYMB["43"] <- DOMAIN.IMCO
DOMAIN.IYP2SYMB["29"] <- DOMAIN.INTA
DOMAIN.IYP2SYMB["33"] <- DOMAIN.ITRE
DOMAIN.IYP2SYMB["39"] <- DOMAIN.JURI
DOMAIN.IYP2SYMB["35"] <- DOMAIN.LIBE
DOMAIN.IYP2SYMB["26"] <- DOMAIN.PECH
DOMAIN.IYP2SYMB["47"] <- DOMAIN.PETI
DOMAIN.IYP2SYMB["27"] <- DOMAIN.REGI
#DOMAIN.IYP2SYMB["Internal regulations of the EP"] <- DOMAIN.RIPE #TODO à compléter en recherchant la catégorie où ces docs là se retrouve avec IYP
DOMAIN.IYP2SYMB["30"] <- DOMAIN.TRAN
DOMAIN.IYP2SYMB["36"] <- DOMAIN.AUTR
DOMAIN.IYP2SYMB["45"] <- DOMAIN.AUTR
DOMAIN.IYP2SYMB["46"] <- DOMAIN.AUTR
DOMAIN.IYP2SYMB["49"] <- DOMAIN.AUTR
DOMAIN.IYP2SYMB["50"] <- DOMAIN.AUTR
DOMAIN.IYP2SYMB["51"] <- DOMAIN.AUTR
DOMAIN.IYP2SYMB["52"] <- DOMAIN.AUTR
DOMAIN.IYP2SYMB["54"] <- DOMAIN.AUTR
DOMAIN.IYP2SYMB["55"] <- DOMAIN.AUTR
DOMAIN.IYP2SYMB["56"] <- DOMAIN.AUTR
DOMAIN.IYP2SYMB["57"] <- DOMAIN.AUTR
DOMAIN.IYP2SYMB["58"] <- DOMAIN.AUTR
		

#############################################################################################
# Vote mapping
#############################################################################################
VOTE.IYP2SYMB <- c()
VOTE.IYP2SYMB["For"] <- VOTE.FOR
VOTE.IYP2SYMB["Abstention"] <- VOTE.ABST
VOTE.IYP2SYMB["Against"] <- VOTE.AGST
#VOTE.IYP2SYMB["Didn't vote"] <- VOTE.NONE
#VOTE.IYP2SYMB["Absent"] <- VOTE.ABSENT
#VOTE.IYP2SYMB["Documented Absence"] <- VOTE.DOCABSENT


#############################################################################################
# Group mapping
#############################################################################################
GROUP.IYP2SYMB <- c()
GROUP.IYP2SYMB["ALDE"] <- GROUP.ALDE
GROUP.IYP2SYMB["ECR"] <- GROUP.ECR
GROUP.IYP2SYMB["EFD"] <- GROUP.EFD
GROUP.IYP2SYMB["PPE"] <- GROUP.EPP
GROUP.IYP2SYMB["Verts/ALE"] <- GROUP.GREENS
GROUP.IYP2SYMB["GUE/NGL"] <- GROUP.GUENGL
GROUP.IYP2SYMB["NI"] <- GROUP.NI
GROUP.IYP2SYMB["SD"] <- GROUP.SD


#############################################################################################
# Read the XML file corresponding to the specified MEP id, and returns the corresponding
# vector of strings.
#
# mep.id: ID of the MEP (in IYP).
# returns: string vector representing the MEP details.
#############################################################################################
iyp.extract.mep.details <- function(mep.id)
{	cat("Processing MEP ",mep.id,"\n",sep="")
	result <- c()
	
	# retrieve XML document
	file <- paste(IYP.MEPS.FOLDER,"/",mep.id,".xml",sep="")
	doc <- readLines(file)
	xml.data <- xmlParse(doc)
	xml <- xmlToList(xml.data)
	
	# id
	result[IYP.ELT.MEPID] <- mep.id
	
	# names
	fullname <- str_trim(xml[[IYP.ELT.FULLNAME]])
	lastname <- str_trim(xml[[IYP.ELT.MEPNAME]])
#	idx <- str_locate(fullname,fixed(lastname,ignore_case=TRUE))[1]-2
	idx <- str_locate(fullname,ignore.case(lastname))[1]-2
	
	firstname <- substr(fullname,1,idx)
	result[COL.LASTNAME] <- lastname
	result[COL.FIRSTNAME] <- firstname
	result[COL.FULLNAME] <- fullname
	
	# other details
	result[COL.STATE] <- str_trim(xml[[IYP.ELT.COUNTRY]])
	result[COL.GROUP] <- GROUP.IYP2SYMB[[str_trim(xml[[IYP.ELT.GROUP]])]]
	result[COL.TITLE] <- str_trim(xml[[IYP.ELT.TITLE]])
	result[COL.PARTY] <- str_trim(xml[[IYP.ELT.PARTY]])
	result[COL.BIRTHDATE] <- str_trim(xml[[IYP.ELT.BIRTHDATE]])
	result[COL.BIRTHPLACE] <- str_trim(xml[[IYP.ELT.BIRTHPLACE]])
	result[COL.EP.ID] <- str_trim(xml[[IYP.ELT.EP_ID]])
	
	return(result)
}



#############################################################################################
# Read the XML files corresponding to all the MEP ids, and returns the corresponding
# string table.
#
# returns: string array representing the MEP details.
#############################################################################################
iyp.extract.meps.details <- function()
{	cat("Retrieving the MEPs details\n",sep="")
	dir.create(OVERALL.FOLDER, recursive=TRUE, showWarnings=FALSE)
	
	# if the file already exists, just load it
	if(file.exists(MEP.DETAILS.FILE))
	{	result <- as.matrix(read.csv2(MEP.DETAILS.FILE,check.names=FALSE))
		result[,COL.MEPID] <- as.integer(result[,COL.MEPID])
		result[,IYP.ELT.MEPID] <- as.integer(result[,IYP.ELT.MEPID])
	}
	
	# otherwise, build the table and record it
	else
	{	# retrieve the list of MEP ids
		files <- list.files(path=IYP.MEPS.FOLDER, full.names=FALSE, no..=TRUE)
		mep.ids <- c()
		for(file in files)
			mep.ids <- c(mep.ids,substr(file,1,str_locate(file,".xml")-1))
		mep.ids <- sort(as.integer(mep.ids))
		
		# build the matrix
		cols <- c(COL.MEPID, COL.LASTNAME, COL.FIRSTNAME,
			COL.FULLNAME, COL.STATE, COL.GROUP, COL.TITLE,
			COL.PARTY, COL.BIRTHDATE, COL.BIRTHPLACE, COL.EP.ID, IYP.ELT.MEPID)
		result <- matrix(NA,nrow=length(mep.ids),ncol=length(cols))
		colnames(result) <- cols
		for(i in 1:length(mep.ids))
		{	data <- iyp.extract.mep.details(mep.ids[i])
			data[COL.MEPID] <- i
			result[i,cols] <- data[cols]
		}
		
		# record matrix
		write.csv2(result,file=MEP.DETAILS.FILE,row.names=FALSE)
	}
	
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
{	cat("Processing domain ",domain.id,"\n",sep="")
	
	# retrieve XML document
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
# returns: a vector associating a vote id to the corresponding policy domain.
#############################################################################################
iyp.extract.domains <- function()
{	cat("Retrieving the domain details\n",sep="")
	dir.create(OVERALL.FOLDER, recursive=TRUE, showWarnings=FALSE)
	
	# if the file already exists, just load it
	if(file.exists(DOC.DOMAINS.FILE))
	{	result <- as.matrix(read.csv2(DOC.DOMAINS.FILE,check.names=FALSE))
		result[,IYP.ELT.VOTEID] <- as.integer(result[,IYP.ELT.VOTEID])
	}
	
	# otherwise, build the table and record it
	else
	{	# get the ids of the effective domains
		files <- list.files(path=IYP.DOMAINS.FOLDER, full.names=FALSE, no..=TRUE)
		dom.ids <- c()
		for(file in files)
			dom.ids <- c(dom.ids,substr(file,1,str_locate(file,".xml")-1))
		dom.ids <- suppressWarnings(as.integer(dom.ids))
		dom.ids <- dom.ids[!is.na(dom.ids)]
		dom.ids <- sort(dom.ids)
		
		# retrieve the main XML document
		doc <- readLines(IYP.DOMAIN.LIST.FILE)
		xml.data <- xmlParse(doc)
		xml <- xmlToList(xml.data)
	
		# init table and vote list
		cols <- c(IYP.ELT.ID, IYP.ELT.COMMITTEE, IYP.ELT.POLICY.NAME)
		domains <- matrix(NA,nrow=length(xml),ncol=length(cols))
		colnames(domains) <- cols
		result <- NULL
		
		# process each domain
		for(i in 1:length(xml))
		{	# update table
			domain <- xml[[i]]
			dom.id <- str_trim(domain[[IYP.ELT.ID]])
			
			# update details table
			domains[i,IYP.ELT.ID] <- dom.id
			domains[i,IYP.ELT.COMMITTEE] <- str_trim(domain[[IYP.ELT.COMMITTEE]])
			domains[i,IYP.ELT.POLICY.NAME] <- str_trim(domain[[IYP.ELT.POLICY.NAME]])
			
			# update vote list
			if(dom.id %in% dom.ids)
			{	vote.ids <- iyp.extract.domain(domains[i,IYP.ELT.ID])
				domain.code <- DOMAIN.IYP2SYMB[[domains[i,IYP.ELT.ID]]]
				result <- rbind(result,
					cbind(vote.ids, rep(domain.code,length(vote.ids))))
			}
		}
		
		# finalize and record result vector
		colnames(result) <- c(IYP.ELT.VOTEID,COL.DOMID)
		result <- result[order(as.integer(result[,IYP.ELT.VOTEID])),]
		write.csv2(result,file=DOC.DOMAINS.FILE,row.names=FALSE)
		
		# record domain details table (not needed, just for information)
		write.csv2(domains,file=DOMAIN.DETAILS.FILE,row.names=FALSE)
	}
		
	return(result)
}



#############################################################################################
# Read the XML file corresponding to the specified vote id, and returns the corresponding
# details and MEP vote values.
#
# vote.id: ID of the vote (in IYP).
# returns: a list containing the vote information (details) and the MEP vote values (votes).
#############################################################################################
iyp.extract.vote <- function(vote.id)
{	cat("Processing vote ",vote.id,"\n",sep="")
	
	# retrieve XML document
	file <- paste(IYP.VOTES.FOLDER,"/",vote.id,".xml",sep="")
	doc <- readLines(file)
	xml.data <- xmlParse(doc)
	xml <- xmlToList(xml.data)
	
	# extract vote information
	details <- c()
	details[IYP.ELT.VOTEID] <- vote.id
	details[COL.TITLE] <- str_trim(xml[[IYP.ELT.VOTE.TITLE]])
	details[COL.FULL.TITLE] <- str_trim(xml[[IYP.ELT.FULL.TITLE]])
	details[COL.DOMID] <- DOMAIN.IYP2SYMB[[str_trim(xml[[IYP.ELT.POLICY.AREA]])]]
	details[COL.DOC.REF] <- str_trim(xml[[IYP.ELT.DOC.REF]])
	details[COL.EP.REF] <- str_trim(xml[[IYP.ELT.EP.REF]])
	details[COL.REPORTER.ID] <- str_trim(xml[[IYP.ELT.REPORTER.ID]])
	details[COL.DATE] <- str_trim(xml[[IYP.ELT.VOTE.DATE]])	
	
	# extract vote values
	votes <- c()
	for(i in 1:length(xml[[IYP.ELT.VOTES]]))
	{	v <- xml[[IYP.ELT.VOTES]][[i]]
		mep.id <- str_trim(v[[IYP.ELT.MEPID]])
		vote.value <- VOTE.IYP2SYMB[[str_trim(v[[IYP.ELT.MEP.VOTE]])]]
		votes[mep.id] <- vote.value #TODO pb du au fait que la numérotation n'est pas contigue
	}
	
	for.count <- length(votes==VOTE.FOR)
	against.count <- length(votes==VOTE.AGST)
	if(against.count>for.count)
		details[COL.RESULT] <- VOTE.FOR
	else
		details[COL.RESULT] <- VOTE.AGST
	
	result <- list(details=details, votes=votes)	
	return(result)
}



#############################################################################################
#############################################################################################
iyp.extract.votes <- function(doc.domains, mep.details)
{	cat("Extract vote-related data\n",sep="")
	dir.create(OVERALL.FOLDER, recursive=TRUE, showWarnings=FALSE)
	
	result <- list()
	
	# check if the files already exist, load everything
	if(all(file.exists(c(ALL.VOTES.FILE,DOC.DETAILS.FILE,GROUP.LINES.FILE,MEP.BEHAVIOR.FILE))))
	{	# vote values
		temp <- as.matrix(read.csv2(ALL.VOTES.FILE,check.names=FALSE))
		temp[,COL.MEPID] <- as.integer(temp[,COL.MEPID])
		result$all.votes <- temp
		# document details
		temp <- as.matrix(read.csv2(DOC.DETAILS.FILE,check.names=FALSE))
		temp[,COL.DOCID] <- as.integer(temp[,COL.DOCID])
		result$doc.details <- temp
		# group lines
		temp <- as.matrix(read.csv2(GROUP.LINES.FILE,check.names=FALSE))
		temp[,COL.MEPID] <- as.integer(temp[,COL.MEPID])
		result$group.lines <- temp
		# MEP behavior
		temp <- as.matrix(read.csv2(MEP.BEHAVIOR.FILE,check.names=FALSE))
		temp[,COL.MEPID] <- as.integer(temp[,COL.MEPID])
		result$mep.behavior <- temp
	}
	
	# otherwise, process everything
	else
	{	# build details matrix
		details.cols <- c(COL.DOCID, IYP.ELT.VOTEID,
			COL.TITLE, COL.FULL.TITLE, COL.DOMID, COL.DOC.REF,
			COL.EP.REF, COL.REPORTER.ID, COL.DATE)
		details.mat <- matrix(NA,nrow=nrow(doc.domains),ncol=length(details.cols))
		colnames(details.mat) <- details.cols
		
		# build vote values matrix
		votes.mat <- matrix(NA,nrow=nrow(mep.details),ncol=nrow(doc.domains))
		colnames(details.mat) <- 1:nrow(doc.domains)
		
		# fill both matrices
		for(i in 1:nrow(doc.domains))
		{	temp <- iyp.extract.vote(doc.domains[i,IYP.ELT.VOTEID])
			temp$details
			temp$votes
			data[COL.MEPID] <- i
			result[i,cols] <- data[cols]
		}
		
	
	
	
	
	
	
	
	
	
	
		# record matrices
		write.csv2(result,file=IYP.MEPS.FILE,row.names=FALSE)
	}
	
	return(result)
}

# TODO
# - charger chaque vote et constituer les tables all-votes et doc-details
# - calculer la ligne de groupe et constituer la table group-lines
# - calculer la loyauté et constituer la table mep-behavior
# (confronter aux fonctions communes dispo)

#TODO faut refaire toutes les numérotations (car ça ne se suit pas dans IYP)
#TODO revoir les MEPs retéléchargés, corriger les prénoms

load.itsyourparliament.data <- function()
{	dir.create(OVERALL.FOLDER, recursive=TRUE, showWarnings=FALSE)
	
	result <- list()
	
	doc.domains <- iyp.extract.domains()
	result$mep.details <- iyp.extract.meps.details()
#	result$doc.details <- vw.clean.doc.details()
#	result$all.votes <- vw.concatenate.votes(mep.details)
#	result$group.lines <- extract.group.lines(all.votes, mep.details)
#	result$behavior.values <- process.behavior.values(all.votes, mep.details, group.lines)
	
	return(result)
}
