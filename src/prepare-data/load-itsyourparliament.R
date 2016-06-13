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
# Activity periods associated to the MEPs
IYP.MEP.PERIODS.FILE <- file.path(IYP.MEPS.FOLDER,"_mep-periods.csv")


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
DOMAIN.IYP2SYMB["Constitutional Affairs"] <- DOMAIN.AFCO
DOMAIN.IYP2SYMB["Foreign Affairs"] <- DOMAIN.AFET
DOMAIN.IYP2SYMB["Agriculture and Rural Development"] <- DOMAIN.AGRI
DOMAIN.IYP2SYMB["Budgets"] <- DOMAIN.BUDG
DOMAIN.IYP2SYMB["Budgetary Control"] <- DOMAIN.CONT
DOMAIN.IYP2SYMB["Culture and Education"] <- DOMAIN.CULT
DOMAIN.IYP2SYMB["Development"] <- DOMAIN.DEVE
DOMAIN.IYP2SYMB["Women\\'s Rights and Gender Equality"] <- DOMAIN.FEMM
DOMAIN.IYP2SYMB["Women’s Rights and Gender Equality"] <- DOMAIN.FEMM
DOMAIN.IYP2SYMB["Women's Rights and Gender Equality"] <- DOMAIN.FEMM
DOMAIN.IYP2SYMB["Economic and Monetary Affairs"] <- DOMAIN.ECON
DOMAIN.IYP2SYMB["Employment and Social Affairs"] <- DOMAIN.EMPL
DOMAIN.IYP2SYMB["Environment, Public Health and Food Safety"] <- DOMAIN.ENVI
DOMAIN.IYP2SYMB["Internal Market and Consumer Protection"] <- DOMAIN.IMCO
DOMAIN.IYP2SYMB["International Trade"] <- DOMAIN.INTA
DOMAIN.IYP2SYMB["Industry, Research and Energy"] <- DOMAIN.ITRE
DOMAIN.IYP2SYMB["Legal Affairs"] <- DOMAIN.JURI
DOMAIN.IYP2SYMB["Civil Liberties, Justice and Home Affairs"] <- DOMAIN.LIBE
DOMAIN.IYP2SYMB["Fisheries"] <- DOMAIN.PECH
DOMAIN.IYP2SYMB["Petitions"] <- DOMAIN.PETI
DOMAIN.IYP2SYMB["Regional Development"] <- DOMAIN.REGI
DOMAIN.IYP2SYMB["Internal regulations of the EP"] <- DOMAIN.RIPE
DOMAIN.IYP2SYMB["Transport and Tourism"] <- DOMAIN.TRAN


#############################################################################################
# Vote mapping
#############################################################################################
# note: no "Didn't vote", "Absent" or "Documented Absence" like with VoteWatch
VOTE.IYP2SYMB <- c()
VOTE.IYP2SYMB["For"] <- VOTE.FOR
VOTE.IYP2SYMB["Abstention"] <- VOTE.ABST
VOTE.IYP2SYMB["Against"] <- VOTE.AGST


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
# Checks if the specified date belongs to one of the periods represented in the specified
# string. The format of this string is xx/xx/xxxx:xx/xx/xxxx::yy/yy/yyyy:yy/yy/yyyy. A NA for
# the end date represents no limit.
#
# periods: string representing a list of temporal period, using the above format.
# date: date of interest (a date object, not a string)
#
# returns: TRUE iff the date belongs to at list one period.
#############################################################################################
iyp.check.date <- function(periods, date)
{	# possible convert the date parameter to an actual date object
	if(is.character(date))
		date <- as.Date(date,"%d/%m/%Y")
	#print(date)
	
	# init variables
	result <- FALSE
	rest <- gsub("NA", "00/00/0000", periods)
	
	# process each period
	while(nchar(rest)>0 && !result)
	{	# get the period
		period <- substr(rest,1,21)
		if(nchar(rest)>=23)
			rest <- substr(rest,24,nchar(rest))
		else
			rest <- ""
		#tlog("Period: ",period," rest=",rest)
		
		# get the dates
		start.date.str <- substr(period,1,10)
		end.date.str <- substr(period,12,21)
		#print(start.date.str);print(end.date.str)
		start.date <- as.Date(start.date.str,"%d/%m/%Y")
		if(end.date.str=="00/00/0000")
			result <- date>=start.date
		else
		{	end.date <- as.Date(end.date.str,"%d/%m/%Y")
			# check if the considered date belongs to the period
			result <- date>=start.date && date<=end.date
		}
	}
	
	return(result)
}



#############################################################################################
# Read the XML file corresponding to the specified MEP id, and returns the corresponding
# vector of strings.
#
# mep.id: ID of the MEP (in IYP).
#
# returns: string vector representing the MEP details.
#############################################################################################
iyp.extract.mep.details <- function(mep.id)
{	tlog("....Processing MEP ",mep.id)
	result <- c()
	
	# retrieve XML document
	file <- file.path(IYP.MEPS.FOLDER,paste(mep.id,".xml",sep=""))
	doc <- readLines(file)
	xml.data <- xmlParse(doc)
	xml <- xmlToList(xml.data)
	
	# MEP id in NetVotes
	result[IYP.ELT.MEPID] <- mep.id
	
	# names
	fullname <- str_trim(xml[[IYP.ELT.FULLNAME]])
	lastname <- str_trim(xml[[IYP.ELT.MEPNAME]])
#	idx <- str_locate(fullname,fixed(lastname,ignore_case=TRUE))[1]-2
	idx <- str_locate(fullname,ignore.case(lastname))[1]-2
	firstname <- substr(fullname,1,idx)
	if(!is.na(lastname) & lastname=="")
		lastname <- NA
	result[COL.LASTNAME] <- lastname
	if(!is.na(firstname) & firstname=="")
		firstname <- NA
	result[COL.FIRSTNAME] <- firstname
	if(!is.na(fullname) & fullname=="")
		fullname <- NA
	result[COL.FULLNAME] <- fullname
	
	# state
	state <- str_trim(xml[[IYP.ELT.COUNTRY]])
	if(!is.na(state) & state=="")
		state <- NA
	result[COL.STATE] <- state
	
	# european political group
	group <- GROUP.IYP2SYMB[str_trim(xml[[IYP.ELT.GROUP]])]
	if(!is.na(group) & group=="")
		group <- NA
	result[COL.GROUP] <- group
	
	# MEP title
	title <- str_trim(xml[[IYP.ELT.TITLE]])
	if(!is.na(title) & title=="")
		title <- NA
	result[COL.TITLE] <- title
	
	# european party
	party <- str_trim(xml[[IYP.ELT.PARTY]])
	if(!is.na(party) & party=="")
		party <- NA
	result[COL.PARTY] <- party
	
	# MEP date of birth
	birthdate <- str_trim(xml[[IYP.ELT.BIRTHDATE]])
	if(!is.na(birthdate) & birthdate=="")
		birthdate <- NA
	result[COL.BIRTHDATE] <- birthdate
	
	# MEP place of birth
	birthplace <- str_trim(xml[[IYP.ELT.BIRTHPLACE]])
	if(!is.na(birthplace) & birthplace=="")
		birthplace <- NA
	result[COL.BIRTHPLACE] <- birthplace
	
	# official MEP ID in the european parliament
	ep.id <- str_trim(xml[[IYP.ELT.EP_ID]])
	if(!is.na(ep.id) & ep.id=="")
		ep.id <- NA
	result[COL.EP.ID] <- ep.id
	
	return(result)
}



#############################################################################################
# Reads the XML files corresponding to all the MEP ids, and returns the corresponding
# string table. Also adds some information retrieved from Europarl, the official European
# Parliament website.
#
# returns: string array representing the MEP details.
#############################################################################################
iyp.extract.meps.details <- function()
{	tlog("..Retrieving the MEPs details")
	dir.create(OVERALL.FOLDER, recursive=TRUE, showWarnings=FALSE)
	
	# if the file already exists, just load it
	if(file.exists(MEP.DETAILS.FILE))
	{	result <- as.matrix(read.csv2(MEP.DETAILS.FILE,check.names=FALSE))
		result[,COL.MEPID] <- as.integer(result[,COL.MEPID])
		result[,IYP.ELT.MEPID] <- as.integer(result[,IYP.ELT.MEPID])
		result[,COL.EP.ID] <- as.integer(result[,COL.EP.ID])
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
			COL.PARTY, COL.BIRTHDATE, COL.BIRTHPLACE, COL.EP.ID,
			IYP.ELT.MEPID, COL.PERIODS)
		result <- matrix(NA,nrow=length(mep.ids),ncol=length(cols))
		colnames(result) <- cols
		for(i in 1:length(mep.ids))
		{	data <- iyp.extract.mep.details(mep.ids[i])
			data[COL.MEPID] <- i
			result[i,cols] <- data[cols]
		}
		
		# retrieve the official list of MEPs activity periods
		ep.table <- as.matrix(read.csv2(IYP.MEP.PERIODS.FILE,check.names=FALSE))
		ep.table[,COL.EP.ID] <- as.integer(ep.table[,COL.EP.ID])
		Encoding(ep.table[,COL.FULLNAME]) <- "UTF-8"
		ep.table[,COL.FULLNAME] <- toupper(ep.table[,COL.FULLNAME])
		# for each MEP in the IYP table, add the official info from the second table
		fullnames <- toupper(paste(result[,COL.FIRSTNAME],result[,COL.LASTNAME]))
		idx  <- match(fullnames,ep.table[,COL.FULLNAME])
		#print(cbind(result[,COL.EP.ID],ep.table[idx,COL.EP.ID],result[,COL.EP.ID]==ep.table[idx,COL.EP.ID]))
		result[,COL.EP.ID] <- ep.table[idx,COL.EP.ID]
		result[,COL.PERIODS] <- ep.table[idx,COL.PERIODS]
		
		# record matrix
		write.csv2(result,file=MEP.DETAILS.FILE,row.names=FALSE)
	}
	
	return(result)
}



#############################################################################################
# Read the XML file corresponding to the specified domain id, and returns the corresponding
# vector of vote ids.
#
# domain.id: ID of the domain (in IYP).
#
# returns: vector of vote ids.
#############################################################################################
iyp.extract.domain <- function(domain.id)
{	tlog("....Processing domain ",domain.id)
	
	# retrieve XML document
	file <- file.path(IYP.DOMAINS.FOLDER,paste(domain.id,".xml",sep=""))
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
# Read the XML file listing the domains, then process each domain, and returns a vector representing
# the policy domain associated to each voted document.
#
# returns: a vector associating a vote id to the corresponding policy domain.
#############################################################################################
iyp.extract.domains <- function()
{	tlog("..Retrieving the domain details")
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
				domain.code <- DOMAIN.IYP2SYMB[domains[i,IYP.ELT.POLICY.NAME]]
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
#
# returns: a list containing the vote information (details) and the MEP vote values (votes).
#############################################################################################
iyp.extract.vote <- function(vote.id)
{	# retrieve XML document
	file <- file.path(IYP.VOTES.FOLDER,paste(vote.id,".xml",sep=""))
	doc <- readLines(file)
	xml.data <- xmlParse(doc)
	xml <- xmlToList(xml.data)
	
	# extract vote information
	details <- c()
	details[IYP.ELT.VOTEID] <- vote.id
	
	# document title
	title <- str_trim(xml[[IYP.ELT.VOTE.TITLE]])
	if(!is.na(title) & title=="")
		title <- NA
	details[COL.TITLE] <- title
	
	# different title (not always present)
	full.title <- str_trim(xml[[IYP.ELT.FULL.TITLE]])
	if(!is.na(full.title) & full.title=="")
		full.title <- NA
	details[COL.FULL.TITLE] <- full.title
	
	# id of the policy domain
	dom.id <- str_trim(xml[[IYP.ELT.POLICY.AREA]])
#	if(!is.na(dom.id) & dom.id=="")
#		dom.id <- DOMAIN.AUTR
#	else
		dom.id <- DOMAIN.IYP2SYMB[dom.id]
	details[COL.DOMID] <- dom.id
tlog("'",dom.id,"' >> ",dom.id)
	
	# document reference
	doc.ref <- str_trim(xml[[IYP.ELT.DOC.REF]])
	if(!is.na(doc.ref) & doc.ref=="")
		doc.ref <- NA
	details[COL.DOC.REF] <- doc.ref
	
	# european parliament official reference
	ep.ref <- str_trim(xml[[IYP.ELT.EP.REF]])
	if(!is.na(ep.ref) & ep.ref=="")
		ep.ref <- NA
	details[COL.EP.REF] <- ep.ref
	
	# id of the reporter of the document
	reporter.id <- str_trim(xml[[IYP.ELT.REPORTER.ID]])
	if(!is.na(reporter.id) & reporter.id=="")
		reporter.id <- NA
	details[COL.REPORTER.ID] <- reporter.id
	
	# date of the vote
	date <- str_trim(xml[[IYP.ELT.VOTE.DATE]])	
	if(!is.na(date) & date=="")
		date <- NA
	details[COL.DATE] <- format(as.Date(date,"%Y-%m-%d"),"%d/%m/%Y")
	
	# extract vote values
	votes <- c()
	for(i in 1:length(xml[[IYP.ELT.VOTES]]))
	{	v <- xml[[IYP.ELT.VOTES]][[i]]
		mep.id <- str_trim(v[[IYP.ELT.MEPID]])
		vote.value <- VOTE.IYP2SYMB[str_trim(v[[IYP.ELT.MEP.VOTE]])]
		votes[as.character(mep.id)] <- vote.value
	}
	
	# process vote total result
	for.count <- length(votes==VOTE.FOR)
	against.count <- length(votes==VOTE.AGST)
	if(against.count>for.count)
		details[COL.RESULT] <- VOTE.FOR
	else
		details[COL.RESULT] <- VOTE.AGST
	
	result <- list(details=details, votes=votes)
#print(result)	
	return(result)
}



#############################################################################################
# Read the XML files corresponding to all the vote ids, and returns the corresponding
# vote data, as two tables: document details and vote values.
#
# returns: a list containing the document details (doc.details) annd the vote values (all.votes).
#############################################################################################
iyp.extract.votes <- function(doc.domains, mep.details)
{	tlog("..Extract vote-related data")
	dir.create(OVERALL.FOLDER, recursive=TRUE, showWarnings=FALSE)
	result <- list()
	
	# check if the files already exist, load everything
	if(file.exists(ALL.VOTES.FILE) & file.exists(DOC.DETAILS.FILE))
	{	# vote values
		temp <- as.matrix(read.csv2(ALL.VOTES.FILE,check.names=FALSE))
		temp[,COL.MEPID] <- as.integer(temp[,COL.MEPID])
		result$all.votes <- temp
		# document details
		temp <- as.matrix(read.csv2(DOC.DETAILS.FILE,check.names=FALSE))
		temp[,COL.DOCID] <- as.integer(temp[,COL.DOCID])
		result$doc.details <- temp
	}
	
	# otherwise, process everything
	else
	{	# retrieve the list of vote ids
		files <- list.files(path=IYP.VOTES.FOLDER, full.names=FALSE, no..=TRUE)
		vote.ids <- c()
		for(file in files)
			vote.ids <- c(vote.ids,substr(file,1,str_locate(file,".xml")-1))
		vote.ids <- sort(as.integer(vote.ids))
		
		# complete the list of document domains (some docs were missing)
		doc.domains0 <- doc.domains
		doc.domains <- cbind(vote.ids,DOMAIN.AUTR)
		colnames(doc.domains) <- c(IYP.ELT.VOTEID, COL.DOMID)
		idx <- match(doc.domains0[,IYP.ELT.VOTEID],vote.ids)
		doc.domains[idx,] <- doc.domains0
		print(doc.domains)
		
		# build details matrix
		details.cols <- c(COL.DOCID, IYP.ELT.VOTEID,
			COL.TITLE, COL.FULL.TITLE, COL.DOMID, COL.DOC.REF,
			COL.EP.REF, COL.REPORTER.ID, COL.DATE)
		details.mat <- matrix(NA,nrow=length(vote.ids),ncol=length(details.cols))
		colnames(details.mat) <- details.cols
		
		# build vote values matrix
		votes.mat <- matrix(NA,nrow=nrow(mep.details),ncol=length(vote.ids))
		colnames(votes.mat) <- 1:length(vote.ids)
		
		# fill both matrices
#vote.ids <- vote.ids[vote.ids>=7065]		
		for(i in 1:length(vote.ids))
		{	tlog("....Processing vote ",vote.ids[i]," (",i,"/",length(vote.ids),")")
			
			temp <- iyp.extract.vote(vote.ids[i])
			# update details matrix
			temp$details[COL.DOCID] <- i
#			print(temp$details)			
#			print(temp$details[COL.DOMID])
#			print(doc.domains[i,COL.DOMID])
			#if(is.na(temp$details[COL.DOMID]))
			#	temp$details[COL.DOMID] <- doc.domains[i,COL.DOMID]
			#else if(temp$details[COL.DOMID]!=doc.domains[i,COL.DOMID])
			#	tlog("WARNING: domain is different in vote (",temp$details[COL.DOMID],") and domain (",doc.domains[i,COL.DOMID],") files")
			if(!is.na(doc.domains[i,COL.DOMID]))
			{	if(is.na(temp$details[COL.DOMID]))
					temp$details[COL.DOMID] <- doc.domains[i,COL.DOMID]
				else if(temp$details[COL.DOMID]!=doc.domains[i,COL.DOMID])
					tlog("....WARNING: domain is different in vote (",temp$details[COL.DOMID],") and domain (",doc.domains[i,COL.DOMID],") files")
			}
			else if(is.na(temp$details[COL.DOMID]))
				tlog("....WARNING: both domains in vote and domain table are missing")
			details.mat[i,details.cols] <- temp$details[details.cols]
			
			# update vote matrix
			idx <- match(as.integer(names(temp$votes)), mep.details[,IYP.ELT.MEPID])
			votes.mat[idx,i] <- temp$votes
			# differentiate absent MEPs and inactive ones (i.e. persons not holding a MEP position at the time of the vote)
			vote.date <- temp$details[COL.DATE]
			active <- sapply(mep.details[,COL.PERIODS],function(periods) iyp.check.date(periods,vote.date))
			nas <- is.na(votes.mat[,i])
			votes.mat[active & nas,i] <- VOTE.ABSENT # we consider active MEP who didn't vote as absent
		}

		# record details matrix
		write.csv2(details.mat,file=DOC.DETAILS.FILE,row.names=FALSE)
		result$doc.details <- details.mat
		
		# record vote values
		votes.mat <- cbind(1:nrow(mep.details),votes.mat)
		colnames(votes.mat)[1] <- COL.MEPID
		write.csv2(votes.mat,file=ALL.VOTES.FILE,row.names=FALSE)
		result$all.votes <- votes.mat
	}
	
	#print(sort(unique(c(result$all.votes))))	
	return(result)
}



#############################################################################################
# Load all the tables and returns them as a list.
#
# returns: a list containing all the loaded tables.
#############################################################################################
load.itsyourparliament.data <- function()
{	tlog("***************************************************")
	tlog("****** LOAD IYP DATA")
	tlog("***************************************************")
	
	dir.create(OVERALL.FOLDER, recursive=TRUE, showWarnings=FALSE)
	
	result <- list()
	
	doc.domains <- iyp.extract.domains()
	result$mep.details <- iyp.extract.meps.details()
	temp <- iyp.extract.votes(doc.domains, result$mep.details)
	result$doc.details <- temp$doc.details
	result$all.votes <- temp$all.votes
	result$group.lines <- extract.group.lines(result$all.votes, result$mep.details)
	result$behavior.values <- process.behavior.values(result$all.votes, result$mep.details, result$group.lines)
	
	return(result)
}
