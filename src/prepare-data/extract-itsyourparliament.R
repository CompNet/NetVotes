#############################################################################################
# Extracts raw data from the itsyourparliament Website.
# 
# 11/2015 Vincent Labatut
#############################################################################################
source("src/define-constants.R")
source("src/prepare-data/load-itsyourparliament.R")



#############################################################################################
# Constants used when downloading the IYP data from the website.
#############################################################################################
# list of the effective MEP ids
IYP.MEP.IDS		<- 1:870#[-c(325,587,627,724,739,746,758,759,760,793,794,795)]
# list of the effective vote ids
IYP.VOTE.IDS	<- 1:7513
# list of the effective domain ids
IYP.DOMAIN.IDS	<- 26:58#[-c(32,45,49,50,52)]
# URLs
	# MEP URL
	IYP.URL.MEP		<- "http://itsyourparliament.eu/api/mep.php"
	# Vote URL
	IYP.URL.VOTE	<- "http://www.itsyourparliament.eu/api/vote.php"
	# List of domains URL
	IYP.URL.DOMAINS	<- "http://itsyourparliament.eu/api/policyareas.php"
	# Domain URL
	IYP.URL.DOMAIN	<- "http://itsyourparliament.eu/api/policyarea.php"
	# ID parameter
	IYP.URL.ID		<- "?id="


#############################################################################################
# Retrieves the list of all MEPs from the website from www.itsyourparliament.eu, and record
# them as XML files for later use.
#############################################################################################
iyp.download.meps <- function()
{	for(i in 1:length(IYP.MEP.IDS))
	{	mep.id <- IYP.MEP.IDS[i]
		cat("Retrieving XML file for MEP id ",mep.id," (",i,"/",length(IYP.MEP.IDS),")\n",sep="")
		url <- paste(IYP.URL.MEP,IYP.URL.ID,mep.id,sep="")
		page <- readLines(url)
		if(length(page)==2 && page[1]=="<br>" && substr(page[2],1,9)=="Warning: ")
			cat("WARNING: the page for MEP id ",mep.id," was empty (",url,")\n",sep="")
		else
		{	file <- paste(IYP.MEPS.FOLDER,"/",mep.id,".xml",sep="")
			writeLines(page,file)
		}
	}
}



#############################################################################################
# Retrieves the list of all votes from the website from www.itsyourparliament.eu, and record
# them as XML files for later use.
#############################################################################################
iyp.download.votes <- function()
{	for(i in 1:length(IYP.VOTE.IDS))
	{	vote.id <- IYP.VOTE.IDS[i]
		cat("Retrieving XML file for vote id ",vote.id," (",i,"/",length(IYP.VOTE.IDS),")\n",sep="")
		url <- paste(IYP.URL.VOTE,IYP.URL.ID,vote.id,sep="")
		page <- readLines(url)
		#if(length(page)==1 && page=="ERROR: invalid arguments")
		if(length(page)==2 && page[1]=="<br>" && substr(page[2],1,9)=="Warning: ")
			cat("WARNING: the page for vote id ",vote.id," was empty (",url,")\n",sep="")
		else
		{	file <- paste(IYP.VOTES.FOLDER,"/",vote.id,".xml",sep="")
			writeLines(page,file)
		}
	}
}



#############################################################################################
# Retrieves the list of all policy domains from the website from www.itsyourparliament.eu, and record
# them as XML files for later use.
#############################################################################################
iyp.download.domains <- function()
{	# get the list of domains
	cat("Retrieving XML file for domain list\n",sep="")
	page <- readLines(IYP.URL.DOMAINS)
	writeLines(page,IYP.DOMAIN.LIST.FILE)
	
	# get the list of votes for each domain
	for(i in 1:length(IYP.DOMAIN.IDS))
	{	dom.id <- IYP.DOMAIN.IDS[i]
		cat("Retrieving XML file for domain id ",dom.id," (",i,"/",length(IYP.DOMAIN.IDS),")\n",sep="")
		url <- paste(IYP.URL.DOMAIN,IYP.URL.ID,dom.id,sep="")
		page <- readLines(url)
		if(length(page)==1 && page=="<b>No votes found</b>")
			cat("WARNING: the page for domain id ",dom.id," contains no vote (",url,") >> domain ignored\n",sep="")
		else
		{	file <- paste(IYP.DOMAINS.FOLDER,"/",dom.id,".xml",sep="")
			writeLines(page,file)
		}
	}
}



#############################################################################################
# Retrieves all the data from the www.itsyourparliament.eu website, and record them as XML 
# files for later use.
#############################################################################################
iyp.download.all <- function()
{	iyp.download.meps()
	iyp.download.votes()
	iyp.download.domains()
}
