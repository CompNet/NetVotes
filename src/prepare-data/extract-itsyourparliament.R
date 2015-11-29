#############################################################################################
# Extracts raw data from the itsyourparliament Website.
# 
# 11/2015 Vincent Labatut
#############################################################################################



#############################################################################################
# Constants used when downloading the IYP data from the website.
#############################################################################################
# id of the last MEP
IYP.MAX.MEP.ID	<- 870
# id of the last vote
IYP.MAX.VOTE.ID	<- 7513
# list of the ids for the domains possessing some votes
IYP.DOMAIN.IDS	<- 26:58[-c(32,45,49,50,52)]
# URLs
	# MEP URL
	IYP.URL.MEP		<- "http://itsyourparliament.eu/api/mep.php"
	# Vote URL
	IYP.URL.VOTE	<- "http://www.itsyourparliament.eu/api/vote.php"
	# Domain URL
	IYP.URL.DOMAIN	<- "http://itsyourparliament.eu/api/policyareas.php"
	# ID parameter
	IYP.URL.ID		<- "?id="


#############################################################################################
# Retrieves the list of all MEPs from the website from www.itsyourparliament.eu, and record
# them as XML files for later use.
#############################################################################################
iyp.download.meps <- function()
{	for(mep.id in 1:IYP.MAX.MEP.ID)
	{	cat("Retrieving XML file for MEP id ",mep.id," (",mep.id,"/",IYP.MAX.MEP.ID,")\n",sep="")
		url <- paste(IYP.URL.MEP,IYP.URL.ID,mep.id,sep="")
		page <- readLines(url)
		file <- paste(IYP.MEP.INFO.FOLDER,"/",mep.id,".xml",sep="")
		writeLines(page,file)
	}
}



#############################################################################################
# Retrieves the list of all votes from the website from www.itsyourparliament.eu, and record
# them as XML files for later use.
#############################################################################################
iyp.download.votes <- function()
{	for(vote.id in 1:IYP.MAX.VOTE.ID)
	{	cat("Retrieving XML file for vote id ",vote.id," (",vote.id,"/",IYP.MAX.VOTE.ID,")\n",sep="")
		url <- paste(IYP.URL.VOTE,IYP.URL.ID,vote.id,sep="")
		page <- readLines(url)
		file <- paste(IYP.VOTES.FOLDER,"/",vote.id,".xml",sep="")
		writeLines(page,file)
	}
}



#############################################################################################
# Retrieves the list of all policy domains from the website from www.itsyourparliament.eu, and record
# them as XML files for later use.
#############################################################################################
iyp.download.domains <- function()
{	# get the list of domains
	cat("Retrieving XML file for domain list\n",sep="")
	page <- readLines(IYP.URL.DOMAIN)
	writeLines(page,IYP.DOMAINS.LIST.FILE)
	
	# get the list of votes for each domain
	for(d in 1:length(IYP.DOMAIN.IDS))
	{	dom.id <- IYP.DOMAIN.IDS[d]
		cat("Retrieving XML file for domain id ",dom.id," (",d,"/",length(IYP.DOMAIN.IDS),")\n",sep="")
		url <- paste(IYP.URL.DOMAIN,IYP.URL.ID,dom.id,sep="")
		page <- readLines(url)
		file <- paste(IYP.DOMAINS.FOLDER,"/",dom.id,".xml",sep="")
		writeLines(page,file)
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
