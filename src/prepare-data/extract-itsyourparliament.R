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
	# offical europarl URL for reports
	IYP.URL.REPORTS	<- "http://www.europarl.europa.eu/sides/getDoc.do?type=REPORT&reference="
	# offical europarl URL for motions
	IYP.URL.MOTIONS	<- "http://www.europarl.europa.eu/sides/getDoc.do?type=MOTION&reference="
	# language suffix for the official europarl website
	IYP.URL.LANG.SUFFIX	<- "&language=EN"
# strings
#IYP.STRING.COMMITTEE <- '<p style=\"margin: 0pt; \">Committee on '


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
		{	file <- file.path(IYP.MEPS.FOLDER,paste(mep.id,".xml",sep=""))
			writeLines(page,file)
		}
	}
}



#############################################################################################
# Retrieves the policy domain of a document thanks to the Europarl website (official website
# of the European Parliament).
#
# title: IYP title of the document (which includes its Europarl ID).
# returns: the domain retrieved from the Europarl website, or NA if none could be found.
#############################################################################################
ep.retrieve.domain <- function(title)
{	domain <- NA
	
	# extract the document Europarl ID from the title
	cat("..title='",title,"'\n",sep="")
	prefix <- substr(title,1,2)
	cat("....prefix='",prefix,"'\n",sep="")
	# known Europarl ID
	if(prefix %in% c("A7","B7","RC"))
	{	# build the appropriate request URL for A7- or B7-type ids
		title <- gsub(" - ", "-", title)
		if(prefix=="A7" | prefix=="B7") # A7-0144/2014
		{	number <- substr(title,4,7)
			cat("....number='",number,"'\n",sep="")
			year <- substr(title,9,12)
			cat("....year='",year,"'\n",sep="")
			reference <- paste(prefix,"-",year,"-",number,sep="")
			cat("..reference='",reference,"'\n",sep="")
			if(prefix=="A7")
				ep.url <- paste(IYP.URL.REPORTS,reference,IYP.URL.LANG.SUFFIX,sep="")
			else if(prefix=="B7")
				ep.url <- paste(IYP.URL.MOTIONS,reference,IYP.URL.LANG.SUFFIX,sep="")
			cat("..url='",ep.url,"'\n",sep="")
		}
		# build the appropriate request URL for RC-type ids
		else #if(prefix=="RC") # RC-B7-0693/2011
		{	prefix2 <- substr(title,4,5)
			cat("....prefix2='",prefix2,"'\n",sep="")
			number <- substr(title,7,10)
			cat("....number='",number,"'\n",sep="")
			year <- substr(title,12,15)
			cat("....year='",year,"'\n",sep="")
			reference <- paste("P7","-",prefix,"-",year,"-",number,sep="")
			cat("..reference='",reference,"'\n",sep="")
			ep.url <- paste(IYP.URL.MOTIONS,reference,IYP.URL.LANG.SUFFIX,sep="")
			cat("..url='",ep.url,"'\n",sep="")
		}
		# request the Europarl server
		ep.page <- readLines(ep.url)
		# identify the associated parliamentary committee
		idx <- str_locate(ep.page, fixed("Committee on "))
		idx2 <- which(apply(idx,1,function(v) !all(is.na(v))))
		# a committee could be found
		if(length(idx2)>0)
		{	# extract domain from committee name
			com.line <- ep.page[idx2[1]]
			cat("..com.line='",com.line,"'\n",sep="")
			start.pos <- idx[idx2[1],2] + 1
			the <- substr(com.line, start=start.pos,stop=start.pos+nchar("the ")-1)
			cat("..the='",the,"'\n",sep="")
			if(the=="the ")
				start.pos <- start.pos + nchar("the ")
			tag.pos <- str_locate(substr(com.line,start=start.pos,stop=nchar(com.line)),"<")[1]
			if(!is.na(tag.pos))
				end.pos <- start.pos + tag.pos - 2
			else
				end.pos <- nchar(com.line)
			# init the result variable
			domain <- str_trim(substr(com.line,start=start.pos,stop=end.pos))
			cat("..domain='",domain,"'\n",sep="")
		}
		# no committee found
		else
			cat("..WARNING: could not find the committee name\n",sep="")
	}
	# unknown Europarl ID
	else
	{	cat("..WARNING: prefix not recognized, skipping this one\n",sep="")
	}
	
	return(domain)
}


#############################################################################################
# Retrieves the list of all votes from the website from www.itsyourparliament.eu, and record
# them as XML files for later use.
#############################################################################################
iyp.download.votes <- function()
{	for(i in 1:length(IYP.VOTE.IDS))
#i <- 6263
	{	vote.id <- IYP.VOTE.IDS[i]
		cat("Retrieving XML file for vote id ",vote.id," (",i,"/",length(IYP.VOTE.IDS),")\n",sep="")
		
		# load the page
		url <- paste(IYP.URL.VOTE,IYP.URL.ID,vote.id,sep="")
		cat("..url=",url,"\n",sep="")
		page <- readLines(url)
		
		#if(length(page)==1 && page=="ERROR: invalid arguments")
		if(length(page)==2 && page[1]=="<br>" && substr(page[2],1,9)=="Warning: ")
			cat("WARNING: the page for vote id ",vote.id," was empty (",url,")\n",sep="")
		else
		{	# clean the page (the presence of "&" prevents the later XML parsing
			page <- gsub(" & ", " &amp; ", page)
			
			# possibly look for the policy domain
			xml.data <- xmlParse(page)
			xml <- xmlToList(xml.data)
			domain <- str_trim(xml[[IYP.ELT.POLICY.AREA]])
			if(domain=="")
			{	cat("No policy domain, trying to get it from europal (domain='",domain,"')\n",sep="")
				# try to get the domain from the official website 
				title <- str_trim(xml[[IYP.ELT.VOTE.TITLE]])
				domain <- ep.retrieve.domain(title)
				# update the xml document
				if(!is.na(domain) && domain!="")
				{	idx <- str_locate(page, fixed(IYP.ELT.POLICY.AREA))
					idx2 <- which(apply(idx,1,function(v) !all(is.na(v))))
					page[idx2[1]] <- paste(page[idx2[1]], domain)
				}
			}
			
			# record the page
			file <- file.path(IYP.VOTES.FOLDER,paste(vote.id,".xml",sep=""))
			writeLines(page,file)
		}
	}
}



#############################################################################################
# Load the existing XML files reprenting votes, and possibly complete them by adding
# a policy domain when missing. This domaib is retrieved from Europarl, the official website
# of the European Parliament.
#############################################################################################
iyp.complete.votes <- function()
{	# retrieve the list of vote ids
	files <- list.files(path=IYP.VOTES.FOLDER, full.names=FALSE, no..=TRUE)
	vote.ids <- c()
	for(file in files)
		vote.ids <- c(vote.ids,substr(file,1,str_locate(file,".xml")-1))
	vote.ids <- sort(as.integer(vote.ids))
	
	for(i in 1:length(vote.ids))
	{	vote.id <- vote.ids[i]
		cat("Processing the XML file for vote id ",vote.id," (",i,"/",length(vote.ids),")\n",sep="")
		
		# load the page
		file <- file.path(IYP.VOTES.FOLDER,paste(vote.id,".xml",sep=""))
		cat("file=",file,"\n",sep="")
		page <- readLines(file)
		# parse the XML code
		xml.data <- xmlParse(page)
		xml <- xmlToList(xml.data)
		
		# possibly look for the policy domain
		changed <- FALSE
		domain <- str_trim(xml[[IYP.ELT.POLICY.AREA]])
		cat("domain=",domain,"\n",sep="")
		if(domain=="")
		{	cat("No policy domain, trying to get it from europal (domain='",domain,"')\n",sep="")
			# try to get the domain from the official website 
			title <- str_trim(xml[[IYP.ELT.VOTE.TITLE]])
			domain <- ep.retrieve.domain(title)
			# update the xml document
			if(!is.na(domain) && domain!="")
			{	idx <- str_locate(page, fixed(IYP.ELT.POLICY.AREA))
				idx2 <- which(apply(idx,1,function(v) !all(is.na(v))))
				page[idx2[1]] <- paste(page[idx2[1]], domain)
				changed <- TRUE
			}
		}
		
		# possibly re-record the page
		if(changed)
		{	cat("!!!!!!! Domain changed: updating the file\n\n")
			writeLines(page,file)
		}
		else
			cat("Nothing changed for this document\n\n")
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
		{	file <- file.path(IYP.DOMAINS.FOLDER,paste(dom.id,".xml",sep=""))
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
