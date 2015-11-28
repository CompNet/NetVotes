#############################################################################################
# Extracts raw data from the itsyourparliament Website.
# 
# 11/2015 Vincent Labatut
#############################################################################################
library("XML","stringr")

source("src/define-constants.R")



#############################################################################################
# Folder names
#############################################################################################
# "It's your parliament" data
IYP.FOLDER <- file.path(IN.FOLDER,"itsyourparliament")
	# raw data (i.e. tables)
	IYP.RAW.FOLDER <- file.path(IYP.FOLDER,"raw")


#############################################################################################
# File names
#############################################################################################
IYP.MEPS.FILE	<- file.path(IYP.FOLDER,"mep-details.csv")

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


iyp.extract.mep <- function(mep.id)
{	result <- c()
	
	url <- paste("http://itsyourparliament.eu/api/mep.php?id=",mep.id,sep="")
	page <- readLines(url)
	xml.data <- xmlParse(page)
	xml <- xmlToList(xml.data)
	
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

#iyp.extract.mep(mep.id=136)
#iyp.extract.meps(mep.ids=136:138)
	

# TODO fonction qui récup tous les votes et en profite pour lister les MEP ids nécessaires (et toute autre info)
