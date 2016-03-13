#############################################################################################
# These functions build file paths and names based on a series of parameters. They allow a 
# uniform access to the resources created by the rest of the scripts.
# 
# 01/2016 Vincent Labatut
#############################################################################################


#############################################################################################
# Builds a path for a file located in the "votes" or "behavior" folders.
#
# vote: whether to consider the "votes" of "behavior" folder.
# country: considered member state (optional).
# group: considered political group (optional).
# domain: considered domain of activity (compulsory).
# period: considered time period.
# returns: the appropriate path for a files in the "votes" or "behavior" folders.
#############################################################################################
get.votes.path <- function(vote, country=NA, group=NA, domain, period)
{	# main folder
	if(vote)
		result <- VOTES.FOLDER
	else
		result <- BEHAVIOR.FOLDER
	
	# country, group or everything (mutuall exclusive)
	if(!is.na(country))
		result <- file.path(result,"bycountry",country)
	else if(!is.na(group))
		result <- file.path(result,"bygroup",group)
	else
		result <- file.path(result,"everything")
	
	# domain (should not be NA)
#	if(!(is.na(domain)))
		result <- file.path(result,domain)
	
	# time period (should not be NA)
#	if(!is.na(period))
		result <- file.path(result,DATE.STR.T7[period])
	
	return(result)
}



#############################################################################################
# Builds a path for a file located in the "agreement" folder.
#
# score: name of the score table used to process the agreement index.
# country: considered member state (optional).
# group: considered political group (optional).
# domain: considered domain of activity (compulsory).
# returns: the appropriate path for a files in the "votes" folder.
#############################################################################################
get.agreement.path <- function(score, country=NA, group=NA, domain)
{	result <- AGREEMENT.FOLDER
	
	# score table (should not be NA)
#	if(!is.na(score))
	result <- file.path(result,score)
	
	# country, group or everything (mutuall exclusive)
	if(!is.na(country))
		result <- file.path(result,"bycountry",country)
	else if(!is.na(group))
		result <- file.path(result,"bygroup",group)
	else
		result <- file.path(result,"everything")
	
	# domain (should not be NA)
	if(!(is.na(domain)))
		result <- file.path(result,domain)
	
	return(result)
}

#############################################################################################
# Builds a path for a file located in the "networks" folder.
#
# score: name of the score table used to process the agreement index.
# thresh: thresholds used for network extraction (vector of two values).
# country: considered member state (optional).
# group: considered political group (optional).
# domain: considered domain of activity (compulsory).
# period: considered time period.
# returns: the appropriate path for a files in the "networks" folder.
#############################################################################################
get.networks.path <- function(score, thresh=NA, country=NA, group=NA, domain, period)
{	result <- NETWORKS.FOLDER
	
	# score table (should not be NA)
#	if(!is.na(score))
	result <- file.path(result,score)
	
	# positive and negative thresholds (can be NA if no threhsold)
	result <- file.path(result,paste("negtr=",thresh[1],"_postr=",thresh[2],sep=""))
	
	# country, group or everything (mutuall exclusive)
	if(!is.na(country))
		result <- file.path(result,"bycountry",country)
	else if(!is.na(group))
		result <- file.path(result,"bygroup",group)
	else
		result <- file.path(result,"everything")
	
	# domain (should not be NA)
	if(!(is.na(domain)))
		result <- file.path(result,domain)
	
	# time period (should not be NA)
#	if(!is.na(period))
		result <- file.path(result,DATE.STR.T7[period])
	
	return(result)
}

#############################################################################################
# Builds a path for a file located in the "partitions" folder.
#
# score: name of the score table used to process the agreement index (compulsory).
# thresh: thresholds used for network extraction (vector of two values).
# country: considered member state (optional).
# group: considered political group (optional).
# domain: considered domain of activity (compulsory).
# period: considered time period (optional).
# repetition: repetition number for the partitioning algorithm (NA for no repetition at all).
# returns: the appropriate path for a files in the "networks" folder.
#############################################################################################
get.partitions.path <- function(score, thresh=NA, country=NA, group=NA, domain, period=NA, repetition=NA)
{	result <- PARTITIONS.FOLDER
	
	# score table (should not be NA)
#	if(!is.na(score))
	result <- file.path(result,score)
	
	# positive and negative thresholds (can be NA if no threhsold)
	result <- file.path(result,paste("negtr=",thresh[1],"_postr=",thresh[2],sep=""))
	
	# country, group or everything (mutuall exclusive)
	if(!is.na(country))
		result <- file.path(result,"bycountry",country)
	else if(!is.na(group))
		result <- file.path(result,"bygroup",group)
	else
		result <- file.path(result,"everything")
	
	# domain (should not be NA)
	if(!(is.na(domain)))
		result <- file.path(result,domain)
	
	# time period
	if(!is.na(period))
		result <- file.path(result,DATE.STR.T7[period])
	
	# repetition number (partitioning algos can be applied several times to the same data)
	if(!is.na(repetition))
		result <- file.path(result,repetition)
	
	return(result)
}
