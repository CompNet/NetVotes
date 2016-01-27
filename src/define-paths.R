#############################################################################################
# These functions build file paths and names based on a series of parameters. They allow a 
# uniform access to the resources created by the rest of the scripts.
# 
# 01/2016 Vincent Labatut
#############################################################################################


#############################################################################################
# Builds a path for a file located in the "votes" folder.
#
# country: considered member state (optional).
# group: considered political group (optional).
# domain: considered domain of activity (compulsory).
# period: considered time period (optional).
# additional: additional folder (last position).
# returns: the appropriate path for a files in the "votes" folder.
#############################################################################################
get.votes.path(country=NA, group=NA, domain, period=NA, additional=NA)
{	result <- VOTES.FOLDER
	
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
		result <- file.path(result,period)
	
	# additional level
	if(!is.na(additional))
		result <- file.path(result,additional)
	
	return(result)
}

