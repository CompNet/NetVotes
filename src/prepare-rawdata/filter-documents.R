#############################################################################################
# Takes the list of documents and filters it to extract only those corresponding to
# certain criteria:
# - Temporal: focus on a given period.
# - Topical: focus on certain policy domains.
# 
# 07/2015 Israel Mendonça (v1)
# 09/2015 Vincent Labatut (v2)
#############################################################################################



#############################################################################################
# Takes the table containing the document details, as well as two dates, and returns the ids
# of the documents matching the period between the dates (included). If one of the dates (or
# both of them) is NA, it is replaced by an extreme date supposed to include all data.
#
# doc.details: table containing the details of the voted documents.
# start.date: starting date.
# end.date: ending date.
# returns: a vector of document ids, corresponding to the document matching the dates.
#############################################################################################
filter.by.date <- function(doc.details, start.date=NA, end.date=NA)
{	# possibly set extreme dates
	if(is.na(start.date))
		start.date <- "01/01/1900"
	if(is.na(end.date))
		end.date <- "31/12/3000"
	
	# possibly convert the start/end dates
	if(class(start.date)!="Date")
		start.date <- as.Date(start.date,"%d/%m/%Y")
	if(class(end.date)!="Date")
		end.date <- as.Date(end.date,"%d/%m/%Y")
	
	# retrieve and convert the date strings
	dates <- as.Date(doc.details[,COL.DATE],"%d/%m/%Y")
	
	# retain only the dates located between start and end (included)
	idx <- which(dates>=start.date & dates<=end.date)
	result <- doc.details[idx,COL.DOCID]
	
	return(result)
}


#############################################################################################
# Takes the table containing the document details, as well as a vector of policy domains, 
# and returns the ids of the documents matching one of the domains. If the domain vector is
# NA, or empty, then all domains are considered.
#
# doc.details: table containing the details of the voted documents.
# domain.details: table containing the domain names.
# domains: a vector of domains, or NA to use all keep domains.
# returns: a vector of document ids, corresponding to the document matching the domains.
#############################################################################################
filter.by.domain <- function(doc.details, domain.details, domains=c())
{	# possibly add all domains
	if(length(domains)==1 && is.na(domains))
		domains <- c()
	if(length(domains)==0)
		domains <- domain.details[,COL.DOMAIN]
	
	# retrieve the document domains
	doms <- doc.details[,COL.DOMAIN]
	
	# retain only the domains matching one of those specified in the specified vector
	idx <- match(doms,domains)
	idx <- which(!is.na(idx))
	result <- doc.details[idx,COL.DOCID]
		
	return(result)
}


#############################################################################################
# Takes the table containing the document details, as well as a vector of policy domains, 
# a starting and an ending dates, and returns the ids of the documents matching the at the same
# time the period between the dates (included) and one of the domains. 
# 
# If one of the dates (or both of them) is NA, it is replaced by an extreme date supposed to 
# include all data. Same thing for the domain vector, if it is NA or empty, then all domains 
# are considered.
#
# doc.details: table containing the details of the voted documents.
# start.date: starting date.
# end.date: ending date.
# domain.details: table containing the domain names.
# domains: a vector of domains, or NA to use all keep domains.
# returns: a vector of document ids, corresponding to the document matching the criteria.
#############################################################################################
filter.by.date.and.domain <- function(doc.details, start.date, end.date, domain.details, domains)
{	# filter by date
	ids1 <- filter.by.date(doc.details, start.date, end.date)
	# filter by domain
	ids2 <- filter.by.domain(doc.details, domain.details, domains)
	
	# keep only the documents appearing in both vectors
	result <- intersect(ids1,ids2)
	return(result)
}


#############################################################################################
# Test
#############################################################################################
#doc.ids <- filter.by.date(doc.details, start.date="07/10/2010", end.date="19/10/2010")
#print(doc.ids)
#doc.ids <- filter.by.domain(doc.details, domain.details, domains=c("Budget"))
#print(doc.ids)
#doc.ids <- filter.by.date.and.domain(doc.details, start.date="07/10/2010", end.date="19/10/2010", domain.details, domains=c("Budget"))
#print(doc.ids)
#doc.ids <- filter.by.date.and.domain(doc.details, start.date="07/10/2010", end.date="19/10/2010", domain.details, domains=c("Budget","Budgetary control"))
#print(doc.ids)
