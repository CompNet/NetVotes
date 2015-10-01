#############################################################################################
# Takes the whole vote matrix and filters it to extract only the votes corresponding to
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
	idx <- which(dates>=start.date & dates<=end.Date)
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
	if(length(domains)==1 & is.na(domains))
		domains <- c()
	if(length(domains==0))
		domains <- domain.details[,COL.DOMAIN]
	
	# retrieve the document domains
	doms <- doc.details[,COL.DOMAIN]
	
	# retain only the domains matching one of those specified in the specified vector
	idx <- which(doms %in% domains)
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










# Function created to remove MePs that have not expression to be calculated, i.e., did not vote
RemoveMePsLoyal <- function(matrix) {
	go.values <- list("Absent"=1,"Rebel"=2,"Loyal"=3,"Didn't vote"=4,"Documented Absence"=5,"NA"=0,"Independent"=6)
	back.values <- list("1"="Absent","2"="Rebel","3"="Loyal","4"="Didn't vote","5"="Documented Absence","6"="Independent","0"="NA")
	reply <- mapply(function(s) go.values[[s]],matrix)
	
	reply[sapply(reply, is.null)] <- NA
	reply <- unlist(reply)
	
	reply <- matrix(reply,ncol=ncol(matrix))
	rownames(reply) <- rownames(matrix)
	reply <- reply[which(rowSums(reply,na.rm=TRUE)!=0),]
	
	rabs <- mapply(function(s) back.values[[s]],reply)
	rabs <- matrix(rabs,ncol=ncol(matrix))
	rownames(rabs) <- rownames(reply)
	colnames(rabs) <- colnames(matrix)
	return(rabs)
}

RemoveMePs <- function(matrix) {
	go.values <- list("Absent"=1,"Abstain"=2,"Against"=3,"Didn't vote"=4,"For"=5,"NA"=0)
	back.values <- list("1"="Absent","2"="Abstain","3"="Against","4"="Didn't vote","5"="For","0"="NA")
	reply <- mapply(function(s) go.values[[s]],matrix)
	
	reply[sapply(reply, is.null)] <- NA
	reply <- unlist(reply)
	reply <- matrix(reply,ncol=ncol(matrix))
	rownames(reply) <- rownames(matrix)
	reply <- reply[which(rowSums(reply,na.rm=TRUE)!=0),]
	
	ret <- mapply(function(s) back.values[[s]],reply)
	ret <- matrix(ret,ncol=ncol(matrix))
	rownames(ret) <- rownames(reply)
	colnames(ret) <- colnames(matrix)
	return(ret)
}

FilterUsingConfigurations <- function() {
	
	# Filtering of the documents using the parameters passed through the configuration file
	
	# Adding columns for DAY,MONTH and YEAR from the column "Date"
	dates <- as.Date(docs$Date,format="%d/%m/%Y")
	if(as.integer(format(dates,format="%Y"))[1]<100) {
		docs$YEAR  <- as.integer(format(dates,format="%Y"))+2000
	}
	
	docs$MONTH <- as.integer(format(dates,format="%m"))
	docs$DAY   <- as.integer(format(dates,format="%d"))
	docs$Date  <- as.Date(paste(docs$DAY,docs$MONTH,docs$YEAR,sep="/"),format="%d/%m/%Y")
	
	# Selection of the Documents chosen by the user
	if("all" %in% DOCS.policies) 
		DOCS.policies  <- levels(factor(docs$Policy.area))
	
	DOCS.time_limits <- as.Date(DOCS.time_limits,format="%d/%m/%Y") # convertion from string to Date
	selected_Documents <- docs[which( docs$Date>=DOCS.time_limits[1] & docs$Date<=DOCS.time_limits[2] & docs$Policy.area %in% DOCS.policies),]
	
	# Filtering concerning MPs
	
	# Selection of the MPs chosen by the user  
	if("all" %in% MP.political_groups) 
		MP.political_groups <- levels(factor(MPs$political_group))
	
	if("all" %in% MP.countries) 
		MP.countries <- levels(factor(MPs$country))
	
	if("all" %in% MP.mp_ids)
		MP.mp_ids <- levels(factor(MPs$id))
	
	selected_MPs <- MPs[which(MPs$political_group %in% MP.political_groups & MPs$country %in% MP.countries & MPs$id %in% MP.mp_ids),]
	
	# Generate a filtered table using the parameters passed by the user
	filtered.table <- big.table[which(big.table$mp_id %in% selected_MPs$id),selected_Documents$doc_id]
	
	if(! is.null(dim(filtered.table))) {
		if(!(dim(filtered.table)[1]>1 && dim(filtered.table)[2]>1)) {
			warning(paste0("In file ",file," , the data is too much filtered (less than 2 MPs or less than 2 documents). Please change the filtering settings in ",file,"\n(No graph generated)"))
			too.much.filtered <- TRUE
		}
	} else {
		warning(paste0("In file ",file," , the data is too much filtered (less than 2 MPs or less than 2 documents). Please change the filtering settings in ",file,"\n(No graph generated)"))
		too.much.filtered <- TRUE
	}
	
	return(filtered.table)
}

# Filtering of the documents using the parameters passed through the configuration file
FilterUsingConfigurationsLoyalty <- function() {
	
	# Adding columns for DAY,MONTH and YEAR from the column "Date"
	dates <- as.Date(docs$Date,format="%d/%m/%Y")
	if(as.integer(format(dates,format="%Y"))[1]<100) {
		docs$YEAR  <- as.integer(format(dates,format="%Y"))+2000
	}
	
	docs$MONTH <- as.integer(format(dates,format="%m"))
	docs$DAY   <- as.integer(format(dates,format="%d"))
	docs$Date  <- as.Date(paste(docs$DAY,docs$MONTH,docs$YEAR,sep="/"),format="%d/%m/%Y")
	
	# Selection of the Documents chosen by the user
	if("all" %in% DOCS.policies)
		DOCS.policies  <- levels(factor(docs$Policy.area))
	
	DOCS.time_limits <- as.Date(DOCS.time_limits,format="%d/%m/%Y") # convertion from string to Date
	selected_Documents <- docs[which( docs$Date>=DOCS.time_limits[1] & docs$Date<=DOCS.time_limits[2] & docs$Policy.area %in% DOCS.policies),]
	
	
	# Filtering concerning MPs
	
	# Selection of the MPs chosen by the user
	
	if("all" %in% MP.political_groups) 
		MP.political_groups <- levels(factor(MPs$political_group))
	
	if("all" %in% MP.countries) 
		MP.countries <- levels(factor(MPs$country))
	
	if("all" %in% MP.mp_ids)
		MP.mp_ids <- levels(factor(MPs$id))
	
	selected_MPs <- MPs[which(MPs$political_group %in% MP.political_groups & MPs$country %in% MP.countries & MPs$id %in% MP.mp_ids),]   
	
	# Generate a filtered table using the parameters passed by the user
	filtered.table <- loyalty.table[which(loyalty.table$mp_id %in% selected_MPs$id),selected_Documents$doc_id]
	
	if(! is.null(dim(filtered.table))){
		if(!(dim(filtered.table)[1]>1 && dim(filtered.table)[2]>1)) {
			warning(paste0("In file ",file," , the data is too much filtered (less than 2 MPs or less than 2 documents). Please change the filtering settings in ",file,"\n(No graph generated)"))
			too.much.filtered <- TRUE
		}
	} else {
		warning(paste0("In file ",file," , the data is too much filtered (less than 2 MPs or less than 2 documents). Please change the filtering settings in ",file,"\n(No graph generated)"))
		too.much.filtered <- TRUE
	}
	
	return(filtered.table)
}
