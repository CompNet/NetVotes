#############################################################################################
# Processes the agreement between MEPs, and generate plots and statistics.
# These scripts directly make use of the "score matrices" stored in in/score as lists. 
# 
# 07/2015 Israel Mendonça (v1)
# 10/2015 Vincent Labatut (v2)
#############################################################################################
source("src/define-constants.R")
source("src/plot-tools/plot-histos.R")
source("src/prepare-data/filter-data.R")



#############################################################################################
# Loads the specified score table, and returns, well, this table.
#
# Note: the agreement between a MEP and himself might not be 1, depending on how the score
# table is defined. For instance, if Absention vs. Abstention gets a score of 0, then a
# MEP who abstained at a vote gets a score of zero when compared to himself (for this vote).
#
# file.name: name of the file containing the scores (without the .txt extension).
#
# returns: the loaded table.
#############################################################################################
load.score.table <- function(file.name)
{	# load the file content
	file.name <- paste0(file.name,".txt")
	file <- file.path(SCORE.FOLDER, file.name)
	df <- read.table(file, header=FALSE, as.is=TRUE, check.names=FALSE)
	# possibly replace NA by equivalent strings
	df[is.na(df[,1]),1] <- "NA"
	df[is.na(df[,2]),2] <- "NA"
	
	# init the result table
	result <- matrix(NA, nrow=length(VOTE.VALUES)+1, ncol=length(VOTE.VALUES)+1)
	rownames(result) <- c(VOTE.VALUES, "NA")
	colnames(result) <- c(VOTE.VALUES, "NA")
	
	# fill the table
	for(i in 1:nrow(df))
	{	result[df[i,1],df[i,2]] <- df[i,3]
		result[df[i,2],df[i,1]] <- df[i,3]
	}
	
	return(result)
}


#############################################################################################
# Processes the agreement scores between all pairs of MEPs, for the specified votes and
# agreement matrix. Note the parameter 'votes' is a vector containing all votes for all MEPs
# for a *single* document. The returned matrix uses the same MEP orders for its rows and columns.
#
# NA values indicate the score is undefined for the considered pair of MEPs, and should
# therefore be ignored in subsequent processings, e.g. when averaging to get the agreement index.
#
# Note: the agreement between a MEP and himself might not be 1, depending on how the score
# matrix is defined. For instance, if Absention vs. Abstention gets a score of 0, then a
# MEP who abstained gets a score of zero when compared to himself.
# 
# votes: vector of all MEP votes for the considered document.
# score.table: matrix containing the reference scores, previously loaded with load.aggreement.matrix.
#
# returns: a square matrix whose size is the number of MEPs, and containing all agreement scores
#  		   for the considered document.
#############################################################################################
process.agreement.scores <- function(votes, score.table)
{	# possibly replace NA by equivalent strings
	votes[is.na(votes)] <- "NA"
	
	# init matrix
	result <- matrix(NA,nrow=length(votes), ncol=length(votes))
	
	#print(votes)
	#print(score.table)
	
	# fill matrix
	for(i in 1:length(votes))
	{	for(j in 1:length(votes))
		{	#tlog(i,":",j)
			result[i,j] <- score.table[votes[i],votes[j]]
		}
	}
	
	return(result)
}


#############################################################################################
# Processes the agreement indices between all pairs of MEPs, for the specified votes and
# agreement matrix. Note the parameter 'votes' is a matrix containing all votes for all MEPs
# (on the rows) for a series of documents (columns). The returned matrix uses the same MEP orders 
# for its row and columns.
#
# NA values indicate the agreement index is undefined for the considered pair of MEPs, and should
# therefore be ignored in subsequent processings, e.g. when extracting a network or generating
# plots.
#
# Note: the agreement between a MEP and himself might not be 1, depending on how the score
# matrix is defined. For instance, if Absention vs. Abstention gets a score of 0, then a
# MEP who abstained gets a score of zero when compared to himself.
# 
# votes: matrix of all MEP (rows) votes for the considered documents (columns).
# score.table: matrix containing the reference scores, previously loaded with load.score.table.
#
# returns: a square matrix whose size is the number of MEPs, and containing all agreement indices
#  		   for the considered documents.
#############################################################################################
process.agreement.index <- function(votes, score.table)
{	# init data structures
	counts <- matrix(0,nrow=nrow(votes),ncol=nrow(votes))
	sums <- matrix(0,nrow=nrow(votes),ncol=nrow(votes))
		
	# process each document in the vote matrix
	for(i in 1:ncol(votes))
	{	tlog("..........Processing document",i,"/",ncol(votes))
		# get scores
		scores <- process.agreement.scores(votes[,i], score.table)
		
		# update counts
		increments <- matrix(0,nrow=nrow(scores),ncol=ncol(scores))
		increments[!is.na(scores)] <- 1
		counts <- counts + increments
		
		# update sums
		scores[is.na(scores)] <- 0
		sums <- sums + scores
	}
	
	# normalize the sums to get the agreement indices
	#print(sums)
	#print(counts)
	result <- sums / counts
	result[is.nan(result)] <- NA
	return(result)
}


#############################################################################################
# Processes the agreement for all domains and time periods, for the specified raw votes.
#
# Note: the agreement between a MEP and himself might not be 1, depending on how the score
# matrix is defined. For instance, if Absention vs. Abstention gets a score of 0, then a
# MEP who abstained gets a score of zero when compared to himself.
#
# all.votes: raw vote data, including how each MEP voted.
# doc.details: description of each voted document.
# score.file: files describing the scores to use when processing the inter-MEP agreement
#			  (without the .txt extension).
# domains: political domains to consider when processing the data.
# dates: time periods to consider when processing the data.
# country: member state currently processed (or NA if none in particular).
# group: political gorup currently processed (or NA if none in particular).
# plot.formats: formats used for the plot files.
#############################################################################################
process.agreement.stats <- function(all.votes, doc.details, score.file, domains, dates, country, group, plot.formats)
{	object <- "Agreement index"
	x.label <- paste0("Agreement index - score=",score.file)
			
	# setup title prefix
	if(is.na(country))
		if(is.na(group))
			plot.prefix <- ""
		else
			plot.prefix <- paste0("[",group,"] ")
	else
		plot.prefix <- paste0("[",country,"] ")
	
	# load the agreement scores
	score.table <- load.score.table(score.file)
	
	# consider each domain individually (including all domains at once)
#	for(dom in domains)
	foreach(dom=domains) %dopar% 
	{	source("src/define-imports.R")
		
		# setup folder
		#folder <- paste0(AGREEMENT.FOLDER,"/",subfolder,"/",score.file,"/",dom,"/")
		folder <- get.agreement.path(score=score.file, country, group, domain=dom)
		dir.create(folder, recursive=TRUE, showWarnings=FALSE)
		
		# consider each time period (each individual year as well as the whole term)
		for(date in dates)
		#date <- DATE.T7.TERM
		{	tlog("......Processing agreement data for domain ",dom," and period ",DATE.STR.T7[date])
			
			# retain only the documents related to the selected topic and dates
			if(dom==DOMAIN.ALL)
				domval <- NA
			else
				domval <- dom
			filtered.doc.ids <- filter.docs.by.date.and.domain(doc.details, 
				start.date=DATE.START.T7[[date]], end.date=DATE.END.T7[[date]], 
				domains=domval)
			# check if there's enough data remaining
			if(length(filtered.doc.ids)>1)
			{	# format data
				cols <- match(filtered.doc.ids, colnames(all.votes))
				active.idx <- which(apply(all.votes[,cols],1,function(v) !all(is.na(v))))
				if(length(active.idx)>1)
				{	votes <- all.votes[active.idx,cols]
					agreement <- process.agreement.index(votes, score.table)
					
					# record raw agreement index values
					colnames(agreement) <- all.votes[active.idx,COL.MEPID]
					rownames(agreement) <- all.votes[active.idx,COL.MEPID]
					table.file <- file.path(folder,paste0(DATE.STR.T7[date],"-agreement.csv"))
					write.csv2(agreement,file=table.file, row.names=TRUE)
					
					# keep only the triangular part of the matrix (w/o the diagonal)
					#print(agreement)				
					agr.vals <- agreement[upper.tri(agreement,diag=FALSE)]
					
					# check there are enough agreement values
					if(all(is.na(agr.vals)))
						tlog("........WARNING: All agreement values are NAs >> not processing these data")
					else
					{	# plot absolute counts as bars
						title <- paste0(plot.prefix,"Distribution of ",object," - domain=",dom,", - period=",DATE.STR.T7[date])
						plot.file <- file.path(folder,paste0(DATE.STR.T7[date],"-counts"))
						data <- plot.histo(plot.file, values=agr.vals,
							x.label, 
							proportions=FALSE, x.lim=c(-1,1), y.max=NA, break.nbr=NA,
							plot.title=title, format=plot.formats)
						# record as a table
						data <- data[,c("y","xmin","xmax")]
						table.file <- paste0(plot.file,".csv")
						write.csv2(data,file=table.file, row.names=FALSE)
						
						# plot proportions as bars
						title <- paste0(plot.prefix,"Distribution of ",object," - domain=",dom,", - period=",DATE.STR.T7[date])
						plot.file <- file.path(folder,paste0(DATE.STR.T7[date],"-proportions"))
						data <- plot.histo(plot.file, values=agr.vals,
							x.label, 
							proportions=TRUE, x.lim=c(-1,1), y.max=0.5, break.nbr=NA, 
							plot.title=title, format=plot.formats)
						# record as a table
						data <- data[,c("y","xmin","xmax")]
						table.file <- paste0(plot.file,".csv")
						write.csv2(data,file=table.file, row.names=FALSE)
					}
				}
				else
					tlog("........WARNING: Only ",length(active.idx)," active MEPs after filtering >> not processing these data")
			}
			else
				tlog("........WARNING: Only ",length(filtered.doc.ids)," documents remaining after filtering >> not processing these data")
		}
	}
}


#############################################################################################
# Main function of this script, generating all agreement-related tables and plots.
#
# Note: the agreement between a MEP and himself might not be 1, depending on how the score
# matrix is defined. For instance, if Absention vs. Abstention gets a score of 0, then a
# MEP who abstained gets a score of zero when compared to himself.
#
# all.votes: individual vote data, i.e. how each MEP voted.
# doc.details: description of each voted document.
# mep.details: description of each MEP.
# score.file: files describing the scores to use when processing the inter-MEP agreement
#			  (without the .txt extension).
# domains: political domains to consider when processing the data.
# dates: time periods to consider when processing the data.
# everything: whether to process all data without distinction of country or political group.
# countries: member states to consider separately when processing the data.
# groups: political groups to consider separately when processing the data.
# plot.formats: formats used for the plot files.
#############################################################################################
process.agreement <- function(all.votes, doc.details, mep.details, score.file, domains, dates, everything, countries, groups, plot.formats)
{	tlog("***************************************************")
	tlog("****** PROCESSING AGREEMENT")
	tlog("***************************************************")
	
	# process agreement by political group
	tlog("..Process stats by group")
	for(group in groups)
	{	tlog("....Process stats for group ",group)
		
		# select data
		filtered.mep.ids <- filter.meps.by.group(mep.details,group)
		idx <- match(filtered.mep.ids,all.votes[,COL.MEPID])
		group.votes <- all.votes[idx,]
		
		# process agreement
		process.agreement.stats(group.votes, doc.details, score.file, domains, dates, country=NA, group, plot.formats)
	}
	
	# process agreement by home country
	tlog("..Process stats by country")
	for(country in countries)
	#country <- COUNTRY.HR
	{	tlog("....Process stats for country ",country)
		
		# select data
		filtered.mep.ids <- filter.meps.by.country(mep.details,country)
		idx <- match(filtered.mep.ids,all.votes[,COL.MEPID])
		country.votes <- all.votes[idx,]
		
		# process agreement
		process.agreement.stats(country.votes, doc.details, score.file, domains, dates, country, group=NA, plot.formats)
	}
	
	# process agreement for all data
	if(everything)
	{	tlog("..Process agreement for all data")
		process.agreement.stats(all.votes, doc.details, score.file, domains, dates, country=NA, group=NA, plot.formats)
	}
}


#############################################################################################
# Tests
#############################################################################################
#agreement.matrix <- load.score.matrix("m3.txt")
#################################################
#votes <- c("For",NA,NA,NA,"For","Against","Abstention")
#scores <- process.agreement.scores(votes, agreement.matrix)
#print(scores)
#################################################
#votes <- matrix(c(
#		"For", NA, NA,	  NA, "For",		"Against", "Abstention",
#		"For", NA, "For", NA, "Abstention", "For",	   "Abstention",
#		"For", NA, NA,	  NA, "For",		"Against", "Abstention"),
#		ncol=3)
#colnames(votes) <- 1:3
#indices <- process.agreement.index(votes, agreement.matrix)
#print(indices)
#################################################
#process.agreement(all.votes, doc.details, mep.details, score.file="m3")
#################################################
