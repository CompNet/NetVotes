#############################################################################################
# Generates plots and data files for various statistics processed on the raw data.
# Some of them concern the votes, other concern the rebellion index.
# 
# 07/2015 Israel Mendonça (v1)
# 10/2015 Vincent Labatut (v2)
#############################################################################################
source("src/plot-tools/plot-bars.R")


#############################################################################################
# Counts the occurrences of policy domains among the voted documents.
#
# doc.details: table describing the voted documents.
# returns: a table containing the policy domains and their frequencies.
#############################################################################################
process.domain.frequencies <- function(doc.details)
{	cat("Retrieving the policy domains\n",sep="")
	
	# if the file already exists, just load it
	if(file.exists(DOMAIN.FREQ.FILE))
		result <- as.matrix(read.csv(DOMAIN.FREQ.FILE,check.names=FALSE))
	
	# otherwise, build the table and record it
	else
	{	# count the domains
		counts <- table(doc.details[,COL.DOMID])
		
		# build the table
		symbols <- names(counts)
		domains <- DOMAIN.FULLNAMES[symbols]
		result <- cbind(symbols,domains,counts[domains])
		colnames(result) <- c(COL.DOMID,COL.DOMAIN,COL.FREQUENCY)
		
		# record the table
		write.csv(result,file=DOMAIN.FREQ.FILE,row.names=FALSE)
	}
	
	return(result)
}


#############################################################################################
#############################################################################################
process.vote.distribution <- function(all.votes, doc.details)
{	# build date-codes to order the votes
	# > not convenient, we rather just number the votes
#	date.codes <- format(as.Date(doc.details[,COL.DATE],"%d/%m/%Y"),"%Y%m%d")
#	date.vals <- sort(unique(date.codes))
#	for(d in date.vals)
#	{	idx <- which(date.codes==d)
#		for(i in 1:length(idx))
#			date.codes[idx[i]] <- paste(
#					date.codes[idx[i]],
#					sprintf("%02d",i),
#					sep="")
#	}
	
	# process a simplified version of the data
	all.votes.smpl <- all.votes
	all.votes.smpl[all.votes.smpl==VOTE.ABSENT] <- VOTE.OTHER
	all.votes.smpl[all.votes.smpl==VOTE.ABST] <- VOTE.OTHER
	all.votes.smpl[all.votes.smpl==VOTE.DOCABSENT] <- VOTE.OTHER
	all.votes.smpl[all.votes.smpl==VOTE.NONE] <- VOTE.OTHER
	
	# build a list containing the detailed and simplified votes
	av <- list()
	av[[1]] <- all.votes
	av[[2]] <- all.votes.smpl
	vv <- list()
	vv[[1]] <- VOTE.VALUES
	vv[[2]] <- VOTE.VALUES.SMPL
	av.names <- c("detailed","simplified")
	
	cat("Plotting vote values (for, against, etc.)\n",sep="")
	# process both detailed and simplified data
	for(i in 1:length(av))
	{	file.prefix <- paste("votes-",av.names[i],"-",sep="")
		
		# consider each time period (each individual year as well as the whole term)
		for(date in names(DATE.STR.T7))
		{	# consider all domains individually (including all domains at once)
			for(dom in c(DOM.ALL,names(DOMAIN.FULLNAMES)))
			{	cat("Processing ",av.names[i]," data for domain ",dom," and period ",DATE.STR.T7[date],"\n",sep="")
				
				# retain only the documents related to the selected topic and dates
				if(dom==DOM.ALL)
					domval <- NA
				else
					domval <- dom
				docids <- filter.by.date.and.domain(doc.details, 
						start.date=DATE.START.T7[[date]], end.date=DATE.END.T7[[date]], 
						domains=domval)
				idx <- match(docids,doc.details[,COL.DOCID])
				if(length(idx)<=1)
					cat("WARNING: Only ",length(idx)," documents remaining after filtering >> not processing these data\n",sep="")
				else
				{	filtered.docs <- doc.details[idx,]
					
					# order the votes by date
					indices <- order(as.Date(filtered.docs[,COL.DATE],"%d/%m/%Y"))
					#x.vals <- rep(NA,length(indices))
					#x.vals[indices] <- 1:length(indices)
					
					# get the corresponding votes from the complete table
					cols <- match(docids, colnames(av[[i]]))
					cols <- cols[indices]
					# convert to list of column-vectors
					#votes <- as.list(data.frame(t(all.votes[,cols]))) 
					votes.temp <- av[[i]][,cols]
					#votes <- lapply(1:nrow(votes.temp), function(i) votes.temp[i,])
					votes <- split(votes.temp, rep(1:ncol(votes.temp), each=nrow(votes.temp)))
					
					# evolution of vote values (for, against, etc.)
						folder <- paste(OUT.FOLDER,"/",dom,"/",DATE.STR.T7[date],"/",sep="")
						dir.create(folder, recursive=TRUE, showWarnings=FALSE)
						# absolute counts as bars
						title <- paste("Numbers of votes for domain ",dom," and period ",DATE.STR.T7[date],sep="")
						plot.file <- paste(folder,file.prefix,"counts-bars",sep="")
						data <- plot.stacked.indiv.raw.bars(plot.file, 
								bar.names=as.character(docids[indices]), color.names=vv[[i]], 
								values=votes, 
								proportions=FALSE, areas=FALSE, y.lim=c(0,nrow(av[[i]])), 
								x.label="Documents (sorted by date)", colors.label="Votes", plot.title=title, 
								x.rotate=TRUE, format=c("PDF","PNG",NA))
						# absolute counts as areas
						plot.file <- paste(folder,file.prefix,"counts-areas",sep="")
						data <- plot.stacked.indiv.raw.bars(plot.file, 
								bar.names=1:length(docids), color.names=vv[[i]], 
								values=votes, 
								proportions=FALSE, areas=TRUE, y.lim=c(0,nrow(av[[i]])), 
								x.label="Documents (sorted by date)", colors.label="Votes", plot.title=title, 
								x.rotate=FALSE, format=c("PDF","PNG",NA))
						# record as a table
						data <- t(data.frame(data))
						data <- cbind(data.frame(docids[indices]),data)
						colnames(data) <- c(COL.DOCID, vv[[i]])
						table.file <- paste(folder,file.prefix,"counts",".csv",sep="")
						write.csv(data,file=table.file, row.names=FALSE)
		
						# proportions as bars
						title <- paste("Proportions of votes for domain ",dom," and period ",DATE.STR.T7[date],sep="")
						plot.file <- paste(folder,file.prefix,"proportions-bars",sep="")
						data <- plot.stacked.indiv.raw.bars(plot.file, 
								bar.names=as.character(docids[indices]), color.names=vv[[i]], 
								values=votes, 
								proportions=TRUE, areas=FALSE, y.lim=c(0,1), 
								x.label="Documents (sorted by date)", colors.label="Votes", plot.title=title, 
								x.rotate=TRUE, format=c("PDF","PNG",NA))
						# proportions as areas
						plot.file <- paste(folder,file.prefix,"proportions-areas",sep="")
						data <- plot.stacked.indiv.raw.bars(plot.file, 
								bar.names=1:length(docids), color.names=vv[[i]], 
								values=votes, 
								proportions=TRUE, areas=TRUE, y.lim=c(0,NA), 
								x.label="Documents (sorted by date)", colors.label="Votes", plot.title=title, 
								x.rotate=FALSE, format=c("PDF","PNG",NA))
						# record as a table
						data <- lapply(data,function(v) v/sum(v))
						data <- t(data.frame(data))
						data <- cbind(data.frame(docids[indices]),data)
						colnames(data) <- c(COL.DOCID, vv[[i]])
						table.file <- paste(folder,file.prefix,"proportions",".csv",sep="")
						write.csv(data,file=table.file, row.names=FALSE)
				}
			}
		}
	}
}

#############################################################################################
#############################################################################################
process.stats <- function()
{	# domains
	process.domain.frequencies(doc.details)
	
	# votes
	process.vote.distribution(all.votes, doc.details)
	
}




#TODO
# - histogram of votes: each bar corresponds to a document and vertically breaks down to all types of votes
#   we can do proportions and absolute values.
# - also record the corresponding tables
# - same thing by year (1 bar = 1 year) and for the whole term (hist bars = each type of vote?)
# - same for all the above, but with domains instead of types of votes
# - same thing but with the domains instead of the years (proportion of types of votes by domain).
# - could also do that by year *and* by domain.
#
# - histogram of rebellion: each bar displays the proportion of loyal/rebel for each *expressed* vote
# - same complementary stuff as above
#
# - move the domain distribution processing here, it has nothing to do in the load script.
# - generate a histogram with the comparative frequencies of the domains for the whole term.
# - histogram: each bar is one year, displaying the proportion for each domain.

# types of values :
# - proportions/counts for categorical variables (eg. proportions of vote types)
# - values for numerical variables (eg. average loyalty)
# modalities:
# - for the whole term
#   - categorical: hist with 1 bar = 1 cat
#   - numerical: irrelevent
# - by year
#	- categorical: stacked histos, 1 bar = 1 year
#	- numerical: line plot, 1 value = 1 year
# - by domain
#	- categorical: stacked histos, 1 bar = 1 domain
#	- numerical: scatter plot, 1 bar = 1 domain
# - by year and domain
#	- categorical: groups of stacked histos, 1 bar = 1 year, 1 group = 1 domain
#	- numerical: line plot, 1 line = 1 domain




# Calculate the rebelion indices for each MeP regarding it Party
CalculateRebelionIndex <- function(matrix) {
	converted.matrix <- ConvertMatrix(matrix)
	reply <- rowSums(converted.matrix,na.rm = TRUE) # Sum the values of each row and store in a vector ignoring NA values
	number_documents <- apply(converted.matrix, 1, function(x) length(which(!is.na(x)))) # Check how many documents are valid for the normalization
	reply <- reply / number_documents # Find the percentage of rebelion for each candidate (only for valid documents)
	reply[which(is.nan(reply))] <- 0
	return(reply)
}

# Plot the Rebelion indices
RebelionPlot <- function(rebelion.indexes) {
	pdf(file=file.path(paste0(output.agreement.dir,"/",dir.title,"/",file.title,"_rebelion_histogram.pdf")))
	hist(rebelion.indexes, xlab="Rebelion",ylab="Frequency",col=1:20,main="Rebelion Index")
	dev.off()
}

# Creates an Edge List for Gephi
GenerateEdgesGephi <- function(edges,filename,directed=FALSE) {
	tab <- as.data.frame(edges)
	write.table(tab,file.path(filename), row.names = FALSE,sep=",",dec=".")  
}

# Creates an Node List for Gephi
GenerateNodesGephi <- function(nodes.table,filename) {
	write.csv(nodes.table,file=filename, row.names = FALSE)		
}
