#############################################################################################
#  Functions useful to the various scripts designed to load raw data.
# 
# 10/2015 Vincent Labatut
#############################################################################################


#############################################################################################
# Process the political lines for each group for each vote. It is simply the vote value
# used by the majority of MEPs in the considered group.
# 
# all.votes: the complete vote matrix, as created by the function concatenate.votes.
# mep.details: details describing the MEPs, as loaded by the function extract.mep.details.
# returns: a matrix describing the political line of each group for each vote.
#############################################################################################
extract.group.lines <- function(all.votes, mep.details)
{	cat("Retrieving the group political lines\n",sep="")
	
	# if the file already exists, just load it
	if(file.exists(GROUP.LINES.FILE))
		result <- as.matrix(read.csv(GROUP.LINES.FILE,check.names=FALSE))
	
	# otherwise, build the table and record it
	else
	{	# init result matrix
		result <- matrix(NA, ncol=ncol(all.votes), nrow=length(GROUP.NAMES))
		cn <- colnames(all.votes)
		cn[1] <- COL.GROUP
		colnames(result) <- cn
		result[,1] <- GROUP.NAMES	
		
		# process the group political lines for each vote
		for(i in 2:ncol(all.votes))
		{	cat("Processing document ",i-1,"/",ncol(all.votes)-1,"\n",sep="")
			votes <- all.votes[,i]
			groups <- sort(unique(mep.details[,COL.GROUP]))
			
			# treat each political group
			for(g in 1:length(groups))
			{	group <- groups[g]
				idx <- which(mep.details[,COL.GROUP]==group)
				counts <- table(votes[idx])
				mx.idx <- which(counts==max(counts))
				if(length(mx.idx)>1)
					result[g,i] <- NA
				else
					result[g,i] <- names(counts)[mx.idx]
			}
		}
		
		# record the table
		write.csv(result,file=GROUP.LINES.FILE,row.names=FALSE)
	}

	return(result)
}


#############################################################################################
# There are different variants to process loyalty. Here, we consider the political line
# of the groups as a reference. For a given MEP, we compare the way he votes to the political
# line of his group. If the group did not express an opinion (in majority), i.e. no FOR, AGAINST
# or ABSTENTION, we suppose there's no line. If the MEP did not express his opinion, we do not
# process his loyaly. When both the group and MEP opinions are expressed, then we compare them.
# The MEP is considered loyal if his expressed opinion matches that of his group.
#
# Note that VoteWatch uses a slightly different approach. They do not consider the NI group
# as an actual group, and its members are labeled as INDEPENDENT. Also, they (apparently) 
# determine the group line using only the expressed votes (we don't, because we think a group
# can very well decide not to vote at all as a way of protesting). Finally, they only consider
# MEPs who voted at least 100 times during the term (we suppose this also holds when determining
# the group majority).
#
# all.votes: the complete vote matrix, as created by the function concatenate.votes.
# mep.details: details describing the MEPs, as loaded by the function extract.mep.details.
# group.lines: the way each group voted for each document, in majority.
#############################################################################################
process.loyalty.values <- function(all.votes, mep.details, group.lines)
{	cat("Processing the loyalty values\n",sep="")
	
	# if the file already exists, just load it
	if(file.exists(MEP.LOYALTY.FILE))
		result <- as.matrix(read.csv(MEP.LOYALTY.FILE,check.names=FALSE))
	
	# otherwise, build the table and record it
	else
	{	# clean the group lines: put NA for non-expressed votes
		group.lines[group.lines==VOTE.NONE] <- NA
		group.lines[group.lines==VOTE.ABSENT] <- NA
		group.lines[group.lines==VOTE.DOCABSENT] <- NA
		
		# same thing with individual votes
		all.votes[all.votes==VOTE.NONE] <- NA
		all.votes[all.votes==VOTE.ABSENT] <- NA
		all.votes[all.votes==VOTE.DOCABSENT] <- NA
		
		# init result matrix
		result <- matrix(NA,nrow=nrow(all.votes),ncol=ncol(all.votes))
		colnames(result) <- colnames(all.votes)
		result[,1] <- all.votes[,1]
		
		# process each MEP
		for(i in 1:nrow(all.votes))
		{	cat("Processing MEP ",i,"/",nrow(all.votes),"\n",sep="")
			
			group <- mep.details[i,COL.GROUP]
			idx <- which(group.lines[,COL.GROUP]==group)
			lines <- group.lines[idx,2:ncol(group.lines)]
			votes <- all.votes[i,2:ncol(all.votes)]
			loyalty <- lines == votes
			idx.loyal <- which(loyalty)
			idx.rebel <- which(!loyalty)
			loyalty[idx.loyal] <- LOYALTY.LOYAL
			loyalty[idx.rebel] <- LOYALTY.REBEL
			result[i,2:ncol(result)] <- loyalty
		}
			
		# record the table
		write.csv(result,file=MEP.LOYALTY.FILE,row.names=FALSE)
	}
	
	return(result)
}
