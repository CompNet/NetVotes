#############################################################################################
# Functions useful to the other more specific scripts designed to load raw data.
# 
# 10/2015 Vincent Labatut
#############################################################################################
source("src/define-constants.R")


#############################################################################################
# Process the political lines for each group for each vote. It is simply the vote value
# used by the majority of MEPs in the considered group. We consider only the expressed
# votes, i.e. FOR, AGAINST and ABSTENTION.
# 
# all.votes: the complete vote matrix, as created by the function concatenate.votes.
# mep.details: details describing the MEPs, as loaded by the function extract.mep.details.
# returns: a matrix describing the political line of each group for each vote.
#############################################################################################
extract.group.lines <- function(all.votes, mep.details)
{	tlog("..Retrieving the group political lines\n",sep="")
	
	# if the file already exists, just load it
	if(file.exists(GROUP.LINES.FILE))
		result <- as.matrix(read.csv2(GROUP.LINES.FILE,check.names=FALSE))
	
	# otherwise, build the table and record it
	else
	{	# clean vote values to keep only expressed ones
		all.votes[all.votes==VOTE.NONE] <- NA
		all.votes[all.votes==VOTE.ABSENT] <- NA
		all.votes[all.votes==VOTE.DOCABSENT] <- NA
		
		# init result matrix
		result <- matrix(NA, ncol=ncol(all.votes), nrow=length(GROUP.VALUES))
		cn <- colnames(all.votes)
		cn[1] <- COL.GROUP
		colnames(result) <- cn
		result[,1] <- GROUP.VALUES	
		
		# process the group political lines for each vote
		for(i in 2:ncol(all.votes))
		{	tlog("....Processing document ",i-1,"/",ncol(all.votes)-1,"\n",sep="")
			votes <- all.votes[,i]
			groups <- sort(unique(mep.details[,COL.GROUP]))
			
			# treat each political group
			for(g in 1:length(groups))
			{	group <- groups[g]
				idx <- which(mep.details[,COL.GROUP]==group)
				counts <- table(votes[idx])
				if(length(counts)==0)
					result[g,i] <- NA
				else
				{	mx.idx <- which(counts==max(counts))
					if(length(mx.idx)>1)
						result[g,i] <- NA
					else
						result[g,i] <- names(counts)[mx.idx]
				}
			}
		}
		
		# record the table
		write.csv2(result,file=GROUP.LINES.FILE,row.names=FALSE)
	}

	return(result)
}


#############################################################################################
# There are different variants to process behavior values. Here, we consider the political line
# of the groups as a reference. For a given MEP, we compare the way he votes to the political
# line of his group. If the group did not express an opinion (in majority), i.e. no FOR, AGAINST
# or ABSTENTION, we suppose there's no line. If the MEP did not express his opinion, we do not
# process his loyaly. When both the group and MEP opinions are expressed, then we compare them.
# The MEP is considered loyal if his expressed opinion matches that of his group.
#
# Note that VoteWatch uses a slightly different approach. They do not consider the NI group
# as an actual group, and its members are labeled as INDEPENDENT. Also, they only consider
# MEPs who voted at least 100 times during the term to determine the group majority.
#
# all.votes: the complete vote matrix, as created by the function concatenate.votes.
# mep.details: details describing the MEPs, as loaded by the function extract.mep.details.
# group.lines: the way each group voted for each document, in majority.
#############################################################################################
process.behavior.values <- function(all.votes, mep.details, group.lines)
{	tlog("..Processing the behavior values\n",sep="")
	
	# if the file already exists, just load it
	if(file.exists(MEP.BEHAVIOR.FILE))
	{	result <- as.matrix(read.csv2(MEP.BEHAVIOR.FILE,check.names=FALSE))
		result[,COL.MEPID] <- as.integer(result[,COL.MEPID])
	}
	
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
		{	tlog("....Processing MEP ",i,"/",nrow(all.votes),"\n",sep="")
			
			group <- mep.details[i,COL.GROUP]
			idx <- which(group.lines[,COL.GROUP]==group)
			lines <- group.lines[idx,2:ncol(group.lines)]
			votes <- all.votes[i,2:ncol(all.votes)]
			behavior <- lines == votes
			idx.loyal <- which(behavior)
			idx.rebel <- which(!behavior)
			behavior[idx.loyal] <- BEHAVIOR.LOYAL
			behavior[idx.rebel] <- BEHAVIOR.REBEL
			result[i,2:ncol(result)] <- behavior
		}
			
		# record the table
		write.csv2(result,file=MEP.BEHAVIOR.FILE,row.names=FALSE)
	}
	
	return(result)
}



#############################################################################################
# Compares two strings character by character, and indicates first pair of different ones.
# Sometimes, two characacters look similar but are actually not.
#
# This function was used mainly for debugging purposes, especially when cleaning raw data.
# 
# str1: first string.
# str2: second string.
# returns: position of the first pair of different characters.
#############################################################################################
#comp.chr <- function(str1, str2)
#{	tlog(str1," vs. ",str2,"\n",sep="")
#	i <- 0
#	if(nchar(str1)==nchar(str2))
#	{	stop <- FALSE
#		while(i<+nchar(str1) && !stop)
#		{	i <- i + 1
#			c1 <- substr(str1,i,i)
#			c2 <- substr(str2,i,i)
#			if(c1!=c2)
#			{	stop <- TRUE
#				tlog(c1," vs. ",c2,"\n",sep="")
#			}
#		}
#	}
#	return(i)
#}


