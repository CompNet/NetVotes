#############################################################################################
# Generates a small fake dataset, used to test the framework. These functions are meant
# to be used independently (and before) the rest of the scripts.
# 
# 01/2016 Vincent Labatut
#############################################################################################
library("msm")	# truncated normal law
source("src/define-constants.R")
source("src/prepare-data/load-votewatch.R")



#############################################################################################
# Constants used only locally (to match VW)
#############################################################################################
V.FOR <- "For"
V.AGAINST <- "Against"
V.ABSTAIN <- "Abstain"
V.ABSENT <- "Absent"


#############################################################################################
# Randomly pick dates in the specified interval.
#
# This function was take from Dirk Eddelbuettel's answer in this StackOverflow post:
# http://stackoverflow.com/a/14721124/1254730
#
# date.nbr: number of dates to generate.
# start.date: date object representing the beginning of the considered period.
# end.date: date object representing the end of the considered period.
# returns: a (sorted) vector containing date.nbr dates between start.date and end.date.
#############################################################################################
generate.dates <- function(date.nbr, start.date, end.date)
{	#st <- as.POSIXct(as.Date(start.date))
	#et <- as.POSIXct(as.Date(end.date))
	st <- as.POSIXct(start.date)
	et <- as.POSIXct(end.date)
	dt <- as.numeric(difftime(et,st,unit="sec"))
	ev <- sort(runif(date.nbr, 0, dt))
	rt <- st + ev
	return(rt)
}


#############################################################################################
# Generate some data describing fake votes and record them using the same format than the 
# VoteWatch data. The whole framewok can consequently be tested on these generated data.
# 
# mep.nbr: number of MEPs
# doc.nbr: number of voted documents.
# folder: folder in which to record the generated files.
#############################################################################################
generate.raw.data <- function(mep.nbr, doc.nbr, folder)
{	root.folder <- paste(IN.FOLDER,"/",folder,"/",sep="")
	raw.folder <- paste(root.folder,"raw/",sep="")
	dir.create(path=raw.folder,showWarnings=FALSE,recursive=TRUE)
	
	# draw constant data
	member.states <- sample(x=COUNTRY.VALUES,size=mep.nbr,replace=TRUE)
	political.groups <- sample(x=names(GROUP.VW2SYMB),size=mep.nbr,replace=TRUE)
	absent.rate <- rtnorm(n=mep.nbr, mean=0, sd=0.1, lower=0, upper=1)
	rebellion.rate <- rtnorm(n=mep.nbr, mean=0.1, sd=0.2, lower=0, upper=1)
	# record them (for debug)
	table.file <- paste(root.folder,"absent-rate.csv",sep="")
	write.csv2(absent.rate,file=table.file,row.names=TRUE)
	table.file <- paste(root.folder,"rebellion-rate.csv",sep="")
	write.csv2(rebellion.rate,file=table.file,row.names=TRUE)
	
	# draw vote outcomes for each group
	group.votes <- matrix(sample(x=c(V.FOR,V.AGAINST,V.ABSTAIN),size=length(GROUP.VW2SYMB)*doc.nbr,replace=TRUE,prob=c(0.4,0.4,0.2)),
			nrow=length(GROUP.VW2SYMB),ncol=doc.nbr)
	colnames(group.votes) <- 1:doc.nbr
	rownames(group.votes) <- names(GROUP.VW2SYMB)
	# record these group political lines
	table.file <- paste(root.folder,"group-lines.csv",sep="")
	write.csv2(group.votes,file=table.file,row.names=TRUE)
	
	# files containing the votes details
	doc.votes <- rep(NA,doc.nbr)
	for(i in 1:doc.nbr)
	{	# proba for each MEP (rows) to vote For/Against/Abstain/Absent (cols) for document i
		probas <- t(sapply(1:mep.nbr, function(mep)
				{	gv <- group.votes[political.groups[mep],i]
					if(gv==V.FOR)
						c((1-absent.rate[mep])*(1-rebellion.rate[mep]),(1-absent.rate[mep])*rebellion.rate[mep]/2,(1-absent.rate[mep])*rebellion.rate[mep]/2,absent.rate[mep])
					else if(gv==V.AGAINST)
						c((1-absent.rate[mep])*rebellion.rate[mep]/2,(1-absent.rate[mep])*(1-rebellion.rate[mep]),(1-absent.rate[mep])*rebellion.rate[mep]/2,absent.rate[mep])
					else if(gv==V.ABSTAIN)
						c((1-absent.rate[mep])*rebellion.rate[mep]/2,(1-absent.rate[mep])*rebellion.rate[mep]/2,(1-absent.rate[mep])*(1-rebellion.rate[mep]),absent.rate[mep])
				}))
		
		# draw the vote of each MEP for document i, depending on these probas
		votes <- apply(probas,1,function(ps) sample(x=c(V.FOR,V.AGAINST,V.ABSTAIN,V.ABSENT),size=1,prob=ps))
		cnts <- table(votes)
		if(is.na(cnts[V.FOR]))
		{	if(is.na(cnts[V.AGAINST]))
				doc.votes[i] <- NA
			else
				doc.votes[i] <- "-"
		}
		else
		{	if(is.na(cnts[V.AGAINST]))
				doc.votes[i] <- "+"
			else
			{	if(cnts[V.FOR]>=cnts[V.AGAINST])
					doc.votes[i] <- "+"
				else
					doc.votes[i] <- "-"
			}
		}
		
		# determine the rebellion/loyalty of each MEP for this document
		rebellion <- rep(NA,mep.nbr)
		group.lines <- group.votes[political.groups]
		idx <- which(votes==group.lines)
		rebellion[idx] <- votes[idx]
		
		# create the document vote table
		vote.df <- data.frame(
			"Name"=paste("Firstname",1:mep.nbr," LASTNAME",1:mep.nbr,sep=""),
			"Member State"=member.states,
			"Loyal / Rebel to political group"=rebellion,
			"Vote"=votes,
			"Group"=political.groups,
		check.names=FALSE,stringsAsFactors=FALSE)

		# record the table
		table.file <- paste(raw.folder,i,".csv",sep="")
		write.csv(vote.df,file=table.file,row.names=FALSE)
	}
	
	# file containing the documents details
	doc.df <- data.frame(
		"Doc Id"=1:doc.nbr,
		"Date"=format(generate.dates(doc.nbr,DATE.START.T7[[DATE.T7.TERM]],DATE.END.T7[[DATE.T7.TERM]]),"%d/%m/%Y"),
		"Name of document"=paste("Document",1:doc.nbr,sep=""),
		"Result of vote"=doc.votes,
		"Parliament or council"=rep("EP",doc.nbr),
		"Policy area"=sample(x=names(DOMAIN.VW2SYMB),size=doc.nbr,replace=TRUE),
	check.names=FALSE,stringsAsFactors=FALSE)

	# record the table
	table.file <- paste(root.folder,"list.csv",sep="")
	write.csv2(doc.df,file=table.file,row.names=FALSE)
}


#############################################################################################
# tests
#############################################################################################
generate.raw.data(mep.nbr=30, doc.nbr=15, folder="test")
