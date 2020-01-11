# TODO: Add comment
# 
# Author: nejat
###############################################################################



############################################################################
#
############################################################################
write.into.log = function(worker.id = NA, offset = NA, ...){	
	filename = paste(LOG.FILENAME.PREFIX, worker.id, ".txt", sep="")

	if(!file.exists(filename))
		sink(filename, append = F)
	else 
		sink(filename, append = T)

	tlog(worker.id, offset, ...)
	sink()
	
}

#############################################################################################
# Logs the specified message on screen, adding current date and time, and possible some
# offset (to represent the hierarchy of function calls).
#
# offset: number of "." used to represent the hierarchical level of the message.
# ...: parameters fetched to the cat function.
#############################################################################################
tlog <- function(worker.id=NA, offset=NA, ...)
{	prefix <- paste("[",format(Sys.time(),"%a %d %b %Y %X"),"] ",sep="")
	if(!is.na(offset))
	{	if(is.numeric(offset))
		{	os <- paste(rep(".",offset), sep="", collapse="")
			prefix <- paste(prefix, os, sep="")
		}
		else
			prefix <- paste(prefix, offset, sep="")
	}
	
	if(is.na(worker.id))
		cat(prefix, " ", ..., "\n", sep="") # cat function can not handle list variables, so provide character or vector
	else
		cat(prefix, " worker-id:", worker.id, "    ", ..., "\n", sep="")
}



############################################################################
#
############################################################################
copy.file.from.template = function(filename){
	new.filename.suffix = ".tmp"
	new.filename = paste(filename, new.filename.suffix, sep="")
	
	file.copy(
			filename,
			new.filename,
			overwrite=TRUE,
			recursive=FALSE,
			copy.mode=TRUE,
			copy.date=FALSE
	)
	return(new.filename)
}


############################################################################
#
############################################################################
delete.file = function(filename){
	unlink(
			filename, 
			recursive=FALSE
	)
}


####################"
#
#####################
roundNumber = function(number, nb.decimal=3){
	return( as.numeric( format(round(number, nb.decimal), nsmall = nb.decimal) ) )
}