#############################################################################################
# Defines common functions for all scripts.
# 
# 05/2016 Vincent Labatut
#############################################################################################




#############################################################################################
# Logs the specified message on screen, adding current date and time, and possibly some
# offset (to represent the hierarchy of function calls).
#
# offset: number of "." used to represent the hierarchical level of the message.
# ...: parameters fetched to the cat function.
#############################################################################################
tlog <- function(offset=NA, ...)
{	prefix <- paste0("[",format(Sys.time(),"%a %d %b %Y %X"),"] ")
	if(!is.na(offset))
	{	if(is.numeric(offset))
		{	os <- paste(rep(".",offset), sep="", collapse="")
			prefix <- paste0(prefix, os)
		}
		else
			prefix <- paste0(prefix, offset)
	}
	cat(prefix, ..., "\n", sep="")
}
