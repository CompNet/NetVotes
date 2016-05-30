#############################################################################################
# Defines common functions for all scripts.
# 
# 05/2016 Vincent Labatut
#############################################################################################




#############################################################################################
# Logs the specified message on screen, adding current date and time, and possible some
# offset (to represent the hierarchy of function calls).
#############################################################################################
tlog <- function(msg, offset=NA)
{	prefix <- paste("[",format(Sys.time(),"%a %d %b %Y %X"),"] ",sep="")
	if(!is.na(offset))
	{	os <- paste(rep(".",offset), sep="", collapse="")
		prefix <- paste(prefix, os, sep="")
	}
	cat(prefix,msg,"\n",sep="")
}
