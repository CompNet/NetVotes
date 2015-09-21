# Function created to write a file to be imported in Gephi
WriteGFile <- function(nrow,d,filename) {
	write.table(data.frame(a=nrow,b=nrow(d)),file=file.path(filename), row.names = FALSE,sep=" ",col.names =FALSE,eol="\n")
	write.table(d[,c(1,2,3)],file=file.path(filename), row.names = FALSE,sep="\t",col.names =FALSE,eol="\n",append=TRUE)
}

generateGraphG <- function(agreementValues) {
	number <- ((nrow(agreementValues)*nrow(agreementValues))-nrow(agreementValues))/2
	g <- matrix(0,nrow=number,ncol=3)
	count <- 1
	pass <- upper.tri(agreementValues)                     
	for(i in 1:nrow(agreementValues)) {
		for(j in 1:ncol(agreementValues)) {
			if(pass[i, j] == TRUE) {
				#  The parameters top and floor are used as threshold values
				#  The threshold is necessary when you want to remove insignificant edges
				if(agreementValues[i, j]>=top || agreementValues[i,j]<=floor) {
					#The indices receives -1 because the nodes number must range from 0 to n-1
					g[count,1] <- i - 1
					g[count,2] <- j - 1
					g[count,3] <- round(agreementValues[i,j],6)
					count <- count+1
				}
			}
		}
	}
	
	colnames(g) <- c("Source", "Target", "Weight")
	
	return(g)
}
