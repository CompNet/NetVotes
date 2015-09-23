#############################################################################################
# 
# 
# 07/2015 Israel Mendonça (v1)
# 09/2015 Vincent Labatut (v2)
#############################################################################################

ConvertMatrix <- function(matrix) {
	
	loyalty.values <- list("Absent"=NA,"Rebel"=1,"Loyal"=0,"Didn't vote"=NA,"Documented Absence"=NA,"NA"=NA,"Independent"=NA)
	
	matrix[sapply(matrix,is.null)] <- NA
	reply <- mapply(function(s) loyalty.values[[s]],matrix)
	reply[sapply(reply, is.null)] <- NA
	reply <- unlist(reply)
	
	reply <- matrix(data = reply, nrow=nrow(matrix), ncol=ncol(matrix))
	
	return(reply)
}

AgreementPlot <- function(agreementMatrix) {
	temp <- agreementMatrix
	temp[lower.tri(temp,diag=TRUE)] <- NA
	temp <- round(temp,2)
	
	pdf(file=file.path(paste0(output.agreement.dir,"/",dir.title,"/",file.title,"_agreement_distribution.pdf")))
	barplot(table(temp),ylim=c(0,30000), xpd=FALSE, xlab="Agreement",ylab="Frequency",col=1:20,main="Agreement Distribution")
	dev.off()
	
	pdf(file=file.path(paste0(output.agreement.dir,"/",dir.title,"/",file.title,"_agreement_histogram.pdf")))
	hist(temp, xlab="Agreement",ylab="Frequency",col=1:20,main="Agreement Distribution")
	dev.off()
}

# Function to generate the agreement between each MeP
CalculateAgreement <- function(vetor) {
	temp <- matrix(0,nrow=nrow(vetor),ncol=nrow(vetor))
	rownames(temp) <- rownames(vetor)
	agreement <- 0
	agr <- 0
	points <- vector(length=ncol(vetor))
	col.size = ncol(vetor)
	
	if(selected.table==2) {
		for(i in 1:(nrow(vetor)-1)) {
			for(j in (i+1):nrow(vetor)) {
				agr <- paste(vetor[i,],vetor[j,],sep="")      # Compare Two-by-Two
				points <- sapply(agr,function(s) table2[[s]]) # Apply the Function to substitue the values
				agreement <- sum(unlist(points))              # Sum the values
				agreement <- agreement/col.size               # Normalize by the number of documents
				temp[i, j] <- agreement                       # Put the value in the adjacency matrix
				temp[j, i] <- agreement                       # Put the value in the adjacency matrix
			}
		}
	} else {
		for(i in 1:(nrow(vetor)-1)) {
			for(j in (i+1):nrow(vetor)) {
				agr <- paste(vetor[i,],vetor[j,],sep="")      # Compare Two-by-Two
				points <- sapply(agr,function(s) table1[[s]]) # Apply the Function to substitue the values
				agreement <- sum(unlist(points))              # Sum the values
				agreement <- agreement/col.size               # Normalize by the number of documents
				temp[i, j] <- agreement                       # Put the value in the adjacency matrix
				temp[j, i] <- agreement                       #Put the value in the adjacency matrix
			}
		}
	}
	
	return(temp)
	
}
