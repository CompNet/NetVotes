#############################################################################################
# 
# 
# 07/2015 Israel Mendonça (v1)
# 09/2015 Vincent Labatut (v2)
#############################################################################################


#TODO
# - histogram of votes: each bar corresponds to a document and breaks down to all types of votes
# - also record the corresponding table
# - same thing by year (1 bar = 1 year) and for the whole term (?)
#
# - histogram of rebellion: each bar displays the proportion of loyal/rebel for each *expressed* vote
# - same complementary stuff as above
#
# - move the domain distribution processing here, it has nothing to do in the load script.
# - (maybe just keep a conversion table there, allowing to convert specific domain names to normalized ones).
# - generate a histogram with the compartive frequencies of the domains for the whole term.
# - histogram: each bar is one year, displaying the proportion for each domain.





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
