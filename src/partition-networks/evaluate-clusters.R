#############################################################################################
# Estimates the quality of the detected partitions, using various measures designed for
# community detection and for correlationc clustering.
# 
# 07/2015 Israel Mendonça (v1)
# 11/2015 Vincent Labatut (v2)
#############################################################################################
library("igraph")

source("src/define-constants.R")



#############################################################################################
# Process two versions of the structural imbalance for the specified graph and partition.
#
# g: graph to consider.
# partition: integer vector representing the partition to consider.
# returns: a list containing the imbalance in terms of number of misplaced links (unweighted
# 		   value) and in terms of sum of the weights of misplaced links (weighted value).
#############################################################################################
process.structural.imbalance <- function(g, partition)
{	# compare the clusters of connected nodes
	edge.mat <- get.edgelist(g)
	clus.mat <- cbind(partition[edge.mat[,1]],partition[edge.mat[,2]])
	same.clus <- partition[,1]==partition[,2]
	
	# compare link signs and positions 
	neg.links <- E(g)$weight<0
	pos.links <- E(g)$weight>0
	neg.misplaced <- same.clus & neg.links
	pos.misplaced <- !same.clus & pos.links
	all.misplaced <- neg.misplaced | pos.misplaced
	
	# count the number of misplaced links
	unweighted <- length(E(g)[mis.placed])
	# sum the weights of misplaced links
	weighted <- sum(E(g)$weight[mis.placed])
	
	# build and return result
	result <- list(unweighted=unweighted, weighted=weighted) 
	return(result)
}

#TODO record 12 values : weighted/unweighted counts/proportions pos/neg/total
#TODO community stats must be recorded in this script, not the comdet one
#TODO the output from pILS needs some conversion


##  Function to read the ILS
#ILSMembership <- function(file.title) {
#	
#	file.name <- paste0(ils.input.dir,"/",file.title)
#	file.name <- paste0(file.name,"/cc-result.txt")
#	# open file (read mode)
#	con <- file(file.name, "r")
#	lines <- readLines(con)
#	close(con)
#	
#	# process the file content
#	i <- 4
#	line <- lines[i]
#	res <- list()
#	while(line!="")
#	{  # process current line
#		#print(line)
#		line2 <- strsplit(x=line, "[ ", fixed=TRUE)[[1]][2]
#		line3 <- strsplit(x=line2, " ]", fixed=TRUE)[[1]][1]
#		nodes <- as.integer(strsplit(x=line3, " ", fixed=TRUE)[[1]]) + 1 # plus one because C++ starts counting from 0
#		res[[length(res)+1]] <- nodes
#		
#		# read next line
#		i <- i + 1
#		line <- lines[i]      
#	}
#	
#	# build the membership vector
#	mx <- max(unlist(res))
#	membership <- rep(NA,mx)
#	for(i in 1:length(res))
#	{  nodes <- res[[i]]
#		membership[nodes] <- i 
#	}
#	
#	return(membership)
#}
#
## Function created to calculate the CC Imbalance
#CalculateCCImbalance <- function(adjacency.matrix, cluster) {
#	
#	sum.intern <- 0 
#	sum.extern <- 0
#	
#	temp <- adjacency.matrix
#	temp[lower.tri(temp,diag=TRUE)] <- 0
#	
#	#Edges intra-clusters
#	for(i in 1:nrow(adjacency.matrix)){
#		clust.i <- cluster[i]
#		a <- temp[i,which(cluster == clust.i)]
#		sum.intern <- sum.intern + sum(a[which(temp[i,which(cluster == clust.i)] < 0)])    
#	}
#	
#	#Edges inter-clusters
#	for(j in 1:nrow(adjacency.matrix)){
#		clust.j <- cluster[j]
#		a <- temp[j,which(cluster != clust.j)]
#		sum.extern <- sum.extern + sum(a[which(temp[j,which(cluster != clust.j)] > 0)])
#	}
#	
#	sum.intern <- abs(sum.intern)
#	sum.extern <- abs(sum.extern)
#	
#	imbalance.sum <- sum.intern + sum.extern
#	edges.sum <- sum(abs(temp))
#	positive.sum <- sum(temp[which(temp > 0)])
#	negative.sum <- abs(sum(temp[which(temp < 0)]))
#	
#	imbalance.percentage <- imbalance.sum / edges.sum #percentage of imbalance considering the sum of all edges of the graph
#	negative.percentage <- sum.intern / imbalance.sum #percentage of negative edges that contributes for the imbalance
#	positive.percentage <- sum.extern / imbalance.sum #percentage of positive edges that contributes for the imbalance
#	
#	imbalance <- list("total.imbalance" = round(imbalance.sum,2), "total.imbalance.percentage" = (100*round(imbalance.percentage,2)),
#			"positive.imbalance" = round(sum.extern,2), "positive.imbalance.percentage" = (100*round(positive.percentage,2)),
#			"negative.imbalance" = round(sum.intern,2), "negative.imbalance.percentage" = (100*round(negative.percentage,2))) 
#	
#	return(imbalance)
#}
#
#source(file.path("./code","ioConstants.R"))
#print("Chart Processing Method Launched")
#
#ProcessData <- function(in.dir, out.dir,title) {
#	#Read all the CSVs from a directory
#	temp <- list.files(in.dir,pattern=".*\\.csv")
#	for (i in 1:length(temp)) assign(temp[i], read.csv(file.path(in.dir,temp[i])))
#	
#	#Gather the number of clusters for each year and algorithm
#	cluster.list <- matrix(0,ncol=length(temp),nrow=length(eval(parse(text=temp[1]))$Clusters))
#	for (i in 1:length(temp)) {
#		cluster.list[,i] <- eval(parse(text=temp[i]))$Clusters
#	}
#	
#	#Gather the percentage of imbalance for each year and algorithm
#	imbalance.list <- matrix(0,ncol=length(temp),nrow=length(eval(parse(text=temp[1]))$Total_Imb_Pctg))
#	rownames(imbalance.list) <- c("Pos Infomap", "Comp Neg Infomap", "Pos Multilevel", "Comp Neg Multilevel", 
#			"Pos FastGreedy", "Comp Neg FastGreedy", "Pos WalkTrap", "Comp Neg WalkTrap","Parallel ILS")
## edit by VL 13/09/2015
##	lgd <- c("2009","2010","2011","2012","2013","Term")
#	lgd <- substr(temp, start=nchar(title)+2, stop=nchar(title)+5)
## end of edit
#	colnames(imbalance.list) <- lgd
#	for (i in 1:length(temp)) {
#		imbalance.list[,i] <- eval(parse(text=temp[i]))$Total_Imb_Pctg
#	}
#	
#	# -X-X-X-X-X-X- Code to Generate the Plot -X-X-X-X-X-X-
#	
#	#Call the function jpeg to save the data
#	pdf(width=10,height=7,file=file.path(out.dir ,paste0(title,".pdf")))
#	
#	#Adjust the Margin of the plot for the names to appear. Default = c(5,4,4,2) + 0.1
#	par(mar = c(10,4,4,2) + 0.1)
#	
#	#Plot the data
#	xx <- barplot(t(imbalance.list), beside=T,col=rainbow(length(lgd)), las=3,ylim=c(0,80),cex.names=1)  
#	
#	mtext(side=3,title,line=1,cex=2)
#	mtext(side=3,"Whole term",line=-1,cex=1.5)
#	
#	#Add the Legend at the top right
#	legend("topright", 
#			legend = lgd, 
#			fill = rainbow(length(lgd)), ncol = 2,
#			cex = 1)
#	
#	#Add the number of clusters above each bar
#	text(x = xx, y = unlist(as.list(t(imbalance.list))), label = unlist(as.list(t(cluster.list))), pos = 3, cex = 0.8, col = "blue")
#	
#	#Turn of the ploting device
#	dev.off()
#	
#}
#
#sub.dirs <- list.files(output.community.csv.dir)
#for(i in 1:length(sub.dirs)) {
#	try({
#				input.dir <- paste0(output.community.csv.dir,"/",sub.dirs[i])
#				output.dir <- paste0(output.plots.dir,"/",sub.dirs[i])
#				ProcessData(input.dir,output.dir,sub.dirs[i])
#			})
#}
