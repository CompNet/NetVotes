source(file.path("./code","ioConstants.R"))
source(file.path("./code","preprocessing.R"))
source(file.path("./code","defaults.R"))
print("Initializating the Code")

################# Functions #################

# Verifies that the user entered correctly the variables in file ".\input_files" 
verif_input_var <- function(var,varname,default=NULL,data_var,file){
  lvls <- levels(factor(data_var))
  if(!is.null(default))
    lvls <- c(lvls,default)
  if(	sum(var %in% lvls) != length(var))
    stop(paste0("\nIn file ",file," , incorrect variable ",varname," = ( ",toString(var)," ) . \n\nHere are the different possibilities : ",toString(lvls)))
  
}

#Check if the inputs are correct
CheckInputs <- function(){  
  verif_input_var(MP.political_groups,"MP.political_groups",default="all"      ,MPs$political_group                            ,file)
  verif_input_var(MP.countries       ,"MP.countries"       ,default="all"      ,MPs$country                     ,file)
  verif_input_var(MP.mp_ids          ,"MP.mp_ids"          ,default="all"      ,MPs$id                               ,file)
  verif_input_var(MP.group_by        ,"MP.group_by"        ,default="DontGroup",factor(c("political_group","country")),file)## dirty : better place in config.R all the names of the possible aggregateion variables
  verif_input_var(DOCS.policies      ,"DOCS.policies"      ,default="all"      ,docs$Policy.area                      ,file)
  
  
  if(length(DOCS.time_limits) != 2)
    stop(paste0("In file : ",file," :invalid variable DOCS.time_limits : DOCS.time_limits = ( ",toString(DOCS.time_limits)," )\nPlease specify exactly 2 dates"))
  if(is.na(as.Date(DOCS.time_limits[1],format="%d/%m/%Y")))
    stop(paste0("In file : ",file," :invalid variable DOCS.time_limits : DOCS.time_limits = ( ",toString(DOCS.time_limits)," )\nThe first date must be of this format : 'dd/mm/YYYY'"))
  if(is.na(as.Date(DOCS.time_limits[2],format="%d/%m/%Y")))
    stop(paste0("In file : ",file," :invalid variable DOCS.time_limits : DOCS.time_limits = ( ",toString(DOCS.time_limits)," )\nThe second date must be of this format : 'dd/mm/YYYY'"))
  
  
  choices <- c("DontGroup","majority_vote_in_each_group","min_each_vote_possibility_between_groups","avg_agreement_between_MPs_of_each_group")
  if(!MP.group_by.mode %in% choices)
    stop(paste0("In file : ",file,", variable MP.group_by.mode must be one of these: ",toString(choices)))
  
  
  choices <- c("%agree-%disagree","+1_or_-1")
  if(!OUTPUTFILES.Gfile.weigth %in% choices)
    stop(paste0("In file : ",file,", variable OUTPUTFILES.Gfile.weigth must be one of these: ",toString(choices)))
  
  
  if (alpha<0 | alpha>1)
    stop(paste0("In file : ",file," alpha must be in the interval [0;1]. Please modify it in file ",file ))
  
  dontGroup <- 0
  if("DontGroup" %in% MP.group_by){
    dontGroup <- 1
    MP.group_by <- "DontGroup"
    MP.group_by.mode <- "DontGroup"
  }
  if(MP.group_by.mode == "DontGroup" & dontGroup==0)	
    stop(paste0("In file : ",file,", as MP.group_by is not 'DontGroup', please select a MP.group_by.mode which is not 'DontGroup'.")) 
}

FilterUsingConfigurations <- function() {
 
  # Filtering of the documents using the parameters passed through the configuration file
  
  # Adding columns for DAY,MONTH and YEAR from the column "Date"
  dates <- as.Date(docs$Date,format="%d/%m/%Y")
  if(as.integer(format(dates,format="%Y"))[1]<100)
    docs$YEAR  <- as.integer(format(dates,format="%Y"))+2000
  docs$MONTH <- as.integer(format(dates,format="%m"))
  docs$DAY   <- as.integer(format(dates,format="%d"))
  docs$Date  <- as.Date(paste(docs$DAY,docs$MONTH,docs$YEAR,sep="/"),format="%d/%m/%Y")
  
  # Selection of the Documents chosen by the user
  if("all" %in% DOCS.policies) DOCS.policies  <- levels(factor(docs$Policy.area))
  DOCS.time_limits <- as.Date(DOCS.time_limits,format="%d/%m/%Y") # convertion from string to Date
  selected_Documents <- docs[which( docs$Date>=DOCS.time_limits[1] & docs$Date<=DOCS.time_limits[2] & docs$Policy.area %in% DOCS.policies),]
    
  # Filtering concerning MPs
  
  # Selection of the MPs chosen by the user
  
  if("all" %in% MP.political_groups) MP.political_groups    <- levels(factor(MPs$political_group))
  if("all" %in% MP.countries       ) MP.countries           <- levels(factor(MPs$country))
  if("all" %in% MP.mp_ids          ) MP.mp_ids              <- levels(factor(MPs$id))
  selected_MPs <- MPs[which(MPs$political_group %in% MP.political_groups & MPs$country %in% MP.countries & MPs$id %in% MP.mp_ids),]
  
  # Generate a filtered table using the parameters passed by the user
  filtered.table <- big.table[which(big.table$mp_id %in% selected_MPs$id),selected_Documents$doc_id]
  
  if(! is.null(dim(filtered.table))){
    if(!(dim(filtered.table)[1]>1 && dim(filtered.table)[2]>1)){
      warning(paste0("In file ",file," , the data is too much filtered (less than 2 MPs or less than 2 documents). Please change the filtering settings in ",file,"\n(No graph generated)"))
      too.much.filtered <- TRUE
    }
  }else{
    warning(paste0("In file ",file," , the data is too much filtered (less than 2 MPs or less than 2 documents). Please change the filtering settings in ",file,"\n(No graph generated)"))
    too.much.filtered <- TRUE
  }
  
  return(filtered.table)
}

FilterUsingConfigurationsLoyalty <- function() {
  
  # Filtering of the documents using the parameters passed through the configuration file
  
  # Adding columns for DAY,MONTH and YEAR from the column "Date"
  dates <- as.Date(docs$Date,format="%d/%m/%Y")
  if(as.integer(format(dates,format="%Y"))[1]<100)
    docs$YEAR  <- as.integer(format(dates,format="%Y"))+2000
  docs$MONTH <- as.integer(format(dates,format="%m"))
  docs$DAY   <- as.integer(format(dates,format="%d"))
  docs$Date  <- as.Date(paste(docs$DAY,docs$MONTH,docs$YEAR,sep="/"),format="%d/%m/%Y")
  
  # Selection of the Documents chosen by the user
  if("all" %in% DOCS.policies) DOCS.policies  <- levels(factor(docs$Policy.area))
  DOCS.time_limits <- as.Date(DOCS.time_limits,format="%d/%m/%Y") # convertion from string to Date
  selected_Documents <- docs[which( docs$Date>=DOCS.time_limits[1] & docs$Date<=DOCS.time_limits[2] & docs$Policy.area %in% DOCS.policies),]
  
  
  # Filtering concerning MPs
  
  # Selection of the MPs chosen by the user
  
  if("all" %in% MP.political_groups) MP.political_groups    <- levels(factor(MPs$political_group))
  if("all" %in% MP.countries       ) MP.countries           <- levels(factor(MPs$country))
  if("all" %in% MP.mp_ids          ) MP.mp_ids              <- levels(factor(MPs$id))
  selected_MPs <- MPs[which(MPs$political_group %in% MP.political_groups & MPs$country %in% MP.countries & MPs$id %in% MP.mp_ids),]   
  
  # Generate a filtered table using the parameters passed by the user
  filtered.table <- loyalty.table[which(loyalty.table$mp_id %in% selected_MPs$id),selected_Documents$doc_id]
  
  if(! is.null(dim(filtered.table))){
    if(!(dim(filtered.table)[1]>1 && dim(filtered.table)[2]>1)){
      warning(paste0("In file ",file," , the data is too much filtered (less than 2 MPs or less than 2 documents). Please change the filtering settings in ",file,"\n(No graph generated)"))
      too.much.filtered <- TRUE
    }
  }else{
    warning(paste0("In file ",file," , the data is too much filtered (less than 2 MPs or less than 2 documents). Please change the filtering settings in ",file,"\n(No graph generated)"))
    too.much.filtered <- TRUE
  }
  
  return(filtered.table)
}

generateGraph <- function(vetor, agreementValues){
  pass <- upper.tri(agreementValues)                     
  for(i in 1:nrow(agreementValues)){
    for(j in 1:ncol(agreementValues)){
      if(pass[i, j] == TRUE){
          edge <- c(i,j,(2*(agreementValues[i,j]>0)-1),agreementValues[i,j])
          vetor <- rbind(vetor,edge)     
      }
    }
  }
  
  vetor <- vetor[-1,]
  colnames(vetor) <- NULL
  rownames(vetor) <- NULL
  colnames(vetor) <- c("Source", "Target", "Signal", "Weight")
  return(vetor)
}

generateGraphG <- function(agreementValues){
  number <- ((nrow(agreementValues)*nrow(agreementValues))-nrow(agreementValues))/2
  retorno <- matrix(0,nrow=number,ncol=3)
  count <- 1
  pass <- upper.tri(agreementValues)                     
  for(i in 1:nrow(agreementValues)){
    for(j in 1:ncol(agreementValues)){
      if(pass[i, j] == TRUE){
        if(agreementValues[i, j]>=top || agreementValues[i,j]<=floor) {
          retorno[count,1] <- i
          retorno[count,2] <- j
          retorno[count,3] <- round(agreementValues[i,j],6)
          count <- count+1
        }
      }
    }
  }
  
  colnames(retorno) <- c("Source", "Target", "Weight")
  
  return(retorno)
}


WriteGFile <- function(nrow,d,filename){
  write.table(data.frame(a=nrow,b=nrow(d)),file=file.path(filename), row.names = FALSE,sep=" ",col.names =FALSE,eol="\n")
  write.table(d[,c(1,2,3)],file=file.path(filename), row.names = FALSE,sep="\t",col.names =FALSE,eol="\n",append=TRUE)
}

calculateAgreement <- function(vetor) {
  temp <- matrix(0,nrow=nrow(vetor),ncol=nrow(vetor))
  rownames(temp) <- rownames(vetor)
  agreement <- 0
  agr <- 0
  points <- vector(length=ncol(vetor))
  col.size = ncol(vetor)
  
  if(selected.table==2) {
    for(i in 1:(nrow(vetor)-1)){
       for(j in (i+1):nrow(vetor)){
          agr <- paste(vetor[i,],vetor[j,],sep="")      #Compare Two-by-Two
          points <- sapply(agr,function(s) table2[[s]])    #Apply the Function to substitue the values
          agreement <- sum(unlist(points))              #Sum the values
          agreement <- agreement/col.size               #Normalize by the number of documents
          temp[i, j] <- agreement                       #Put the value in the adjacency matrix
          temp[j, i] <- agreement                       #Put the value in the adjacency matrix
      }
    }
  }else {
    for(i in 1:(nrow(vetor)-1)){
      for(j in (i+1):nrow(vetor)){
        agr <- paste(vetor[i,],vetor[j,],sep="")      #Compare Two-by-Two
        points <- sapply(agr,function(s) table1[[s]])    #Apply the Function to substitue the values
        agreement <- sum(unlist(points))              #Sum the values
        agreement <- agreement/col.size               #Normalize by the number of documents
        temp[i, j] <- agreement                       #Put the value in the adjacency matrix
        temp[j, i] <- agreement                       #Put the value in the adjacency matrix
      }
    }
  }
  
  return(temp)
  
}


calculateLoyalty <- function(vetor) {
  temp <- matrix(0,nrow=nrow(vetor),ncol=nrow(vetor))
  rownames(temp) <- rownames(vetor)
  agreement <- 0
  agr <- 0
  points <- vector(length=ncol(vetor))
  col.size = ncol(vetor)
  
 for(i in 1:(nrow(vetor)-1)){
      for(j in (i+1):nrow(vetor)){
        points <- vetor[i,]*vetor[j,]        
        agreement <- sum(points)
        agreement <- agreement/col.size               #Normalize by the number of documents
        temp[i, j] <- agreement                       #Put the value in the adjacency matrix
        temp[j, i] <- agreement                       #Put the value in the adjacency matrix
      }
    }
  return(temp)
  
}


AgreementPlot <- function(agreementMatrix) {
  tempo <- agreementMatrix
  tempo[lower.tri(tempo,diag=TRUE)] <- NA
  tempo <- round(tempo,2)
  
  pdf(file=file.path(output.dir,"agreement_distribution.pdf"))

  barplot(table(tempo),ylim=c(0,30000), xpd=FALSE, xlab="Agreement",ylab="Frequency",col=1:20,main="Agreement Distribution")
  dev.off()
  
  pdf(file=file.path(output.dir,"agreement_histogram.pdf"))
  hist(tempo, xlab="Agreement",ylab="Frequency",col=1:20,main="Agreement Distribution")
  dev.off()
}

RebelionPlot <- function(rebelion.indexes) {
  pdf(file=file.path(output.dir,"rebelion_histogram.pdf"))
  hist(rebelion.indexes, xlab="Rebelion",ylab="Frequency",col=1:20,main="Rebelion Index")
  dev.off()
}

# Writes a .g file
createGFile <- function(numberNodes,d,filename){
  write.table(data.frame(a=numberNodes,b=nrow(d)),file=file.path(filename), row.names = FALSE,sep=" ",col.names =FALSE,eol="\n")
  
  if(OUTPUTFILES.Gfile.weigth != "+1_or_-1"){
    write.table(d[,c(1,2,3)],file=file.path(filename), row.names = FALSE,sep="\t",col.names =FALSE,eol="\n",append=TRUE)
  }else{
    write.table(d[,c(1,2,4)],file=file.path(filename), row.names = FALSE,sep="\t",col.names =FALSE,eol="\n",append=TRUE)
  }
}

generateEdgesGephi <- function(edges,filename,directed=FALSE) {
  tab <- as.data.frame(edges)
  write.table(tab,file.path(filename), row.names = FALSE,sep=",",dec=".")  # dirty  
}

generateNodesGephi <- function(nodes.table,filename) {
  write.csv(nodes.table,file=filename, row.names = FALSE)		
}

ConvertMatrix <- function(matrix) {
  
  loyalty.values <- list("Absent"=NA,"Rebel"=1,"Loyal"=0,"Didn't vote"=NA,"Documented Absence"=NA,"NA"=NA,"Independent"=NA)
  
  matrix[sapply(matrix,is.null)] <- NA
  reply <- mapply(function(s) loyalty.values[[s]],matrix)
  reply[sapply(reply, is.null)] <- NA
  reply <- unlist(reply)
  
  reply <- matrix(data = reply, nrow=nrow(matrix), ncol=ncol(matrix))
  
  return(reply)
}

CalculateRebelionIndex <- function(matrix) {
  converted.matrix <- ConvertMatrix(matrix)
  reply <- rowSums(converted.matrix,na.rm = TRUE) #Sum the values of each row and store in a vector ignoring NA values
  number_documents <- apply(converted.matrix, 1, function(x) length(which(!is.na(x)))) #check how many documents are valid for the normalization
  reply <- reply / number_documents #Find the percentage of rebelion for each candidate (only for valid documents)
  reply[which(is.nan(reply))] <- 0
  return(reply)
}

RemoveMePs <- function(matrix) {
  go.values <- list("Absent"=1,"Abstain"=2,"Against"=3,"Didn't vote"=4,"For"=5,"NA"=0)
  back.values <- list("1"="Absent","2"="Abstain","3"="Against","4"="Didn't vote","5"="For","0"="NA")
  reply <- mapply(function(s) go.values[[s]],matrix)

  reply[sapply(reply, is.null)] <- NA
  reply <- unlist(reply)
  reply <- matrix(reply,ncol=ncol(matrix))
  rownames(reply) <- rownames(matrix)
  reply <- reply[which(rowSums(reply,na.rm=TRUE)!=0),]
  
  rabs <- mapply(function(s) back.values[[s]],reply)
  rabs <- matrix(rabs,ncol=ncol(matrix))
  rownames(rabs) <- rownames(reply)
  colnames(rabs) <- colnames(matrix)
  return(rabs)
}

CalculateCCImbalance <- function(adjacency.matrix, cluster) {
  
  sum.intern <- 0 
  sum.extern <- 0
  
  temp <- adjacency.matrix
  temp[lower.tri(temp,diag=TRUE)] <- 0
  
  #Edges intra-clusters
  for(i in 1:nrow(adjacency.matrix)){
    clust.i <- cluster[i]
    a <- temp[i,which(cluster == clust.i)]
    sum.intern <- sum.intern + sum(a[which(temp[i,which(cluster == clust.i)] < 0)])    
  }
  
  #Edges inter-clusters
  for(j in 1:nrow(adjacency.matrix)){
    clust.j <- cluster[j]
    a <- temp[j,which(cluster != clust.j)]
    sum.extern <- sum.extern + sum(a[which(temp[j,which(cluster != clust.j)] > 0)])
  }
  
  sum.intern <- abs(sum.intern)
  sum.extern <- abs(sum.extern)
  
  imbalance.sum <- sum.intern + sum.extern
  edges.sum <- sum(abs(temp))
  positive.sum <- sum(temp[which(temp > 0)])
  negative.sum <- abs(sum(temp[which(temp < 0)]))
  
  imbalance.percentage <- imbalance.sum / edges.sum #percentage of imbalance considering the sum of all edges of the graph
  negative.percentage <- sum.intern / imbalance.sum #percentage of negative edges that contributes for the imbalance
  positive.percentage <- sum.extern / imbalance.sum #percentage of positive edges that contributes for the imbalance
  
  imbalance <- list("total.imbalance" = round(imbalance.sum,2), "total.imbalance.percentage" = (100*round(imbalance.percentage,2)),
                    "positive.imbalance" = round(sum.extern,2), "positive.imbalance.percentage" = (100*round(positive.percentage,2)),
                    "negative.imbalance" = round(sum.intern,2), "negative.imbalance.percentage" = (100*round(negative.percentage,2))) 
  
  return(imbalance)
}

 
CalculateNetworkMeasures <- function(adjacency.matrix) {
  temp <- adjacency.matrix
  pos.matrix <- adjacency.matrix
  neg.matrix <- adjacency.matrix
  
  pos.matrix[which(pos.matrix < 0)] <- 0
  neg.matrix[which(neg.matrix > 0)] <- 0
  
  pos.matrix[lower.tri(pos.matrix)] <- 0
  neg.matrix[lower.tri(neg.matrix)] <- 0
  
  pos.qtd.edges <- sum(ifelse(pos.matrix != 0,1,0))
  neg.qtd.edges <- sum(ifelse(neg.matrix != 0,1,0))
  
  pos.wght.edges <- sum(pos.matrix)
  neg.wght.edges <- abs(sum(neg.matrix))
  
  total.qtd.edges <- pos.qtd.edges + neg.qtd.edges
  total.wght.edges <- pos.wght.edges + abs(neg.wght.edges)
    
  pos.pctg.edges <- pos.qtd.edges/total.qtd.edges
  pos.pctg.weight <- pos.wght.edges/total.wght.edges
  
  neg.pctg.edges <- neg.qtd.edges/total.qtd.edges
  neg.pctg.weight <- neg.wght.edges/total.wght.edges
  
  edge.measures <- matrix(0,ncol=2,nrow=3)
  edge.measures[1,] <- c(total.qtd.edges,100)
  edge.measures[2,] <- c(pos.qtd.edges,(100*pos.pctg.edges))
  edge.measures[3,] <- c(neg.qtd.edges,(100*neg.pctg.edges))
  colnames(edge.measures) <- c("Total", "Percentage")
  rownames(edge.measures) <- c("All Edges", "Positive Edges", "Negative Edges")
  
  weight.measures <- matrix(0,ncol=2,nrow=3)
  weight.measures[1,] <- c(total.wght.edges,100)
  weight.measures[2,] <- c(pos.wght.edges,(100*pos.pctg.weight))
  weight.measures[3,] <- c(neg.wght.edges,(100*neg.pctg.weight))
  colnames(weight.measures) <- c("Total", "Percentage")
  rownames(weight.measures) <- c("All Edges", "Positive Edges", "Negative Edges")
  
  network.measures <- list("edges" = round(edge.measures,2), "weights" = round(weight.measures,2))
  
  return(network.measures)
}

RemoveMePsLoyal <- function(matrix) {
  go.values <- list("Absent"=1,"Rebel"=2,"Loyal"=3,"Didn't vote"=4,"Documented Absence"=5,"NA"=0,"Independent"=6)
  back.values <- list("1"="Absent","2"="Rebel","3"="Loyal","4"="Didn't vote","5"="Documented Absence","6"="Independent","0"="NA")
  reply <- mapply(function(s) go.values[[s]],matrix)
  
  reply[sapply(reply, is.null)] <- NA
  reply <- unlist(reply)
  
  reply <- matrix(reply,ncol=ncol(matrix))
  rownames(reply) <- rownames(matrix)
  reply <- reply[which(rowSums(reply,na.rm=TRUE)!=0),]
  
  rabs <- mapply(function(s) back.values[[s]],reply)
  rabs <- matrix(rabs,ncol=ncol(matrix))
  rownames(rabs) <- rownames(reply)
  colnames(rabs) <- colnames(matrix)
  return(rabs)
}

CalculateClusterization <- function(adjacency.matrix,more.filtered.table,rebelion.indexes) {
  
  gc <- graph.adjacency(adjacency.matrix, mode = c("upper"), weighted = TRUE, diag = FALSE, add.rownames = TRUE)
  
  positive.adjacency.matrix <- adjacency.matrix
  positive.adjacency.matrix[which(positive.adjacency.matrix < 0)] <- 0
  positive.adjacency.matrix <- round(positive.adjacency.matrix,2)
  gp <- graph.adjacency(positive.adjacency.matrix, mode = c("upper"), weighted = TRUE, diag = FALSE, add.rownames = TRUE)
  
  negative.adjacency.matrix <- adjacency.matrix
  negative.adjacency.matrix[which(negative.adjacency.matrix > 0)] <- 0
  negative.adjacency.matrix <- round(negative.adjacency.matrix,2)
  
  complementary.negative.adjacency.matrix <- negative.adjacency.matrix
  complementary.negative.adjacency.matrix[which(negative.adjacency.matrix >= 0)] <- 1
  complementary.negative.adjacency.matrix[which(negative.adjacency.matrix < 0)] <- 0
  gcm <- graph.adjacency(complementary.negative.adjacency.matrix, mode = c("upper"), weighted = TRUE, diag = FALSE, add.rownames = TRUE)

  info.gp <- infomap.community(gp)  
  mult.gp <- multilevel.community(gp)
  fast.gp <- fastgreedy.community(gp)
  walk.gp <- walktrap.community(gp, steps = 10)
  betw.gp <- betweenness.estimate(gp, directed = FALSE, cutoff = 0)
  
  info.cmp <- infomap.community(gcm)
  mult.cmp <- multilevel.community(gcm)
  fast.cmp <- fastgreedy.community(gcm)
  walk.cmp <- walktrap.community(gcm, steps = 10)
  
  imb.info.positive.graph <- CalculateCCImbalance(adjacency.matrix,info.gp$membership)
  imb.mult.positive.graph <- CalculateCCImbalance(adjacency.matrix,mult.gp$membership)
  imb.fast.positive.graph <- CalculateCCImbalance(adjacency.matrix,membership(fast.gp))
  imb.walk.positive.graph <- CalculateCCImbalance(adjacency.matrix,membership(walk.gp))
  
  imb.info.negative.complementary.graph <- CalculateCCImbalance(adjacency.matrix,info.cmp$membership)
  imb.mult.negative.complementary.graph <- CalculateCCImbalance(adjacency.matrix,mult.cmp$membership)
  imb.fast.negative.complementary.graph <- CalculateCCImbalance(adjacency.matrix,membership(fast.cmp))
  imb.walk.negative.complementary.graph <- CalculateCCImbalance(adjacency.matrix,membership(walk.cmp))
  
  imbalance <- matrix(0,nrow=8,ncol=7)
  imbalance[1,1] <- length(communities(info.gp))
  imbalance[2,1] <- length(communities(info.cmp))
  imbalance[3,1] <- length(communities(mult.gp))
  imbalance[4,1] <- length(communities(mult.cmp))
  imbalance[5,1] <- length(communities(fast.gp))
  imbalance[6,1] <- length(communities(fast.cmp))
  imbalance[7,1] <- length(communities(walk.gp))
  imbalance[8,1] <- length(communities(walk.cmp))
  
  imbalance[1,2:7] <- unlist(imb.info.positive.graph)
  imbalance[2,2:7] <- unlist(imb.info.negative.complementary.graph)
  imbalance[3,2:7] <- unlist(imb.mult.positive.graph)
  imbalance[4,2:7] <- unlist(imb.mult.negative.complementary.graph)
  imbalance[5,2:7] <- unlist(imb.fast.positive.graph)
  imbalance[6,2:7] <- unlist(imb.fast.negative.complementary.graph)
  imbalance[7,2:7] <- unlist(imb.walk.positive.graph)
  imbalance[8,2:7] <- unlist(imb.walk.negative.complementary.graph)
  
  colnames(imbalance) <- c("Clusters", "Total_Imb", "Total_Imb_Pctg", "Pos_Imb", "Pos_Imb_Pctg", "Neg_Imb", "Neg_Imb_Pctg")
  rownames(imbalance) <- c("Positive Graph InfoMap", "Complementary Negative Graph InfoMap",
                       "Positive Graph MultiLevel", "Complementary Negative Graph MultiLevel",
                       "Positive Graph FastGreedy", "Complementary Negative Graph FastGreedy",
                       "Positive Graph WalkTrap", "Complementary Negative Graph WalkTrap")

  vi.info <- compare.communities(info.gp,info.cmp, method = c("vi"))
  vi.mult <- compare.communities(mult.gp,mult.cmp, method = c("vi"))
  vi.fast <- compare.communities(fast.gp,fast.cmp, method = c("vi"))
  vi.walk <- compare.communities(walk.gp,walk.cmp, method = c("vi"))

  nmi.info <- compare.communities(info.gp,info.cmp, method = c("nmi"))
  nmi.mult <- compare.communities(mult.gp,mult.cmp, method = c("nmi"))
  nmi.fast <- compare.communities(fast.gp,fast.cmp, method = c("nmi"))
  nmi.walk <- compare.communities(walk.gp,walk.cmp, method = c("nmi"))

  spjn.info <- compare.communities(info.gp,info.cmp, method = c("split.join"))
  spjn.mult <- compare.communities(mult.gp,mult.cmp, method = c("split.join"))
  spjn.fast <- compare.communities(fast.gp,fast.cmp, method = c("split.join"))
  spjn.walk <- compare.communities(walk.gp,walk.cmp, method = c("split.join"))

  rand.info <- compare.communities(info.gp,info.cmp, method = c("rand"))
  rand.mult <- compare.communities(mult.gp,mult.cmp, method = c("rand"))
  rand.fast <- compare.communities(fast.gp,fast.cmp, method = c("rand"))
  rand.walk <- compare.communities(walk.gp,walk.cmp, method = c("rand"))

  adrand.info <- compare.communities(info.gp,info.cmp, method = c("adjusted.rand"))
  adrand.mult <- compare.communities(mult.gp,mult.cmp, method = c("adjusted.rand"))
  adrand.fast <- compare.communities(fast.gp,fast.cmp, method = c("adjusted.rand"))
  adrand.walk <- compare.communities(walk.gp,walk.cmp, method = c("adjusted.rand"))

  comparison <- matrix(0,ncol=5,nrow=4)
  comparison[1,] <- c(vi.info,nmi.info,spjn.info,rand.info,adrand.info)
  comparison[2,] <- c(vi.mult,nmi.mult,spjn.mult,rand.mult,adrand.mult)
  comparison[3,] <- c(vi.fast,nmi.fast,spjn.fast,rand.fast,adrand.fast)
  comparison[4,] <- c(vi.walk,nmi.walk,spjn.walk,rand.walk,adrand.walk)

  colnames(comparison) <- c("VI", "NMI", "Split Join", "Random", "Adjusted Random")
  rownames(comparison) <- c("InfoMap (Positve and Complementary Negative)", "MultiLevel (Positve and Complementary Negative)",
                            "FastGreedy (Positve and Complementary Negative)", "WalkTrap (Positve and Complementary Negative)")
                         

  reply <- list("cluster.information" = imbalance, "cluster.comparison" = comparison)

  a <- rownames(more.filtered.table)
  names <- as.character(MPs[a,2]) #Get the names of each selected MeP
  countries <- as.character(MPs[a,3]) #Get the country of each selected MeP
  political_groups <- as.character(MPs[a,4]) #Get the political group of each selected MeP
  V(gc)$name <- names
  V(gc)$country <- countries
  V(gc)$political_group <- political_groups
  V(gc)$rebelion_index <- rebelion.indexes

  V(gc)$infomap_positive_community <- info.gp$membership
  V(gc)$multilevel_positive_community <- mult.gp$membership
  V(gc)$fastgreedy_positive_community <- membership(fast.gp)
  V(gc)$walktrap_positive_community <- membership(walk.gp)
  
  V(gc)$infomap_comp_neg_community <- info.cmp$membership
  V(gc)$multilevel_comp_neg_community <- mult.cmp$membership
  V(gc)$fastgreedy_comp_neg_community <- membership(fast.cmp)
  V(gc)$walktrap_comp_neg_community <- membership(walk.cmp)

  write.graph(graph = gc, file = file.path(output.dir,"complete_graph.graphml"),format = "graphml")
  
  return(reply)
}



Calculate <- function() {
  vetor <- 0
  too.much.filtered <- FALSE
  CheckInputs()
  tabela.filtrada <- FilterUsingConfigurations()
  tabela.matrizada <- as.matrix(tabela.filtrada)

  corresp.table <- data.frame(id=(0:(nrow(tabela.filtrada)-1)), name=rownames(tabela.filtrada))
  corresp.table <- cbind(corresp.table, MPs[as.character(corresp.table[,"name"]),c("names","country","political_group")])
  rownames(corresp.table) <- corresp.table$id

  loyalty.table <- FilterUsingConfigurationsLoyalty()
  loyalty.matrix<- as.matrix(loyalty.table)

  more.filtered.table <- RemoveMePs(tabela.matrizada)
  more.filtered.loyalty <- RemoveMePsLoyal(loyalty.matrix)
  
  rebelion.indexes <- CalculateRebelionIndex(more.filtered.loyalty)

  agreementMatrix <- calculateAgreement(more.filtered.table)
  graph <- generateGraphG(agreementMatrix)
  fileName <- file.path(output.dir,"graph.g")
  WriteGFile(nrow(agreementMatrix),graph,fileName)

  AgreementPlot(agreementMatrix)
  RebelionPlot(rebelion.indexes)

  fileName <- file.path(output.dir, "edges_Gephi.csv")
  generateEdgesGephi(graph,fileName)
  
  fileName <- file.path(output.dir, "nodes_Gephi.csv")
  generateNodesGephi(corresp.table,fileName)

  net.measures <- CalculateNetworkMeasures(agreementMatrix)
  nome.file <- file.path(output.dir,"net_measures.csv")
  write.csv(net.measures,file=nome.file)    

  clustering.data <- CalculateClusterization(agreementMatrix,more.filtered.table,rebelion.indexes)
  
  nome.file <- file.path(output.csv.dir,paste0(file.title,"_cluster_information.csv"))
  write.csv(clustering.data$cluster.information,file=nome.file)    
  nome.file <- file.path(output.dir,"cluster_comparison.csv")
  write.csv(clustering.data$cluster.comparison,file=nome.file)  	
}

##### Main Program #####
library(igraph)

# scores associated to each possible pair of votes
table1 <- list(ForFor=1,ForAgainst=-1,ForAbstain=-0.5,
               AgainstFor=-1,AgainstAgainst=1,AgainstAbstain=-0,5,
               AbstainFor=-0.5,AbstainAgainst=-0.5,AbstainAbstain=0.5)

table2 <- list(ForFor=1,ForAgainst=-1,ForAbstain=0,
               AgainstFor=-1,AgainstAgainst=1,AgainstAbstain=0,
               AbstainFor=0,AbstainAgainst=0,AbstainAbstain=1)

top <- 0
floor<- 0

sub.dirs <- list.files(input.dir)
for(i in 1:length(sub.dirs)) {
  adapt <- paste0(input.dir,"/",sub.dirs[i])
  sub.sub.dirs <- list.files(adapt)
  for(j in 1:length(sub.sub.dirs)) {
    rooty <- paste0(adapt,"/")
    try({
    source(file.path(rooty,sub.sub.dirs[j]))    
    print(paste0("Processing Directory: ",sub.dirs[i]))
    print(paste0("Configuration File: ",sub.sub.dirs[j]))
    Calculate()
    print("Process Complete")
    }
    )
  }
}

