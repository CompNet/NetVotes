#############################################################################################
# Main script.
# This script launches the whole process:
# - Load the raw data
# - Preprocess/filter the resulting tables
# - Process the voting agreement index and other statistics
# - Extract the collection of networks
# - Process various network statistics
# - Apply the community detection algorithms
# - Compare the resulting partitions, also with the pILS results if available.
# - Incidentally generate plots related to certain of these steps
# 
# The parameters located at the beginning of the script allow to control it,
# and to restrict the focus to certain topics/years, or control certain points
# of the network extraction.
# 
# 07/2015 Israel Mendonça (v1)
# 09/2015 Vincent Labatut (v2)
#############################################################################################

# Verifies that the user entered correctly the variables in file ".\input_files" 
VerifyInputVariables <- function(var, varname, data_var, file, default=NULL){
	lvls <- levels(factor(data_var))
	if(!is.null(default))
		lvls <- c(lvls,default)
	if(  sum(var %in% lvls) != length(var))
		stop(paste0("\nIn file ",file," , incorrect variable ", varname," = ( ",toString(var)," ) . \n\nHere are the different possibilities : ",toString(lvls)))
}

#Check if the inputs are correct
CheckInputs <- function(){  
	VerifyInputVariables(MP.political_groups, "MP.political_groups", MPs$political_group                   , file, default="all")
	VerifyInputVariables(MP.countries       , "MP.countries"       , MPs$country                           , file, default="all")
	VerifyInputVariables(MP.mp_ids          , "MP.mp_ids"          , MPs$id                                , file, default="all")
	VerifyInputVariables(MP.group_by        , "MP.group_by"        , factor(c("political_group","country")), file, default="DontGroup")
	VerifyInputVariables(DOCS.policies      , "DOCS.policies"      , docs$Policy.area                      , file, default="all")
	
	if(length(DOCS.time_limits) != 2)
		stop(paste0("In file : ",file," :invalid variable DOCS.time_limits : DOCS.time_limits = ( ", toString(DOCS.time_limits), " )\nPlease specify exactly 2 dates"))
	if(is.na(as.Date(DOCS.time_limits[1], format="%d/%m/%Y")))
		stop(paste0("In file : ", file, " :invalid variable DOCS.time_limits : DOCS.time_limits = ( ", toString(DOCS.time_limits), " )\nThe first date must be of this format : 'dd/mm/YYYY'"))
	if(is.na(as.Date(DOCS.time_limits[2], format="%d/%m/%Y")))
		stop(paste0("In file : ", file, " :invalid variable DOCS.time_limits : DOCS.time_limits = ( ", toString(DOCS.time_limits), " )\nThe second date must be of this format : 'dd/mm/YYYY'"))
	
	choices <- c("DontGroup","majority_vote_in_each_group","min_each_vote_possibility_between_groups","avg_agreement_between_MPs_of_each_group")
	if(!MP.group_by.mode %in% choices)
		stop(paste0("In file : ",file,", variable MP.group_by.mode must be one of these: ", toString(choices)))
	
	choices <- c("%agree-%disagree", "+1_or_-1")
	if(!OUTPUTFILES.Gfile.weigth %in% choices)
		stop(paste0("In file : ", file, ", variable OUTPUTFILES.Gfile.weigth must be one of these: ", toString(choices)))
	
	
	if (alpha<0 | alpha>1)
		stop(paste0("In file : ",file," alpha must be in the interval [0;1]. Please modify it in file ", file))
	
	dontGroup <- 0
	
	if("DontGroup" %in% MP.group_by) {
		dontGroup <- 1
		MP.group_by <- "DontGroup"
		MP.group_by.mode <- "DontGroup"
	}
	
	if(MP.group_by.mode == "DontGroup" & dontGroup==0)	
		stop(paste0("In file : ", file, ", as MP.group_by is not 'DontGroup', please select a MP.group_by.mode which is not 'DontGroup'.")) 
}

# Used to be the Main function, now it is encapsulated in a function in order to be
# generalized and appliable for different configurations
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
	
	agreementMatrix <- CalculateAgreement(more.filtered.table)
	fileNameMat <- file.path(output.graphs.dir,paste0(dir.title,"/",file.title,"_mymatrix.txt"))
	write.table(agreementMatrix, file=fileNameMat, row.names=FALSE, col.names=FALSE)
	graph <- generateGraphG(agreementMatrix)
	fileName <- file.path(output.graphs.dir,paste0(dir.title,"/",file.title,"_graph.g"))
	WriteGFile(nrow(agreementMatrix),graph,fileName)
	
	AgreementPlot(agreementMatrix)
	RebelionPlot(rebelion.indexes)
	
	fileName <- file.path(output.graphs.dir,paste0(dir.title,"/",file.title,"_edges_Gephi.csv"))
	GenerateEdgesGephi(graph,fileName)
	
	fileName <- file.path(output.graphs.dir,paste0(dir.title,"/",file.title,"_nodes_Gephi.csv"))
	GenerateNodesGephi(corresp.table,fileName)
	
	net.measures <- CalculateNetworkMeasures(agreementMatrix)
	nome.file <- file.path(output.graphs.dir,paste0(dir.title,"/",file.title,"_net_measures.csv"))
	write.csv(net.measures,file=nome.file)    
	
	clustering.data <- CalculateClusterization(agreementMatrix,more.filtered.table,rebelion.indexes)
	
	nome.file <- file.path(output.community.csv.dir,paste0(dir.title,"/",file.title,"_cluster_information.csv"))
	write.csv(clustering.data$cluster.information,file=nome.file)    
	nome.file <- file.path(output.community.dir,paste0(dir.title,"/",file.title,"_cluster_comparison.csv"))
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
      dir.title <- sub.dirs[i]
      print(paste0("Processing Directory: ",sub.dirs[i]))
      print(paste0("Configuration File: ",sub.sub.dirs[j]))
      Calculate()
      print("Process Complete")
    })
  }
}
