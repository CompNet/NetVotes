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
# The parameters located at the beginning of the script (section "Init parameters", right below)
# allow to control it, and to restrict the focus to certain topics/years, or control certain 
# points of the network extraction.
# 
# 07/2015 Israel Mendonça (v1)
# 09/2015 Vincent Labatut (v2)
#############################################################################################



####################################################
# Init parameters
####################################################
# Names of the political groups to consider, NA for all
MP.political_groups <- c(NA)
# Names of the countries to consider, NA for all
MP.countries        <- c(NA)
# Ids of the MEPs to consider, NA for all
MP.mp_ids           <- c(NA)
# Group (TODO what?) according to some criterion, possible values: "country" , "group" , "none" 
MP.group_by         <- c("DontGroup") # possibilities : 
# Type of grouping (TODO ?), possible values: 
# "None"
# "majority_vote_in_each_group"
# "min_each_vote_possibility_between_groups"
# "avg_agreement_between_MPs_of_each_group"
MP.group_by.mode    <- c("avg_agreement_between_MPs_of_each_group")
# Consisdered time range (format: dd/mm/yyyy), NA for no limit
DOCS.time_limits    <- c(NA,NA)
# Considered policies, NA for all
DOCS.policies       <- c(NA)

# TODO don't know what this is
table               <- c("2")
# TODO minimum weight threshold (absolute value) used when building the signed graphs. Must be in [O;1]
alpha <- 0

# TODO probably whether to nominal or numeric values in the exported graph files
OUTPUTFILES.Gfile.weigth <- "%agree-%disagree" # possibilities : "+1_or_-1" , "%agree-%disagree"
# Whether or not to export the graphs using the .g fromat 
OUTPUTFILES.Gfile            <- TRUE
# Whether or not to export histograms (#TODO of what) 
OUTPUTFILES.Histo            <- TRUE
# Whether or not to export the graphs using the Gephi fromat 
OUTPUTFILES.GephiLinkTable   <- TRUE
# Whether or not to export somthing (TODO what?) 
OUTPUTFILES.Corresp          <- TRUE
# Whether or not to export the graphs using the .gexf fromat 
OUTPUTFILES.GEXF             <- FALSE
# Whether or not to export some rankings (TODO which ones?) 
OUTPUTFILES.RANKING          <- TRUE

# TODO additional parameters
# Name of the files containing the score tables, when processing the agreement


####################################################
# Clean/Check parameters
####################################################
#TODO load the lists of possible values then check them?




####################################################
# Process the data TODO
####################################################
# Load the raw data
# Preprocess/filter the resulting tables
# Process the voting agreement index and other statistics
# Extract the collection of networks
# Process various network statistics
# Apply the community detection algorithms
# Compare the resulting partitions, also with the pILS results if available.
# Incidentally generate plots related to certain of these steps




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
