#############################################################################################
# This script allows defining all the file constants used by the other scripts and functions.
# This include both file names and folder names.
# 
# 07/2015 Israel Mendonça (v1)
# 09/2015 Vincent Labatut (v2)
#############################################################################################

######## Folders
# general input folder
in.folder <- "in"
	# outputs of the CC method (processed independently)
	cc.folder <- file.path(in.folder,"pils")
	# raw data (i.e. tables)
	raw.folder <- file.path(in.folder,"raw")
		# aggregated files
		agg.folder <- file.path(raw.folder,"aggregated")
		# original files
		original.folder <- file.path(raw.folder,"original")
# general ouput folder
out.folder <- "out"
	#TODO
		
		
######## files
# input files
	all.votes.file <- file.path(agg.folder,"all-votes.csv")
	mep.details.file <- file.path(agg.folder,"mep-details.csv")
	mep.loyalty.file <- file.path(agg.folder,"mep-loyalty.csv")
	policy.freq.file <- file.path(agg.folder,"policy-freq.csv")
	vote.details.file <- file.path(agg.folder,"vote-details.csv")
# output files

		






# Name of the colums (in the .csv files) that concern the MPs. 
# The first one must be the column containing the complete name of the MP 
columns.names.MPs <- c("Name","Member.State","Group")   		

# Name of the colums (in the .csv files) that concern the votes (For,Against,Abstain...)
columns.names.votes <- c("Vote") 

# Name of the colums (in the .csv files) that concern the loyality (Loyal/Rebel)
columns.names.loyalty <- c("Loyal...Rebel.to.political.group") 

# File listing all the vote sessions (ie. "documents") 
docs.filename <- "VoteWatch Europe European Parliament, Council of the EU.csv"	

# Name of the directory where all the plots will be stored
output.plots.dir <- file.path(output.dir,"plots")

# Name of the directory where all the community detection algorithms results will be stored
output.community.dir <- file.path(output.dir,"community_algorithms_results")

# Name of the directory where all the detection algorithms csv files will be stored
output.community.csv.dir <- file.path(output.dir,"community_algorithms_csv")

# Name of the directory where all the generated graphs will be stored
output.graphs.dir <- file.path(output.dir,"graphs")

# Name of the directory where all the results related to agreement will be stored
output.agreement.dir <- file.path(output.dir,"agreement")
