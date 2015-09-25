#############################################################################################
# This script allows defining all the file constants used by the other scripts and functions.
# This include both file names and folder names.
# 
# 07/2015 Israel Mendonça (v1)
# 09/2015 Vincent Labatut (v2)
#############################################################################################

######## Folders
# general input folder
IN.FOLDER <- "in"
	# outputs of the CC method (processed independently)
	CC.FOLDER <- file.path(IN.FOLDER,"pils")
	# raw data (i.e. tables)
	RAW.FOLDER <- file.path(IN.FOLDER,"raw")
		# aggregated files
		AGG.FOLDER <- file.path(RAW.FOLDER,"aggregated")
		# original files
		ORIG.FOLDER <- file.path(RAW.FOLDER,"original")
# general ouput folder
OUT.FOLDER <- "out"
	#TODO
		
		
######## files
# input files
	ALL.VOTES.FILE		<- file.path(AGG.FOLDER,"all-votes.csv")
	MEP.DETAILS.FILE	<- file.path(AGG.FOLDER,"mep-details.csv")
	MEP.loyalty.FILE	<- file.path(AGG.FOLDER,"mep-loyalty.csv")
	POLICY.FREQ.FILE	<- file.path(AGG.FOLDER,"policy-freq.csv")
	DOC.DETAILS.FILE	<- file.path(AGG.FOLDER,"document-details.csv")
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
