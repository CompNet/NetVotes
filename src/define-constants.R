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
# general ouput folder
OUT.FOLDER <- "out"
# folder containing configuration files
CFG.FOLDER <- "config"

		
######## files
# TODO

		








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
