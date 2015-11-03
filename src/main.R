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
#
# setwd("C:/Eclipse/workspaces/Networks/NetVotes")
# source("src/main.R")
#############################################################################################
source("src/define-constants.R")
source("src/build-networks/extract-networks.R")
source("src/build-networks/process-agreement.R")
source("src/prepare-data/load-itsyourparliament.R")
source("src/prepare-data/load-parltrack.R")
source("src/prepare-data/load-votewatch.R")
source("src/prepare-data/process-stats.R")



#############################################################################################
# Init parameters
#############################################################################################
## raw data
dataset.name <- "VW"		# VoteWatch
#dataset.name <- "IYP"		# It's your Parliament
#dataset.name <- "PT"		# Parltrack

# filtering parameters
domains <- c(DOMAIN.ALL, DOMAIN.VALUES)		# which domains to process individually
#domains <- DOMAIN.AFCO
dates <- c(DATE.T7.TERM, DATE.T7.YEARS)		# which time period to process individually
everything <- TRUE							# whether or not to process all data without distinction of country or date
countries <- c(COUNTRY.ALL, COUNTRY.VALUES)	# which country to process individually
#countries <- COUNTRY.ALL
groups <- c(GROUP.ALL, GROUP.VALUES)		# which group to process individually
#groups <- c()

## score matrix used to process agreement
score.file <- "m3"			# see folder in/score
neg.thresh <- -0.34			# threshold applied to negative agreement index values (during network extraction)
pos.thresh <- +0.34			# same thing, but for positive values

#############################################################################################
# Load raw data
#############################################################################################
if(dataset.name=="VW")
{	data <- load.votewatch.data()
}else if(dataset.name=="IYP")
{	data <- load.itsyourparliament.data()
}else if(dataset.name=="PT")
{	data <- load.parltrack.data()
}



#############################################################################################
# Process raw data stats (this might take a while)
#############################################################################################
process.stats(data$all.votes, data$behavior.values, data$doc.details, data$mep.details,
		domains, dates, everything, countries, groups)



#############################################################################################
# Process agreement and related stats (this might also take a while)
#############################################################################################
process.agreement(data$all.votes, data$doc.details, data$mep.details, score.file,
		domains, dates, everything, countries, groups)
	


#############################################################################################
# Extract all the networks
#############################################################################################
extract.all.networks(data$mep.details, neg.thresh, pos.thresh, score.file,
		domains, dates, everything, countries, groups)




# Problèmes
# - agreement: for complete dataset, some nodes such as 599 have only 1s: possible, but improbable 

# test
# - définir de petits fichiers pour retester tout ça à fond

# Extraction de réseaux
# - generate plots for each graph
# - on peut extraire des réseaux au niveau des partis politiques
# - peut être aussi pour chaque vote ? mais les clusters seront triviaux (pr vs ctr) 

# Stats sur données brutes
# - on pourrait aussi voir le comportement individuel: nombre de vote de chaque type pour une personne.
#   ça peut être complet, agrégé par année, par législature... 
