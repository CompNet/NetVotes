#############################################################################################
# Main script, launches the whole process:
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
source("src/partition-networks/compare-clusters.R")
source("src/partition-networks/detect-clusters.R")
source("src/partition-networks/evaluate-clusters.R")
source("src/prepare-data/generate-data.R")
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

## filtering parameters
#domains <- c(DOMAIN.ALL, DOMAIN.VALUES)		# which domains to process individually
#domains <- DOMAIN.AFCO
domains <- DOMAIN.VW2SYMB[TEST.DOMAINS]
#dates <- c(DATE.T7.TERM, DATE.T7.YEARS)		# which time period to process individually
#dates <- c(DATE.T7.Y1)
dates <- TEST.YEARS
everything <- TRUE							# whether or not to process all data without distinction of country or date
#everything <- FALSE
#countries <- COUNTRY.VALUES				# which country to process individually
#countries <- c(COUNTRY.AT)
countries <- TEST.COUNTRIES
#countries <- c()
#groups <- GROUP.VALUES					# which group to process individually
#groups <- c(GROUP.SD)
groups <- GROUP.VW2SYMB[TEST.GROUPS]
#groups <- c()

## score matrix used to process agreement
score.file <- "m3"			# see folder in/score
neg.thresh <- -0.34			# threshold applied to negative agreement index values (during network extraction)
pos.thresh <- +0.34			# same thing, but for positive values

## partitioning algorithms
comdet.algos <- COMDET.ALGO.VALUES		# community detection algorithms
#corclst.algos <- CORCLST.ALGO.VALUES	# correlation clustering algorithms
corclst.algos <- c()
repetitions <- 10						# number of times each algorithm must be applied

## measures used to compare partitions
comp.measures <- c(
	"nmi", 
	"rand", 
	"adjusted.rand"
)

## formats of the generated plot (NA for screen -- mainly for debug)
plot.formats <- c(
	"PDF", 
	"PNG"
#	NA
)

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
#process.stats(data$all.votes, data$behavior.values, data$doc.details, data$mep.details,
#		domains, dates, everything, countries, groups, plot.formats)



#############################################################################################
# Process agreement and related stats (this might also take a while)
#############################################################################################
#process.agreement(data$all.votes, data$doc.details, data$mep.details, score.file,
#		domains, dates, everything, countries, groups, plot.formats)
	


#############################################################################################
# Extract all the networks
#############################################################################################
#extract.all.networks(data$mep.details, neg.thresh, pos.thresh, score.file,
#		domains, dates, everything, countries, groups, plot.formats)


#############################################################################################
# Detect communities for all the networks
#############################################################################################
#partition.all.graphs(data$mep.details, neg.thresh, pos.thresh, score.file,
#		domains, dates, everything, countries, groups, comdet.algos, corclst.algos, repetitions)


#############################################################################################
# Evaluate the detected partitions, for all the networks
#############################################################################################
#evaluate.all.partitions(data$mep.details, neg.thresh, pos.thresh, score.file,
#		domains, dates, everything, countries, groups, comdet.algos, corclst.algos, repetitions, plot.formats)


#############################################################################################
# Compare the detected partitions, for all the networks
#############################################################################################
#compare.all.partitions(data$mep.details, neg.thresh, pos.thresh, score.file,
#		domains, dates, everything, countries, groups, comdet.algos, corclst.algos, comp.measures, repetitions)




#############################################################################################
#############################################################################################
#############################################################################################
#############################################################################################
# Problèmes
# - agreement: for complete dataset, some nodes such as 599 have only 1s: possible, but improbable
#   note: might be due to small numbers of expressed votes (i.e. non-NA)

# test
# - définir de petits fichiers pour retester tout ça à fond
# TODO to test the scripts, define probas on mep-to-mep voting similarity, 
# then generate very small data set and see if it is recovered when extracting the network

# Extraction de réseaux
# - on peut extraire des réseaux au niveau des partis politiques
# - peut être aussi pour chaque vote ? mais les clusters seront triviaux (pr vs ctr) 

# Stats sur données brutes
# - on pourrait aussi voir le comportement individuel: nombre de votes de chaque type pour une personne.
#   ça peut être complet, agrégé par année, par législature... 
