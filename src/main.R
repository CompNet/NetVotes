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
# setwd("D:/Eclipse/workspaces/Networks/NetVotes")
# source("src/main.R")
#############################################################################################
source("src/define-constants.R")
source("src/define-paths.R")
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
#dataset.name <- "VW"		# VoteWatch
dataset.name <- "IYP"		# It's your Parliament
#dataset.name <- "PT"		# Parltrack

## filtering parameters
domains <- c(DOMAIN.ALL, DOMAIN.VALUES)			# which domains to process individually
#domains <- DOMAIN.AFCO
#domains <- c(DOMAIN.VW2SYMB[TEST.DOMAINS],DOMAIN.ALL)
#dates <- c(DATE.T7.TERM, DATE.T7.YEARS)			# which time period to process individually
dates <- c(
#		DATE.T7.Y1
#		DATE.T7.Y2
#		DATE.T7.Y3
#		DATE.T7.Y4
#		DATE.T7.Y5
		DATE.T7.TERM
)
#dates <- TEST.YEARS
everything <- TRUE								# whether or not to process all data without distinction of country or date
#everything <- FALSE
#countries <- COUNTRY.VALUES						# which country to process individually
#countries <- c(COUNTRY.AT)
#countries <- TEST.COUNTRIES
#countries <- c()
countries <- c(
#		COUNTRY.AT,COUNTRY.BE,COUNTRY.BG,COUNTRY.HR,COUNTRY.CY,COUNTRY.CZ,COUNTRY.DK
#		COUNTRY.EE,COUNTRY.FI,COUNTRY.FR,COUNTRY.DE,COUNTRY.GR,COUNTRY.HU,COUNTRY.IE
#		COUNTRY.IT,COUNTRY.LV,COUNTRY.LT,COUNTRY.LU,COUNTRY.MT,COUNTRY.NL,COUNTRY.PL
#		COUNTRY.PT,COUNTRY.RO,COUNTRY.SK,COUNTRY.SI,COUNTRY.ES,COUNTRY.SE,COUNTRY.UK
)
#groups <- GROUP.VALUES							# which group to process individually
#groups <- c(GROUP.SD)
#groups <- GROUP.VW2SYMB[TEST.GROUPS]
#groups <- c()
groups <- c(
#	GROUP.ALDE,GROUP.ECR,GROUP.EFD,GROUP.EPP
#	GROUP.GREENS,GROUP.GUENGL,GROUP.NI,GROUP.SD
)

## score matrix used to process agreement
score.file <- "m3"					# see folder in/score
thresh <- c(0,0)					# no thresholding at all
#thresh <- c(-0.34,+0.34)			# thresholds applied to agreement index values during network extraction (use c(0,0) for no filtering)
#thresh <- NA						# both thresholds automatically estimated (through k-means)

## partitioning algorithms
comdet.algos <- COMDET.ALGO.VALUES		# community detection algorithms
#corclst.algos <- CORCLST.ALGO.VALUES	# correlation clustering algorithms
corclst.algos <- c()
repetitions <- 5						# number of times each algorithm must be applied

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
extract.all.networks(data$mep.details, thresh, score.file,
		domains, dates, everything, countries, groups, plot.formats)


#############################################################################################
# Detect communities for all the networks
#############################################################################################
#partition.all.graphs(data$mep.details, thresh, score.file,
#		domains, dates, everything, countries, groups, comdet.algos, corclst.algos, repetitions, plot.formats)


#############################################################################################
# Evaluate the detected partitions, for all the networks
#############################################################################################
#evaluate.all.partitions(data$mep.details, thresh, score.file,
#		domains, dates, everything, countries, groups, comdet.algos, corclst.algos, repetitions, plot.formats)


#############################################################################################
# Compare the detected partitions, for all the networks
#############################################################################################
#compare.all.partitions(data$mep.details, thresh, score.file,
#		domains, dates, everything, countries, groups, comdet.algos, corclst.algos, comp.measures, repetitions)



#############################################################################################
#############################################################################################
#############################################################################################
#############################################################################################
# Notes
# - when dealing with filtered nodes (countries, groups): shouldn't we record only the concerned nodes (in the networks)?

# Problems
# - Pb with IYP: absence not explicitly represented >> peaks of zero agreement for yearly votes.
# - agreement: for complete dataset, some nodes such as 599 have only 1s: possible, but improbable
#   note: might be due to small numbers of expressed votes (i.e. non-NA)

# Extraction de r�seaux
# - on peut extraire des r�seaux au niveau des partis politiques
# - peut �tre aussi pour chaque vote ? mais les clusters seront triviaux (pr vs ctr) 

# Stats sur donn�es brutes
# - on pourrait aussi voir le comportement individuel: nombre de votes de chaque type pour une personne.
#   �a peut �tre complet, agr�g� par ann�e, par l�gislature... 
