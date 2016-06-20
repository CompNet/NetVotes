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
# setwd("~/eclipse/workspaces/Networks/NetVotes")
# source("src/main.R")
#############################################################################################
# libraries for parallel processing
library(foreach)
library(doParallel)

source("src/define-imports.R")



#############################################################################################
# Init parameters
#############################################################################################
##################### raw data
#dataset.name <- "VW"		# VoteWatch
dataset.name <- "IYP"		# It's your Parliament
#dataset.name <- "PT"		# Parltrack

##################### domains
domains <- c(DOMAIN.VALUES, DOMAIN.ALL)			# which domains to process individually
#domains <- DOMAIN.CULT
#domains <- c(DOMAIN.VW2SYMB[TEST.DOMAINS],DOMAIN.ALL)
##################### dates
dates <- c(DATE.T7.YEARS, DATE.T7.TERM)			# which time periods to process individually
#dates <- c(
#		DATE.T7.Y1
#		DATE.T7.Y2
#		DATE.T7.Y3
#		DATE.T7.Y4
#		DATE.T7.Y5
#		DATE.T7.TERM
#)
#dates <- TEST.YEARS
##################### everything at once
everything <- TRUE								# whether or not to process all data without distinction of country or date
#everything <- FALSE
##################### countries
countries <- COUNTRY.VALUES						# which country to process individually
#countries <- c(COUNTRY.HR)
#countries <- TEST.COUNTRIES
#countries <- c(
#		COUNTRY.AT,COUNTRY.BE,COUNTRY.BG,COUNTRY.HR,COUNTRY.CY,COUNTRY.CZ,COUNTRY.DK
#		COUNTRY.EE,COUNTRY.FI,COUNTRY.FR,COUNTRY.DE,COUNTRY.GR,COUNTRY.HU,COUNTRY.IE
#		COUNTRY.IT,COUNTRY.LV,COUNTRY.LT,COUNTRY.LU,COUNTRY.MT,COUNTRY.NL,COUNTRY.PL
#		COUNTRY.PT,COUNTRY.RO,COUNTRY.SK,COUNTRY.SI,COUNTRY.ES,COUNTRY.SE,COUNTRY.UK
#)
##################### groups
groups <- GROUP.VALUES							# which group to process individually
#groups <- c(GROUP.SD)
#groups <- GROUP.VW2SYMB[TEST.GROUPS]
#groups <- c(
#	GROUP.ALDE,GROUP.ECR,GROUP.EFD,GROUP.EPP
#	GROUP.GREENS,GROUP.GUENGL,GROUP.NI,GROUP.SD
#)

##################### score matrix used to process agreement
score.file <- "m3"					# see folder in/score
thresh <- c(0,0)					# no thresholding at all
#thresh <- c(-0.34,+0.34)			# thresholds applied to agreement index values during network extraction (use c(0,0) for no filtering)
#thresh <- NA						# both thresholds automatically estimated (through k-means)

##################### partitioning algorithms
comdet.algos <- COMDET.ALGO.VALUES		# community detection algorithms
#corclst.algos <- CORCLST.ALGO.VALUES	# correlation clustering algorithms
corclst.algos <- c()
repetitions <- 5						# number of times each algorithm must be applied

##################### measures used to compare partitions
comp.measures <- c(
	"nmi",
	"rand",
	"adjusted.rand"
)

##################### formats of the generated plot (NA for screen -- mainly for debug)
plot.formats <- c(
	"PDF", 
	"PNG"
#	NA
)

##################### configure parallel processing
#cn <- detectCores(all.tests=TRUE)
#if(!is.na(cl))
#	cl <- makeCluster(cn)		# automatically use all the available processors
#else
	cl <- makeCluster(4)		# manually set the number of processors to use
registerDoParallel(cl)


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
# Extract all the networks (just a bit faster)
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
#	xx get official dates for each MEP and check with that
#	>> check that, when filtering, MEPs voting *only* NA (i.e. not absent) are not taken into account (this is general, not IYP-specific).
# - in IYP, there's also a duplicate problem, some MEPs are represented several times.
#	>> this could be handled when filtering the data? or at loading/preparation time?
# - agreement: for complete dataset, some nodes such as 599 have only 1s: possible, but improbable
#   note: might be due to small numbers of expressed votes (i.e. non-NA)
#	note2: version from a long time ago. check again now.

# Extraction de réseaux
# - on peut extraire des réseaux au niveau des partis politiques
# - peut étre aussi pour chaque vote ? mais les clusters seront triviaux (pr vs ctr) 

# Stats sur données brutes
# - on pourrait aussi voir le comportement individuel: nombre de votes de chaque type pour une personne.
#   ça peut être complet, agrégé par année, par législature... 
