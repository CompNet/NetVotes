#!/usr/bin/Rscript

# TODO: Add comment
# 
# Author: nejat
###############################################################################

source("src/define-consts.R")
source("src/main-par.R")
source("src/main-seq.R")


# ==============================================================================
#### COMMENTS

# The variables we are using are global variables. they will be used throughout the flow of the code.
#

# Stats'lardaki plotlar icin: regarding "assessment for filtering": isolated node'lari kaldirmadan islem yapiyorum



# in 'prepare.grasp.ils.parameter.set': g.size'a gore parameter set degisiyo
# "Evaluating balancing on social networks through the efficient solution of correlation clustering problems"
# Mario Levorato, Rosa Figueiredo, Yuri Frota & Lúcia Drummond, 2017



# handling membership - nb.cluster variables for isolated nodes:
# we put all isolated nodes in the same cluster in the 'membership' variable.
# their cluster no is CLU.NO.FOR.ALL.ISOLATED.NODES ==> in plotsm they are visualized in white color
# ==============================================================================




###############################################################################
if(PARALLEL.COMPUTING.ENABLED){
 library(package="iterators", lib.loc=LIBRARY.LOC.PATH)
 library(package="foreach", lib.loc=LIBRARY.LOC.PATH)
 library(package="doParallel", lib.loc=LIBRARY.LOC.PATH)
 
 	if(!is.na(NB.CORES.FOR.PARALLEL.COMPUTING)){
 		NB.CORES.FOR.PARALLEL.COMPUTING = detectCores()-1
 	}
}
###############################################################################




LOG.UNIQUE.DESC = NA
LOG.FILENAME.PREFIX = NA
if(LOG.ENABLED){
	LOG.FILE.DIR.PATH = "logs-NetVotes" # TODO BELKI BUNU ILS ile ayni klasorde tutarim
	dir.create(LOG.FILE.DIR.PATH, showWarnings = FALSE)
	
	LOG.UNIQUE.DESC = paste(format(Sys.time(),"%d-%m-%Y-%X"),sep="") # will be used in 'src/define-consts.R'
	# LOG.UNIQUE.DESC:  we need to define once at first, not every file loading
	LOG.FILENAME.PREFIX = paste(LOG.FILE.DIR.PATH, "/", LOG.UNIQUE.DESC,"-log",sep="")
}



TARGET.STATES = c(
		"Austria"
#		"Belgium"
#		"Bulgaria",
#		"Croatia",
#		"Cyprus",
#		"Czech Republic",
#		"Denmark",
#		"Estonia",
#		"Finland",
#		"France"
#		"Germany",
#		"Greece",
#		"Hungary",
#		"Ireland",
#		"Italy",
#		"Latvia",
#		"Lithuania",
#		"Luxembourg",
#		"Malta",
#		"Netherlands",
#		"Poland",
#		"Portugal",
#		"Romania",
#		"Slovakia",
#		"Slovenia",
#		"Spain",
#		"Sweden",
#		"United Kingdom"
)


TARGET.GROUPS = c(
#		"ALDE",
#		"ECR",
#		"EFD",
#		"EPP",
#		"Greens",
#		"GUENGL",
#		"NI"
#		"SD"
)


TARGET.DOMAINS = c(
# 		"AFCO",
# 		"AGRI",
# #		"AUTR", # always empty ?
# 		"CONT",
# 		"DEVE",
# 		"EMPL",
# 		"FEMM",
# 		"INTA",
# 		"JURI",
# 		"PECH",
# 		"REGI",
# 		"TRAN",
# 		"AFET",
# 		"BUDG",
# 		"CULT",
# 		"ECON",
# 		"ENVI",
# 		"IMCO",
# 		"ITRE",
# 		"LIBE",
# 		"PETI",
		"RIPE"
)


TARGET.PERIODS = c(
		"2009-10",
		"2010-11",
		"2011-12",
		"2012-13",
		"2013-14",
		"Term"
)


FILTERING.THRESHOLDS = c("NA","0") # TODO I might do it in a cleaner way
TARGET.TYPES = c("State") # "Group" # TODO I might do it in a cleaner way



# =============================================================================
# =============================================================================
# =============================================================================


if(RECORDING.STATS.CSV){
	source("stats/define-consts.R")
	
	unlink(DIR.REAL.INSTANCES.CSV, recursive = TRUE) # delete the old one
	dir.create(DIR.REAL.INSTANCES.CSV, showWarnings = TRUE)
}





PARTITIONS.DIR = paste(OUT.DIR, "/", "partitions", sep="")
NETWORKS.DIR = paste(DATA.DIR, "/", "networks", sep="")
PARTITIONS.DIR.BY.VOTE.AGREEMENT = paste(PARTITIONS.DIR, "/", VOTE.AGREEMENT.TYPE, sep="")
NETWORKS.DIR.BY.VOTE.AGREEMENT = paste(NETWORKS.DIR, "/", VOTE.AGREEMENT.TYPE, sep="")

dir.create(PARTITIONS.DIR, showWarnings = FALSE)
#dir.create(NETWORKS.DIR, showWarnings = FALSE) # should be created before running, bc it contains data
dir.create(PARTITIONS.DIR.BY.VOTE.AGREEMENT, showWarnings = FALSE)
#dir.create(NETWORKS.DIR.BY.VOTE.AGREEMENT, showWarnings = FALSE) # should be created before running, bc it contains data


for(FILTERING.THRESHOLD in FILTERING.THRESHOLDS){
	
	PARTITIONS.DIR.BY.VOTE.AGREEMENT.THRESHOLD = paste(PARTITIONS.DIR.BY.VOTE.AGREEMENT, "/", "negtr=", FILTERING.THRESHOLD,"_postr=", FILTERING.THRESHOLD, sep="")
	NETWORKS.DIR.BY.VOTE.AGREEMENT.THRESHOLD = paste(NETWORKS.DIR.BY.VOTE.AGREEMENT, "/", "negtr=", FILTERING.THRESHOLD,"_postr=", FILTERING.THRESHOLD, sep="")
	BYSTATE.PARTITIONS = paste(PARTITIONS.DIR.BY.VOTE.AGREEMENT.THRESHOLD, "/", "bycountry", sep="")
	BYGROUP.PARTITIONS = paste(PARTITIONS.DIR.BY.VOTE.AGREEMENT.THRESHOLD, "/", "bygroup", sep="")
	BYSTATE.NETWORKS = paste(NETWORKS.DIR.BY.VOTE.AGREEMENT.THRESHOLD, "/", "bycountry", sep="")
	BYGROUP.NETWORKS = paste(NETWORKS.DIR.BY.VOTE.AGREEMENT.THRESHOLD, "/", "bygroup", sep="")
	
	dir.create(PARTITIONS.DIR.BY.VOTE.AGREEMENT.THRESHOLD, showWarnings = FALSE)
#	dir.create(NETWORKS.DIR.BY.VOTE.AGREEMENT.THRESHOLD, showWarnings = FALSE) # should be created before running, bc it contains data
	dir.create(BYSTATE.PARTITIONS, showWarnings = FALSE)
	dir.create(BYGROUP.PARTITIONS, showWarnings = FALSE)
#	dir.create(BYSTATE.NETWORKS, showWarnings = FALSE) # should be created before running, bc it contains data
#	dir.create(BYGROUP.NETWORKS, showWarnings = FALSE) # should be created before running, bc it contains data
	
	for(TARGET.TYPE in TARGET.TYPES){
		
		PARTITIONS = NA; NETWORKS = NA; TARGETS = NA
		if(TARGET.TYPE == "State"){
			PARTITIONS = BYSTATE.PARTITIONS
			NETWORKS = BYSTATE.NETWORKS
			TARGETS = TARGET.STATES
		} else { # if(TARGET.TYPE == "Group")
			PARTITIONS = BYGROUP.PARTITIONS
			NETWORKS = BYGROUP.NETWORKS
			TARGETS = TARGET.GROUPS
		}
		

		
		# ==============================================================
		
		if(RECORDING.STATS.CSV){
			source("stats/define-consts.R")
			
			output.path = paste(DIR.REAL.INSTANCES.CSV,TARGET.TYPE,sep="/") # TODO
			dir.create(output.path, showWarnings = TRUE)
		}
		
		
		# ==============================================================

	
		
		if(PARALLEL.COMPUTING.ENABLED){
		
			do.parallel.computing()
			
		} else {

			do.sequential.computing()
			
		}
	}
}


