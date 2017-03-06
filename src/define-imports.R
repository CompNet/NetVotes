#############################################################################################
# Just loads the scripts necessary to process the dataset. This script is needed when
# using foreach (parallel processing), since each worker must load all these dependencies.
# 
# 05/2016 Vincent Labatut
#############################################################################################
source("src/define-constants.R")
source("src/define-algos.R")
source("src/define-functions.R")
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
