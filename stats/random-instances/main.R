# TODO: Add comment
# 
# Author: nejat
###############################################################################

source("stats/define-consts.R")

unlink(DIR.RANDOM.GENERATED.NETWORKS, recursive = TRUE)
unlink(DIR.RANDOM.INSTANCES.CSV, recursive = TRUE)
unlink(DIR.RANDOM.INSTANCES.PLOTS, recursive = TRUE)





source(paste(DIR.RANDOM.INSTANCES, "generate-networks.R",sep="/"))

source(paste(DIR.RANDOM.INSTANCES, "generate-stats-csv.R",sep="/"))

source(paste(DIR.RANDOM.INSTANCES, "plot-stats.R",sep="/"))