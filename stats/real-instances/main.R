# TODO: Add comment
# 
# Author: nejat
###############################################################################

source("stats/define-consts.R")

# ----------------------------------------
# Remarks:
# - This script requires the existence of the results for both thresh=NA and thresh=c(0,0)
# ----------------------------------------





################################################
# 1) (Optional) If you want to generate csv files that will be used for stats,
#    you can uncomment the code block below. What this does:
#           Run 'src/main.R' and assing TRUE only to RECORDING.STATS.CSV,
#               ILSCC.ENABLED, EXCC.ENABLED, INFOMAP.ENABLED
################################################
# ####### unlink(DIR.REAL.INSTANCES.PLOTS, recursive = TRUE)
# 
# # .libPaths() does not contain the path located in user dir, you might add this into .libPaths()
# LIBRARY.LOC.PATH = .libPaths() # for nejat's pc
# #LIBRARY.LOC.PATH = "../../../../libs/R" # for cluster
# 
# # PARALLEL.COMPUTING is good for process speed, but SEQ.COMPUTING is good for debugging and printing on the console
# PARALLEL.COMPUTING.ENABLED = FALSE
# NB.CORES.FOR.PARALLEL.COMPUTING = 3 # NA will cause to use the max nb cores if PARALLEL.COMPUTING is enabled
# 
# LOG.ENABLED = TRUE
# RUNNING.PARTITIONING.ALGOS.ENABLED = FALSE
# PLOTTING.ENABLED = FALSE
# UPDATING.GRAPHML.CONTENT.ENABLED = FALSE
# RECORDING.STATS.CSV = TRUE
# 
# ILSCC.ENABLED = TRUE
# EXCC.ENABLED = TRUE
# ILSRCC.ENABLED = FALSE
# KMBS.ENABLED = FALSE
# INFOMAP.ENABLED = TRUE
# MULTIPLOT.ENABLED = FALSE
# source("src/main.R") # run it to get stats csv
## ==========================================================================================





################################################
# 2) Data cleansing: if there is some inconsistency between csv result files, we need to keep only the common instances
#       2.1) it checks if there is some inconsistency between only filtered files (e.g. comparison between ExCC and ILSCC)
#       2.2) it checks if there is some inconsistency between only unfiltered files (e.g. comparison between ExCC and ILSCC)
#       2.3) it checks if there is some inconsistency between filtered and unfiltered files (based on the same result type, i.e. nb cluster)
#       In the end, we keep only the common instances.
################################################
source(paste(DIR.REAL.INSTANCES, "data-cleansing-csv.R",sep="/"))


################################################
# 3) Combine the content of the csv files for State and Group in the directory, called 'All'
################################################
source(paste(DIR.REAL.INSTANCES, "combine-state-group-csv.R",sep="/"))


################################################
# 4) Perform plotting
################################################
source(paste(DIR.REAL.INSTANCES, "plot-stats.R",sep="/"))
