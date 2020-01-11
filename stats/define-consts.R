# TODO: Add comment
# 
# Author: nejat
###############################################################################





# import algo names
source("src/define-consts.R")

G.STR = "graph-structure"

EXCC.STATS.CSV.FILENAME = paste(ExCC,"-info.csv",sep="")
IM.STATS.CSV.FILENAME = paste(COMDET.INFOMAP,"-info.csv",sep="")
ILSCC.STATS.CSV.FILENAME = paste(ILSCC,"-info.csv",sep="")
ILSRCC.STATS.CSV.FILENAME = paste(ILSRCC,"-info.csv",sep="")
KMBS.STATS.CSV.FILENAME = paste(KMBS,"-info.csv",sep="")
GRAPH.STR.STATS.CSV.FILENAME = paste(G.STR,"-info.csv",sep="")


###############################################################################
# REAL INSTANCES
###############################################################################

DIR.REAL.INSTANCES = "stats/real-instances"
DIR.REAL.INSTANCES.CSV = paste(DIR.REAL.INSTANCES,"/csv",sep="")
DIR.REAL.INSTANCES.PLOTS = paste(DIR.REAL.INSTANCES,"/plots",sep="")
DIR.REAL.INSTANCES.PLOTS.GRAPH.STR = paste(DIR.REAL.INSTANCES.PLOTS,"/",G.STR,"-analysis",sep="")


###############################################################################
# RANDOM INSTANCES
###############################################################################

MAX.G.SIZE = 150 # ExCC spend a lot of time for running an instance with graph size > 200
# Actually, the max graph size is 850

DIR.RANDOM.INSTANCES = "stats/random-instances"
DIR.RANDOM.GENERATED.NETWORKS = paste(DIR.RANDOM.INSTANCES,"/generated-networks",sep="")
DIR.RANDOM.INSTANCES.CSV = paste(DIR.RANDOM.INSTANCES,"/csv",sep="")
DIR.BASE.GRAPH = "stats/random-instances/in"
DIR.RANDOM.INSTANCES.PLOTS = paste(DIR.RANDOM.INSTANCES,"/plots",sep="")

BASE.GRAPH.FILENAME = paste(DIR.BASE.GRAPH,"/signed-all-all-Term-original.graphml",sep="")


# ===========================================================
# when generating random instances, step is increasing amount of node number for each random instance
STEP = 10 
# ===========================================================