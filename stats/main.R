# TODO: Add comment
# 
# Author: nejat
###############################################################################


source("stats/define-consts.R")
source("stats/common.R")

start=Sys.time()


# ==============================================================================
# RANDOM INSTANCES
# ==============================================================================

source("stats/random-instances/main.R")





# ==============================================================================
# REAL INSTANCES
# ==============================================================================

# if both "State" and "Group" are considered, a new directory called 'All' is created and it combines the results of both of them
#TARGET.TYPES = c("State","Group") 
TARGET.TYPES = c("State")

source("stats/real-instances/main.R")




end=Sys.time()
print(end - start)
warnings()
