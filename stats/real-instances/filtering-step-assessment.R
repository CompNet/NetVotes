# TODO: Add comment
# 
# Author: nejat
###############################################################################

#!/usr/bin/Rscript
source("stats/define-plots.R")

csv.path = paste(DIR.REAL.INSTANCES.CSV,TARGET.TYPE,sep="/") # TODO bunun normlade FOR loop'dan gelmesi lazim : country and group
output.path = paste(DIR.REAL.INSTANCES.PLOTS,TARGET.TYPE,"filtering-step-assessment",sep="/")
method1 = COMDET.INFOMAP
method2 = ExCC


filename.orig.IM = paste("original-",method1,"-info-after-data-cleansing.csv",sep="")
filepath.orig.IM = paste(csv.path,filename.orig.IM,sep="/")
filename.filt.IM = paste("filtered-",method1,"-info-after-data-cleansing.csv",sep="")
filepath.filt.IM = paste(csv.path,filename.filt.IM,sep="/")
filename.orig.ExCC = paste("original-",method2,"-info-after-data-cleansing.csv",sep="")
filepath.orig.ExCC = paste(csv.path,filename.orig.ExCC,sep="/")
filename.filt.ExCC = paste("filtered-",method2,"-info-after-data-cleansing.csv",sep="")
filepath.filt.ExCC = paste(csv.path,filename.filt.ExCC,sep="/")

filename.orig.graph.str = paste("original-",G.STR,"-info-after-data-cleansing.csv",sep="")
filepath.orig.graph.str = paste(csv.path,filename.orig.graph.str,sep="/")
filename.filt.graph.str = paste("filtered-",G.STR,"-info-after-data-cleansing.csv",sep="")
filepath.filt.graph.str = paste(csv.path,filename.filt.graph.str,sep="/")


################################################################################
################################################################################


# NOT: Metodlara "filepath.orig.graph.str" input parametre olarak vermemin bi mantikli aciklamasi var:
# filtering oncesinde eger isolated node varsa bunlar hem filtered hem de original versiyondan cikar.
# AMA cikarmasan da olur cunku teoride original versiyonda isolated node cok cok az olmali
# EXAMPLE: plot.instances.vs.nbCluster.betw.orig.and.filt(output.path, filepath.orig.ExCC, filepath.filt.ExCC, filepath.orig.graph.str, filepath.orig.graph.str, method2)

print("INSTANCES vs. NB.CLUSTER")
# =====================================
# INSTANCES vs. NB.CLUSTER
# =====================================
plot.instances.vs.nbCluster.betw.orig.and.filt(output.path, filepath.orig.ExCC,
		filepath.filt.ExCC, NA, NA, method2)
plot.instances.vs.nbCluster.betw.orig.and.filt(output.path, filepath.orig.IM, filepath.filt.IM,
		NA, NA, method1)


################################################################################
################################################################################
print("NMI vs. FREQUENCE")
# =====================================
# NMI vs. FREQUENCE
# =====================================

plot.nmi.vs.freq.betw.orig.and.filt(output.path, filepath.orig.ExCC, filepath.filt.ExCC,
		NA, NA, method2)
plot.nmi.vs.freq.betw.orig.and.filt(output.path, filepath.orig.IM, filepath.filt.IM,
		NA, NA, method1)


################################################################################
################################################################################
print("DELTA.IMBALANCE vs. FREQUENCE")
# =====================================
# DELTA.IMBALANCE vs. FREQUENCE
# =====================================
plot.delta.imb.vs.freq.betw.orig.and.filt(output.path, filepath.orig.ExCC, filepath.filt.ExCC, method2)
plot.delta.imb.vs.freq.betw.orig.and.filt(output.path, filepath.orig.IM, filepath.filt.IM, method1)


################################################################################
################################################################################
print("NMI vs. DELTA.NB.CLUSTER")
# =====================================
# NMI vs. DELTA.NB.CLUSTER
# =====================================

plot.nmi.vs.nbCluster.betw.orig.and.filt(output.path, filepath.orig.ExCC, filepath.filt.ExCC,
		NA, NA, method2)
plot.nmi.vs.nbCluster.betw.orig.and.filt(output.path, filepath.orig.IM, filepath.filt.IM,
		NA, NA, method1)
