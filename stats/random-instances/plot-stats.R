#!/usr/bin/Rscript
source("stats/define-plots.R")
source("stats/define-consts.R")

output.path = DIR.RANDOM.INSTANCES.PLOTS
dir.create(output.path, showWarnings=TRUE)
csv.path = DIR.RANDOM.INSTANCES.CSV

filename.ILSCC = paste(ILSCC,"-info.csv",sep="")
filepath.ILSCC = paste(csv.path,filename.ILSCC,sep="/")
filename.ExCC = paste(ExCC,"-info.csv",sep="")
filepath.ExCC = paste(csv.path,filename.ExCC,sep="/")

filename.graph.str = paste(G.STR,"-info.csv",sep="")
filepath.graph.str = paste(csv.path,filename.graph.str,sep="/")

method1 = ExCC
method2 = ILSCC

################################################################################
################################################################################

# =====================================
# EXECUTION TIME PERFORMANCE ANALYSIS
# =====================================
plot.exec.time.perf.analysis(output.path, filepath.ExCC, filepath.ILSCC, filepath.graph.str, method1, method2)


################################################################################
################################################################################

# =====================================
# IMB vs. GRAPH.SIZE
# =====================================
plot.imb.vs.graph.size(output.path, filepath.ExCC, filepath.ILSCC, filepath.graph.str, method1, method2)


################################################################################
################################################################################

# =====================================
# NMI vs. FREQUENCE
# =====================================
plot.nmi.vs.freq.betw.2.methods(output.path, filepath.ExCC, filepath.ILSCC, filepath.graph.str, method1, method2)


################################################################################
################################################################################

# =====================================
# DELTA.IMBALANCE vs. FREQUENCE
# =====================================
plot.delta.imb.vs.freq.betw.2.methods(output.path, filepath.ExCC, filepath.ILSCC, method1, method2)


################################################################################
################################################################################

# =====================================
# NMI vs. DELTA.NB.CLUSTER
# =====================================
plot.nmi.vs.nbCluster.betw.2.methods(output.path, filepath.ExCC, filepath.ILSCC, filepath.graph.str, method1, method2)


################################################################################
################################################################################

# =====================================
# GRAPH DENSITY
# =====================================
plot.graph.density(output.path, filepath.graph.str)
