source("clean-data.R")


################################################################################
################################################################################
#
#
#df = read.csv("filtered-ilscc-ccexact-info.csv")
#   
#sPartition = df[, "sPartition.cc.exact"]
#partition.cc.exact = lapply(sPartition, function(x) unlist( strsplit(as.character(x), " ") ))
#
#sPartition = df[, "sPartition.ilscc"]
#partition.ilscc = lapply(sPartition, function(x) unlist( strsplit(as.character(x), " ") ))
#indx.to.be.removed = identify.erroneous.obs(partition.cc.exact, partition.ilscc)
#df2 = remove.erreonous.obs(df, indx.to.be.removed)    
#
#df3 = df2[, c("country", "domain", "period", "network.filtering", "cc.exact.imbalance.value", "cc.exact.imbalance.percentage", "ilscc.imbalance.value", "ilscc.imbalance.percentage", "sPartition.cc.exact", "sPartition.ilscc")]
#write.csv(file="filtered.csv", x=df3)
#
#
#
################################################################################
################################################################################
#
#
##input.filename = "filtered-im-ccexact-info.csv"
##df.f = read.csv(input.filename)
##input.filename = "original-im-ccexact-info.csv"
##df.o = read.csv(input.filename)
#
## remove unmatched observations
##res = pre.processing(df.f, df.o)
##df.f = res$df1
##df.o = res$df2
#
##write.csv(file="filtered.csv", x=df.f)
##write.csv(file="original.csv", x=df.o)

