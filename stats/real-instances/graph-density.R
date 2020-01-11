#!/usr/bin/Rscript
source("stats/define-plots.R")

csv.path = paste(DIR.REAL.INSTANCES.CSV,TARGET.TYPE,sep="/") # TODO bunun normalde FOR loop'dan gelmesi lazim : country and group
output.path = paste(DIR.REAL.INSTANCES.PLOTS,TARGET.TYPE,"graph-density",sep="/")

filename.filt.graph.str = paste("filtered-",G.STR,"-info-after-data-cleansing.csv",sep="")
filepath.filt.graph.str = paste(csv.path,filename.filt.graph.str,sep="/")


df = read.csv(filepath.filt.graph.str)

neg.edg.link = df[, "neg.edges.total.links"]
pos.edg.link = df[, "pos.edges.total.links"]
total.link = neg.edg.link + pos.edg.link

# ================================

# when a float in this format: "0,126", it is a problem for converting into numeric.
# I replaced "," by ".". Then, all is ok
densities = as.numeric( gsub(",", ".", as.character(df[, "density"])) )

orders = order(-total.link) # negate in order to rank in desc order
densities = densities[orders] # order by total.link in desc order
y = densities
x = seq(1, nb.obs)

# ================================

pdf(file=paste(output.path,"filter-step-filtered-networks-density.pdf",sep="/"), bg="white",compress=FALSE)
plot(x, y, xlab = "instances (filtered) => desc ordered by total link numbers", ylab = "density", type="p", col="blue", cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
#legend('topright', c("CC Exact", "ILS CC"), lty=c(1,1), lwd=c(2.5, 2.5), col=c(col.cc.exact, col.ilscc))
dev.off()

