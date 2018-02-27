#############################################################################################
# Produces a series of small plots, each one focusing on one political group and representing 
# links only (by opposition to other information, such as MEP names, clusters, etc.).
# 
# setwd("D:/Eclipse/workspaces/Networks/NetVotes")
# setwd("~/eclipse/workspaces/Networks/NetVotes")
# source("src/plot-tools/circos/groups.R")
#############################################################################################
source("src/define-imports.R")



#############################################################################################
# load the graph
#in.folder <- "/home/vlabatut/eclipse/workspaces/Networks/NetVotes/out/partitions/m3/negtr=NA_postr=NA/bycountry/France/AGRI/2012-13"
in.folder <- "/home/vlabatut/eclipse/workspaces/Networks/NetVotes/out/partitions/m3/negtr=NA_postr=NA/bycountry/France/AGRI/Term"
g <- read.graph(file.path(in.folder,"signed.graphml"),format="graphml")

# load MEP meta-data
data <- load.itsyourparliament.data()
md <- data$mep.details
idx <- match(V(g)$MEPid,md[,COL.MEPID])
md <- md[idx,]

# load the partition
#p <- as.matrix(read.table(file.path(in.folder,"1/IM-membership.txt")))
p <- as.matrix(read.table(file.path(in.folder,"1/GRASP-CC_l1_k7_a1_g0_p30-membership.txt")))
md <- cbind(md,p)
colnames(md)[ncol(md)] <- "CLUSTER_ID"
cluster.cols <- c( # arbitrary colors for up to 9 clusters, taken from http://colorbrewer2.org/#type=qualitative&scheme=Set1&n=9
	"fill_color=(228,26,28)",
	"fill_color=(55,126,184)",
	"fill_color=(77,175,74)",
	"fill_color=(152,78,163)",
	"fill_color=(255,127,0)",
	"fill_color=(255,255,51)",
	"fill_color=(166,86,40)",
	"fill_color=(247,129,191)",
	"fill_color=(153,153,153)"
)

# add node id to the table
md <- cbind(md,1:vcount(g))
colnames(md)[ncol(md)] <- "NODE_ID"

# set up political groups
ngrp <- length(unique(md[,COL.GROUP]))
GROUPS_ORDERED <- c(GROUP.GUENGL, GROUP.GREENS, GROUP.SD, GROUP.ALDE, GROUP.EPP, GROUP.ECR, GROUP.EFD, GROUP.NI)
md <- cbind(md,match(md[,COL.GROUP],GROUPS_ORDERED))
colnames(md)[ncol(md)] <- "GROUP_ID"
CIRCOS_GROUP_COLORS <- c()	# traditional colors of the political parties
CIRCOS_GROUP_COLORS[GROUP.GUENGL] <- "198,0,0"
CIRCOS_GROUP_COLORS[GROUP.GREENS] <- "115,204,32"
CIRCOS_GROUP_COLORS[GROUP.SD] <- "252,138,138"
CIRCOS_GROUP_COLORS[GROUP.ALDE] <- "255,181,53"
CIRCOS_GROUP_COLORS[GROUP.EPP] <- "127,146,255"
CIRCOS_GROUP_COLORS[GROUP.ECR] <- "46,72,221"
CIRCOS_GROUP_COLORS[GROUP.EFD] <- "104,39,216"
CIRCOS_GROUP_COLORS[GROUP.NI] <- "119,65,29"

# process group-related node id
md <- md[order(as.numeric(md[,"GROUP_ID"]),md[,COL.LASTNAME]),]
md <- cbind(md,rep(NA,nrow(md)))
colnames(md)[ncol(md)] <- "GNODE_ID"
nid <- 1
gid <- 1
for(i in 1:nrow(md))
{	if(md[i,"GROUP_ID"]>gid)
	{	gid <- md[i,"GROUP_ID"]
		nid <- 1
	}
	md[i,"GNODE_ID"] <- nid
	nid <- nid + 1
}
md <- md[order(as.numeric(md[,"NODE_ID"])),]



###########################as#############################################################
# convert to appropriate format
out.folder <- "/home/vlabatut/Downloads/circos-test/EP"
el <- get.edgelist(g)
grp <- paste0("group",md[,"GROUP_ID"])
beg.pos <- as.numeric(md[,"GNODE_ID"]) - 1
end.pos <- md[,"GNODE_ID"]
node.names <- md[,COL.FULLNAME]
node.names <- sapply(node.names, function(node.name) # possibly shorten firstnames
{	if(nchar(node.name)>1) #initially, that threshold was 20, but it's better to do it systematically
		{	tmp <- strsplit(x=node.name, split=" ", fixed=TRUE)[[1]]
			if(grepl("-",tmp[1]))
			{	tmp2 <- strsplit(x=tmp[1], split="-", fixed=TRUE)[[1]]
				tmp2 <- sapply(tmp2,function(s) toupper(paste0(substr(x=s, start=1, stop=1),".")))
				tmp[1] <- paste(tmp2,collapse="-")
			}
			else
				tmp[1] <- paste0(substr(x=tmp[1], start=1, stop=1),".")
			result <- paste(tmp,collapse=" ")
		}
		else
			result <- node.name
		return(result)
	}
)

# histograms (node imbalance participation)
tmp <- cbind(md[el[,1],"CLUSTER_ID"],md[el[,2],"CLUSTER_ID"])
misplaced <- (tmp[,1]==tmp[,2] & E(g)$weight<0) | (tmp[,1]!=tmp[,2] & E(g)$weight>=0)
#total.imbalance <- sum(abs(E(g)$weight)[misplaced])
lpos.imbalance <- E(g)$weight * as.numeric(misplaced & E(g)$weight>=0)
lneg.imbalance <- abs(E(g)$weight) * as.numeric(misplaced & E(g)$weight<0)
npos.imbalance <- sapply(1:vcount(g), function(u) 
	{	idx <- which(el[,1]==u | el[,1]==u)
		result <- sum(lpos.imbalance[idx])
		return(result)
	})
nneg.imbalance <- sapply(1:vcount(g), function(u) 
	{	idx <- which(el[,1]==u | el[,1]==u)
		result <- sum(lneg.imbalance[idx])
		return(result)
	})
vals <- paste(npos.imbalance,nneg.imbalance,sep=",")
nh <- cbind(grp,beg.pos,end.pos,vals)
nh <- nh[order(nh[,1],as.numeric(nh[,2])),]
write.table(nh, file.path(out.folder,"hist.txt"), sep="\t", col.names=FALSE, row.names=FALSE, quote=FALSE)

# links
idx <- 1:nrow(el)
restrict.links <- NA
#	# only keep the links connecting people in the same group
#	restrict.group <- GROUP.EPP # GROUP.GREENS GROUP.SD GROUP.ALDE, GROUP.EPP
#	idx <- which(md[el[,1],COL.GROUP]==restrict.group | md[el[,2],COL.GROUP]==restrict.group)
#	el <- el[idx,]
#	# only keep the links connecting people in the same cluster
#	restrict.clust <- 4
#	restrict.links <- paste0("Cluster",restrict.clust)
#	idx <- which(md[el[,1],"CLUSTER_ID"]==restrict.clust | md[el[,2],"CLUSTER_ID"]==restrict.clust)
#	el <- el[idx,]
grp1 <- grp[el[,1]]
beg.pos1 <- beg.pos[el[,1]]
end.pos1 <- end.pos[el[,1]]
grp2 <- grp[el[,2]]
beg.pos2 <- beg.pos[el[,2]]
end.pos2 <- end.pos[el[,2]]
col <- sapply(E(g)$weight[idx],function(w) if(w>=0)
		"color=greens-13-seq-9_a5" else "color=reds-3-seq-3_a5"
#		"color=(0,255,0,1)" else "color=(255,0,0,0.8)"      -- last color value=transparency, 0-127 (does not work...)
) # green vs. red
set.seed(1)	# fix the random number generator
col <- paste0(col,rep(",bezier_radius=",length(col)),
		runif(n=length(col),min=0,max=0.8),		# random bezier
#		seq(from=0,to=0.8,length.out=length(col)), # ordered bezier
		rep("r",length(col))
)
col <- paste0(col,rep(",thickness=",length(col)),abs(E(g)$weight[idx])*20+5)
sl <- cbind(grp1,beg.pos1,end.pos1, grp2,beg.pos2,end.pos2, col) # group1	0	1	group1	2	3	color=red
#sl <- sl[order(sl[,1],as.numeric(sl[,2]),sl[,4],as.numeric(sl[,5])),]	# order by node
sl <- sl[order(E(g)$weight[idx],decreasing=TRUE),]	# order by weight
write.table(sl, file.path(out.folder,"links.txt"), sep="\t", col.names=FALSE, row.names=FALSE, quote=FALSE)

# nodes
chr <- rep("chr",ngrp)
hyphen <- rep("-",ngrp)
grp.cds <- as.numeric(sort(unique(md[,"GROUP_ID"])))
grp.ids <- paste0("group",grp.cds)
grp.names <- GROUPS_ORDERED[grp.cds]
grp.min <- rep(0,ngrp)
grp.max <- sapply(grp.cds, function(code) max(as.numeric(end.pos[which(md[,"GROUP_ID"]==code)])))
col <- CIRCOS_GROUP_COLORS[grp.cds]
cs <- cbind(chr,hyphen,grp.ids,grp.names,grp.min,grp.max,col) # chr	-	group1	GUE-NGL	0	3	red     ,
band <- rep("band",nrow(md))
nid <- paste0("n",md[,"NODE_ID"])
col <- rep("white",nrow(md)) # color is compulsory, but later overriden
bs <- cbind(band,grp,nid,node.names,beg.pos,end.pos,col) # band	group1	n2	Mep2	1	2	white
as <- rbind(cs,bs)
as <- as[order(as[,2],as.numeric(as[,5])),]
write.table(as, file.path(out.folder,"nodes.txt"), sep="\t", col.names=FALSE, row.names=FALSE, quote=FALSE)        

# clusters
col <- cluster.cols[as.numeric(md[,"CLUSTER_ID"])]
cl <- cbind(grp, beg.pos, end.pos, col) # group1	0	1	fill_color=red
cl <- cl[order(cl[,1],as.numeric(cl[,2])),]
write.table(cl, file.path(out.folder,"clusters.txt"), sep="\t", col.names=FALSE, row.names=FALSE, quote=FALSE)

# node names
nn <- cbind(grp,beg.pos,end.pos,node.names) # group1	0	1	Mep1
nn <- nn[order(nn[,1],as.numeric(nn[,2])),]
write.table(nn, file.path(out.folder,"node_names.txt"), sep="\t", col.names=FALSE, row.names=FALSE, quote=FALSE)



#############################################################################################
# apply circos
CIRCOS_FOLDER <- "/home/vlabatut/Downloads/circos-0.69-5"
conf.file <- file.path(out.folder,"circos.conf")
if(is.na(restrict.links))
{	out.file <- "all_links.png"
}else
	out.file <- paste0(restrict.links,".png")
circos.cmd <- paste0(CIRCOS_FOLDER,"/bin/circos -conf ",conf.file," -outputdir ",out.folder," -outputfile ",out.file)
system(command=circos.cmd)
