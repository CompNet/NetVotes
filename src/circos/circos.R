#############################################################################################
#
# setwd("D:/Eclipse/workspaces/Networks/NetVotes")
# setwd("~/eclipse/workspaces/Networks/NetVotes")
# source("src/circos/circos.R")
#############################################################################################
source("src/define-imports.R")




#############################################################################################
# CONSTANTS
#############################################################################################
# location of the Circos software TODO this should be changed according to your installation
CIRCOS_FOLDER <- "/home/vlabatut/Downloads/circos-0.69-5"
# location and name of the Circos command
CIRCOS_CMD <- file.path(CIRCOS_FOLDER,"/bin/circos")
# name of Circos configuration file
CIRCOS_CONFIG_FILE <- "circos.conf"

# arbitrary colors for up to 9 clusters, taken from http://colorbrewer2.org/#type=qualitative&scheme=Set1&n=9
CIRCOS_CLUSTER_COLORS <- c( 
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

# colors traditionally associated to the political groups
CIRCOS_GROUP_COLORS <- c()
CIRCOS_GROUP_COLORS[GROUP.GUENGL] <- "198,0,0"
CIRCOS_GROUP_COLORS[GROUP.GREENS] <- "115,204,32"
CIRCOS_GROUP_COLORS[GROUP.SD] <- "252,138,138"
CIRCOS_GROUP_COLORS[GROUP.ALDE] <- "255,181,53"
CIRCOS_GROUP_COLORS[GROUP.EPP] <- "127,146,255"
CIRCOS_GROUP_COLORS[GROUP.ECR] <- "46,72,221"
CIRCOS_GROUP_COLORS[GROUP.EFD] <- "104,39,216"
CIRCOS_GROUP_COLORS[GROUP.NI] <- "119,65,29"

# groups in political order, from far-left to far-right
ORDERED_GROUPS <- c(GROUP.GUENGL, GROUP.GREENS, GROUP.SD, GROUP.ALDE, GROUP.EPP, GROUP.ECR, GROUP.EFD, GROUP.NI)




#############################################################################################
# Produces a plot of a graph using the Circos tool.
# 
# g: signed graph that will be plot.
# partition: integer vector indicating the cluster of each node in the graph.
# mep.details: table representing all the MEPs and their information (not only those from the graph).
# show.names: if TRUE, the MEP names are included in the plot.
# focus: if "GROUPS", the function generates one plot for each represented political group,
#		 containing only the links attached to this group. If "CLUSTERS", same thing but
#		 with the clusters. If NA, then all the links are represented at once, and only one
#		 plot is produced.
# out.folder: where to write all the circos files.
#############################################################################################
produce.circos.plot <- function(g, partition, mep.details, show.names, focus=NA, out.folder)
{	# set up the MEP data table
	md <- circos.setup.mepdata(g, partition, mep.details)
	
	# convert the data to circos format (generates the appropriate files)
	circos.convert.data(g, md, show.names, focus, out.folder)
	
	#TODO dédoubler histos + supprimer prénoms qd possible
}




#############################################################################################
# Sets up the MEPs data table, for function produce.circos.plot.
# 
# g: signed graph that will be plot.
# partition: integer vector indicating the cluster of each node in the graph.
# mep.details: table representing all the MEPs and their information (not only those from the graph).
#
# returns: a filtered and completed table representing the necessary MEPs data.
#############################################################################################
circos.setup.mepdata <- function(g, partition, mep.details)
{	# keep only the MEPs present in the graph, in the MEP data table
	idx <- match(V(graph)$MEPid,mep.details[,COL.MEPID])
	md <- mep.details[idx,]
	
	# add the partition
	md <- cbind(md,partition)
	colnames(md)[ncol(md)] <- "CLUSTER_ID"
	
	# add the node ids
	md <- cbind(md,1:vcount(g))
	colnames(md)[ncol(md)] <- "NODE_ID"
	
	# add the political ids
	ngrp <- length(unique(md[,COL.GROUP]))
	md <- cbind(md,match(md[,COL.GROUP],ORDERED_GROUPS))
	colnames(md)[ncol(md)] <- "GROUP_ID"
	
	# add the group-related node id
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
	
	return(md)
}




##########################################################################################
# Produces the files required by Circos, based on the specified data and parameters.
# 
# g: signed graph that will be plot.
# md: table representing the MEPs present in the considered graph.
# show.names: if TRUE, the MEP names are included in the plot.
# focus: if "GROUPS", the function generates one plot for each represented political group,
#		 containing only the links attached to this group. If "CLUSTERS", same thing but
#		 with the clusters. If NA, then all the links are represented at once, and only one
#		 plot is produced.
# out.folder: where to write all the circos files.
##########################################################################################
circos.convert.data <- function(g, md, show.names, focus, out.folder)
{	# create the output folder if necessary 
	dir.create(path=out.folder, showWarnings=FALSE, recursive=FALSE)
	
	# convert to the appropriate format
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
			})
	
	# create the file containing the histograms data (i.e. node participation to imbalance)
	tmp <- cbind(md[el[,1],"CLUSTER_ID"],md[el[,2],"CLUSTER_ID"])
	misplaced <- (tmp[,1]==tmp[,2] & E(g)$weight<0) | (tmp[,1]!=tmp[,2] & E(g)$weight>=0)
#	total.imbalance <- sum(abs(E(g)$weight)[misplaced])
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
#	sl <- sl[order(sl[,1],as.numeric(sl[,2]),sl[,4],as.numeric(sl[,5])),]	# order by node
	sl <- sl[order(E(g)$weight[idx],decreasing=TRUE),]	# order by weight
	write.table(sl, file.path(out.folder,"links.txt"), sep="\t", col.names=FALSE, row.names=FALSE, quote=FALSE)
	
	# create the file describing the nodes
	chr <- rep("chr",ngrp)
	hyphen <- rep("-",ngrp)
	grp.cds <- as.numeric(sort(unique(md[,"GROUP_ID"])))
	grp.ids <- paste0("group",grp.cds)
	grp.names <- ORDERED_GROUPS[grp.cds]
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
	
	# create the file containing the clusters
	# group1	0	1	fill_color=red
	col <- cluster.cols[as.numeric(md[,"CLUSTER_ID"])]
	cl <- cbind(grp, beg.pos, end.pos, col)
	cl <- cl[order(cl[,1],as.numeric(cl[,2])),]
	write.table(cl, file.path(out.folder,"clusters.txt"), sep="\t", col.names=FALSE, row.names=FALSE, quote=FALSE)
	
	# create the file containing the node names
	# group1	0	1	Mep1
	nn <- cbind(grp,beg.pos,end.pos,node.names) 
	nn <- nn[order(nn[,1],as.numeric(nn[,2])),]
	write.table(nn, file.path(out.folder,"node_names.txt"), sep="\t", col.names=FALSE, row.names=FALSE, quote=FALSE)
	
}




#############################################################################################
# This function applies Circos on the previously generated files.
# 
# 
#############################################################################################
circos.apply.formatting <- function(show.names, focus=NA, out.folder)
{	# set up the parameters
	conf.file <- file.path(out.folder,CIRCOS_CONFIG_FILE)
	if(is.na(restrict.links))
		out.file <- "all_links.png"
	else
		out.file <- paste0(restrict.links,".png")
	
	# define the command line
	circos.cmd <- paste0(CIRCOS_CMD," -conf ",conf.file," -outputdir ",out.folder," -outputfile ",out.file)
	# call Circos
	system(command=circos.cmd)
}




#############################################################################################
# load the graph
#in.folder <- "/home/vlabatut/eclipse/workspaces/Networks/NetVotes/out/partitions/m3/negtr=NA_postr=NA/bycountry/France/AGRI/2012-13"
in.folder <- "/home/vlabatut/eclipse/workspaces/Networks/NetVotes/out/partitions/m3/negtr=NA_postr=NA/bycountry/France/AGRI/Term"
g <- read.graph(file.path(in.folder,"signed.graphml"),format="graphml")

# load MEP meta-data
data <- load.itsyourparliament.data()
md <- data$mep.details

# load the partition
#p <- as.matrix(read.table(file.path(in.folder,"1/IM-membership.txt")))
p <- as.matrix(read.table(file.path(in.folder,"1/GRASP-CC_l1_k7_a1_g0_p30-membership.txt")))

# setup the output folder
out.folder <- "/home/vlabatut/Downloads/circos-test/EP"

# call the function that generates the plot(s)
produce.circos.plot(g, partition=p, mep.details=md, show.names=TRUE, focus=NA, out.folder)
