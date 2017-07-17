#############################################################################################
#
# setwd("D:/Eclipse/workspaces/Networks/NetVotes")
# setwd("~/eclipse/workspaces/Networks/NetVotes")
# source("src/circos/circos.R")
#############################################################################################
source("src/define-imports.R")




#############################################################################################
# GENERAL CONSTANTS
#############################################################################################
# location and name of the Circos command in the Circos folder 
CIRCOS_CMD <- "/home/vlabatut/Downloads/circos-0.69-5/bin/circos"	# TODO this should be changed according to your installation
# location of the temp Circos file in the project
CIRCOS_RES <- "res/circos"
# extension of the Circos configuration files
CIRCOS_CONFIG_FILE <- "circos.conf"
# name of the file containing the histogram data
CIRCOS_HISTO_FILE <- "hist.txt"
# name of the file describing the links
CIRCOS_LINKS_FILE <- "links.txt"
# name of the file describing the nodes
CIRCOS_NODES_FILE <- "nodes.txt"
# name of the file describing the node names
CIRCOS_NAMES_FILE <- "node_names.txt"
# name of the file describing the clusters
CIRCOS_CLUSTERS_FILE <- "clusters.txt"
# name of the output file representing the whole network
CIRCOS_OVERALL_NET <- "all_links.png"

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
# color used for isolates
CIRCOS_CLUSTER_WHITE <- "fill_color=(255,255,255)"

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
CIRCOS_GROUPS_ORDERED <- c(GROUP.GUENGL, GROUP.GREENS, GROUP.SD, GROUP.ALDE, GROUP.EPP, GROUP.ECR, GROUP.EFD, GROUP.NI)

# formated names of the groups
GROUPS_FULLNAMES <- c()
GROUPS_FULLNAMES[GROUP.GUENGL] <- "European United Left-Nordic Green Left (GUE-NGL)"
GROUPS_FULLNAMES[GROUP.GREENS] <- "Greens-European Free Alliance (G-EFA)"
GROUPS_FULLNAMES[GROUP.SD] <- "Progressive Alliance of Socialists and Democrats (S&D)"
GROUPS_FULLNAMES[GROUP.ALDE] <- "Alliance of Liberals and Democrats for Europe (ALDE)"
GROUPS_FULLNAMES[GROUP.EPP] <- "European People's Party (EPP)"
GROUPS_FULLNAMES[GROUP.ECR] <- "European Conservatives and Reformists (ECR)"
GROUPS_FULLNAMES[GROUP.EFD] <- "Europe of Freedom and Democracy (EFD)"
GROUPS_FULLNAMES[GROUP.NI] <- "Non-Inscrits (NI)"




#############################################################################################
# BASE CIRCOS CONFIGURATION FILE
#############################################################################################
# ideogram part (general plot configuration)
CIRCOS_CONFIGURATION_IDEOGRAM <- 
"# override column delimitor
file_delim* = \\t

# node file
karyotype = nodes.txt

# plot formatting
<ideogram>
	<spacing>
		default = 0.005r
		<pairwise group<<<lastgroup>>> group<<<firstgroup>>>>
			spacing = 10r
		</pairwise>
	</spacing>

	# political groups
	radius = 1r
	thickness = 75p
	fill = yes
	stroke_color = dgrey
	stroke_thickness = 2p

	# group names
	show_label = yes
	label_font = default
	label_radius = (dims(ideogram,radius_outer)+dims(ideogram,radius_inner))/2
	label_center = yes
	label_size = 30
	label_parallel = yes
</ideogram>

#<<include tracks/link.conf>>
#<<include /etc/circos/tracks/link.conf>>\n\n"

# part concerning the graph links
CIRCOS_CONFIGURATION_LINKS <- 
"# display the links
<links>
	<link>
		file = links.txt
		radius = <<<linkradius>>>r
		bezier_radius = <<<bezradius>>>r
		ribbon = <<<ribbon>>>
		thickness = 5
	</link>
</link>

<plots>\n"

# part concerning the node names
CIRCOS_CONFIGURATION_PLOTS_NAMES <- 
"	# node names
	<plot>
		type = text
		file = node_names.txt
		color = black
		r0 = <<<namesinside>>>r
		r1 = <<<namesoutside>>>r
		label_font = default
		label_size = 23p #25p
	</plot>\n"

# part concerning the node clusters
CIRCOS_CONFIGURATION_PLOTS_CLUSTER <- 
"	# cluster colors
	<plot>
		type = highlight
		file = clusters.txt
		r0 = <<<clusinside>>>r
		r1 = <<<clusoutside>>>r
	</plot>\n"

# part concerning the node histograms
CIRCOS_CONFIGURATION_PLOTS_HISTO <- 
"	# node imbalance (as a histogram)
	<plot>
		type = histogram
		file = hist.txt
		r0 = <<<histinside>>>r
		r1 = <<<histoutside>>>r
#		fill_color= green,red
		orientation = out
	</plot>\n"

# general part concerning the produced image
CIRCOS_CONFIGURATION_IMAGE <- 
"</plots>

<image>
	dir = /home/vlabatut/Downloads/circos-test
	file = circos.png
	png = no
	svg = yes
	radius = 1500p
	angle_offset = -100 		# -90 minus some angle depending on the total number of MEPs and groups... don't know how to compute that automatically (seems possible, though)
	angle_orientation = counterclockwise
	auto_alpha_colors = yes
	auto_alpha_steps = 5
	background = white
</image>

<<include colors_fonts_patterns.conf>>
<<include housekeeping.conf>>\n"	




#############################################################################################
# Produces plots of the specified graph, using the Circos tool.
# 
# g: signed graph that will be plotted.
# partition: integer vector indicating the cluster for each node in the graph.
# absences: number of times the MEP was present for a vote.
# mep.details: table representing all the MEPs and their information (not only those from the graph).
# show.names: if TRUE, the MEP names are included in the plot.
# show.histos: if TRUE, the imbalance participation of each node is shown as a histogram.
# show.clusters: if TRUE, displays the cluster as a colored strip.
# out.folder: where to write all the circos files.
# clean.files: remove the files generated for Circos, which are not needed anymore.
#############################################################################################
produce.circos.plot <- function(g, partition, absences=NA, mep.details, show.names=TRUE, show.histos=FALSE, show.clusters=TRUE, out.folder, clean.files=TRUE)
{	# set up the MEP data table
	md <- circos.setup.mepdata(g, partition, absences, mep.details)
	
	# plot the full graph
	circos.convert.data(g, md, show.names, show.histos, show.clusters, group=NA, cluster=NA, out.folder)
	circos.generate.plot(md, show.names, show.histos, show.clusters, group=NA, cluster=NA, out.folder, clean.files)
	
#	# plot the group-related graphs
#	for(group in CIRCOS_GROUPS_ORDERED)
#	{	if(any(md[,COL.GROUP]==group))
#		{	circos.convert.data(g, md, show.names=FALSE, show.histos=FALSE, show.clusters=FALSE, group, cluster=NA, out.folder)
#			circos.generate.plot(md, show.names=FALSE, show.histos=FALSE, show.clusters=FALSE, group, cluster=NA, out.folder, clean.files)
#		}
#	}
#
#	# plot the cluster-related graphs
#	clusters <- sort(unique(partition))
#	for(cluster in clusters)
#	{	circos.convert.data(g, md, show.names=FALSE, show.histos=FALSE, show.clusters=TRUE, group=NA, cluster, out.folder)
#		circos.generate.plot(md, show.names=FALSE, show.histos=FALSE, show.clusters=TRUE, group=NA, cluster, out.folder, clean.files)
#	}
}




#############################################################################################
# Sets up the MEPs data table, for function produce.circos.plot.
# 
# g: signed graph that will be plot.
# partition: integer vector indicating the cluster of each node in the graph.
# absences: number of times the MEP was present for a vote.
# mep.details: table representing all the MEPs and their information (not only those from the graph).
#
# returns: a filtered and completed table representing the necessary MEPs data.
#############################################################################################
circos.setup.mepdata <- function(g, partition, absences, mep.details)
{	# keep only the MEPs present in the graph, in the MEP data table
	idx <- match(V(g)$MEPid,mep.details[,COL.MEPID])
	md <- mep.details[idx,]
	
	# add the partition
	md <- cbind(md,partition)
	colnames(md)[ncol(md)] <- "CLUSTER_ID"
	
	# add the absences counts
	md <- cbind(md, absences)
	colnames(md)[ncol(md)] <- "ABSENCES"
	
	# add the node ids
	md <- cbind(md,1:vcount(g))
	colnames(md)[ncol(md)] <- "NODE_ID"
	
	# add the political ids
	md <- cbind(md,match(md[,COL.GROUP],CIRCOS_GROUPS_ORDERED))
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
# show.histos: if TRUE, the imbalance participation of each node is shown as a histogram.
# show.clusters: if TRUE, displays the cluster as a colored strip.
# group: name of the focus group, or NA if none. If this parameter is not NA, then cluster
#		 is ignored.
# cluster: number id of the focus cluster. If group is not NA, then this parameter is ignored.
#		   If both group and cluster are NA, then the whole network is plotted.
# out.folder: where to write all the circos files.
##########################################################################################
circos.convert.data <- function(g, md, show.names, show.histos, show.clusters, group, cluster, out.folder)
{	# create the output folder if necessary 
	dir.create(path=out.folder, showWarnings=FALSE, recursive=FALSE)
	
	# init some common variables used several times later
	hist.bar.nbr <- 3	# number of histogram bars
	el <- get.edgelist(g)
	grp <- paste0("group",md[,"GROUP_ID"])
	beg.pos <- (as.numeric(md[,"GNODE_ID"]) - 1)*hist.bar.nbr
	end.pos <- as.numeric(md[,"GNODE_ID"])*hist.bar.nbr
	firstnames <- normalize.firstnames(md[,COL.FIRSTNAME])
	lastnames <- normalize.lastnames(md[,COL.LASTNAME])
	node.names <- paste(firstnames,lastnames)
	
	# create the file containing the histograms data (i.e. node participation to imbalance)
	if(show.histos)
	{	tmp <- cbind(md[el[,1],"CLUSTER_ID"],md[el[,2],"CLUSTER_ID"])
		misplaced <- (tmp[,1]==tmp[,2] & E(g)$weight<0) | (tmp[,1]!=tmp[,2] & E(g)$weight>=0)
#		total.imbalance <- sum(abs(E(g)$weight)[misplaced])
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
		npos.imbalance <- npos.imbalance / max(c(npos.imbalance,nneg.imbalance))
		nneg.imbalance <- nneg.imbalance / max(c(npos.imbalance,nneg.imbalance))
		absences <- as.numeric(md[,"ABSENCES"])
		absences <- absences / max(absences)
		nhp <- cbind(grp,beg.pos+2,beg.pos+3,npos.imbalance,"fill_color=green")
		nhn <- cbind(grp,beg.pos+1,beg.pos+2,nneg.imbalance,"fill_color=red")
		nht <- cbind(grp,beg.pos  ,beg.pos+1,absences,"fill_color=yellow")
		nh <- rbind(nhp,nhn,nht)
		nh <- nh[order(nh[,1],as.numeric(nh[,2])),]
		write.table(nh, file.path(out.folder,CIRCOS_HISTO_FILE), sep="\t", col.names=FALSE, row.names=FALSE, quote=FALSE)
	}
	
	# links
	idx <- 1:nrow(el)
	# only keep the links attached to the focus group
	if(!is.na(group))
	{	idx <- which(md[el[,1],COL.GROUP]==group | md[el[,2],COL.GROUP]==group)
		el <- el[idx,]
	}
	# only keep the links attached to the focus cluster
	else if(!is.na(cluster))
	{	idx <- which(md[el[,1],"CLUSTER_ID"]==cluster | md[el[,2],"CLUSTER_ID"]==cluster)
		el <- el[idx,]
	}
	grp1 <- grp[el[,1]]
	beg.pos1 <- beg.pos[el[,1]]
	end.pos1 <- end.pos[el[,1]]
	grp2 <- grp[el[,2]]
	beg.pos2 <- beg.pos[el[,2]]
	end.pos2 <- end.pos[el[,2]]
	col <- sapply(E(g)$weight[idx],function(w) if(w>=0)
					"color=greens-13-seq-9_a5" else "color=reds-3-seq-3_a5"
#					"color=(0,255,0,1)" else "color=(255,0,0,0.8)"      -- last color value=transparency, 0-127 (does not work...)
	) 													# green vs. red
	set.seed(1)											# fix the random number generator
	if(show.names & show.histos & show.clusters)
		max.bezier <- 0.6
	else if (show.names & show.histos)
		max.bezier <- 0.7
	else
		max.bezier <- 0.8
	col <- paste0(col,rep(",bezier_radius=",length(col)),
			runif(n=length(col),min=0,max=max.bezier),	# random bezier
#			seq(from=0,to=0.8,length.out=length(col)), 	# ordered bezier
			rep("r",length(col))
	)
	col <- paste0(col,rep(",thickness=",length(col)),abs(E(g)$weight[idx])*20+5)
	sl <- cbind(grp1,beg.pos1,end.pos1, grp2,beg.pos2,end.pos2, col) 			# group1	0	1	group1	2	3	color=red
#	sl <- sl[order(sl[,1],as.numeric(sl[,2]),sl[,4],as.numeric(sl[,5])),]		# order by node
	sl <- sl[order(E(g)$weight[idx],decreasing=TRUE),]							# order by weight
	write.table(sl, file.path(out.folder,CIRCOS_LINKS_FILE), sep="\t", col.names=FALSE, row.names=FALSE, quote=FALSE)
	
	# create the file describing the nodes
	ngrp <- length(unique(md[,COL.GROUP]))
	chr <- rep("chr",ngrp)
	hyphen <- rep("-",ngrp)
	grp.cds <- as.numeric(sort(unique(md[,"GROUP_ID"])))
	grp.ids <- paste0("group",grp.cds)
#	grp.names <- GROUPS_FULLNAMES[grp.cds] # full names are too long
	grp.names <- CIRCOS_GROUPS_ORDERED[grp.cds]
	grp.min <- rep(0,ngrp)
	grp.max <- sapply(grp.cds, function(code) max(as.numeric(end.pos[which(md[,"GROUP_ID"]==code)])))
	col <- CIRCOS_GROUP_COLORS[grp.cds]
	cs <- cbind(chr,hyphen,grp.ids,grp.names,grp.min,grp.max,col) 				# chr	-	group1	GUE-NGL	0	3	red     ,
	if(show.clusters)
	{	band <- rep("band",nrow(md))
		nid <- paste0("n",md[,"NODE_ID"])
		col <- rep("white",nrow(md)) # color is compulsory, but later overriden
		bs <- cbind(band,grp,nid,node.names,beg.pos,end.pos,col) 				# band	group1	n2	Mep2	1	2	white
		as <- rbind(cs,bs)
		as <- as[order(as[,2],as.numeric(as[,5])),]
	}
	else
		as <- cs
	write.table(as, file.path(out.folder,CIRCOS_NODES_FILE), sep="\t", col.names=FALSE, row.names=FALSE, quote=FALSE)        
	
	# create the file containing the clusters
	if(show.clusters)
	{	col <- CIRCOS_CLUSTER_COLORS[as.numeric(md[,"CLUSTER_ID"])]
		col[degree(g)==0] <- CIRCOS_CLUSTER_WHITE	# set isolates to white
		cl <- cbind(grp, beg.pos, end.pos, col)
		cl <- cl[order(cl[,1],as.numeric(cl[,2])),]								# group1	0	1	fill_color=red
		write.table(cl, file.path(out.folder,CIRCOS_CLUSTERS_FILE), sep="\t", col.names=FALSE, row.names=FALSE, quote=FALSE)
	}
	
	# create the file containing the node names
	if(show.names)
	{	nn <- cbind(grp,beg.pos,end.pos,node.names) 
		nn <- nn[order(nn[,1],as.numeric(nn[,2])),]								# group1	0	1	Mep1
		write.table(nn, file.path(out.folder,CIRCOS_NAMES_FILE), sep="\t", col.names=FALSE, row.names=FALSE, quote=FALSE)
	}
}




#############################################################################################
# This function applies Circos on the previously generated files.
# 
# 
# md: table representing the MEPs present in the considered graph.
# show.names: if TRUE, the MEP names are included in the plot.
# show.histos: if TRUE, the imbalance participation of each node is shown as a histogram.
# show.clusters: if TRUE, displays the cluster as a colored strip.
# group: name of the focus group, or NA if none. If this parameter is not NA, then cluster
#		 is ignored.
# cluster: number id of the focus cluster. If group is not NA, then this parameter is ignored.
#		   If both group and cluster are NA, then the whole network is plotted.
# out.folder: where to write all the circos files.
#############################################################################################
circos.generate.plot  <- function(md, show.names, show.histos, show.clusters, group, cluster, out.folder, clean.files)
{	names.height <- 0.19
	cluster.height <- 0.05
	hist.height <- 0.08
	margin <- 0.01
	
	# compute the relative sizes of the plot elements, depending on the show.xxx options
	if(!show.names & !show.histos & !show.clusters)
	{	link.radius <- 0.99
		bez.radius <- 0.25
	}
	else if(!show.names & !show.histos & show.clusters)
	{	clus.radius.outside <- 0.99
		clus.radius.inside <- clus.radius.outside-cluster.height
		link.radius <- clus.radius.inside-margin
		bez.radius <- 0.25
	}
	else if(!show.names & show.histos & !show.clusters)
	{	hist.radius.outside <- 0.99
		hist.radius.inside <- hist.radius.outside-hist.height
		link.radius <- hist.radius.inside-margin		
		bez.radius <- 0.25
	}
	else if(!show.names & show.histos & show.clusters)
	{	clus.radius.outside <- 0.99
		clus.radius.inside <- clus.radius.outside-cluster.height
		hist.radius.outside <- clus.radius.inside-margin
		hist.radius.inside <- hist.radius.outside-hist.height
		link.radius <- hist.radius.inside-margin
		bez.radius <- 0.25
	}
	else if(show.names & !show.histos & !show.clusters)
	{	names.radius.outside <- 0.99
		names.radius.inside <- names.radius.outside-names.height
		link.radius <- names.radius.inside-margin
		bez.radius <- 0.25
	}
	else if(show.names & !show.histos & show.clusters)
	{	names.radius.outside <- 0.99
		names.radius.inside <- names.radius.outside-names.height
		clus.radius.outside <- names.radius.inside-margin
		clus.radius.inside <- clus.radius.outside-cluster.height
		link.radius <- clus.radius.inside-margin
		bez.radius <- 0.25
	}
	else if(show.names & show.histos & !show.clusters)
	{	names.radius.outside <- 0.99
		names.radius.inside <- names.radius.outside-names.height
		hist.radius.outside <- names.radius.inside-margin
		hist.radius.inside <- hist.radius.outside-hist.height
		link.radius <- hist.radius.inside-margin
		bez.radius <- 0.25
	}
	else if(show.names & show.histos & show.clusters)
	{	names.radius.outside <- 0.99
		names.radius.inside <- names.radius.outside-names.height
		clus.radius.outside <- names.radius.inside-margin
		clus.radius.inside <- clus.radius.outside-cluster.height
		hist.radius.outside <- clus.radius.inside-margin
		hist.radius.inside <- hist.radius.outside-hist.height
		link.radius <- hist.radius.inside-margin
		bez.radius <- 0.00
	}
	
	# add the ideogram part
	temp <- CIRCOS_CONFIGURATION_IDEOGRAM
	first.grp <- min(md[,"GROUP_ID"])
	temp <- gsub(pattern="<<<firstgroup>>>",replacement=first.grp,x=temp,fixed=TRUE)
	last.grp <- max(md[,"GROUP_ID"])
	temp <- gsub(pattern="<<<lastgroup>>>",replacement=last.grp,x=temp,fixed=TRUE)
	config.content <- temp
	
	# add the links part
	temp <- CIRCOS_CONFIGURATION_LINKS
	temp <- gsub(pattern="<<<linkradius>>>",replacement=link.radius,x=temp,fixed=TRUE)
	temp <- gsub(pattern="<<<bezradius>>>",replacement=bez.radius,x=temp,fixed=TRUE)
	if(is.na(group) & is.na(cluster))
		temp <- gsub(pattern="<<<ribbon>>>",replacement="no",x=temp,fixed=TRUE)
	else
		temp <- gsub(pattern="<<<ribbon>>>",replacement="yes",x=temp,fixed=TRUE)
	config.content <- paste0(config.content, temp)
	
	# pssibly add the names part
	if(show.names)
	{	temp <- CIRCOS_CONFIGURATION_PLOTS_NAMES
		temp <- gsub(pattern="<<<namesinside>>>",replacement=names.radius.inside,x=temp,fixed=TRUE)
		temp <- gsub(pattern="<<<namesoutside>>>",replacement=names.radius.outside,x=temp,fixed=TRUE)
		config.content <- paste0(config.content, temp)
	}
	
	# possibly add the clusters part
	if(show.clusters)
	{	temp <- CIRCOS_CONFIGURATION_PLOTS_CLUSTER
		temp <- gsub(pattern="<<<clusinside>>>",replacement=clus.radius.inside,x=temp,fixed=TRUE)
		temp <- gsub(pattern="<<<clusoutside>>>",replacement=clus.radius.outside,x=temp,fixed=TRUE)
		config.content <- paste0(config.content, temp)
	}
	# possibly add the histograms part
	if(show.histos)
	{	temp <- CIRCOS_CONFIGURATION_PLOTS_HISTO
		temp <- gsub(pattern="<<<histinside>>>",replacement=hist.radius.inside,x=temp,fixed=TRUE)
		temp <- gsub(pattern="<<<histoutside>>>",replacement=hist.radius.outside,x=temp,fixed=TRUE)
		config.content <- paste0(config.content, temp)
	}
	
	# add the image part
	temp <- CIRCOS_CONFIGURATION_IMAGE
	config.content <- paste0(config.content, temp)
	
	# record the configuration file
	conf.file <- file.path(out.folder,CIRCOS_CONFIG_FILE)
	writeLines(config.content, conf.file)
	
	# define and execute the command
	if(!is.na(group))
		out.file <- paste0("group=",group,".png")
	else if(!is.na(cluster))
		out.file <- paste0("cluster=",cluster,".png")
	else
		out.file <- CIRCOS_OVERALL_NET
	circos.cmd <- paste0(CIRCOS_CMD," -conf ",conf.file," -outputdir ",out.folder," -outputfile ",out.file)
	system(command=circos.cmd)
	
	# possibly remove the Circos files
	if(clean.files)
	{	file.remove(file.path(out.folder,CIRCOS_CONFIG_FILE))
		file.remove(file.path(out.folder,CIRCOS_LINKS_FILE))
		file.remove(file.path(out.folder,CIRCOS_NODES_FILE))
		if(show.clusters)
		file.remove(file.path(out.folder,CIRCOS_CLUSTERS_FILE))
		if(show.histos)
			file.remove(file.path(out.folder,CIRCOS_HISTO_FILE))
		if(show.names)
			file.remove(file.path(out.folder,CIRCOS_NAMES_FILE))
		file.remove(file.path(out.folder,out.file)) # also remove the PNG plot
	}
}




#############################################################################################
# Takes a list of firstnames and returns the corresponding list of initials.
#
# firstnames: list of firstnames
# 
# returns: list of initials.
#############################################################################################
normalize.firstnames <- function(firstnames)
{	result <- sapply(firstnames, function(firstname)
			{	tmp <- strsplit(x=firstname, split=" ", fixed=TRUE)[[1]]
				for(i in 1:length(tmp))
				{	if(grepl("-",tmp[i]))
					{	tmp2 <- strsplit(x=tmp[i], split="-", fixed=TRUE)[[1]]
						tmp2 <- sapply(tmp2,function(s) toupper(paste0(substr(x=s, start=1, stop=1),".")))
						tmp[i] <- paste(tmp2,collapse="-")
					}
					else
						tmp[i] <- paste0(substr(x=tmp[i], start=1, stop=1),".")
				}
				res <- paste(tmp,collapse="")
				return(res)
			})
	return(result)
}




#############################################################################################
# Takes a list of lastnames and tries to shorten them if they are too long, by keeping only
# certain initials.
#
# firstnames: list of lastnames
# 
# returns: list of shortened lastnames.
#############################################################################################
normalize.lastnames <- function(lastnames)
{	result <- sapply(lastnames, function(lastname)
			{	tmp <- strsplit(x=lastname, split=" ", fixed=TRUE)[[1]]
				if(length(tmp)>1)
				{	for(i in 1:(length(tmp)-1))
					{	if(grepl("-",tmp[i]))
						{	tmp2 <- strsplit(x=tmp[i], split="-", fixed=TRUE)[[1]]
							tmp2 <- sapply(tmp2,function(s) toupper(paste0(substr(x=s, start=1, stop=1),".")))
							tmp[i] <- paste(tmp2,collapse="-")
						}
						else if(nchar(tmp[i])>4)
							tmp[i] <- paste0(substr(x=tmp[i], start=1, stop=1),".")
						else
							tmp[i] <- paste0(tmp[i]," ")
					}
					res <- paste(tmp,collapse="")
				}
				else
				{	if(grepl("-",lastname))
					{	tmp2 <- strsplit(x=lastname, split="-", fixed=TRUE)[[1]]
						tmp3 <- sapply(tmp2,function(s) toupper(paste0(substr(x=s, start=1, stop=1),".")))
						tmp3[length(tmp3)] <- tmp2[length(tmp2)]
						res <- paste(tmp3,collapse="-")
					}
					else
						res <- lastname
				}
				return(res)
			})
	return(result)
}




#############################################################################################
# targeted instance
score <- "m3"; thresh <- c(NA,NA); country <- COUNTRY.IT; group=NA; domain=DOMAIN.FEMM; period=DATE.T7.Y1
#score <- "m3"; thresh <- c(NA,NA); country <- COUNTRY.FR; group=NA; domain=DOMAIN.AGRI; period=DATE.T7.TERM

# targeted algo
#algo <- "GRASP-CC_l1_k7_a1_g0_p30"; repetition=1
algo <- "IM"; repetition=1
	
# load the graph
#in.folder <- "/home/vlabatut/eclipse/workspaces/Networks/NetVotes/out/partitions/m3/negtr=NA_postr=NA/bycountry/France/AGRI/2012-13"
#in.folder <- "/home/vlabatut/eclipse/workspaces/Networks/NetVotes/out/partitions/m3/negtr=NA_postr=NA/bycountry/France/AGRI/Term"
graph.folder <- get.networks.path(score, thresh, country, group, domain, period)
g <- read.graph(file.path(graph.folder,"signed.graphml"),format="graphml")

# load MEP meta-data
data <- load.itsyourparliament.data()
mep.details <- data$mep.details

# load the partition
partition.folder <- get.partitions.path(score, thresh, country, group, domain, period, repetition)
#partition <- as.matrix(read.table(file.path(graph.folder,"1/IM-membership.txt")))
#partition <- as.matrix(read.table(file.path(graph.folder,"1/GRASP-CC_l1_k7_a1_g0_p30-membership.txt")))
partition <- as.matrix(read.table(file.path(partition.folder,paste0(algo,"-membership.txt"))))

## load the absence data
#turnout.folder <- get.votes.path(vote="Turnout", country, group, domain, period)
#expressions <- as.matrix(read.csv2(file.path(turnout.folder,"expr-indiv.csv"),check.names=FALSE))
#expressions <- expressions[,VOTE.EXPRESSED]
#tmp <- as.matrix(read.csv2(file.path(turnout.folder,"expr-counts.csv"),check.names=FALSE))
#total <- nrow(tmp) 
#absences <- total - expressions
absences <- NA

# setup the output folder
out.folder <- "/home/vlabatut/Downloads/circos-test/EP3"

# call the function that generates the plot(s)
produce.circos.plot(g, partition, absences, mep.details, show.names=FALSE, show.histos=FALSE, show.clusters=FALSE, out.folder, clean.files=TRUE)






# TODO when names are displayed, we could use their background color to represent the political groups, and therefore hide the explicit group names
# TODO draw the histograms behind the names?
# TODO allow drawing several clusters on the same plot, as several rings?
# TODO plot the cluster to cluster network (and the group to group one, too), and try with the rubbon stuff (ribbon=yes)
