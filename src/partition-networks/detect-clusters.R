#############################################################################################
# 
# 
# 07/2015 Israel Mendonça (v1)
# 09/2015 Vincent Labatut (v2)
#############################################################################################

# Clusterization Measures
CalculateClusterization <- function(adjacency.matrix,more.filtered.table,rebelion.indexes) {
	
	gc <- graph.adjacency(adjacency.matrix, mode = c("upper"), weighted = TRUE, diag = FALSE, add.rownames = TRUE)
	
	positive.adjacency.matrix <- adjacency.matrix
	positive.adjacency.matrix[which(positive.adjacency.matrix < 0)] <- 0
	positive.adjacency.matrix <- round(positive.adjacency.matrix,2)
	gp <- graph.adjacency(positive.adjacency.matrix, mode = c("upper"), weighted = TRUE, diag = FALSE, add.rownames = TRUE)
	
	negative.adjacency.matrix <- adjacency.matrix
	negative.adjacency.matrix[which(negative.adjacency.matrix > 0)] <- 0
	negative.adjacency.matrix <- round(negative.adjacency.matrix,2)
	
	complementary.negative.adjacency.matrix <- negative.adjacency.matrix
	complementary.negative.adjacency.matrix[which(negative.adjacency.matrix >= 0)] <- 1
	complementary.negative.adjacency.matrix[which(negative.adjacency.matrix < 0)] <- 0
	gcm <- graph.adjacency(complementary.negative.adjacency.matrix, mode = c("upper"), weighted = TRUE, diag = FALSE, add.rownames = TRUE)
	
	info.gp <- infomap.community(gp)  
	mult.gp <- multilevel.community(gp)
	fast.gp <- fastgreedy.community(gp)
	walk.gp <- walktrap.community(gp, steps = 10)
	betw.gp <- betweenness.estimate(gp, directed = FALSE, cutoff = 0)
	
	info.cmp <- infomap.community(gcm)
	mult.cmp <- multilevel.community(gcm)
	fast.cmp <- fastgreedy.community(gcm)
	walk.cmp <- walktrap.community(gcm, steps = 10)
	
	imb.info.positive.graph <- CalculateCCImbalance(adjacency.matrix,info.gp$membership)
	imb.mult.positive.graph <- CalculateCCImbalance(adjacency.matrix,mult.gp$membership)
	imb.fast.positive.graph <- CalculateCCImbalance(adjacency.matrix,membership(fast.gp))
	imb.walk.positive.graph <- CalculateCCImbalance(adjacency.matrix,membership(walk.gp))
	
	imb.info.negative.complementary.graph <- CalculateCCImbalance(adjacency.matrix,info.cmp$membership)
	imb.mult.negative.complementary.graph <- CalculateCCImbalance(adjacency.matrix,mult.cmp$membership)
	imb.fast.negative.complementary.graph <- CalculateCCImbalance(adjacency.matrix,membership(fast.cmp))
	imb.walk.negative.complementary.graph <- CalculateCCImbalance(adjacency.matrix,membership(walk.cmp))
	
	imbalance <- matrix(0,nrow=9,ncol=7)
	imbalance[1,1] <- length(communities(info.gp))
	imbalance[2,1] <- length(communities(info.cmp))
	imbalance[3,1] <- length(communities(mult.gp))
	imbalance[4,1] <- length(communities(mult.cmp))
	imbalance[5,1] <- length(communities(fast.gp))
	imbalance[6,1] <- length(communities(fast.cmp))
	imbalance[7,1] <- length(communities(walk.gp))
	imbalance[8,1] <- length(communities(walk.cmp))
	
	imbalance[1,2:7] <- unlist(imb.info.positive.graph)
	imbalance[2,2:7] <- unlist(imb.info.negative.complementary.graph)
	imbalance[3,2:7] <- unlist(imb.mult.positive.graph)
	imbalance[4,2:7] <- unlist(imb.mult.negative.complementary.graph)
	imbalance[5,2:7] <- unlist(imb.fast.positive.graph)
	imbalance[6,2:7] <- unlist(imb.fast.negative.complementary.graph)
	imbalance[7,2:7] <- unlist(imb.walk.positive.graph)
	imbalance[8,2:7] <- unlist(imb.walk.negative.complementary.graph)
	
	colnames(imbalance) <- c("Clusters", "Total_Imb", "Total_Imb_Pctg", "Pos_Imb", "Pos_Imb_Pctg", "Neg_Imb", "Neg_Imb_Pctg")
	rownames(imbalance) <- c("Positive Graph InfoMap", "Complementary Negative Graph InfoMap",
			"Positive Graph MultiLevel", "Complementary Negative Graph MultiLevel",
			"Positive Graph FastGreedy", "Complementary Negative Graph FastGreedy",
			"Positive Graph WalkTrap", "Complementary Negative Graph WalkTrap","Parallel ILS")
	
	try({
				ils <- ILSMembership(file.title)
				
				ils.imb <-  CalculateCCImbalance(adjacency.matrix,ils)
				imbalance[9,1] <- max(ils)
				imbalance[9,2:7] <- unlist(ils.imb)
				
				vi.ils.info <- compare.communities(info.gp,ils, method = c("vi"))
				nmi.ils.info <- compare.communities(info.gp,ils, method = c("nmi"))
				
				vi.ils.mult <- compare.communities(mult.gp,ils, method = c("vi"))
				nmi.ils.mult <- compare.communities(mult.gp,ils, method = c("nmi"))
				
				vi.ils.fast <- compare.communities(fast.gp,ils, method = c("vi"))
				nmi.ils.fast <- compare.communities(fast.gp,ils, method = c("nmi"))
				
				vi.ils.walk <- compare.communities(walk.gp,ils, method = c("vi"))
				nmi.ils.walk <- compare.communities(walk.gp,ils, method = c("nmi"))
				
				comparison <- matrix(0,ncol=2,nrow=4)
				comparison[1,] <- c(vi.ils.info,nmi.ils.info)
				comparison[2,] <- c(vi.ils.mult,nmi.ils.mult)
				comparison[3,] <- c(vi.ils.fast,nmi.ils.fast)
				comparison[4,] <- c(vi.ils.walk,nmi.ils.walk)
				
				colnames(comparison) <- c("VI", "NMI")
				rownames(comparison) <- c("InfoMap", "MultiLevel", "FastGreedy", "WalkTrap")
				
				nome.file <- file.path(output.community.dir,paste0(dir.title,"/",file.title,"_ils_cluster_comparison.csv"))
				write.csv(comparison,file=nome.file)
			})
	
	vi.info <- compare.communities(info.gp,info.cmp, method = c("vi"))
	vi.mult <- compare.communities(mult.gp,mult.cmp, method = c("vi"))
	vi.fast <- compare.communities(fast.gp,fast.cmp, method = c("vi"))
	vi.walk <- compare.communities(walk.gp,walk.cmp, method = c("vi"))
	
	nmi.info <- compare.communities(info.gp,info.cmp, method = c("nmi"))
	nmi.mult <- compare.communities(mult.gp,mult.cmp, method = c("nmi"))
	nmi.fast <- compare.communities(fast.gp,fast.cmp, method = c("nmi"))
	nmi.walk <- compare.communities(walk.gp,walk.cmp, method = c("nmi"))
	
	spjn.info <- compare.communities(info.gp,info.cmp, method = c("split.join"))
	spjn.mult <- compare.communities(mult.gp,mult.cmp, method = c("split.join"))
	spjn.fast <- compare.communities(fast.gp,fast.cmp, method = c("split.join"))
	spjn.walk <- compare.communities(walk.gp,walk.cmp, method = c("split.join"))
	
	rand.info <- compare.communities(info.gp,info.cmp, method = c("rand"))
	rand.mult <- compare.communities(mult.gp,mult.cmp, method = c("rand"))
	rand.fast <- compare.communities(fast.gp,fast.cmp, method = c("rand"))
	rand.walk <- compare.communities(walk.gp,walk.cmp, method = c("rand"))
	
	adrand.info <- compare.communities(info.gp,info.cmp, method = c("adjusted.rand"))
	adrand.mult <- compare.communities(mult.gp,mult.cmp, method = c("adjusted.rand"))
	adrand.fast <- compare.communities(fast.gp,fast.cmp, method = c("adjusted.rand"))
	adrand.walk <- compare.communities(walk.gp,walk.cmp, method = c("adjusted.rand"))
	
	comparison <- matrix(0,ncol=5,nrow=4)
	comparison[1,] <- c(vi.info,nmi.info,spjn.info,rand.info,adrand.info)
	comparison[2,] <- c(vi.mult,nmi.mult,spjn.mult,rand.mult,adrand.mult)
	comparison[3,] <- c(vi.fast,nmi.fast,spjn.fast,rand.fast,adrand.fast)
	comparison[4,] <- c(vi.walk,nmi.walk,spjn.walk,rand.walk,adrand.walk)
	
	colnames(comparison) <- c("VI", "NMI", "Split Join", "Rand Index", "Adjusted Rand Index")
	rownames(comparison) <- c("InfoMap (Positve and Complementary Negative)", "MultiLevel (Positve and Complementary Negative)",
			"FastGreedy (Positve and Complementary Negative)", "WalkTrap (Positve and Complementary Negative)")
	
	
	reply <- list("cluster.information" = imbalance, "cluster.comparison" = comparison)
	
	a <- rownames(more.filtered.table)
	names <- as.character(MPs[a,2]) #Get the names of each selected MeP
	countries <- as.character(MPs[a,3]) #Get the country of each selected MeP
	political_groups <- as.character(MPs[a,4]) #Get the political group of each selected MeP
	V(gc)$name <- names
	V(gc)$country <- countries
	V(gc)$political_group <- political_groups
	V(gc)$rebelion_index <- rebelion.indexes
	
	V(gc)$infomap_positive_community <- info.gp$membership
	V(gc)$multilevel_positive_community <- mult.gp$membership
	V(gc)$fastgreedy_positive_community <- membership(fast.gp)
	V(gc)$walktrap_positive_community <- membership(walk.gp)
	
	V(gc)$infomap_comp_neg_community <- info.cmp$membership
	V(gc)$multilevel_comp_neg_community <- mult.cmp$membership
	V(gc)$fastgreedy_comp_neg_community <- membership(fast.cmp)
	V(gc)$walktrap_comp_neg_community <- membership(walk.cmp)
	
	#write the membership vector to a file
	cat(info.gp$membership, file = file.path(output.community.dir,paste0(dir.title,"/",file.title,"_infomap_community.txt")), sep=",")
	cat(mult.gp$membership, file = file.path(output.community.dir,paste0(dir.title,"/",file.title,"_multilevel_community.txt")), sep=",")
	cat(membership(fast.gp), file = file.path(output.community.dir,paste0(dir.title,"/",file.title,"_fastgreedy_community.txt")), sep=",")
	cat(membership(walk.gp), file = file.path(output.community.dir,paste0(dir.title,"/",file.title,"_walktrap_community.txt")), sep=",")
	
	write.graph(graph = gc, file = file.path(output.graphs.dir,paste0(dir.title,"/",file.title,"_complete_graph.graphml")),format = "graphml")
	
	pdf(file=file.path(output.community.dir,paste0(dir.title,"/",file.title,"_infomap_distribution.pdf")))
	barplot(sort(table(info.gp$membership),decreasing = TRUE), ylim=c(0,840), main = "InfoMap - Community Distribution",
			col = rainbow(length(table(info.gp$membership))))
	dev.off()
	
	pdf(file=file.path(output.community.dir,paste0(dir.title,"/",file.title,"_multilevel_distribution.pdf")))
	barplot(sort(table(mult.gp$membership),decreasing = TRUE), ylim=c(0,840),main = "Multilevel - Community Distribution",
			col = rainbow(length(table(mult.gp$membership))))
	dev.off()
	
	pdf(file=file.path(output.community.dir,paste0(dir.title,"/",file.title,"_fastgreedy_distribution.pdf")))
	barplot(sort(table(membership(fast.gp)),decreasing = TRUE), ylim=c(0,840), main = "FastGreedy - Community Distribution",
			col = rainbow(length(table(membership(fast.gp)))))
	dev.off()
	
	pdf(file=file.path(output.community.dir,paste0(dir.title,"/",file.title,"_walktrap_distribution.pdf")))
	barplot(sort(table(membership(walk.gp)),decreasing = TRUE), ylim=c(0,840), main = "Walktrap - Community Distribution", 
			col = rainbow(length(table(membership(walk.gp)))))
	dev.off()
	
	try({
				ils_members <- ILSMembership(file.title)
				V(gc)$ils_community <- ils_members
				pdf(file=file.path(output.community.dir,paste0(dir.title,"/",file.title,"_ils_distribution.pdf")))
				barplot(sort(table(ils_members),decreasing = TRUE), ylim=c(0,840), main = "Parallel ILS - Community Distribution",
						col = rainbow(length(table(ils_members))))
				dev.off()
			})
	
	return(reply)
}
