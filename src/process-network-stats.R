CalculateNetworkMeasures <- function(adjacency.matrix) {
	temp <- adjacency.matrix
	pos.matrix <- adjacency.matrix
	neg.matrix <- adjacency.matrix
	
	pos.matrix[which(pos.matrix < 0)] <- 0
	neg.matrix[which(neg.matrix > 0)] <- 0
	
	pos.matrix[lower.tri(pos.matrix)] <- 0
	neg.matrix[lower.tri(neg.matrix)] <- 0
	
	pos.qtd.edges <- sum(ifelse(pos.matrix != 0,1,0))
	neg.qtd.edges <- sum(ifelse(neg.matrix != 0,1,0))
	
	pos.wght.edges <- sum(pos.matrix)
	neg.wght.edges <- abs(sum(neg.matrix))
	
	total.qtd.edges <- pos.qtd.edges + neg.qtd.edges
	total.wght.edges <- pos.wght.edges + abs(neg.wght.edges)
	
	pos.pctg.edges <- pos.qtd.edges/total.qtd.edges
	pos.pctg.weight <- pos.wght.edges/total.wght.edges
	
	neg.pctg.edges <- neg.qtd.edges/total.qtd.edges
	neg.pctg.weight <- neg.wght.edges/total.wght.edges
	
	edge.measures <- matrix(0,ncol=2,nrow=3)
	edge.measures[1,] <- c(total.qtd.edges,100)
	edge.measures[2,] <- c(pos.qtd.edges,(100*pos.pctg.edges))
	edge.measures[3,] <- c(neg.qtd.edges,(100*neg.pctg.edges))
	colnames(edge.measures) <- c("Total", "Percentage")
	rownames(edge.measures) <- c("All Edges", "Positive Edges", "Negative Edges")
	
	weight.measures <- matrix(0,ncol=2,nrow=3)
	weight.measures[1,] <- c(total.wght.edges,100)
	weight.measures[2,] <- c(pos.wght.edges,(100*pos.pctg.weight))
	weight.measures[3,] <- c(neg.wght.edges,(100*neg.pctg.weight))
	colnames(weight.measures) <- c("Total", "Percentage")
	rownames(weight.measures) <- c("All Edges", "Positive Edges", "Negative Edges")
	
	network.measures <- list("edges" = round(edge.measures,2), "weights" = round(weight.measures,2))
	
	return(network.measures)
}
