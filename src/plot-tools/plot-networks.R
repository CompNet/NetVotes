#############################################################################################
# Functions used to plot networks.
# 
# 11/2015 Vincent Labatut
#############################################################################################
library("igraph")



#############################################################################################
# Plot the specified signed graph, generating files of the specified formats.
#
# g: signed graph to plot.
# plot.file: base name (and path) of the generated files.
# format: format(s) to handle.
#############################################################################################
plot.network <- function(g, plot.file, format=c("PDF","PNG",NA))
{	# setup node parameters
	vertex.sizes <- 10
	vertex.label <- V(g)$MEPid
	
	# setup link parameters
	if(ecount(g)>0)
	{	edge.colors <- rep(NA,ecount(g))
		edge.colors[E(g)$weight<0] <- adjustcolor("RED", alpha.f=0.5)
		edge.colors[E(g)$weight>0] <- adjustcolor("BLUE", alpha.f=0.5)
		edge.widths <- abs(E(g)$weight)*10
	}
	
	# setup layout
#	lyt <- layout.kamada.kawai(graph=g)
	lyt <- layout.fruchterman.reingold(graph=g)
#	lyt <- layout.circle(graph=g)
	
	# process each specified format
	for(frmt in format)
	{	# set plot file name
		plot.filename <- paste(plot.file,".",frmt,sep="")
		# create the file
		if(!is.na(frmt))
		{	if(frmt=="PNG")
			{	png(filename=plot.filename,width=800,height=800,units="px",pointsize=20,bg="white")
			}
			else if(frmt=="PDF")
			{	pdf(file=plot.filename,bg="white")
			}
		}
		
		# create the plot
		if(ecount(g)>0)
		{	plot(g, layout=lyt, #main=g$name,
				vertex.size=vertex.sizes, vertex.label=vertex.label,
				edge.color=edge.colors, edge.width=edge.widths
#				asp=1
			)
		}
		else
		{	plot(g, layout=lyt,
				vertex.size=vertex.sizes, vertex.label=vertex.label
			)
		}
		title(g$name, cex.main=0.5)
		
		# finalize plot file
		if(!is.na(frmt))
			dev.off()
	}
}
