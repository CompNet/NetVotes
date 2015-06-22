source(file.path("./code","ioConstants.R"))
print("Chart Processing Method Launched")

ProcessData <- function(in.dir, out.dir,title) {
  #Read all the CSVs from a directory
  temp <- list.files(in.dir,pattern=".*\\.csv")
  for (i in 1:length(temp)) assign(temp[i], read.csv(file.path(in.dir,temp[i])))
  
  #Gather the number of clusters for each year and algorithm
  cluster.list <- matrix(0,ncol=length(temp),nrow=length(eval(parse(text=temp[1]))$Clusters))
  for (i in 1:length(temp)) {
    cluster.list[,i] <- eval(parse(text=temp[i]))$Clusters
  }
  
  #Gather the percentage of imbalance for each year and algorithm
  imbalance.list <- matrix(0,ncol=length(temp),nrow=length(eval(parse(text=temp[1]))$Total_Imb_Pctg))
  rownames(imbalance.list) <- c("Pos Infomap", "Comp Neg Infomap", "Pos Multilevel", "Comp Neg Multilevel", 
                              "Pos FastGreedy", "Comp Neg FastGreedy", "Pos WalkTrap", "Comp Neg WalkTrap")
  colnames(imbalance.list) <- c("2009","2010","2011","2012","2013","term")
  for (i in 1:length(temp)) {
    imbalance.list[,i] <- eval(parse(text=temp[i]))$Total_Imb_Pctg
  }
  
  # -X-X-X-X-X-X- Code to Generate the Plot -X-X-X-X-X-X-
  
  #Call the function jpeg to save the data
  jpeg(width = 890, height = 606, file=file.path(out.dir ,paste0(title,".peg")))
  
  #Adjust the Margin of the plot for the names to appear. Default = c(5,4,4,2) + 0.1
  par(mar = c(10,4,4,2) + 0.1)
  
  #Plot the data
  xx <- barplot(t(imbalance.list), beside=T,col=rainbow(6), las=3,ylim=c(0,80),cex.names=1)  
  
  mtext(side=3,title,line=1,cex=2)
  mtext(side=3,"Whole term",line=-1,cex=1.5)
  
  #Add the Legend at the top right
  legend("topright", 
         legend = c("2009","2010","2011","2012","2013","Term"), 
         fill = rainbow(6), ncol = 2,
         cex = 1)
  
  #Add the number of clusters above each bar
  text(x = xx, y = unlist(as.list(t(imbalance.list))), label = unlist(as.list(t(cluster.list))), pos = 3, cex = 0.8, col = "blue")
  
  #Turn of the ploting device
  dev.off()
  
}

sub.dirs <- list.files(input.tables.dir)
for(i in 1:length(sub.dirs)) {
  try({
    input.dir <- paste0(input.tables.dir,"/",sub.dirs[i])
    output.dir <- paste0(output.charts.dir,"/",sub.dirs[i])
    ProcessData(input.dir,output.dir,sub.dirs[i])
  })
}