
############################################################################################################
# when you have 2 data.frames which are supposed to contain the same observations
# (i.e. the same networks in this context), you need to remove unmatched observations from both of them.
# For doing that, iterate over the first data.frame and remove the observations that data.frame2 does not have.
# Then, iterate over the second data.frame and remove the observations that data.frame1 does not have.
#
# returns cleaned data.frame1 and cleaned data.frame2
#############################################################################################################
remove.unmatched.obs = function(df1, df2){
  indx = sapply(seq(1, nrow(df1)), function(i){

      # Given a set of logical vectors, is at least one of the values true?
      x1 = df1[i, ]
      return( any(df2[, "target"] == x1$target & df2[, "domain"] == x1$domain & df2[, "period"] == x1$period) )
  })
#  print(seq(1,nrow(df1))[!indx])
#  print(df1[!indx,c("target","domain","period")])
 
  df1 = df1[indx,]

 #print(indx)


  indx = sapply(seq(1, nrow(df2)), function(i){

      x2 = df2[i, ]
      # Given a set of logical vectors, is at least one of the values true?
      return( any(df1[, "target"] == x2$target & df1[, "domain"] == x2$domain & df1[, "period"] == x2$period) )
  })
#  print(seq(1,nrow(df2))[!indx])
#  print(df2[!indx,c("target","domain","period")])
 
  df2 = df2[indx,]
  

  res = list()
  res$df1 = df1
  res$df2 = df2
  return(res)
}


##########################################################################################################


############################################################################################################
# To do a comparison between 2 partitions, the length of them must be the same.
# I dont know how but we have a couple of erroneous observations that the lengths are not correct
# This function identify the observations where length(partition1) != length(partition2)
#
# We suppose that these 2 partitions in input parameter are of the same size
#
# returns the index of partitions that we should remove from data.frame in order to clean data
#############################################################################################################
identify.erroneous.obs = function(partition1, partition2){
  
  nb.obs = length(partition1)

  res = sapply(seq(1, nb.obs), function(i){
    p1 = partition1[[i]]
    p2 = partition2[[i]]
  
    #cat("i: ", i, " p1: ", length(p1), " p2: ", length(p2), "\n")
    #nmi.scor = compare(p1, p2, method = "nmi")
    #return(nmi.scor)
    return( as.integer(length(p1) != length(p2)) )
  })

  indx = which(res == 1)
  return(indx)
}

########################################################################################################
# This function removes erroneous obsevations whose indexes are indicated in the variable "indx.to.be.removed"
#
# returns cleaned data.frame
########################################################################################################
remove.erreonous.obs = function(df, indx.to.be.removed){

  if(length(indx.to.be.removed) > 0){
    df = df[-indx.to.be.removed, ]
  }

  return(df)
}



