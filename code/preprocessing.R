#Pre-Processing of all the files
#Function to read all the files from the folder and return a list containing all the MPS
createMPsList <- function(data.dir,votes.files,columns.names.MPs){
  
  temp <- read.csv(file.path(data.dir,votes.files[1]))
  MPs <- temp[ columns.names.MPs ]
  names(MPs) <- c("names","country","political_group")
  warning <- ""
  
  for( i in  2:length(votes.files) ){
    file_name <- file.path(data.dir,votes.files[i])
    temp <- read.csv(file_name)
    MPs.temp <- temp[ columns.names.MPs ]
    names(MPs.temp) <- c("names","country","political_group")
    
    MPs <- merge(MPs,MPs.temp,by.x="names",by.y="names",all=TRUE)
    
    new.MPs <- which(is.na( MPs[,2] )) 				 
    if(length(new.MPs)) MPs[new.MPs, c(2,3) ] <- MPs[new.MPs, c(4,5)]	 
    MPs <- MPs[1:3]									
    
    names(MPs) <- names(MPs) <- c("names","country","political_group")
    
  }
  MPs <- cbind(paste0("m_",rownames(MPs)),MPs)
  names(MPs) <- c("id","names","country","political_group")
  
  write.csv(MPs, file=file.path(generation.dir,MPs.filename),row.names = FALSE)
  
  print(warning)
  
  return(MPs)
}

#Read all the votes documents and return a data frame containing (all the MPs X Votes in Documents)
create.allvotes.table <- function(data.dir,votes.files,columns.names.MPs,columns.names.votes,MPs){
  
  big.table <- data.frame(MP=MPs)
  big.table$ind <- 1:nrow(big.table)
  names(big.table) <- c("mp_id","names","country","political_group","ind")
  
  for( i in  1:length(votes.files) ){
    
    smaller.big.table <- big.table[c("ind","names")]
    
    file_name <- file.path(data.dir,votes.files[i])
    temp <- read.csv(file_name)[c(columns.names.votes,columns.names.MPs[1])]
    names(temp) <- c(columns.names.votes,"names")
    
    merged <- merge(temp,smaller.big.table,by.x="names",by.y="names")
    
    ncol <- ncol(big.table)
    big.table[as.integer(merged$ind),ncol+1] <- merged[columns.names.votes]
    names(big.table)[ncol+1] <- votes.files[i]
  }
  
  write.csv(big.table, file=file.path(generation.dir,allvotes.filename),row.names = FALSE)
  
  return(big.table)
}

CreateLoyaltyTable <- function(data.dir,votes.files,columns.names.MPs,columns.names.loyalty,MPs) {
  
  loyalty.table <- data.frame(MP=MPs)
  loyalty.table$ind <- 1:nrow(loyalty.table)
  names(loyalty.table) <- c("mp_id","names","country","political_group","ind")
  print(columns.names.loyalty)
  
  for( i in  1:length(votes.files) ){
    
    smaller.loyalty.table <- loyalty.table[c("ind","names")]
    
    file_name <- file.path(data.dir,votes.files[i])
    temp <- read.csv(file_name)[c(columns.names.loyalty,columns.names.MPs[1])]
    names(temp) <- c(columns.names.loyalty,"names")
    
    merged <- merge(temp,smaller.loyalty.table,by.x="names",by.y="names")
    
    ncol <- ncol(loyalty.table)
    loyalty.table[as.integer(merged$ind),ncol+1] <- merged[columns.names.loyalty]
    names(loyalty.table)[ncol+1] <- votes.files[i]
  }
  write.csv(loyalty.table, file=file.path(generation.dir,loyalty.filename),row.names = FALSE)
  
  return(loyalty.table)
  
}


if(length(columns.names.MPs)!=3)
  stop(paste0("In file config_Files&Directories.R , variable columns.names.MPs must indicate exactly 3 colum names. Please modify it regarding the .csv files contained in directory ",data.dir))

if(length(columns.names.votes)!=1)		
  stop(paste0("In file config_Files&Directories.R , variable columns.names.vote must indicate exactly 3 colum names. Please modify it regarding the .csv files contained in directory ",data.dir))

#Generate a list of files containing all the vote documents
votes.files <- list.files(data.dir)

#Create a folder "generation.dir" if doesn't exist
dir.create(generation.dir, showWarnings = FALSE) 

#Store all the MPs in a variable
if (!MPs.filename %in% list.files(generation.dir)){ 					
  # If the file was not created before, creates the file with all the MPs
  MPs <- createMPsList(data.dir,votes.files,columns.names.MPs) 		
}else{
  #If the file already exists, read it
  filename <- file.path(generation.dir,MPs.filename)
  MPs <- read.csv(filename)
}
# Renaming the colums of the table "MPs"
names(MPs) <- c("id","names","country","political_group") 		
# Edits the row names of MPs with the MPs ids
rownames(MPs) <- MPs$id 								

#File listing the names of all the vote sessions
filename <- file.path(data.dir2,docs.filename)
docs <- read.csv(filename)

# TODO: change because it's a bit dirty
set <- unlist(strsplit(votes.files,"\\."))[2*(1:length(votes.files))-1]	#dirty
votes.files <- votes.files[order(as.integer(set))] 						#dirty
docs$doc_id <- votes.files 												#dirty


#Create the Big Table, the one containing all the votes and MPs
# Check if the file has been created already
if (!allvotes.filename %in% list.files(generation.dir)){
  # Creates the file and puts the result in the variable "big.table"  
  big.table <- create.allvotes.table(data.dir,votes.files,columns.names.MPs,columns.names.votes,MPs) 	
}else{
  # Reads the file and puts the result in the variable "big.table"  			
  filename <- file.path(generation.dir,allvotes.filename)
  big.table <- read.csv(filename,check.names=FALSE)
}
rownames(big.table) <- big.table$mp_id 	# edits the row names of big.table with the MPs ids

# Check if the file has been created already
if (!loyalty.filename %in% list.files(generation.dir)){
  # Creates the file and puts the result in the variable "big.table"  
  loyalty.table <- CreateLoyaltyTable(data.dir,votes.files,columns.names.MPs,columns.names.loyalty,MPs)   
}else{
  # Reads the file and puts the result in the variable "big.table"  			
  filename <- file.path(generation.dir,loyalty.filename)
  loyalty.table <- read.csv(filename,check.names=FALSE)
}
rownames(loyalty.table) <- loyalty.table$mp_id   # edits the row names of big.table with the MPs ids