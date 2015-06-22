# Folder where all the .csv files are situated
data.dir <- file.path(".","votewatch_data","votes_by_documents")

# Name of the colums (in the .csv files) that concern the MPs. 
# The first one must be the column containing the complete name of the MP 
columns.names.MPs <- c("Name","Member.State","Group") 			

# Name of the colums (in the .csv files) that concern the votes (For,Against,Abstain...)
columns.names.votes <- c("Vote") 

# Name of the colums (in the .csv files) that concern the loyality (Loyal/Rebel)
columns.names.loyalty <- c("Loyal...Rebel.to.political.group") 

# File listing all the vote sessions (ie. "documents") 
docs.filename <- "VoteWatch Europe European Parliament, Council of the EU.csv"	

# Folder where all the files are situated
data.dir2 <- file.path(".","votewatch_data")

# Folder where all the votation files are situated
input.dir <- file.path(".","input_files")

# Folder to store the output files
output.dir <- file.path(".","output_files")

# Folder to store the intermediate files (allvotes and allMPS)
generation.dir <- file.path(".","votewatch_data","intermediate_files")

# Name of the file containing all the MPs info
MPs.filename <- "MPs.csv"

# Name of the file containing all the votes by MPs and by vote session
allvotes.filename <- "allvotes.csv"

loyalty.filename <- "loyalty.csv"

# Name of the directory where all the tables are going to be read to be processed
input.tables.dir <- file.path(".","csv_results")

# Name of the directory where all the plots will be stored
output.charts.dir <- file.path(".","plots")