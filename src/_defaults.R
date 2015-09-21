# Setting the defauld variable for each input file
MP.political_groups <- c("all")
MP.countries        <- c("all")
MP.mp_ids           <- c("all")
MP.group_by         <- c("DontGroup") # possibilities : "country" , "political_group" , "dontGroup"
MP.group_by.mode    <- c("avg_agreement_between_MPs_of_each_group") # possibilities :"DontGroup" , "majority_vote_in_each_group" , "min_each_vote_possibility_between_groups" , "avg_agreement_between_MPs_of_each_group"
DOCS.time_limits    <- c("01/01/0001","31/12/9999")
DOCS.policies       <- c("all")
table               <- c("2")

alpha <- 0
OUTPUTFILES.Gfile.weigth <- "%agree-%disagree" # possibilities : "+1_or_-1" , "%agree-%disagree"

OUTPUTFILES.Gfile            <- TRUE
OUTPUTFILES.Histo            <- TRUE
OUTPUTFILES.GephiLinkTable   <- TRUE
OUTPUTFILES.Corresp          <- TRUE
OUTPUTFILES.GEXF             <- FALSE
OUTPUTFILES.RANKING          <- TRUE