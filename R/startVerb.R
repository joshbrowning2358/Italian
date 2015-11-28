##' Start Verb Quiz
##' 
##' This function gets the user name which then defines the file needed to run.
##' 
##' @return The user name, as a character string.
##' 

startVerb <- function(){
    name = readline("Please input your name: ")
#     if(paste0(name,".RData") %in% list.files())
#         load(paste0(name,".RData"))
    if(paste0(name,"Verb.csv") %in% list.files()){
        verbFile = read.csv(paste0(name,"Verb.csv"), stringsAsFactors = FALSE)
        verbFile = data.table(verbFile)
    }
    if(exists("verbFile"))
        verbFile <<- verbFile
    return(name)
}