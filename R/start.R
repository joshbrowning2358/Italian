##' Start
##' 
##' This function begins the program by getting the user name.
##' 
##' @return The user name.
##' 

start <- function(){
  name = readline("Please input your name: ")
#   if(paste0(name,".RData") %in% list.files())
#     load(paste0(name,".RData"))
  if(paste0(name,".csv") %in% list.files())
    vocabFile = read.csv(paste0(name,".csv"), stringsAsFactors = FALSE)
  else {
    needResponse = TRUE
    while(needResponse){
      create = readline("No profile found for that user.  Would you like to create a new one (y/n)?")
      if(create=="y"){
        vocabFile = matrix(0, nr=0, nc=4)
        vocabFile = as.data.frame(vocabFile)
        colnames(vocabFile) = c("English", "Italian", "Success", "Total")
        numLanguages <<- 2
        lockBinding("numLanguages", env = .GlobalEnv)
        needResponse=F
      }
      if(create=="n")
        needResponse=F
    }
  }
  if(exists("vocabFile")){
    vocabFile <<- vocabFile
    numLanguages <<- ncol(vocabFile) - 2
    lockBinding("numLanguages", env = .GlobalEnv)

  }
  return(name)
}