start <- function(){
  name = readline("Please input your name: ")
  if(paste0(name,".RData") %in% list.files())
    load(paste0(name,".RData"))
  else {
    needResponse = TRUE
    while(needResponse){
      create = readline("No profile found for that user.  Would you like to create a new one (y/n)?")
      if(create=="y"){
        vocabFile = matrix(0, nr=0, nc=4)
        vocabFile = as.data.frame(vocabFile)
        colnames(vocabFile) = c("English", "Italian", "Success", "Total")
        needResponse=F
      }
      if(create=="n")
        needResponse=F
    }
  }
  if(exists("vocabFile"))
    vocabFile <<- vocabFile
  return(name)
}