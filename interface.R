interface <- function(){
  #Load vocabFile: either record of progress or new file to add words
  while(!"vocabFile" %in% ls(".GlobalEnv")){
    name = start()
  }
  finished=FALSE
  while(!finished){
    prompt = -1
    while(!prompt %in% 1:5){
      cat("Your vocabulary list currently has",nrow(vocabFile),"words.\n")
      cat("What would you like to do?\n")
      cat("(1) Enter new words\n")
      cat("(2) Begin quiz\n")
      cat("(3) Save and Exit\n")
      cat("(4) Create Backup\n")
      cat("(5) Don't Save and Exit\n")
      prompt = readline()
    }
    if(prompt==1)
      addWord()
    if(prompt==2)
      beginQuiz()
    if(prompt==3){
      finished=TRUE
      save(vocabFile, file=paste0(name,".RData"))
    }
    if(prompt==4){
      file =paste0(name,"_",round(as.numeric(Sys.time())),".RData")
      save(vocabFile, file=file)
      cat("Backup file", file, "created.\n\n")
    }
    if(prompt==5)
      finished=TRUE
  }
}