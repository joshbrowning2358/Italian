interface <- function(){
  #Load vocabFile: either record of progress or new file to add words
  while(!"vocabFile" %in% ls(".GlobalEnv")){
    name = start()
  }
  finished=FALSE
  while(!finished){
    prompt = -1
    while(!prompt %in% 1:8){
      cat("Your vocabulary list currently has",nrow(vocabFile),"words.\n")
      cat("What would you like to do?\n")
      cat("(1) Enter new words\n")
      cat("(2) Begin vocabulary quiz\n")
      cat("(3) Begin conjugation quiz\n")
      cat("(4) Save and Exit\n")
      cat("(5) Create Backup\n")
      cat("(6) Don't Save and Exit\n")
      cat("(7) Check for duplicates\n")
      cat("(8) Show Statistics\n")
      prompt = readline()
    }
    if(prompt==1)
      addWord()
    if(prompt==2)
      beginQuiz(type = "vocabulary")
    if(prompt==3)
      beginQuiz(type = "conjugation")
    if(prompt==4){
      finished=TRUE
      #save(vocabFile, file=paste0(name,".RData"))
      write.csv(vocabFile, file=paste0(name,".csv"), row.names = FALSE)
    }
    if(prompt==5){
#       file =paste0(name,"_",round(as.numeric(Sys.time())),".RData")
#       save(vocabFile, file=file)
      file =paste0(name,"_",round(as.numeric(Sys.time())),".csv")
      write.csv(vocabFile, file=file, row.names = FALSE)
      cat("Backup file", file, "created.\n\n")
    }
    if(prompt==6)
      finished=TRUE
    if(prompt==7)
      checkDupes()
    if(prompt==8)
      computeStats()
  }
}