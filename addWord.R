addWord <- function(){
  continue = TRUE
  while(continue){
    prompt = -1
    if(nrow(vocabFile)>1)
      rownames(vocabFile) = 1:nrow(vocabFile)
    while(!prompt %in% 1:2){
      cat("Your vocabulary list currently has",nrow(vocabFile),"words.\n")
      cat("What would you like to do?\n")
      cat("(1) Add another word\n")
      cat("(2) Quit\n")
      prompt = readline()
    }
    if(prompt==1){
      Eng = readline("New word in english: ")
      Ital = readline("New word in italian: ")
      vocabFile[as.character(nrow(vocabFile)+1),] = c(Eng, Ital, 0, 0)
    }
    if(prompt==2){
      continue = FALSE
      vocabFile[,3] = as.numeric(vocabFile[,3])
      vocabFile[,4] = as.numeric(vocabFile[,4])
    }
  }
  vocabFile <<- vocabFile
}