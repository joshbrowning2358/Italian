beginQuiz <- function(){
  ranking = rbinom(nrow(vocabFile), size=5, p=.5)
  ranking = (ranking+vocabFile$Success)/(5+vocabFile$Total)
  vocabFile = vocabFile[order(ranking),]
  continue = TRUE
  while(continue){
    prompt = -1
    while(!prompt %in% 1:1000){
      cat("What would you like to do?\n")
      cat("(1) Quit\n")
      cat("(2-1000) Continue quiz for 2-1000 words\n")
      prompt = readline()
    }
    if(prompt==1){
      continue = FALSE
    }
    if(prompt %in% 2:1000){
      prompt = as.numeric(prompt)
      for(i in 1:prompt){
        chooseItalian = sample(c(T,F),size=1)
        if(chooseItalian)
          readline(paste("Italian word is", vocabFile[i,"Italian"]))
        else
          readline(paste("English word is", vocabFile[i,"English"]))
        if(chooseItalian)
          cat("English word is", vocabFile[i,"English"],"\n")
        else
          cat("Italian word is",vocabFile[i,"Italian"],"\n")
        
        rightPrompt = 0
        while(!rightPrompt %in% c("y", "n")){
          rightPrompt = readline("Did you get the word right (y/n)?")
        }
        if(rightPrompt=="y")
          vocabFile[i,"Success"] = vocabFile[i,"Success"]+1
        vocabFile[i,"Total"] = vocabFile[i,"Total"]+1
        
        #Don't overrun vocabFile
        if(i+1>nrow(vocabFile)){
          break
        }
      }
    }
  }
  vocabFile <<- vocabFile
}