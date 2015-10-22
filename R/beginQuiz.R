beginQuiz <- function(type){
  ranking = rbinom(nrow(vocabFile), size=5, p=.5)
  ranking = (ranking+vocabFile$Success)/(5+vocabFile$Total)
  vocabFile = vocabFile[order(ranking),]
  continue = TRUE
  while(continue){
    prompt = -1
    while(!prompt %in% 1:1000){
      cat("What would you like to do?\n")
      cat("(1) Quit\n")
      cat("(2) Adjust random factor\n")
      cat("(3-1000) Continue quiz for 3-1000 words\n")
      prompt = readline()
    }
    if(prompt==1){
      continue = FALSE
    }
    if(prompt==2){
      adjustment = NA
      while(is.na(adjustment)){
        adjustment = readline(paste("Please enter the number of random",
             "rolls (0-Inf):"))
        adjustment = as.numeric(adjustment)
        adjustment = round(adjustment)
      }
      ranking = rbinom(nrow(vocabFile), size=adjustment, p=.5)
      ranking = (ranking+vocabFile$Success)/(adjustment+vocabFile$Total)
      vocabFile = vocabFile[order(ranking),]
    }
    if(prompt %in% 3:1000){
      prompt = as.numeric(prompt)
      testCnt = 1
      rowNumber = 1
      while(testCnt <= prompt){
#         if(grepl("\\*", vocabFile[rowNumber,"Italian"]) &
#                type == "conjugation"){
#           readline(paste0("(", testCnt, "/", prompt, ") ", "English verb is ",
#                           vocabFile[rowNumber,"English"]))
#           cat("Italian conjugation is:\n")
#           verb = strsplit(vocabFile[rowNumber,"Italian"], "\\*")[[1]]
#           lengths = sapply(verb, nchar)
#           mLength = ceiling((max(lengths)+1)/8)
#           tabs = mLength - floor(lengths/8)
#           for(j in 1:3){
#             cat("\t",verb[j], rep("\t",tabs[j]), verb[j+3], rep("\t",tabs[j]),"\n")
#           }
#           rightPrompt = 0
#           while(!rightPrompt %in% c("y", "n")){
#             rightPrompt = readline("Did you get the word right (y/n)?")
#           }
#           if(rightPrompt=="y")
#             vocabFile[rowNumber, "Success"] = vocabFile[rowNumber, "Success"]+1
#           vocabFile[rowNumber, "Total"] = vocabFile[rowNumber, "Total"]+1
#           testCnt = testCnt + 1
#         } else if(!grepl("\\*", vocabFile[rowNumber,"Italian"]) & type == "vocabulary"){
        if(numLanguages == 2){
          chooseItalian = sample(c(T,F),size=1)
          if(chooseItalian)
            readline(paste0("(", testCnt, "/", prompt, ") ", "Italian word is ",
                           vocabFile[rowNumber,"Italian"]))
          else
            readline(paste0("(", testCnt, "/", prompt, ")", "English word is ",
                           vocabFile[rowNumber,"English"]))
          if(chooseItalian)
            cat("English word is", vocabFile[rowNumber,"English"],"\n")
          else
            cat("Italian word is",vocabFile[rowNumber,"Italian"],"\n")
        } else if(numLanguages == 3){
          chooseColumn = sample(1:3, size = 1)
          readline(paste0("(", testCnt, "/", prompt, ") ",
                          colnames(vocabFile)[chooseColumn], " word is ",
                          vocabFile[rowNumber, chooseColumn]))
          if(chooseColumn != 1)
            cat("Palabra espanol es", vocabFile[rowNumber, 1], "\n")
          if(chooseColumn != 2)
            cat("Parola italiano e", vocabFile[rowNumber, 2], "\n")
          if(chooseColumn != 3)
            cat("English word is", vocabFile[rowNumber, 3], "\n")
        } else {
          stop("Unexpected value for numLanguages: ", numLanguages)
        }
        rightPrompt = 0
        while(!rightPrompt %in% c("y", "n")){
          rightPrompt = readline("Did you get the word right (y/n)?")
        }
        if(rightPrompt=="y")
          vocabFile[rowNumber,"Success"] = vocabFile[rowNumber,"Success"]+1
        vocabFile[rowNumber,"Total"] = vocabFile[rowNumber,"Total"]+1
        testCnt = testCnt + 1
        rowNumber = rowNumber + 1
        
        #Don't overrun vocabFile
        if(rowNumber + 1 > nrow(vocabFile)){
          break
        }
      }
    }
  }
  vocabFile <<- vocabFile
}