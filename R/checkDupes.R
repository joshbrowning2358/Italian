checkDupes <- function(){
  tab = table( vocabFile[,1] )
  repeats = names(tab)[tab>1]
  for(wrd in repeats){
    print(vocabFile[vocabFile[,1]==wrd,])
    prompt = -1
    while(!prompt %in% 1:4){
      cat("Please choose an action:\n")
      cat("(1) Merge the records, first Italian word is kept\n")
      cat("(2) Merge the records, combine Italian words with commas\n")
      cat("(3) Correct the English words\n")
      cat("(4) Skip this duplicate\n")
      prompt = readline()
      filt = vocabFile[,1] == wrd
      if(prompt==1){
        cnts = as.data.frame(t(apply(vocabFile[filt,3:4], 2, sum)))
        vocabFile = rbind(vocabFile, cbind(vocabFile[filt,1:2][1,], cnts) )
        vocabFile = vocabFile[!filt,]
      }
      if(prompt==2){
        cnts = as.data.frame(t(apply(vocabFile[filt,3:4], 2, sum)))
        newEntry = data.frame(English=vocabFile[filt,1][1]
                             ,Italian=paste(vocabFile[filt,2], collapse=", "))
        vocabFile = rbind(vocabFile, cbind(newEntry, cnts) )
        vocabFile = vocabFile[!filt,]
      }
      if(prompt==3){
        for(i in (1:nrow(vocabFile))[filt] ){
          newWrd = readline(paste0("New English word for '", vocabFile[i,1]
              , "'/'", vocabFile[i,2], "': "))
          vocabFile[i,1] = newWrd
        }
      }
    }
  }

  tab = table( vocabFile[,2] )
  repeats = names(tab)[tab>1]
  for(wrd in repeats){
    print(vocabFile[vocabFile[,2]==wrd,])
    prompt = -1
    while(!prompt %in% 1:4){
      cat("Please choose an action:\n")
      cat("(1) Merge the records, first English word is kept\n")
      cat("(2) Merge the records, combine English words with commas\n")
      cat("(3) Correct the Italian words\n")
      cat("(4) Skip this duplicate\n")
      prompt = readline()
      filt = vocabFile[,2] == wrd
      if(prompt==1){
        cnts = as.data.frame(t(apply(vocabFile[filt,3:4], 2, sum)))
        vocabFile = rbind(vocabFile, cbind(vocabFile[filt,1:2][1,], cnts) )
        vocabFile = vocabFile[!filt,]
      }
      if(prompt==2){
        cnts = as.data.frame(t(apply(vocabFile[filt,3:4], 2, sum)))
        newEntry = data.frame(English=paste(vocabFile[filt,1], collapse=", ")
                             ,Italian=vocabFile[filt,2][1])
        vocabFile = rbind(vocabFile, cbind(newEntry, cnts) )
        vocabFile = vocabFile[!filt,]
      }
      if(prompt==3){
        for(i in (1:nrow(vocabFile))[filt] ){
          newWrd = readline(paste0("New Italian word for '", vocabFile[i,1]
              , "'/'", vocabFile[i,2], "': "))
          vocabFile[i,2] = newWrd
        }
      }
    }
  }

  #Save to global namespace
  vocabFile <<- vocabFile
}