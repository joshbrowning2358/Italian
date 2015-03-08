currDir = getwd()
setwd("~/GitHub/Italian")
library(data.table)
source("interface.R")
source("start.R")
source("addWord.R")
source("checkDupes.R")
source("computeStats.R")
source("beginQuiz.R")
if(exists("vocabFile")){
    prompt = "a"
    while(!prompt %in% c("y","n"))
        prompt = readline("A vocab file already exists. Continue and delete current file (y/n)?")
        if(prompt=="y"){
            rm(vocabFile)
            interface()
        }
    } else {
    interface()
}
setwd(currDir)