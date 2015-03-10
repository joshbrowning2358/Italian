currDir = getwd()
setwd("~/GitHub/Italian")
library(data.table)
files = dir("R", full.names = TRUE)
files = files[files != "R/runVocab.R"]
for(file in files)
    source(file)
if(exists("vocabFile")){
    prompt = "a"
    while(!prompt %in% c("y","n")){
        prompt = readline("A vocab file already exists. Continue and delete current file (y/n)?")
        if(prompt=="y"){
            rm(vocabFile)
            interface()
        }
    }
} else {
    interface()
}
setwd(currDir)