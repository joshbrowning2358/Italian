library(data.table)

currDir = getwd()
if(Sys.info()[4] == "joshua-Ubuntu-Linux")
    setwd("~/Documents/Github/Italian")
if(Sys.info()[4] == "JOSH_LAPTOP")
    setwd("~/GitHub/Italian/")
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
            rm(numLanguages)
            interface()
        }
    }
} else {
    interface()
}
setwd(currDir)