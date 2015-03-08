startVerb <- function(){
    name = readline("Please input your name: ")
#     if(paste0(name,".RData") %in% list.files())
#         load(paste0(name,".RData"))
    if(paste0(name,"Verb.csv") %in% list.files()){
        verbFile = read.csv(paste0(name,"Verb.csv"), stringsAsFactors = FALSE)
        verbFile = data.table(verbFile)
    }
    if(exists("verbFile"))
        verbFile <<- verbFile
    load("verbData.RData")
    italianDatabase = data.table(italianDatabase)
    englishDatabase = data.table(englishDatabase)
    return(name)
}