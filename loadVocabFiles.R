source("~/GitHub/Italian/getConjugations.R")
source("~/GitHub/Italian/parseOneTense.R")
library(dplyr)
library(rvest)

verbs = read.csv("~/GitHub/Italian/verbs.csv", stringsAsFactors = FALSE)
verbs = gsub("[[:space:]]*", "", verbs[, 1])

verbConjugations = NULL
for(verb in verbs){
    print(verb)
    newWords = try(getConjugations(verb))
    if(!is(newWords, "try-error")){
        newWords$infinitivo = verb
        verbConjugations = rbind(verbConjugations, newWords)
    }
    save(verbConjugations, file = "~/GitHub/Italian/verbConjugations.RData")
    print(dim(verbConjugations))
}
