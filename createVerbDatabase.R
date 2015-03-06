library(rvest)
library(stringr)

setwd("~/GitHub/Italian/")

source("getConjugations.R")
source("parseOneTense.R")
source("getConjugationsEnglish.R")
source("parseOneTenseEnglish.R")

verbs = read.csv("verbs_both.csv", stringsAsFactors = FALSE)
verbMap = read.csv("tenseMap.csv")

englishDatabase = NULL
italianDatabase = NULL
for(i in 1:nrow(verbs)){
    english = gsub("to ", "", verbs[i, 1])
    if(grepl(" ", english))
        ending = gsub("[a-zA-Z]* ", "", english)
    else
        ending = ""
    english = gsub(" .*", "", english)
    engConj = getConjugationsEnglish(english)
    engConj$verb = english
    engConj$conjugation = paste0(engConj$conjugation, ending)
    englishDatabase = rbind(englishDatabase, engConj)
    
    italian = verbs[i, 2]
    if(grepl(" ", italian))
        ending = gsub("[a-zA-Z]* ", "", italian)
    else
        ending = ""
    italian = gsub(" .*", "", italian)
    italConj = try(getConjugations(italian))
    if(!is.null(italConj) & !is(italConj, "try-error")){
        italConj$verb = italian
        italConj$conjugation = paste0(italConj$conjugation, ending)
        italianDatabase = rbind(italianDatabase, italConj)
    }

    save(italianDatabase, englishDatabase, verbs, verbMap,
         file = "verbData.RData")
    cat("Added word", verbs[i, 1], ",", verbs[i, 2], "\n")
}

## Manually add uncertainty tenses
for(verb in unique(englishDatabase$verb)){
    englishDatabase = rbind(englishDatabase, data.frame(
        tense = "present conditional",
        person = c("I", "you", "he, she, it", "we", "you (plural)", "they"),
        conjugation = paste(c("could", "could", "could",
                              "could", "could", "could"), verb),
        verb = verb))
    participle = englishDatabase[englishDatabase$verb == verb &
                                 englishDatabase$tense == "present perfect" &
                                 englishDatabase$person == "I", "conjugation"]
    participle = gsub("have ", "", participle)
    englishDatabase = rbind(englishDatabase, data.frame(
        tense = "past conditional",
        person = c("I", "you", "he, she, it", "we", "you (plural)", "they"),
        conjugation = paste(c("could", "could", "could",
                              "could", "could", "could"), participle),
        verb = verb))
}

## Manually add  conditional tenses
for(verb in unique(englishDatabase$verb)){
    englishDatabase = rbind(englishDatabase, data.frame(
        tense = "present conditional",
        person = c("I", "you", "he, she, it", "we", "you (plural)", "they"),
        conjugation = paste(c("could", "could", "could",
                              "could", "could", "could"), verb),
        verb = verb))
    participle = englishDatabase[englishDatabase$verb == verb &
                                 englishDatabase$tense == "present perfect" &
                                 englishDatabase$person == "I", "conjugation"]
    participle = gsub("have ", "", participle)
    englishDatabase = rbind(englishDatabase, data.frame(
        tense = "past conditional",
        person = c("I", "you", "he, she, it", "we", "you (plural)", "they"),
        conjugation = paste(c("could", "could", "could",
                              "could", "could", "could"), participle),
        verb = verb))
}

## Manually add congiuntivo tenses
subset = englishDatabase[englishDatabase$tense %in%
                             c("indicative", "preterite",
                               "present perfect", "past perfect"), ]
subset$tense = paste(subset$tense, "+ uncertainty") 
subset$conjugation = paste(subset$conjugation, "(maybe)")
englishDatabase = rbind(englishDatabase, subset)

save(italianDatabase, englishDatabase, verbs, verbMap,
     file = "verbData.RData")
load("verbData.RData")
verbMap$englishTense[which(!verbMap$englishTense %in% englishDatabase$tense)]
verbMap$italianTense[which(!verbMap$italianTense %in% italianDatabase$tense)]
