##' Get Conjugations
##' 
##' @param infinitive A character string giving the verb for which all the
##' conjugations are desired.
##' 
##' @return A data.frame containing three columns: the tense, the person,
##' and the conjugation of the verb.
##' 

#library(rvest)
#library(stringr)
#source("parseOneTenseEnglish.R")

getConjugationsEnglish = function(infinitive){
    url = sprintf("http://conjugator.reverso.net/conjugation-english-verb-%s.html",
                  infinitive)
    
    verbList = html(url) %>%
        html_nodes("tr") %>%
        xml_text()

    verbList = sapply(verbList, gsub, pattern = "\r\n[[:space:]]*", replacement = " ")
    names(verbList) = NULL
    
    indicative = gsub("( Indicative Present |Preterite.*)", "", verbList[7])
    indicative = parseOneTenseEnglish(indicative)

    preterite = gsub("(.*Preterite |Infinitive.*)", "", verbList[7])
    preterite = parseOneTenseEnglish(preterite)

    infinitive = gsub("(.*Infinitive | Imperative.*)", "", verbList[7])

    imperative = gsub(".*Imperative ", "", verbList[7])
    imperative = gsub("let's.*", "", imperative)
    imperative = data.frame(person = c("I", "you", "he, she, it", "we", ".", "they"),
                            elementList = c(".", imperative, imperative,
                                            paste("Let's", imperative),
                                            imperative, imperative))
    
#     participle = gsub("(.*Present| Past.*)", "", verbList[18])
#     participle = gsub("(.*Past| )", "", verbList[18])
    
    PresentContinuous = gsub("( Indicative Present continuous| Present perfect.*)",
                             "", verbList[28])
    PresentContinuous = parseOneTenseEnglish(PresentContinuous)

    PresentPerfect = gsub("(.* Present perfectI| Future.*)", "", verbList[28])
    PresentPerfect = paste0("I", PresentPerfect)
    PresentPerfect = parseOneTenseEnglish(PresentPerfect)

    Future = gsub("(.* FutureI| Future perfect.*)", "", verbList[28])
    Future = paste0("I", Future)
    Future = parseOneTenseEnglish(Future)

    FuturePerfect = gsub("(.* Future perfectI| Past continuous.*)", "", verbList[28])
    FuturePerfect = paste0("I", FuturePerfect)
    FuturePerfect = parseOneTenseEnglish(FuturePerfect)

    PastContinuous = gsub("(.* Past continuousI| Past perfect.*)", "", verbList[28])
    PastContinuous = paste0("I", PastContinuous)
    PastContinuous = parseOneTenseEnglish(PastContinuous)

    PastPerfect = gsub("(.* Past perfectI| Future continuous.*)", "", verbList[28])
    PastPerfect = paste0("I", PastPerfect)
    PastPerfect = parseOneTenseEnglish(PastPerfect)

    FutureContinuous = gsub("(.* Future continuousI| Present perfect continuous.*)", "", verbList[28])
    FutureContinuous = paste0("I", FutureContinuous)
    FutureContinuous = parseOneTenseEnglish(FutureContinuous)

    PresentPerfectContinuous = gsub("(.* Present perfect continuousI| Past perfect continuous.*)",
                                    "", verbList[28])
    PresentPerfectContinuous = paste0("I", PresentPerfectContinuous)
    PresentPerfectContinuous = parseOneTenseEnglish(PresentPerfectContinuous)

    PastPerfectContinuous = gsub("(.* Past perfect continuousI| Future perfect continuous.*)",
                                 "", verbList[28])
    PastPerfectContinuous = paste0("I", PastPerfectContinuous)
    PastPerfectContinuous = parseOneTenseEnglish(PastPerfectContinuous)

    FuturePerfectContinuous = gsub(".* Future perfect continuousI", "", verbList[28])
    FuturePerfectContinuous = paste0("I", FuturePerfectContinuous)
    FuturePerfectContinuous = parseOneTenseEnglish(FuturePerfectContinuous)

    return(rbind(data.frame(tense = "indicative", indicative),
                 data.frame(tense = "preterite", preterite),
                 data.frame(tense = "imperative", imperative),
                 data.frame(tense = "present continuous", PresentContinuous),
                 data.frame(tense = "present perfect", PresentPerfect),
                 data.frame(tense = "future", Future),
                 data.frame(tense = "future perfect", FuturePerfect),
                 data.frame(tense = "past continuous", PastContinuous),
                 data.frame(tense = "past perfect", PastPerfect),
                 data.frame(tense = "future continuous", FutureContinuous),
                 data.frame(tense = "present perfect continuous", PresentPerfectContinuous),
                 data.frame(tense = "past perfect continuous", PastPerfectContinuous),
                 data.frame(tense = "future perfect continuous", FuturePerfectContinuous),
                 data.frame(tense = "infinitive", infinitive)))
}