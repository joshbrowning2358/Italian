##' Italian to English
##' 
##' This function converts an Italian verb to an English verb.
##' 
##' @return The english verb
##' 

italian2English = function(person, tense, verb){

    ## Switch / for . so regex works
    person = gsub("/", ".", person, fixed = TRUE)
    englishPerson = switch(person,
                           io = "I", tu = "you", lui.lei.Lei = "he/she",
                           noi = "us", voi = "you guys", loro = "they")
    
    ## Load the tense map from a file and apply it
    tenseMap = read.csv("tenseMap.csv", stringsAsFactors = FALSE)
    englishTense = tenseMap[tenseMap$italianTense == tense, "englishTense"]

    ## Load the verb map from a file and apply it
    verbMap = read.csv("verbs_both.csv", stringsAsFactors = FALSE)
    englishVerb = verbMap[verbMap$Italian == verb, "English"]
    return(list(englishPerson = englishPerson,
                englishTense = englishTense,
                englishVerb = englishVerb))
}