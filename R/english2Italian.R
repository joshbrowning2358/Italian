english2Italian = function(person, tense, verb){

    ## Switch '/' and ' ' for '.' so regex works
    person = gsub("/", ".", person, fixed = TRUE)
    person = gsub(" ", ".", person, fixed = TRUE)
    englishPerson = switch(person,
                           I = "io", you = "tu", he.she = "lui/lei/loro",
                           us = "noi", you.guys = "voi", they = "loro")
    
    ## Load the tense map from a file and apply it
    tenseMap = read.csv("tenseMap.csv", stringsAsFactors = FALSE)
    italianTense = tenseMap[tenseMap$englishTense == tense, "italianTense"]

    ## Load the verb map from a file and apply it
    verbMap = read.csv("verbs_both.csv", stringsAsFactors = FALSE)
    italianVerb = verbMap[verbMap$English == verb, "Italian"]
    return(list(italianPerson = italianPerson,
                italianTense  = italianTense,
                italianVerb   = italianVerb))
}