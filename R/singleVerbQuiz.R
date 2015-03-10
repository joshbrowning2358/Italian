##' Single Verb Quiz
##' 
##' This function uses the globally defined verbFile object to run the verb
##' conjugation quiz.  The verbFile should be a data.table with 5 columns:
##' category (either person, tense, or verb), word, active, attempts, and
##' successes.  category is needed as the one file contains the persons (io,
##' tu, ...) to be tested, the verbs to be conjugated, and the tenses to use.
##' 
##' @param verbFile The verb
##' 
##' @return The passed verbFile, updated with incremented counts.
##' 

singleVerbQuiz = function(verbFile){
    random = runif(4)
    personIndex = findInterval(random[1], persn$cumulativeProbability) + 1
    tenseIndex  = findInterval(random[2], tense$cumulativeProbability) + 1
    verbIndex   = findInterval(random[3], verb$cumulativeProbability)  + 1
    giveItalian = ifelse(random[4] > .5, TRUE, FALSE)
    personWord  = persn$word[personIndex]
    tenseWord   = tense$word[tenseIndex]
    verbWord    = verb$word[verbIndex]
    personWord  = ifelse(personWord == "lui/lei/Lei", "lui, lei, Lei",
                         ifelse(personWord == "loro", "loro, Loro",
                                personWord))
    
    ## Access dataset
    subset = finalDatabase[italianTense == tenseWord &
                           italianPerson == personWord &
                           italianVerb == verbWord, ]
    cat(tenseWord, personWord, verbWord, "\n")

    ## Write out question
    if(giveItalian){
        readline(paste("\t", subset[1, italianPerson],
                       subset[1, italianConjugation]))
        for(i in 1:nrow(subset))
            cat("\t", as.character(subset[i, englishPerson]),
                subset[i, englishConjugation], "\n")
    } else {
        englishWord = subset[sample(nrow(subset), size = 1), ]
        readline(paste("\t", englishWord[1, englishPerson],
                       englishWord[1, englishConjugation]))
        subset = finalDatabase[englishPerson == englishWord$englishPerson &
                               englishTense == englishWord$englishTense &
                               englishVerb == englishWord$englishVerb, ]
        for(i in 1:nrow(subset))
            cat("\t", as.character(subset[i, italianPerson]),
                subset[i, italianConjugation], "\n")
    }

    ## Check if correct
    rightPrompt = 0
    while(!rightPrompt %in% c("y", "n")){
        rightPrompt = readline("Did you get the word right (y/n)?")
    }
    
    ## Update statistics
    verbFile[category == "person" & word == personWord,
             successes := (rightPrompt == "y") + successes]
    verbFile[category == "tense"  & word == tenseWord,
             successes := (rightPrompt == "y") + successes]
    verbFile[category == "verb"   & word == verbWord,
             successes := (rightPrompt == "y") + successes]
    verbFile[category == "person" & word == personWord,
             attempts := attempts + 1]
    verbFile[category == "tense"  & word == tenseWord,
             attempts := attempts + 1]
    verbFile[category == "verb"   & word == verbWord,
             attempts := attempts + 1]
    
    return(verbFile)
}