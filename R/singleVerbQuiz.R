##' Single Verb Quiz
##' 
##' This function uses the globally defined verbFile object to run the verb
##' conjugation quiz.  The verbFile should be a data.table with 5 columns:
##' category (either person, tense, or verb), word, active, attempts, and
##' successes.  category is needed as the one file contains the persons (io,
##' tu, ...) to be tested, the verbs to be conjugated, and the tenses to use.
##' 
##' @param mode Specify how the function is being run.  If "interactive", then
##'   it's from the command line.  If "shiny", then it's being run from within a
##'   shiny app.
##' 
##' @return The passed verbFile, updated with incremented counts.
##' 

singleVerbQuiz = function(mode = "interactive"){
    stopifnot(mode %in% c("interactive", "shiny"))

    subset = data.frame()
    while(nrow(subset) == 0){
        #random = runif(4)
        random = runif(3)
        personIndex = findInterval(random[1], persn$cumulativeProbability) + 1
        tenseIndex  = findInterval(random[2], tense$cumulativeProbability) + 1
        verbIndex   = findInterval(random[3], verb$cumulativeProbability)  + 1
        #giveItalian = ifelse(random[4] > .5, TRUE, FALSE)
        giveItalian = FALSE
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
        subset = unique(subset)
    }

    ## Write out question
    if(mode == "interactive"){
        if(giveItalian){
            readline(paste("\t", subset[1, italianPerson],
                           subset[1, italianConjugation]))
            for(i in 1:nrow(subset))
                cat("\t", as.character(subset[i, englishPerson]),
                    subset[i, englishConjugation], "\n")
        } else {
            englishWord = subset[sample(nrow(subset), size = 1), ]
            ans = readline(paste0("\t", englishWord[1, englishPerson], " ",
                                  englishWord[1, englishConjugation],
                                  " (", englishWord$italianTense, "): "))
            if(tolower(ans) == tolower(englishWord$italianConjugation))
                correct = TRUE
            else
                correct = FALSE
            if(correct)
                cat("Good job!  You're right.\n")
            else
                cat("That's not correct.  The answer is ",
                    englishWord$italianConjugation)
        }
        updateVerbFile(correct = correct, personWord = personWord,
                       tenseWord = tenseWord, verbWord = verbWord)
    } else if(mode == "shiny"){
        # Nothing required by shiny
    } else {
        stop("mode not yet implemented!")
    }

#     ## Check if correct
#     rightPrompt = 0
#     while(!rightPrompt %in% c("y", "n")){
#         rightPrompt = readline("Did you get the word right (y/n)?")
#     }
    
    if(mode == "interactive"){
        return(verbFile)
    } else if(mode == "shiny"){
        return(subset[sample(nrow(subset), size = 1), ])
    } else {
        stop("Mode not implemented yet!")
    }
}