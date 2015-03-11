##' Single Verb Quiz
##' 
##' This function uses the globally defined verbFile object to run the verb
##' conjugation quiz.  The verbFile should be a data.table with 5 columns:
##' category (either person, tense, or verb), word, active, attempts, and
##' successes.  category is needed as the one file contains the persons (io,
##' tu, ...) to be tested, the verbs to be conjugated, and the tenses to use.
##' 
##' @param name The username provided previously.  This is used to specify what
##' the verbFile should be saved as (i.e. the file name).
##' 

wrapVerbQuiz = function(name){
    
    ## Access dataset
    verbFile[, attempts := as.integer(attempts)]
    verbFile[, successes := as.integer(successes)]
    verbFile[, active := as.integer(active)]
    load("~/GitHub/Italian/finalVerbDatabase.RData")
    
    ## Subset verbFile
    filter = verbFile[, active == 1 & (word %in% finalDatabase$italianVerb |
                                       category != "verb")]
    verbFile[filter, successProbability := (successes + 0.5) / (attempts + 1)]
    verbFile[filter, selectProbability :=
                 (1 - successProbability) / sum(1 - successProbability),
             by = category]
    verbFile[filter, cumulativeProbability := cumsum(selectProbability),
             by = category]
    ## persn instead of person to avoid global definition:
    persn <<- verbFile[category == "person" & filter,
                        .(word, cumulativeProbability)]
    tense <<- verbFile[category == "tense" & filter,
                       .(word, cumulativeProbability)]
    verb <<- verbFile[category == "verb" & filter,
                      .(word, cumulativeProbability)]
    
    continue = TRUE
    prompt = -1
    while(!prompt %in% 1:1000){
        cat("What would you like to do?\n")
        cat("(1) Quit\n")
        cat("(2-1000) Continue quiz for 2-1000 words\n")
        prompt = readline()
    }
    if(prompt==1){
        continue = FALSE
    }
    if(prompt %in% 2:1000){
        for(i in 1:prompt)
            verbFile = singleVerbQuiz(verbFile = verbFile)
    }
    
    ## Update statistics
    written = FALSE
    while(!written){
        test = try(write.csv(verbFile[, .(category, word, active,
                                          attempts, successes)],
                             file = paste0(name,"Verb.csv"),
                             row.names = FALSE))
        if(!is(test, "try-error"))
            written = TRUE
        else
            cat("File not written.  Is it open?")
    }
}