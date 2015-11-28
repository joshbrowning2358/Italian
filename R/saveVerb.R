saveVerb = function(verbFile){
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
            readline("File not written.  Is it open?")
    }
}