##' Update verb file
##' 
##' @param correct Logical, indicates if the answer was correct or not.
##' @param personWord Character.  Which person?  io, tu, ...
##' @param tenseWord Character.  Which tense?  presente, imperfetto, ...
##' @param verbWord Character.  Which verb?  pensare, capire, ...
##' 
##' @return Nothing is returned, but verbFile is updated.
##' 

updateVerbFile = function(correct, personWord, tenseWord, verbWord){
    ## Update statistics
    personWord  = ifelse(personWord == "lui, lei, Lei", "lui/lei/Lei",
                         ifelse(personWord == "loro, Loro", "loro",
                                personWord))
    verbFile[category == "person" & word == personWord,
             successes := correct + successes]
    verbFile[category == "tense"  & word == tenseWord,
             successes := correct + successes]
    verbFile[category == "verb"   & word == verbWord,
             successes := correct + successes]
    verbFile[category == "person" & word == personWord,
             attempts := attempts + 1]
    verbFile[category == "tense"  & word == tenseWord,
             attempts := attempts + 1]
    verbFile[category == "verb"   & word == verbWord,
             attempts := attempts + 1]    
    saveVerb(verbFile)
}