##' Verb Quiz
##' 
##' This function uses the globally defined verbFile object to run the verb
##' conjugation quiz.  The verbFile should be a data.table with 5 columns:
##' category (either person, tense, or verb), word, active, attempts, and
##' successes.  category is needed as the one file contains the persons (io,
##' tu, ...) to be tested, the verbs to be conjugated, and the tenses to use.
##' 

verbQuiz = function(){
    verbFile[active == 1, successProbability := (successes + 0.5) / (attempts + 1)]
    verbFile[active == 1, selectProbability :=
                 (1 - successProbability) / sum(1 - successProbability),
             by = category]
    verbFile[active == 1, cumulativeProbability := cumsum(selectProbability),
             by = category]
    person = verbFile[category == "person" & active == 1,
                      .(word, cumulativeProbability)]
    tense = verbFile[category == "tense" & active == 1,
                     .(word, cumulativeProbability)]
    verb = verbFile[category == "verb" & active == 1,
                    .(word, cumulativeProbability)]
    random = runif(4)
    personIndex = findInterval(random[1], person$cumulativeProbability) + 1
    tenseIndex  = findInterval(random[2], tense$cumulativeProbability)  + 1
    verbIndex   = findInterval(random[3], verb$cumulativeProbability)   + 1
    giveItalian = ifelse(random[4] > .5, TRUE, FALSE)
    personWord  = person$word[personIndex]
    tenseWord   = tense$word[tenseIndex]
    verbWord    = verb$word[verbIndex]
    italianWord = italianDatabase[tense == tenseWord & person == personWord
                                  & verb == verbWord, conjugation]
    italian2English(person = personWord, tense = tenseWord, verb = verbWord)
    englishWord = englishDatabase[tense == tenseWord & person == personWord
                                  & verb == verbWord, verb]
    if(giveItalian){
    } else {
        
    }
    verbFile[category == "person" & word == personWord, attempts := attempts + 1]
    verbFile[category == "tense"  & word == tenseWord, attempts := attempts + 1]
    verbFile[category == "verb"   & word == verbWord, attempts := attempts + 1]
    verbFile[category == "person" & word == personWord,
             successes := successes + correctAnswer]
    verbFile[category == "tense"  & word == tenseWord,
             successes := successes + correctAnswer]
    verbFile[category == "verb"   & word == verbWord,
             successes := successes + correctAnswer]
}