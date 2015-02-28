##' Parse One Tense
##' 
##' @param elementList A list of seven elements, generated from a call such as
##' verbList = data %>%
##'     html_nodes("tr") %>%
##'     xml_text()
##' The first element should be the name of the tense, and the remaining seven
##' the conjugations.  Each conjugation may have the person (i.e. "io", "tu")
##' on it, and this function scrubs that off.
##' 
##' @return A data.frame with three columns: the name of the tense, the 
##' person/form, and the conjugation.
##' 

parseOneTenseEnglish = function(charVec){
    
    stopifnot(is(charVec, "character"))
    stopifnot(length(charVec) == 1)
    
    I = gsub("(I |you .*)", "", charVec)
    charVec = gsub(paste0("I ", I, "you "), "", charVec)
    you = gsub("he/she/it .*", "", charVec)
    charVec = gsub(paste0(you, "he/she/it "), "", charVec)
    he = gsub("we .*", "", charVec)
    charVec = gsub(paste0(he, "we "), "", charVec)
    we = gsub("you .*", "", charVec)
    charVec = gsub(paste0(we, "you "), "", charVec)
    you2 = gsub("they .*", "", charVec)
    charVec = gsub(paste0(you2, "they "), "", charVec)
    they = charVec
    
    return(data.frame(person = c("I", "you", "he, she, it", "we",
                                 ".", "they"),
                      elementList = c(I, you, he, we, you2, they)))
}
