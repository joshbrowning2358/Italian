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

parseOneTense = function(elementList){
    stopifnot(length(elementList) == 7)
    
    tenseName = elementList[[1]]
    elementList = elementList[2:7]
    
    elementList[[1]] = gsub("^io", "", elementList[[1]])
    elementList[[2]] = gsub("^tu", "", elementList[[2]])
    elementList[[3]] = gsub("^lui, lei, Lei", "", elementList[[3]])
    elementList[[4]] = gsub("^noi", "", elementList[[4]])
    elementList[[5]] = gsub("^voi", "", elementList[[5]])
    elementList[[6]] = gsub("^loro, Loro", "", elementList[[6]])
    return(data.frame(tense = tenseName,
                      person = c("io", "tu", "lui, lei, Lei", "noi",
                                 "voi", "loro, Loro"),
                      elementList))
}
