##' Get Conjugations
##' 
##' @param infinitive A character string giving the verb for which all the
##' conjugations are desired.
##' 
##' @return A data.frame containing three columns: the tense, the person,
##' and the conjugation of the verb.
##' 

#library(dplyr)
#library(rvest)
#library(dplyr)
#source("parseOneTense.R")

getConjugations = function(infinitive){
    url = sprintf("http://italian.about.com/library/verb/blverb_%s.htm",
                  infinitive)
    
    verbList = html(url, encoding = "UTF-8") %>%
        html_nodes("tr") %>%
        xml_text()
    
    if(length(verbList) == 0)
        return(NULL)
    
    cursor = 2
    indicativo <- congiuntivo <- condizionale <- imperative <- infinito <-
        participio <- gerundio <- c()
    ## Indicativo
    while(TRUE){
        new = verbList[cursor:(cursor+6)]
        ## If there's a newline character, skip to next tense
        if(any(grepl("\n", new))){
            cursor = cursor + 1
            break
        } else {
        ## If there's no newline character, append and continue adding
            indicativo = c(indicativo, new)
            cursor = cursor + 7
        }
    }
    
    ## Congiuntivo
    while(TRUE){
        new = verbList[cursor:(cursor+6)]
        ## If there's a newline character, skip to next tense
        if(any(grepl("\n", new))){
            cursor = cursor + 1
            break
        } else {
        ## If there's no newline character, append and continue adding
            congiuntivo = c(congiuntivo, new)
            cursor = cursor + 7
        }
    }

    ## Condizionale
    while(TRUE){
        new = verbList[cursor:(cursor+6)]
        ## If there's a newline character, skip to next tense
        if(any(grepl("\n", new))){
            cursor = cursor + 1
            break
        } else {
        ## If there's no newline character, append and continue adding
            condizionale = c(condizionale, new)
            cursor = cursor + 7
            ## Must also force a break here, as conditional and imperative
            ## aren't separated in the html like they seem to be on the page
            if(length(condizionale) == 14)
                break
        }
    }

    ## Imperative
    while(TRUE){
        new = verbList[cursor:(cursor+6)]
        ## If there's a newline character, skip to next tense
        if(any(grepl("\n", new))){
            cursor = cursor + 1
            break
        } else {
        ## If there's no newline character, append and continue adding
            imperative = c(imperative, new)
            cursor = cursor + 7
        }
    }
    
    ## Infinito
    ## Adjust cursor for infinito
    while(TRUE){
        if(verbList[cursor] != "Presente")
            cursor = cursor + 1
        else
            break
    }
    infinito = verbList[cursor + c(1, 3)]
    cursor = cursor + 3

    ## Participio
    while(TRUE){
        if(verbList[cursor] != "Presente")
            cursor = cursor + 1
        else
            break
    }
    participio = verbList[cursor + c(1, 3)]
    cursor = cursor + 3
    
    ## Gerundio
    while(TRUE){
        if(verbList[cursor] != "Presente")
            cursor = cursor + 1
        else
            break
    }
    gerundio = verbList[cursor + c(1, 3)]

    ## Extract appropriate structure, convert to data.frame
    indicative = do.call("rbind", 
        tapply(X = indicativo,
               INDEX = rep(1:(length(indicativo)/7), each = 7),
               FUN = parseOneTense))
    congiuntivo = do.call("rbind", 
        tapply(X = congiuntivo,
               INDEX = rep(1:(length(congiuntivo)/7), each = 7),
               FUN = parseOneTense))
    congiuntivo$tense = paste(congiuntivo$tense, "Congiuntivo")
    condizionale = do.call("rbind",
        tapply(X = condizionale,
               INDEX = rep(1:(length(condizionale)/7), each = 7),
               FUN = parseOneTense))
    condizionale$tense = paste(condizionale$tense, "Condizionale")
    
    ## infinito, participio, e gerundio in una lista
    infinito = data.frame(tense = "Infinito", person = NA,
                          elementList = infinito)
    participio = data.frame(tense = "Participio", person = NA,
                            elementList = participio)
    gerundio = data.frame(tense = "Gerundio", person = NA,
                          elementList = gerundio)
    
    ## Raggiungere tutto
    out = rbind(indicative, congiuntivo, condizionale, infinito, participio, gerundio)
    
    colnames(out) = c("tense", "person", "conjugation")
    out
}