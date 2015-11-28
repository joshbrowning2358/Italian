library(shiny)
library(data.table)
library(ggplot2)
files = dir("~/GitHub/Italian/R", full.names = TRUE)
sapply(files, source)

shinyServer(function(input, output) {

    ########################## DATA GENERATION ##########################    
    if(Sys.info()[4] == "joshua-Ubuntu-Linux")
        dataDir = "~/Documents/Github/Italian/"
    if(Sys.info()[4] == "JOSH_LAPTOP")
        dataDir = "~/GitHub/Italian/"
    reactive({
        name = input$userName
        wrapVerbQuiz(name = name, mode = "shiny", dataDir = dataDir)
    })
    newQuiz = reactive({
        dummy = input$submitButton # Force reactivity on this button
        singleVerbQuiz(mode = "shiny")
    })
    
    # One button for "Next quiz".  It should:
    # - check if the current answer is right, if it exists
    # - generate the next question
    
#     eventReactive(input$submitButton, eventExpr = {
#         updateVerbFile(correct = correct, personWord = quiz$italianPerson,
#                        tenseWord = quiz$italianTense, verbWord = quiz$italianVerb)
#     })

    ########################## PLOT OUTPUTS ##########################

    ########################## TEXT OUTPUTS ##########################
    output$question = renderText({
        quiz <<- newQuiz()
        paste0(quiz$englishPerson, " ", quiz$englishConjugation,
               "\t(", quiz$italianTense, ")")
    })
    
    successText = renderText({
        quiz = newQuiz()
        correct <<- input$answer == quiz$italianConjugation
        if(correct){
            "Success!"
        } else {
            "That's not correct..."
        }
    })
    
    output$response = renderText({
        successText()
    })
    
    ########################## TABLE OUTPUTS ##########################
        
    ########################## DYNAMIC MENUS ##########################

})