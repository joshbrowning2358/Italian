computeStats <- function(){
    library(ggplot2)
    library(scales)
    cat("Most successes:\n")
    cat(paste(vocabFile[which.max(vocabFile$Success),]), "\n")
    cat("Most failures:\n")
    cat(paste(vocabFile[which.max(vocabFile$Total-vocabFile$Success),]), "\n")
    cat("Highest Success Rate:\n")
    cat(paste(vocabFile[which.max(vocabFile$Success/vocabFile$Total*(vocabFile$Total>1)),]), "\n")
    cat("Lowest Success Rate:\n")
    if(min(vocabFile$Success) == 0){
        temp = vocabFile[vocabFile$Success == 0, ]
        cat(paste(temp[which.max(temp$Total), ]), "\n")
    } else {
        cat(paste(vocabFile[which.min(vocabFile$Success/vocabFile$Total*(vocabFile$Total>1)),]), "\n")
    }
    print(qplot(vocabFile$Success/vocabFile$Total, fill=vocabFile$Total
            ,group=vocabFile$Total, binwidth=.05) +
        labs(x = "Success Rate", y = "Number of Words", fill = "Total") +
        scale_x_continuous(label=percent))
    cat("\n")
    if(min(vocabFile$Success) == 0){
        cat("Words which have never been correctly answered:\n")
        for(i in 1:nrow(temp))
            cat(paste(temp[i, ]), "\n")
    }
}