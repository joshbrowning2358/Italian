computeStats <- function(){
    library(ggplot2)
    library(scales)
    cat("Most successes:\n")
    cat(paste(vocabFile[which.max(vocabFile[,3]),]), "\n")
    cat("Most failures:\n")
    cat(paste(vocabFile[which.max(vocabFile[,4]-vocabFile[,3]),]), "\n")
    cat("Highest Success Rate:\n")
    cat(paste(vocabFile[which.max(vocabFile[,3]/vocabFile[,4]*(vocabFile[,4]>1)),]), "\n")
    cat("Lowest Success Rate:\n")
    if(min(vocabFile[, 3]) == 0){
        temp = vocabFile[vocabFile[, 3] == 0, ]
        cat(paste(temp[which.max(temp[, 4]), ]), "\n")
    } else {
        cat(paste(vocabFile[which.min(vocabFile[,3]/vocabFile[,4]*(vocabFile[,4]>1)),]), "\n")
    }
    print(qplot(vocabFile[,3]/vocabFile[,4], fill=vocabFile[,4]
            ,group=vocabFile[,4], binwidth=.05) +
        labs(x = "Success Rate", y = "Number of Words", fill = "Total") +
        scale_x_continuous(label=percent))
    cat("\n")
    if(min(vocabFile[, 3]) == 0){
        cat("Words which have never been correctly answered:\n")
        for(i in 1:nrow(temp))
            cat(paste(temp[i, ]), "\n")
    }
}