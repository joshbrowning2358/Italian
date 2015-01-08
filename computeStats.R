computeStats <- function(){
    library(ggplot2)
    cat("Most successes:\n")
    cat(paste(vocabFile[which.max(vocabFile[,3]),]), "\n")
    cat("Most failures:\n")
    cat(paste(vocabFile[which.max(vocabFile[,4]-vocabFile[,3]),]), "\n")
    cat("Highest Success Rate:\n")
    cat(paste(vocabFile[which.max(vocabFile[,3]/vocabFile[,4]*(vocabFile[,4]>1)),]), "\n")
    cat("Lowest Success Rate:\n")
    cat(paste(vocabFile[which.min(vocabFile[,3]/vocabFile[,4]*vocabFile[,4]>1),]), "\n")
    qplot(vocabFile[,3]/vocabFile[,4], fill=vocabFile[,4]
            ,group=vocabFile[,4], binwidth=.05) +
        labs(x="Success Rate")
}