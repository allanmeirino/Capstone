options(java.parameters = "-Xss2048k")
library(rJava)
library(dplyr)
library(tm)
library(stringi)


## Reading .csv files
sixgrams <- read.table("D:/Coursera/sixgrams.txt", sep = ",")
sixgrams <- as.factor(sixgrams$x)
fivegrams <- read.table("D:/Coursera/fivegrams.txt", sep = ",")
fivegrams <- as.factor(fivegrams$x)
fourgrams <- read.table("D:/Coursera/fourgrams.txt", sep = ",")
fourgrams <- as.factor(fourgrams$x)
trigrams <- read.table("D:/Coursera/trigrams.txt", sep = ",")
trigrams <- as.factor(trigrams$x)
bigrams <- read.table("D:/Coursera/bigrams.txt", sep = ",")
bigrams <- as.factor(bigrams$x)

## Function to predict and suggest next word
suggestNextWord <- function(input_string) {
        
        ## Treating the input string
        input_string <- stripWhitespace(input_string)
        input_string <- removePunctuation(input_string)
        input_string <- removeNumbers(input_string)
        input_string <- stri_trans_tolower(input_string)
        splited_string <- strsplit(input_string, " ")
        
        ## Picking the last 5 words of the inputed string
        words_above <- length(splited_string[[1]]) - 5
        if (words_above > 0) {
                splited_string[[1]] <- splited_string[[1]][(words_above+1):(words_above+5)]
        }
        words_count <- length(splited_string[[1]])
        ref_string <- stri_join_list(splited_string, sep = " ")
        suggest <- NULL
        i <- 1
        
        ## Test for case inputed string is 5 or more words long
        if (words_count == 5) {
                sweep <- startsWith(as.character(sixgrams), ref_string)
                while (i <= length(sweep)) {
                        if (sweep[i]) {
                                suggest <- strsplit(as.character(sixgrams[i]), " ")[[1]][6]
                                return(suggest)
                        } else {
                                i <- i + 1
                        }
                }
                if (is.null(suggest)) {
                        splited_string[[1]] <- splited_string[[1]][2:5]
                        ref_string <- stri_join_list(splited_string, sep = " ")
                        words_count <- 4
                        i <- 1
                }
        }
        
        ## Test for case inputed string is/become 4 words long
        if (words_count == 4) {
                sweep <- startsWith(as.character(fivegrams), ref_string)
                while (i <= length(sweep)) {
                        if (sweep[i]) {
                                suggest <- strsplit(as.character(fivegrams[i]), " ")[[1]][5]
                                return(suggest)
                        } else {
                                i <- i + 1
                        }
                }
                if (is.null(suggest)) {
                        splited_string[[1]] <- splited_string[[1]][2:4]
                        ref_string <- stri_join_list(splited_string, sep = " ")
                        words_count <- 3
                        i <- 1
                }
        }
        
        ## Test for case inputed string is/become 3 words long
        if (words_count == 3) {
                sweep <- startsWith(as.character(fourgrams), ref_string)
                while (i <= length(sweep)) {
                        if (sweep[i]) {
                                suggest <- strsplit(as.character(fourgrams[i]), " ")[[1]][4]
                                return(suggest)
                        } else {
                                i <- i + 1
                        }
                }
                if (is.null(suggest)) {
                        splited_string[[1]] <- splited_string[[1]][2:3]
                        ref_string <- stri_join_list(splited_string, sep = " ")
                        words_count <- 2
                        i <- 1
                }
        }
        
        ## Test for case inputed string is/become 2 words long
        if (words_count == 2) {
                sweep <- startsWith(as.character(trigrams), ref_string)
                while (i <= length(sweep)) {
                        if (sweep[i]) {
                                suggest <- strsplit(as.character(trigrams[i]), " ")[[1]][3]
                                return(suggest)
                        } else {
                                i <- i + 1
                        }
                }
                if (is.null(suggest)) {
                        ref_string <- splited_string[[1]][2]
                        words_count <- 1
                        i <- 1
                }
        }
        
        ## Test for case inputed string is/become a single word
        if (words_count == 1) {
                sweep <- startsWith(as.character(bigrams), ref_string)
                while (i <= length(sweep)) {
                        if (sweep[i]) {
                                suggest <- strsplit(as.character(bigrams[i]), " ")[[1]][2]
                                return(suggest)
                        } else {
                                i <- i + 1
                        }
                }
                if (is.null(suggest)) {
                        suggest <- "."
                }
        }
        suggest
}