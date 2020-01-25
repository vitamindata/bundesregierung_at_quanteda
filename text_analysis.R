#installing
#install.packages("quanteda")
#install.packages("readtext")
#install.packages("devtools")
#install.packages("tm")
#install.packages("stm")
#install.packages("ldatuning")
#install.packages("tidyverse")
#install.packages("topicmodels")
#install.packages("writexl")

#package quanteda.corpora from github using the install_github function from the devtools package
#devtools::install_github('quanteda/quanteda.corpora')

#load
library(tidyverse)
library(tm)
library(quanteda)
library(quanteda.corpora)
library(readtext)
library(stm)
library(ldatuning)
library(topicmodels)
library(writexl)

#create corpus
draft01 <- readtext("/Users/severin/Code/personal-projects/git@github.com:vitamindata/bundesregierung_at_quanteda.git/src/Regierungsprogramm_2020.txt")
corpus01 <- corpus(draft01)

#create documentfeaturematrix (bow); remove punctuation and numbers from the texts when constructing the dfm
dfm01 <- dfm(corpus01, 
             remove = c(stopwords("german"), 
                        "z.b", 
                        "dass", 
                        "mehr", 
                        "bzw", 
                        "seite", 
                        "sowie", 
                        "o", 
                        "etc",
                        "fahne österreich" ),
             remove_punct = TRUE, 
             remove_numbers = TRUE,
             remove_symbols = TRUE)

#remove infrequent words
dfm02 <- dfm_trim(dfm01, min_termfreq = 2, min_docfreq = 0.0025, docfreq_type = "prop")

export1 <- convert(dfm02, to ="data.frame")
export <- as.data.frame(t(as.matrix(dfm02)))
write.csv(export, "/Users/severin/Code/personal-projects/git@github.com:vitamindata/bundesregierung_at_quanteda.git/export.csv")
write_xlsx(export1,"/Users/severin/Code/personal-projects/git@github.com:vitamindata/bundesregierung_at_quanteda.git/export.xlsx")

topfeatures(dfm02,50) 


#topic modelling STM
topic_count <- 5
dfm02stm <- convert(dfm02, to = "stm")
model_stm <- stm(dfm02stm$documents, dfm02stm$vocab, K = topic_count, data = dfm02stm$meta, init.type = "Spectral") 
# this is the actual stm call
#load("/Users/severin/Desktop/quanteda regierung/src/Regierungsprogramm_2020.txt")
data.frame(t(labelTopics(model_stm, n = 3)$prob))


#topic modelling LDA
dfm02topicmodel <- convert(dfm02, to = "topicmodels")
lda_model <- LDA(dfm02topicmodel, topic_count)
as.data.frame(terms(lda_model, 10))

