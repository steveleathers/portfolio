
library(stringr)
library(tm)
library(ggmap)
library(dplyr)
library(plyr)
library(wordcloud)

setwd("~/Downloads")
dwp = read.csv("DWP pledge template - Full List.csv")

dwp$we.need.more.lower=tolower(dwp$we.need.more)
dwp$so.i.will.lower=tolower(dwp$so.i.will)

length(which(dwp$we.need.more != ""))
length(which(dwp$so.i.will != ""))

table(grepl("affordable", dwp$we.need.more.lower))
table(grepl("taxes", dwp$so.i.will.lower))


col=brewer.pal(6, "Dark2")

soiwill = Corpus(VectorSource(dwp$so.i.will))
soiwill=tm_map(soiwill, tolower)
soiwill = tm_map(soiwill, function(x) removeWords(x, stopwords()))
soiwill=tm_map(soiwill, PlainTextDocument)
wordcloud(soiwill, min.freq = 5, scale=c(5, 2), rot.per=.25, random.color=T, max.word=25, random.order = F, colors=col)
weneedmore = Corpus(VectorSource(dwp$we.need.more))
weneedmore=tm_map(weneedmore, tolower)
weneedmore = tm_map(weneedmore, function(x) removeWords(x, stopwords()))
weneedmore=tm_map(weneedmore, PlainTextDocument)
wordcloud(weneedmore, min.freq = 4, scale=c(5, 2), rot.per=.25, random.color=T, max.word=30, random.order = F, colors=col)


posi=readLines("opinion-lexicon-English/positive-words.txt")
nega=readLines("opinion-lexicon-English/negative-words.txt")
we.need.more.lower = dwp$we.need.more.lower
so.i.will.lower = dwp$so.i.will.lower

score.sentiment = function(sentences, pos.words, neg.words, .progress = 'none'){
  
  scores = laply(sentences, function(sentence, pos.words, neg.words){
    
    sentence = gsub("[[:punct:]]", "", sentence)
    sentence = gsub("[[:cntrl:]]", "", sentence)
    sentence = gsub("\\d+", "", sentence)
    
    # and convert to lower case:
    sentence = tolower(sentence)
    
    # split into words. str_split is in the stringr package
    word.list = str_split(sentence, "\\s+")
    
    # sometimes a list() is one level of hierarchy too much
    words = unlist(word.list)
    
    # compare our words to the dictionaries of positive & negative terms
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    
    # match() returns the position of the matched term or NA
    # we just want a TRUE/FALSE:
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    
    ### we could have used "pos.matches = words %in% pos.words" to
    #get the TRUE/FALSE instead of using "match() and !is.na()"
    # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
    score = sum(pos.matches) - sum(neg.matches)
    
    return(score)
    
  }, pos.words, neg.words, .progress = .progress )
  scores.df = data.frame(score = scores, text = sentences)
  return(scores.df)
}

scores = score.sentiment(we.need.more.lower, posi, nega, .progress = 'text')
dwp$we.need.more.score = scores


scores = score.sentiment(so.i.will.lower, posi, nega, .progress = 'text')
dwp$so.i.will.lower.score = scores


housing = subset(dwp, dwp$topic.lowercase == 'housing')
equity = subset(dwp, dwp$topic.lowercase == 'equity')
employment = subset(dwp, dwp$topic.lowercase == 'employment')
transportation = subset(dwp, dwp$topic.lowercase == 'transportation')
education = subset(dwp, dwp$topic.lowercase == 'education')

mean(housing$we.need.more.score$score)
mean(housing$so.i.will.lower.score$score)
qplot(housing$we.need.more.score$score, geom="histogram", binwidth=1, xlab="HOUSING: We need more sentiment score")
qplot(housing$so.i.will.lower.score$score, geom="histogram", binwidth=1, xlab="HOUSING: So I will sentiment score")

mean(transportation$we.need.more.score$score)
mean(transportation$so.i.will.lower.score$score)
qplot(transportation$we.need.more.score$score, geom="histogram", binwidth=1, xlab="TRANSPORTATION: We need more sentiment score")
qplot(transportation$so.i.will.lower.score$score, geom="histogram", binwidth=1, xlab="TRANSPORTATION: So I will sentiment score")

mean(equity$we.need.more.score$score)
mean(equity$so.i.will.lower.score$score)
qplot(equity$we.need.more.score$score, geom="histogram", binwidth=1, xlab="EQUITY: We need more sentiment score")
qplot(equity$so.i.will.lower.score$score, geom="histogram", binwidth=1, xlab="EQUITY: So I will sentiment score")

mean(education$we.need.more.score$score)
mean(education$so.i.will.lower.score$score)
qplot(education$we.need.more.score$score, geom="histogram", binwidth=1, xlab="EDUCATION: We need more sentiment score")
qplot(education$so.i.will.lower.score$score, geom="histogram", binwidth=1, xlab="EDUCATION: So I will sentiment score")

mean(employment$we.need.more.score$score)
mean(employment$so.i.will.lower.score$score)
qplot(employment$we.need.more.score$score, geom="histogram", binwidth=1, xlab="EMPLOYMENT: We need more sentiment score")
qplot(employment$so.i.will.lower.score$score, geom="histogram", binwidth=1, xlab="EMPLOYMENT: So I will sentiment score")
