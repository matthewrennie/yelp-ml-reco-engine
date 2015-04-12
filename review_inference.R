# install.packages("tm")
# install.packages("plyr")
# install.packages("SnowballC")
library(plyr)
# setup data
yelp_review <- read.csv("yelp_academic_dataset_review.csv", stringsAsFactors=FALSE)
yelp_business <- read.csv("yelp_academic_dataset_business.csv", stringsAsFactors=FALSE)
yelp_rev_bus <- join(yelp_review, yelp_business, by="business_id", type="inner")
yelp_rev_bus_AZ <- subset(yelp_rev_bus, state == "AZ")
save(yelp_rev_bus_AZ, file="yelp_rev_bus_AZ.Rda")

# create the corpus http://stackoverflow.com/questions/24501514/keep-document-id-with-r-corpus
library(tm)
reviewReader <- readTabular(mapping=list(content="text", business_id="business_id", user_id="user_id", review_id="review_id"))
tm_yelp_rev_bus_AZ <- VCorpus(DataframeSource(yelp_rev_bus_AZ), readerControl=list(reader=reviewReader))

# cleanup http://stackoverflow.com/questions/26510298/rs-tm-package-for-word-count
tm_yelp_rev_bus_AZ <- tm_map(tm_yelp_rev_bus_AZ, content_transformer(tolower))
tm_yelp_rev_bus_AZ <- tm_map(tm_yelp_rev_bus_AZ, removeWords, stopwords("english"))
tm_yelp_rev_bus_AZ <- tm_map(tm_yelp_rev_bus_AZ, removePunctuation)
tm_yelp_rev_bus_AZ <- tm_map(tm_yelp_rev_bus_AZ, removeNumbers)
tm_yelp_rev_bus_AZ <- tm_map(tm_yelp_rev_bus_AZ, stripWhitespace)
tm_yelp_rev_bus_AZ_stem <- tm_map(tm_yelp_rev_bus_AZ, stemDocument)

# keep only words #http://stackoverflow.com/questions/24501514/keep-document-id-with-r-corpus
keepOnlyWords<-crerrontent_transformer(function(x,words) {
    regmatches(x, 
        gregexpr(paste0("\\b(",  paste(words,collapse="|"),"\\b)"), x)
    , invert=T)<-" "
    x
})

# attempt to infer gender and marital status
maritial.terms<-c("husband","hubby","hubbie","wife")
tm_yelp_rev_bus_AZ_married<-tm_map(tm_yelp_rev_bus_AZ_stem, keepOnlyWords, maritial.terms)
tm_yelp_rev_bus_AZ_married<-tm_map(tm_yelp_rev_bus_AZ_married, stripWhitespace)
tdm_yelp_rev_bus_AZ_married <- TermDocumentMatrix(tm_yelp_rev_bus_AZ_married, control = list(wordLengths=c(1,Inf)))
maritial.freq <- rowSums(as.matrix(tdm_yelp_rev_bus_AZ_married))
maritial.freq <- subset(maritial.freq, maritial.freq >= 1)

# plot marital terms
df <- data.frame(term = names(maritial.freq), freq= maritial.freq)
library(ggplot2)
ggplot(df, aes(x = term, y = freq)) + geom_bar(stat = "identity") + xlab("Marital Terms") + ylab("Count") + coord_flip()

# attempt to infer parenthood and gender of child
parent.terms<-c("child","kid","son","daughter")
tm_yelp_rev_bus_AZ_parent<-tm_map(tm_yelp_rev_bus_AZ_stem, keepOnlyWords, parent.terms)
tm_yelp_rev_bus_AZ_parent<-tm_map(tm_yelp_rev_bus_AZ_parent, stripWhitespace)
tdm_yelp_rev_bus_AZ_parent <- TermDocumentMatrix(tm_yelp_rev_bus_AZ_parent, control = list(wordLengths=c(1,Inf)))
parent.freq <- rowSums(as.matrix(tdm_yelp_rev_bus_AZ_parent))
parent.freq <- subset(parent.freq, parent.freq >= 1)

# plot parental item
df <- data.frame(term = names(parent.freq), freq= parent.freq)
library(ggplot2)
ggplot(df, aes(x = term, y = freq)) + geom_bar(stat = "identity") + xlab("Parental Terms") + ylab("Count") + coord_flip()


