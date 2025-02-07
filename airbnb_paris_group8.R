######################
# Sentiment Analysis #
######################

## Install Packages (if needed)

install.packages("sentimentr")
install.packages("syuzhet")
install.packages("tm")

## Load Packages and Set Seed

library(sentimentr)
library(syuzhet)
library(tm)
set.seed(1)

## Read in the Listings and Reviews Data

listings <- read.csv(file.choose()) ## Choose Paris_listings.csv
reviews <- read.csv(file.choose()) ## Choose Paris_reviews.csv

## Run if Mac
reviews$comments <- iconv(reviews$comments, to = "utf-8-mac")


## Process reviews for sentiment analysis

text <- VCorpus(VectorSource(reviews$comments))
text <- tm_map(text, stripWhitespace)
text <- tm_map(text, content_transformer(tolower))
text <- tm_map(text, removeWords, stopwords("english"))
text <- tm_map(text, stemDocument)
sentiment <- get_sentiment(text$content, method="syuzhet")

## Add sentiment score to listings file

listings$sentiment <- sentiment

#################
# Marketing Mix #
#################

## Look at means of variables

lapply(sapply(listings,mean),round,2)

## Check for multicollinearity
cor_vars <- c ("occupancy", "price","number_of_reviews","rating","accommodates", "minimum_nights",
               "bedrooms","bathrooms", "beds", "host_is_superhost","pro_host","entire_home","instant_bookable","sentiment")
cor_data <- listings[cor_vars]
cor_table_test <- cor(cor_data)
round(cor_table_test,2)

## Check for multicollinearity - minus beds, bedrooms, bathrooms
cor_vars <- c ("occupancy", "price","number_of_reviews","rating", "minimum_nights",
                "accommodates", "host_is_superhost","pro_host","entire_home","instant_bookable","sentiment")
cor_data <- listings[cor_vars]
cor_table <- cor(cor_data)
round(cor_table,2)

## Run the regression
paris_reg_test <- lm(log(occupancy+1) ~ log(price) + log(number_of_reviews+1) + rating +
                  log(accommodates) + bedrooms + bathrooms + beds + log(minimum_nights+1) +  host_is_superhost + 
                  pro_host + entire_home + instant_bookable + sentiment, data = listings)
summary(paris_reg_test)


paris_reg <- lm(log(occupancy+1) ~ log(price) + log(number_of_reviews+1) + rating +
                       log(accommodates)  + log(minimum_nights+1)  + 
                       pro_host + entire_home + instant_bookable + sentiment, data = listings)
summary(paris_reg)


#############################
# Structural Topic Modeling #
#############################

## Install Packages (if needed)

install.packages("stm")
install.packages("tm")
install.packages("Rtsne")
install.packages("rsvd")
install.packages("geometry")
install.packages("SnowballC")
install.packages("wordcloud")

## Load Packages and Set Seed

library(stm)
library(tm)
library(Rtsne)
library(rsvd)
library(geometry)
library(SnowballC)
library(wordcloud)
set.seed(1)

reviews$rating <- listings$rating

## Run if Mac
reviews$comments <- iconv(reviews$comments, to = "utf-8-mac")

## Process Documents

customwords = c("Airbnb", "Paris", "paris", "airbnb")
processed <- textProcessor(reviews$comments, metadata = reviews, 
    customstopwords=customwords)
out <- prepDocuments(processed$documents, processed$vocab, processed$meta)
docs <- out$documents
vocab <- out$vocab
meta <- out$meta

## Determine Number of Topics (Takes Significant Time)

reviewsFit <- stm(documents = out$documents, vocab = out$vocab, K = 0, seed = 1,
  prevalence =~ rating, data = out$meta, init.type = "Spectral")

## See how many topics

num_topics <- reviewsFit$settings$dim$K
num_topics

## See which topics relate to high vs. low ratings

out$meta$rating <- as.factor(out$meta$rating)
prep <- estimateEffect(1:num_topics ~ rating, reviewsFit, meta=out$meta, 
     uncertainty="Global")
plot(prep, covariate="rating", topics=c(1:num_topics), model=reviewsFit, 
     method="difference", cov.value1=5, cov.value2=3,
     xlab="Lower Rating ... Higher Rating", main="Relationship between Topic and Rating",
     labeltype ="custom", custom.labels=c(1:num_topics))

## Visualize Topics
## Replace X with topic number you want to generate a word cloud for

## Positive Topics - Top 3

cloud(reviewsFit, topic=97) 
cloud(reviewsFit, topic=81)
cloud(reviewsFit, topic=18)

## Negative Topics - Bottom 3

cloud(reviewsFit, topic=31)
cloud(reviewsFit, topic=51)
cloud(reviewsFit, topic=60)
