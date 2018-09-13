---
title: Amazon reviews wordcloud
author: Kasia Kulma
date: '2017-03-07'
slug: amazon-reviews-word-cloud
categories:
  - r
tags:
  - amazon
  - wordcloud
description: ''
topics: []
---

In the last month I discovered two things that changed my life: audiobooks and [Yuval Harari](https://en.wikipedia.org/wiki/Yuval_Noah_Harari). The former completely transformed my daily commute, the latter changed the way I think about the surrounding world (with more appreciation for history and politics, to say the least). The cocktail of the two made my brain cells sing.

Harari published two books in the last three years: [Sapiens: A Brief History of Humankind](https://www.amazon.co.uk/Sapiens-Humankind-Yuval-Noah-Harari/dp/1846558239) and more recently: [Homo Deus: A Brief History of Tomorrow](https://www.amazon.co.uk/Homo-Deus-Brief-History-Tomorrow/dp/1910701874/). Both perspective - changing, right?

My opinions aside, what do other readers think about his books? Let's dig into it by scraping Amazon reviews of "Sapiens" and then visualising their most common words in a wordcloud.

### loading packages

```
install.packages("pacman", repos = "http://cran.us.r-project.org")
pacman::p_load(XML, dplyr, stringr, rvest, xml2) # web-scraping
pacman::p_load(tm, rvest, SnowballC, wordcloud) # wordcloud-building
```

### importing Amazon reviews for Yuval Harari's "Sapiens"

The web-scraping procedures that follow were shamelessly "borrowed" from [Riki Saito's blog](https://justrthings.wordpress.com/2016/08/17/web-scraping-and-sentiment-analysis-of-amazon-reviews/). Thanks, mate!

The following code requires *Amazon's product code*, which can be found in the product's URL. Next, it scrapes the product's name, just to confirm we got everything right!

```
# define a function removing all white space
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

# define product code and the url
prod_code = "1846558239"
url <- paste0("https://www.amazon.co.uk/dp/", prod_code)
doc <- xml2::read_html(url)

#obtain the text in the node, remove "\n" from the text, and remove white space
prod <- html_nodes(doc, "#productTitle") %>% html_text() %>% gsub("\n", "", .) %>% trim()
prod
```

    ## [1] "Sapiens: A Brief History of Humankind"

So far, so good. Now, after sourcing `amazon_scraper` function from Riki's Github page, I import first 50 pages of reviews of *Sapiens*:

```
# THE KEY: Source function to parse Amazon html pages for data
source("https://raw.githubusercontent.com/rjsaito/Just-R-Things/master/Text%20Mining/amazonscraper.R")
```

```
pages <- 50

reviews_all <- NULL

for(page_num in 1:pages){
  url2 <- paste0("http://www.amazon.co.uk/product-reviews/",prod_code,"/?pageNumber=", page_num)
  doc2 <- read_html(url2)

  reviews <- amazon_scraper(doc2, reviewer = F, delay = 2)
  reviews_all <- rbind(reviews_all, reviews)
}
```

It looks like everything worked! The returned data frame contains not only date, title, author and content of every review, but also number of stars given, format of the book that the review is for and even the number of people that thought this particular review was helpful, priceless!


```
str(reviews_all)
```

    ## 'data.frame':    500 obs. of  8 variables:
    ##  $ title       : chr  "A subjective history of Humankind" "Insights on every page" "Thrilling iconoclastic book of human history" "Good view of the past, questionnable view of the future" ...
    ##  $ author      : chr  "Strv 74" "M. D. Holley" "Wasim Zarniwoop" "Hugo" ...
    ##  $ date        : chr  "17 November 2014" "27 September 2014" "10 September 2014" "19 January 2015" ...
    ##  $ ver.purchase: num  1 0 1 1 0 1 0 1 1 0 ...
    ##  $ format      : chr  "Format: Hardcover" "Format: Hardcover" "Format: Kindle Edition" "Format: Paperback" ...
    ##  $ stars       : num  4 5 5 3 5 4 5 5 2 1 ...
    ##  $ comments    : chr  "This book looked interesting and I bought it without having read up on everything about it. I thought is was about how we came "| __truncated__ "Beautifully written, this book contains one incredible idea: that the success of homo sapiens derives from our inclination to b"| __truncated__ "The problem with human prehistory is the lack of evidence and information. What we have is little and many books use this littl"| __truncated__ "I had given this book five stars in my original review. Then, I down-rated it to 4 stars. As I read more about the subjects in "| __truncated__ ...
    ##  $ helpful     : num  109 174 89 120 54 39 67 2 26 73 ...

In this post I'll focus on the review content only. Here's what the exemplary comment looks like:

``` 
reviews_all[1, 7]
```

    ## [1] "This book looked interesting and I bought it without having read up on everything about it. I thought is was about how we came to be the one surviving race of Humans on the planet. It was not. That was taken care of during the first 20 pages. The Rest is about how the single surviving human race developed into what we are today.But I am glad that I bought and read it because it was well worth reading. It is a tour through a number of different parts of human history that the author thinks has had a great influence on our species. It is a very easy read and the book is filled with facts that makes you stop and think before reading on. A lot of the theories that he puts forward are very interesting and at times thought provoking. Most of the times I found myself agreeing with his views or at least gave him credit for a very interesting view point.But there are also a number of points that he brings up that are not that convincing and in some cases probably wrong. Let me give you a few examples:- He states that the Middle East has never been as peaceful as today during the last 3000 years. This probably comes as a surprise to a lot of people fleeing from the ongoing wars. I am fairly certain that there were long periods of peace between wars during 1000 BC and 1 AD that had no wars at all.- He states that humans have not evolved during the last 70 000 years. If you put a 70 000 year old human up for an autopsy no one would know that he was 70 000 years old. This is a strange statement since humans mutate all the time and the rate of mutations has never been faster than during the last 500 years (see \"The 10 000 Year Explosion. How Civilization Accelerated Human Evolution\")- He states that there is no proof that human intelligence has changed during the last 70 000 years. But only this year there has been published studies claiming that we are more intelligent than just 100 years ago. Of course we are more intelligent than thousands of years ago. That does not mean that people were not intelligent thousands of years ago as well.There are a lot of discussion on why men are still the rulers of the world and why women are now catching up. Here his views are not very convincing. His views on war has taken a sharp hit from the ongoing Russian war against Ukraine showing that even a book published in 2014  did not see that coming.His views on religion, sex and economy are far stronger and more convincing. But his logic behind proving that homosexuality is not \"unnatural\" (and of course it is not) also proves that all kinds of sexual variants are \"natural\". Even those that are considered criminal today. Maybe a few more lines on this subject would have been helpful.Strange enough there are aspects of history that he for some reason does not discuss at all. One example is Democracy and the impact of that on the world. An other aspect is the environment.But the book is very engaging. You are all the time caught up in his argument and more often than not find yourself supporting them. A book well worth reading and discussing with your friends and colleagues."

As you can tell, it's not in the best shape for the analysis: it contains punctuation signs, numbers, lower and upper letters, etc. Let's sort it out by doing some text pre-processing:

### text pre-processing

```
#### creating corpus
m <- list(content = "comments")
myReader <- readTabular(mapping = m)

final_reviews <- data.frame(comments = reviews_all$comments)
ds <- DataframeSource(final_reviews)

## create corpus with all the reviews
sapiens_corpus <- VCorpus(ds)

## remove punctuation
sapiens_corpus = tm_map(sapiens_corpus, removePunctuation)

## remove numbers
sapiens_corpus = tm_map(sapiens_corpus, removeNumbers)

## LowerCase
sapiens_corpus = tm_map(sapiens_corpus, tolower)
```

I also removed some uninformative words, together with English stopwords, to make the results clearer:

```
## remove stopwords and other words
myWords=c("format", "paperback", "kindle", "edit", "hardcov", "book", "read", "will", "just", "can", "much")

sapiens_corpus <- tm_map(sapiens_corpus, removeWords, c(stopwords("english"), myWords))

## treat pre-processed documents as text documents
sapiens_corpus <- tm_map(sapiens_corpus, PlainTextDocument)

## turn into doc matrix
sapiens_dtm <- DocumentTermMatrix(sapiens_corpus)
```

Let's have a quick peek into the top 20 most frequent words:

```
# displaying most frequent words
freq <- sort(colSums(as.matrix(sapiens_dtm)), decreasing=TRUE)   
head(freq, 20)  
```

    ##     history       human     sapiens         one      author        well
    ##         257         181         151         144         112         112
    ##      harari     reading        many interesting     thought     species
    ##         110         104         100          99          97          96
    ##      really        homo         way       think       world      humans
    ##          92          91          85          84          82          78
    ##        like        time
    ##          78          78

And finally, crème de la crème, the wordcloud showing 250 most frequent words found in Amazon reviews of *Sapiens*:

### creating a wordcloud with the top 250 most frequent words

```
pal=brewer.pal(9, "Set1")

set.seed(100)
wordcloud(words = names(freq), freq = freq, max.words=250,
          random.order=FALSE,
          colors=pal)
```

![wordcloud](/post/2017-03-07-amazon-reviews-word-cloud_files/unnamed-chunk-10-1.png)

As pretty as it looks, it's not too informative regarding how positive / negative the reviews were. For this, in my next post I'll run a [sentiment analysis](https://en.wikipedia.org/wiki/Sentiment_analysis) on reviews of both books, so watch this space!