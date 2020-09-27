AlanTuringLDA
========================================================
author: Nicole Nisbett
date: 30 September 2020
font-family: 'Helvetica'
autosize: true


Latent Dirichlet Allocation (LDA)
========================================================

For more details on authoring R presentations please visit <https://support.rstudio.com/hc/en-us/articles/200486468>.

- Probabilistic algorithm to extract topics from text
- Developed in 2003
- Main assumptions
> > 1. each topic is a collection of words
> > 2. each document is a collection of topics


Preparation
========================================================

To run an LDA model we first need to load some necessary libraries for natural language processing and data manipulation.


```r
library(dplyr)
```

Create the data frames

```r
library(textstem)
library(readr)
library(broom)

raw=read_csv("/Users/nicolenisbett/Documents/PhD/R/Complete Analysis/AnimalFurDebate.csv")

raw[1,]
```

```
# A tibble: 1 x 8
  `Comment Id`    Time    Message           `From Id` From  X6    Likes Comments
  <chr>           <chr>   <chr>                 <dbl> <chr> <lgl> <dbl>    <dbl>
1 60412321332013… 20/05/… I believe the se…        NA <NA>  NA       20        0
```

```r
get_comments = function(file){
  comments=broom::tidy(file$Message)

  #create the comment numbers
  comment_list=paste0( "comment", 1:nrow(file))
  comments[,2]=comment_list
  colnames(comments)=(c("message", "document"))
  comments$message=lemmatize_strings(comments$message)
  return(comments)
}

comment.file=get_comments(raw)
```


Preparation
========================================================


```r
library(tm)

#Prepare data structure for corpus
prep_corpus=function(file){
  #rearrange so file is [id, message]
  df.forCorpus=data.frame(file$document, file$message)
  #rename columns for input into corpus
  colnames(df.forCorpus)=c("doc_id", "text")
  #create corpus
  preCorpus=VCorpus(DataframeSource(df.forCorpus))
  
  return(preCorpus)
}

#Create a corpus
clean_corpus <- function(corpus){
  remove.urls <- function (x) gsub ("http[[:alnum:]]*", "", x)
  corpus <- tm_map(corpus, content_transformer(remove.urls))
  remove.weird.characters <- function (x) gsub("[^0-9A-Za-z///' ]", "", x)
  remove.more.weird.characters <- function (x) gsub('[^ -~]', '', x)
  corpus <- tm_map(corpus, content_transformer(remove.weird.characters))
  
  corpus <- tm_map(corpus, content_transformer(remove.more.weird.characters))
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords, stopwords("en"))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, content_transformer(stripWhitespace))
  
  
  return(corpus)
}

corpus=clean_corpus(prep_corpus(comment.file))
#Create a document-term matrix
dtm=DocumentTermMatrix(corpus)
#remove non-zero rows
rowTotals=apply(dtm, 1, sum)
dtm=dtm[rowTotals>0,]
inspect(dtm)
```

```
<<DocumentTermMatrix (documents: 2783, terms: 3443)>>
Non-/sparse entries: 30073/9551796
Sparsity           : 100%
Maximal term length: 22
Weighting          : term frequency (tf)
Sample             :
             Terms
Docs          absolutely animal ban cruel farm fur need real wear yes
  comment1063          0      5   0     0    1   1    2    0    0   0
  comment1130          1      2   0     0    1  12    0    4    1   0
  comment2105          0     15   3     0    4   8    1    0    0   1
  comment2111          0      3   8     0    2  13    0    0    0   0
  comment2347          0      9   0     0    2   9    0    0    0   0
  comment2367          0      5   2     1    3  16    1    5    0   0
  comment2384          0     10   1     0    9  13    2    3    0   0
  comment2422          0     14   1     1    9  16    5    3    0   1
  comment2521          0      3   3     0    2  11    4    5    1   0
  comment633           0     10   1     5    0  11    0    0    0   1
```

```r
rows <- nrow(dtm)

splitter <- sample(1:rows, round(rows * 0.75))

train.dtm=dtm[splitter,]
test.dtm=dtm[-splitter,]
```

Model building
========================================================


```r
library(ldatuning)
library(topicmodels)

optimal=FindTopicsNumber(
    train.dtm,
    topics = seq(from = 2, to = 20, by = 2),
    metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010"),
    method = "VEM",
    control = list(seed = 1234),
    mc.cores = 2L,
    verbose = FALSE
  )

FindTopicsNumber_plot(optimal)
```

![plot of chunk unnamed-chunk-2](AlanTuringLDA-figure/unnamed-chunk-2-1.png)

```r
#lda.model = LDA(dtm, k=, control = list(seed = 1234), method="Gibbs")
```

Model building
========================================================

Use perplexity to evaluate performance of models


```r
#create the models
get_topic_model=function(dtm, k){
  model = LDA((dtm), k=(k), control = list(seed = 1234), method="VEM")
  return(model)
}

#Test with different values of k indicated in plot
perplexity(get_topic_model(train.dtm, 10), test.dtm)
```

```
[1] 216608.1
```

```r
perplexity(get_topic_model(train.dtm, 12), test.dtm)
```

```
[1] 220320.3
```

```r
perplexity(get_topic_model(train.dtm, 14), test.dtm)
```

```
[1] 220772.9
```

```r
perplexity(get_topic_model(train.dtm, 16), test.dtm)
```

```
[1] 221847.2
```

```r
#lda.model = LDA(dtm, k=, control = list(seed = 1234), method="Gibbs")
```


Model building
========================================================

They all have similar scores so I will chose a manageable number of 10 

```r
lda.model = LDA(dtm, k=10, control = list(seed = 1234), method="VEM")
```

Exploring the topics
========================================================

Gamma scores give the per-document topic distributions

![plot of chunk unnamed-chunk-5](AlanTuringLDA-figure/unnamed-chunk-5-1.png)

Visualisation
========================================================
<!--

```
     row col
     row col
```

<!--html_preserve--><!DOCTYPE html>
<html>
  <head>
    <meta http-equiv="Content-Type" content="text/html; charset=utf-8">
    <title>LDAvis</title>
    <script src="d3.v3.js"></script>
    <script src="ldavis.js"></script>
    <link rel="stylesheet" type="text/css" href="lda.css">
  </head>

  <body>
    <div id = "lda"></div>
    <script>
      var vis = new LDAvis("#lda", "lda.json");
    </script>
  </body>

</html><!--/html_preserve-->

<head>
  <script src="alan/d3.v3.js"></script>
  <script src="alan/ldavis.js"></script>
  <link rel="stylesheet" type="text/css" href="alan/lda.css">
</head>
<body>
  <div id = "mydiv"></div>
  <script>
    var vis = new LDAvis("#mydiv", "lda.json");
  </script>
</body>

<iframe width="2000" height="1000" src="alan/index.html" frameborder="0"></iframe>

-->
