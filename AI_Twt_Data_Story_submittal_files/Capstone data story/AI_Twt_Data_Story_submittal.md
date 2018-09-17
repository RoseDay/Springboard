Milestone Report - AI Tweets Data Story
=======================================

Introduction
------------

This is an Rmarkdown document for the data exploration of tweets related to artificial intelligence. In the current narrative, the concerns around artificial intelligence (AI) are captured in: the lack of transparency denoted by references to malevolent hidden algorithms; the much heralded current and potential job losses; poorly explained or understood advances in science.

After Americans are finally awakening to the privacy incursions made by businesses whose revenue models are based on customer data and surveillance, the trust in technology companies has rapidly declined. This analysis will provide a baseline assessment upon which to design branding, postioning and remedial measures, perhaps obtaining insights that can be used to build trust.

My proposed approach is to use R to search and select Twitter message content for the keywords "artificial intelligence" and "AI." Then conduct sentiment analysis on the messages (text) for negative and positive sentiments, especially trust. Results will include frequencies of sentiments, and additional analysis for negative sentiments and lack of trust.

### Data Extraction and Changes

The Twitter data will be used from seven consecutive days in June 2018. The data was acquired by API and reflects the revised policy on downloading Twitter data which restricts the period for which it may be extracted for free to seven days preceding extraction. This differs from my original proposal for selecting Twitter data from the days immediately following two separate events: July 25, 2017 occurence of the Twitter exchange between Zuckerberg and Musk; and the New Work Summit which began on February 12, 2018.

As a result, the Tweeting public's sentiment about the argument between Mark Zuckerberg and Elon Musk about whether AI should be vaunted or feared cannot be assessed. Because there were no AI newsworthy events in the 7-day period, my assumption is that only Tweeters that have a special interest in AI and marketers of products in the AI space are included. My data set does not allow for informative time series analysis and topic modeling. Hence, my initial proposal for establishing a baseline for public sentiments surrounding use of AI cannot be accomplished. However, using my study plan for multiple 7-day data extractions or 7-day data extractions following AI news events could achieve my original proposed goal.

Twitter Data about AI from 7 days, June 2018
--------------------------------------------

This milestone report reflects the data extracted, my selection of which variables to use, my cleaning of the data selected, and my plan on how to use the data for sentiment analysis.

``` r
library(tidyverse)
```

    ## -- Attaching packages --------------------------------------------------------------- tidyverse 1.2.1 --

    ## v ggplot2 2.2.1     v purrr   0.2.4
    ## v tibble  1.4.2     v dplyr   0.7.4
    ## v tidyr   0.8.0     v stringr 1.3.0
    ## v readr   1.1.1     v forcats 0.3.0

    ## -- Conflicts ------------------------------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(rtweet)
library(qdapRegex)
```

    ## 
    ## Attaching package: 'qdapRegex'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     explain

    ## The following object is masked from 'package:ggplot2':
    ## 
    ##     %+%

``` r
library(tidytext)
```

Collection of free Twitter data for 7-day period using the `rtweet` package.

AItwtdata &lt;- search\_tweets(q="artificial intelligence OR AI OR Artificial Intelligence", n = 12000, retryonratelimit = TRUE, type = "mixed")

Saving data for use later.

``` r
#saveRDS(AItwtdata, "AItwtdata.Rds")
AItwtdata <-readRDS("AItwtdata.RDS")
```

Data preparation and exploration
--------------------------------

Filtering for tweets only in English.

``` r
AItwtdata <-AItwtdata %>% filter(lang == 'en')
```

The data is comprised of 11,276 English tweets and 42 variables. The tweet content text variable is the area of focus for sentiment analysis. This is a sample of text.

``` r
head(AItwtdata$text, 10)
```

    ##  [1] "supercomputing  artificialintelligence  skills  our new digital europe programme will shape europe's digital transformation to the benefit of citizens and businesses  today  we will present our  eubudget proposals on digital europe  outline explained  "                  
    ##  [2] "our artificialintelligence polling firm  polly  says 35  of voters are talking about the  renataford controversy  with 74  feeling more negative about  fordnation because of it  21  are defending ford and 5  neutral  no idea about fx on seat count   onpoli  election2018"
    ##  [3] "ok  here's a question for  narendramodi how do you promote excellence in india including in artificialintelligence etc while depressing the standards dramatically for half the population in college and in govt jobs  seems you're setting india up for a huge fall"         
    ##  [4] "our 2018 cgg essay prize is out   chance to win  100 amazon vouchers  up to 2 500 words on the topic  will artificialintelligence eventually replace cancer geneticists   details at   clingensoc  britsocgenmed  bmastudents  theagnc  acgs news"                             
    ##  [5] "rt  peopledoc inc   rpa doesn't mean hr will be replaced with robots  instead   hr will take  a hybrid approach  mixing processes with peop "                                                                                                                                  
    ##  [6] "rt  wired  apple hopes to inspire a new generation of ideas from developers by making it easier for them to incorporate machinelearning in "                                                                                                                                   
    ##  [7] "ai is driving the evolution of interconnected digital supply networks  this change can help supply chain managers be more dynamic  flexible  and efficient in their planning and execution "                                                                                   
    ##  [8] "rt  deepaerodrones  artificialintelligence chipmaker hailo raises  12 5 million  see more    ai  blockch "                                                                                                                                                                     
    ##  [9] "rt  civichall   opportunity   mozilla wants to help you help internet users understand ai  media makers and  technologists  apply now for a "                                                                                                                                  
    ## [10] "webinar  how artificialintelligence is transforming insurance  june 22  2018  ai  ml  dl"

Removing URLs from the text and add the selection criteria words.

``` r
AItwtdata$text <- rm_twitter_url(AItwtdata$text)

custom1_stop_words <- bind_rows(data_frame(word = c("artificialintelligence", "ai"), 
lexicon = c("custom")), stop_words)
```

### Initial tokenization and word count

``` r
tidyAItwt0 <- AItwtdata %>%
select(text) %>%
mutate(line = row_number()) %>%
unnest_tokens(word, text) %>%
anti_join(custom1_stop_words)
```

    ## Joining, by = "word"

Looking at word counts:

``` r
tidyAItwt0 %>% count(word, sort = TRUE)
```

    ## # A tibble: 11,372 x 2
    ##    word                n
    ##    <chr>           <int>
    ##  1 rt               6602
    ##  2 machinelearning  1375
    ##  3 gt                755
    ##  4 bigdata           752
    ##  5 future            685
    ##  6 read              653
    ##  7 business          652
    ##  8 amp               594
    ##  9 3                 506
    ## 10 artificial        479
    ## # ... with 11,362 more rows

Visualizing all words except for criteria selection words and the 6602 "rt," an artifact of Twitter, indicating retweet.

``` r
 tidyAItwt0 %>% 
  count(word, sort=TRUE) %>%
  filter(substr(word, 1, 1) != '#', # omit hashtags
         substr(word, 1, 1) != '@', # omit Twitter handles
         n > 400, n < 2500) %>% # only most common words
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = word)) +
  geom_bar(stat = 'identity') +
  xlab(NULL) +
  ylab("Word Count") +
  ggtitle('Most common words in tweets') +
  theme(legend.position="none") +
  coord_flip()
```

![](AI_Twt_Data_Story_submittal_files/figure-markdown_github/unnamed-chunk-8-1.png)

The word count reveals some non-English characters (converted they look like this <U+___>) and some word pairs that are technical jargon, which would not add anything to the sentiment analysis. However, the words of the jargon word-pairs occurring singly may contribute to the sentiment analysis. For example, "learning" may contribute positively, but "deep learning" should not be included. In order to render the most sentiment from these small messages, further cleaning of the data needs to occur preserving the unpaired words.

Removing the remaining non-English characters using `qdapRegex`.

``` r
AItwtdata$text <- gsub("[^0-9A-Za-z///']"," ", AItwtdata$text)
```

Preserving single words by joining word pairs

``` r
AItwtdata$text <- tolower(AItwtdata$text)
AItwtdata$text <- gsub(pattern = "artificial intelligence", "artificialintelligence", AItwtdata$text)
AItwtdata$text <- gsub(pattern = "deep learning", "deeplearning", AItwtdata$text)
AItwtdata$text <- gsub(pattern = "machine learning", "machinelearning", AItwtdata$text)
AItwtdata$text <- gsub(pattern = "big data", "bigdata", AItwtdata$text)
AItwtdata$text <- gsub(pattern = "data science", "datascience", AItwtdata$text)
```

Adding words to second custom stop word list

``` r
custom2_stop_words <- bind_rows(data_frame(word = c("artificialintelligence", "ai", "rt", "gt", "amp", "machinelearning","ml", "deeplearning","bigdata", "datascience", "3", "000", "1", "5", "10", "12", "2018", "2", "tech", "dl","iot", "software", "cybersecurity", "hr"), lexicon = c("custom")), stop_words)
```

Using added custom stop words and checking a word cloud for other words which do not belong in sentiment analysis.

``` r
tidyAItwt0 <- AItwtdata %>%
  select(text) %>%
  mutate(line = row_number()) %>%
  unnest_tokens(word, text) %>%
  anti_join(custom2_stop_words)
```

    ## Joining, by = "word"

``` r
tidyAItwt0 %>% count(word, sort = TRUE)
```

    ## # A tibble: 11,350 x 2
    ##    word            n
    ##    <chr>       <int>
    ##  1 future        685
    ##  2 read          653
    ##  3 business      652
    ##  4 world         479
    ##  5 ipfconline1   477
    ##  6 google        457
    ##  7 human         403
    ##  8 technology    390
    ##  9 data          382
    ## 10 fintech       370
    ## # ... with 11,340 more rows

Making a word cloud.

``` r
library(wordcloud)
```

    ## Loading required package: RColorBrewer

``` r
tidyAItwt0 %>%
   anti_join(custom2_stop_words) %>%
   count(word) %>%
   with(wordcloud(word, n, max.words = 100))
```

    ## Joining, by = "word"

![](AI_Twt_Data_Story_submittal_files/figure-markdown_github/unnamed-chunk-14-1.png) Adding proper nouns to a third custom stop word list and tokenizing.

``` r
custom3_stop_words <- bind_rows(data_frame(word = c("artificialintelligence", "ai", "rt", "gt", "amp", "machinelearning","deeplearning","bigdata", "datascience", "apple", "microsoft", "ibm", "amazon", "ipfconline1", "quindazzi", "mckinsey", "mit", "iainljbrown", "nitiaayog", "vanloon", "google","ronald", "apple's", "forbes", "blockchain", "mozilla", "deeplearn007", "thomas1774paine", "3", "000", "1", "5", "10", "12", "2018", "2", "tech", "ml", "dl", "pentagon", "digitaltransformation", "mgi", "weftrends", "india", "europe","iot", "software", "cybersecurity", "hr", "fintech", "deepaerodrones", "mikequindazzi", "thenextweb", "valaafshar"),
lexicon = c("custom")), stop_words)
```

``` r
tidyAItwts <- AItwtdata %>%
  select(text) %>%
  mutate(line = row_number()) %>%
  unnest_tokens(word, text) %>%
  anti_join(custom3_stop_words)
```

    ## Joining, by = "word"

The final word cloud displays the words for conducting sentiment analysis. There will be some words that will go unmatched in the three reference lexicons' word lists and other neutral words.

``` r
tidyAItwts %>%
   anti_join(custom3_stop_words) %>%
   count(word) %>%
   with(wordcloud(word, n, max.words = 100))
```

    ## Joining, by = "word"

![](AI_Twt_Data_Story_submittal_files/figure-markdown_github/unnamed-chunk-17-1.png)

Sentiment Analysis
==================

`Tidyverse`contains reference lexicons for conducting sentiment analysis. The word-wise sentiment analyses to be performed on the AI tweet data are:

-   on the corpus, composed of sentiment words in all tweet texts (tweets) for the 7 days preceding the data extraction (NOTE: this has changed since my original proposal because the Twitter data policy has changed, not allowing the selection of data specific extractions);
-   characterizing users' tweets as postive or negative with data grouped by user; and
-   using bigrams for context, especially for negating sentiments.

There are three lexicons in the `tidyverse` package, two binary lexicons - `Bing` and `NRC`, and one that is an ordinal range from -5 to 5, `AFINN`. I will utilize the specific lexicon which I think works best for the aspect of analysis while making use of all three. I will provide visualization of my results with a plot which illustrates the data's meaning.

Sentiments of single words in corpus of tweets
----------------------------------------------

### Analysis with the Bing lexicon

The `Bing` lexicon reference list provides for *positive* and *negative* sentiments. I will explore the net sentiment of corpus by word sentiment with `bing,` capturing the frequencies that the words occur and identifying the words themselves.

``` r
bing_nets <- tidyAItwts %>%
  inner_join(get_sentiments("bing")) %>%
  count(line, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)
```

    ## Joining, by = "word"

### Using the `NRC` lexicon

The `NRC` lexicon has ten sentiments, the *positive* and the *negative*, like are included in `Bing`, and more specific descriptors: *anger*; *anticipation*; *disgust*; *fear*; *joy*; *sadness*; *surprise*; and *trust*.

I will explore the *negative* and *positive* word counts in the total pool of tweets using `NRC` just as I did with `Bing.`

``` r
nrc_counts <- tidyAItwts %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment)
```

    ## Joining, by = "word"

Then, I will explore tweet words associated with the specific sentiments included in the `NRC` lexicon. This will be important because my initial reason for the study is determining if artificial intelligence is trusted. Given my truncated data sample (relative to what I had intended with substantially access to more tweets), this turns out to be whether Tweeters trust AI and not an indicator of the general public's trust.

Then, I will compare the frequencies of the specific sentiments to identify and explore the top two sentiments.

``` r
nrc_specific_tops <- nrc_counts %>%
  group_by(sentiment) %>% 
  filter(sentiment %in% c("anger", "anticipation", "disgust", "fear", "joy", "sadness", "surprise", "trust")) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n))
```

    ## Selecting by n

Characterizing tweeters' sentiments by words
--------------------------------------------

I will explore the sentament word usage by author (associated with the identifier `user_id`). The total pool of words included in tweet texts may provide a more meaningful sentiment analysis to single word analysis in the corpus. Although, the approach provided by `Bing` and `NRC` would allow net sentiments as the aggregation of words by user, the `AFINN` lexicon provides more information about the individual words used.

### Using the `AFINN` lexicon

`AFINN` scores words from a negative sentiment to positive sentiment in a range of scoring -5 to 5. Through aggregating the word score, the net sentiment of the user (i.e., the total score by user) can be positive or negative. This measure of sentiment is subject to how many words are common to `AFINN` and the tweets, the relative degree of the word score, and the length and quantity of the tweets of a particular user.

Assigning words to a particular user.

``` r
tidyIDtwts <- AItwtdata %>%
select(text, user_id) %>%
unnest_tokens(word, text) %>%
anti_join(custom3_stop_words)
```

    ## Joining, by = "word"

``` r
head(tidyIDtwts)
```

    ## # A tibble: 6 x 2
    ##   user_id   word          
    ##   <chr>     <chr>         
    ## 1 157981564 supercomputing
    ## 2 157981564 skills        
    ## 3 157981564 digital       
    ## 4 157981564 programme     
    ## 5 157981564 shape         
    ## 6 157981564 europe's

Bigrams
-------

I will word pairs occurring most frequently in the corpus.

``` r
twtAIbigrams<- AItwtdata %>%
select(text) %>%
mutate(line = row_number()) %>%
unnest_tokens(bigram, text, token = "ngrams", n = 2)

twtAIbigrams %>% 
  count(bigram, sort = TRUE)
```

    ## # A tibble: 44,760 x 2
    ##    bigram                         n
    ##    <chr>                      <int>
    ##  1 artificialintelligence and   840
    ##  2 of the                       629
    ##  3 of artificialintelligence    593
    ##  4 artificialintelligence to    587
    ##  5 in the                       554
    ##  6 artificialintelligence in    544
    ##  7 the future                   506
    ##  8 and artificialintelligence   468
    ##  9 how artificialintelligence   453
    ## 10 read more                    451
    ## # ... with 44,750 more rows

Then, I will separate the bigrams to allow filtering of stop words from word1 and/or word2, and provide the opportunity to filter for specific words. Using the prior custom stop word list developed for single words provides more meaningful bigrams.

``` r
AIbigram_sep <- twtAIbigrams %>% 
  separate(bigram, c("word1", "word2"), sep = " ")
```

Also, I will conduct contextual analysis where there are Word pairings in which the first word is negative word, thereby reversing the meaning of the sentiment identified.
