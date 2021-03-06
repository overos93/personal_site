---
title: "Saving multiple RData Files Automatically"
author: "Henry Overos"
date: "8/10/2020"
output: html_document
---
# How to save a file in R on a script automatically

Recently, I needed to save data files automatically after they were scraped for a research project. I realized that this presented an issue when it comes to naming objects in `R`. My normal method for creating objects in `R` code would involve something simple like this:

```{r, eval=FALSE}
x <- (1:10)
df <- x
save(df, file = "df.RData")
```

This would work if there was just the one object that wasn't updated. Then I realized that what I needed to do was save the object with a unique name every day that I created the new dataset so that the file wouldn't be overwritten in my directory.

Here's my example. I am saving a batch of tweets from the twitter API once a day. Let's say that I'm searching for tweets about the spread of COVID-19 in Kenya.

```{R eval = F}
# kenya twitter search
# henry overos
# 

pacman::p_load(tidyverse, rtweet, tidytext)

appname <- "app"

key <- "Y"

secret <- "X"

access_token <- "Z"

access_secret <- "W"

twitter_token <- create_token(
  app = appname,
  consumer_key = key,
  consumer_secret = secret,
  access_token = access_token,
  access_secret = access_secret
)

kenya_tweets <- search_tweets2(q = c("#kenya",
                                   "Covid-19",
                                   "Kenya debt"),
                            n = 100,
                            include_rts = FALSE)
kenya_users <- search_users(q = "kenya",
                          n = 50)                            
```

Okay so if I just ran this once I could simply write:

```{r, eval=F}
save(kenya_tweets, file = "kt.RData")
save(kenya_users, file = "ku.RData")
```

But I plan on having this script run automatically every day for a while.

So, I'll use the `Sys.Date()` function to create unique timestamps for the file names. That way they won't override each other in my directory.

```{r eval=F}

dates <- Sys.Date() 

for (date in dates) {
  outStr1 <- paste0("kt", date)
  outStr2 <- paste0("ku", date)
  assign(outStr1, sgr_tweets)
  assign(outStr2, sgr_users)
  save(kenya_tweets, file = paste0(outStr1,".RData"))
  save(kenya_users, file = paste0(outStr2,".RData"))
}
```

This is my initial solution. I'm sure there are packages or alternative options to solve a simple file-related problem. That being said, it is working for me now and the code is easy to expand upon if I need to save more files. The key lines here are the two assign funcs. This is how you add a new name for an object post-hoc.
