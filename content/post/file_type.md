---
title: "File Types in R - Rookie Mistakes and Lessons Learned"
author: "Henry Overos"
date: "9/19/2020"
output: html_document
---
I did something dumb. Something that I will never do again. It caused so much frustration that was needless but, as with many things that I fail at, I've learned a great deal in the process.

In a previous post I mentioned how I had come up with a way to deal with an issue with RData files. It turns out - that code was all wrong. I also learned that RData files are not intended for what I thought - general saving of R objects.

You *can* save any object in R as an RData file. This is true. But the utility of the `save()` and `load()` functions is that you can actually combine multiple files into the RData output. This means, for example, that if I had wanted to combine the models and data necessary to replicate a paper I could save them under one file name.
That's useful for specific situations. In my use case, however, I was simply saving data frames at timed intervals after using the twitter API. I thought that I had overcome the problem of named objects by saving each one as a differently named RData file. I was wrong.

Rdata files are unique because, as I said previously, the file type is meant to contain a number of objects if necessary. This means that the file name and the object name can and usually are different. So even thought I loaded a file called "tweets123.RData", the object that was loaded is called "tweets". This is a problem because I have 30 objects, all with the same name. So loading them means that they override each other. I eventually came up with a sloppy workaround. Basically, I made an empty data frame and bind each individual data frame to the empty one as the objects loaded. The process looked like this:

```{r}
file <- list.files(pattern = "tweets")
df <- data.frame()

for (i in 1:length(file)) {
  load(file[[i]])
  newdf <- tweets
  df <- rbind(df, newdf)
}
```
This was an annoying - albeit simple step. To overcome this issue in the future I will save objects as rds files. Rds saves single objects, unlike the RData type. This overcomes the name and file issue that I discovered. The process of loading data frames was such a pain because I just wanted it to work right. Now I know, rookie mistake but lessons learned.
