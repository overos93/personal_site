---
title: Analyzing Stocks and ROI via R
author: Henry Overos
date: '2020-10-05'
slug: analyzing-stocks
categories: []
tags: []
subtitle: ''
summary: ''
authors: []
lastmod: '2020-10-05T14:07:25-04:00'
featured: no
image:
  caption: ''
  focal_point: ''
  preview_only: no
projects: []
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE, message = FALSE, warning = FALSE)
```

# Checking the Portfolio:

## Setting up `R`

First, I set up R by loading the `tidyverse` and `tidyquant` packages. These packages allow for easier coding and downloading of financial data from **Yahoo Finance**. 

```{r setup2}
pacman::p_load(tidyverse,tidyquant,quantmod,forecast,tseries,timeSeries,prophet,fGarch)
```

I'm going to start by demonstrating a simple analysis of the **Vanguard S&P 500 ETF** (VOO). VOO is a simple ETF that buys equal fractions of the S&P 500 stocks available. It basically mirrors the market itself. This is a useful baseline to compare the returns on a more advanced portfolio.

First, I download the stock data from the past decade for VOO.
```{r}
VOO <- "VOO" %>% tq_get(get = "stock.prices",
                        from = "2010-01-01",
                        to = "2020-09-21")
knitr::kable(head(VOO))
```

As you can see, the dataset available provides that day's high, low, open and closing prices. It also shows the adjusted price which we will use for our intiial analysis.

I also use the `periodReturn` function, which takes the adjusted closing price and calculates the return for each day in the dataset.

```{r}
VOO_return <- VOO %>% tq_transmute(select = adjusted,
               mutate_fun = periodReturn,
               period = "daily",
               col_rename = "return")
```

Because I wasn't sure if their code was the same as how I would measure returns, I compared returns based on the simple formula below to the `periodReturn` function and the results are the same.
```{r}
n <- nrow(VOO)

VOO_return$hand[1] <- 0

VOO_return$hand[2:n] <- ((VOO$close[2:n] - VOO$close[1:(n-1)])/VOO$close[1:(n-1)])

knitr::kable(head(VOO_return))
```

Lets now visualize the prices of the stock and get an idea of how much you'd make with a small investment a decade ago.

```{r fig.cap= "Figure 1 presents the daily adjusted closing price from the data. The black line is the actual closing price, while the blue is the smoothed average of the price over time."}
VOO %>% 
  ggplot(aes(x = date, y = adjusted)) + 
  geom_line() +
  geom_smooth(method = "loess") +
  labs(title = "VOO Daily Closing Price Over Time") +
  xlab("Date") +
  ylab("Price (US Dollars)")
```

We can see that the price starts at around $100 but today it is worth around 300.

The following graph shows the daily returns of the stock over time. It looks like a general series of positive then negative returns.
```{r}
n <- nrow(VOO_return)
VOO_return$ccret <- NULL 
VOO_return$ccret[1] <- 0
VOO_return$ccret[2:n] <- log(VOO_return$return[2:n]) - log(VOO_return$return[1:(n-1)])

VOO_return %>% 
  ggplot(aes(x = date, y = ccret)) +
  geom_line()
```

This next plot is pretty obvious, considering the price of the stock, but lets say we invested \$1 into it a decade ago. The value of that dollar today would be about \$3.50. That's a pretty great investment! It's definitely slower than other stocks but consider that this is essentially the least risky stock in which you could invest.

```{r}
VOO_return$gret <- 1 + VOO_return$return

VOO_return$fv <- cumprod(VOO_return$gret)

VOO_return %>% 
  ggplot(aes(x = date, y = fv)) + 
  geom_line() +
  geom_smooth(method = "loess") +
  labs(title = "Value of $1 Investment into VOO over Time") +
  xlab("Date") +
  ylab("Value of Investment (US $)")
```

Lets say we wanted to have a regular investment at monthly intervals. So, we put \$1 into the VOO stock every month for the past decade. Lets visualize what that would look like.


```{r}
VOO_return_m <- VOO %>% tq_transmute(select = adjusted,
               mutate_fun = periodReturn,
               period = "monthly",
               col_rename = "return")

VOO_return_m$total_input <- 1:121

VOO_return_m$gret <- 1 + mean(VOO_return_m$return)
mean(VOO_return_m$return)
```

The mean return over 10 years is 0.012 a month. Using the future value of an annuity formula, the average value of $121 invested over the course of 10 years into VOO is:

```{r}
future_value_annuity <- function(pv,pmt,r,k,n){
  pv*(1+(r/k))^(n*k)+pmt*(((1+(r/k))^(n*k)-1)/(r/k))
  }
  
future_value_annuity(pv=1,pmt=1,n=10,r=0.011,k=12)
```
$127.90

Lets say we invested $100 a month, a total of \$12000 over the course of the decade.

```{r}
100*12*10
future_value_annuity(pv=100,pmt=100,n=10,r=0.011,k=12)
```
\$12,790 isn't bad. But given the amount of money you put in, you're actually not making that much in the investment.

For the sake of comparison, lets make a portfolio that invests in multiple, popular stocks as well as VOO. I picked Apple, Microsoft, Netflix and Facebook as well as Vanguard.

```{r}
tickers <- c("VOO", "AAPL", "MSFT", "NFLX", "FB")

mult_stocks <- tickers %>% 
  tq_get(get = "stock.prices",
         from = "2010-01-01",
         to = "2020-09-21")

mult_stocks %>% 
  ggplot(aes(x = date, y = adjusted, color = symbol)) +
  geom_line() +
  ggtitle("Price Chart for Portfolio")

mult_stocks %>% 
  ggplot(aes(x = date, y = adjusted)) +
  geom_line() +
  facet_wrap(~symbol, scales = "free_y") +
  theme_dark() +
  labs(x = "Date", y = "Price")+
  ggtitle("Price for Individual Stocks in Portfolio")
```
As you can see in this comparison chart and the individual graphs for each stock, VOO is a more stable investment that does steadily rise. It has fewer dips and they're not drastic at all, except in the case of a major recession like what occurred in March 2020. These other stocks started out the decade worth a lot less than VOO at its start but their growth has been exponential (literally, in the case of NFLX).

Lets see the returns on this portfolio.

```{r}

mult_returns <- mult_stocks %>% 
  group_by(symbol) %>% 
  tq_transmute(select = adjusted,
               mutate_fun = periodReturn,
               period = "monthly",
               col_rename = "return")

mult_returns %>%
  ggplot(aes(x = date, y = return)) +
  geom_bar(stat = "identity") +
  geom_hline(yintercept = 0) +
  facet_wrap(~symbol, scales = "free_y") +
  scale_y_continuous(labels = scales::percent,
                     breaks = seq(-0.5,0.75,0.05)) +
  ggtitle("Monthly returns for Stock") +
  labs(x = "Date", y = "Returns") +
  scale_fill_brewer(palette = "Set1",   # We will give them different colors instead of black
                     name = "",
                     guide = FALSE)
```
You can see from the above graphs that VOO is relatively consistent in its returns followed by Microsoft and Apple. Facebook presents larger returns in general, usually between 10-20 percent but sometimes spiking to 50. Netflix is the best in terms of what it provides, some months almost providing a return of 80 percent.

```{r}
mult_returns %>%
  mutate(return = if_else(date == "2010-01-29", 0, return)) %>%
  group_by(symbol) %>%  # Need to group multiple stocks
  mutate(cr = cumprod(1 + return)) %>%
  mutate(cumulative_returns = cr - 1) %>%
  ggplot(aes(x = date, y = cumulative_returns, color = symbol)) +
  geom_line() +
  labs(x = "Date", y = "Cumulative Returns") +
  ggtitle("Cumulative returns for all since 2010") +
  scale_y_continuous(breaks = seq(0,70,10),
                     labels = scales::percent) +
  scale_color_brewer(palette = "Set1",
                     name = "") +
  theme_bw()
```
The cumulative returns tell the same story but the visuals present the stark contrast between the companies. Netflix is by far the best investment with a cumulative return nearing 6000 percent of the intial investment of \$1 a decade ago. Second, is Apple. But its a far second. Let me demonstrate the same graph sans Netflix to provide a sense of scale between the smaller returns.

```{r}
mult_returns %>%
  dplyr::filter(symbol != "NFLX") %>% 
  mutate(return = if_else(date == "2010-01-29", 0, return)) %>%
  group_by(symbol) %>%  # Need to group multiple stocks
  mutate(cr = cumprod(1 + return)) %>%
  mutate(cumulative_returns = cr - 1) %>%
  ggplot(aes(x = date, y = cumulative_returns, color = symbol)) +
  geom_line() +
  labs(x = "Date", y = "Cumulative Returns") +
  ggtitle("Cumulative returns for all since 2010") +
  scale_y_continuous(breaks = seq(0,20,5),
                     labels = scales::percent) +
  scale_color_brewer(palette = "Set1",
                     name = "") +
  theme_bw()
```

So, you can see here that Apple is still a great investment when comparing these other major companies. VOO is not a sexy stock. It follows a much more linear trend than the other stocks, this is because VOO is actually just a representation of the trend of the market as a whole. Thus, we can conclude that regardless of your investment strategy, the value of any investment is definitely moving up in the longterm. But to capitalize on the fluctuation of the market, a good strategy would involve investments into separate stocks that carry some level of risk. VOO doesn't compare in cumulative returns to the likes of FANG stocks. That doesn't mean a simple ETF isn't an important part of the portfolio just that it counters the higher fluctuations of the individual company stocks over time.
