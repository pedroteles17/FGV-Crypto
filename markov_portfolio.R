library(tidyverse)
library(lubridate)
library(xts)
library(PerformanceAnalytics)
library(plotly)
library(crypto2) # Crypto data
library(depmixS4) # Hidden Markov Chain
library(rbcb) # CDI
library(frenchdata) # T-bill


##################################################################
##                       HMM and Potfolio                       ##
##################################################################

# Get NLP results
reddit <- read.csv('reddit_sent_analysis.csv')

# Reddit Analysis
reddit <- reddit %>% 
  dplyr::select(id, time, num_upvotes, sent_neg, sent_pos, sent_neu, sent_compound) %>% 
  mutate(weghted_sent = sent_compound * num_upvotes,
         time = as_datetime(time)) %>% 
  mutate(week = cut.Date(as.Date(time), breaks = "1 week", labels = FALSE)) %>%
  arrange(time) %>% 
  group_by(week) %>% 
  summarise(Date = tail(time, 1), Sentiment = mean(weghted_sent)) %>% 
  mutate(Date = format(as.Date(Date), "%Y-%m-%d")) %>% 
  dplyr::select(!week)

# Get Bitcoin price
all_coins <- crypto2::crypto_history(start_date = '20210101', limit = 5)

btc <- all_coins %>% 
  dplyr::filter(symbol == 'BTC') %>% 
  dplyr::select(timestamp, close) %>% 
  set_names(c('Date', 'BTC')) %>% 
  mutate(Date = format(as.Date(Date), "%Y-%m-%d"))

# Merge the two datasets
dados <- merge(reddit, btc, by = 'Date', all.x = TRUE)

dados <- dados %>% 
  mutate(Date = as.Date(Date),
         Sentiment = as.numeric(Sentiment),
         BTC = as.numeric(BTC)) %>% 
  mutate(across(!Date, ~append(NA, diff(.x)/.x[-length(.x)]))) %>% 
  slice(-1)

dados <- xts(dados[,-1], dados$Date)

# Markov Chain
inicio <- 30
prob_atual_series <- list()
prob_state_one <- vector()
exp_ret <- vector()
ratio_buy <- vector()
for(i in 1:(nrow(dados)-inicio)){
  dados_loop <- dados[1:(inicio - 1 + i), , drop = FALSE]
  
  mod4 <- depmix(list(dados_loop[, 2] ~ 1, dados_loop[, 1] ~ 1), family = list(gaussian(), gaussian()), nstates = 2, data = dados_loop)
  
  set.seed(1)
  fm4 <- fit(mod4, verbose = FALSE)
  
  selm <- fm4
  
  tsp <- as.ts(dados_loop)
  
  matriz_trans <- t(sapply(selm@transition,function(ob)ob@parameters$coefficients))
  
  prob_atual <- as.numeric(posterior(selm, type = "smoothing")[nrow(dados_loop),])
  
  ret_estado <- summary(selm, which = "response")[,1]
  
  prob_state_one[i] <- (prob_atual %*% matriz_trans)[1,1]
  exp_ret[i] <- sum(ret_estado * (prob_atual %*% matriz_trans))
  names(exp_ret)[i] <- as.character(index(dados[(inicio + i), , drop = FALSE]))
  
  prob_atual_series[[i]] <- data.frame(Date = as.character(index(dados[(inicio + i - 1), , drop = FALSE])),
                                       state1 = prob_atual[1], state2 = prob_atual[2])

  ratio_buy[i] <- sum(exp_ret[i] > dados_loop[, 2])/nrow(dados_loop)
  names(ratio_buy)[i] <- as.character(index(dados[(inicio + i), , drop = FALSE]))
}

# CDI (interbank rate) data from BCB
cdi <- get_series(list(CDI = 12), start_date = '2021-01-01') 

cdi <- cdi %>% 
  mutate(week = cut.Date(as.Date(date), breaks = "1 week", labels = FALSE)) %>%
  arrange(date) %>% 
  group_by(week) %>% 
  summarise(Date = tail(date, 1), CDI = prod(1 + CDI / 100) - 1) %>% 
  mutate(Date = format(as.Date(Date), "%Y-%m-%d")) %>%
  dplyr::select(!week) %>% 
  dplyr::filter(Date >= '2021-01-10' & Date <= '2022-10-20')

# T-Bill data
t_bill <- download_french_data('Fama/French 3 Factors [Daily]')

t_bill <- t_bill[['subsets']][['data']][[1]]

t_bill <- t_bill %>% 
  mutate(date = ymd(date)) %>% 
  mutate(week = cut.Date(as.Date(date), breaks = "1 week", labels = FALSE)) %>%
  arrange(date) %>% 
  group_by(week) %>% 
  summarise(Date = tail(date, 1), RF = prod(1 + RF / 100) - 1) %>% 
  dplyr::filter(Date >= '2021-01-10' & Date <= '2022-08-31') %>% 
  dplyr::select(!week)

# Get all data togheter
dados$CDI <- c(cdi$CDI, cdi$CDI[length(cdi$CDI)])
dados$T_bill <- c(t_bill$RF, rep(t_bill$RF[length(t_bill$RF)], 8))
dados$zero_ret <- 0

# Markov information
markov_info <- xts(data.frame(Prob_state_one = prob_state_one,
                          Exp_ret = exp_ret,
                          Ratio_buy = ratio_buy), as.Date(names(exp_ret)))

# Merge the two datasets
dados <- merge.xts(dados, markov_info, join = 'right')

# Get Portfolios
weights <- dados$Prob_state_one
weights$Prob_state_two <- 1 - dados$Prob_state_one

## BTC - CDI
port_btc_cdi <- Return.portfolio(dados[, c('BTC', 'CDI')], weights = weights)

## BTC - T-bill
port_btc_tbill <- Return.portfolio(dados[, c('BTC', 'T_bill')], weights = weights)

## BTC - Zero Return
port_btc_zero_ret <- Return.portfolio(dados[, c('BTC', 'zero_ret')], weights = weights)

# All backtests
backtests <- dados$BTC[-1]
backtests$T_bill <- dados$T_bill[-1]
backtests$btc_cdi <- port_btc_cdi
backtests$btc_tbill <- port_btc_tbill
backtests$btc_zero <- port_btc_zero_ret

#################################################################
##                           Results                           ##
#################################################################

# Markov Plot
prob_atual <- do.call('rbind', prob_atual_series)

prob_atual <-  prob_atual %>% 
  mutate(Date = as.Date(Date),
         State = ifelse(state1 > state2, 'Bullish', 'Bearish')) %>% 
  dplyr::select(Date, State)

dados_df <- data.frame(Date = index(dados), dados) %>% 
  dplyr::select(Date, BTC) %>% 
  inner_join(prob_atual, by = 'Date')

ggplot(dados_df) +
  geom_line(aes(x = Date, y = BTC)) +
  geom_point(aes(x = Date, y = BTC, color = State)) + 
  geom_abline(slope = 0, intercept = 0, color = 'grey') + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_classic() + xlab('') +
  labs(color='Hidden State') +
  ggtitle('Bitcoin Return and HMM Hidden States')


# Plot Portfolio Return and Drawdown
PerformanceAnalytics::charts.PerformanceSummary(backtests[,c(1,3,4,5)], 
                                                plot.engine = 'plotly', 
                                                main = 'Cumulative Returns')

# Portfolio Return Statistics
port_stats <- function(port_xts, risk_free_xts){
  annual_ret <- prod(1 + port_xts) ^ (252 / length(port_xts)) - 1
  
  annual_vol <- sd(port_xts) * sqrt(252)
  
  # Israelsen Sharpe Ratio
  ER <- prod(1 + port_xts - risk_free_xts) ^ (252 / length(port_xts)) - 1
  SD <- sd(port_xts) * sqrt(252)
  msr <- ER / (SD^(ER / abs(ER)))
  
  max_drawdown <- maxDrawdown(port_xts)
  
  cvar <- CVaR(port_xts)
  
  skew <- skewness(port_xts)
  
  kurt <- kurtosis(port_xts)
  
  ret_stat <- data.frame(c(annual_ret, annual_vol, msr,
                         max_drawdown, cvar, skew, kurt))
  
  return(ret_stat)
}

ret_stat <- apply(backtests, 2, function(x) port_stats(x, backtests$T_bill))

ret_stat <- do.call('cbind', ret_stat)

colnames(ret_stat) <- c('BTC', 'T-bill', 'BTC-CDI', 'BTC-TBill', 'BTC-Cash')

rownames(ret_stat) <- c('Annualized Return', 'Annualized Volatility', 'Modified Sharpe Ratio',
                        'Max. Drawdown', 'CVaR', 'Skewness', 'Kurtosis')


# Pie plot positive, neutal, negative
# Get NLP results
reddit <- read.csv('reddit_sent_analysis.csv')

sentiment_posts <- reddit %>% 
  mutate(Sentiment = ifelse(sent_compound > 0.3, 'Positive', ifelse(sent_compound < -0.3, 'Negative', 'Neutral')))

sentiment_posts <- as.data.frame(table(sentiment_posts$Sentiment))
colnames(sentiment_posts) <- c('Sentiment', 'Frequency')

fig <- plot_ly(sentiment_posts, x = ~Sentiment, y = ~Frequency, type = 'bar', name = 'Sentiment Frequency')
fig <- fig %>% layout(title = 'Posts Sentiment Frequency')

fig
