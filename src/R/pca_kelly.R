library(tseries)
library(zoo)
library(lubridate)
library(fGarch)

rua <- get.hist.quote(instrument = "^RUA", start = "2000/01/01",
  end = "2021/11/12", quote = c("Adjusted"))
rua <- na.approx(rua)
# d <- read.csv('./data/r3000_20yrs.csv', header = T)
d = read.csv("data/R3000 20yrs raw.csv", header = T)
row.names(d) <- d$X
date <- as.Date(row.names(d), format="%m/%d/%Y")
d <- d[,-1]
d <- Filter(function(x)!any(is.na(x)), d)
r <- diff(log(d[,1]))
for (i in 2:length(d)){
  r <- cbind(r,diff(log(d[,i])))
}
colnames(r) <- names(d)
row.names(r) <- row.names(d)[-1]
d <- d[-1,]
date <- date[-1]
r_hist <- r[1:750,]
PCA <- princomp(r_hist)
pca1 <- PCA$loadings[,1]
weight <- abs(pca1)/sum(abs(pca1))
pw <- portfolio.optim(r_hist[,order(pca1, decreasing = FALSE)[1:10]])$pw
pw <- floor(pw*100)
portfolio <- as.matrix(d[,order(pca1, decreasing = FALSE)[1:10]])%*%pw
GARCH_var <- function(u, dist=c("norm", "std")){
  u <- as.numeric(u)
  fG <- garchFit(~garch(1, 1), data=u, include.mean=FALSE, 
                 cond.dist=c(dist), trace=FALSE)
  return(tail(fG@h.t, 1))
}
PCA_update <- function(today) {
  r_hist <- r[(today-751):today-1,]
  PCA <- princomp(r_hist)
  pca1 <- PCA$loadings[,1]
  pw <- portfolio.optim(r_hist[,order(pca1, decreasing = FALSE)[1:10]])$pw
  pw <- floor(pw*100)
  portfolio <- as.matrix(d[,order(pca1, decreasing = FALSE)[1:10]])%*%pw
  output<-list(pw,portfolio)
  print("Changed portfolio:")
  print(names(sort(pca1,decreasing = FALSE)[1:10]))
  return(output)
}
EWMA_var <- function(u, lambda=0.94){
  u <- as.numeric(u)
  window_size <- length(u)
  power <- 0:(window_size-1)
  weights <- (1-lambda)*lambda^power
  return (sum(weights*u^2))
}
N_obs <- 500
cash0 <- 10000
nstock_garch <- 0
nstock_ewma <- 0
cash_garch <- cash0
cash_ewma <- cash0
goodness_garch <- as.numeric()
goodness_ewma <- as.numeric()
value_GARCH <- as.numeric()
value_EWMA <- as.numeric()
value_BH <- as.numeric()
u <- diff(log(portfolio))
win_mu <- 9

year_start <- "2002-12-31"
Trading_Period <- which(date >= year_start)
Examine_Period <- (Trading_Period[1] - 1):length(u)
plot_date <- date[seq(Trading_Period[1], length(date), 50)]

for (today in Examine_Period) {
  print(date[today])
  nu <- GARCH_var(u[(today-N_obs):today], dist="norm")
  goodness_garch <- c(goodness_garch, mean(u[(today-win_mu+1):today])/nu - 1/2)
  if (today < Trading_Period[1]){next}
  
  ref <- length(goodness_garch)
  today_portfolio <- as.numeric(portfolio[today])
  if ((nstock_garch == 0) & (goodness_garch[ref] > 0) & 
      (goodness_garch[ref-1] < 0)){
    nstock_garch <- floor(cash_garch/today_portfolio)
    cash_garch <- cash_garch - nstock_garch*today_portfolio - max(2.05,0.013*sum(pw)*nstock_garch)
  }else if ((nstock_garch > 0) & (goodness_garch[ref] < 0) & 
            (goodness_garch[ref-1] > 0)){
    cash_garch <- cash_garch + nstock_garch*today_portfolio - max(2.05,0.013*sum(pw)*nstock_garch)
    nstock_garch <- 0
    output <- PCA_update(today)
    pw <- unlist(output[1])
    portfolio <- unlist(output[2])
    u <- diff(log(portfolio))
  }
  value_GARCH <- c(value_GARCH, cash_garch+nstock_garch*today_portfolio)
  
}

r_hist <- r[1:750,]
PCA <- princomp(r_hist)
pca1 <- PCA$loadings[,1]
pw <- portfolio.optim(r_hist[,order(pca1, decreasing = FALSE)[1:10]])$pw
pw <- floor(pw*100)
portfolio <- as.matrix(d[,order(pca1, decreasing = FALSE)[1:10]])%*%pw
u <- diff(log(portfolio))

for (today in Examine_Period){
  print(date[today])
  nu <- EWMA_var(u[(today-N_obs):(today-1)], lambda=0.91)
  goodness_ewma <- c(goodness_ewma, mean(u[(today-win_mu+1):today])/nu - 1/2)
  if (today < Trading_Period[1]){next}
  
  ref <- length(goodness_ewma)
  today_portfolio <- as.numeric(portfolio[today])
  if ((nstock_ewma == 0) & (goodness_ewma[ref] > 0) & 
      (goodness_ewma[ref-1] < 0)){
    nstock_ewma <- floor(cash_ewma/today_portfolio)
    cash_ewma <- cash_ewma - nstock_ewma*today_portfolio - max(2.05,0.013*sum(pw)*nstock_ewma)
  }else if ((nstock_ewma > 0) & (goodness_ewma[ref] < 0) & 
            (goodness_ewma[ref-1] > 0)){
    cash_ewma <- cash_ewma + nstock_ewma*today_portfolio - max(2.05,0.013*sum(pw)*nstock_ewma)
    nstock_ewma <- 0
    output <- PCA_update(today)
    pw <- unlist(output[1])
    portfolio <- unlist(output[2])
    u <- diff(log(portfolio))
  }
  value_EWMA <- c(value_EWMA, cash_ewma+nstock_ewma*today_portfolio)
}
r_hist <- r[1:750,]
PCA <- princomp(r_hist)
pca1 <- PCA$loadings[,1]
pw <- portfolio.optim(r_hist[,order(pca1, decreasing = FALSE)[1:10]])$pw
pw <- floor(pw*100)
portfolio <- as.matrix(d[,order(pca1, decreasing = FALSE)[1:10]])%*%pw
u <- diff(log(portfolio))
nstock <- floor(cash0/head(portfolio[Trading_Period], 1))
for (today in Trading_Period){
  today_portfolio <- as.numeric(portfolio[today])
  value_BH <- c(value_BH, nstock*today_portfolio)
}


value_GARCH <- zoo(value_GARCH)
index(value_GARCH) <- date[date > year_start]

value_EWMA <- zoo(value_EWMA)
index(value_EWMA) <- date[date > year_start]

value_BH <- zoo(value_BH)
index(value_BH) <- date[date >= year_start]

max_val <- max(value_GARCH, value_EWMA, value_BH)

plot(value_GARCH, type = 'l', main="Portfolio value", ylab="value", 
     xlab="Date", xaxt="n", col="blue", lwd=2, ylim=c(0, max_val))
lines(value_EWMA, col="red", lwd=2)
lines(value_BH, col="green", lwd=2)
axis(side=1, plot_date, format(plot_date, "%d-%m-%y"), 
     cex.axis=0.7, cex.axis=1)
legend(x="topleft", legend=c("GARCH", "EWMA", "BH"), 
       col=c("blue", "red", "green"), lwd=2)

PNL <- as.numeric()

for (y in unique(year(date[Trading_Period[-1]]))){
  y_start <- min(index(value_GARCH)[year(index(value_GARCH))==y])
  y_end <- max(index(value_GARCH)[year(index(value_GARCH))==y])
  garch_start <- as.numeric(value_GARCH[y_start])
  garch_end <- as.numeric(value_GARCH[y_end])
  ewma_start <- as.numeric(value_EWMA[y_start])
  ewma_end <- as.numeric(value_EWMA[y_end])
  bh_start <- as.numeric(value_BH[y_start])
  bh_end <- as.numeric(value_BH[y_end])
  PNL <- rbind(PNL, c(y,
                      1+(garch_end-garch_start)/garch_start,
                      1+(ewma_end-ewma_start)/ewma_start,
                      1+(bh_end-bh_start)/bh_start))
}
row.names(PNL)<-PNL[,1]
PNL <- PNL[,-1]
colnames(PNL) <- c("Garch", "EWMA", "BH")

plot(PNL[,1],type = 'l', main= "Profit and loss", ylab="Percentage change"
     , xlab="Year",col = 'blue',ylim=c(0,2) )
lines(PNL[,2],col='red')
lines(PNL[,3],col = 'green')
legend(x="topleft", legend=c("GARCH", "EWMA", "BH"), 
       col=c("blue", "red", "green"), lwd=0.001)

