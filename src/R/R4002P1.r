library("tseries")
library("zoo")
library(lubridate)
library(fGarch)
rua <- get.hist.quote(instrument="^RUA" , start="2000/01/01" , end="2021/11/13" ,quote=c("Adjusted"))
rua <- na.approx(rua)
d <- read.csv('data/R3000open1.csv', header = T)
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
r_hist <- r[1:252,]
PCA <- prcomp(r_hist)
pca1 <- PCA$rotation[,1]
p_name <- names(sort(pca1,decreasing = FALSE)[1:10])
pname_old <- p_name
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
  portfolio <- as.numeric()
  r_hist <- r[(today-253):(today-1),]
  PCA <- prcomp(r_hist)
  pca1 <- PCA$rotation[,1]
  pw <- portfolio.optim(r_hist[,order(pca1, decreasing = FALSE)[1:10]],shorts = FALSE, reslow = c(rep(0.01,10)),rf=0.025)$pw
  pw <- floor(pw*100)
  portfolio <- as.matrix(d[,order(pca1, decreasing = FALSE)[1:10]])%*%pw
  p_name <- names(sort(pca1,decreasing = FALSE)[1:10])
  print("Changed portfolio:")
  print(p_name)
  output<-list(pw,portfolio,p_name)

  return(output)
}

EWMA_var <- function(u, lambda=0.94){
  u <- as.numeric(u)
  window_size <- length(u)
  power <- 0:(window_size-1)
  weights <- (1-lambda)*lambda^power
  return (sum(weights*u^2))
}
N_obs <- 252
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
win_mu <- 50
tradedateBG<-c()
tradedateSG<-c()
tradeportG<-c()
tradedateBE<-c()
tradedateSE<-c()
tradeportE<-c()

tradeportBG<-c()

year_start <- "2001-01-04"
Trading_Period <- which(date >= year_start)
Examine_Period <- (Trading_Period[1] - 1):length(u)
plot_date <- date[seq(Trading_Period[1], length(date), 50)]


for (today in Examine_Period) {
  print(date[today])
  nu <- GARCH_var(u[(today-N_obs):(today)], dist="norm")
  goodness_garch <- c(goodness_garch, mean(u[(today-win_mu+1):(today)])/nu - 1/2)
  if (today < Trading_Period[1]){next}
  
  ref <- length(goodness_garch)
  today_portfolio <- as.numeric(portfolio[today])
  if ((nstock_garch == 0) & (goodness_garch[ref] > 0) & 
      (goodness_garch[ref-1] < 0)){
    if (cash_garch >= today_portfolio) {
      nstock_garch <- floor(cash_garch/(today_portfolio+0.013*sum(pw)))
      cash_garch <- cash_garch - nstock_garch*today_portfolio - max(2.05,0.013*sum(pw)*nstock_garch)
      print("BUY:")
      print(nstock_garch)
      print(p_name)
      print("Cost:")
      print(nstock_garch*today_portfolio)
      tradedateBG<-c(date[today],tradedateBG)
      tradeportBG<-c(p_name,tradedateBG)
    }
  }else if ((nstock_garch > 0) & (goodness_garch[ref] < 0) & 
            (goodness_garch[ref-1] > 0)){
    cash_garch <- cash_garch + nstock_garch*today_portfolio - max(2.05,0.013*sum(pw)*nstock_garch)
    
    print("SELL:")
    print(nstock_garch)
    print(p_name)
    print("Value:")
    print(nstock_garch*today_portfolio)
    nstock_garch <- 0
    output <- PCA_update(today)
    pw <- unlist(output[1])
    portfolio <- unlist(output[2])
    p_name <- unlist(output[3])
    pname_old <- append(pname_old,p_name)
    u <- diff(log(portfolio))
    tradeportG<-c(p_name,tradeportG)
    tradedateSG<-c(date[today],tradedateSG)
    
  }
  value_GARCH <- c(value_GARCH, cash_garch+nstock_garch*today_portfolio)
  
}

r_hist <- r[1:252,]
PCA <- prcomp(r_hist)
pca1 <- PCA$rotation[,1]
pw <- portfolio.optim(r_hist[,order(pca1, decreasing = FALSE)[1:10]])$pw
pw <- floor(pw*100)
portfolio <- as.matrix(d[,order(pca1, decreasing = FALSE)[1:10]])%*%pw
u <- diff(log(portfolio))


for (today in Examine_Period){
  print(date[today])
  nu <- EWMA_var(u[(today-N_obs):today], lambda=0.94)
  goodness_ewma <- c(goodness_ewma, mean(u[(today-win_mu+1):today])/nu - 1/2)
  if (today < Trading_Period[1]){next}
  
  ref <- length(goodness_ewma)
  today_portfolio <- as.numeric(portfolio[today])
  if ((nstock_ewma == 0) & (goodness_ewma[ref] > 0) & 
      (goodness_ewma[ref-1] < 0)){
    nstock_ewma <- floor(cash_ewma/(today_portfolio+0.0013*sum(pw)))
    cash_ewma <- cash_ewma - nstock_ewma*today_portfolio - max(2.05,0.013*sum(pw)*nstock_ewma)
    print("BUY:")
    print(nstock_ewma)
    print(p_name)
    print("Cost:")
    print(nstock_ewma*today_portfolio)
    tradedateBE<-c(date[today],tradedateBE)

  }else if ((nstock_ewma > 0) & (goodness_ewma[ref] < 0) & 
            (goodness_ewma[ref-1] > 0)){
    cash_ewma <- cash_ewma + nstock_ewma*today_portfolio - max(2.05,0.013*sum(pw)*nstock_ewma)
    print("SELL:")
    print(nstock_ewma)
    print(p_name)
    print("Value:")
    print(nstock_ewma*today_portfolio)
    nstock_ewma <- 0
    output <- PCA_update(today)
    pw <- unlist(output[1])
    portfolio <- unlist(output[2])
    p_name <- unlist((output[3]))
    pname_old <- append(pname_old,p_name)
    u <- diff(log(portfolio))
    tradeportE<-c(p_name,tradeportE)
    tradedateSE<-c(date[today],tradedateSE)
  }
  value_EWMA <- c(value_EWMA, cash_ewma+nstock_ewma*today_portfolio)
}
r_hist <- r[1:252,]
PCA <- prcomp(r_hist)
pca1 <- PCA$rotation[,1]
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
  rua_start <- as.numeric(rua[y_start])
  rua_end <- as.numeric(rua[y_end])
  PNL <- rbind(PNL, c(y,
                      1+(garch_end-garch_start)/garch_start,
                      1+(ewma_end-ewma_start)/ewma_start,
                      1+(bh_end-bh_start)/bh_start,
                      1+(rua_end-rua_start)/rua_start))
}
row.names(PNL)<-PNL[,1]
PNL <- PNL[,-1]
colnames(PNL) <- c("Garch", "EWMA", "BH", "RUA")

plot(x= as.numeric(row.names(PNL)), y=PNL[,1],type = 'l', main= "Profit and loss",
     ylab="Percentage change", xlab="Year",col = 'blue',ylim=c(0,3))
lines(x= as.numeric(row.names(PNL)),y=PNL[,2],col='red')
lines(x= as.numeric(row.names(PNL)),PNL[,3],col = 'green')
lines(x= as.numeric(row.names(PNL)),PNL[,4],col = 'purple')
legend(x="topleft", legend=c("GARCH", "EWMA", "BH", "RUA"), 
       col=c("blue", "red", "green", "purple"), lwd=0.001)


#Results:
finalvalueGARCH<-value_GARCH[length(value_GARCH)]
finalvalueEWMA<-value_EWMA[length(value_EWMA)]
finalvalueBH<-value_BH[length(value_BH)]
tradedateBG #Buy dates in GARCH model
tradedateSG #Sell dates in GARCH model
tradedateBE #Buy dates in EWMA model
tradedateSE #Sell dates in EWMA model
sort(table(tradeportG),decreasing=TRUE) #number of selling stocks in GARCH model
sort(table(tradeportE),decreasing=TRUE) #number of selling stocks in EWMA model

