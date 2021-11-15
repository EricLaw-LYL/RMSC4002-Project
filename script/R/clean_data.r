# Import Packages ---------------------------------------------------------
library(tidyverse) 
# warning: dplyr::filter() masks stats::filter()
# warning: dplyr::lag()    masks stats::lag()
library(stringr)



# Read Data ---------------------------------------------------------------
# read csv
r3000 = read.csv("data/r3000.csv")

# change date column to date format
Date = r3000$Date |> as.Date("%d %m %Y")
row.names(r3000) = Date

# sort the columns by stock names
r3000.stocks = r3000[, 2:ncol(r3000)]
r3000 = cbind(Date, r3000.stocks[, order(names(r3000.stocks))])

# change the stocks' name
stocks = subset(r3000, select = -c(Date)) |> colnames()
stocks.ticker = sapply(stocks, function(x) 
  str_match(x, "([A-Z\\.]+)\\.[A-Z]+\\.E")[, 2])
colnames(r3000) = c("Date", stocks.ticker)

# change all columns to dbl
r3000 = r3000 %>%
  mutate_each(funs(if(is.character(.)) as.numeric(.) else .))
# missing data will save as NA

# remove data after 2021-11-12
r3000 = r3000[Date <= "2021-11-12", ]

# save clean data
write.csv(r3000, "data/r3000_clean.csv", append = T, 
          row.names = T, col.names = T)
save(r3000, file = "data/r3000_clean.rda", append = T)

# read rda file
load("data/r3000_clean.rda")
