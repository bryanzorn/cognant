#install.packages('ggplot2')
library(ggplot2)

btc <- read.table("btc.csv", sep=",", header=TRUE)
abb <- read.table("metrics_info.csv", sep=",", header=TRUE, row.names=1)

# Remove non-numeric, no variance data, and 'clean' via NA replacement with zero
btc <- btc[sapply(btc, is.numeric)]
btc <- btc[ - as.numeric(which(apply(btc, 2, var) == 0))]
btc[is.na(btc)] <- 0

# Finally, only trade-era BTC data is used and removed from the data set
btc <- subset(btc, PriceUSD > 0)

# To reduce some of the daily noise, we're aggregating on a 14 day time frame around the mean for each epoch
n <- 14
btc <- aggregate(btc, list(rep(1:(nrow(btc) %/% n + 1), each = n, len = nrow(btc))), mean)[-1]

# We don't want to compare to `PriceUSD` at any point, so we'll remove it from the data and use it independently
btcprice <- btc$PriceUSD
btc <- subset(btc, select = - PriceUSD)

# Create `name` to use as a length variable as well as `results` to store data as it's generated
name <- colnames(btc)
results <- c()

# This loop will compare `PriceUSD` against all 'suriving' metrics post clean-up
for (i in seq(1,length(name))){

    # Try this with and without log price
    ccfobj <- ccf(btcprice, btc[i], type='correlation', plot = FALSE)
    corcoef <- ccfobj$acf
    laglist <- ccfobj$lag

    # By using squares, we're able to determine which direction the highest/lowest correlation `ce` is going
    ce <- if ( max(corcoef)^2 > min(corcoef)^2 ) {
        max(corcoef)
        } else { 
        min(corcoef)
        }

    # There's probably a much more elegant way to `match` coefficient values and lag value, but in the interest of time...
    matchlist <- cbind(laglist, corcoef)

    agg <- c(subset(matchlist, laglist == 0)[2],
            subset(matchlist, corcoef == ce)[2],
            round(subset(matchlist, corcoef == ce)[1])
            )

    # Results stores the aggregated values of unshifted correlation coefficient, best `ce`, lag as shift for each comparison
    results <- rbind(results, agg)
}

rownames(results) <- name
# Add category from `metrics_info.csv`
results <- cbind(results, abb[name,][2])
colnames(results) <- c('Coefficient', 'Best', 'Shift','Category')

ggplot(results, aes(x = Category, y = Best, fill = Category)) +
    geom_boxplot() +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90)) + ylab("Best coefficient")
    
# Let's take the top five best fitting coeffcients from each category to whittle down a beter perspective
num = 5
toptable <- results[order(results$Best, decreasing = TRUE), ]
toptable <- Reduce(rbind,
            by(toptable,
            toptable["Category"],
            head,
            n = num))

# Arrange the table to make a little more human-sense
toptable <- cbind(toptable$Category, abb[rownames(toptable),][3], abb[rownames(toptable),][1], toptable$Best)
colnames(toptable) <- c('Category', 'Subcategory', 'Fullname', 'Coefficient')
toptable

# Filter
topnew <- toptable[!grepl("USD|\\$", toptable$Fullname),]

# Add lag for interpretation
topnew$Lag = results$Shift[match(rownames(topnew),rownames(results))]

# Sort by Coefficient
topnew[order(-topnew$Coefficient),]

library(forecast)

# Read `btc.csv`, create a matrix of TxCnt by year (assume 365 days) with NA
btctx <- read.table("btc.csv", sep=",", header=TRUE)$TxCnt
ncol <- round(length(btctx)/365)
add <- (365 * ncol) - length(btctx)
btcmat <- matrix(append(btctx,rep(NA,add)),365,ncol)

yearlytx <- as.matrix(colSums(btcmat, na.rm = TRUE))

# We're dropping the current year, as we're only half way through it, and forecasting 3 years out
future <- forecast(yearlytx[1:12],h=4)
autoplot(future) +
    geom_smooth() +
    geom_point(aes(x=ncol, y=yearlytx[13]),
        size=5,
        shape=19,
        alpha=1/2) +
        theme_bw() + xlab("Year") + ylab("Average transaction count") + ggtitle("")
        
# (Current day ratio) * Forecast model mean for 2021
predtoday <- 195/365*future$mean[1]

round(predtoday/yearlytx[13],2)

# Let's sort by highest lag, pick and choose some metrics with relatively high coefficients
subset(topnew[order(-topnew$Lag),], Coefficient > 0.5 & Lag > 1)

# While TxCnt was the example metric and has been empirically validated as potentially
# the best to use, we'll go with TxTfrCnt instead

# Reload everything, but aggregate to a 30 day time frame to lessen the aggressiveness of the ups and downs

btc <- read.table("btc.csv", sep=",", header=TRUE)

btc <- btc[sapply(btc, is.numeric)]
btc <- btc[ - as.numeric(which(apply(btc, 2, var) == 0))]
btc[is.na(btc)] <- 0

# Going to make this greater than $1 because we're applying ln
btc <- subset(btc, PriceUSD > 1)

n <- 30
btc <- aggregate(btc, list(rep(1:(nrow(btc) %/% n + 1), each = n, len = nrow(btc))), mean)[-1]

btcprice <- btc$PriceUSD
txtfrcnt <- btc$TxTfrCnt

scaleFactor <- max(log(btcprice)) / max(txtfrcnt)

ggplot() +
    geom_line(aes(x = seq(1:length(btcprice)), y = (log(btcprice)), color = "red", alpha = 1/5), size = 1) +
    geom_line(aes(x = seq(1:length(btcprice)), y = (txtfrcnt * scaleFactor)), color = "black", alpha = 1/5, size = 1) +
        scale_y_continuous(name="log(Bitcoin Price)", sec.axis=sec_axis(~./scaleFactor, name="TxTfrCnt")) +
    theme_bw() + xlab("Month") + ylab("log(Bitcoin Price)") + ggtitle("") + theme(legend.position = "none")
