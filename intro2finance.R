#------------------------------------------------------------------------------
## intro2finance.R - practice looking at finance using data.table and ggplot2 
##
## Script is based on a recent data shenanigan blog.
## https://datashenanigan.wordpress.com/2016/05/24/a-gentle-introduction-to-finance-using-r-efficient-frontier-and-capm-part-1/
#------------------------------------------------------------------------------

library(data.table)
library(scales)
library(ggplot2)
 
link <- "https://raw.githubusercontent.com/DavZim/Efficient_Frontier/master/data/fin_data.csv"
dt <- data.table(read.csv(link))
dt[, date := as.Date(date)]
 
# create indexed values
dt[, idx_price := price/price[1], by = ticker]
 
# plot the indexed values
ggplot(dt, aes(x = date, y = idx_price, color = ticker)) +
  geom_line() +
	# Miscellaneous Formatting
	theme_bw() + 
	labs(ggtitle = "Price Developments", 
		 xlab = "Date",
		 ylab = "Price\n(Indexed 2000 = 1") +
	scale_color_discrete(name = "Company")

#------------------------------------------------------------------------------
# calculate the arithmetic returns
dt[, ret := price / shift(price, 1) - 1, by = ticker]
 
# summary table
# take only non-na values
tab <- dt[!is.na(ret), .(ticker, ret)]
 
# calculate the expected returns (historical mean of returns) and volatility (standard deviation of returns)
tab <- tab[, .(er = round(mean(ret), 4),
	sd = round(sd(ret), 4)),
	by = "ticker"]


ggplot(tab, aes(x = sd, y = er, color = ticker)) +
geom_point(size = 5) +
# Miscellaneous Formatting
theme_bw() + ggtitle("Risk-Return Tradeoff") +
xlab("Volatility") + ylab("Expected Returns") +
scale_y_continuous(label = percent, limits = c(0, 0.03)) +
scale_x_continuous(label = percent, limits = c(0, 0.1))

# load the data
link <- "https://raw.githubusercontent.com/DavZim/Efficient_Frontier/master/data/mult_assets.csv"
df <- data.table(read.csv(link))
 
# calculate the necessary values:
# I) expected returns for the two assets
er_x <- mean(df$x)
er_y <- mean(df$y)
 
# II) risk (standard deviation) as a risk measure
sd_x <- sd(df$x)
sd_y <- sd(df$y)
 
# III) covariance
cov_xy <- cov(df$x, df$y)
 
# create 1000 portfolio weights (omegas)
x_weights <- seq(from = 0, to = 1, length.out = 1000)
 
# create a data.table that contains the weights for the two assets
two_assets <- data.table(wx = x_weights,
wy = 1 - x_weights)
 
# calculate the expected returns and standard deviations for the 1000 possible portfolios
two_assets[, ':=' (er_p = wx * er_x + wy * er_y,
	sd_p = sqrt(wx^2 * sd_x^2 +
	wy^2 * sd_y^2 +
	2 * wx * (1 - wx) * cov_xy))]
two_assets

# lastly plot the values
ggplot() +
geom_point(data = two_assets, aes(x = sd_p, y = er_p, color = wx)) +
geom_point(data = data.table(sd = c(sd_x, sd_y), mean = c(er_x, er_y)),
aes(x = sd, y = mean), color = "red", size = 3, shape = 18) +
# Miscellaneous Formatting
theme_bw() + ggtitle("Possible Portfolios with Two Risky Assets") +
xlab("Volatility") + ylab("Expected Returns") +
scale_y_continuous(label = percent, limits = c(0, max(two_assets$er_p) * 1.2)) +
scale_x_continuous(label = percent, limits = c(0, max(two_assets$sd_p) * 1.2)) +
scale_color_continuous(name = expression(omega[x]), labels = percent)

#------------------------------------------------------------------------------
## adding a third asset
#------------------------------------------------------------------------------
# load the data
link <- "https://raw.githubusercontent.com/DavZim/Efficient_Frontier/master/data/mult_assets.csv"
df <- data.table(read.csv(link))
 
# calculate the necessary values:
# I) expected returns for the two assets
er_x <- mean(df$x)
er_y <- mean(df$y)
er_z <- mean(df$z)
 
# II) risk (standard deviation) as a risk measure
sd_x <- sd(df$x)
sd_y <- sd(df$y)
sd_z <- sd(df$z)
 
# III) covariance
cov_xy <- cov(df$x, df$y)
cov_xz <- cov(df$x, df$z)
cov_yz <- cov(df$y, df$z)
 
# create portfolio weights (omegas)
x_weights <- seq(from = 0, to = 1, length.out = 1000)
 
# create a data.table that contains the weights for the three assets
three_assets <- data.table(wx = rep(x_weights, each = length(x_weights)),
	wy = rep(x_weights, length(x_weights)))
 
three_assets[, wz := 1 - wx - wy]
 
 
# calculate the expected returns and standard deviations for the 1000 possible portfolios
three_assets[, ':=' (er_p = wx * er_x + wy * er_y + wz * er_z,
	sd_p = sqrt(wx^2 * sd_x^2 +
	wy^2 * sd_y^2 +
	wz^2 * sd_z^2 +
	2 * wx * wy * cov_xy +
	2 * wx * wz * cov_xz +
	2 * wy * wz * cov_yz))]
 
# take out cases where we have negative weights (shortselling)
three_assets <- three_assets[wx >= 0 & wy >= 0 & wz >= 0]
three_assets

# lastly plot the values
ggplot() +
geom_point(data = three_assets, aes(x = sd_p, y = er_p, color = wx - wz)) +
geom_point(data = data.table(sd = c(sd_x, sd_y, sd_z), mean = c(er_x, er_y, er_z)),
aes(x = sd, y = mean), color = "red", size = 3, shape = 18) +
# Miscellaneous Formatting
theme_bw() + ggtitle("Possible Portfolios with Three Risky Assets") +
xlab("Volatility") + ylab("Expected Returns") +
scale_y_continuous(label = percent, limits = c(0, max(three_assets$er_p) * 1.2)) +
scale_x_continuous(label = percent, limits = c(0, max(three_assets$sd_p) * 1.2)) +
scale_color_gradientn(colors = c("red", "blue", "yellow"),
name = expression(omega[x] - omega[z]), labels = percent)

#------------------------------------------------------------------------------
calcEFParams <- function (rets) {
	retbar <- colMeans(rets, na.rm = T)
	covs <- var(rets, na.rm = T) # calculates the covariance of the returns
	invS <- solve(covs)
	i <- matrix(1, nrow = length(retbar))
 
	alpha <- t(i) %*% invS %*% i
	beta <- t(i) %*% invS %*% retbar
	gamma <- t(retbar) %*% invS %*% retbar
	delta <- alpha * gamma - beta * beta
 
	retlist <- list(alpha = as.numeric(alpha),
	beta = as.numeric(beta),
	gamma = as.numeric(gamma),
	delta = as.numeric(delta))
	
	return(retlist)
}
 
# load data
link <- "https://raw.githubusercontent.com/DavZim/Efficient_Frontier/master/data/mult_assets.csv"
df <- data.table(read.csv(link))
 
abcds <- calcEFParams(df)
abcds

#------------------------------------------------------------------------------
calcEFValues <- function (x, abcd, upper = T) {
	alpha <- abcd$alpha
	beta <- abcd$beta
	gamma <- abcd$gamma
	delta <- abcd$delta
 
	if (upper) {
		retval <- beta / alpha + sqrt((beta / alpha) ^ 2 - (gamma - delta * x ^ 2) / (alpha))
	} else {
		retval <- beta / alpha - sqrt((beta / alpha) ^ 2 - (gamma - delta * x ^ 2) / (alpha))
	}
 
	return(retval)
}
#--------------------------------------
## this gives an error
#-------------------------------------- 
# calculate the risk-return tradeoff the two assets (for plotting the points)
df_table <- melt(df)[, .(er = mean(value),
	sd = sd(value)), by = names]
#--------------------------------------
#--------------------------------------
 
# plot the values
ggplot(df_table, aes(x = sd, y = er)) +
# add the stocks
geom_point(size = 4, color = "red", shape = 18) +
# add the upper efficient frontier
stat_function(fun = calcEFValues, args = list(abcd = abcds, upper = T), n = 10000,
color = "red", size = 1) +
# add the lower "efficient" frontier
stat_function(fun = calcEFValues, args = list(abcd = abcds, upper = F), n = 10000,
color = "blue", size = 1) +
# Miscellaneous Formatting
theme_bw() + ggtitle("Efficient Frontier with Short-Selling") +
xlab("Volatility") + ylab("Expected Returns") +
scale_y_continuous(label = percent, limits = c(0, max(df_table$er) * 1.2)) +
scale_x_continuous(label = percent, limits = c(0, max(df_table$sd) * 1.2))

#------------------------------------------------------------------------------
## without Short-Selling
#------------------------------------------------------------------------------
library(tseries)
##
##     'tseries' version: 0.10-34
##
##     'tseries' is a package for time series analysis and
##     computational finance.
##
##     See 'library(help="tseries")' for details.

link <- "https://raw.githubusercontent.com/DavZim/Efficient_Frontier/master/data/mult_assets.csv"
df <- data.table(read.csv(link))
 
df_table <- melt(df)[, .(er = mean(value),
sd = sd(value)), by = variable]
 
er_vals <- seq(from = min(df_table$er), to = max(df_table$er), length.out = 1000)
 
# find an optimal portfolio for each possible possible expected return
# (note that the values are explicitly set between the minimum and maximum of the expected returns per asset)
sd_vals <- sapply(er_vals, function(er) {
op <- portfolio.optim(as.matrix(df), er)
return(op$ps)
})
 
plot_dt <- data.table(sd = sd_vals, er = er_vals)
 
# find the lower and the upper frontier
minsd <- min(plot_dt$sd)
minsd_er <- plot_dt[sd == minsd, er]
plot_dt[, efficient := er >= minsd_er]
plot_dt