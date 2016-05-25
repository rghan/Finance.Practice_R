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