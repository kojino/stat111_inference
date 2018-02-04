setwd('/Users/kojin/psets/stat111/hw1')
# 1
df <- read.table("https://www.dartmouth.edu/~chance/teaching_aids/data/golf.txt", header=TRUE)
df
hist(df$StockRate)
summary(df$StockRate)

# 2
df <- read.csv("Births/births.csv")
hist(df$time,xlab="Time until Birth", 
     freq=FALSE,main="Histogram of Time until Birth")

# 3
df <- read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/auto-mpg/auto-mpg.data")
colnames <- c('mpg','cylinders','displacement','horsepower','weight','acceleration','model year','origin','car name')
colnames(df) <- colnames

calc_ybar <- function(x,bandwidth) {
  # select elements within the bin
  bin_df <- df[(df$weight > x - bandwidth / 2) 
               & (df$weight < x + bandwidth / 2),]
  # take the mean mpg value of each bin
  ybar <- mean(bin_df$mpg)
  ybar
}

plot_ybar <- function(bandwidth) {
  # select center points of bins
  xs <- seq(min(df$weight),max(df$weight),bandwidth)
  # calculate ybar for each bin
  ybars <- sapply(xs,calc_ybar,bandwidth=bandwidth)
  plot(ybars)
}

plot_ybar(60)
plot_ybar(160)

# 4
df <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data",header=FALSE)
colnames <- c('age','workclass','fnlwgt','education','education-num','marital-status','occupation','relationship','race','sex','capital-gain','capital-loss','hours-per-week','native-country')
colnames(df) <- colnames
df
# 5
