# Ebeid ElSayed 
# Ebeid@gatech.edu
# GTID_903060599
# -------------------------------------------------------------------------------------------------------------------
library(ggplot2)
data(midwest)

# Q 1 - Professional Employment by State
states <- unique(midwest$state)  # [1] "IL" "IN" "MI" "OH" "WI"
boxplot(percprof~state, midwest, main="Professional Employment by State", xlab="State", 
        ylab="Percentage of people that have a professional employment", xaxt="n")

axis(side=2, labels = seq(1:20), at = seq(1:20))
axis(side=1, labels = states, at = seq(1:5))

# Q 2 - School and College Education by State
ggplot(midwest, aes(x = perchsd, y = percollege), 
      xlab = "Percentage of people with a High School diploma",
      ylab = "Percentage of people with a college degree.",
      main = "School and College Education by State") +
  geom_point(shape=1) +
  facet_grid(. ~ state)

# Q4 - Random Scatterplots

mainDir <- getwd()
dir.create(file.path(mainDir, "GTID_903060599"), showWarnings = FALSE)
setwd(file.path(mainDir, "GTID_903060599"))
results <- data.frame()

matrixSizes = c(1:10 %o% 10^(1:4))
fileExtensions = c("ps", "pdf", "jpeg", "png")
for(matrixSize in matrixSizes)
{
  df = data.frame(matrix(runif(2*matrixSize), nrow = matrixSize, ncol = 2))
  names(df) = c("X", "Y")
  chart = ggplot(df, aes(X, Y)) + geom_point()
  tmp = c()
  for(fileExtension in fileExtensions)
  {
    fileName = paste(matrixSize, ".", fileExtension, sep = "")
    ggsave(file=fileName)
    tmp <- c(tmp, as.numeric(file.info(fileName)$size))
  }
  tmp <- c(matrixSize, tmp)
  results <- rbind(results, data.frame(matrix(tmp, ncol = length(fileExtensions)+1, byrow = TRUE)))
}                         
names(results) = c("n",fileExtensions)

unlink(file.path(mainDir, "GTID_903060599"), recursive = TRUE)
setwd(mainDir)

ggplot(results, aes(n)) +
  geom_point(data=results, aes(y=ps, color="ps")) +
  geom_line(data=results, aes(y=ps, color="ps")) +
  geom_point(data=results, aes(y=pdf, color="pdf")) +
  geom_line(data=results, aes(y=pdf, color="pdf")) +
  geom_point(data=results, aes(y=jpeg, color="jpeg")) +
  geom_line(data=results, aes(y=jpeg, color="jpeg")) +
  geom_point(data=results, aes(y=png, color="png")) +
  geom_line(data=results, aes(y=png, color="png")) +
  labs(title="Number of scatter plot total number of points vs. Saved plot file size",
       x="Number of random points",
       y="File size in bytes")

# Q5 - Diamonds, larger dataset
require(ggplot2)
data(diamonds)

# 5.1 Plot histograms for color, carat, and price, and comment on their shapes
# Carat
hist(diamonds$carat, main="Histogram for diamonds carat", xlab = "Diamond carat", border="blue", col="green")
# Color using barplot
barplot(height=table(diamonds$color), main="Histogram for diamonds color", xlab = "Diamond color", ylab = "Frequency")
# Color using histogram
diamonds$colorCode <- match(diamonds[,"color"][[1]], LETTERS)
h <-hist(diamonds$colorCode, main="Histogram for diamonds color", xlab = "Diamond color", border="blue", col="green", xaxt="n")
axis(side = 1, at = h$mids, labels = c("D","E","","F","","G","","H","","I","","J"))
# Price
hist(diamonds$price, main="Histogram for diamonds price", xlab = "Diamond price", border="blue", col="green")

# Three-way relationship
install.packages("GGally")
require(GGally)
D <- diamonds[, c("color", "carat", "price")]
# Order by price to get the colors by heat
D <- D[with(D, order(price)), ]
ggpairs(D,
        mapping=aes(color=color, alpha=0.5),
        columnLabels=c("color", "carat", "price"),
        title="Diamond Color vs. Carat vs. Price") +
  theme(axis.text.x=element_text(angle=45, vjust=1))



