# Bush vs Buchanan
#
# Google "butterfly ballot" if not already familiar.

x <- read.csv("BushBuchananElection2000.csv", as.is=TRUE)
head(x)
tail(x)
pb <- x[x$county=="PALM BEACH",]
y <- x[x$county!="PALM BEACH",]

# Using `x`, here is a scatterplot with votes for Buchanan on the y-axis
# and votes for Bush on the x-axis.  Using text() I've labeled the point for
# Palm Beach County. The idea is that in Palm Beach the number of votes
# for Buchanan was surprisingly high; could be that many were intended
# for Gore but the butterfly ballot confused voters. Here, I'll try to use
# support for Bush to predict votes for Buchanan in Palm Beach.

plot((x$buchanan2000) ~ (x$bush2000), data=x, 
     xlab="Votes for Bush",
     ylab="Votes for Buchanan")
text(pb$bush2000, pb$buchanan2000, labels = "PALM BEACH", pos=4)

# Now using `y`, I explore log() and sqrt() transformations of the
# variables that make the association close to linear with with roughly
# constant variance around the general trend.

par(mfrow=c(1,2))

plot(log(buchanan2000) ~ log(bush2000), data=y)

plot(sqrt(buchanan2000) ~ sqrt(bush2000), data=y)

# Next I fit a linear model complete with scatterplot of the transformed
# variables and superimposed regression line.  

y$logpat2000 <- log(y$buchanan2000)
y$logw2000 <- log(y$bush2000)

lm.test1 <- lm(log(buchanan2000) ~ log(bush2000), data=y)
plot(log(buchanan2000) ~ log(bush2000), data=y,
     main="2000 Election Votes (log)",
     xlab="Votes for Bush (log)",
     ylab="Votes for Buchanan (log)")
abline(lm.test1)
summary(lm.test1)

# high R^2 (0.8658) and low residual std error (0.4198),
# meaning that the model fits the data well. Unsure if it 
# fits so well as to appear slightly unreasonable. But good fit nonetheless.

lm.test2 <- lm(sqrt(buchanan2000) ~ sqrt(bush2000), data=y)
plot(sqrt(buchanan2000) ~ sqrt(bush2000), data=y,
     main="2000 Election Votes (sqrt)",
     xlab="Votes for Bush (sqrt)",
     ylab="Votes for Buchanan (sqrt)")
abline(lm.test2)
summary(lm.test2)

# Also high R^2 (0.8499)  and not quite as low resid std error (2.739),
# but still strong fit.
#
# Now that we've fitted the model, I use predict() to predict votes for Buchanan, 
# with `newdata=pb` as an option. I want to compare this prediction to the actual 
# votes for Buchanan in Palm Beach, 3407.  

pb$logpat2000 <- log(pb$buchanan2000)
pb$logw2000 <- log(pb$bush2000)
pb$sqrtpat2000 <- sqrt(pb$buchanan2000)
pb$sqrtw2000 <- sqrt(pb$bush2000)

exp(predict(lm.test1, newdata=pb)) # ~592 votes
(predict(lm.test2, newdata=pb))^2  # ~626 votes

# These are way off from the observed value of 3407 votes. Given that previously
# we saw the models excluding PALM BEACH data had good linear fit,
# it strikes me as anomolous that observed votes differs drastically from what 
# should be a relatively good linear prediction.
