# install.packages("e1071", "~/Rpackages")
# install.packages("plyr", "~/Rpackages")
# install.packages("vcd", "~/Rpackages")
library(e1071, lib.loc="~/Rpackages")
library(plyr, lib.loc="~/Rpackages")
library(vcd, lib.loc="~/Rpackages")

# business:
cat("business.review_count.instances: ", sum(yelp_business$review_count), "\n")
cat("business.review_count.mean: ", mean(yelp_business$review_count), "\n")
cat("business.review_count.median: ", median(yelp_business$review_count), "\n")
cat("business.review_count.variance: ", var(yelp_business$review_count), "\n")
cat("business.review_count.stddev: ", sd(yelp_business$review_count), "\n")
cat("business.review_count.skewness: ", skewness(yelp_business$review_count), "\n")
cat("business.review_count.kurtosis: ", kurtosis(yelp_business$review_count), "\n")
print("business.review_count.percentiles:")
quantile(yelp_business$review_count, c(.05, 0.25, 0.75, .95))
jpeg('business.review_count.hist.jpg')
hist(yelp_business$review_count, right=FALSE, xlab="Business Reviews", col=c("red"))
dev.off()

cat("business.stars.instances: ", sum(yelp_business$stars), "\n")
cat("business.stars.mean: ", mean(yelp_business$stars), "\n")
cat("business.stars.median: ", median(yelp_business$stars), "\n")
cat("business.stars.variance: ", var(yelp_business$stars), "\n")
cat("business.stars.stddev: ", sd(yelp_business$stars), "\n")
cat("business.stars.skewness: ", skewness(yelp_business$stars), "\n")
cat("business.stars.kurtosis: ", kurtosis(yelp_business$stars), "\n")
print("business.stars.percentiles:")
quantile(yelp_business$stars, c(.05, 0.25, 0.75, .95))
jpeg('business.stars.box.jpg')
boxplot(yelp_business$stars, horizontal=TRUE, col=c("blue"), xlab="Business Stars")
dev.off()

yelp_business_happy = subset(yelp_business, attributes.Happy.Hour == "True")
cat("business.happy.review_count.instances: ", sum(yelp_business_happy$review_count), "\n")
cat("business.happy.stars.mean: ", mean(yelp_business_happy$stars), "\n")
yelp_business_nothappy = subset(yelp_business, attributes.Happy.Hour == "False")
cat("business.nothappy.review_count.instances: ", sum(yelp_business_nothappy$review_count), "\n")
cat("business.nothappy.stars.mean: ", mean(yelp_business_nothappy$stars), "\n")

yelp_business_CA = subset(yelp_business, state == "CA")
cat("business.ca.review_count.instances: ", sum(yelp_business_CA$review_count), "\n")
cat("business.ca.stars.mean: ", mean(yelp_business_CA$stars), "\n")
yelp_business_PA = subset(yelp_business, state == "PA")
cat("business.pa.review_count.instances: ", sum(yelp_business_PA$review_count), "\n")
cat("business.pa.stars.mean: ", mean(yelp_business_PA$stars), "\n")
yelp_business_AZ = subset(yelp_business, state == "AZ")
cat("business.az.review_count.instances: ", sum(yelp_business_AZ$review_count), "\n")
cat("business.az.stars.mean: ", mean(yelp_business_AZ$stars), "\n")

# review:
# useful
cat("review.votes.useful.instances: ", sum(yelp_review$votes.useful), "\n")
cat("review.votes.useful.mean: ", mean(yelp_review$votes.useful), "\n")
cat("review.votes.useful.median: ", median(yelp_review$votes.useful), "\n")
cat("review.votes.useful.variance: ", var(yelp_review$votes.useful), "\n")
cat("review.votes.useful.stddev: ", sd(yelp_review$votes.useful), "\n")
cat("review.votes.useful.skewness: ", skewness(yelp_review$votes.useful), "\n")
cat("review.votes.useful.urtosis: ", kurtosis(yelp_review$votes.useful), "\n")
print("review.votes.useful.percentiles:")
quantile(yelp_review$votes.useful, c(.05, 0.25, 0.75, .95))
jpeg('review.votes.useful.hist.jpg')
hist(yelp_user$votes.useful, right=FALSE, xlab="Useful Review", col=c("green"))
dev.off()

# user
# review count
cat("user.review_count.instances: ", length(yelp_user$review_count), "\n")
cat("user.review_count.mean: ", mean(yelp_user$review_count), "\n")
cat("user.review_count.median: ", median(yelp_user$review_count), "\n")
cat("user.review_count.variance: ", var(yelp_user$review_count), "\n")
cat("user.review_count.stddev: ", sd(yelp_user$review_count), "\n")
cat("user.review_count.skewness: ", skewness(yelp_user$review_count), "\n")
cat("user.review_count.kurtosis: ", kurtosis(yelp_user$review_count), "\n")
print("user.review_count.percentiles:")
quantile(yelp_user$review_count, c(.05, 0.25, 0.75, .95))
jpeg('user.review_count.hist.jpg')
hist(yelp_user$review_count, right=FALSE, xlab="Reviews by User", col=c("red"))
dev.off()

cat("user.average_stars.instances: ", sum(yelp_user$average_stars), "\n")
cat("user.average_stars.mean: ", mean(yelp_user$average_stars), "\n")
cat("user.average_stars.median: ", median(yelp_user$average_stars), "\n")
cat("user.average_stars.variance: ", var(yelp_user$average_stars), "\n")
cat("user.average_stars.stddev: ", sd(yelp_user$average_stars), "\n")
cat("user.average_stars.skewness: ", skewness(yelp_user$average_stars), "\n")
cat("user.average_stars.kurtosis: ", kurtosis(yelp_user$average_stars), "\n")
print("user.review_count.percentiles:")
quantile(yelp_user$average_stars, c(.05, 0.25, 0.75, .95))
quantile(yelp_business$stars, c(.05, 0.25, 0.75, .95))
jpeg('user.stars.box.jpg')
boxplot(yelp_business$stars, horizontal=TRUE, col=c("blue"), xlab="Stars by User")
dev.off()

cat("covariance between review count and average stars: ", cov(yelp_user$review_count, yelp_user$average_stars), "\n")
cat("covariance between review count and number of fans: ", cov(yelp_user$review_count, yelp_user$fans), "\n")

# funny, cool, useful grouped by stars
# ddply(yelp_review, "stars", function(x) {
# 	average_stars = x$average_stars
# 	review_count = x$review_count
# 	nfriends = as.numeric(x$friends)
# 	data.frame(average_stars = average_stars, review_count = review_count, nfriends = nfriends)
# })

# takes a long time!
yelp_user_friends = ddply(yelp_user, "user_id", function(x) {
	as = x$average_stars
	rc = x$review_count
	nf = as.numeric(x$friends)
	data.frame(average_stars = as, review_count = rc, nfriends = nf)
})
cat("covariance between review count and number of friends: ", cov(yelp_user_friends$review_count, yelp_user_friends$nfriends), "\n")
cat("covariance between average stars and number of friends: ", cov(yelp_user_friends$average_stars, yelp_user_friends$nfriends), "\n")



