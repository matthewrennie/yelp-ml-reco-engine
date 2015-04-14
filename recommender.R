# http://cran.r-project.org/web/packages/recommenderlab/vignettes/recommenderlab.pdf

#install.packages("recommenderlab")
#install.packages("plyr")

# construct a matrix for users and businesses
library(recommenderlab)
library(plyr)

# read the reviews
yelp_review = read.csv("yelp_academic_dataset_review_AZ_food_no_text.csv", stringsAsFactors=FALSE)
yelp_review = yelp_review[1:3000,] #limit the set like this
# project relevant columns
yelp_rating_flat = yelp_review[,c("user_id","business_id","stars")]
# discard non-unique entries (based on user_id and business_id only)
yelp_rating_unique = yelp_rating_flat[!duplicated(yelp_rating_flat[1:2]),]
# reshape data frame
yelp_rating = daply(yelp_rating_unique, .(user_id, business_id), function(x) x$stars)
# convert to a matrix
m = data.matrix(yelp_rating)
# --- example output --- (rows are user_ids, columns are business_ids)
# > head(m[,1:2])
#                         business_id
# user_id                  2I666dqzs1XB6xigIKa9bA 2ZnCITVa0abGce4gZ6RhIw
#   _-Cpw9hiKyO-cJydjH9b2g                     NA                     NA
#   _-Zrzf2QYiw4KHugUTXNYQ                     NA                     NA
#   _0cGchIRQ63keN5YN_jkcg                     NA                     NA
#   _2oYcAukTIaexAnsjUo9tg                     NA                     NA
#   _4lqpCYCqOQzbB6xQGGhrQ                     NA                     NA
#   _7el1cOgnfkNfmZKi277bQ                     NA                     NA

# create a "rating matrix"
r = as(m, "realRatingMatrix")

# plot a histograms
# hist(getRatings(r), breaks=100) # all the ratings
# hist(colMeans(r)) # average rating per business
# hist(rowCounts(r)) # number of ratings per user

# create test and train sets
r_train = r[1:(nrow(r)-5),]
r_test = r[(nrow(r)-4):nrow(r),]

# create a recommender
recommender = Recommender(r_train, method = "UBCF")
# --- example output ---
# > recommender
# Recommender of type ‘UBCF’ for ‘realRatingMatrix’
# learned using 2200 users.

# create a prediction for the test set
recom = predict(recommender, r_test, n=5)
# --- example output ---
# > recom
# Recommendations as ‘topNList’ with n = 5 for 5 users.
# > as(recom, "list")
# [[1]]
# [1] "2I666dqzs1XB6xigIKa9bA" "2ZnCITVa0abGce4gZ6RhIw" "45puCRQ6Vh_IIAy7kkfFDQ"
# [4] "4WcxnNv_lzCIG2eb8O5Mlg" "5XIvwfbIa-vuFEkkeCQ6kQ"

# [[2]]
# [1] "kaIue7GRCmkPzDeHDBTttQ" "KPoTixdjoJxSqRSEApSAGg" "muCl5p-9ut1sY0aKeUeRhw"
# [4] "VsO_rhXi5lgbaGxNwoEZsQ" "q1nDSpmV-FdTBNj6UuZvTg"

# [[3]]
# [1] "2I666dqzs1XB6xigIKa9bA" "2ZnCITVa0abGce4gZ6RhIw" "45puCRQ6Vh_IIAy7kkfFDQ"
# [4] "4WcxnNv_lzCIG2eb8O5Mlg" "5XIvwfbIa-vuFEkkeCQ6kQ"

# [[4]]
# [1] "2I666dqzs1XB6xigIKa9bA" "2ZnCITVa0abGce4gZ6RhIw" "45puCRQ6Vh_IIAy7kkfFDQ"
# [4] "4WcxnNv_lzCIG2eb8O5Mlg" "5XIvwfbIa-vuFEkkeCQ6kQ"

# [[5]]
# [1] "2I666dqzs1XB6xigIKa9bA" "2ZnCITVa0abGce4gZ6RhIw" "45puCRQ6Vh_IIAy7kkfFDQ"
# [4] "4WcxnNv_lzCIG2eb8O5Mlg" "5XIvwfbIa-vuFEkkeCQ6kQ"

# retrieve the best rating for each user
# recom3 = bestN(recom, n = 3)

# predict ratings
recom_rate = predict(recommender, r_test, type="ratings")
# --- example output ---
# > recom_rate
# 5 x 50 rating matrix of class ‘realRatingMatrix’ with 244 ratings.
# > as(recom_rate, "matrix")[,1:3]
#      2I666dqzs1XB6xigIKa9bA 2ZnCITVa0abGce4gZ6RhIw 45puCRQ6Vh_IIAy7kkfFDQ
# [1,]               1.000000               1.000000                1.00000
# [2,]               2.453671               2.464201                2.52184
# [3,]               2.000000               2.000000                2.00000
# [4,]               4.000000               4.000000                4.00000
# [5,]               3.000000               3.000000                3.00000