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
# --- example output ---
# > r
# 2205 x 50 rating matrix of class ‘realRatingMatrix’ with 2889 ratings.

# create an evaluation scheme
e = evaluationScheme(r, method="split", train=0.9, given=1, goodRating=4)
# --- example output ---
# > e
# Evaluation scheme with 1 items given
# Method: ‘split’ with 1 run(s).
# Training set proportion: 0.900
# Good ratings: >=4.000000
# Data set: 2205 x 50 rating matrix of class ‘realRatingMatrix’ with 2889 ratings.

# create a user-based collaborative filtering recommender
r1 = Recommender(getData(e, "train"), "UBCF")
# --- example output ---
# > r1
# Recommender of type ‘UBCF’ for ‘realRatingMatrix’
# learned using 1984 users.

# create an item based collaborative filtering recommender
r2 = Recommender(getData(e, "train"), "IBCF")
# --- example output ---
# > r2
# Recommender of type ‘IBCF’ for ‘realRatingMatrix’
# learned using 1984 users.

# predict the rates for both recommenders for both the "known" test set (15 items)
p1 <- predict(r1, getData(e, "known"), type="ratings")
p2 <- predict(r2, getData(e, "known"), type="ratings")
# --- example output ---
# > p1
# 221 x 50 rating matrix of class ‘realRatingMatrix’ with 4786 ratings.
# > p2
# 221 x 50 rating matrix of class ‘realRatingMatrix’ with 389 ratings.

# evaluate the predicted ratings
error = rbind(
	calcPredictionAccuracy(p1, getData(e, "unknown")),
	calcPredictionAccuracy(p2, getData(e, "unknown"))
)
rownames(error) <- c("UBCF","IBCF")
# --- example output ---
# > error
#          RMSE      MSE      MAE
# UBCF 6.896676 47.56414 3.779785
# IBCF 5.590138 31.24965 3.407941

