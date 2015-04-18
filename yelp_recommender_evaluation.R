# http://cran.r-project.org/web/packages/recommenderlab/vignettes/recommenderlab.pdf
# http://datasciencelondon.org/recommender-systems-in-r-by-tamas-jambor-sky/

#install.packages("recommenderlab")
#install.packages("plyr")

# construct a matrix for users and businesses
library(recommenderlab)
library(plyr)

# read the reviews
yelp_review = read.csv("yelp_academic_dataset_review_AZ_food_no_text.csv", stringsAsFactors=FALSE)
# TODO: filter by Phoenix
# TODO: #NAME?

# project relevant columns
yelp_rating_flat = yelp_review[,c("user_id","business_id","stars")]
# discard non-unique entries (based on user_id and business_id only)
yelp_rating_unique = yelp_rating_flat[!duplicated(yelp_rating_flat[1:2]),]

# select reviews of only the most reviewed businesses
topNReviewedBusinesses = 50
business_review_frequency = data.frame(table(yelp_rating_unique$business_id))
top_n_most_reviewed_businesses = business_review_frequency[order(-business_review_frequency$Freq),][1:topNReviewedBusinesses,]
yelp_rating_unique = yelp_rating_unique[yelp_rating_unique$business_id %in% top_n_most_reviewed_businesses[,1], ]

# filter users who have made less than x reviews
#TODO: how does min user review affect evaluation?
minUserReviews = 10
user_ids <- table(yelp_rating_unique$user_id)
yelp_rating_unique = yelp_rating_unique[yelp_rating_unique$user_id %in% names(user_ids)[user_ids >= minUserReviews], ]

# reshape data frame
yelp_rating = daply(yelp_rating_unique, .(user_id, business_id), function(x) x$stars)
# convert to a matrix
m = data.matrix(yelp_rating)
# create a "rating matrix"
r = as(m, "realRatingMatrix")

# clean up temp vars
# rm(yelp_rating_flat)
# rm(yelp_rating_unique)
# rm(yelp_rating)
# rm(m)
# this is a good place to save your workspace!!

algorithms = list(
	"random" = list(name="RANDOM", param=NULL),
	"popular" = list(name="POPULAR", param=NULL)
	#"user-based CF" = list(name="UBCF", param=list(method="Cosine", nn=50, minRating=5))
	# TODO: UBCF
	# TODO: IBCF
	# TODO: others
)
# TODO: Variety of Similarity methods
# TODO: Variety of Normalization methods (centering?)

# evaluation schemes
# split
split = evaluationScheme(r, method="split", train = 0.9, k=1, given=minUserReviews, goodRating=5)
split_evals = evaluate(split, algorithms, n=c(1, 3, 5, 10, 15, 20))
# TODO: k-fold
# TODO: bootstrap sampling

# evaluation results
getConfusionMatrix(split_evals[["popular"]])

# visualization
# Split: ROC Curve
plot(split_evals, annotate=1:length(algorithms), legend="topleft")
# TODO: plot prec/recall chart

# TODO: Evaluate Predictions (RMSE)
# TODO: Evaluate Binarized data
# TODO: Build a script that can make a prediction (top 5) and retrieve the restaurant names for a user

# --- misc functions ---
# simple search for value
# yelp_rating_unique[yelp_rating_unique$user_id == 'ZzMIcp1Ev47C3owOnjwQ9Q',]