library(recommenderlab)
library(plyr)

yelp_review = read.csv("yelp_academic_dataset_review_AZ_food_noText.csv", stringsAsFactors=FALSE)

yelp_rating_flat = yelp_review[,c("user_id","business_id","stars")]

yelp_rating_unique = yelp_rating_flat[!duplicated(yelp_rating_flat[1:2]),]

topNReviewedBusinesses = 50
business_review_frequency = data.frame(table(yelp_rating_unique$business_id))
top_n_most_reviewed_businesses = business_review_frequency[order(-business_review_frequency$Freq),][1:topNReviewedBusinesses,]
yelp_rating_unique = yelp_rating_unique[yelp_rating_unique$business_id %in% top_n_most_reviewed_businesses[,1], ]

minUserReviews = 10
user_ids <- table(yelp_rating_unique$user_id)
yelp_rating_unique = yelp_rating_unique[yelp_rating_unique$user_id %in% names(user_ids)[user_ids >= minUserReviews], ]

yelp_rating = daply(yelp_rating_unique, .(user_id, business_id), function(x) x$stars)
m = data.matrix(yelp_rating)
r = as(m, "realRatingMatrix")

r = binarize(r, minRating=4)

algorithms = list(
  "random" = list(name="RANDOM", param=NULL),
  "popular" = list(name="POPULAR", param=NULL),
  "UBCF" = list(name="UBCF", param=list()),
  "UBCF-j" = list(name="UBCF", param=list(method="Jaccard", nn=50)),
  "IBCF" = list(name="IBCF", param=list())
)

minUserReviews = 1
split = evaluationScheme(r, method="split", train = 0.3, k=1, given=minUserReviews, goodRating=1)
split_evals = evaluate(split, algorithms, n=c(1, 3, 5, 10, 15, 20))

getConfusionMatrix(split_evals[["UBCF"]])
getConfusionMatrix(split_evals[["UBCF-j"]])
getConfusionMatrix(split_evals[["IBCF"]])

plot(split_evals, annotate=1:length(algorithms), legend="topleft")

plot(split_evals, "prec/rec", annotate=1:length(algorithms))