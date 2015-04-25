# http://en.wikibooks.org/wiki/Data_Mining_Algorithms_In_R/Classification/Naïve_Bayes
# install.packages('e1071', dependencies = TRUE)
library(e1071)
library(class)
library(plyr)

# create a dataframe of interesting business attributes
yelp_business = read.csv("az_restaurant_businesses.csv", stringsAsFactors=FALSE)
content_cols = c("business_id", "attributes_ambience_touristy", "attributes_ambience_divey", "attributes_dietaryrestrictions_vegan", "attributes_happyhour", "attributes_orderatcounter", "attributes_byob", "attributes_outdoorseating", "attributes_alcohol", "attributes_ambience_classy", "attributes_parking_lot", "attributes_corkage", "attributes_goodfor_brunch", "attributes_waiterservice", "attributes_parking_street", "attributes_ambience_hipster", "attributes_byobcorkage", "attributes_music_live", "attributes_dietaryrestrictions_dairyfree", "attributes_music_background_music", "attributes_goodfor_dinner", "attributes_goodfor_breakfast", "attributes_parking_garage", "attributes_music_karaoke", "attributes_goodfordancing", "attributes_acceptscreditcards", "attributes_goodfor_lunch", "attributes_goodforkids", "attributes_parking_valet", "attributes_takeout", "attributes_paymenttypes_cash_only", "attributes_goodfor_dessert", "attributes_music_video", "attributes_dietaryrestrictions_halal", "attributes_takesreservations", "attributes_ambience_trendy", "attributes_delivery",  "attributes_wifi",  "attributes_wheelchairaccessible", "attributes_dietaryrestrictions_glutenfree", "attributes_caters", "attributes_ambience_intimate", "attributes_goodfor_latenight", "attributes_coatcheck", "attributes_parking_validated", "attributes_music_dj", "attributes_hastv", "attributes_ambience_casual", "attributes_dietaryrestrictions_kosher", "attributes_dogsallowed", "attributes_drivethru", "attributes_drivethru", "attributes_noiselevel", "attributes_noiselevel", "attributes_smoking", "attributes_goodforgroups", "attributes_goodforgroups", "attributes_ambience_romantic", "attributes_ambience_upscale")
yelp_business_content = yelp_business[,content_cols]

# process review data
yelp_review = read.csv("yelp_academic_dataset_review_AZ_food_no_text.csv", stringsAsFactors=FALSE)
yelp_rating_flat = yelp_review[,c("user_id","business_id","stars")]
yelp_rating_unique = yelp_rating_flat[!duplicated(yelp_rating_flat[1:2]),]

# find top reviewers
topNReviewingUsers = 50
user_review_frequency = data.frame(table(yelp_rating_unique$user_id))
top_n_most_reviewing_users = user_review_frequency[order(-user_review_frequency$Freq),][1:topNReviewingUsers,]
# head(top_n_most_reviewing_users)
# 6979  3gIfcQq5KxAegwCPXc83cQ  637
# 42888 ikm0UCahtK34LbLCEw4YTw  574
# 15604 90a6z--_CUrl84aCzZyPsg  557
# 28250 DrWLhrK8WMZf7Jb-Oqc7ww  525
# 80778 uZbTb-u-GVjTa2gtQfry5g  496

# for a chosen user, create a dataframe of business reviews indicating whether the user likes/dislikes the business
chosen_one = "ikm0UCahtK34LbLCEw4YTw"
good_review = 4
chosen_one_reviews = subset(yelp_rating_unique, user_id == chosen_one)
chosen_one_reviews$like = c(TRUE, FALSE)
chosen_one_reviews$like[chosen_one_reviews$stars >= good_review] = TRUE
chosen_one_reviews$like[chosen_one_reviews$stars < good_review] = FALSE

user_model = join(yelp_business_content, chosen_one_good_reviews, by = "business_id", type = "inner", match = "first")
drop_cols = c("user_id")
user_model = user_model[,!(names(user_model) %in% drop_cols)]

# create a naive bayes classifier to predict likes/dislikes
# NOTE: this doesnt work
classifier = naiveBayes(user_model[,"attributes_ambience_touristy":"attributes_ambience_upscale"], iris[,"like"]) 

# TODO: improve data (accurately represent "True","False","")
# TODO: evaluatation scheme
# TODO: create a hybrid between content and collaborative