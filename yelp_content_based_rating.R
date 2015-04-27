# http://en.wikibooks.org/wiki/Data_Mining_Algorithms_In_R/Classification/Naïve_Bayes
# install.packages('e1071', dependencies = TRUE)
library(plyr)

# create a dataframe of interesting business attributes
yelp_business = read.csv("az_restaurant_businesses.csv", na.strings = "")
# content_cols = c("business_id", "attributes_ambience_touristy", "attributes_ambience_divey", "attributes_dietaryrestrictions_vegan", "attributes_happyhour", "attributes_orderatcounter", "attributes_byob", "attributes_outdoorseating", "attributes_alcohol", "attributes_ambience_classy", "attributes_parking_lot", "attributes_corkage", "attributes_goodfor_brunch", "attributes_waiterservice", "attributes_parking_street", "attributes_ambience_hipster", "attributes_byobcorkage", "attributes_music_live", "attributes_dietaryrestrictions_dairyfree", "attributes_music_background_music", "attributes_goodfor_dinner", "attributes_goodfor_breakfast", "attributes_parking_garage", "attributes_music_karaoke", "attributes_goodfordancing", "attributes_acceptscreditcards", "attributes_goodfor_lunch", "attributes_goodforkids", "attributes_parking_valet", "attributes_takeout", "attributes_paymenttypes_cash_only", "attributes_goodfor_dessert", "attributes_music_video", "attributes_dietaryrestrictions_halal", "attributes_takesreservations", "attributes_ambience_trendy", "attributes_delivery",  "attributes_wifi",  "attributes_wheelchairaccessible", "attributes_dietaryrestrictions_glutenfree", "attributes_caters", "attributes_ambience_intimate", "attributes_goodfor_latenight", "attributes_coatcheck", "attributes_parking_validated", "attributes_music_dj", "attributes_hastv", "attributes_ambience_casual", "attributes_dietaryrestrictions_kosher", "attributes_dogsallowed", "attributes_drivethru", "attributes_drivethru", "attributes_noiselevel", "attributes_noiselevel", "attributes_smoking", "attributes_goodforgroups", "attributes_goodforgroups", "attributes_ambience_romantic", "attributes_ambience_upscale")
content_cols = c("business_id", "categories_as_strings", "attributes_pricerange", "attributes_ambience_touristy", "attributes_ambience_divey", "attributes_dietaryrestrictions_vegan", "attributes_happyhour", "attributes_orderatcounter", "attributes_byob", "attributes_outdoorseating", "attributes_ambience_classy", "attributes_parking_lot", "attributes_corkage", "attributes_goodfor_brunch", "attributes_waiterservice", "attributes_parking_street", "attributes_ambience_hipster", "attributes_music_live", "attributes_dietaryrestrictions_dairyfree", "attributes_music_background_music", "attributes_goodfor_dinner", "attributes_goodfor_breakfast", "attributes_parking_garage", "attributes_music_karaoke", "attributes_goodfordancing", "attributes_goodfor_lunch", "attributes_goodforkids", "attributes_parking_valet", "attributes_takeout", "attributes_paymenttypes_cash_only", "attributes_goodfor_dessert", "attributes_music_video", "attributes_dietaryrestrictions_halal", "attributes_takesreservations", "attributes_ambience_trendy", "attributes_delivery", "attributes_wheelchairaccessible", "attributes_dietaryrestrictions_glutenfree", "attributes_caters", "attributes_ambience_intimate", "attributes_goodfor_latenight", "attributes_coatcheck", "attributes_parking_validated", "attributes_music_dj", "attributes_hastv", "attributes_ambience_casual", "attributes_dietaryrestrictions_kosher", "attributes_dogsallowed", "attributes_drivethru", "attributes_drivethru", "attributes_goodforgroups", "attributes_goodforgroups", "attributes_ambience_romantic", "attributes_ambience_upscale")
content_cols = c("business_id", "categories_as_strings", "attributes_pricerange")
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

# 37450 GrSixRnGIxNUJ1Cn5DNX9A  327
# 49528 Kqvfep2mxS10S50FbVDi4Q  325
# 3782  1BW2HC851fJKPfJeQxjkTA  316
# 66215 q9XgOylNsSbqZqF_SO3-OQ  312
# 53881 m7vtKWpZ9wdEQ95wJxrMrg  298
# 82981 VPXgY9lGJF3XC4ZpusxNuA  297
# 62977 p7dvahRPZIWL7T6pFA_0dg  296
# 53820 M6oU3OBf_E6gqlfkLGlStQ  293
# 45529 JgDkCER12uiv4lbpmkZ9VA  292
# 64830 Ps1Db9zOatoF_76FZNO5CQ  290
# 70371 rLtl8ZkDX5vH5nAx9C3q5Q  282
# 14423 8dbRf1UsWp2ktXHZ6Zv06w  275
# 85189 wHg1YkCzdZq9WBJOTRgxHQ  274
# 62758 P2kVk4cIWyK4e4h14RhK-Q  271
# 75970 thdVzCfKx-DV0zYWqId3pw  264
# 59018 NvDR3SPVPXrDB_dbKuGoWA  255
# 80192 usQTOj7LQ9v0Fl98gRa3Iw  255
# 45221 jdeNI5TTTuM6mj3HTgstRA  252
# 64876 PShy2RYNadDUhJf4ErOJ7w  250
# 74089 ST8Yzlk2MqKlcaLqL2djBg  250
# 82176 vhxFLqRok6r-D_aQz0s-JQ  248
# 8427  4E_nPWw89FLFHdNsEgMH-g  238
# 14991 8p4at4zdzCpueAmSBaorZA  236
# 24974 Cp-PV8rsypbO-xBrQ6KmQg  236

# for a chosen user, create a dataframe of business reviews indicating whether the user stars_nums/disstars_nums the business
chosen_one = "DrWLhrK8WMZf7Jb-Oqc7ww"
good_review = 5
chosen_one_reviews = subset(yelp_rating_unique, user_id == chosen_one)
# chosen_one_reviews$stars_num = as.factor("True")
# chosen_one_reviews$stars_num <- ifelse(chosen_one_reviews$stars >= good_review, as.factor("True"), as.factor("False"))
# chosen_one_reviews$stars_num[chosen_one_reviews$stars >= good_review] = "True"
# chosen_one_reviews$stars_num[chosen_one_reviews$stars < good_review] = "False"

user_model = join(yelp_business_content, chosen_one_reviews, by = "business_id", type = "inner", match = "first")
user_model_good = user_model[user_model$stars >= good_review,]
categories = tolower(gsub("'","", unlist(strsplit(gsub("\\[|\\]| |", "", user_model_good$categories_as_strings), ","))))
stop_categories = c("restaurants", "food", "bars")
categories_filtered = categories[!categories %in% stop_categories]
categories_freq = count(categories_filtered)
categories_freq$tf = count(categories_filtered)$freq/length(categories_filtered)

f = function(categories_as_strings) {
	review_categories = tolower(gsub("'","", unlist(strsplit(gsub("\\[|\\]| |", "", categories_as_strings), ","))))
	review_categories_filtered = review_categories[!review_categories %in% stop_categories]
	avgfreq = sum(categories_freq[categories_freq$x %in% review_categories_filtered,]$tf)/length(review_categories)
	avgfreq
}
user_model = adply(user_model, 1, function(x) f(x$categories_as_strings))
colnames(user_model)[ncol(user_model)] = "avgtf"

user_model$rating = user_model$stars - mean(user_model$stars)
# user_model$like = as.factor(c("True","False",rep("True",nrow(user_model)-2)))
user_model$attributes_pricerange = as.factor(user_model$attributes_pricerange)
drop_cols = c("user_id", "categories_as_strings", "stars", "business_id")
user_model = user_model[,!(names(user_model) %in% drop_cols)]
user_model_unique = user_model
# user_model[is.na(user_model)]<-"False"
# user_model_unique = unique(user_model[duplicated(user_model[1:ncol(user_model)-1]),])

# create a naive bayes classifier to predict stars_nums/disstars_nums
# NOTE: this doesnt work
# TODO: improve data (accurately represent "True","False","")

# > model
# table(predict(model$finalModel, x)$class, y)


# gsub("\\[|\\]| |", "", yelp_business$categories_as_strings)

library(caret)
library(klaR)
library(doMC)
registerDoMC(cores=4)

set.seed(123456)
train_idx <- createDataPartition(user_model_unique$rating, p = .6, list = FALSE, times = 1)
user_model_train <- user_model_unique[ train_idx,]
user_model_test  <- user_model_unique[-train_idx,]

folds=5
repeats=1
simple = trainControl(method='cv', number=10, allowParallel = TRUE)
myControl1 <- trainControl(method='cv', number=folds, repeats=repeats, classProbs=TRUE, returnData=TRUE, savePredictions=TRUE, verboseIter=TRUE, allowParallel=TRUE, summaryFunction=twoClassSummary, index=createMultiFolds(user_model_train$rating, k=folds, times=repeats))
myControl <- trainControl(method='cv', number=folds, repeats=repeats, returnResamp='none', classProbs=TRUE, returnData=FALSE, savePredictions=TRUE, verboseIter=TRUE, allowParallel=TRUE, summaryFunction=twoClassSummary, index=createMultiFolds(user_model_train$rating, k=folds, times=repeats))
trControl = trainControl(method = 'cv', number = 2, returnResamp = 'none', classProbs = T, allowParallel = T, returnData = F)

model <- train(rating ~ ., data=user_model_train, method = 'svmLinear', trControl = myControl)
model <- train(x=user_model_train[,1:ncol(user_model_train)-1], y=user_model_train[,ncol(user_model_train)], method = 'nb', trControl = trControl)
model <- train(rating ~ ., data=user_model_train, method='gbm', trControl=trControl, tuneGrid=expand.grid(.n.trees=500, .interaction.depth=15, .shrinkage = 0.01))
model <- train(rating ~ ., data=user_model_train, method='gbm', trControl=trControl)
model <- train(rating ~ ., data=user_model_train, method='knn', trControl=trControl)
model <- train(rating ~ ., data=user_model_train, method='rf', trControl=trControl)
model <- train(rating ~ ., data=user_model_train, method='wsrf', trControl=trControl)
model <- train(rating ~ ., data=user_model_train, method='gcvEarth', trControl=trControl)
model <- train(rating ~ ., data=user_model_train, method='ada', trControl=trControl)
model <- train(rating ~ ., data=user_model_train, method='avNNet', trControl=trControl)
model <- train(rating ~ ., data=user_model_train, method='bayesglm', trControl=trControl)
model <- train(rating ~ ., data=user_model_train, method='Boruta', trControl=trControl)
model <- train(rating ~ ., data=user_model_train, method='svmRadial', trControl=trControl)  #0.26
model <- train(rating ~ ., data=user_model_train, method='svmRadialCost', trControl=trControl)  #0.65
model <- train(rating ~ ., data=user_model_train, method='svmRadialWeights', trControl=trControl)
model <- train(rating ~ ., data=user_model_train, method='xyf', trControl=trControl)

#eval
test_pred <- predict(model, user_model_test)
postResample(test_pred, user_model_test$stars_num)
# sensitivity(test_pred, user_model_test$stars_num)
confusionMatrix(test_pred, user_model_test$stars_num)