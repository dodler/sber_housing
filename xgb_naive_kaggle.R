require(Matrix)
require(xgboost)
require(data.table)
require(dplyr)


prepare_data = function(inp){
  res = data.frame(inp)
  
  res = res %>%
    summarise(mkad_to_kremlin = mean(mkad_km / kremlin_km)
    ) %>%
    right_join(res, by='sub_area')
  
  return(res)
}

train <- fread("train.csv", sep=",", na.strings = "NA")

re_investment = 
  train %>% 
  filter(product_type=='Investment',timestamp>='2011-10-01') %>% 
  group_by(ts=substring(timestamp,1,7)) %>% 
  summarise(n=n(),
            n1M=sum(ifelse(price_doc<=1000000,1,0))/n(),
            n2M=sum(ifelse(price_doc==2000000,1,0))/n(),
            n3M=sum(ifelse(price_doc==3000000,1,0))/n())

m1=floor(mean(re_investment$n1M[re_investment$ts>='2015-01'])/10*nrow(train)) #undersampling by magic numbers
m2=floor(mean(re_investment$n2M[re_investment$ts>='2015-01'])/3*nrow(train)) #undersampling by magic numbers
m3=floor(mean(re_investment$n3M[re_investment$ts>='2015-01'])/2*nrow(train)) 

set.seed(1)
i1 = train %>% filter(price_doc<=1000000,product_type=='Investment') %>% sample_n(m1)
i2 = train %>% filter(price_doc==2000000,product_type=='Investment') %>% sample_n(m2)
i3 = train %>% filter(price_doc==3000000,product_type=='Investment') %>% sample_n(m3)

train = train %>% filter(!(price_doc<=1000000 & product_type=='Investment'))
train = train %>% filter(!(price_doc==2000000 & product_type=='Investment'))
train = train %>% filter(!(price_doc==3000000 & product_type=='Investment'))

train = rbind(train,i1,i2,i3) %>% arrange(id)


test <- data.frame(fread("test.csv", sep=",", na.strings = "NA"))
macro <- read.csv("macro.csv")
sample_submission <- read.csv("sample_submission.csv")

#add some indexes

#train = prepare_data(train)
#test = prepare_data(test)


# vars

demo_vars <- c('area_m', 'raion_popul', 'full_all', 'male_f', 'female_f', 'young_all', 
               'young_female', 'work_all', 'work_male', 'work_female', 'price_doc')

internal_chars <- c('full_sq', 'life_sq', 'floor', 'max_floor', 'build_year', 'num_room', 
                    'kitch_sq', 'state', 'price_doc')

school_chars <- c('children_preschool', 'preschool_quota', 'preschool_education_centers_raion',
                  'children_school', 'school_quota', 'school_education_centers_raion', 
                  'school_education_centers_top_20_raion', 'university_top_20_raion',
                  'additional_education_raion', 'additional_education_km', 'university_km',
                  'price_doc')

cult_chars <- c('sport_objects_raion', 'culture_objects_top_25_raion', 'shopping_centers_raion',
                'park_km', 'fitness_km', 'swim_pool_km', 'ice_rink_km','stadium_km', 'basketball_km', 
                'shopping_centers_km', 'big_church_km','church_synagogue_km', 'mosque_km', 'theater_km',
                'museum_km', 'exhibition_km', 'catering_km', 'price_doc')

inf_features <- c('nuclear_reactor_km', 'thermal_power_plant_km', 'power_transmission_line_km',
                  'incineration_km','water_treatment_km', 'incineration_km', 'railroad_station_walk_km',
                  'railroad_station_walk_min', 'railroad_station_avto_km', 'railroad_station_avto_min',
                  'public_transport_station_km', 'public_transport_station_min_walk', 'water_km',
                  'mkad_km', 'ttk_km', 'sadovoe_km','bulvar_ring_km', 'kremlin_km', 'price_doc')

# 1 - panel, 2 - brick, 3 - wood, 4 - mass concrete, 5 - breezeblock, 6 - mass concrete plus brick

#end

area_use_ratio = train$life_sq / train$full_sq
year_old = 2017 - train$build_year
living_area_per_room = train$life_sq / train$num_room
kitch_rel_size = train$kitch_sq / train$full_sq
house_res_pos = train$floor / train$max_floor
popul_density = train$raion_popul / train$area_m
avg_dist= (train$ttk_km + train$sadovoe_km + train$mkad_km + train$bulvar_ring_km + train$kremlin_km) / 5

train = data.frame(cbind(train,area_use_ratio,year_old,living_area_per_room,
                         popul_density, kitch_rel_size, house_res_pos, avg_dist))

area_use_ratio = test$life_sq / test$full_sq
year_old = 2017 - test$build_year
living_area_per_room = test$life_sq / test$num_room
kitch_rel_size = test$kitch_sq / test$full_sq
house_res_pos = test$floor / test$max_floor
popul_density = test$raion_popul / test$area_m
avg_dist= (test$ttk_km + test$sadovoe_km + test$mkad_km + test$bulvar_ring_km + test$kremlin_km) / 5

test = data.frame(cbind(test, area_use_ratio, year_old, living_area_per_room,
                        popul_density, kitch_rel_size, house_res_pos, avg_dist))
# remove correlated features


library(caret)
nums = sapply(train, is.numeric)
cor_mtr = cor(train[,nums])
cor_mtr[is.nan(cor_mtr)] <- 0
cor_mtr[is.na(cor_mtr)] <- 0
high_corr = findCorrelation(cor_mtr)
train = train[,-high_corr]
test = test[,-high_corr]

# end


#


id_test = test$id

y_train <- train$price_doc

x_train <- subset(train, select = -c(id, timestamp, price_doc))
x_test <- subset(test, select = -c(id, timestamp))

len_train <- nrow(x_train)
len_test <- nrow(x_test)

train_test <- rbind(x_train, x_test)

features <- colnames(train_test)

for (f in features) {
  if ((class(train_test[[f]])=="factor") || (class(train_test[[f]])=="character")) {
    #cat("VARIABLE : ",f,"\n")
    levels <- unique(train_test[[f]])
    train_test[[f]] <- as.numeric(factor(train_test[[f]], levels=levels))
  }
}

x_train = train_test[1:len_train,]
x_test = train_test[(len_train+1):(len_train+len_test),]

dtrain = xgb.DMatrix(as.matrix(x_train), label=log(y_train+1))
dtest = xgb.DMatrix(as.matrix(x_test))

xgb_params = list(
  seed = 0,
  colsample_bytree = 0.7,
  subsample = 0.7,
  eta = 0.075,
  objective = 'reg:linear',
  max_depth = 6,
  num_parallel_tree = 1,
  min_child_weight = 1,
  base_score = 15.8123
)

#res = xgb.cv(xgb_params,
#             dtrain,
#             nrounds=2000,
#             nfold=10,
#             early_stopping_rounds=20,
#             print_every_n = 10,
#             verbose= 1,
#             maximize=F)

#best_nrounds = res$best_iteration
best_nrounds = 1450

gbdt = xgb.train(xgb_params, dtrain, best_nrounds)

prediction <- predict(gbdt,dtest)
#sample_submission$price_doc <- as.integer(exp(prediction)-1) / 10000) * 10000 # ?? round to thousands
sample_submission$price_doc <- exp(prediction)-1 # ?? round to thousands

write.csv(sample_submission, "xgb_out.csv", row.names = F, quote = FALSE)