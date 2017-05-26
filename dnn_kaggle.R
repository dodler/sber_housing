library(readr)
library(ggplot2)
library(caret)
library(h2o)

library(data.table)
library(Matrix)
library(xgboost)
library(dplyr)
library(lubridate)


data.train <- fread('train.csv', na.strings = "NA")
data.test <- fread("test.csv", sep=",", na.strings = "NA")
data.macro = fread("macro.csv", sep=",", na.strings="NA")

# resample train data

re_investment = 
  data.train %>% 
  filter(product_type=='Investment',timestamp>='2011-10-01') %>% 
  group_by(ts=substring(timestamp,1,7)) %>% 
  summarise(n=n(),
            n1M=sum(ifelse(price_doc<=1000000,1,0))/n(),
            n2M=sum(ifelse(price_doc==2000000,1,0))/n(),
            n3M=sum(ifelse(price_doc==3000000,1,0))/n())

m1=floor(mean(re_investment$n1M[re_investment$ts>='2015-01'])/10*nrow(data.train)) #undersampling by magic numbers
m2=floor(mean(re_investment$n2M[re_investment$ts>='2015-01'])/3*nrow(data.train)) #undersampling by magic numbers
m3=floor(mean(re_investment$n3M[re_investment$ts>='2015-01'])/2*nrow(data.train)) 

set.seed(1)
i1 = data.train %>% filter(price_doc<=1000000,product_type=='Investment') %>% sample_n(m1)
i2 = data.train %>% filter(price_doc==2000000,product_type=='Investment') %>% sample_n(m2)
i3 = data.train %>% filter(price_doc==3000000,product_type=='Investment') %>% sample_n(m3)

data.train = data.train %>% filter(!(price_doc<=1000000 & product_type=='Investment'))
data.train = data.train %>% filter(!(price_doc==2000000 & product_type=='Investment'))
data.train = data.train %>% filter(!(price_doc==3000000 & product_type=='Investment'))

data.train = rbind(data.train,i1,i2,i3) %>% arrange(id)
data.train = as.data.table(data.train)

# Transform target variable, so that we can use RMSE in XGBoost
data.train[,price_doc := log1p(as.integer(price_doc))]
data.test[,price_doc:=-1]

data.train[is.na(data.train)] = -1
data.test[is.na(data.test)] = -1

# Combine data.tables
data <- rbind(data.train, data.test)

#clean full_sq and life_sq. sometime full_sq is smaller than life_sq
data[ ,life_sq:=ifelse(is.na(life_sq),full_sq,life_sq)]
data[ ,full_sq:=ifelse(life_sq>full_sq,life_sq,full_sq)]

#build_year
data[ ,build_year:=ifelse((build_year >1690 & build_year<2020),build_year,'NA')]
data[ ,build_year:=as.integer(build_year)]

#num_rooms
data[ ,num_room:=ifelse(num_room==0,'NA',num_room)]
data[ ,num_room:=as.integer(num_room)]

#state
data[ ,state := ifelse(state==33,3,state)]

# Convert characters to factors/numeric
cat("Feature engineering")
data[,":="(product_type=as.factor(product_type)
           ,sub_area=as.factor(sub_area)
           ,ecology=as.factor(ecology)
           ,thermal_power_plant_raion=ifelse(thermal_power_plant_raion=="no",0,1)
           ,incineration_raion=ifelse(incineration_raion=="no",0,1)
           ,oil_chemistry_raion=ifelse(oil_chemistry_raion=="no",0,1)
           ,radiation_raion=ifelse(radiation_raion=="no",0,1)
           ,railroad_terminal_raion=ifelse(railroad_terminal_raion=="no",0,1)
           ,big_market_raion=ifelse(big_market_raion=="no",0,1)
           ,nuclear_reactor_raion=ifelse(nuclear_reactor_raion=="no",0,1)
           ,detention_facility_raion=ifelse(detention_facility_raion=="no",0,1)
           ,culture_objects_top_25=ifelse(culture_objects_top_25=="no",0,1)
           ,water_1line=ifelse(water_1line=="no",0,1)
           ,big_road1_1line=ifelse(big_road1_1line=="no",0,1)
           ,railroad_1line=ifelse(railroad_1line=="no",0,1)
)]


# Date features
data[,timestamp := as.Date(timestamp)]
data[,":="(date_year=year(timestamp))]

# Count NA's
data[,na_count := rowSums(is.na(data))]

# Some relative features
data[,":="(rel_floor = floor/max_floor
           ,diff_floor = max_floor-floor
           ,rel_kitchen_sq = kitch_sq/full_sq
           ,rel_life_sq = life_sq/full_sq
           ,rel_kitchen_life = kitch_sq/life_sq
           ,rel_sq_per_floor = full_sq/floor
           ,diff_life_sq = full_sq-life_sq
           ,building_age = date_year - build_year
)]

# Load macro data
cat("Load macro data")
data.macro = data.macro[,.(timestamp,balance_trade,balance_trade_growth,eurrub,
                           average_provision_of_build_contract,micex_rgbi_tr,
                           micex_cbi_tr, deposits_rate, mortgage_value, mortgage_rate,
                           income_per_cap, museum_visitis_per_100_cap,cpi,
                           apartment_build)]

data.macro[,timestamp := as.Date(timestamp)]
data.macro <- sapply(data.macro,as.numeric) # Simply cast to numeric here, we can do feature engineering later
data <- merge(data, data.macro, by="timestamp", all.x=TRUE);

#exclude these variables
varnames <- setdiff(colnames(data), c("young_male", "school_education_centers_top_20_raion", "0_17_female", "railroad_1line", "7_14_female", "0_17_all", "children_school",
                                      "ecology", "16_29_male", "mosque_count_3000", "female_f", "church_count_1000", "railroad_terminal_raion",
                                      "mosque_count_5000", "big_road1_1line", "mosque_count_1000", "7_14_male", "0_6_female", "oil_chemistry_raion",
                                      "young_all", "0_17_male", "ID_bus_terminal", "university_top_20_raion", "mosque_count_500","ID_big_road1",
                                      "ID_railroad_terminal", "ID_railroad_station_walk", "ID_big_road2", "ID_metro", "ID_railroad_station_avto",
                                      "0_13_all", "mosque_count_2000", "work_male", "16_29_all", "young_female", "work_female", "0_13_female",
                                      "ekder_female", "7_14_all", "big_church_count_500",
                                      "leisure_count_500", "cafe_sum_1500_max_price_avg", "leisure_count_2000",
                                      "office_count_500", "male_f", "nuclear_reactor_raion", "0_6_male", "church_count_500", "build_count_before_1920",
                                      "thermal_power_plant_raion", "cafe_count_2000_na_price", "cafe_count_500_price_high",
                                      "market_count_2000", "museum_visitis_per_100_cap", "trc_count_500", "market_count_1000", "work_all", "additional_education_raion",
                                      "build_count_slag", "leisure_count_1000", "0_13_male", "office_raion",
                                      "raion_build_count_with_builddate_info", "market_count_3000", "ekder_all", "trc_count_1000", "build_count_1946-1970",
                                      "office_count_1500", "cafe_count_1500_na_price", "big_church_count_5000", "big_church_count_1000", "build_count_foam",
                                      "church_count_1500", "church_count_3000", "leisure_count_1500",
                                      "16_29_female", "build_count_after_1995", "cafe_avg_price_1500", "office_sqm_1000", "cafe_avg_price_5000", "cafe_avg_price_2000",
                                      "big_church_count_1500", "full_all", "cafe_sum_5000_min_price_avg",
                                      "office_sqm_2000", "church_count_5000","0_6_all", "detention_facility_raion", "cafe_avg_price_3000"
                                      
))

features = colnames(data)
for (f in features){
  if( (class(data[[f]]) == "character") || (class(data[[f]]) == "factor"))
  {
    levels = unique(data[[f]])
    data[[f]] = as.numeric(factor(data[[f]], level = levels)) 
  }
}


#split train and test
tr = data[price_doc!=-1,varnames,with=FALSE]
ts = data[price_doc==-1,varnames,with=FALSE]

tr[,logSales:=price_doc]

# split data

set.seed(1)
preds <- list()
n_folds = 5
flds <- createFolds(1:nrow(tr), k = n_folds)

# initialize h2o
h2o.init(nthreads=-1,max_mem_size='6G')
testHex = as.h2o(ts)

# define the features
features<-colnames(tr)[!(colnames(tr) %in% c("id","price_doc","logSales", "timestamp"))]

# training function

dnn.train.am <- function (tr.1, tr.eva.1, it) {
  
  valid_loss = tr.eva.1$logSales ## validation set log price_doc
  
  trainHex = as.h2o(tr.1)
  train_valid_Hex = as.h2o(tr.eva.1)
  
  print(paste("[", it, "] training dnn begin ",sep=""," : ",Sys.time()))
  dl <- h2o.deeplearning(x = features
                         ,y = "logSales" 
                         ,hidden = c(300,300,300)
                         ,seed = it
                         #,reproducible = TRUE
                         ,epochs = 2
                         ,variable_importances = TRUE
                         ,training_frame=trainHex
                         ,validation_frame=train_valid_Hex
                         #,nfolds = 2
  )
  print(dl)
  val_pred_dnn = as.matrix(predict(dl, train_valid_Hex))# predict loss for the validation set
  
  pred <- as.matrix(predict(dl, testHex))
  
  pred
}



#-------------------------------------------- run and get  ----------------------------------

for (i in 1:n_folds) {
  
  preds[[i]] <- dnn.train.am(tr.1 = tr[-flds[i][[1]], ]
                             , tr.eva.1 = tr[flds[i][[1]], ]
                             , it = i
  )  
}

# average

preds.t <- as.data.table((preds))
preds.t[, price_doc := rowMeans(.SD)]


dnn.sub <- data.table(id = ts$id, price_doc = expm1(preds.t[, price_doc]))
write.csv(dnn.sub, paste("dnn.upload_",as.character(Sys.Date()),"_1.csv",sep=""), row.names=FALSE, quote=FALSE)