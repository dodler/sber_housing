setwd('C:/Users/SBT-Lyan-AI/Documents/sber_housing')

library(ggplot2)
library(reshape2)
library(corrplot)
library(dplyr)


# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(plots, plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  #plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
    ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
    layout.pos.col = matchidx$col))
    }
  }
}

prepare_data = function(inp){
  res = data.frame(inp)
  
  res$timestamp = as.Date(res$timestamp, format='%Y-%m-%d')
  res$sub_area_factors = as.factor(res$sub_area)
  res = res %>%
    group_by(sub_area) %>%
    summarise(trc_metro = mean(trc_count_1000 / metro_min_walk),
              trc_mkad = mean(trc_count_1000 / mkad_km),
              trc_ts = mean(trc_count_1000 / ts_km),
              trc_ts = mean(trc_count_1000 / ts_km),
              m_vs_f = mean(female_f / male_f),
              new_bld_cnt = mean(build_count_after_1995),
              prom_grn_500 = mean(prom_part_500 / green_part_500),
              mkad_to_sad = mean(sadovoe_km / mkad_km),
              child_ratio = mean(preschool_km),
              prom_grn_1500 = mean(prom_part_1500 / green_part_500),
              young_to_work = mean(young_all / work_all),
              young_to_old = mean(young_all / ekder_all),
              work_to_ekder = mean(work_all / ekder_all)
              ) %>%
    right_join(res, by='sub_area')
  
  #
  #leisure_ratio = mean(market_shop_km * swim_pool_km * stadium_km * fitness_km),
  #prom_ratio = mean(radiation_km * oil_chemistry_km * nuclear_reactor_km * power_transmission_line_km * thermal_power_plant_km),
  #zd_ratio = mean(railroad_km * zd_vokzaly_avto_km),
  #study_ratio = mean(school_km * university_km),
  #religio_ratio = mean(church_synagogue_km * big_church_km * big_church_count_500 * church_count_500),
  #work_ratio = mean(office_sqm_3000 * office_sqm_1500 * office_sqm_1000 * office_sqm_500 * workplaces_km * office_km),
  
  return(res)
}

# investigate material

train = read.csv('train.csv')
test = read.csv('test.csv')

train = prepare_data(train)
test = prepare_data(test)

# price district ratios


train = train %>%
  group_by(sub_area) %>%
  summarise(dist_sq_prc = mean(price_doc / full_sq),
            dist_sq_prc_to_metro_km = mean(dist_sq_prc / metro_min_walk),
            dist_sq_prc_to_kremlin = mean(dist_sq_prc / kremlin_km),
            dist_sq_prc_to_mkad_km = mean(dist_sq_prc / mkad_km),
            dist_sq_prc_to_sad = mean(dist_sq_prc / sadovoe_km),
            dist_sq_prc_to_cmtr = mean(dist_sq_prc / cemetery_km),
            dist_sq_prc_to_water = mean(dist_sq_prc / water_km),
            dist_sq_prc_to_railroad =mean(dist_sq_prc / railroad_km),
            dist_sq_prc_to_statio = mean(dist_sq_prc / railroad_station_avto_km)
            ) %>%
  right_join(train, by='sub_area')

test = train %>%
  group_by(sub_area) %>%
  summarise(dist_sq_prc = mean(price_doc / full_sq),
            dist_sq_prc_to_metro_km = mean(dist_sq_prc / metro_min_walk),
            dist_sq_prc_to_kremlin = mean(dist_sq_prc / kremlin_km),
            dist_sq_prc_to_mkad_km = mean(dist_sq_prc / mkad_km),
            dist_sq_prc_to_sad = mean(dist_sq_prc / sadovoe_km),
            dist_sq_prc_to_cmtr = mean(dist_sq_prc / cemetery_km),
            dist_sq_prc_to_water = mean(dist_sq_prc / water_km),
            dist_sq_prc_to_railroad =mean(dist_sq_prc / railroad_km),
            dist_sq_prc_to_statio = mean(dist_sq_prc / railroad_station_avto_km)
            ) %>%
  right_join(test, by='sub_area')
#end

library(caret)
nums = sapply(train, is.numeric)
cor_mtr = cor(train[,nums])
cor_mtr[is.nan(cor_mtr)] <- 0
cor_mtr[is.na(cor_mtr)] <- 0
high_corr = findCorrelation(cor_mtr)
train = train[,-high_corr]
test = test[,-high_corr]


# moving high corr features

#end

col_names = colnames(train)
col_names_len = length(colnames(train))

features = col_names[col_names != 'price_doc' & col_names != 'id']

make_xgb_predict(features)

#sub_train = train[,col_names[1:50]]
#corrplot(cor(sub_train[sapply(sub_train, is.numeric)]), method='circle')


CORR_THRES = 0.15

filter = function(t){
  if (!is.numeric(train[,t])){
    return(FALSE)
  }
  corr = cor(train[,'price_doc'], train[,t])
  return(corr > CORR_THRES && corr < 0.99)
}

res = sapply(col_names, filter)
col_names[(which(as.vector(res) == TRUE))]

# end

# high corr plot
plot_density = function(t){
  p = ggplot(train, aes_string(x='id', y=t)) + geom_line()
  return(p)
}

plots = lapply(high_cor_feat, plot_density)

multiplot(plots, cols=2)

#end

#high corr model

library(xgboost)
library(Metrics)
library(DiagrammeR)
make_xgb_predict = function(features) {
  split_factor = .75
  
  dt = sample(nrow(train), nrow(train) * split_factor)
  train_set = train[dt,]
  test_set = train[-dt,]
  
  xgb_train = xgb.DMatrix(data = data.matrix(sapply(train_set[, features], as.numeric)), label =
           data.matrix(train_set[, 'price_doc']))
  xgb <- xgboost(
    data = xgb_train,
    eta = 0.0075,
    max_depth = 350,
    nround = 350,
    subsample = 0.7,
    colsample_bytree = 0.7,
    seed = 1,
    eval_metric = "rmse",
    objective = "reg:linear",
    nthread = 4
  )
  
  #xgb.plot.tree(feature_names = features, model=xgb)
  
  eval_test = xgb.DMatrix(data = data.matrix(sapply(test_set[, features], as.numeric)))
  eval_labels = abs(predict(xgb, eval_test))
  print(rmsle(eval_labels, sapply(test_set$price_doc,as.numeric)))

  xgb_test = xgb.DMatrix(data = data.matrix(sapply(test[, features], as.numeric)))
  pred = predict(xgb, xgb_test)
  pred = abs(pred)
  
  output = data.frame(cbind(test$id, pred))
  output.colnames = c('id', 'price_doc')
  
  write.table(
    x = output,
    'subm.csv',
    sep = ',',
    row.names = FALSE,
    col.names = c('id', 'price_doc'),
    quote = FALSE
  )
  return(pred)
}

make_xgb_predict(high_cor_feat)
#end

# end

ggplot(train, aes(x=state, y=price_doc)) + geom_jitter()

price_to_all = cor(sub_train$price_doc, train[sapply(sub_train, is.numeric)])
corrplot(price_to_all)
#format(as.Date(timestamp, format='%Y-%m-%d'), '%Y')

ggplot(train, aes(x = timestamp , y = price_doc))  +
  geom_line() +
  ggtitle("Price_doc density plot")

get_quantiles = function(ser){
  quant = data.frame(quantile(train$area_m, probs = c(.25, .50,.75)))
  return(quant)
}

# macro investigation
# here x and y are column names
plot_line = function(data, x_col,y_col){
  return(ggplot(data=data, aes_string(x=x_col, y=y_col)) + geom_line() + geom_smooth(colour='red') + labs(x=x_col, y=y_col))
}

macro = read.csv('macro.csv')
macro_col_names = colnames(macro)
macro$timestamp =  as.Date(macro$timestamp, format='%Y-%m-%d')
#plot_line(macro, macro_col_names[1], macro_col_names[4])
plots = lapply(macro_col_names[51:100], function(t) plot_line(macro, macro_col_names[1], t))
multiplot(plots, cols=10)
# end

get_dist_index = function(x,y){
  col_x = 1:length(unique(train$sub_area_factors))
  col_y = 1:length(unique(train$sub_area_factors))
  n_dist = 1:length(unique(train$sub_area_factors))
  for(i in seq(1, length(train$sq_meter_price))){
    col_x[train$sub_area_factors[i]] = col_x[train$sub_area_factors[i]] + train[i,x]
    col_y[train$sub_area_factors[i]] = col_y[train$sub_area_factors[i]] + train[i,y]
    n_dist[train$sub_area_factors[i]] = n_dist[train$sub_area_factors[i]] + 1
  }
  
  avg_x = col_x / n_dist
  avg_y = col_y / n_dist
  
  return (avg_x / avg_y)
}

# different indexes by districts
par(mfrow=c(3,3))
plot(get_dist_index('price_doc','raion_popul'))
plot(get_dist_index('price_doc','mosque_count_1000'))
plot(get_dist_index('price_doc','bulvar_ring_km'))
plot(get_dist_index('price_doc','fitness_km'))
plot(get_dist_index('price_doc','industrial_km'))
plot(get_dist_index('price_doc','mkad_km'))
plot(get_dist_index('price_doc','water_km'))
plot(get_dist_index('price_doc','metro_km_walk'))
plot(get_dist_index('price_doc','university_km'))

library(outliers)

for(n in col_names[2:col_names_len-1]){
  if(is.numeric(train[,n])){
    train[,n] = rm.outlier(train[,n], fill = TRUE)
  }
}

# vars collections
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
