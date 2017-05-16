setwd('/home/lyan/Documents/sber_housing')

library(ggplot2)
library(reshape2)
library(corrgram)
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

train = read.csv('train.csv')
test = read.csv('test.csv')

train$timestamp = as.Date(train$timestamp, format='%Y-%m-%d')

col_names = colnames(train)
col_names_len = length(colnames(train))

train[is.na(train)] = NaN

sub_train = train[,col_names[1:50]]
corrplot(cor(sub_train[sapply(sub_train, is.numeric)]), method='circle')

# macro parameters to square meter price connection
# 

# corr analysis

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
high_cor_feat = col_names[(which(as.vector(res) == TRUE))]

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
make_xgb_predict = function(features) {
  split_factor = .7
  
  dt = sample(nrow(train), nrow(train) * split_factor)
  train_set = train[dt,]
  test_set = train[-dt,]
  
  xgb_train = xgb.DMatrix(data = data.matrix(sapply(train_set[, features], as.numeric)), label =
                            data.matrix(train_set[, 'price_doc']))
  xgb <- xgboost(
    data = xgb_train,
    eta = 0.9,
    max_depth = 500,
    nround = 100,
    subsample = 0.5,
    colsample_bytree = 0.5,
    seed = 1,
    eval_metric = "rmse",
    objective = "reg:linear",
    nthread = 4
  )
  
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

#xbgoost on expert selected features

expert_features = c('life_sq', 'floor', 'max_floor', 'build_year', 'num_room', 'state', 'material', 'raion_popul')
make_xgb_predict(expert_features)
make_xgb_predict((col_names[3:col_names_len-1]))

# end

ggplot(train, aes(x=state, y=price_doc)) + geom_jitter()

price_to_all = cor(sub_train$price_doc, train[sapply(sub_train, is.numeric)])
corrplot(price_to_all)
#format(as.Date(timestamp, format='%Y-%m-%d'), '%Y')

ggplot(train, aes(x = timestamp , y = price_doc))  +
  geom_line() +
  ggtitle("Price_doc density plot")

ggplot(train, aes(x=floor, y=price_doc)) + geom_line() + geom_smooth(color='red') + ggtitle('floor vs price')
ggplot(train, aes(x=fitness_km, y=price_doc)) + geom_line() + geom_smooth(color='red')
ggplot(train, aes(x=metro_km_walk, y=price_doc)) + geom_jitter() + geom_smooth(color='red') + ggtitle('metro_walk_min')

ggplot(train, aes(x=build_year, y=price_doc)) + geom_line() + geom_smooth(color='green') + ggtitle('build year vs price')
train[train$build_year < 1600] = NaN
train[train$build_year > 2020] = NaN
# wtf

get_quantiles = function(ser){
  quant = data.frame(quantile(train$area_m, probs = c(.25, .50,.75)))
  return(quant)
}

ggplot(train, aes(x=indust_part, y=price_doc)) + geom_smooth()
ggplot(train, aes(x=area_m, y=price_doc)) + geom_jitter() + geom_smooth(colour='green') + 
  geom_vline(data=quant, aes(xintercept=quant, color='red'))

quant = data.frame(quantile(train$full_sq, probs = c(.001, 0.15, .50,.70,.80,.90,.95, .99, .9995)))
ggplot(data=train, aes(x = full_sq, y = price_doc)) + 
  geom_line() + geom_vline(data=data.frame(quant), aes(xintercept=quant, color='green'))

ggplot(data=train[train$life_sq < 2000,], aes(x=life_sq, y=price_doc)) + geom_line() + 
  geom_vline(data=data.frame(quant), aes(xintercept=quant, colour='green'))

ggplot(train[train$full_sq < 240 && train$full_sq > 10,], aes(x=full_sq, y=price_doc))+ geom_jitter() + ggtitle('life square vs price')
ggplot(train, aes(x=life_sq, y=price_doc))+ geom_jitter() + ggtitle('life square vs price')

train[train$full_sq > 2000,]$full_sq = mean(train$full_sq)

ggplot(train, aes(x=build_year, y=price_doc)) + geom_line() + geom_smooth(color='green') + ggtitle('build_year vs price')
#$build_year > 1800 && train$build_year < 2200,]

ggplot(train, aes(x=build_year, y=price_doc)) +
  geom_jitter() + ggtitle('build_year jitter')

ggplot(train[train$price_doc < 60000000,], aes(x=timestamp,y=price_doc, color='red')) + geom_line() + geom_smooth(color='green')


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

# trying to clean up data
library(outliers)

for (n in col_names[3:col_names_len]){
  if (is.numeric(train[,n])){
    train[,n] = rm.outlier(train[,n], fill=TRUE)
  }
}

# adjust build year

adj_build_year = as.numeric(train$build_year)
med_bld_year = median(adj_build_year)
adj_build_year[adj_build_year > 2015 & adj_build_year < 1500] <- NaN

train$adj_build_year = adj_build_year
#end


#average square meter price
#prepare full square
full_sq = train$full_sq
full_sq[full_sq > 400] = median(full_sq[full_sq<400])

sq_meter_price  = train$price_doc / full_sq
sq_meter_price[sq_meter_price > 1e06] = median(sq_meter_price[sq_meter_price < 1e07])

train$adj_full_sq = full_sq
train$sq_meter_price = sq_meter_price
# sq meter price is ready

# adjusting number of room
adj_num_room = train$num_room
adj_num_room[adj_num_room > 10] = median(adj_num_room[adj_num_room <10])
adj_num_room[adj_num_room < 1] = median(adj_num_room)
train$adj_num_room = adj_num_room

ggplot(train, aes(x=adj_num_room, y=sq_meter_price)) + geom_jitter()
#end

# adj kitchen
adj_kitch_sq = train$kitch_sq
adj_kitch_sq[adj_kitch_sq > 100] = median(adj_kitch_sq)
train$adj_kitch_sq = adj_kitch_sq

ggplot(train, aes(x=timestamp, y=adj_kitch_sq)) + geom_jitter()
#end

#adj sub area
sub_area_factors = as.numeric(as.factor(train$sub_area))
train$sub_area_factors = sub_area_factors
ggplot(train, aes(x=sub_area_factors, y=sq_meter_price)) + geom_jitter()
#end

lin_reg = lm(formula = sq_meter_price ~ adj_kitch_sq+ adj_num_room + adj_full_sq + sub_area_factors + build_year, data=train)

test_vars = data.frame(test$kitch_sq, test$num_room, test$full_sq, as.numeric(as.factor(test$sub_area)), test$build_year)
colnames(test_vars) <- c('adj_kitch_sq', 'adj_num_room', 'adj_full_sq', 'sub_area_factors', 'build_year')

lg_pred_sq = predict(lin_reg, test_vars)
lg_pred_sq[is.na(lg_pred_sq)] <- median(lg_pred_sq[!is.na(lg_pred_sq)])
lg_pred = lg_pred_sq * test$full_sq
res = data.frame(test$id, lg_pred)
colnames(res) = c('id', 'price_doc')

write.csv(res, col.names = FALSE, row.names = FALSE, file = 'subm.csv', quote = FALSE)