#MH_Restaurant Prices

#Restaurant Cost Prediction
#install.packages("xlsx")
library(xlsx)
library(reshape2)

train <- readxl::read_xlsx("F:/Data Competitions/Participants_Data_Final_MH/Participants_Data_Final_MH/Participants_Data_Final/Data_Train.xlsx")
test <- readxl::read_xlsx("F:/Data Competitions/Participants_Data_Final_MH/Participants_Data_Final_MH/Participants_Data_Final/Data_Test.xlsx")
sample <- readxl::read_xlsx("F:/Data Competitions/Participants_Data_Final_MH/Participants_Data_Final_MH/Participants_Data_Final/Sample_submission.xlsx")

dim(train)
head(train)
colnames(train)

all_cuisines <- str_split_fixed(train$CUISINES, ",",10)%>%as.data.frame()
colnames(all_cuisines)

#Getting number of cuisines of each restaurnat
train_1 <- cbind(train, all_cuisines)
a <- train_1%>%
  select(RESTAURANT_ID, CUISINES, 10:19)%>%
  melt(id = c("RESTAURANT_ID","CUISINES"))%>%
  arrange(CUISINES)%>%
  filter(!value == "")

a%>%colnames()
restIDCount <- a%>%
  count(RESTAURANT_ID)

#Added no.of cuisines across restaurants 
train_2 <- left_join(train_1, restIDCount, by = c("RESTAURANT_ID"))%>%dim()

#checking if test data values are same in train
test$TITLE %in% train$TITLE%>%table()

#############################
#Joining test and train and then breaking up later
test$COST <- NA
tr_te <- rbind(train, test)

all_cuisines <- str_split_fixed(tr_te$CUISINES, ",",10)%>%as.data.frame()
tr_te1 <- cbind(tr_te, all_cuisines)

a <- tr_te1%>%
  select(RESTAURANT_ID, CUISINES, 10:19)%>%
  melt(id = c("RESTAURANT_ID","CUISINES"))%>%
  arrange(CUISINES)%>%
  filter(!value == "")

a%>%colnames()
restIDCount <- a%>%
  count(RESTAURANT_ID)

tr_te2 <- left_join(tr_te1, restIDCount, by = c("RESTAURANT_ID"))

#GEtting no. of titles

all_titles <- str_split_fixed(tr_te$TITLE, ",",10)%>%as.data.frame()
tr_te3 <- cbind(tr_te, all_titles)

b <- tr_te3%>%
  select(RESTAURANT_ID, TITLE, 10:19)%>%
  melt(id = c("RESTAURANT_ID","TITLE"))%>%
  arrange(TITLE)%>%
  filter(!value == "")
#filter(RESTAURANT_ID == 13198)

b%>%colnames()
restTitleCount <- b%>%
  count(RESTAURANT_ID, TITLE)

colnames(restTitleCount)[3] <- "NoofTitles"

tr_te4 <- left_join(tr_te2, restTitleCount, by = c("RESTAURANT_ID", "TITLE"))

colnames(tr_te4)[20] <- "NoofCuisines"

colSums(is.na(tr_te4))

#Impute Votes and Ratingss

tr_te4$RATING[is.na(tr_te4$RATING)] <- 3.7844
#mean(as.numeric(tr_te4$RATING),na.rm = TRUE)

regexp <- "[[:digit:]]+"
tr_te4$VOTES <- str_extract(tr_te4$VOTES, regexp)
mean(as.numeric(tr_te4$VOTES), na.rm = TRUE)
tr_te4$VOTES[is.na(tr_te4$VOTES)] <- 410


model_data <- tr_te4%>%select(2, 7,8,20,21, 9)
model_data$RATING[model_data$RATING == "NEW"] <- 3.78

model_data$RATING <- as.numeric(as.character(model_data$RATING))
model_data$VOTES <- as.numeric(as.character(model_data$VOTES))

model_data <- model_data%>%select(-1)

library(xgboost)
library(caret)

tr <- model_data[1:12690, ]
te <- model_data[12691:16921, ]
tr <- as.matrix(tr)
te <- as.matrix(te)

y <- tr$COST

val <- caret::createDataPartition(y, p = 0.2, list = FALSE)
dtrain <- xgb.DMatrix(data = tr[-val, ], label = y[-val])
dval <- xgb.DMatrix(data = tr[val, ], label = y[val])
dtest <- xgb.DMatrix(data = te)

cols <- colnames(model_data[ ,1:4])

p <- list(booster = "gbtree",
          eval_metric = "rmse",
          nthread = 5,
          eta = 0.1,
          max_depth = 6,
          min_child_weight = 100,
          gamma = 0.1,
          subsample = 0.6,
          colsample_bytree = 0.9,
          colsample_bylevel = 0.9,
          alpha = 0,
          lambda = 1)

set.seed(1)
m_xgb <- xgb.train(p, dtrain, 10000, list(val = dval), print_every_n = 10, early_stopping_rounds = 2000)


xgb.importance(cols, model = m_xgb) %>% 
  xgb.plot.importance(top_n = 20) + theme_minimal()


COST = predict(m_xgb, dtest)
as.data.frame(COST)%>%
  write.xlsx(paste0("MH_Rest_Price", round(m_xgb$best_score, 5), ".xlsx"))

##################################
class(tr)

dim(tr)
dim(te)

lmod <- lm(COST ~. , tr)
COST <- predict(lmod, te)
COST%>%
  write.xlsx(paste0("MH_Rest_Price_lmodel", ".xlsx"))



train%>%
  count(RESTAURANT_ID)%>%
  arrange(desc(n))

colSums(is.na(train))

train%>%
  select(RESTAURANT_ID, CITY, LOCALITY)%>%
  filter(!is.na(CITY))%>%
  filter(!is.na(LOCALITY))

train%>%
  distinct(tolower(CITY))

train%>%
  distinct(tolower(LOCALITY))

colSums(is.na(test))

#Initial Submission
sample$COST <- 670
sample%>%
  write.xlsx("F:/Data Competitions/Participants_Data_Final_MH/Participants_Data_Final_MH/Participants_Data_FinalRes1.xlsx")
#########
train_t <- train
train_t <- train_t%>%
  filter(!is.na(CITY))%>%
  filter(!is.na(LOCALITY))%>%
  filter(!is.na(RATING))%>%
  filter(!is.na(VOTES))

colSums(is.nan(train_t))

#Running a random Forest

library(randomForest)
library(caret)
library(party)
dt <- ctree(COST ~ . , train_t)

train_t%>%glimpse()

##############################################3

#New Approach with uunest, imputation and weightages

finaldata(df){
  #Break down cuisines to rows
  train%>%
    mutate(all_cui = )
  #Break down titles to rows
  
  #Extract number from Votes
  
  
  #Impute using mice
  #rleid the city and locality and then use mice with other additional columns
  
  
  
  
  
  #Check the cuts of Cost and Votes
  
  
  
  #Assign weightages to the ratings
  
  
  
  
  
  
  
}


library(DataExplorer)
train%>%
  create_report()

train%>%
  mutate(cuts = as.factor(cut(COST, breaks = c(0,100,250,500,1000,5000,15000))))


# Test --------------------------------------------------------------------

#Giving a Route number
train <- train%>%
  mutate(routenum = group_indices(train, Route))

#Splitting the Routes into Citywise
route_splits <- str_split_fixed(train$Route, "??? ", 6)%>%as.data.frame()
t1 <- cbind(train$routenum, route_splits)
colnames(t1)[1] <- c("Routenum") 

#Assigning a num to each city and getting back in req table format
route_brkup <- t1%>%
  mutate(pid = row.names(t1))%>%
  select(pid, everything())%>%
  melt(id.vars = c("pid","Routenum"))%>%
  arrange(pid)%>%
  mutate(value = str_trim(value))%>%
  mutate(citylabel = group_indices(.,value))%>%
  filter(!value == "")%>%
  select(pid, Routenum, variable, citylabel)%>%
  dcast(pid ~ variable, value.var = "citylabel")

pidvdrnum <- t1%>%
  mutate(pid = row.names(t1))%>%
  select(pid, everything())%>%
  melt(id.vars = c("pid","Routenum"))%>%
  arrange(pid)%>%
  mutate(value = str_trim(value))%>%
  mutate(citylabel = group_indices(.,value))%>%
  filter(!value == "")%>%
  select(pid, Routenum, variable, citylabel)%>%
  distinct(pid, Routenum)

route_brkup <- route_brkup%>%
  left_join(pidvdrnum)%>%
  select(-pid)

colnames(route_brkup)[7] <- "routenum"
route_brkup <- route_brkup%>%
  distinct()

#Joining that data to train data
train <- train%>%
  left_join(route_brkup)

