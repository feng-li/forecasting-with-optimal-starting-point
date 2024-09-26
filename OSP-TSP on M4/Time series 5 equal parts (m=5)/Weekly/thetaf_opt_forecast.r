library("xgboost")
library("Matrix")
library('Ckmeans.1d.dp')
library('lightgbm')


time_matrix <- matrix(0,ncol = 3, nrow =4)
colnames(time_matrix) <- c("user_time", "system_time", "elapsed_time")
start_time = Sys.time()

data=read.csv('Weekly_features_matrix.csv')
data=data[,2:(dim(data)[2]-6)]

dim(data)

head(data)

dlist= load('week_thetaf_datalist.RData')
datalist=eval(parse(text = dlist ))
res=datalist[[1]]
MASE=res[,,,6]
m=5

dim(MASE)

whichmin<-function(x){
    minx=min(x[x>0])
    loc=which(x==minx)[1]-1
    loc
}

meanunique=function(x)
    {
    mean(unique(x))
}

nanum=c()
for(i in seq(1,dim(MASE)[1]))
    {
    count=MASE[i,,]
    a=apply(count,1,min)
    if(sum(is.na(a))>0)
        {
        nanum=append(nanum,i)
    }
    }

nanum

realbestmin=matrix(0,dim(MASE)[1],1)
for(i in seq(1,dim(MASE)[1]))
    {
    count=MASE[i,,]
    min_value=apply(count,1,min)
    if (max(min_value,na.rm = TRUE)==0){
        realbestmin[i,]=0
        }
    else
        {
        min_value[is.na(min_value)]=100 
        realbestmin[i,]= whichmin(min_value)
    }
    }



table(realbestmin)



realbestmean=matrix(0,dim(MASE)[1],1)
for(i in seq(1,dim(MASE)[1]))
    {
    count=MASE[i,,]
    mean_value=apply(count,1,meanunique)
    if (max(mean_value,na.rm = TRUE)==0){
        realbestmean[i,]=0
        }
    else
        {
        mean_value[is.na(mean_value)]=100 
        realbestmean[i,]= whichmin(mean_value)
    }
    }

realbestmean

head(data)



set.seed(100)
index = sample(2,nrow(data),replace = TRUE,prob=c(0.7,0.3))

train_data=data[index==1,]
test_data=data[index==2,]
train_label_min=realbestmin[index==1,]
test_label_min=realbestmin[index==2,]
train_label_mean=realbestmean[index==1,]
test_label_mean=realbestmean[index==2,]

head(train_data)

end_time = Sys.time()

time_matrix[1,]=end_time-start_time

start_time = Sys.time()

dtrain_xg_min_reg <- xgb.DMatrix(data = as.matrix(train_data),label = as.matrix(train_label_min)) 
dtrain_xg_min_cl <- xgb.DMatrix(data = as.matrix(train_data),label = as.matrix(as.factor(train_label_min)) )

dtrain_lg_min_reg <- lgb.Dataset(data = as.matrix(train_data),label = as.matrix(train_label_min))
dtrain_lg_min_cl <- lgb.Dataset(data = as.matrix(train_data),label = as.matrix(as.factor(train_label_min)))



xgb_min_reg <- xgboost(data = dtrain_xg_min_reg, nround=100)

xgb_min_cl <- xgboost(data = dtrain_xg_min_cl, nround=100, objective='multi:softmax',num_class=5)

lgb_min_reg <- lgb.train(data = dtrain_lg_min_reg, nrounds = 100)

params <- list(objective = "multiclass",
               num_class = 5, 
               metric = "multi_logloss")
lgb_min_cl <- lgb.train(data = dtrain_lg_min_cl,nrounds = 100,params=params)

end_time = Sys.time()
time_matrix[2,]=end_time-start_time

start_time = Sys.time()

dtrain_xg_mean_reg <- xgb.DMatrix(data = as.matrix(train_data),label = as.matrix(train_label_mean)) 
dtrain_xg_mean_cl <- xgb.DMatrix(data = as.matrix(train_data),label = as.matrix(as.factor(train_label_mean)) )

dtrain_lg_mean_reg <- lgb.Dataset(data = as.matrix(train_data),label = as.matrix(train_label_mean))
dtrain_lg_mean_cl <- lgb.Dataset(data = as.matrix(train_data),label = as.matrix(as.factor(train_label_mean)))

xgb_mean_reg <- xgboost(data = dtrain_xg_mean_reg, nround=100)

xgb_mean_cl <- xgboost(data = dtrain_xg_mean_cl, nround=100, objective='multi:softmax',num_class=5)

lgb_mean_reg <- lgb.train(data = dtrain_lg_mean_reg,nrounds = 100)

params <- list(objective = "multiclass",
               num_class = 5, 
               metric = "multi_logloss")
lgb_mean_cl <- lgb.train(data = dtrain_lg_mean_cl,params=params,nrounds = 100)

end_time = Sys.time()
time_matrix[3,]=end_time-start_time







start_time = Sys.time()

alldatalgb <- lgb.Dataset(data = as.matrix(data))
alldataxgb <- xgb.DMatrix(data = as.matrix(data))


xgbregmin=predict(xgb_min_reg,alldataxgb)
xgbclsmin=predict(xgb_min_cl,alldataxgb)
lgbregmin=predict(lgb_min_reg,as.matrix(data))
lgbclsmin=predict(lgb_min_cl,as.matrix(data))

xgbregmin

xgbclsmin

lgbregmin

lgbclsmin

datalength=dim(data)[1]
lgbclsm=matrix(lgbclsmin,m,datalength)
lgbclsminr=matrix(0,datalength,1)
for(i in 1:datalength)
    {
    lgbclsminr[i,]=which.max(lgbclsm[,i])
}
lgbclsminr=lgbclsminr-1

lgbclsminr

xgbregmean=predict(xgb_mean_reg,alldataxgb)
xgbclsmean=predict(xgb_mean_cl,alldataxgb)
lgbregmean=predict(lgb_mean_reg,as.matrix(data))
lgbclsmean=predict(lgb_mean_cl,as.matrix(data))

lgbclsm=matrix(lgbclsmean,m,datalength)
lgbclsmeanr=matrix(0,datalength,1)
for(i in 1:datalength)
    {
    lgbclsmeanr[i,]=which.max(lgbclsm[,i])
}
lgbclsmeanr=lgbclsmeanr-1



preallmin=cbind(xgbclsmin,xgbregmin)
preallmin=cbind(preallmin,lgbclsminr)
preallmin=cbind(preallmin,lgbregmin)

colnames(preallmin)=c('xgbclsmin','xgbregmin','lgbclsmin','lgbregmin')
head(preallmin)

preallmean=cbind(xgbclsmean,xgbregmean)
preallmean=cbind(preallmean,lgbclsmeanr)
preallmean=cbind(preallmean,lgbregmean)

head(preallmean)

colnames(preallmean)=c('xgbclsmean','xgbregmean','lgbclsmean','lgbregmean')
head(preallmean)

end_time = Sys.time()
time_matrix[4,]=end_time-start_time

result_list=list(preallmin,preallmean,time_matrix)

save(result_list, file = "weekly_thetaf_opt_pre_result.RData")

time_matrix

importance_matrix1 <- xgb.importance(model = xgb_mean_cl)
importance_matrix2 <- xgb.importance(model = xgb_mean_reg)
importance_matrix3 <- lgb.importance(lgb_mean_cl)
importance_matrix4 <- lgb.importance(lgb_mean_reg)

importance_matrix1=importance_matrix1[order(importance_matrix1$Feature),]
importance_matrix2=importance_matrix2[order(importance_matrix2$Feature),]
importance_matrix3=importance_matrix3[order(importance_matrix3$Feature),]
importance_matrix4=importance_matrix4[order(importance_matrix4$Feature),]

importance_matrix_mean=(importance_matrix1[,2:4]+importance_matrix2[,2:4]+importance_matrix3[,2:4]+importance_matrix4[,2:4])/4

importance=cbind(importance_matrix1[,1],importance_matrix_mean)

write.csv(importance,'w_thetaf_imp_mean.csv')










