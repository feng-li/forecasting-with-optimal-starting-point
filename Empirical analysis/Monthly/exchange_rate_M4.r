library(tsfeatures)
library(imputeTS)
library(forecast)
library(dplyr)



data=read.csv('exchange_rate.csv')

head(data)

tail(data)

data=data[3:276,c(2:dim(data)[2])]



dim(data)

which_baddata=function(data){
baddata=c()
for(i in 1:dim(data)[2])
{loc=min(which(is.na(data[,i])==FALSE))-1
if((dim(data)[1]-loc)<40)
    {
    baddata=append(baddata,i)
}
if(sum(is.na(data[loc:dim(data)[1],i])==TRUE)>10)
 {
     baddata=append(baddata,i)
 }
 if(sum(is.na(data[(dim(data)[1]-11):dim(data)[1],i])==TRUE)>3)
 {
     baddata=append(baddata,i)
 }
}
baddata=unique(baddata)
return(baddata)}

baddata=which_baddata(data)

baddata

data=data[,-1*baddata]



data

freq=12
ll=dim(data)[2]
hh=12
start=c(2001,1)
m=5
n=4

process_data <- function(data, start, freq, hh, ll) {
  data_ts <- list()
  data_tstrain <- list()
  data_features <- NULL
  
  for (i in 1:ll) {
    # 寻找第一个非NA值的位置
    loc <- min(which(is.na(data[, i]) == FALSE)) - 1
    zc <- loc %/% freq
    yu <- loc %% freq
    
    # 提取子序列
    x <- data[(loc + 1):dim(data)[1], i]
    
    # 检测并处理NA值
    r <- which(is.na(x) == TRUE)
    if (length(r) != 0) {
      x <- na_kalman(x) # 假设na_kalman是一个处理NA值的函数
    }
    
    # 创建时间序列对象
    da <- ts(x[1:(length(x) - hh)], start = c(start[1] + zc, start[2] + yu), frequency = freq)
    da1 <- ts(x, start = c(start[1] + zc, start[2] + yu), frequency = freq)
    
    # 保存时间序列对象
    data_ts[[i]] <- da1
    data_tstrain[[i]] <- da
    
    # 提取时间序列特征
    f <- tsfeatures(da)
    if (is.null(data_features)) {
      data_features <- f
    } else {
      data_features <- rbind(data_features, f)
    }
  }
  
  # 函数返回一个列表，包含处理后的时间序列和特征
  list(time_series = data_ts, time_series_train = data_tstrain, features = data_features)
}


processed_data <- process_data(data, start = c(1995, 1), freq, hh, ll)

data_ts=processed_data$time_series
data_tstrain=processed_data$time_series_train
data_features=processed_data$features



head(data_features)

length=matrix(0,ll,1)
for( i in 1:ll)
    {
    length[i,1]=length(data_tstrain[[i]])
}

data_features=cbind(data_features,length)



head(data_features)











f1=function(x){
    if( x%%1>=0.5)
        {
        return(as.integer(x)+1)
    }
    else{return(as.integer(x))}
}

forecast_and_evaluate <- function(data_ts, data_tstrain, m,n,hh, ll) {
  predets <- array(0, dim = c(ll, m, n, 6))
  predhets <- array(0, dim = c(ll, m, n, hh))
  predthetaf <- array(0, dim = c(ll, m, n, 6))
  predhthetaf <- array(0, dim = c(ll, m, n, hh))
  yreal <- list()
  
  for (k in 1:ll) {
    y <- data_tstrain[[k]]
    yall <- data_ts[[k]]
    y_pred <- ts(yall[(length(yall) - hh + 1):length(yall)], end = end(yall), frequency = frequency(yall))
    y_l=length(y)
    loc = 1:length(y)
    loc_m = as.integer(loc*m/length(y))
    filt_d = data.frame(y,loc_m)
    filt_0=filter(filt_d ,loc_m==0)
    m_l=count(filt_0)
    n_l=m_l/n
    m_l=as.integer(m_l)
    n_l=as.numeric(n_l)
    
    yreal[[k]] <- y_pred
    
    for (i in 0:(m - 1)) {
      for (j in 0:(n-1)) {
        Y <- ts(y[round((i*m_l+j*n_l)+1):y_l], end = end(y), frequency = frequency(yall))
        M <- ets(Y)
        pd <- forecast(M, h = hh)
        predhets[k, (i + 1), (j + 1), ] <- pd$mean
        predets[k, (i + 1), (j + 1), ] <- accuracy(pd, y_pred)[2, 1:6]
        
        M <- thetaf(Y, h = hh)
        pd <- forecast(M, h = hh)
        predhthetaf[k, (i + 1), (j + 1), ] <- pd$mean
        predthetaf[k, (i + 1), (j + 1), ] <- accuracy(pd, y_pred)[2, 1:6]
      }
    }
  }
  
  list(
    pred_ets = list(predhets = predhets, predets = predets),
    pred_thetaf = list(predhthetaf = predhthetaf, predthetaf = predthetaf),
    y_real = yreal
  )
}

forecast_list=forecast_and_evaluate(data_ts, data_tstrain, m,n,hh, ll)

predhets=forecast_list$pred_ets$predhets
predets=forecast_list$pred_ets$predets
predhthetaf=forecast_list$pred_thetaf$predhthetaf
predthetaf=forecast_list$pred_thetaf$predthetaf
y_real=forecast_list$y_real

length5=matrix(0,ll,m)
for(k in 1:ll)
{
y = data_tstrain[[k]]
y_l=length(y)
loc = 1:length(y)
loc_m = as.integer(loc*m/length(y))
filt_d = data.frame(y,loc_m)
filt_0=filter(filt_d ,loc_m==0)
m_l=count(filt_0)
m_l=as.integer(m_l)
length5[k,]=c(0,1*m_l,2*m_l,3*m_l,4*m_l)
    }



library("xgboost")
library("Matrix")
library('Ckmeans.1d.dp')
library(lightgbm)

library(lightgbm)

etsxgbreg <- readRDS("etsxgbreg.rds")
etsxgbcls <- readRDS("etsxgbcls.rds")
etslgbreg <- readRDS("etslgbreg.rds")
etslgbcls <- readRDS("etslgbcls.rds")

thetafxgbreg <- readRDS("thetafxgbreg.rds")
thetafxgbcls <- readRDS("thetafxgbcls.rds")
thetaflgbreg <- readRDS("thetaflgbreg.rds")
thetaflgbcls <- readRDS("thetaflgbcls.rds")





meanunique=function(x)
    {
    mean(unique(x))
}

whichmin<-function(x){
    minx=min(x[x>0])
    loc=which(x==minx)[1]-1
    loc
}

realbest_construct<-function(MASE)
    {
realbestmean=matrix(0,dim(MASE)[1],1)
for(i in seq(1,dim(MASE)[1]))
    {
    count=MASE[i,,]
    line=apply(count,1,meanunique)
    if (max(line,na.rm = TRUE)==0){
        realbestmean[i,]=0
        }
    else
        {
        line[is.na(line)]=100 
        realbestmean[i,]= whichmin(line)
    }
    }
    return(realbestmean)
    }
    



etslgbreg

etsxgbcls

alldatalgb <- lgb.Dataset(data = as.matrix(data_features))
alldataxgb <- xgb.DMatrix(data = as.matrix(data_features))

xgbregets=predict(etsxgbreg,alldataxgb)
xgbclsets=predict(etsxgbcls,alldataxgb)
lgbregets=predict(etslgbreg,as.matrix(data_features))
lgbclsets=predict(etslgbcls,as.matrix(data_features))

xgbregthetaf=predict(thetafxgbreg,alldataxgb)
xgbclsthetaf=predict(thetafxgbcls,alldataxgb)
lgbregthetaf=predict(thetaflgbreg,as.matrix(data_features))
lgbclsthetaf=predict(thetaflgbcls,as.matrix(data_features))

lgbclsm=matrix(lgbclsets,5,ll)
lgbclsetsr=matrix(0,ll,1)
for(i in 1:ll)
    {
    lgbclsetsr[i,]=which.max(lgbclsm[,i])
}

lgbclsm=matrix(lgbclsthetaf,5,ll)
lgbclsthetafr=matrix(0,ll,1)
for(i in 1:ll)
    {
    lgbclsthetafr[i,]=which.max(lgbclsm[,i])
}

round10=function(x)
    {
    l=round(x)
    for(i in 1:length(l))
    {
    if(l[i]>5)
        {
        l[i]=5
    }
    if(l[i]<1)
    {
        l[i]=1
    }
}
    l
}

ets_pr=matrix(0,ll,4)
ets_pr[,1]=round10(xgbregets+1)
ets_pr[,2]=xgbclsets+1
ets_pr[,3]=round10(lgbregets+1)
ets_pr[,4]=lgbclsetsr



thetaf_pr=matrix(0,ll,4)
thetaf_pr[,1]=round10(xgbregthetaf+1)
thetaf_pr[,2]=xgbclsthetaf+1
thetaf_pr[,3]=round10(lgbregthetaf+1)
thetaf_pr[,4]=lgbclsthetafr

meanpre=function(pred_array,opt_pr,h,testindex)
    {
    data_length=length(testindex)
    meanpreh=matrix(0,data_length,h)
    for (i in 1:data_length){
    index=testindex[i]
    loc=opt_pr[index]
    predh=pred_array[index,loc,,]
    predh=unique(predh)
    meanpreh[i,]=apply(predh,2,mean)
}
    return(meanpreh)
}

predres=function(pred_matrix,data,real_data,opt_pr,lengthmatrix,h,testindex)
    {
    data_length=length(testindex)
    pred_res=matrix(0,data_length,6)
    model=ets(data[[1]])
    fore_l=forecast(model,h=h)
    freq=frequency(data[[1]])
    for (i in 1:data_length){
    index=testindex[i]
    loc=opt_pr[index]
    start=lengthmatrix[i,loc]
    y_all=data[[index]]
    y=y_all[start:length(y_all)]
    fore_l$x=ts(y,frequency=freq,end=end(y_all))
    real=real_data[[index]]
    fore_l$mean=ts(pred_matrix[i,],start=start(real),frequency=freq)
    res=accuracy(fore_l,real)
    pred_res[i,]=res[2,1:6]
    }
    return(pred_res)
    }

testindex=c(1:ll)
lengthtest=length5[testindex,]
etsxgbcls=meanpre(predhets,ets_pr[,1],hh,testindex)
etsxgbreg=meanpre(predhets,ets_pr[,2],hh,testindex)
etslgbcls=meanpre(predhets,ets_pr[,3],hh,testindex)
etslgbreg=meanpre(predhets,ets_pr[,4],hh,testindex)
etsxgbclsres=predres(etsxgbcls,data_tstrain,y_real,ets_pr[,1],lengthtest,hh,testindex)
etsxgbregres=predres(etsxgbreg,data_tstrain,y_real,ets_pr[,2],lengthtest,hh,testindex)
etslgbclsres=predres(etslgbcls,data_tstrain,y_real,ets_pr[,3],lengthtest,hh,testindex)
etslgbregres=predres(etslgbreg,data_tstrain,y_real,ets_pr[,4],lengthtest,hh,testindex)

thetafxgbcls=meanpre(predhthetaf,thetaf_pr[,1],hh,testindex)
thetafxgbreg=meanpre(predhthetaf,thetaf_pr[,2],hh,testindex)
thetaflgbcls=meanpre(predhthetaf,thetaf_pr[,3],hh,testindex)
thetaflgbreg=meanpre(predhthetaf,thetaf_pr[,4],hh,testindex)
thetafxgbclsres=predres(thetafxgbcls,data_tstrain,y_real,thetaf_pr[,1],lengthtest,hh,testindex)
thetafxgbregres=predres(thetafxgbreg,data_tstrain,y_real,thetaf_pr[,2],lengthtest,hh,testindex)
thetaflgbclsres=predres(thetaflgbcls,data_tstrain,y_real,thetaf_pr[,3],lengthtest,hh,testindex)
thetaflgbregres=predres(thetaflgbreg,data_tstrain,y_real,thetaf_pr[,4],lengthtest,hh,testindex)

apply(predets[,1,1,],2,mean)
apply(etsxgbclsres,2,mean)
apply(etsxgbregres,2,mean)
apply(etslgbclsres,2,mean)
apply(etslgbregres,2,mean)

apply(predthetaf[,1,1,],2,mean)
apply(thetafxgbclsres,2,mean)
apply(thetafxgbregres,2,mean)
apply(thetaflgbclsres,2,mean)
apply(thetaflgbregres,2,mean)

alltest=matrix(0,10,6)
alltest[1,]=apply(predets[,1,1,],2,mean)
alltest[2,]=apply(etsxgbclsres,2,mean)
alltest[3,]=apply(etsxgbregres,2,mean)
alltest[4,]=apply(etslgbclsres,2,mean)
alltest[5,]=apply(etslgbregres,2,mean)

alltest[6,]=apply(predthetaf[,1,1,],2,mean)
alltest[7,]=apply(thetafxgbclsres,2,mean)
alltest[8,]=apply(thetafxgbregres,2,mean)
alltest[9,]=apply(thetaflgbclsres,2,mean)
alltest[10,]=apply(thetaflgbregres,2,mean)

write.csv(alltest,'exchange_rate_M4_12.csv')




