library(tsfeatures)
library(imputeTS)
library(forecast)
library(dplyr)



data=read.csv('Confidence_index100.csv')

head(data)

tail(data)

data=data[1:345,c(2:dim(data)[2])]



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
start=c(1995,1)
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

long=matrix(0,ll,1)
for( i in 1:ll)
    {
    long[i,1]=length(data_tstrain[[i]])
}

data_features=cbind(data_features,long)



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



library(gratis)
library(feasts)

my_features <- function(x) {
  output <- c(tsfeatures(x))
  output["long"] <- length(x)
  unlist(output)
}

n_g=50

library(parallel)
gratislist=list()
n_g=50
for(i in 1:ll) 
{  
  datamatrix=matrix(0,length(data_tstrain[[i]]),n)  
  x <- generate_ts_with_target(n=n_g,
                               ts.length = length(Goldtstrain[[i]]),
                               freq = frequency(Goldtstrain[[i]]),
                               seasonal = 1,
                               features ="my_features",
                               selected.features = colnames(Goldf),
                               target = as.vector(Goldf[i,]))
  
  gratislist[[i]]=x
}       

# clnum <- detectCores()
# cl <- makeCluster(getOption("cl.cores", 5))

# fun <- function(i) {
#   datamatrix=matrix(0,length(Goldtstrain[[i]]),n)  
#   x <- generate_ts_with_target(n=n,
#                                ts.length = length(Goldtstrain[[i]]),
#                                freq = frequency(Goldtstrain[[i]]),
#                                seasonal = 1,
#                                features ="my_features",
#                                selected.features = colnames(Goldf),
#                                target = as.vector(Goldf[i,]))
#   return(x)
# }

# # 导出'Goldtstrain', 'Goldf', 'my_features'到每个任务的环境中
# clusterExport(cl, c("Goldtstrain", "Goldf", "my_features","n",'generate_ts_with_target','tsfeatures'))

# gratislist <- parLapply(cl, 1:ll, fun)

# stopCluster(cl)

save(gratislist,file='gratislist_Confidence_index_50_12.RData')

loaddata=load('gratislist_Confidence_index_50_12.RData')
gratislist=eval(parse(text = loaddata))

gratislist[[1]]

predetsy = array(0,dim = c(ll*n_g,5,4,6))
predhetsy = array(0,dim = c(ll*n_g,5,4,hh))
for(k in 1:ll){
    yall=gratislist[[k]]
    yall_n=dim(yall)[2]
    for(t in 1:yall_n)
    {
    y=yall[,t]
    y_pred = ts(y[(length(y)-hh+1):length(y)],end = end(y), frequency=frequency(y))
    y_past = ts(y[1:(length(y)-hh)],start = start(y), frequency=frequency(y))
    loc = 1:length(y_past)
    loc0 = as.integer(loc*5/length(y_past))
    d = data.frame(y_past,loc0)
    y1=filter(d,loc0==0)
    h=count(y1)
    m_l=h/4
    h=as.integer(h)
    m_l=as.numeric(m_l)
    for(i in 0:(m-1)){
        for(j in 0:(n-1)){
            Y = ts(y_past[f1((i*h+j*m_l)+1):length(y_past)],end = end(y_past), frequency=frequency(yall))
            M = ets(Y) 
            pd=forecast(M, h=hh)
            predhetsy[(k-1)*yall_n+t,(i+1),(j+1),] =pd$mean 
            predetsy[(k-1)*yall_n+t,(i+1),(j+1),] = forecast::accuracy(pd, y_pred)[2,1:6]
        }
    }
}
}





predthetafy = array(0,dim = c(ll*n_g,5,4,6))
predhthetafy = array(0,dim = c(ll*n_g,5,4,hh))
for(k in 1:ll){
    yall=gratislist[[k]]
    yall_n=dim(yall)[2]
    for(t in 1:yall_n)
    {
    y=yall[,t]
    y_pred = ts(y[(length(y)-hh+1):length(y)],end = end(y), frequency=frequency(y))
    y_past = ts(y[1:(length(y)-hh)],start = start(y), frequency=frequency(y))
    loc = 1:length(y_past)
    loc0 = as.integer(loc*5/length(y_past))
    d = data.frame(y_past,loc0)
    y1=filter(d,loc0==0)
    h=count(y1)
    m_l=h/4
    h=as.integer(h)
    m_l=as.numeric(m_l)
    for(i in 0:(m-1)){
        for(j in 0:(n-1)){
            Y = ts(y_past[f1((i*h+j*m_l)+1):length(y_past)],end = end(y_past), frequency=frequency(yall))
            M = thetaf(Y,h=hh) 
            pd=forecast(M, h=hh)
            predthetafy[(k-1)*yall_n+t,(i+1),(j+1),] = forecast::accuracy(pd, y_pred)[2,1:6]
            predhthetafy[(k-1)*yall_n+t,(i+1),(j+1),] = pd$mean
        }
    }
}
}

gratistrainlist=list()
long=matrix(0,ll*n,1)
for(i in 1:ll)
    {
    n1=dim(gratislist[[i]])[1]
    n2=dim(gratislist[[i]])[2]
    gratisdata=gratislist[[i]]
    x=gratisdata[,1]
    da1=ts(x[1:(length(x)-hh)],start=start(x),frequency=frequency(x))
    long[(i-1)*n+1]=length(da1)
    for(j in 2:n2)
        {
        x=gratisdata[,j]
        da=ts(x[1:(length(x)-hh)],start=start(x),frequency=frequency(x))
        da1=cbind(da1,da)
        long[(i-1)*n+j]=length(da)
    }
    new_colnames <- paste("Series", 1:ncol(da1))
    # 设置列名
    colnames(da1) <- new_colnames
    gratistrainlist[[i]]=da1
}



gratistrainlist[[1]]



datatsy=list()
datatstrainy=list()
i=1
da=gratistrainlist[[i]]
f=tsfeatures(da)
datay=f

datatsy=list()
datatstrainy=list()
i=1
da=gratistrainlist[[i]]
f=tsfeatures(da)
f$long=dim(da)[1]
datay=f
for( i in 2:ll)
    {
    da=gratistrainlist[[i]]
    f=tsfeatures(da)
    f$long=dim(da)[1]
    datay=rbind(datay,f)
}


library("xgboost")
library("Matrix")
library('Ckmeans.1d.dp')
library(lightgbm)









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
    

MASEets

MASEets=predetsy[,,,6]
MASEthetaf=predthetafy[,,,6]
realbestmeanthetaf=realbest_construct(MASEthetaf)
realbestmeanets=realbest_construct(MASEets)

# 假设你的数组名为MASEets
# 首先检查整个数组中是否存在缺失值
missing_values <- is.na(MASEets)

# 然后对第一个维度（2200）应用sum函数，统计每个元素中的缺失值数量
missing_in_dim1 <- apply(missing_values, 1, sum)

# 找出含有缺失值的元素的索引
indices_with_NA <- which(missing_in_dim1 > 0)

# 打印含有缺失值的元素的索引
print(indices_with_NA)


train_data=datay
train_label=realbestmeanets
train_label1=realbestmeanthetaf



dtrain20 <- xgb.DMatrix(data = as.matrix(train_data),label = as.matrix(train_label)) 
dtrain21 <- xgb.DMatrix(data = as.matrix(train_data),label = as.matrix(as.factor(train_label)))

etsxgbreg  <- xgboost(data = dtrain20, nround=100)

etsxgbcls  <- xgboost(data = dtrain21, nround=100, objective='multi:softmax',num_class=5)

dtrain22 <- lgb.Dataset(data = as.matrix(train_data),label = as.matrix(train_label))
dtrain23 <- lgb.Dataset(data = as.matrix(train_data),label = as.matrix(as.factor(train_label)))

# 定义参数列表
params <- list(
  objective = "regression",
  metric = "rmse"
)

etslgbreg  <- lgb.train(data = dtrain22,nrounds = 100,params = params)

# 定义参数列表
params <- list(
  objective = 'multiclass',
  num_class = 5,
  num_iterations = 100  # 使用 num_iterations 代替 nrounds
)

# 使用参数列表进行训练
etslgbcls <- lgb.train(
  data = dtrain23,
  params = params
)

dtrain20 <- xgb.DMatrix(data = as.matrix(train_data),label = as.matrix(train_label1)) 
dtrain21 <- xgb.DMatrix(data = as.matrix(train_data),label = as.matrix(as.factor(train_label1)))

thetafxgbreg  <- xgboost(data = dtrain20, nround=100)

thetafxgbcls  <- xgboost(data = dtrain21, nround=100, objective='multi:softmax',num_class=5)

dtrain22 <- lgb.Dataset(data = as.matrix(train_data),label = as.matrix(train_label1))
dtrain23 <- lgb.Dataset(data = as.matrix(train_data),label = as.matrix(as.factor(train_label1)))

# 定义参数列表
params <- list(
  objective = "regression",
  metric = "rmse"
)

thetaflgbreg  <- lgb.train(data = dtrain22,nrounds = 100,params = params)

# 定义参数列表
params <- list(
  objective = 'multiclass',
  num_class = 5,
  num_iterations = 100  # 使用 num_iterations 代替 nrounds
)

# 使用参数列表进行训练
thetaflgbcls <- lgb.train(
  data = dtrain23,
  params = params
)





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


write.csv(alltest,'Confidence_index_gratis_12.csv')




