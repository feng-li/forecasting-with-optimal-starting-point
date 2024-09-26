library(forecast)
library(M4comp2018)
library(dplyr)

etsl=load('Quarterly_ets_datalist.RData')
ets_datalist <- eval(parse(text = etsl))
thetafl=load('Quarterly_thetaf_datalist.RData')
thetaf_datalist <- eval(parse(text = thetafl))

ets_predh=ets_datalist[[2]]
thetaf_predh=thetaf_datalist[[2]]

ets_pred_res=ets_datalist[[1]]
thetaf_pred_res=thetaf_datalist[[1]]

etsoptl=load('Quarterly_ets_opt_pre_result.RData')
ets_opt_pre_list<- eval(parse(text = etsoptl))
thetafoptl=load('Quarterly_thetaf_opt_pre_result.RData')
thetaf_opt_pre_list<- eval(parse(text = thetafoptl))

time_matrix <- matrix(0,ncol = 3, nrow =3)
colnames(time_matrix) <- c("user_time", "system_time", "elapsed_time")
start_time = Sys.time()

ets_pr_min=ets_opt_pre_list[[1]]
ets_pr_mean=ets_opt_pre_list[[2]]
thetaf_pr_min=thetaf_opt_pre_list[[1]]
thetaf_pr_mean=thetaf_opt_pre_list[[2]]

head(ets_pr_min)



ets_pr_min=ets_pr_min+1
ets_pr_mean=ets_pr_mean+1
thetaf_pr_min=thetaf_pr_min+1
thetaf_pr_mean=thetaf_pr_mean+1


head(ets_pr_min)



round5=function(x)
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
    return(l)
}

ets_pr_min[,2]=round5(ets_pr_min[,2])
ets_pr_min[,4]=round5(ets_pr_min[,4])

ets_pr_mean[,2]=round5(ets_pr_mean[,2])
ets_pr_mean[,4]=round5(ets_pr_mean[,4])

thetaf_pr_min[,2]=round5(thetaf_pr_min[,2])
thetaf_pr_min[,4]=round5(thetaf_pr_min[,4])

thetaf_pr_mean[,2]=round5(thetaf_pr_mean[,2])
thetaf_pr_mean[,4]=round5(thetaf_pr_mean[,4])

head(ets_pr_min)

Quarterly_M4 <- Filter(function(l) l$period == "Quarterly", M4)

data=Quarterly_M4

datalength=length(data)
h=data[[1]]$h
m=10
n=4
freq=frequency(data[[1]]$x)

realxx=matrix(0,datalength,h)
for(i in 1:datalength){
    realxx[i,]=data[[i]]$xx
}

length5=matrix(0,datalength,m)
for(k in 1:datalength)
{
y = data[[k]]$x
y_l=length(y)
loc = 1:length(y)
loc_m = as.integer(loc*m/length(y))
filt_d = data.frame(y,loc_m)
filt_0=filter(filt_d ,loc_m==0)
m_l=count(filt_0)
m_l=as.integer(m_l)
length5[k,]=c(0,1*m_l,2*m_l,3*m_l,4*m_l)
    }



end_time = Sys.time()
time_matrix[1,]=end_time-start_time

end_time-start_time

start_time = Sys.time()

set.seed(100)
index = sample(2,length(data),replace = TRUE,prob=c(0.7,0.3))
testindex=c(1:length(data))[index==2]

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

predres=function(pred_matrix,data,opt_pr,lengthmatrix,h,testindex)
    {
    data_length=length(testindex)
    pred_res=matrix(0,data_length,6)
    model=ets(data[[1]]$x)
    fore_l=forecast(model,h=h)
    freq=frequency(data[[1]]$x)
    for (i in 1:data_length){
    index=testindex[i]
    loc=opt_pr[index]
    start=lengthmatrix[i,loc]
    y_all=data[[index]]$x
    y=y_all[start:length(y_all)]
    fore_l$x=ts(y,frequency=freq,end=end(y_all))
    real=data[[index]]$xx
    fore_l$mean=ts(pred_matrix[i,],start=start(real),frequency=freq)
    res=accuracy(fore_l,real)
    pred_res[i,]=res[2,1:6]
    }
    return(pred_res)
    }

predres_mul=function(pred_matrix,data,opt_pr_mul,lengthmatrix,h,testindex)
    {
    data_length=length(testindex)
    pred_res=matrix(0,data_length,6)
    model=ets(data[[1]]$x)
    fore_l=forecast(model,h=h)
    freq=frequency(data[[1]]$x)
    for (i in 1:data_length){
    index=testindex[i]
    loc=min(opt_pr_mul[index,])
    start=lengthmatrix[i,loc]
    y_all=data[[index]]$x
    y=y_all[start:length(y_all)]
    fore_l$x=ts(y,frequency=freq,end=end(y_all))
    real=data[[index]]$xx
    fore_l$mean=ts(pred_matrix[i,],start=start(real),frequency=freq)
    res=accuracy(fore_l,real)
    pred_res[i,]=res[2,1:6]
    }
    return(pred_res)
    }

etsxgbcls_min=meanpre(ets_predh,ets_pr_min[,1],h,testindex)
etsxgbreg_min=meanpre(ets_predh,ets_pr_min[,2],h,testindex)
etslgbcls_min=meanpre(ets_predh,ets_pr_min[,3],h,testindex)
etslgbreg_min=meanpre(ets_predh,ets_pr_min[,4],h,testindex)

etsxgbcls_mean=meanpre(ets_predh,ets_pr_mean[,1],h,testindex)
etsxgbreg_mean=meanpre(ets_predh,ets_pr_mean[,2],h,testindex)
etslgbcls_mean=meanpre(ets_predh,ets_pr_mean[,3],h,testindex)
etslgbreg_mean=meanpre(ets_predh,ets_pr_mean[,4],h,testindex)

lengthtest=length5[testindex,]

etsminxgbclsres=predres(etsxgbcls_min,data,ets_pr_min[,1],lengthtest,h,testindex)
etsminxgbregres=predres(etsxgbreg_min,data,ets_pr_min[,2],lengthtest,h,testindex)
etsminlgbclsres=predres(etslgbcls_min,data,ets_pr_min[,3],lengthtest,h,testindex)
etsminlgbregres=predres(etslgbreg_min,data,ets_pr_min[,4],lengthtest,h,testindex)

etsmeanxgbclsres=predres(etsxgbcls_mean,data,ets_pr_mean[,1],lengthtest,h,testindex)
etsmeanxgbregres=predres(etsxgbreg_mean,data,ets_pr_mean[,2],lengthtest,h,testindex)
etsmeanlgbclsres=predres(etslgbcls_mean,data,ets_pr_mean[,3],lengthtest,h,testindex)
etsmeanlgbregres=predres(etslgbreg_mean,data,ets_pr_mean[,4],lengthtest,h,testindex)

end_time = Sys.time()
time_matrix[2,]=end_time-start_time

etsallmin=(etsxgbcls_min+etslgbcls_min+etsxgbreg_min+etslgbreg_min)/4
etsallmean=(etsxgbcls_mean+etslgbcls_mean+etsxgbreg_mean+etslgbreg_mean)/4

etsallminres=predres_mul(etsallmin,data,ets_pr_min,lengthtest,h,testindex)
etsallmeanres=predres_mul(etsallmean,data,ets_pr_mean,lengthtest,h,testindex)

etsclsmin=(etsxgbcls_min+etslgbcls_min)/2
etsregmin=(etsxgbreg_min+etslgbreg_min)/2
etsclsmean=(etsxgbcls_mean+etslgbcls_mean)/2
etsregmean=(etsxgbreg_mean+etslgbreg_mean)/2

etsclsminres=predres_mul(etsclsmin,data,ets_pr_min[,c(1,3)],lengthtest,h,testindex)
etsregminres=predres_mul(etsregmin,data,ets_pr_min[,c(2,4)],lengthtest,h,testindex)
etsclsmeanres=predres_mul(etsclsmean,data,ets_pr_mean[,c(1,3)],lengthtest,h,testindex)
etsregmeanres=predres_mul(etsregmean,data,ets_pr_mean[,c(2,4)],lengthtest,h,testindex)

etspred_change<- aperm(ets_predh[1:datalength,1:m,1:n,1:h], c(1, 3, 2, 4))  
# 使用array函数将其转换为新的形状
etspred_change <- array(etspred_change, dim = c(datalength, m*n, h))

model=ets(data[[1]]$x)
fore_l=forecast(model,h=h)
etsrandomres=matrix(0,datalength,6)
for (i in 1:datalength){
    random=sample(m*n,n)
    minloc=min(random)
    # 使用cut函数将数据分割为区间
    loc <- cut(minloc, breaks =seq(0,m*n,n), labels = FALSE)
    loc<-as.numeric(loc)
    etsrandompred=apply(etspred_change[i,random,],2,mean)
    start=length5[i,loc]
    y_all=data[[i]]$x
    y=y_all[start:length(y_all)]
    fore_l$x=ts(y,frequency=freq,end=end(y_all))
    real=data[[i]]$xx
    fore_l$mean=ts(etsrandompred,start=start(real),frequency=freq)
    res=accuracy(fore_l,real)
    etsrandomres[i,]=res[2,1:6]
    }



mean1=function(x)
    {
# 使用is.finite函数检查哪些值是有限的
finite_values <- x[is.finite(x)]
# 计算有限值的平均值
d=mean(finite_values, na.rm = TRUE)
return(d)
}

print(apply(ets_pred_res[testindex,1,1,],2,mean1))
print(apply(etsminxgbclsres,2,mean1))
print(apply(etsminxgbregres,2,mean1))
print(apply(etsminlgbclsres,2,mean1))
print(apply(etsminlgbregres,2,mean1))

print(apply(etsmeanxgbclsres,2,mean1))
print(apply(etsmeanxgbregres,2,mean1))
print(apply(etsmeanlgbclsres,2,mean1))
print(apply(etsmeanlgbregres,2,mean1))

print(apply(etsallminres,2,mean1))
print(apply(etsallmeanres,2,mean1))



print(apply(etsclsminres,2,mean1))
print(apply(etsregminres,2,mean1))
print(apply(etsclsmeanres,2,mean1))
print(apply(etsregmeanres,2,mean1))

print(apply(etsrandomres[testindex,],2,mean))

etsall=matrix(0,16,6)
etsall[1,]=apply(ets_pred_res[testindex,1,1,],2,mean1)[1:6]
etsall[2,]=apply(etsminxgbclsres,2,mean1)
etsall[3,]=apply(etsminxgbregres,2,mean1)
etsall[4,]=apply(etsminlgbclsres,2,mean1)
etsall[5,]=apply(etsminlgbregres,2,mean1)
etsall[6,]=apply(etsmeanxgbclsres,2,mean1)
etsall[7,]=apply(etsmeanxgbregres,2,mean1)
etsall[8,]=apply(etsmeanlgbclsres,2,mean1)
etsall[9,]=apply(etsmeanlgbregres,2,mean1)
etsall[10,]=apply(etsallminres,2,mean1)
etsall[11,]=apply(etsallmeanres,2,mean1)
etsall[12,]=apply(etsclsminres,2,mean1)
etsall[13,]=apply(etsregminres,2,mean1)
etsall[14,]=apply(etsclsmeanres,2,mean1)
etsall[15,]=apply(etsregmeanres,2,mean1)
etsall[16,]=apply(etsrandomres[testindex,],2,mean)

etsall

write.csv(etsall,'ets_Quarterly_final_res.csv')

start_time = Sys.time()

thetafxgbcls_min=meanpre(thetaf_predh,thetaf_pr_min[,1],h,testindex)
thetafxgbreg_min=meanpre(thetaf_predh,thetaf_pr_min[,2],h,testindex)
thetaflgbcls_min=meanpre(thetaf_predh,thetaf_pr_min[,3],h,testindex)
thetaflgbreg_min=meanpre(thetaf_predh,thetaf_pr_min[,4],h,testindex)

thetafxgbcls_mean=meanpre(thetaf_predh,thetaf_pr_mean[,1],h,testindex)
thetafxgbreg_mean=meanpre(thetaf_predh,thetaf_pr_mean[,2],h,testindex)
thetaflgbcls_mean=meanpre(thetaf_predh,thetaf_pr_mean[,3],h,testindex)
thetaflgbreg_mean=meanpre(thetaf_predh,thetaf_pr_mean[,4],h,testindex)

thetafminxgbclsres=predres(thetafxgbcls_min,data,thetaf_pr_min[,1],lengthtest,h,testindex)
thetafminxgbregres=predres(thetafxgbreg_min,data,thetaf_pr_min[,2],lengthtest,h,testindex)
thetafminlgbclsres=predres(thetaflgbcls_min,data,thetaf_pr_min[,3],lengthtest,h,testindex)
thetafminlgbregres=predres(thetaflgbreg_min,data,thetaf_pr_min[,4],lengthtest,h,testindex)

thetafmeanxgbclsres=predres(thetafxgbcls_mean,data,thetaf_pr_mean[,1],lengthtest,h,testindex)
thetafmeanxgbregres=predres(thetafxgbreg_mean,data,thetaf_pr_mean[,2],lengthtest,h,testindex)
thetafmeanlgbclsres=predres(thetaflgbcls_mean,data,thetaf_pr_mean[,3],lengthtest,h,testindex)
thetafmeanlgbregres=predres(thetaflgbreg_mean,data,thetaf_pr_mean[,4],lengthtest,h,testindex)

end_time = Sys.time()
time_matrix[3,]=end_time-start_time

end_time-start_time

thetafallmin=(thetafxgbcls_min+thetaflgbcls_min+thetafxgbreg_min+thetaflgbreg_min)/4
thetafallmean=(thetafxgbcls_mean+thetaflgbcls_mean+thetafxgbreg_mean+thetaflgbreg_mean)/4

thetafallminres=predres_mul(thetafallmin,data,thetaf_pr_min,lengthtest,h,testindex)
thetafallmeanres=predres_mul(thetafallmean,data,thetaf_pr_mean,lengthtest,h,testindex)

thetafclsmin=(thetafxgbcls_min+thetaflgbcls_min)/2
thetafregmin=(thetafxgbreg_min+thetaflgbreg_min)/2
thetafclsmean=(thetafxgbcls_mean+thetaflgbcls_mean)/2
thetafregmean=(thetafxgbreg_mean+thetaflgbreg_mean)/2

thetafclsminres=predres_mul(thetafclsmin,data,thetaf_pr_min[,c(1,3)],lengthtest,h,testindex)
thetafregminres=predres_mul(thetafregmin,data,thetaf_pr_min[,c(2,4)],lengthtest,h,testindex)
thetafclsmeanres=predres_mul(thetafclsmean,data,thetaf_pr_mean[,c(1,3)],lengthtest,h,testindex)
thetafregmeanres=predres_mul(thetafregmean,data,thetaf_pr_mean[,c(2,4)],lengthtest,h,testindex)

thetafpred_change<- aperm(thetaf_predh[1:datalength,1:m,1:n,1:h], c(1, 3, 2, 4))  
# 使用array函数将其转换为新的形状
thetafpred_change <- array(thetafpred_change, dim = c(datalength, m*n, h))

model=ets(data[[1]]$x)
fore_l=forecast(model,h=h)
thetafrandomres=matrix(0,datalength,6)
for (i in 1:datalength){
    random=sample(m*n,n)
    minloc=min(random)
    # 使用cut函数将数据分割为区间
    loc <- cut(minloc, breaks =seq(0,m*n,n), labels = FALSE)
    loc<-as.numeric(loc)
    thetafrandompred=apply(thetafpred_change[i,random,],2,mean)
    start=length5[i,loc]
    y_all=data[[i]]$x
    y=y_all[start:length(y_all)]
    fore_l$x=ts(y,frequency=freq,end=end(y_all))
    real=data[[i]]$xx
    fore_l$mean=ts(thetafrandompred,start=start(real),frequency=freq)
    res=accuracy(fore_l,real)
    thetafrandomres[i,]=res[2,1:6]
    }

print(apply(thetaf_pred_res[testindex,1,1,],2,mean1))
print(apply(thetafminxgbclsres,2,mean1))
print(apply(thetafminxgbregres,2,mean1))
print(apply(thetafminlgbclsres,2,mean1))
print(apply(thetafminlgbregres,2,mean1))

print(apply(thetafmeanxgbclsres,2,mean1))
print(apply(thetafmeanxgbregres,2,mean1))
print(apply(thetafmeanlgbclsres,2,mean1))
print(apply(thetafmeanlgbregres,2,mean1))

print(apply(thetafallminres,2,mean1))
print(apply(thetafallmeanres,2,mean1))

print(apply(thetafallminres,2,mean1))
print(apply(thetafallmeanres,2,mean1))

print(apply(thetafclsminres,2,mean1))
print(apply(thetafclsmeanres,2,mean1))
print(apply(thetafregminres,2,mean1))
print(apply(thetafregmeanres,2,mean1))

print(apply(thetafrandomres[testindex,],2,mean1))

thetafall=matrix(0,16,6)
thetafall[1,]=apply(thetaf_pred_res[testindex,1,1,],2,mean1)[1:6]
thetafall[2,]=apply(thetafminxgbclsres,2,mean1)
thetafall[3,]=apply(thetafminxgbregres,2,mean1)
thetafall[4,]=apply(thetafminlgbclsres,2,mean1)
thetafall[5,]=apply(thetafminlgbregres,2,mean1)
thetafall[6,]=apply(thetafmeanxgbclsres,2,mean1)
thetafall[7,]=apply(thetafmeanxgbregres,2,mean1)
thetafall[8,]=apply(thetafmeanlgbclsres,2,mean1)
thetafall[9,]=apply(thetafmeanlgbregres,2,mean1)
thetafall[10,]=apply(thetafallminres,2,mean1)
thetafall[11,]=apply(thetafallmeanres,2,mean1)
thetafall[12,]=apply(thetafclsminres,2,mean1)
thetafall[13,]=apply(thetafclsmeanres,2,mean1)
thetafall[14,]=apply(thetafregminres,2,mean1)
thetafall[15,]=apply(thetafregmeanres,2,mean1)
thetafall[16,]=apply(thetafrandomres[testindex,],2,mean1)

thetafall

write.csv(thetafall,'thetaf_Quarterly_final_res.csv')

time_matrix

write.csv(time_matrix,'Quarterly_time_matrix.csv')


