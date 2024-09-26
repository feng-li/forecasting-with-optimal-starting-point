library(M4comp2018)
library(forecast)
library(dplyr)

Yearly_M4 <- Filter(function(l) l$period == "Yearly", M4)

data=Yearly_M4

data[[1]]

# pre_forecast_ets=function(data,freq,h,n,m,train_index)
# {
#     data_length=length(data)
#     pred = array(0,dim = c(data_length,n,m,6))
#     predh = array(0,dim = c(data_length,n,m,6))
#     train_time = numeric(data_length)
#     test_time = numeric(data_length)
#     for(k in 1:data_length){
#         start_time = Sys.time()
#         data=Yearly_M4
#         y = data[[k]]$x
#         y_pred = data[[k]]$xx
#         y_l=length(y)
#         loc = 1:length(y)
#         loc_n = as.integer(loc*n/length(y))
#         filt_d = data.frame(y,loc_n)
#         filt_0=filter(filt_d ,loc_n==0)
#         n_l=count(filt_0)
#         m_l=n_l/m
#         n_l=as.integer(n_l)
#         m_l=as.numeric(m_l)
#         for(i in 0:(n-1)){
#             for(j in 0:(m-1)){
#                 Y = ts(y[round((i*n_l+j*m_l)+1):y_l], end = end(y), frequency=freq)
#                 M = ets(Y) 
#                 pd=forecast(M, h=h)
#                 predh[k,(i+1),(j+1),] =pd$mean 
#                 pred[k,(i+1),(j+1),] = accuracy(pd, y_pred)[2,1:6]
#             }
#         }
#         end_time = Sys.time()
#         if (k %in% train_index) {
#             train_time[k] = end_time - start_time
#         } else {
#             test_time[k] = end_time - start_time
#         }
#     }
#     return(list(pred, predh, train_time, test_time))
# }

pre_forecast_ets=function(data,freq,h,m,n,train_index)
{
    data_length=length(data)
    pred = array(0,dim = c(data_length,m,n,6))
    predh = array(0,dim = c(data_length,m,n,h))
    train_matrix <- matrix(0,ncol = 3, nrow =data_length)
    test_matrix <- matrix(0,ncol = 3, nrow =data_length)
    colnames(train_matrix) <- c("user_time", "system_time", "elapsed_time")
    colnames(test_matrix) <- c("user_time", "system_time", "elapsed_time")
    for(k in 1:data_length){
        start_time = Sys.time()
        y = data[[k]]$x
        y_pred = data[[k]]$xx
        y_l=length(y)
        loc = 1:length(y)
        loc_m = as.integer(loc*m/length(y))
        filt_d = data.frame(y,loc_m)
        filt_0=filter(filt_d ,loc_m==0)
        m_l=count(filt_0)
        n_l=m_l/n
        m_l=as.integer(m_l)
        n_l=as.numeric(n_l)
        for(i in 0:(m-1)){
            for(j in 0:(n-1)){
                Y = ts(y[round((i*m_l+j*n_l)+1):y_l], end = end(y), frequency=freq)
                M = ets(Y) 
                pd=forecast(M, h=h)
                predh[k,(i+1),(j+1),] =pd$mean 
                pred[k,(i+1),(j+1),] = accuracy(pd, y_pred)[2,1:6]
            }
        }
        end_time = Sys.time()
        if (k %in% train_index) {
            train_matrix[k,] = end_time - start_time
        } else {
            test_matrix[k,] = end_time - start_time
        }
    }
    return(list(pred, predh, train_matrix, test_matrix))
}

set.seed(100)
index = sample(2,length(data),replace = TRUE,prob=c(0.7,0.3))
trainindex=c(1:length(data))[index==1]

head(trainindex)

datalist=pre_forecast_ets(data,1,6,5,4,trainindex)

save(datalist, file = "year_ets_datalist.RData")

start_time = Sys.time()
pre_forecast_ets(data[1:50],1,6,5,4,trainindex)
end_time = Sys.time()
end_time - start_time








































