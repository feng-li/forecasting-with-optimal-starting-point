library(M4comp2018)
library(tsfeatures)

Daily_M4 <- Filter(function(l) l$period == "Daily", M4)

data=Daily_M4





Feature_extraction=function(data,train_index)
{
    datalength=length(data)
    feature_ep=tsfeatures(data[[1]]$x)
    feature_matrix=matrix(0,datalength,length(feature_ep)+1)
    colnames(feature_matrix)=c(colnames(feature_ep),'length')
    train_matrix <- matrix(0,ncol = 3, nrow =datalength)
    test_matrix <- matrix(0,ncol = 3, nrow =datalength)
    colnames(train_matrix) <- c("user_time", "system_time", "elapsed_time")
    colnames(test_matrix) <- c("user_time", "system_time", "elapsed_time")
    for(i in 1:datalength){
        start_time = Sys.time()
        y =  data[[i]]$x
        features=cbind(as.matrix(tsfeatures(y)),length(y))
        feature_matrix[i,] = features
        end_time = Sys.time()
        if (i %in% train_index) {
                train_matrix[i,] = end_time - start_time
        } else {
                test_matrix[i,] = end_time - start_time
            }
    
}
    return(list(feature_matrix, train_matrix, test_matrix))
    }

set.seed(100)
index = sample(2,length(data),replace = TRUE,prob=c(0.7,0.3))
trainindex=c(1:length(data))[index==1]



feature_matrix=Feature_extraction(data,trainindex)

write.csv(feature_matrix,file='Daily_features_matrix.csv')










