library(h2o)
library(data.table)
library(Metrics)
h2o.init(nthreads=4)
print(paste("reading training file:",Sys.time()))
train<-fread("./data/train.csv",select=c(1,2,3,4,6,7,8,10,11,16,18, 19, 24))
train <- subset(train, Expected < 69)
train$Ref_5x5_50th[which(train$Ref_5x5_50th < 0)] <- NA
train$Ref_5x5_90th[which(train$Ref_5x5_90th < 0)] <- NA
train$RefComposite[which(train$RefComposite < 0)] <- NA
train$RefComposite_5x5_50th[which(train$RefComposite_5x5_50th_5x5_50th < 0)] <- NA
train$RefComposite_5x5_90th[which(train$RefComposite_5x5_50th_5x5_90th < 0)] <- NA
train$Ref[which(train$Ref < 0)] <- NA
trainHex<-as.h2o(train[,.(
    dist   = mean(radardist_km, na.rm = T),
     refArea5   = mean(Ref_5x5_50th, na.rm = T),
     refArea9  = mean(Ref_5x5_90th, na.rm = T),
     meanRefcomp = mean(RefComposite,na.rm=T),
     meanRefcomp5 = mean(RefComposite_5x5_50th,na.rm=T),
     meanRefcomp9 = mean(RefComposite_5x5_90th,na.rm=T),
     zdr   = mean(Zdr, na.rm = T),
     zdr5   = mean(Zdr_5x5_50th, na.rm = T),
     zdr9   = mean(Zdr_5x5_90th, na.rm = T),
     target = log1p(mean(Expected)),
    meanRef = mean(Ref,na.rm=T),
    sumRef = sum(Ref,na.rm=T),
    records = .N,
    naCounts = sum(is.na(Ref))
    ),Id][records>naCounts,],destination_frame="train.hex")

rfHex<-h2o.gbm(x=c("dist", "refArea5", "refArea9", "meanRefcomp","meanRefcomp5","meanRefcomp5", "zdr",
                           "zdr5", "zdr9", "meanRef","sumRef", "records","naCounts"),
    y="target", 
    training_frame=trainHex,
    model_id="rfStarter.hex", 
    nfolds=10)
