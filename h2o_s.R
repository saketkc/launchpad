library(h2o)
library(data.table)
library(Metrics)
h2o.init(nthreads=4)

## use data table to only read the Estimated, Ref, and Id fields
print(paste("reading training file:",Sys.time()))
train<-fread("./data/train.csv",select=c(1,2,3,4,6,7,8,10,11,16,18, 19, 24))

#Cut off outliers of Expected >= 69
train <- subset(train, Expected < 69)

summary(train)

#Cut off Ref values < 0
train$Ref_5x5_50th[which(train$Ref_5x5_50th < 0)] <- NA
train$Ref_5x5_90th[which(train$Ref_5x5_90th < 0)] <- NA
train$RefComposite[which(train$RefComposite < 0)] <- NA
train$RefComposite_5x5_50th[which(train$RefComposite_5x5_50th_5x5_50th < 0)] <- NA
train$RefComposite_5x5_90th[which(train$RefComposite_5x5_50th_5x5_90th < 0)] <- NA
train$Ref[which(train$Ref < 0)] <- NA

cor(train, use = "pairwise.complete.obs")
summary(train)

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
    
    summary(trainHex)

rfHex<-h2o.randomForest(x=c("dist", "refArea5", "refArea9", "meanRefcomp","meanRefcomp5","meanRefcomp5", "zdr",
                           "zdr5", "zdr9", "meanRef","sumRef", "records","naCounts"
                        ),
    y="target",training_frame=trainHex,model_id="rfStarter.hex", ntrees=1000, sample_rate = 0.8)
rfHex
h2o.varimp(rfHex)
rm(train)

test<-fread("./data/test.csv",select=c(1,2,3,4,6,7,8,10,11,16,18, 19))

#Cut off Ref values < 0
test$Ref_5x5_50th[which(test$Ref_5x5_50th < 0)] <- NA
test$Ref_5x5_90th[which(test$Ref_5x5_90th < 0)] <- NA
test$RefComposite[which(test$RefComposite < 0)] <- NA
test$RefComposite_5x5_50th[which(test$RefComposite_5x5_50th < 0)] <- NA
test$RefComposite_5x5_90th[which(test$RefComposite_5x5_90th < 0)] <- NA
test$Ref[which(test$Ref < 0)] <- NA


testHex<-as.h2o(test[,.(
     dist   = mean(radardist_km, na.rm = T),
      refArea5   = mean(Ref_5x5_50th, na.rm = T),
      refArea9  = mean(Ref_5x5_90th, na.rm = T),
     meanRefcomp = mean(RefComposite,na.rm=T),
     meanRefcomp5 = mean(RefComposite_5x5_50th,na.rm=T),
     meanRefcomp9 = mean(RefComposite_5x5_90th,na.rm=T),
     zdr   = mean(Zdr, na.rm = T),
     zdr5   = mean(Zdr_5x5_50th, na.rm = T),
     zdr9   = mean(Zdr_5x5_90th, na.rm = T),
      
    meanRef = mean(Ref,na.rm=T),
    sumRef = sum(Ref,na.rm=T),
    records = .N,
    naCounts = sum(is.na(Ref))
    ),Id],destination_frame="test.hex")
    
    summary(testHex)

submission<-fread("./data/sample_solution.csv")
predictions<-as.data.frame(h2o.predict(rfHex,testHex))
submission$Expected<- 0.7 * expm1(predictions$predict) + 0.3 * submission$Expected

#convert expected values to 0.01in values
submission$Expected <- round(submission$Expected / 0.254) * 0.254

summary(submission)
write.csv(submission,"rfv3cn3.csv",row.names=F)

