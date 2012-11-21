setwd("/Users/michiel/Documents/Kaggle/Titanic/")
chosen.columns <- c("survived","sex","pclass","fare","embarked","sibsp","parch","age")

train.data <- read.csv("train.csv", stringsAsFactors = F)
test.data <- read.csv("test.csv", stringsAsFactors = F)
test.data.with.prediction <- cbind(NA, test.data)
colnames(test.data.with.prediction) <- colnames(train.data)
all.data <- rbind( train.data, test.data.with.prediction)
for ( col in 1:ncol(all.data))
	all.data[,col] <- type.convert ( as.character( all.data[,col] ) )
train.data <- all.data[1:nrow(train.data),,drop=F]
test.data <- all.data[-(1:nrow(train.data)),,drop=F]

in.sample <- train.data[1:800,,drop=F]
out.sample <- train.data[-(1:800),,drop=F]

require(randomForest)

in.sample.rf <- randomForest(survived ~ ., data=in.sample[,chosen.columns], ntree=1000,
                              keep.forest=T, importance=T, na.action=na.exclude)
print ( in.sample.rf )
out.sample.pr<-predict(in.sample.rf,out.sample[,chosen.columns[-1]],predict.all=T)
out.sample.pr$aggregate[ is.na ( out.sample.pr$aggregate )] <- 0
test.results <- cbind(ifelse(out.sample.pr$aggregate>0.5,1,0),out.sample[,"survived"])


train.rf <- randomForest(survived ~ ., data=train.data[,chosen.columns], ntree=1000,
                              keep.forest=T, importance=T, na.action=na.exclude)

test.pr<-predict(train.rf,	
	test.data[,chosen.columns[-1]],
	predict.all=T)
test.output <- test.pr$aggregate
test.output [ is.na ( test.output )] <- 0
test.output <- ifelse(test.output > 0.5 , 1, 0)

write.table(test.output, row.names=F, col.names = F, file="output.csv" )