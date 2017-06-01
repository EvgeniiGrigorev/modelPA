rm(list=ls())

# Work Directory
datDir <- c('E:/work/git/base')
setwd(datDir)


# Read data
setwd(paste(datDir,"/pa",sep=""))

base <- read.csv('base-01-06-2017.txt',header=TRUE, sep='\t', quote = "\'")


summary(base)


plot(base$koeff_otcenki)

# Фильтруем по показателям, которые доступны
rdata <- base[which(base$koeff_otcenki != 'NaN'),]
summary(rdata$koeff_otcenki)

med <- median(rdata$koeff_otcenki)

rdata$success <- 0

# Целевая функция - если значение Кцели больше медианы, тогда сотрудник успешен
for(i in 1:nrow(rdata))
  if(rdata$koeff_otcenki[i] > med) rdata$success[i] <- 1

table(rdata$success)
pie(table(rdata$success))


# Multiple Linear Regression Example 
reg <- lm(koeff_otcenki ~ max_razryad_za_period + vozrast + koeff_rascheta_premii +kolvo_obrazovanii
          + kolvo_detei + let_posle_vyza + koeff_otrab_vremeni
          , data=rdata)
summary(reg) # show results



#График зависимости "переменных / факторов"
# pairs(rdata[11:20],main="Main Data", pch=19, col=as.numeric(rdata$success)+1)
pairs(rdata[6:10],main="Main Data", pch=19, col=as.numeric(rdata$success)+1)
pairs(rdata[10:14],main="Main Data", pch=19, col=as.numeric(rdata$success)+1)

coefficients(reg) # model coefficients
confint(reg, level=0.95) # CIs for model parameters 
fitted(reg) # predicted values
residuals(reg) # residuals
anova(reg) # anova table 
vcov(reg) # covariance matrix for model parameters 
influence(reg) # regression diagnostics


# Split randomly
data.target.split <- rdata[sample(1:nrow(rdata), nrow(rdata), replace = F),]
data.target.split.train <- data.target.split[1:floor(nrow(data.target.split)*.50), ]
data.target.split.evaluate <- data.target.split[(floor(nrow(data.target.split)*.50)+1):nrow(data.target.split), ]


library(rpart)

# Rpart
data.target.split.rpart <- rpart(data.target.split.train$success ~ max_razryad_za_period + vozrast + koeff_rascheta_premii +kolvo_obrazovanii
                                 + kolvo_detei + let_posle_vyza + koeff_otrab_vremeni
                                 , data = data.target.split.train, method = "class", cp = 0.011, na.action = na.rpart)

summary(data.target.split.rpart)


# Размер дерева
plotcp(data.target.split.rpart)

# Графика
plot(data.target.split.rpart,uniform = TRUE)
text(data.target.split.rpart,use.n = TRUE, cex = 0.75)
# printcp(data.target.split.rpart)

# Use the model R-part to predict the evaluation.
# data.target.split.evaluate <- data.target.split
data.target.split.evaluate$prediction <- predict(data.target.split.rpart, newdata=data.target.split.evaluate, type="class")
summary(data.target.split.evaluate$prediction)

# Calculate the overall accuracy.
data.target.split.evaluate$correct <- data.target.split.evaluate$prediction == data.target.split.evaluate$success
print(paste("% of predicted classifications correct", 100*mean(data.target.split.evaluate$correct)))
table(data.target.split.evaluate$prediction, data.target.split.evaluate$success)

# Проверка кластеров по дереву решений

library(sqldf)

# Calculate the overall accuracy.
data.target.split.evaluate$correct <- data.target.split.evaluate$prediction == data.target.split.evaluate$success
print(paste("% of predicted classifications correct", 100*mean(data.target.split.evaluate$correct)))
table(data.target.split.evaluate$prediction, data.target.split.evaluate$success)


# Если в модели оставить только тех, кого предсказывает, как успешные "1", тогда точность модели составит ~80%
# accurasy <- sqldf("SELECT count(tabelnyi), success,prediction from che t group by success,prediction")
# print(paste("% of predicted classifications correct", 100*sum(accurasy[which(accurasy$prediction == 1 & accurasy$success == 1),]$`count(tabelnyi)`)/sum(accurasy[which(accurasy$prediction == 1),]$`count(tabelnyi)`)))


# # Save results
# setwd('E:/work/git/base/news')
# 
# # save(data.target.split.rpart, file = "rpart_model_73.Rdata")
# load("rpart_model_73.Rdata", envir = e <- new.env())
# identical(data.target.split.rpart, e$data.target.split.rpart, ignore.environment = TRUE)
# data.target.split.rpart <- e$data.target.split.rpart
# 
# # save(data.target.split, file = "data_target_split_73.Rdata")
# load("data_target_split_73.Rdata", envir = e <- new.env())
# identical(data.target.split, e$data.target.split, ignore.environment = TRUE)
# data.target.split <- e$data.target.split.rpart
# 
# # save(data.target.split.evaluate, file = "data_target_split_evaluate_73.Rdata")
# load("data_target_split_evaluate_73.Rdata", envir = e <- new.env())
# identical(data.target.split.evaluate, e$data.target.split.evaluate, ignore.environment = TRUE)
# data.target.split.evaluate <- e$data.target.split.evaluate
# 
# # save(data.target.split.train, file = "data_target_split_train_73.Rdata")
# load("data_target_split_train_73.Rdata", envir = e <- new.env())
# identical(data.target.split.train, e$data.target.split.train, ignore.environment = TRUE)
# data.target.split.train <- e$data.target.split.train
