rm(list=ls())

# Work Directory
datDir <- c('E:/work/git/base')
setwd(datDir)


# Read data
setwd(paste(datDir,"/pa",sep=""))

base <- read.csv('base-02-06-2017.txt',header=TRUE, sep='\t', quote = "\'")


summary(base$koeff_otsenki)
plot(base$koeff_otsenki)

# Фильтруем по показателям, которые доступны
rdata <- base[which(base$koeff_otsenki != 'NaN'),]
summary(rdata$koeff_otsenki)

med <- median(rdata$koeff_otsenki)

rdata$success <- 0

# Целевая функция - если значение Кцели больше медианы, тогда сотрудник успешен
for(i in 1:nrow(rdata))
  if(rdata$koeff_otsenki[i] > med) rdata$success[i] <- 1

table(rdata$success)
pie(table(rdata$success))


# Multiple Linear Regression Example 
reg <- lm(koeff_otsenki ~ razryad_sotrydnika_max_za_period + vozrast_let	+ kolichestvo_vyshih_obrazovanii	+ 
          kolichestvo_detei +	kol_vo_dnei_na_bolnichnom_za_kvartal	+ kolichestvo_dnei_neoplachiemogo_otpyska_za_kv	+
          kol_vo_dnei_opl_otpyska_za_kvartal + kol_vo_let_posle_poslednego_vyshego_obr	+ koeff_otrabot_vremeni	+
          koeff_rabochego_vremeni +	prirost_greida_za_kvartal	+ prirost_oklada_za_kvartal	+ obshii_shtat_v_podchinenii	+
          staj_v_sberbanke + summarnoe_kol_vo_let_obych_v_vyzah	+ sredniy_razryad_sotrydnika_za_period
          , data=rdata)

summary(reg) # show results


# diagnostic plots 
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
plot(reg)

# abline(lm(koeff_otsenki ~ max_razryad_za_period , data=rdata))

# # K-fold cross-validation
# library(DAAG)
# cv.lm(df=rdata, reg, m=3) # 3 fold cross-validation


#График зависимости "переменных / факторов"
# pairs(rdata[11:20],main="Main Data", pch=19, col=as.numeric(rdata$success)+1)
# pairs(rdata[16:19],main="Main Data", pch=19, col=as.numeric(rdata$success)+1)
# pairs(rdata[20:23],main="Main Data", pch=19, col=as.numeric(rdata$success)+1)
# pairs(rdata[14:17],main="Main Data", pch=19, col=as.numeric(rdata$success)+1)


# coefficients(reg) # model coefficients
# confint(reg, level=0.95) # CIs for model parameters 
# fitted(reg) # predicted values
# residuals(reg) # residuals
# anova(reg) # anova table 
# vcov(reg) # covariance matrix for model parameters 
# influence(reg) # regression diagnostics


# Split randomly
data.target.split <- rdata[sample(1:nrow(rdata), nrow(rdata), replace = F),]
data.target.split.train <- data.target.split[1:floor(nrow(data.target.split)*.50), ]
data.target.split.evaluate <- data.target.split[(floor(nrow(data.target.split)*.50)+1):nrow(data.target.split), ]


library(rpart)

# Rpart
data.target.split.rpart <- rpart(data.target.split.train$success ~ razryad_sotrydnika_max_za_period + vozrast_let	+ kolichestvo_vyshih_obrazovanii	+ 
                                   kolichestvo_detei +	kol_vo_dnei_na_bolnichnom_za_kvartal	+ kolichestvo_dnei_neoplachiemogo_otpyska_za_kv	+
                                   kol_vo_dnei_opl_otpyska_za_kvartal + kol_vo_let_posle_poslednego_vyshego_obr	+ koeff_otrabot_vremeni	+
                                   koeff_rabochego_vremeni +	prirost_greida_za_kvartal	+ prirost_oklada_za_kvartal	+ obshii_shtat_v_podchinenii	+
                                   staj_v_sberbanke + summarnoe_kol_vo_let_obych_v_vyzah	+ sredniy_razryad_sotrydnika_za_period
                                 
                                 , data = data.target.split.train, method = "class", cp = 0.011, na.action = na.rpart)


data.target.split.rpart <- rpart(data.target.split.train$success ~ top_300_16_plus + status_na_moment_voznagrazdeniya	+ 
                                   semeinyi_status_text +	kod_semeinogo_statusa +	flag_prepodavanie_sb	+	
                                   flag_naliciya_uchenoi_step	+ maimenovanie_uchenoi_stepeni	+ pol
                                   
                                 , data = data.target.split.train, method = "class", cp = 0.011, na.action = na.rpart)


# data.target.split.rpart <- rpart(data.target.split.train$success ~ top_300_16_plus + status_na_moment_voznagrazdeniya	+ 
#                                  semeinyi_status_text +	kod_semeinogo_statusa +	flag_prepodavanie_sb	+ date_birth +	
#                                  flag_naliciya_uchenoi_step	+ maimenovanie_uchenoi_stepeni	+ pol + 
#                                  razryad_sotrydnika_max_za_period + vozrast_let	+ kolichestvo_vyshih_obrazovanii	+ 
#                                  kolichestvo_detei +	kol_vo_dnei_na_bolnichnom_za_kvartal	+ kolichestvo_dnei_neoplachiemogo_otpyska_za_kv	+
#                                  kol_vo_dnei_opl_otpyska_za_kvartal + kol_vo_let_posle_poslednego_vyshego_obr	+ koeff_otrabot_vremeni	+
#                                  koeff_rabochego_vremeni +	prirost_greida_za_kvartal	+ prirost_oklada_za_kvartal	+ obshii_shtat_v_podchinenii	+
#                                  staj_v_sberbanke + summarnoe_kol_vo_let_obych_v_vyzah	+ sredniy_razryad_sotrydnika_za_period
#                                  
#                                  , data = data.target.split.train, method = "class", cp = 0.011, na.action = na.rpart)


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

# # Проверка кластеров по дереву решений
# 
# library(sqldf)
# 
# # Calculate the overall accuracy.
# data.target.split.evaluate$correct <- data.target.split.evaluate$prediction == data.target.split.evaluate$success
# print(paste("% of predicted classifications correct", 100*mean(data.target.split.evaluate$correct)))
# table(data.target.split.evaluate$prediction, data.target.split.evaluate$success)



library(randomForest)
data.target.split.rforest <- randomForest(data.target.split.train$success ~ 
                                            max_razryad_za_period + vozrast + kolvo_obrazovanii
                                          + kolvo_detei + let_posle_vyza + koeff_otrab_vremeni + pol + grade + status_na_mom_voznagrazdeniya
                                          + ychen_stepen, 
                                          data = data.target.split.train , na.action=na.omit)

print(data.target.split.rforest) # view results 
importance(data.target.split.rforest) # importance of each predictor
plot(data.target.split.rforest)

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
