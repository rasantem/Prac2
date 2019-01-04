## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ----echo=FALSE, fig.cap="A caption", out.width = '100%'-----------------
#knitr::include_graphics("nutriscore-a.png")

## ------------------------------------------------------------------------
data <- read.csv("es_food_nutrients.csv", na.strings=c("Na"))

head(data,3)
str(data)

## ------------------------------------------------------------------------
# Valor Energy
data$Energy <- gsub(" kj([()0-9 kcal]+)","",data$Energy)

# Elimina caracteres
cols <- c(1, 15, 16)
data[,-cols] <- as.data.frame(apply(data[,-cols], 2, function(y) gsub(" g", "", y)))
data[,-cols] <- as.data.frame(apply(data[,-cols], 2, function(y) gsub(" mg", "", y)))
data[,-cols] <- as.data.frame(apply(data[,-cols], 2, function(y) gsub(" % vol", "", y)))
data[,-cols] <- as.data.frame(apply(data[,-cols], 2, function(y) gsub("~ ", "", y)))
data[,-cols] <- as.data.frame(apply(data[,-cols], 2, function(y) gsub("< ", "", y)))
data[,-cols] <- as.data.frame(apply(data[,-cols], 2, function(y) gsub("> ", "", y)))

data[, -cols] <- as.data.frame(apply(data[, -cols], 2, 
                                     function(y) as.numeric(gsub(",", ".", y))))
summary(data[, -cols])

## ------------------------------------------------------------------------
# NA's
na.data <- sapply(data, function(y) sum(length(which(is.na(y)))))
# Representación
barplot(na.data, main="NA's por variable",las= 2)

## ------------------------------------------------------------------------
# Elimina observaciones Score = Na
data_clean  <- subset(data, Score != "NA")
x_na.data <- sapply(data_clean, function(y) sum(length(which(is.na(y)))))
barplot(x_na.data, main="NA's por variable ",las= 2)

## ------------------------------------------------------------------------
# Eliminación variables Alcohol, Zinc, Magnesium, Omega3, Fiber, Img y Name
data_clean <- data_clean[, -c(1, 7, 11:14, 16)]

sapply(data_clean, function(y) sum(length(which(is.na(y)))))

## ----message=FALSE, warning=FALSE----------------------------------------
library(VIM)
library(psych)

# Imputación
imp_data_clean <- kNN(data_clean)[1:8]

# Cáculo estmadores
f <- function(x) {
      c(Mean = mean(x, na.rm = TRUE), Median = median(x, na.rm = TRUE), 
        SD = sd(x, na.rm = TRUE), RIC = IQR(x, na.rm = TRUE))
}

# Tablas comparación estimadores
est_table<-sapply(data_clean[1:8], f)
knitr::kable(est_table, caption = "Estimadores con datos sin imputar")

est_table<-sapply(imp_data_clean, f)
knitr::kable(est_table, caption = "Estimadores con datos imputados")

data_clean[1:8] <-imp_data_clean

## ------------------------------------------------------------------------
# N valores extremos
out_f <- function(x) {
      length(boxplot.stats(x)$out)
}

nout_table<-sapply(data_clean[1:8], out_f)
knitr::kable(nout_table, caption = "Numero de valores detectados como extreme scores")

## ------------------------------------------------------------------------
write.csv(data_clean, "es_food_nutrients_clean.csv")

## ------------------------------------------------------------------------
library(plyr)
# Codififcación clase Score
data_m <- data_clean
data_m$Score <- as.numeric(revalue(data_m$Score, 
                                c("A"="5", "B"="4","C"="3", "D"="2", "E"="1")))
# Escalado
maxs <- apply(data_m, 2, max) 
mins <- apply(data_m, 2, min)
scaled.data_m <- as.data.frame(scale(data_m, center = mins, scale = maxs - mins))

# Preparación conjuntos 70 train-30 test
samplesize = 0.70 * nrow(data_m)
set.seed(40)
indexA = sample( seq_len ( nrow ( data_m)), size = samplesize )
data.train = data_m[ indexA,]
data.test = data_m[ -indexA,]

scaled.data.train = scaled.data_m[ indexA,] #70% 
scaled.data.test = scaled.data_m[ -indexA,] #30%

## ----message=FALSE, warning=FALSE----------------------------------------
library(ggplot2)

# p.value variables
test_f <- function(x) {
      shapiro.test(x)$p.value
}

t_table<-sapply(data_clean[1:8], test_f)
t_table

par(mfrow=c(2,4))

qqnorm(data_clean$Sugars, pch = 1, frame = FALSE, main ="Sugars")
qqline(data_clean$Sugars, col = "steelblue", lwd = 2)
qqnorm(data_clean$Energy, pch = 1, frame = FALSE, main ="Energy")
qqline(data_clean$Energy, col = "steelblue", lwd = 2)
qqnorm(data_clean$Salt, pch = 1, frame = FALSE, main ="Salt")
qqline(data_clean$Salt, col = "steelblue", lwd = 2)
qqnorm(data_clean$Fat, pch = 1, frame = FALSE, main ="Fat")
qqline(data_clean$Fat, col = "steelblue", lwd = 2)
qqnorm(data_clean$Proteins, pch = 1, frame = FALSE, main ="Proteins")
qqline(data_clean$Proteins, col = "steelblue", lwd = 2)
qqnorm(data_clean$Carbohydrate, pch = 1, frame = FALSE, main ="Carbohydrate")
qqline(data_clean$Carbohydrate, col = "steelblue", lwd = 2)
qqnorm(data_clean$Saturated_fat, pch = 1, frame = FALSE, main ="Sat_Fat")
qqline(data_clean$Saturated_fat, col = "steelblue", lwd = 2)
qqnorm(data_clean$Sodium, pch = 1, frame = FALSE, main ="Sodium")
qqline(data_clean$Sodium, col = "steelblue", lwd = 2)


## ----message=FALSE, warning=FALSE----------------------------------------
library(car)
lev_f <- function(x) {
      leveneTest(x ~ Score, data = data_clean)$`Pr(>F)`[1]
}
lev_table<-sapply(data_clean[1:8], lev_f)
lev_table

## ----message=FALSE, warning=FALSE----------------------------------------
library(corrplot)
cor_d <- round(cor(data_m, use="complete.obs", method="kendall"), 3)#data_clean[1:8]
knitr::kable(cor_d, caption = "Matriz de correlación")
corrplot(cor_d, method = "ellipse")


## ------------------------------------------------------------------------
cor.test(data_m$Salt, data_m$Sodium, method = "kendall")

## ------------------------------------------------------------------------

library(neuralnet)

#Formula 
feats <- names(data_m[1:7])
f <- paste(feats,collapse=' + ') 
f <- paste("Score ~",f)
f <- as.formula(f)

nn <- neuralnet(f,scaled.data.train,hidden=c(5:3),lifesign = "full", threshold = 0.05
                , linear.output=FALSE, stepmax=1e6)

## ----message=FALSE, warning=FALSE----------------------------------------
library(NeuralNetTools)

predict_test <- compute(nn,scaled.data.test[1:7])

predict_test_ = predict_test$net.result * (max(data_m$Score) - min(data_m$Score)) + 
  min(data_m$Score) 
test.r <- (scaled.data.test$Score)*(max(data_m$Score)-min(data_m$Score))+min(data_m$Score)

predict_testnn_<- sapply(predict_test_,round,digits=0) 
knitr::kable(table(data.test$Score,predict_testnn_), caption = "Matriz de confusion modelo nn")

## ------------------------------------------------------------------------
mosaicplot(table(predict_testnn_, data.test$Score), xlab='Real value', ylab='Prediction',
           main='Mosaic Plot nn ', shade = T)

## ------------------------------------------------------------------------
# Variable importance
olden(nn)


## ------------------------------------------------------------------------
# outliers variabless
boxplot.stats(data_clean$Energy)$out
boxplot.stats(data_clean$Fat)$out
boxplot.stats(data_clean$Saturated_fat)$out
boxplot.stats(data_clean$Carbohydrate)$out
boxplot.stats(data_clean$Sugars)$out
boxplot.stats(data_clean$Proteins)$out
boxplot.stats(data_clean$Salt)$out
boxplot.stats(data_clean$Sodium)$out


