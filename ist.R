library("readxl")
read_excel("C:\\Users\\Jacob\\Desktop\\Risk Models\\risk.xlsx")
data<-read_excel("C:\\Users\\Jacob\\Desktop\\Risk Models\\risk.xlsx")
str(data)

is.factor(data$Gender)
data$Gender<-as.factor(data$Gender)

data$Owns_Car<-as.factor(data$Owns_Car)
is.factor(data$Owns_Car)
str(data)

data$Employment_status<-as.factor(data$Employment_status)
str(data)

data$Score<-as.ordered(data$Score)
str(data)


summary(data)

nrow(data[is.na(data)])

ind <- sample(2, nrow(data), replace = TRUE, prob = c(0.8,0.2))
train <- data[ind==1,]
test <- data[ind==2,]

library(MASS)

model <- polr(Score~Gender + Employment_status + Age, train ,Hess = TRUE)
summary(model)
(ctable<-coef(summary(model)))

#pvalue
p<- pnorm(abs(ctable[,"t value"]), lower.tail = FALSE)*2
(ctable <- cbind(ctable, "p value" = p))


#predict probability

pred <-predict(model, train[1:5,], type = "prob")
print(pred, digits = 3)

pred <-predict(model, train)
(tab <-table(pred,train$Score))









