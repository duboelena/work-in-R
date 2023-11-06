library(rstan)
library(coda)

diabete <- read.csv("diabetes.csv")
head(diabete)
xdiab <- cbind(intercept = 1, 
               preg = diabete$Pregnancies,
               BP = diabete$BloodPressure,
               SkinThick = diabete$SkinThickness,
               insulin = diabete$Insulin,
               BMI = diabete$BMI,
               DiabetePed = diabete$DiabetesPedigreeFunction,
               Outcome=diabete$Outcome==1)
datad = list(N=nrow(diabete), y=diabete$Outcome, x=diabete$Glucose)

# model
model <- stan_model('logregres.stan')
fit <- sampling(model, datad, iter = 3000, warmup = 1500, thin = 10, chains=4)

print(fit)
