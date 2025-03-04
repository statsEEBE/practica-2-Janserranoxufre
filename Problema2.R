#Codigo para problema 2
mis_dades <- iris
mis_dades
x <- mis_dades$Petal.Length
mean(x) #media(x)
sd(x)
hist(x)
y <- mis_dades$Sepal.Length
y
mean(y)
plot(x,y)

#Desviació quadràtica: Q=sum(yi-mitjana(y)^2)
                      #Q=(yi-(mx+b))^2
                      #m=(sum(xi-mitjana(x)*(yi-mitjana(y))/(sum(xi-mitjana(x))^2)

m <-sum((x-mean(x))*(y-mean(y)))/sum((x-mean(x))^2)
m
b<- mean(y)-m*mean(x)
b

m*1.5+b   #valor predicció en 1.5


###

mod <-lm(y~x)    # ~ = altgr 4 espai
mod
summary(mod)

data.frame(x=1.5)

predict(mod, data.frame(x=1.5))

data.frame(x=x)

ypred <-predict(mod, data.frame(x=x))

plot(x,y,col='red', pch=16)
lines(x, ypred)

Rsq <- sum((ypred-mean(y))^2)/sum((y-mean(y))^2)
Rsq
