library(dplyr)
#Load raw data
data <- read.csv("project1.csv", header = TRUE)

y1 <- data$sales
x1 <- data$rebate
x2 <- data$ad.spent
x3 <- data$xmas

f1 <- function(par, x, y){
  a <- par[1]
  b <- par[2]
  r <- par[3]
  if(r == 0) sum((y-(a + b*x))^2)
  else sum((y-(a + (b*(1-exp(-r*x)))/r))^2)
}

par <- c(0.5, 0.5, 0.5)

model.1 <- nlminb(par, f1, x = x1, y = y1)
names(model.1)
model.1$par
par
model.1$objective

f1(par, x1, y1)

#Simulate data and check the model

X1 <- runif(100)
X1
Y1 <- 10 + ((2*(1-exp(-0.05*X1)))/0.05) + rnorm(100, sd = 0.1)
Y1

model.2 <- nlminb(par, f1, x = X1, y = Y1)
names(model.2)
model.2$par
model.2$objective

plot(X1, Y1)

#Bootstrap
#data1 <- select(data, ad.spent, sales)
#b.data1 <- data1[sample(nrow(data1), replace = TRUE), ]

b1 <- data.frame(x = X1, y = Y1)
b1
null_SSE <- as.numeric(list())
for (i in 1 : 100){
  b.data <- b1[sample(nrow(b1), replace = TRUE), ]
  b.x1 <- b.data[,1]
  b.y1 <- b.data[,2]
  
  par1 <- c(0, 0)
  
  fnull <- function(par1, x, y){
    a <- par1[1]
    b <- par1[2]
    sum((b.y1 - (a + b * b.x1))^2)
  }
  model.3 <- nlminb(par1, fnull, x = b.x1, y = b.y1)
  null_SSE[i] <- model.3$objective 
}
null_SSE
hist(null_SSE)

#Alternate model
par_full <- c(1, 1, 1, 1, 1, 1, 1, 1)
par_switch <- c(1, 1, 0, 1, 1, 1, 1, 0)
par_null <- c(0, 0, 0, 0, 0, 0, 0, 0)

L <- c(-Inf, -Inf, -Inf, -Inf, -Inf, -Inf, -Inf, -Inf)
U <- c(Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf) 

for (i in 1:8){
  if(par_switch[i] == 0) L[i] = 0
  else L[i] = L[i]
}

for (i in 1:8){
  if(par_switch[i] == 0) U[i] = 0.00001
  else U[i] = U[i]
}

for (i in 1:8){
  if(par_switch[i] == 0) par_null[i] = par_switch[i]
  else par_null[i] = par_full[i]
}


rss_alt <- as.numeric(list())


f2 <- function(par_null, x1, x2, x3, y1){
  
  a0 <- par_null[1]
  a1 <- par_null[2]
  a2 <- par_null[3]
  a3 <- par_null[4]
  a4 <- par_null[5]
  a5 <- par_null[6]
  r1 <- par_null[7]
  r2 <- par_null[8]
  
  n1 <- ifelse (r1 ==0, x1, (1-exp(-r1*x1))/r1)
  n2 <- ifelse (r2 ==0, x2, (1-exp(-r2*x2))/r2)
  n3 <- x3
  n4 <- x1*x3
  n5 <- x2*x3
  
  sum((y1 - (a0 + a1*n1 + a2*n2 + a3*n3 + a4*n4 + a5*n5))^2)
}


model.alt <- nlminb(par_null, f2, x1 = x1, x2 = x2, x3 = x3, y1 = y1, lower = L, upper = U)
rss_alt <- model.alt$objective
rss_alt
model.alt$par
model.alt


#Bootstrap full model

b.alt_sse <- as.numeric(list())
for (i in 1 : 100) {
  
  boot.data <- data[sample(nrow(data), replace = TRUE), ]
  c.x1 <- boot.data[,1]
  c.x2 <- boot.data[,2]
  c.x3 <- boot.data[,3]
  c.y1 <- boot.data[,4]
  
  b.model.alt <- nlminb(par_null, f2, x1 = c.x1, x2 = c.x2, x3 = c.x3, y1 = c.y1, lower = L, upper = U)
  b.alt_sse[i] <- b.model.alt$objective
}

b.alt_sse
b.model.alt$par
hist(b.alt_sse)

pvalue <- (sum(b.alt_sse<rss_alt)) / 100
pvalue
