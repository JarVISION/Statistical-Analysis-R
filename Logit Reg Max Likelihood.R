data <- read.csv("Proj2.csv", header = TRUE)
id <- data$Id
decay <- data$time_decay
num_cases <- data$num_cases
num_miss <- data$num_respmiss + data$num_visitmiss
num_esc <- data$num_esc
status <- data$status
data1 <- read.csv("uni_x.csv", header = TRUE)
status1 <- data1$status
x <- ifelse(status1 == "Loss", 0, 1)

df1 <- data.frame(id = id, n1 = decay, n2 = num_cases, n3 = num_miss, n4 = num_esc)
uni_id <- unique(df1$id)
length(uni_id)
length(x)


par_switch <- c(1, 1, 1, 1, 1)
par <- c(0.02, 0.02, 0.02, 0.02, 0.02)

L <- c(-Inf, -Inf, -Inf, -Inf, -Inf)
U <- c(Inf, Inf, Inf, Inf, Inf) 

for (i in 1:5){
  if(par_switch[i] == 0) L[i] = 0
  else L[i] = L[i]
}

for (i in 1:5){
  if(par_switch[i] == 0) U[i] = 0.00001
  else U[i] = U[i]
}


M <- function(par, x, df1) {
  
  y <- rep(0,length(uni_id))
  v <- rep(0, length(id))
  a <- par[1]
  b <- par[2]
  c <- par[3]
  d <- par[4]
  e <- par[5]
  p2 <- rep(-1,length(y))
  
  for( j in 1:length(uni_id)) {
    for(i in 1: length(id)) {
      if(id[i] == uni_id[j]) v[i] <- (exp(-c*df1$n1[i]))*(log(1 + df1$n2[i]))*(1 + d*(log(1 + df1$n3[i])) + e*(log(1 + df1$n4[i])))
      }
    y[j] <- sum(v)
    if(x[j] ==1) p2[j] <- (1/(1+exp(-(a+b*y[j]))))
    else p2[j] <- 1-((1/(1+exp(-(a+b*y[j])))))
  }
  return (-sum(log(p2)))
}

s <- nlminb(par, M, x = x, df1 = df1, lower = L, upper = U)
s$par
s$objective
s$convergence
s$iterations

