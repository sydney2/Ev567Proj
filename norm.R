
dta <- read.csv("evenv.csv")
env <- na.omit(dta)
rm(dta)
env <- env[,-1]
env.lg <-log(env+1)

par(mfrow=c(3,2))

nm <- rnorm(30) #Normal distribution
hist(nm)
qqnorm(nm)
qqline(nm)

exp <- rexp(30, rate=1) #Exponential distribution
hist(exp)
qqnorm(exp)
qqline(exp)

lg <-log(exp)
hist(lg)
qqnorm(lg)
qqline(lg)

lshap <- lapply(env, shapiro.test) #shapiro test on normal data
lshap[[1]] ## look at the first column results
# You will need to extract the things you want from these objects, which all have the structure:
str(lshap[[1]])
lres <- sapply(lshap, `[`, c("statistic","p.value"))
lres
t(lres)#transposed

lshap <- lapply(env.lg, shapiro.test) #shapiro test on log transformed data
lres <- sapply(lshap, `[`, c("statistic","p.value"))
t(lres)

