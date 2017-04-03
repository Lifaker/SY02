runifa <- function(n) {
    if(!exists("param")) param <<- sample(10:20,1)
    runif(n, min=0, max=param)
    }

# X suit L(a)
# E(X) = a/2
# On utilise la méthode des moments :
# 2 * E(X) = a
# D'ou 2 Xbarre = achapeau


# 1)
# On propose l'estimlateur
# â = 2Xbar
n<-100
2*mean(runifa(n))

# 2)
estim <- function(x) {
    2*mean(x)
}

# 3)
estim(runifa(n))
a <- replicate(10000, estim(runifa(n)))

# 4)
boxplot(a)
param
                                        
# 5)
# On à a = ((k+1) ^mk)^1/k 
# ^a = ((k+1) ^mk)^1/k
# ^a = ((k+1)/n * SUM Xi^k)^1/k
k<-10

estimk <- function(x,k){
    ((k+1) * mean(x^k))^(1/k)
}

estimk(x,k)
ak<- replicate(10000, estimk(runifa(n),k))
boxplot(a,ak)

# 6)
runknown <- function(n) {
    bn <- rbinom(n, 1, 0.5)
    bn * rnorm(n, mean=-4,sd=1) + (1 - bn) * rnorm(n, mean=10, sd=1)
    }

mean(runknown(1000))-3
sd(runknown(1000))-sqrt(50)

# 7)
hist(runknown(1000),100)

# 8)
plot(ecdf(runknown(1000)))

# 9)
calculT <- function(n) {
    x <- runknown(n)
    si <- sqrt(50)
    mu <- 3
    T <- (mean(x)-mu)/(si/sqrt(n))
    return(T)
}

calculT(n)

# 10) 11)
T <- (replicate(100, calculT(n)))
hist(T) 
sd(T) # Presque 1
mean(T) # Presque 0
# On a bien une loi normale centrée réduite
                                        
random.T<-calculT

# 12)
plot(ecdf(T))

# 13) 14)
curve(pnorm, add=TRUE)
10^4*10^4

# 15)
x<-rexp(1000,3)
hist(x,100) # On retrouve bien la loi exponentielle
# 16)
f<-function(lambda) {
    x<-rexp(1000,lambda)
    dexp(x, lambda)
} 
f(3)
# 17)
L<-function(lambda){
    prod(f(lambda))
}
L(3)
# 18)
logL<-function(lambda){
    sum(log(f(lambda)))
}
# 19)
logL(3.1)>log(2.8) # Renvoie presque toujours VRAI donc 3.1 est plus probable

# 20)
opt <- optimize(logL,lower=0, upper=10, maximum=TRUE)
opt$maximum
opt$objective

# 21)
sim.EMV <- function() {
    x <- rexp(n, lambda)
    # Fonction de densité
    f <- function(lambda) {
        dexp(x, lambda)
    }
    # Vraisemblance
    L <- function(lambda) {
        prod(f(lambda))
    }
    # Log-vraisemblance
    logL <- function(lambda) {
        sum(log(f(lambda)))
    }

    # Maximization de la log-vraisemblance
    opt <- optimize(logL, lower=0, upper=10, maximum=TRUE)
    opt$maximum
}

# 22)
sim.EMV <- replicate(10000, sim.EMV())

# 23)
mean(sim.EMV)
var(sim.EMV)

# 24)
mean(sim.EMV) - lambda
n/(n-1)*lambda - lambda

# 25)
install.packages("pracma")
library(pracma)

# 26)
sim.Fisher <- function() {
    x <- rexp(n, lambda)

    # Fonction de densité
    f <- function(lambda) {
        dexp(x, lambda)
    }
    # Vraisemblance
    L <- function(lambda) {
        prod(f(lambda))
    }
    # Log-vraisemblance
    logL <- function(lambda) {
        sum(log(f(lambda)))
    }

    # Information de Fisher
    (grad(logL, 3))^2
}

# 27)
(inf.Fisher <- mean(replicate(10000, sim.Fisher())))

# 28)
n/lambda^2

# 29)
(1/inf.Fisher)
var(sim.EMV)

# 30)

lambda <- 3
n <- 100

grad2 <- function(f, x) {
    df <- function(x) {
        grad(f, x)
    }
    grad(df, x)
}

sim.Fisher <- function() {
    x <- rexp(n, lambda)
    # Fonction de densité
    f <- function(lambda) {
        dexp(x, lambda)
    }
    # Vraisemblance
    L <- function(lambda) {
        prod(f(lambda))
    }
    # Log vraisemblance
    logL <- function(lambda) {
        sum(log(f(lambda)))
    }
    # Information de Fisher
    grad2(logL, 3)
}

(-mean(replicate(1000, sim.Fisher())))
