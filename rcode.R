set.seed(3555676577); #Pseudo Random Number Generator

lamda<-0.2        # lambda
num<-40         # number of exponentials
sample<-1000      # number of simulations

sim <- matrix(rexp(num*sample, rate=lamda), num, sample)#store in matrix 1000 simulations following exponential distribution

row_means <- rowMeans(sim)

sample_mean <- mean(row_means)
theoritical_mean <- 1/lamda

sample_variance <- mean(row_means)
theoritical_variance <- 1/lamda

theoretical_var <- (1/lamda)^2/num;
theoretical_sd <- 1/lamda/sqrt(num);


hist(row_means, breaks=50, prob=TRUE,
     main="Distribution of samples' mean,
     drawn from exponential distribution with lambda=0.2",
     xlab="")


# density of the averages of samples
lines(density(row_means))

# theoretical center of distribution based on formula of rate 1/lamda so value 5 as center
abline(v=1/lamda, col="red")


# theoretical density of the averages of samples
xfit <- seq(min(row_means), max(row_means), length=100)
yfit <- dnorm(xfit, mean=1/lamda, sd=(1/lamda/sqrt(sample)))
lines(xfit, yfit, pch=22, col="red", lty=2)

# add legend
legend('topright', c("simulation", "theoretical"), lty=c(1,2), col=c("black", "red"))









library("ggplot2")




ggplot(melt(row_means), aes(x=row_means)) +
        geom_histogram(binwidth=0.1, colour="black", fill="white") +
        geom_vline(aes(xintercept=mean(1/lamda, na.rm=T)),   # Ignore NA values for mean
                   color="red", linetype="dashed", size=1)

