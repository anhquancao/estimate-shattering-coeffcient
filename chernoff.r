
n=1:10000
epsilon = c(0.01, 0.05, 0.1)
upper = NULL

for (i in 1:length(epsilon)) {
	upper = cbind(upper, 2*exp(-2*n*epsilon[i]^2))
}

plot(upper[,3], t="l")
lines(upper[,2], col=2)
lines(upper[,1], col=3)
