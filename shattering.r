
# Accepted divergence in between the empirical and the expected risks
epsilon = 0.1

# How my sample size varies along the collection
n = 1:10000 

# 1-MLP or Perceptron
# Input data space: R^2
# Class label: additional column
# Hidden units: 1
m1 = n^2

# 5-MLP
# Input data space: R^2
# Class label: additional column
# Hidden units: 5
m5 = (n^2)^5

plot(2*m5*exp(-2*n*epsilon^2), col=1, ylim=c(0,1))
lines(2*m1*exp(-2*n*epsilon^2), col=2)
