
require(nnet)

estimating.shattering.coefficient.for.single.n <- function(n, k=2, R=2) {

	# Building up a data sample
	S = NULL
	for (i in 1:R) {
		# rnorm -> Normal distribution
		S = cbind(S, rnorm(mean=0, sd=1, n=n))
	}

	# classify???
	HashTable = list()
	max.attempts.classify = n * 10^k
	for (i in 1:max.attempts.classify) {
		# Uniform distribution to produce hyperplane coefficients
		hyperplane = runif(min=-5, max=5, n=R+1)
		labels = sign(cbind(S,1) %*% hyperplane)
		key = paste(labels, collapse="", sep="")
		HashTable[[key]] = 1
	}

	return (length(HashTable))
}

mlp.assessment <- function(dataset,
			   max.hyp, 
			   delta=0.05,
			   training.sample.size=0.7) {

	training.set.size = round(training.sample.size * nrow(dataset))
	training.ids = sample(1:nrow(dataset), size=training.set.size)

	class.id = ncol(dataset)

	X = dataset[,1:(class.id-1)]
	Y = class.ind(dataset[,class.id]) # Transformation of expected outputs

	#Shattering.matrix = estimating.shattering.coefficient(gn=25, R=ncol(X))
	#m_n = ?? # next class

	# Information/Examples for training
	train.X = X[training.ids,]
	train.Y = Y[training.ids,]

	# Information/Examples for test 
	test.X = X[-training.ids,]
	test.Y = Y[-training.ids,]

	Results  = NULL
	for (num.hyp in 1:max.hyp) {
		# model -> f in F subseteq F_all
		model = nnet(train.X, train.Y, size=num.hyp)

		correct = 0
		for (i in 1:nrow(test.X)) {
			x = test.X[i,]
			y = test.Y[i,]
			o = round(predict(model, x))

			if (sum((y-o)^2) == 0) { # hit
				correct = correct + 1
			}
		}

		accuracy = correct / nrow(test.X)
		R_emp = 1 - accuracy # Empirical Risk

		m_n = estimating.shattering.coefficient.for.single.n(n=nrow(train.X), R=ncol(X))
		Variance = sqrt(1/nrow(train.X)*(log(2*m_n) - log(delta)))
		upper.bound.for.R_f = R_emp + Variance

		Results = rbind(Results, c(num.hyp, upper.bound.for.R_f))
	}

	return (Results)
}




