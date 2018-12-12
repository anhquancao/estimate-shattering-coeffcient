library(combinat)

# generate all possible classifications by finding all mappings from region to class
gen.maps <- function (region.index, num.classes, region.strings) {
    
    # map each class to new index, so we will not be overlap when replace
    classes = 1:num.classes + num.classes
    
    if (length(region.index) == 0) {
        # there is no region left
        return (region.strings)
    }
    
    # select the first region
    x = region.index[1]
    
    # Generate all possible mapping of this region to all classes
    new.region.strings = NULL
    for (s in region.strings) {
        for (i in classes) {
            new.s = gsub(x, i, s)
            new.region.strings = c(new.region.strings, new.s)
        }
    }
    
    # recursive call for all regions left
    return (gen.maps(region.index[region.index != x], num.classes, new.region.strings))
}

gen.all.classifications <- function (region.index, num.classes, region.string) {
    return (gen.maps(region.index, num.classes, c(region.string)))
}


estimate.shattering <- function (num.hyperplanes = 2, num.classes = 2,
                                 n.start = 1, n.end = 15, k = 2 , R = 2) {
    
    shattering = NULL
    origin = 1:num.classes
    #perms = permn(origin)
    
    for (n in n.start : n.end) {
        print(paste("n",n, sep = " "))
        # Generate the data sample
        S = NULL
        for (i in 1:R) {
            S = cbind(S, rnorm(mean = 0, sd = 1, n = n))    
        }
        
        # classify
        hashTable = list()
        max.attempts = n * 10 ^ k
        for (i in 1:max.attempts) {
            labels = NULL
            
            # get prediction from all hyperplanes
            for (h in 1:num.hyperplanes) {
                hyperplane = runif(min=-5, max=5, n=R+1)
                s = cbind(S,1) %*% hyperplane
                labels = cbind(labels, s)
            }
            labels[labels > 0] = 1
            labels[labels <= 0] = 0
            
            # find all region and assign index to each region
            regions = list()
            predicts = NULL
            region.index = NULL
            index = 1
            for (r in 1:nrow(labels)) {
                label = paste(labels[r,], collapse="")
                if (is.null(regions[[label]])) {
                    regions[[label]] = index
                    region.index = c(region.index, index)
                    index = index + 1
                }
                predicts = c(predicts, regions[[label]])
            }
            
            key = paste(predicts, collapse="")
            classifications = gen.all.classifications(region.index, num.classes, key)
            
            for (classification in classifications) {
                hashTable[[classification]] = 1        
            }
            
        }
        shattering = rbind(shattering, c(n, length(hashTable)))
    }
    return (shattering)
}

# Compare with the code in the lecture
# 1 hyperplane, R^2
shatterings = estimate.shattering(num.hyperplanes = 1, n.start = 1, n.end = 15, k = 4, num.classes = 2, R = 2)
in.class.shatterings = read.table(file = 'shattering_R2_single_hyperplane.dat', header = FALSE)
plot(shatterings, xlab = "Number of points", ylab = "Shattering coefficient")
points(in.class.shatterings, col = "red")

# 1 hyperplane, R^3
shatterings = estimate.shattering(num.hyperplanes = 1, n.start = 1, n.end = 15, k = 4, num.classes = 2, R = 3)
in.class.shatterings = read.table(file = 'shattering_R3_single_hyperplane.dat', header = FALSE)
plot(shatterings, xlab = "Number of points", ylab = "Shattering coefficient")
points(in.class.shatterings, col = "red")

# 2 hyperplanes, 3 classes , R^2
shatterings = estimate.shattering(num.hyperplanes = 2, n.start = 1, n.end = 15, k = 4, num.classes = 3, R = 2)
plot(shatterings, xlab = "Number of points", ylab = "Shattering coefficient")

fit.regression.one.neuron <- function (gn = 20, R = 2, k = 3, a = 12, b = 0.1) {
    shattering.one.neuron = estimate.shattering(num.hyperplanes = 1, R = R, n.start = 1, n.end = gn, k = k)
    x = shattering.one.neuron[, 1]
    y = shattering.one.neuron[, 2]
    model = nls(y ~ a*exp(b*x),start=list(a = a,b = b)) 
    
    plot(x, y, xlab = 'number of points', ylab = "shattering")
    predicts = predict(model, data.frame(x = x))
    predicts = cbind(1:20, predicts)
    lines(predicts,  col='red')
    
    return (model)
}

estimate.shattering.nnet <- function(n, num.hyperplanes = 2, k=3, R=2, model) {
    gn = 20
    
    # get the shattering for one neuron
    a = data.frame(x = n)
    shattering = predict(model, a)
    
    # compute the shattering for all neurons
    shattering.all.neurons = shattering ^ num.hyperplanes
    
    return (shattering.all.neurons)
}


estimate.architecture.shattering <- function (n, model1, model2, model3) {
    # compute the shattering coefficient for each layer
    
    hidden.layer1.shattering = estimate.shattering.nnet(n, num.hyperplanes = 9, k=3, R=7, model = model1)
    hidden.layer2.shattering = estimate.shattering.nnet(n, num.hyperplanes = 5, k=3, R=9, model = model2)
    output.layer.shattering = estimate.shattering.nnet(n, num.hyperplanes = 3, k=3, R=5, model = model3)
    return (hidden.layer1.shattering + hidden.layer2.shattering + output.layer.shattering)
}

# regression model for hidden layer 1
model1 = fit.regression.one.neuron(gn = 20, R = 7, k = 3, a = 12, b = 0.1) 

# regression model for hidden layer 2
model2 = fit.regression.one.neuron(gn = 20, R = 9, k = 3, a = 10, b = 0.3)

# regression model for output layer 2
model3 = fit.regression.one.neuron(gn = 20, R = 5, k = 3, a = 10, b = 0.3)

shatterings = NULL
n = 30
for (i in 1:n) {
    shattering = estimate.architecture.shattering(i, model1, model2, model3)    
    shatterings = rbind(shatterings, shattering)
}
shatterings = cbind(1:n, shatterings)
x = shatterings[, 1]
y = shatterings[, 2]
# plot the shatterings with log scale
plot(x, y, xlab = "number of points", ylab = "Log Scale shattering coefficient", log = 'y')
print(shatterings)
