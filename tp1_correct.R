
estimate.shattering <- function(gn=15, num.hyperplanes = 2, k=2, R=2) {
    
    Shattering = NULL
    
    for (n in 1:gn) {
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
        
        num.ways = length(HashTable)
        
        Shattering = rbind(Shattering, c(n, num.ways))
    }
    
    return (Shattering)
}

