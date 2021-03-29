# mix_comp is a nxk matrix where the [i,j]th point represents
# the probability that the ith data point belongs jth mixture component
cavi_gaussian_mixture <- function(x, prior_variance, means, 
                                  vars, mix_comp){
  n <- length(x)
  num_components <- length(means)
  for(iter in 1:500){
    for(i in 1:n){
      for(k in 1:num_components){
        mix_comp[i,k] <- exp(means[k]*x[i] - (vars[k] + means[k]^2)/2) 
      }
    }
    mix_comp <- t(apply(mix_comp, 1, function(x){x/sum(x)}))
    for(k in 1:num_components){
      means[k] <- sum(mix_comp[,k] * x)/(1/prior_variance + sum(mix_comp[, k]))
      vars[k] <- 1 / (1/prior_variance + sum(mix_comp[, k]))
    }
  }
  return(list("means"=means, "vars"=vars, "mix_comps"=mix_comp))
}

elbo <- function()

generate_gaussian_mixture_data <- function(N, num_comps, mix_comps, means, sds){
  components <- sample(1:num_comps,prob=mix_comps,size=N,replace=TRUE)
  return(rnorm(n=N,mean=means[components],sd=sd[components]))
}

# Real Data 
# N <- 1000
# num_comps <- 2
# mix_comps <- c(0.7,0.3)
# means <- rnorm(num_comps, 0, 7)
# sd <- sqrt(c(5,5))
# x <- generate_gaussian_mixture_data(N, num_comps, mix_comps, means, sds)
# 
# m <- cavi_gaussian_mixture(x, 10, c(-1,1), c(1,1), matrix(rep(c(0.5, 0.5), N), ncol=2))
# 
# pred_means <- m$means
# pred_vars <- m$vars
# pred_mix_component <- c(sum(apply(m$mix_comps, 1, function(x){x[1] > x[2]})), 
#                         N-sum(apply(m$mix_comps, 1, function(x){x[1] > x[2]}))) / N
# 
# x_pred <- generate_gaussian_mixture_data(N, num_comps, pred_mix_component, 
#                                          pred_means, sqrt(pred_vars))
# 
# plot(density(x), col="red")
# lines(density(x_pred), col="blue")


# Real Data
N <- 1000
num_comps <- 2
mix_comps <- c(0.7, 0.3) 
prior_variance <- 100
means <- rnorm(num_comps, 0, sqrt(prior_variance))
sd <- sqrt(c(5,5))
x <- generate_gaussian_mixture_data(N, num_comps, mix_comps, means, sd)

# Initial Parameters
init_means <- c(-1, 1)
init_vars <- c(1, 1)
init_mix_comps <- matrix(rep(c(0.5, 0.5), N), ncol=2)


m <- cavi_gaussian_mixture(x, 10, c(-1,1), c(1,1), matrix(rep(c(0.5, 0.5), N), ncol=2))

pred_means <- m$means
pred_vars <- m$vars
pred_mix_component <- c(sum(apply(m$mix_comps, 1, function(x){x[1] > x[2]})),
                        N-sum(apply(m$mix_comps, 1, function(x){x[1] > x[2]}))) / N

x_pred <- generate_gaussian_mixture_data(N, num_comps, pred_mix_component,
                                         pred_means, sqrt(pred_vars))

plot(density(x), col="red")
lines(density(x_pred), col="blue")

