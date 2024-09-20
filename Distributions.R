
  # Distribution class
  Distribution <- R6Class(
    "Distribution",
    public = list(
      name = NULL,
      parameters = NULL,
      fit_function = NULL,
      model = NULL,
      generate_signal = NULL,
      
      initialize = function(name,
                            parameters,
                            fit_function,
                            generate_signal) {
        self$name <- name
        self$parameters <- parameters
        self$fit_function <- fit_function
        self$generate_signal <- generate_signal
      },
      
      set_parameters = function(parameters) {
        self$parameters <- parameters
      },
      
      # Function to fit data to model
      fit = function(x, y) {
        start_params <- unlist(lapply(self$parameters, function(p)
          mean(p)))
        self$model <- self$fit_function(x, y, start_params)
      },
      
      check_parameters = function(param_values) {
        within_ranges <- sapply(names(param_values), function(p) {
          param_values[p] >= self$parameters[[p]][1] && param_values[p] <= self$parameters[[p]][2]
        })
        return(all(within_ranges))
      },
      
      check_fit = function(){
        estimated_parameters <- coef(self$model)
        within_ranges <- sapply(names(estimated_parameters), function(p) {
          estimated_parameters[p] >= (self$parameters[[p]][1] * 0.9) && estimated_parameters[p] <= (self$parameters[[p]][2] * 0.9)
        })
        if(all(within_ranges)) {
          return(TRUE)
        } else {
          return(FALSE)
        }
      },
      
      get_metrics = function() {
        if (is.null(self$model)) {
          stop("Model not fitted yet. Please run the `fit` method.")
        }
        AIC_val <- AIC(self$model)
        BIC_val <- BIC(self$model)
        RMSE_val <- sqrt(mean(residuals(self$model) ^ 2))
        metrics <- list(AIC = AIC_val, BIC = BIC_val, RMSE = RMSE_val)
        return(metrics)
      }
    )
  )

########################## BETA ##########################
# Beta distribution fitting function using nlsLM
beta_fit_function <- function(x, y, start_params) {
  # DA LI KORISTITI I NON-CENTRALITY PARAMETER
  tryCatch({
    model <- nlsLM(
      y ~ pbeta(x, shape1, shape2),
      start = list(shape1 = mean(start_params[1]),
                   shape2 = mean(start_params[2]))
      # ,
      # lower = c(start_params[[1]][1], start_params[[2]][1]),
      # upper = c(start_params[[1]][2], start_params[[2]][2])
    )
    return(model)
  }, error = function(e) {
    NULL
  })
  
  
  return(NULL)
}

# generate_signal function for the Beta distribution
beta_generate_signal <- function(num_samples, params) {
  # Extract the shape1 and shape2 parameter values
  shape1 <- params$shape1
  shape2 <- params$shape2
  # Generate a signal based on the Beta distribution
  signal <- rbeta(num_samples, shape1, shape2)
  return(signal)
}

# Create a Beta distribution object
beta_dist <- Distribution$new(
  name = "beta",
  parameters = list(shape1 = c(0, 5), shape2 = c(0, 5)),
  fit_function = beta_fit_function,
  generate_signal = beta_generate_signal
)
# Fit the distribution to data
# beta_dist$fit(x,y)
# Get the metrics
# metrics <- beta_dist$get_metrics()
# print(metrics)


########################## EXPONENTIAL ##########################
# Exponential distribution fitting function using nlsLM
exponential_fit_function <- function(x, y, start_params) {
  tryCatch({
    model <- nlsLM(
      y ~ pexp(x, rate),
      start = list(rate = mean(start_params[1]))
      # ,
      # lower = c(start_params[[1]][1]),
      # upper = c(start_params[[1]][2])
    )
    return(model)
  }, error = function(e) {
    NULL
  })
  
  return(NULL)
}

# Define the generate_signal function for the exponential distribution
exponential_generate_signal <- function(num_samples, params) {
  rate <- params$rate
  # Generate a signal based on the exponential distribution
  signal <- rexp(num_samples, rate)
  
  return(signal)
}

# Create an exponential distribution object
exponential_dist <- Distribution$new(
  name = "exponential",
  parameters = list(rate = c(1, 10)),
  fit_function = exponential_fit_function,
  generate_signal = exponential_generate_signal
)
# Fit the distribution to data
# exponential_dist$fit(x, y)
# # Get the metrics
# metrics <- exponential_dist$get_metrics()
# print(metrics)


########################## GAMMA ##########################
# Gamma distribution fitting function using nlsLM
gamma_fit_function <- function(x, y, start_params) {
  tryCatch({
    model <- nlsLM(y ~ pgamma(x, shape, scale),
                   start = list(
                     shape = mean(start_params[1]),
                     scale = mean(start_params[2])
                   )
                   # ,
                   # lower = c(start_params[[1]][1], start_params[[2]][1]), 
                   # upper = c(start_params[[1]][2], start_params[[2]][2]) 
                   )
    return(model)
  }, error = function(e) {
    NULL
  })
  
  
  return(NULL)
}

# Define the generate_signal function for the Gamma distribution
gamma_generate_signal <- function(num_samples, params) {
  # Extract the shape and scale parameter values
  shape <- params$shape
  scale <- params$scale
  # Generate a signal based on the Gamma distribution
  signal <- rgamma(num_samples, shape, scale)
  return(signal)
}

# Create a gamma distribution object
gamma_dist <- Distribution$new(
  name = "gamma",
  parameters = list(shape = c(1, 10), scale = c(1, 10)),
  fit_function = gamma_fit_function,
  generate_signal = gamma_generate_signal
)
# Fit the distribution to data
# gamma_dist$fit(x,y)
# # Get the metrics
# metrics <- gamma_dist$get_metrics()
# print(metrics)


########################## GEV ##########################
library(evd)
# GEV distribution fitting function using nlsLM
gev_fit_function <- function(x, y, start_params) {
  tryCatch({
    model <- nlsLM(y ~ pgev(x, loc, scale, shape),
                   start = list(
                     loc = mean(start_params[1]),
                     scale = mean(start_params[2]),
                     shape = mean(start_params[3])
                   )
                   # ,
                   # lower = c(start_params[[1]][1], start_params[[2]][1], start_params[[3]][1]), 
                   # upper = c(start_params[[1]][2], start_params[[2]][2], start_params[[3]][2]) 
                   )
    return(model)
  }, error = function(e) {
    NULL
  })
  
  return(NULL)
}

# Define the generate_signal function for the GEV distribution
gev_generate_signal <- function(num_samples, params) {
  # Extract the location, scale, and shape parameter values
  loc <- params$loc
  scale <- params$scale
  shape <- params$shape
  # Generate a signal based on the GEV distribution
  signal <-
    rgev(num_samples, loc, scale, shape)
  return(signal)
}

# Create a GEV distribution object
gev_dist <- Distribution$new(
  name = "gev",
  parameters = list(
    loc = c(-10, 10),
    scale = c(1, 10),
    shape = c(1, 5)
  ),
  fit_function = gev_fit_function,
  generate_signal = gev_generate_signal
)
# Fit the distribution to data
# gev_dist$fit(x, y)
# # Get the metrics
# metrics <- gev_dist$get_metrics()
# print(metrics)


########################## GUMBEL ##########################
# Gumbel distribution fitting function using nlsLM
gumbel_fit_function <- function(x, y, start_params) {
  tryCatch({
    model <- nlsLM(y ~ pgumbel(x, loc, scale),
                   start = list(
                     loc = mean(start_params[1]),
                     scale = mean(start_params[2])
                   )
                   # ,
                   # lower = c(start_params[[1]][1], start_params[[2]][1]), 
                   # upper = c(start_params[[1]][2], start_params[[2]][2]) 
                   )
    return(model)
  }, error = function(e) {
    NULL
  })
  
  return(NULL)
}

gumbel_generate_signal <- function(num_samples, params) {
  # Extract the location and scale parameter values
  loc <- params$loc
  scale <- params$scale
  # Generate a signal based on the Gumbel distribution
  signal <- rgumbel(num_samples, loc, scale)
  return(signal)
}

# Create a Gumbel distribution object
gumbel_dist <- Distribution$new(
  name = "gumbel",
  parameters = list(loc = c(-10, 10), scale = c(1, 10)),
  fit_function = gumbel_fit_function,
  generate_signal = gumbel_generate_signal
)
# Generate Gumbel-distributed data
# data <-  rgumbel(n = 1000, location = 0, scale = 1)
# Fit the distribution to data
# gumbel_dist$fit(x,y)
# # Get the metrics
# metrics <- gumbel_dist$get_metrics()
# print(metrics)


########################## LOGISTIC ##########################
# Logistic distribution fitting function using nlsLM
logistic_fit_function <- function(x, y, start_params) {
  tryCatch({
    model <- nlsLM(y ~ plogis(x, location, scale),
                   start = list(
                     location = mean(start_params[1]),
                     scale = mean(start_params[2])
                   )
                   # ,
                   # lower = c(start_params[[1]][1], start_params[[2]][1]), 
                   # upper = c(start_params[[1]][2], start_params[[2]][2]) 
                   )
    return(model)
  }, error = function(e) {
    NULL
  })
  
  return(NULL)
}

logistic_generate_signal <- function(num_samples, params) {
  # Extract the location and scale parameter values
  location <- params$location
  scale <- params$scale
  # Generate a signal based on the Logistic distribution
  signal <- rlogis(num_samples, location, scale)
  return(signal)
}

# Create a Logistic distribution object
logistic_dist <- Distribution$new(
  name = "logistic",
  parameters = list(location = c(-10, 10), scale = c(1, 10)),
  fit_function = logistic_fit_function,
  generate_signal = logistic_generate_signal
)
# Generate Logistic-distributed data
# rlogis(n, location = m, scale = s)
# Fit the distribution to data
# logistic_dist$fit(x, y)
# # Get the metrics
# metrics <- logistic_dist$get_metrics()
# print(metrics)


########################## LOGNORMAL ##########################
# Lognormal distribution fitting function using nlsLM
lognormal_fit_function <- function(x, y, start_params) {
  tryCatch({
    model <-
      nlsLM(
        y ~ plnorm(x, meanlog, sdlog),
        start = list(meanlog = mean(start_params[1]),
                     sdlog = mean(start_params[2]))
        # ,
        # lower = c(start_params[[1]][1], start_params[[2]][1]), 
        # upper = c(start_params[[1]][2], start_params[[2]][2]) 
      )
    return(model)
  }, error = function(e) {
    NULL
  })
  
  return(NULL)
}

lognormal_generate_signal <- function(num_samples, params) {
  # Extract the mean and standard deviation parameter values
  
  meanlog <- params$meanlog
  sdlog <- params$sdlog
  # Generate a signal based on the Lognormal distribution
  signal <- rlnorm(num_samples, meanlog, sdlog)
  return(signal)
}

# Create a Lognormal distribution object
lognormal_dist <- Distribution$new(
  name = "lognormal",
  parameters = list(meanlog = c(0, 10), sdlog = c(0, 2)),
  fit_function = lognormal_fit_function,
  generate_signal = lognormal_generate_signal
)
# Fit the distribution to data
# lognormal_dist$fit(x,y)
# # Get the metrics
# metrics <- lognormal_dist$get_metrics()
# print(metrics)


########################## NAKAGAMI ##########################
# Nakagami distribution fitting function using nlsLM
nakagami_fit_function <- function(x, y, start_params) {
  tryCatch({
    # In the Nakagami distribution, the shape parameter is related to
    # mu and the scale parameter is related to omega as follows:
    # shape = mu^2 / omega^2
    # scale = omega^2 / mu
    # m is the shape parameter
    model <-
      nlsLM(y ~ pnaka(x, scale, shape),
            start = list(
              scale = mean(start_params[1]),
              shape = mean(start_params[2])
            )
            # ,
            # lower = c(start_params[[1]][1], start_params[[2]][1]), 
            # upper = c(start_params[[1]][2], start_params[[2]][2]) 
            )
    
    return(model)
  }, error = function(e) {
    NULL
  })
  
  return(NULL)
}

nakagami_generate_signal <- function(num_samples, params) {
  # Extract the shape and scale parameter values
  scale <- params$scale
  shape <- params$shape
  # Generate a signal based on the Nakagami distribution
  signal <- rnaka(num_samples, scale, shape)
  return(signal)
}

# Create a Nakagami distribution object
nakagami_dist <- Distribution$new(
  name = "nakagami",
  parameters = list(scale = c(1, 10), shape = c(0.5, 5)),
  fit_function = nakagami_fit_function,
  generate_signal = nakagami_generate_signal
)
# Fit the distribution to data
# nakagami_dist$fit(x,y)
# # Get the metrics
# metrics <- nakagami_dist$get_metrics()
# print(metrics)


########################## NORMAL ##########################
# Normal distribution fitting function using nlsLM
normal_fit_function <- function(x, y, start_params) {
  tryCatch({
    model <-
      nlsLM(y ~ pnorm(x, mean, sd), start = list(
        mean = mean(start_params[1]),
        sd = mean(start_params[2])
      )
      # ,
      # lower = c(start_params[[1]][1], start_params[[2]][1]), 
      # upper = c(start_params[[1]][2], start_params[[2]][2]) 
      )
    return(model)
  }, error = function(e) {
    NULL
  })
  
  return(NULL)
}

normal_generate_signal <- function(num_samples, params) {
  # Extract the mean and standard deviation parameter values
  mean <- params$mean
  sd <- params$sd
  # Generate a signal based on the Normal distribution
  signal <- rnorm(num_samples, mean, sd)
  return(signal)
}

# Create a normal distribution object
normal_dist <- Distribution$new(
  name = "normal",
  parameters = list(mean = c(0, 10), sd = c(0, 5)),
  fit_function = normal_fit_function,
  generate_signal = normal_generate_signal
)
# Fit the distribution to data
# normal_dist$fit(x,y)
# # Get the metrics
# metrics <- normal_dist$get_metrics()
# print(metrics)


########################## PARETO ##########################
# Pareto distribution fitting function using nlsLM
pareto_fit_function <- function(x, y, start_params) {
  tryCatch({
    model <- nlsLM(y ~ ppareto(x, shape, scale),
                   start = list(
                     shape = mean(start_params[1]),
                     scale = mean(start_params[2])
                   )
                   # ,
                   # lower = c(start_params[[1]][1], start_params[[2]][1]), 
                   # upper = c(start_params[[1]][2], start_params[[2]][2]) 
    )
    return(model)
    
  }, error = function(e) {
    NULL
  })
  
  return(NULL)
}

# Define the generate_signal function for the Pareto distribution
pareto_generate_signal <- function(num_samples, params) {
  # Extract the shape and scale parameter values
  shape <- params$shape
  scale <- params$scale
  # Generate a signal based on the Pareto distribution
  signal <- rpareto(num_samples, shape, scale)
  return(signal)
}

# Create a pareto distribution object
pareto_dist <- Distribution$new(
  name = "pareto",
  parameters = list(shape = c(1, 10), scale = c(1, 10)),
  fit_function = pareto_fit_function,
  generate_signal = pareto_generate_signal
)
# Fit the distribution to data
# pareto_dist$fit(x,y)
# # Get the metrics
# metrics <- pareto_dist$get_metrics()
# print(metrics)



########################## RAYLEIGH ##########################
# Rayleigh distribution fitting function using nlsLM
rayleigh_fit_function <- function(x, y, start_params) {
  tryCatch({
    model <- nlsLM(
      # y ~ (1 - exp(-((x - a) / b) ^ 2)),
      y ~ prayleigh(x, scale),
      start = list(scale = mean(start_params[1])#,
                   #b = mean(start_params[2])
                   ),
    #  lower = c(start_params[[1]][1]), #, start_params[[2]][1]), 
    #  upper = c(start_params[[1]][2]), # start_params[[2]][2]), 
                   control = list(maxiter = 500)
      )
      return(model)
      
      # PROVERITI:
      # f(x) = (x / sigma^2) * exp(-x^2 / (2 * sigma^2))
  }, error = function(e) {
    NULL
  })
    return(NULL)
}

rayleigh_generate_signal <- function(num_samples, params) {
  # Extract the sigma parameter value
  scale <- params$scale
  # Generate a signal based on the Rayleigh distribution
  signal <- rrayleigh(num_samples, scale)
  return(signal)
}

# Create a Rayleigh distribution object
rayleigh_dist <- Distribution$new(
  name = "rayleigh",
  parameters = list(scale = c(1, 10)),
  fit_function = rayleigh_fit_function,
  generate_signal = rayleigh_generate_signal
)
# Fit the distribution to data
# rayleigh_dist$fit(x,y)
# # Get the metrics
# metrics <- rayleigh_dist$get_metrics()
# print(metrics)


########################## RICIAN ##########################
# Rician distribution fitting function using nlsLM
rician_fit_function <- function(x, y, start_params) {
  tryCatch({
    model <- nlsLM(y ~ price(x, sigma, vee),
                   start = list(
                     sigma = mean(start_params[1]),
                     vee = mean(start_params[2])
                   )#,
                #   lower = c(start_params[[1]][1], start_params[[2]][1]), 
                #  upper = c(start_params[[1]][2], start_params[[2]][2]) 
                )
    
    return(model)
    
  }, error = function(e) {
    NULL
  })
  
  return(NULL)
}

rician_generate_signal <- function(num_samples, params) {
  # Extract the nu and sigma parameter values
  sigma <- params$sigma
  vee <- params$vee
  # Generate a signal based on the Rician distribution
  signal <- rrice(num_samples, sigma, vee)
  return(signal)
}

# Create a Rician distribution object
rician_dist <- Distribution$new(
  name = "rician",
  parameters = list(sigma = c(0, 5), vee = c(0, 5)),
  fit_function = rician_fit_function,
  generate_signal = rician_generate_signal
)
# Generate Rician-distributed data
#data <- rnorm(100, 0, 1)
# Fit the distribution to data
# rician_dist$fit(x,y)
# # Get the metrics
# metrics <- rician_dist$get_metrics()
# print(metrics)


########################## UNIFROM ##########################
# Uniform distribution fitting function using nlsLM
uniform_fit_function <- function(x, y, start_params) {
  tryCatch({
    model <- nlsLM(y ~ punif(x, min, max),
                   start = list(
                     min = mean(start_params[1]),
                     max = mean(start_params[2])
                   )
                   # ,
                   # lower = c(start_params[[1]][1], start_params[[2]][1]), 
                   # upper = c(start_params[[1]][2], start_params[[2]][2]) 
                   )
    return(model)
  }, error = function(e) {
    NULL
  })
  
  return(NULL)
}

uniform_generate_signal <- function(num_samples, params) {
  # Extract the minimum and maximum parameter values
  min <- params$min
  max <- params$max
  # Generate a signal based on the Uniform distribution
  signal <- runif(num_samples, min, max)
  return(signal)
}

# Create a Uniform distribution object
uniform_dist <- Distribution$new(
  name = "uniform",
  parameters = list(min = c(-1, 1), max = c(1, 10)),
  fit_function = uniform_fit_function,
  generate_signal = uniform_generate_signal
)
# Generate Uniform-distributed data
# runif(1000, min = 0, max = 1)
# Fit the distribution to data
# uniform_dist$fit(x,y)
# # Get the metrics
# metrics <- uniform_dist$get_metrics()
# print(metrics)


########################## WEIBULL ##########################
# Weibull distribution fitting function using nlsLM
weibull_fit_function <- function(x, y, start_params) {
  tryCatch({
    model <- nlsLM(y ~ pweibull(x, shape, scale),
                   # nlsLM(y ~ (1 - exp(-((x / b) ^ c))),
                   start = list(
                     shape = mean(start_params[1]),
                     scale = mean(start_params[2])
                   )
                   # ,
                   # lower = c(start_params[[1]][1], start_params[[2]][1]), 
                   # upper = c(start_params[[1]][2], start_params[[2]][2]) 
                   )
    
    return(model)
    
  }, error = function(e) {
    NULL
  })
  
  return(NULL)
}

weibull_generate_signal <- function(num_samples, params) {
  # Extract the shape and scale parameter values
  shape <- params$shape
  scale <- params$scale
  # Generate a signal based on the Weibull distribution
  signal <- rweibull(num_samples, shape, scale)
  return(signal)
}

# Create a Weibull distribution object
weibull_dist <- Distribution$new(
  name = "weibull",
  parameters = list(shape = c(0.5, 3), scale = c(1, 10)),
  fit_function = weibull_fit_function,
  generate_signal = weibull_generate_signal
)
# Fit the distribution to data
# weibull_dist$fit(x,y)
# # Get the metrics
# metrics <- weibull_dist$get_metrics()
# print(metrics)



########################## HOYT ##########################

########################## BINOMIAL ##########################
# # Binomial distribution fitting function using nlsLM
# binomial_fit_function <- function(x, y, start_params) {
#   # Calculate initial values for size and prob based on the data
#   # init_size <- round(length(s) * 0.9)
#   # init_prob <- mean(s) / init_size
#   model <- nlsLM(y ~ pbinom(x, size, prob),
#                  start = list( size = 100, prob = 0.5))
#   return(model)
# }
# # Create a Binomial distribution object
# binomial_dist <- Distribution$new(
#   name = "binomial",
#   parameters = list(prob = c(0.001, 1), size = c(1, 1000)),
#   fit_function = binomial_fit_function
# )
# # Generate Binomial-distributed data
# # rbinom(1000, size = 1000, prob = 0.5)
# # Fit the distribution to data
# binomial_dist$fit(x, y)
# # Get the metrics
# metrics <- binomial_dist$get_metrics()
# print(metrics)


# Map distribution names to their corresponding objects
distribution_map <- list(
  "Beta" = beta_dist,
  "Exponential" = exponential_dist,
  "Gamma" = gamma_dist,
  "General Extreme Value (GEV)" = gev_dist,
  "Gumbel" = gumbel_dist,
  "Logistic" = logistic_dist,
  "Lognormal" = lognormal_dist,
  "Nakagami-m" = nakagami_dist,
  "Normal" = normal_dist,
  "Pareto" = pareto_dist,
#  "Poisson" = poisson_dist,
  "Rayleigh" = rayleigh_dist,
  "Rician" = rician_dist,
  "Uniform" = uniform_dist,
  "Weibull" = weibull_dist
)


########################## POISSON ##########################
# # Poisson distribution fitting function using nlsLM
# poisson_fit_function <- function(x, y, start_params) {
#   tryCatch({
#     model <- nlsLM(y ~ ppois(x, lambda),
#                    start = list(lambda = mean(start_params[1]))
#                    # ,
#                    # lower = c(start_params[[1]][1], start_params[[2]][1]), 
#                    # upper = c(start_params[[1]][2], start_params[[2]][2]) 
#                    )
#     
#     # lambda is a vector of (non-negative) means
#     return(model)
#   }, error = function(e) {
#     NULL
#   })
#   
#   return(NULL)
# }
# 
# poisson_generate_signal <- function(num_samples, params) {
#   # Extract the lambda parameter value
#   lambda <- params$lambda
#   # Generate a signal based on the Poisson distribution
#   signal <- rpois(num_samples, lambda)
#   return(signal)
# }
# 
# # Create a Poisson distribution object
# poisson_dist <- Distribution$new(
#   name = "poisson",
#   parameters = list(lambda = c(0, 100)),
#   fit_function = poisson_fit_function,
#   generate_signal = poisson_generate_signal
# )
# # Generate Poisson-distributed data
# # rpois(1000, lambda = 3)
# # Fit the distribution to data
# # poisson_dist$fit(x, y)
# # # Get the metrics
# metrics <- poisson_dist$get_metrics()
# print(metrics)

