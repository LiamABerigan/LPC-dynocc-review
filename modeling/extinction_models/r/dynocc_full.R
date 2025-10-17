library(tidyverse)
library(here)
library(jagsUI)

database <- read_csv(here("covariates", "lek_covariates", "leks_censored_2025_2_11.csv"))

# Get LPC count for each lek and year
database_leks <- database %>% 
  dplyr::select(Route, route_leknum, Year, Date, count, pct_crp_est, pct_grass_est, real) %>% 
  mutate(count = if_else(real, count, 0)) %>% #if using fake data, set count value to zero
  filter(Year >= 1985 & Year <= 2021)

# Nest datasets and modify as needed
database_nested <- database_leks %>% 
  group_by(Route) %>% 
  nest()

database_nested$count_matrix_1 <- map(database_nested$data, function(x){
  
  # condense into one row per year
  y <- x %>% 
    dplyr::select(-Date) %>% 
    group_by(route_leknum, Year) %>% 
    summarise(count = list(count), 
              pct_crp_est = first(pct_crp_est), 
              pct_grass_est = first(pct_grass_est),
              real = first(real)) %>% 
    ungroup()
  
  #pull the first count from that year
  y$count <- map(y$count, function(x){x[[1]]}) %>% 
    as.numeric()
  # for second and third, return NA if a second and third record doesn't exist (should be most)
  
  iter <- y %>% #unfold
    dplyr::select(-pct_crp_est, -pct_grass_est, -real) %>% 
    arrange(Year) %>% 
    pivot_wider(names_from = Year, values_from = count) %>% 
    dplyr::select(-route_leknum)
  
  # if the first obs is not in the first column, then ensure that the first obs is consistent for the fake data
  if(is.na(iter[1,1])){ #if the first column is na, indicating that sampling started after the first interval
    last_na <- iter[1,1:37] %>% #all columns before this should be NA
      is.na()
    last_na <- !last_na
    last_na <- last_na %>% 
      which() %>% 
      min()
    last_na <- last_na - 1
    
    iter[,1:last_na] <- NA
  }
  return(iter)
})

database_nested$count_matrix_2 <- map(database_nested$data, function(x){
  
  # condense into one row per year
  y <- x %>% 
    dplyr::select(-Date) %>% 
    group_by(route_leknum, Year) %>% 
    summarise(count = list(count), 
              pct_crp_est = first(pct_crp_est), 
              pct_grass_est = first(pct_grass_est),
              real = first(real)) %>% 
    ungroup()
  
  #pull the first count from that year
  y$count <- map(y$count, function(x){
    if(length(x) >= 2) {
      return(x[[2]])
    } else {
      return(NA)
    }
  }) %>% 
    as.numeric()
  # for second and third, return NA if a second and third record doesn't exist (should be most)
  
  iter <- y %>% #unfold
    dplyr::select(-pct_crp_est, -pct_grass_est, -real) %>% 
    arrange(Year) %>% 
    pivot_wider(names_from = Year, values_from = count) %>% 
    dplyr::select(-route_leknum)
  
  return(iter)
})

database_nested$crp_est <- map(database_nested$data, function(x){
  y <- x %>% 
    dplyr::select(-Date) %>% 
    group_by(route_leknum, Year) %>% 
    summarise(count = list(count), 
              pct_crp_est = first(pct_crp_est), 
              pct_grass_est = first(pct_grass_est),
              real = first(real)) %>% 
    ungroup()
  
  iter <- y %>% 
    dplyr::select(-count, -pct_grass_est, -real) %>%  
    arrange(Year) %>% 
    pivot_wider(names_from = Year, values_from = pct_crp_est)  %>% 
    dplyr::select(-route_leknum)
  
  return(iter)
})

database_nested$grass_est <- map(database_nested$data, function(x){
  y <- x %>% 
    dplyr::select(-Date) %>% 
    group_by(route_leknum, Year) %>% 
    summarise(count = list(count), 
              pct_crp_est = first(pct_crp_est),
              pct_grass_est = first(pct_grass_est), 
              real = first(real)) %>% 
    ungroup()
  
  iter <- y %>% 
    dplyr::select(-count, -pct_crp_est, -real) %>%  
    arrange(Year) %>% 
    pivot_wider(names_from = Year, values_from = pct_grass_est)  %>% 
    dplyr::select(-route_leknum)
  
  return(iter)
})

database_nested$binary_matrix_1 <- map(database_nested$count_matrix_1, function(x){
  target <- x
  for (i in 1:nrow(x)){
    for(j in 1:ncol(x)){
      target[i,j] <- if_else(x[i,j] > 0, 1, 0)
    }
  }
  return(target)
})

database_nested$binary_matrix_2 <- map(database_nested$count_matrix_2, function(x){
  target <- x
  for (i in 1:nrow(x)){
    for(j in 1:ncol(x)){
      target[i,j] <- if_else(x[i,j] > 0, 1, 0)
    }
  }
  return(target)
})

database_nested$first_obs <- map_dbl(database_nested$count_matrix_1, function(x){
  if(any(is.na(x[1,1:37]))){
    last_na <- x[1,1:37] %>% #all columns before this should be NA
      is.na()
    last_na <- !last_na
    last_na <- last_na %>% 
      which() %>% 
      min()
    last_na <- last_na - 1
  } else {
    last_na <- 0
  }
  return(last_na + 1)
})

database_nested$n_obs <- map_dbl(database_nested$binary_matrix_1, function(x){
  nrow(x)
})

# unfold the nested dataframe into a series of arrays
binary_array_1 <- array(data = NA, dim = c(max(database_nested$n_obs), #leks
                                           ncol(database_nested$binary_matrix_1[[1]]), #years
                                           nrow(database_nested))) #routes
for(i in 1:dim(binary_array_1)[1]){
  for(j in 1:dim(binary_array_1)[2]){
    for(k in 1:dim(binary_array_1)[3]){
      binary_array_1[i,j,k] <- as.numeric(database_nested$binary_matrix_1[[k]][i,j])
    }
  }
}

binary_array_2 <- array(data = NA, dim = c(max(database_nested$n_obs), ncol(database_nested$binary_matrix_1[[1]]), nrow(database_nested)))
for(i in 1:dim(binary_array_2)[1]){
  for(j in 1:dim(binary_array_2)[2]){
    for(k in 1:dim(binary_array_2)[3]){
      binary_array_2[i,j,k] <- as.numeric(database_nested$binary_matrix_2[[k]][i,j])
    }
  }
}

crp_est_array <- array(data = NA, dim = c(max(database_nested$n_obs), ncol(database_nested$binary_matrix_1[[1]]), nrow(database_nested)))
for(i in 1:dim(binary_array_1)[1]){
  for(j in 1:dim(binary_array_1)[2]){
    for(k in 1:dim(binary_array_1)[3]){
      crp_est_array[i,j,k] <- as.numeric(database_nested$crp_est[[k]][i,j])
    }
  }
}

grass_est_array <- array(data = NA, dim = c(max(database_nested$n_obs), ncol(database_nested$binary_matrix_1[[1]]), nrow(database_nested)))
for(i in 1:dim(binary_array_1)[1]){
  for(j in 1:dim(binary_array_1)[2]){
    for(k in 1:dim(binary_array_1)[3]){
      grass_est_array[i,j,k] <- as.numeric(database_nested$grass_est[[k]][i,j])
    }
  }
}

# scale between 0 and 1
crp_est_array <- crp_est_array/100

grass_est_array <- grass_est_array/100

## build an array structure for the forecasting
crp_sim <- array(data = NA, dim = c(max(database_nested$n_obs), # leks,
                                    nrow(database_nested), #routes
                                    6)) #forecasts

# CRP in each forecast is modified from the CRP surrounding each lek in 2021
for(i in 1:dim(crp_sim)[1]){
  for(k in 1:dim(crp_sim)[2]){
    # simulation 1: normal circumstances
    crp_sim[i,k,1] <- crp_est_array[i,37,k] 
    
    # simulation 2: 20% decline in CRP
    crp_sim[i,k,2] <- crp_est_array[i,37,k] * 0.8
    
    # simulation 3: 50% decline in CRP
    crp_sim[i,k,3] <- crp_est_array[i,37,k] * 0.5
    
    # simulation 4: 20% increase in CRP
    crp_sim[i,k,4] <- crp_est_array[i,37,k] * 1.2
    
    # simulation 5: 50% increase in CRP
    crp_sim[i,k,5] <- crp_est_array[i,37,k] * 1.5
    
    # simulation 6: no CRP remaining
    crp_sim[i,k,6] <- 0
  }
}

sink(here("modeling", "extinction_models", "jags", "dynocc_full.jags"))
cat("model{
## Likelihood
for(k in 1:n_routes){
  for(i in 1:n_obs[k]){
    # defining initial occupancy as a function of a route-specific psi
    binary_real[i,first_obs[k],k] ~ dbern(psi[k])
    
    for(j in first_obs[k]:n_col){ #years
      # detection model
      binary_obs_1[i,j,k] ~ dbern(ifelse(binary_real[i,j,k] == 1, p, 0)) # if alive, then probability of detection. Else 0.
      binary_obs_2[i,j,k] ~ dbern(ifelse(binary_real[i,j,k] == 1, p, 0)) # note multiple observations per year. Here assuming that the within-year sampling period is a closed interval.
    }
    
    #fix NA values of binary_real to unavailable for purposes of summing the number of available leks per year.
    for(j in 1:first_obs[k]){
      available[i,j,k] <- 0
    }
    
    for(j in (first_obs[k]+1):n_col){
      # determine whether a lek is unoccupied
      available[i,j,k] <- 1 - binary_real[i,j-1,k] # is this lek currently unoccupied? If so, consider it to be available (1)
    
      # dynamic occupancy model
      binary_real[i,j,k] ~ dbern(available[i,j,k] * gamma[i,j,k] + (1 - available[i,j,k]) * phi[i,j,k]) # if formerly 0, colonization. Else, persistence
      logit(phi[i,j,k]) <- alpha_0 + alpha_1 * crp_est[i,j,k] + alpha_2 * grass_est[i,j,k] # persistence is solely the function of CRP
      logit(gamma[i,j,k]) <- beta_0 + beta_1 * crp_est[i,j,k] + beta_2 * grass_est[i,j,k] + beta_3 * sum(available[1:n_obs[k],j,k]) # colonization is the function of CRP and the number of leks still available to be colonized (see Kéry and Schaub 2012 for explanation why this is necessary)
    }
  }
}

## Priors
# Presence on initial survey (psi as a function of route)
for(k in 1:n_routes){
  psi[k] ~ dunif(0, 1)
}

# Persistence
alpha_0 ~ dnorm(0, 50^-2)
alpha_1 ~ dnorm(0, 50^-2)
alpha_2 ~ dnorm(0, 50^-2)

# Colonization
beta_0 ~ dnorm(0, 50^-2)
beta_1 ~ dnorm(0, 50^-2)
beta_2 ~ dnorm(0, 50^-2)
beta_3 ~ dnorm(0, 50^-2)

# Detection
p ~ dunif(0,1)

## Forecast
for(k in 1:n_routes){
  for(i in 1:n_obs[k]){
    for(l in 1:6){
      # specifying initial occupancy using the 2021 occupancy from the real data
      binary_forecast[i,1,k,l] = binary_real[i,n_col,k]
      
      for(j in 2:16){ # forecasted years
        # determine whether a lek is unoccupied
        available_forecast[i,j,k,l] <- 1 - binary_forecast[i,j-1,k,l]
        
        # dynamic occupancy model
        binary_forecast[i,j,k,l] ~ dbern(available_forecast[i,j,k,l] * gamma_forecast[i,j,k,l] + (1 - available_forecast[i,j,k,l]) * phi_forecast[i,j,k,l])
        logit(phi_forecast[i,j,k,l]) <- alpha_0 + alpha_1 * crp_forecast[i,k,l] + alpha_2 * grass_est[i,n_col,k] # take the simulated crp cover, and the grass cover from 2021 (latest year)
        logit(gamma_forecast[i,j,k,l]) <- beta_0 + beta_1 * crp_forecast[i,k,l] + beta_2 * grass_est[i,n_col,k] + beta_3 * sum(available_forecast[1:n_obs[k],j,k,l])
      }
    }
  }
}

#sum total of leks per year
for(l in 1:6){ #forecasts
  for(j in 1:16){
    lpy_forecast[j,l] <- sum(b_forecast[j,,l])
    for(k in 1:n_routes){
      b_forecast[j,k,l] <- sum(binary_forecast[1:n_obs[k],j,k,l])
    }
  }
}

}")
sink()

# prepping jags
nc <- 3
ni <- 10000
nb <- 1000
nt <- 1

parameters <- c("psi", "alpha_0", "alpha_1", "alpha_2", "beta_0", "beta_1", "beta_2", "beta_3", "p", "lpy_forecast")

### Initial values
binary.init <- array(data = NA, dim = c(max(database_nested$n_obs), ncol(database_nested$binary_matrix_1[[1]]), nrow(database_nested)))

for(k in 1:nrow(database_nested)){
  for(i in 1:database_nested$n_obs[k]){
    for(j in database_nested$first_obs[k]:dim(binary_array_1)[2]){
      binary.init[i,j,k] <- 1 # set the initial value to 1 (occupied) for everything. Avoids issues where the likelihood can't compute.
    }
  }
}

inits <- function(){ list(
  binary_real = binary.init
)}

jags_data <- list(
  binary_obs_1 = binary_array_1,
  binary_obs_2 = binary_array_2,
  first_obs = database_nested$first_obs,
  n_obs = database_nested$n_obs,
  n_routes = nrow(database_nested),
  n_col = dim(binary_array_1)[2],
  crp_est = crp_est_array,
  grass_est = grass_est_array,
  crp_forecast = crp_sim
)

set.seed(20)
m_results <- jags(data=jags_data, parameters.to.save = parameters,
                  inits = inits,
                  model.file=here("modeling", "extinction_models", "jags", "dynocc_full.jags"),
                  n.chains=nc, n.iter=ni, n.burnin=nb, n.thin=nt, parallel=F)

print(m_results)
jagsUI::traceplot(m_results)

saveRDS(m_results, here("modeling", "extinction_models", "results", "dynocc_full.rds"))

