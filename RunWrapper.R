#Import libraries and source additional functions
library(dplyr)
library(rslurm)

source("SimFunctions.R")

# Define the simulation conditions
simulation_conditions <- data.frame(
  id = 1:4, 
  iter = 1000,
  n = c(100, 100, 200, 200),  # Sample size
  betas = NA, # Coefficients 
  residual_error = c(2, 3, 2, 3)  # Residual error
)

#Adding in the beta list as a complex entry.
betas <- rep(list(c(2, -3, 1.5)), each = nrow(simulation_conditions))
simulation_conditions$betas = betas

#This function runs each condition (i.e. each row in the simulation condition data.frame)
run_wrapper <- function(sim_condition) {
  results_list = list()
  for(i in 1:sim_condition$iter){
    data <- generate_data(sim_condition$n, sim_condition$betas[[1]], sim_condition$residual_error)
    model <- fit_model(data)
    bias <- calculate_relative_bias(model, sim_condition$betas[[1]])
    results_list[[i]] = list(id = sim_condition$id,iter = i, bias = bias)
  }
  toReturn = do.call("rbind", results_list)
  return(toReturn)
}

#This function actually runs the whole simulation study by deploying it to the SLURM cluster
sjob = slurm_map(
          #The use of split here breaks the simulation conditions into a list of rows
          #so it can be used by slurm_map
          split(simulation_conditions, simulation_conditions$id),
          run_wrapper,
          ###From here to the next comment are control parameters, you will likely not change these
          nodes=nrow(simulation_conditions),
          cpus_per_node = 1,
          submit = TRUE,
          preschedule_cores = F,
          #The slurm options is where you specify the time, as well as our lab account
          #The partition should be usually set to standard.
          slurm_options =
            c(account = "netlab", partition = "standard",time = "2:00:00"),
          #This line is vitally important: It imports all functions you have in your environment
          #Because you've sourced your SimFunction file, you should have all necessary functions
          #In the environment.
          global_objects = lsf.str()
          )

#This saves the sjob object, if you don't save it, you can't easily pull out the results
save(sjob, file = "your_simulation_run.Rdata")

#You run these lines after your simulation is complete.
load("your_simulation_run.Rdata")
output = get_slurm_out(sjob, outtype = "table")

