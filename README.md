# Using probabilistic forecasting to assess impacts of the Conservation Reserve Program on grassland birds

Code and data are provided for review only. Lesser prairie-chickens (*Tympanuchus pallidicinctus*) are a sensitive species which have a complicated relationship with the U.S. Endangered Species Act. In order to protect the locations of their leks, we have omitted some of the code used to preprocess lek locations and extract covariate values from this repository. 

The code to run each of our analyses is included in the following folders:

1) Generating CRP layers from NLCD, and summarizing changes in CRP enrollment over time (directory locations: ~/covariates/CRP and ~/changes_in_crp)

2) Running the dynamic occupancy model and associated forecasts (~/modeling/extinction_models/r)

3) Creating summary statistics and plots (~/modeling/extinction_models/statistics and ~/modeling/extinction_models/forecasts)

Note that we've used *renv* to record versions of R and packages used in this project. In addition to R, running this code will require the free software *Just Another Gibbs Sampler*, available here: https://sourceforge.net/projects/mcmc-jags/files/.