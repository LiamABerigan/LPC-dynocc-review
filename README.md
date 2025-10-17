# Using probabilistic forecasting to assess impacts of the Conservation Reserve Program on grassland birds

Code and data are provided for review only. Lesser prairie-chickens (*Tympanuchus pallidicinctus*) are an at-risk species, and the locations of their leks are considered sensitive information. Accordingly, we have we have omitted some of the code used to preprocess survey data in order to ensure those lek locations remain concealed. All other code in the project should run normally despite this omission.

The code to run our analyses is included in the following folders:

1) Generating CRP layers from NLCD, and summarizing changes in CRP enrollment over time (directory locations: ~/covariates/CRP and ~/changes_in_crp)

2) Running the dynamic occupancy model and associated forecasts (~/modeling/extinction_models/r)

3) Creating summary statistics and plots (~/modeling/extinction_models/statistics and ~/modeling/extinction_models/forecasts)

Note that we've used *renv* to record versions of R and packages used in this project. In addition to R, running this code will require the free software *Just Another Gibbs Sampler (JAGS)*, available here: https://sourceforge.net/projects/mcmc-jags/files/.