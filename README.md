# Using probabilistic forecasting to assess impacts of the Conservation Reserve Program on grassland birds

Repository provided for review only. Because of the need to respect the sensitivity of lesser prairie-chicken lek locations (U.S. ESA-listed species), we've censored the preprocessing section of our code considerably. The following sections of our code can be examined and run using this repository:

1) Generating CRP layers from NLCD, and summarizing changes in CRP enrollment over time (directory locations: ~/covariates/CRP and ~/changes_in_crp)

2) Running the dynamic occupancy model and associated forecasts (~/modeling/extinction_models/r)

3) Creating summary statistics and plots (~/modeling/extinction_models/statistics and ~/modeling/extinction_models/forecasts)

Note that we've used *renv* to record versions of R and packages used in this project. In addition to R, running this code will require the free software *Just Another Gibbs Sampler*, available here: https://sourceforge.net/projects/mcmc-jags/files/.