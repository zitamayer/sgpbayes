
# load fitted models for this part

# unconditional means model
load("um_goalimportance.rda")

# basic growth models
load("lg.importance.ri.Rdata")
load("lg.importance.rs.Rdata")
load("qg.importance.Rdata")
load("spline.importance.fe.Rdata")

# prior variations
load("lg.importance.pweak.Rdata")
load("lg.importance.pmoderate.Rdata")
load("lg.importance.pstrong.Rdata")

# ar1
load("lg.importance.rs.ar.Rdata")
load("lg.importance.fe.ar.Rdata")

# level-2 predictor
load("lg.importance.whenshelved.interaction.Rdata")


#########################################################################
##                    Intercept-only model                             ##
#########################################################################

# unconditional means model (with random intercept)
# um_goalimportance <- brm(goalimportance ~ 1 + (1|id),
#                                data = dt,
#                                iter = 6000,
#                                warmup = 3000,
#                                chains = 3)
# 
# # save
# save(um_goalimportance, file = "um_goalimportance.rda")
load("um_goalimportance.rda")

# view results
summary(um_goalimportance)

# variance decomposition (ICC)
performance::variance_decomposition(um_goalimportance)

# Between-person variance = 70%, and within-person variance = 100 - 70 = 30%

#############################################################################
##                        LINEAR GROWTH MODEL                              ##
#############################################################################

# random intercept model
# lg.importance.ri <- brm(goalimportance ~ time + (1|id), # model equation
#                         family = gaussian, # distribution type of dependent variable
#                         data = dt, # data object for model fitting
#                         iter = 10000, # number of draws from posterior
#                         warmup = 4000, # how many of the draws are just used for tuning
#                         chains = 4, # how many monte carlo-chains should be used for posterior draws
#                         cores = parallel::detectCores(), # giving each chain its own core for computation
#                         seed = 123) # seed for chain initiation for reprudicible results

# save
# save(lg.importance.ri, file = "lg.importance.ri.Rdata")
load("lg.importance.ri.Rdata")

# view results
summary(lg.importance.ri) # show central model fitting results (parameter estimates + convergence)

# plot model predictions
nd <- distinct(dt, id, time) # create new dataframe for fitted values
fitted(lg.importance.ri, newdata = nd) %>% # fitted/predicted values
  data.frame() %>% # out into data frame
  bind_cols(nd) %>% # connect nd-data frame with fitted value-columns
  ggplot(aes(x = time, y = Estimate, group = id)) + # fit time on x-axis, predicted values on y-axis, for each person individually
  geom_abline(intercept = fixef(lg.importance.ri)[1, 1], # plot regression line based on estimated intercept for each person ..
              slope = fixef(lg.importance.ri)[2, 1], # .. and the estimated slope (fixed / the same for each person)
              # .. the remaining lines are plot aesthetics
              size = 3, color = "black") +
  geom_line(size = 0.3, alpha = 2/3) +
  scale_x_continuous(breaks = 0:37) +
  scale_y_continuous(limits=c(0,100), n.breaks = 10)


# discuss:

# 1) model convergence
plot(lg.importance.ri) # posterior density- & trace plots
# (I) posterior trace plots
# (II) posterior density plots
summary(lg.importance.ri)
# (III) rhats (between-chain vs. within-chain variation)
# (IV) effective sample sizes ("independent draws")
# (V) divergent transitions:
# https://discourse.mc-stan.org/t/divergent-transitions-a-primer/17099
# https://mc-stan.org/docs/reference-manual/divergent-transitions.html
# other convergence problems:
# https://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup

# 2) parameter interpretations
plot(lg.importance.ri) # posterior density- & trace plots: inspect posterior plots
hdi(lg.importance.ri, ci = 0.9, effects = c("all"))


# any questions regarding model convergence and parameter inspection?

# random intercept + slope model
# lg.importance.rs <- brm(goalimportance ~ time + (time | id),
#                     family = gaussian,
#                     data = dt,
#                     iter = 3000,
#                     warmup = 1000,
#                     chains = 3,
#                     cores = parallel::detectCores(),
#                     seed = 123,
#                     save_pars = save_pars(all = TRUE)
# )
# 
# # save
# save(lg.importance.rs, file = "lg.importance.rs.Rdata")
load("lg.importance.rs.Rdata")

# view results
summary(lg.importance.rs)

# check model convergence
plot(lg.importance.rs, ask = FALSE)


# model predictions
nd <- distinct(dt, id, time) # create new dataframe for fitted values
fitted(lg.importance.rs, newdata = nd) %>% 
  data.frame() %>% 
  bind_cols(nd) %>% 
  ggplot(aes(x = time, y = Estimate, group = id)) +
  geom_abline(intercept = fixef(lg.importance.rs)[1, 1],
              slope = fixef(lg.importance.rs)[2, 1],
              size = 3, color = "black") +
  geom_line(size = 0.3, alpha = 2/3) +
  scale_x_continuous(breaks = 0:37) +
  scale_y_continuous(limits=c(0,100), n.breaks = 10)





#############################################################################
##                        QUADRATIC GROWTH MODEL                           ##
#############################################################################

# qg.importance <- brm(goalimportance ~ time + I(time^2) + (time | id),
#                      family = gaussian,
#                      data = dt,
#                      iter = 4000,
#                      warmup = 2000,
#                      chains = 3,
#                      cores = parallel::detectCores(),
#                      seed = 123,
#                      save_pars = save_pars(all = TRUE)
#  )
# 
# # save
# save(qg.importance, file = "qg.importance.Rdata")
load("qg.importance.Rdata")

# view results
summary(qg.importance) # discuss: fixed effect of quadratic term

# check model convergence
plot(qg.importance, ask = FALSE)

# plot conditional effects
plot.qg.importance <- conditional_effects(qg.importance, re_formula = NA, probs = c(0.05, 0.95), robust = F, method = "fitted")
plot.qg.importance <- plot(plot.qg.importance, plot = FALSE)[[1]] + 
  ylab("Goal Importance") +
  xlab("Occasion") +
  scale_x_continuous(n.breaks = 10) +
  theme(text = element_text(size=30)) 
plot.qg.importance

# change y-axis
plot.qg.importance.scaled <- plot(plot.qg.importance) + ggplot2::ylim(0,100)
plot.qg.importance.scaled


# as comparison: linear growth model
# plot conditional effects
plot.lg.importance.rs <- conditional_effects(lg.importance.rs, re_formula = NA, probs = c(0.05, 0.95), robust = F, method = "fitted")
plot.lg.importance.rs <- plot(plot.lg.importance.rs, plot = FALSE)[[1]] + 
  ylab("Goal Importance") +
  xlab("Occasion") +
  scale_x_continuous(n.breaks = 10) +
  theme(text = element_text(size=30)) 
plot.lg.importance.rs

# change y-axis
plot.lg.importance.rs.scaled <- plot(plot.lg.importance.rs) + ggplot2::ylim(0,100)
plot.lg.importance.rs.scaled


#############################################################################
##                             SPLINE MODEL                                ##
#############################################################################

# spline.importance.fe <- brm(goalimportance ~ s(time),
#                          family = gaussian,
#                          data = dt,
#                          iter = 2000,
#                          warmup = 1000,
#                          chains = 2,
#                          control = list(adapt_delta = 0.95),
#                          seed = 123
#                          )
# 
# # save
# save(spline.importance.fe, file = "spline.importance.fe.Rdata")
load("spline.importance.fe.Rdata")

# view results
summary(spline.importance.fe)

# check model convergence
plot(spline.importance.fe, ask = FALSE)

# plot conditional effects
plot.spline.importance.fe <- conditional_effects(spline.importance.fe, re_formula = NA, probs = c(0.05, 0.95), robust = F, method = "fitted")
plot.spline.importance.fe <- plot(plot.spline.importance.fe, plot = FALSE)[[1]] + 
  ylab("Goal Importance") +
  xlab("Time") +
  scale_x_continuous(n.breaks = 10) +
  theme(text = element_text(size=30))
plot.spline.importance.fe

# change y-axis
plot.spline.importance.fe.scaled <- plot(plot.spline.importance.fe) +  ggplot2::ylim(0,100)
plot.spline.importance.fe.scaled

# combine plots in grid
plot_grid(plot.lg.importance.rs.scaled, plot.qg.importance.scaled, plot.spline.importance.fe.scaled)


#############################################################################
##                            MODEL COMPARISON                             ##
#############################################################################

# posteriorpredictive checks
pp.lg.importance.rs <- brms::pp_check(lg.importance.rs, ndraws = 100) +
  xlim(0,100)

pp.qg.importance <- brms::pp_check(qg.importance, ndraws  = 100) +
  xlim(0,100)

pp.spline.fe.importance <- brms::pp_check(spline.importance.fe, ndraws= 100) +
  xlim(0,100)

# combine plots in grid
plot_grid(plot.lg.importance.rs,
          plot.lg.importance.rs.scaled,
          pp.lg.importance.rs,
          plot.qg.importance,
          plot.qg.importance.scaled,
          pp.qg.importance,
          plot.spline.importance.fe,
          plot.spline.importance.fe.scaled,
          pp.spline.fe.importance
          )

# LOO (see also, WAIC)
loo(lg.importance.rs, qg.importance)
# elpd: expected log pointwise predictive density
# higher elpd indicated higher predictive accuracy
# common cutoff: if diff_elpd is 2 (or better 4) times as large as its standard error, one model fits better; if it's lower, the evidence regarding predictive accuracy is indecisive

# bayesfactor via bridge sampling
bayes_factor(lg.importance.rs, qg.importance)
#  bayes factor quantifies for one model over the other
# discuss instability of BFs for these kinds of models

# decision:
# if interested in the initial drop (see e.g., https://psyarxiv.com/udwrg/): quadratic
#  if not, and because the curve looks almost linear:linear


#############################################################################
##         LINEAR GROWTH MODEL WITH AUTOCORRELATED LAG-1 RESIDUALS         ##
#############################################################################

# lg.importance.rs.ar <- brm(goalimportance ~ time + ar(p = 1, time = time, gr = id) + (time | id),
#                            family = gaussian,
#                            data = dt,
#                            iter = 2000,
#                            warmup = 1000,
#                            chains = 3,
#                            seed = 123
# )
# 
# # save
# save(lg.importance.rs.ar, file = "lg.importance.rs.ar.Rdata")
load("lg.importance.rs.ar.Rdata")

# view results
summary(lg.importance.rs.ar)

# look at ar(1) parameter
lg.importance.rs.ar %>% tidybayes::spread_draws(`ar[1]`) %>% tidybayes::mean_hdi()

# compare to model without random time effect
# lg.importance.fe.ar <- brm(goalimportance ~ time + ar(p = 1, time = time, gr = id) + (1 | id),
#                            family = gaussian,
#                            data = dt,
#                            iter = 4000,
#                            warmup = 2000,
#                            chains = 3,
#                            seed = 123
# )
# 
# # save
# save(lg.importance.fe.ar, file = "lg.importance.fe.ar.Rdata")
load("lg.importance.fe.ar.Rdata")


# view results
summary(lg.importance.fe.ar)

# look at ar(1) parameter
lg.importance.fe.ar %>% tidybayes::spread_draws(`ar[1]`) %>% tidybayes::mean_hdi()
# residual correlations larger when not accounting for random time effect



#############################################################################
##                    PRIOR TUNING & ROBUSTNESS CHECK                      ##
#############################################################################

# discuss:
# theoretical vs. empirical informativity

#### view model priors for linear model
get_prior(goalimportance ~ time + (time | id),
          family = gaussian,
          data = dt)

##### model with weakly informative prior #####

# specify prior
pweak = c(set_prior("normal(0,2)", class = "b", coef = "time"))

# fit model
# lg.importance.pweak <- brm(goalimportance ~ time + (time | id),
#                            family = gaussian,
#                            data = dt,
#                            iter = 2000,
#                            warmup = 1000,
#                            chains = 3,
#                            cores = parallel::detectCores(),
#                            prior = pweak,
#                            seed = 123,
#                            sample_prior = T
# )
# 
# # save
# save(lg.importance.pweak, file = "lg.importance.pweak.Rdata")
load("lg.importance.pweak.Rdata")

# view model
lg.importance.pweak$model

# view results
summary(lg.importance.pweak)

# visualize prior und posterior
plot.pweak <- plot(hypothesis(lg.importance.pweak, "time > 0"), plot = FALSE)[[1]]
plot.pweak


##### model with moderately informative prior #####

# specify prior
pmoderate = c(set_prior("normal(0,0.33)", class = "b", coef = "time"))

# fit model
# lg.importance.pmoderate <- brm(goalimportance ~ time + (time | id),
#                                family = gaussian,
#                                data = dt,
#                                iter = 2000,
#                                warmup = 1000,
#                                chains = 3,
#                                cores = parallel::detectCores(),
#                                prior = pmoderate,
#                                seed = 123,
#                                sample_prior = T
# )
# 
# # save
# save(lg.importance.pmoderate, file = "lg.importance.pmoderate.Rdata")
load("lg.importance.pmoderate.Rdata")


# view results
summary(lg.importance.pmoderate)

# visualize prior und posterior
plot.pmoderate <- plot(hypothesis(lg.importance.pmoderate, "time > 0"), plot = FALSE)[[1]]
plot.pmoderate


##### model with strongly informative prior #####

# specify prior
pstrong = c(set_prior("normal(0,0.11)", class = "b", coef = "time"))

# fit model
# lg.importance.pstrong <- brm(goalimportance ~ time + (time | id),
#                              family = gaussian,
#                              data = dt,
#                              iter = 2000,
#                              warmup = 1000,
#                              chains = 3,
#                              cores = parallel::detectCores(),
#                              prior = pstrong,
#                              seed = 123,
#                              sample_prior = T
# )
# 
# # save
# save(lg.importance.pstrong, file = "lg.importance.pstrong.Rdata")
load("lg.importance.pstrong.Rdata")


# view results
summary(lg.importance.pstrong)

# visualize prior und posterior
plot.pstrong <- plot(hypothesis(lg.importance.pstrong, "time > 0"), plot = FALSE)[[1]]

# combine plots in grid
plot_grid(plot.pweak, plot.pmoderate, plot.pstrong)


##########################################################################################
##    LEVEL-2 PREDICTOR: BETWEEN-PERSON DIFFERENCES IN INTERCEPTS + SLOPES              ##
##########################################################################################


# lg.importance.whenshelved.interaction <- brm(goalimportance ~ time*whenshelved + ar(p = 1, time = time, gr = id) + (time | id),
#                                              family = gaussian,
#                                              data = dt,
#                                              iter = 4000,
#                                              warmup = 2000,
#                                              chains = 3,
#                                              seed = 123
# )
# 
# # save
# save(lg.importance.whenshelved.interaction, file = "lg.importance.whenshelved.interaction.Rdata")
load("lg.importance.whenshelved.interaction.Rdata")

# view results
summary(lg.importance.whenshelved.interaction)

# look at ar(1) parameter
lg.importance.whenshelved.interaction %>% tidybayes::spread_draws(`ar[1]`) %>% tidybayes::mean_hdi()

# plot conditional effects
plot.lg.importance.whenshelved.interaction <- conditional_effects(lg.importance.whenshelved.interaction, re_formula = NA, probs = c(0.05, 0.95), robust = F, method = "fitted")
plot.lg.importance.whenshelved.interaction  <- plot(plot.lg.importance.whenshelved.interaction , plot = FALSE)[[3]] + 
  ylab("Goal Importance") +
  xlab("Time") +
  scale_x_continuous(n.breaks = 10) +
  theme(text = element_text(size=30))
plot.lg.importance.whenshelved.interaction

# change y-axis
plot.lg.importance.whenshelved.interaction.scaled <- plot(plot.lg.importance.whenshelved.interaction) + ggplot2::ylim(0,100)
plot.lg.importance.whenshelved.interaction.scaled








