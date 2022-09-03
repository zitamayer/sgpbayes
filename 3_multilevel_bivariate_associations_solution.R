
# load fitted models for this part

# unconditional means model
load("um_goalmissing.rda")

# random intercept and slope model
load("rs.goalimportance.goalmissing.Rdata")

# random intercept and slope model with additional level-2 predictor
load("rs.goalimportance.goalmissing.whenshelved.Rdata")

# random intercept and slope model with heterogeneous variances
load("fe.ls.goalimportance.goalmissing.Rdata")

#########################################################################
##                         PREPARE DATA                                ##
#########################################################################

# Disaggregate and person-mean center the predictor variable

## split predictor variable into "trait (between-person differences) and "state" (within-person deviation) components
# sample-mean centered between-person component of goal missing variable
# person-mean centered within-person component  of goal missing variable

# compute the person means (i.e., the level-2 part of the level-1 predictor)
dt.pm <- ddply(dt, "id", summarize, goalmissing.pm = mean(goalmissing, na.rm = TRUE))
describe(dt.pm)

# sample-mean center (i.e., grand-mean center) the person-specific means (i.e., the level-2 predictor) for better interpretability (i.e., between-person centered)
dt.pm$goalmissing.bpc <- scale(dt.pm$goalmissing.pm, center = TRUE, scale = FALSE)
describe(dt.pm)

# merge the person-level (i.e., level-2) variables into the long data file
dt <- merge(dt, dt.pm, by = "id")

# within-person center the level-1 predictor
dt$goalmissing.wpc <- dt$goalmissing - dt$goalmissing.pm

# plot the "trait" and "state" scores to see if it looks rights

# create subset of 5 participants
dts <- dt %>%
  filter(id == '3WXNCWYM2F'|
           id == '37TSCPD2F5' |
           id == '3UNGK822V7' |
           id == '3QNBBWBZBV' |
           id == '3XZX97SB3P')

# trait + state
ggplot(dts, ) +
  geom_line(aes(x = time, y = goalmissing, group = id, color=factor(id))) +
  geom_line(aes(x = time, y = goalmissing.pm, group = id, color=factor(id))) +
  xlab("Day") + 
  ylab("Trait- and State Goal Missing") + ylim(0,100) +
  scale_x_continuous(breaks=seq(0,40,by=1))

#########################################################################
##                    Intercept-only model                             ##
#########################################################################

# unconditional means model (with random intercept)
# um_goalmissing <- brm(goalmissing ~ 1 + (1|id),
#                             data = dt,
#                             iter = 2000,
#                             warmup = 1000,
#                             chains = 4,
#                             cores = 4)
# # save
# save(um_goalmissing, file = "um_goalmissing.rda")
load("um_goalmissing.rda")

# view results
summary(um_goalmissing)

# variance decomposition (ICC)
performance::variance_decomposition(um_goalmissing)

# Between-person variance = 63%, and within-person variance = 100 - 63 = 37%



#########################################################################
##               RANDOM INTERCEPT AND SLOPE MODEL                      ##
#########################################################################

# rs.goalimportance.goalmissing <- brm(goalimportance ~ goalmissing.wpc + goalmissing.bpc + time + ar(p = 1, time = time, gr = id) + (goalmissing.wpc + time | id),
#                                      family = gaussian,
#                                      data = dt,
#                                      iter = 4000,
#                                      warmup = 2000,
#                                      chains = 3,
#                                      cores = parallel::detectCores(),
#                                      seed = 123
# )
# 
# # save
# save(rs.goalimportance.goalmissing, file = "rs.goalimportance.goalmissing.Rdata")
load("rs.goalimportance.goalmissing.Rdata")

# view results
summary(rs.goalimportance.goalmissing)

# view fixed effects
fixef(rs.goalimportance.goalmissing)

# look at ar(1) parameter
rs.goalimportance.goalmissing %>% tidybayes::spread_draws(`ar[1]`) %>% tidybayes::mean_hdi()

# check model convergence
plot(rs.goalimportance.goalmissing, ask = FALSE)

# check adequacy: postersor predictive check
brms::pp_check(rs.goalimportance.goalmissing, ndraws = 100)

# plot conditional effects
plot.rs.goalimportance.goalmissing <- conditional_effects(rs.goalimportance.goalmissing, re_formula = NA, probs = c(0.05, 0.95), robust = F, method = "fitted")

plot.rs.goalimportance.goalmissing.wpc  <- plot(plot.rs.goalimportance.goalmissing, plot = FALSE)[[1]] + 
  ylab("Goal Importance") +
  xlab("Goalmissing.WPC") +
  scale_x_continuous(n.breaks = 10) +
  theme(text = element_text(size=30))
plot.rs.goalimportance.goalmissing.wpc


######################################################################################################
##              LEVEL-2 PREDICTOR: BETWEEN-PERSON DIFFERENCES INTERCEPTS AND SLOPES                 ##
######################################################################################################

# rs.goalimportance.goalmissing.whenshelved <- brm(goalimportance ~ goalmissing.wpc + goalmissing.bpc + time + ar(p = 1, time = time, gr = id) + goalmissing.wpc:whenshelved + (goalmissing.wpc + time | id),
#                                                  family = gaussian,
#                                                  data = dt,
#                                                  iter = 4000,
#                                                  warmup = 2000,
#                                                  chains = 3,
#                                                  cores = parallel::detectCores(),
#                                                  seed = 123
# )
# 
# # save
# save(rs.goalimportance.goalmissing.whenshelved, file = "rs.goalimportance.goalmissing.whenshelved.Rdata")
load("rs.goalimportance.goalmissing.whenshelved.Rdata")

# view results
summary(rs.goalimportance.goalmissing.whenshelved)

# view fixed effects
fixef(rs.goalimportance.goalmissing.whenshelved)


########################################################################################################
##              LEVEL-2 PREDICTOR: BETWEEN-PERSON DIFFERENCES IN SLOPES AND VARIANCES                 ##
########################################################################################################

# ls: Location-scale modeling (location: expectation/first moment; scale: variation/second moment)

# fe.ls.goalimportance.goalmissing <- brm(bf(goalimportance ~ 1 + goalmissing.bpc,
#                                         sigma ~ (1|id) + goalmissing.bpc),
#                                      data = dt,
#                                      iter = 2000,
#                                      warmup = 1000,
#                                      chains = 2,
#                                      cores = parallel::detectCores(),
#                                      seed = 123
# )
# save(fe.ls.goalimportance.goalmissing, file = "fe.ls.goalimportance.goalmissing.Rdata")
load("fe.ls.goalimportance.goalmissing.Rdata")

summary(fe.ls.goalimportance.goalmissing)


########################################################################################################
##                                     CROSS-LAGGED ASSOCIATIONS                                      ##
########################################################################################################

#Code for cross-lagged model:
#https://discourse.mc-stan.org/t/cross-lagged-multilevel-panel-model/3992/10 # scroll down a bit for the relevant code.
#creating lagged variables:
#https://stackoverflow.com/questions/26291988/how-to-create-a-lag-variable-within-each-group

#Create lagged variables
dt_lags <- dt %>%
  group_by(id) %>%
  mutate(
    goalimportance_lag = lag(goalimportance, order_by = time),
    goalmissing_lag = lag(goalmissing, order_by = time)
  )

# Inspect created lagged variables (rightmost columns)
View(dt_lags)

#Define first part of model: Goal importance as dependent variable,
#regressted on itself at last time point (autoregression) and goal missing at the last time point (cross-lag)
f1 <- bf(goalimportance ~ goalimportance_lag + goalmissing_lag + (goalimportance_lag | id))

#Define second part of model: Goal missng as dependent variable,
#regressted on itself at last time point (autoregression) and goal importance at the last time point (cross-lag)
f2 <- bf(goalmissing ~ goalmissing_lag + goalimportance_lag + (goalmissing_lag | id))

# crosslag.goalmissing.goalimportance.smaller <- brm(f1 + f2 + set_rescor(TRUE), #Combines two defined models into one multivarte model, with correlated residuals of DVs
#                                         data = dt_lags,
#                                         iter = 2000,
#                                         warmup = 1000,
#                                         chains = 2,
#                                         cores = parallel::detectCores(),
#                                         seed = 123,
# control = list(adapt_delta = 0.95))
# save(crosslag.goalmissing.goalimportance.smaller, file = "crosslag.goalmissing.goalimportance.smaller.Rdata")
load("crosslag.goalmissing.goalimportance.smaller")

summary(crosslag.goalmissing.goalimportance.smaller)
