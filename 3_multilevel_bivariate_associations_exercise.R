
# load fitted models for this part

# unconditional means model
load("um_goalmissing.rda")




#########################################################################
##                    Intercept-only model                             ##
#########################################################################

# unconditional means model

um_goalmissing <- brm(goalmissing ~ 1 + (1|id),
                            data = dt,
                            iter = 6000,
                            warmup = 3000,
                            chains = 4,
                            cores = 4)
save(um_goalmissing, file = "um_goalmissing.rda")


summary(um_goalmissing)
plot(um_goalmissing)
performance::variance_decomposition(um_goalmissing)

# Between-person variance = 63%, and within-person variance = 100 - 63 = 37%


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
##                              EXERCISE                               ##
#########################################################################











