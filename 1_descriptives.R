
##########################################################
##                  DATA CHARACTERISTICS                ##
##########################################################


# how many unique persons are there in these data?

# make a vector of the unique ids
idlist <- unique(dt$id)

#length of the id vector
length(idlist)


# how many occasions are there in these data?

# make a vector of the unique entries in the "occasion" variable" 
occlist <- unique(dt$time)

# length of the id vector
length(occlist)

# check on how many people completed each measure on each occasion
describeBy(dt[,9:10],group=dt$time)


# who provided how much "goalimportance" data ?
  
# calculate the number of rows for each id, after removing rows where goalimportance is missing 
numobs.goalimportance <- table(dt[which(dt$goalimportance != "NA"),]$id)
numobs.goalimportance

# make a frequency table of the observation counts
tbl <- table(numobs.goalimportance)
tbl

# use that table to calculate number of persons and proportion of sample that provided each amount of observations
tbl.pct <- cbind(numobs=as.numeric(rownames(tbl)), freq=tbl, cumul=cumsum(tbl), 
                 relative.pct=prop.table(tbl), cumulative.pct=cumsum(tbl)/length(numobs.goalimportance),
                 invcumulative.pct=1-(cumsum(tbl)/length(numobs.goalimportance)))
round(tbl.pct,3)

##########################################################
##                       DESCRIBE DATA                  ##
##########################################################


# there are multiple ways to describe intensive repeated measures data - by aggregating in different ways

# daily goal importance at the sample level = all persons and occasions
describe(dt$goalimportance)

# histogram
ggplot(data=dt, aes(x=goalimportance)) +
  geom_histogram(binwidth=1, boundary=.5, fill="white", color="black") + 
  labs(x = "Daily Goal Importance")

# daily goal importance at the daily level = all persons faceted by occasion

# sample descriptives by occasion
describeBy(dt[,c("goalimportance")],group=dt$time)

# boxplot by occasion
qplot(x=factor(time), y=goalimportance, data=dt, geom="boxplot", ylab="Daily Goal Importance")



# daily goal importance at the individual level = each individual described as a separate entity

# create subset of 5 participants
dts <- dt %>%
  filter(id == '3WXNCWYM2F'|
           id == '37TSCPD2F5' |
           id == '3UNGK822V7' |
           id == '3QNBBWBZBV' |
           id == '3XZX97SB3P')


# sample descriptives by individual 
describeBy(dts[,c("goalimportance")],group=dts$id, na.rm=TRUE)

# histogram faceted by individual
ggplot(dts, aes(x=goalimportance)) +
  geom_histogram(fill="white", color="black") + 
  labs(x = "Daily Goal Importance") +
  facet_grid(id ~ .)


# individual-level trajectories

# few individuals
ggplot(dts, aes(x = time, y = goalimportance, group = id, color=factor(id))) +
  geom_point() + 
  geom_line() +
  xlab("Occasion") + 
  ylab("Daily Goal Importance")

# sample (facetted by person)
ggplot(dt, aes(x = time, y = goalimportance, color = as.factor(id), group = id)) + 
  geom_point() + 
  geom_line() + 
  theme(legend.position = "none") + 
  labs(y = "goalimportance", x = "occasion") +
  facet_wrap(~ id)

ggsave("caterpillar_id.png", width = 20, height = 15, units = "in", dpi = 600)

# We see that there is inter-individual variation in the overall level, amount of variation, and change in level across time

