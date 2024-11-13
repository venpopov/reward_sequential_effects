library(tidyverse)
library(lme4)
source('scripts/prior_item.R')

dat <- read.csv('data/Middlebrooks2017_exp2.csv')
names(dat) <- tolower(names(dat))
dat <- select(dat, exp_username, exp_trial, exp_condition.description, exp_serial_position, exp_serial_answer, exp_serial_value, exp_serial_output_order, exp_serial_strictacc)
names(dat) <- c('subject','trial','condition','sp','stim','value','op','acc')
dat <- filter(dat, !is.na(sp))

#get prioritem value info
dat <- dat %>% 
  group_by(subject, trial) %>% 
  prior_item_analysis('value','value')

dat$value <- as.numeric(dat$value)
dat$value_prioritem <- as.numeric(dat$value_prioritem)


# -------------------------------------------------------------------
# FIGUREs
# -------------------------------------------------------------------

# effect of value
dat %>% 
  ggplot(aes(value, acc)) +
  stat_summary(fun.data = mean_se, geom="pointrange") +
  stat_summary(fun.data = mean_se, geom="line")


# effect of prioritem value
dat %>% 
  filter(!is.na(value_prioritem)) %>% 
  ggplot(aes(value_prioritem, acc)) +
  stat_summary(fun.data = mean_se, geom="pointrange") +
  stat_summary(fun.data = mean_se, geom="line")


# -------------------------------------------------------------------
# ANALYSIS
# -------------------------------------------------------------------

ml1 <- glmer(acc ~ value + value_prioritem + (value|subject), data=dat, family='binomial')


# subject's slopes for value
subj <- data.frame(subject=rownames(ranef(ml1)$subject), slopes = 0.190676+ranef(ml1)$subject[,2])
subj <- arrange(subj, slopes) %>% mutate(subject = as.integer(as.character(subject)))


# effect of prioritem value excluding bad subjects
dat <- dat %>% 
  filter(!is.na(value_prioritem)) %>% 
  left_join(subj, by='subject') %>%
  mutate(group = ifelse(slopes < 0.190676, 'low','high')) 

dat %>% 
  ggplot(aes(value_prioritem, acc, color=group)) +
  stat_summary(fun.data = mean_se, geom="pointrange") +
  stat_summary(fun.data = mean_se, geom="line") +
  theme_classic() +
  scale_color_discrete('Participant group', labels=c('Large value-directed effect','Small value-directed effect'))
  

dat %>% 
  ggplot(aes(ceiling(value_prioritem/2), acc, color=group)) +
  stat_summary(fun.data = mean_se, geom="pointrange") +
  stat_summary(fun.data = mean_se, geom="line") +
  theme_classic() +
  scale_x_continuous("Value of preceding study item", labels=c('1-2','3-4','5-6','7-8','9-10')) +
  scale_color_discrete('Participant group', labels=c('Large value-directed effect','Small value-directed effect')) +
  ylab('Accuracy')

dat %>% 
  ggplot(aes(ceiling(value_prioritem/2), acc, color=condition)) +
  stat_summary(fun.data = mean_se, geom="pointrange") +
  stat_summary(fun.data = mean_se, geom="line") +
  theme_classic() +
  scale_x_continuous("Value of preceding study item", labels=c('1-2','3-4','5-6','7-8','9-10')) +
  ylab('Accuracy') +
  facet_wrap(~group)



ml2 <- glmer(acc ~ value + value_prioritem + value_prioritem:group + (1|subject), data=dat, family='binomial')

# check correlation between slopes
ml3 <- glmer(acc ~ value + value_prioritem + (value + value_prioritem||subject), data=dat, family='binomial', control=glmerControl(optimizer='bobyqa'))


cordat <- ranef(ml3)$subject[,c(2,3)] %>% as.data.frame()
cor.test(cordat$value, cordat$value_prioritem)

ranef(ml3)$subject %>% 
  ggplot(aes(value+summary(ml3)$coefficients[2,1], value_prioritem+summary(ml3)$coefficients[3,1])) +
  geom_point() +
  geom_smooth(method='lm') +
  theme_classic() +
  xlab('Slope of the value-directed remembering effect') +
  ylab('Slope of the preceding-item value effect')
