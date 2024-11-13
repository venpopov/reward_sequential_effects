library(tidyverse)
library(lme4)
source('scripts/prior_item.R')

# exp1
dat <- read.csv('data/Middlebrooks2017_exp1.csv')
names(dat) <- tolower(names(dat))
dat <- select(dat,exp_username, exp_trial, exp_condition.description, exp_serial_position, exp_serial_cue, exp_serial_answer, exp_serial_strictacc)
names(dat) <- c('subject', 'trial','condition','sp','value','stim','acc')
dat <- filter(dat, !is.na(sp))
dat1 <- dat
dat1$exp <- 1

# exp 2
dat <- read.csv('data/Middlebrooks2017.csv')
names(dat) <- tolower(names(dat))
dat <- select(dat, exp_username, exp_trial, exp_condition.description, exp_serial_position, exp_serial_answer, exp_serial_value, exp_serial_strictacc)
names(dat) <- c('subject','trial','condition','sp','stim','value','acc')
dat <- filter(dat, !is.na(sp))
dat2 <- dat
dat2$exp <- 2
dat2$subject <- as.factor(dat2$subject)

dat <- bind_rows(dat1, dat2)

#get prioritem value info
dat <- dat %>% 
  group_by(subject, trial, exp) %>% 
  prior_item_analysis('value','value')

#get postitem  value info
dat <- dat %>% 
  group_by(subject, trial, exp) %>% 
  arrange(desc(sp)) %>% 
  prior_item_analysis('value','postvalue')

dat$value <- as.numeric(dat$value)
dat$value_prioritem <- as.numeric(dat$value_prioritem)
dat$postvalue_prioritem <- as.numeric(dat$postvalue_prioritem)


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

# effect of postvalue value
dat %>% 
  filter(!is.na(postvalue_prioritem)) %>% 
  ggplot(aes(postvalue_prioritem, acc)) +
  stat_summary(fun.data = mean_se, geom="pointrange") +
  stat_summary(fun.data = mean_se, geom="line")

dat %>% 
  ggplot(aes(ceiling(value_prioritem/2), acc)) +
  stat_summary(fun.data = mean_se, geom="pointrange") +
  stat_summary(fun.data = mean_se, geom="line") +
  theme_classic() +
  scale_x_continuous("Value of preceding study item", labels=c('1-2','3-4','5-6','7-8','9-10')) +
  ylab('Accuracy')


# -------------------------------------------------------------------
# ANALYSIS
# -------------------------------------------------------------------

ml1 <- glmer(acc ~ value + value_prioritem + (value|subject), data=dat, family='binomial',control = glmerControl(optimizer='bobyqa'),nAGQ=0)


# subject's slopes for value
subj <- data.frame(subject=rownames(ranef(ml1)$subject), slopes = summary(ml1)$coefficients[2,1]+ranef(ml1)$subject[,2])
subj <- arrange(subj, slopes)


# effect of prioritem value excluding bad subjects
dat <- dat %>%  
  filter(!is.na(value_prioritem)) %>% 
  left_join(subj, by='subject') %>%
  mutate(group = ifelse(slopes < summary(ml1)$coefficients[2,1], 'low','high')) 

dat %>% 
  ggplot(aes(value_prioritem, acc, color=group)) +
  stat_summary(fun.data = mean_se, geom="pointrange") +
  stat_summary(fun.data = mean_se, geom="line") +
  theme_classic() +
  scale_color_discrete('Participant group', labels=c('Large value-directed effect','Small value-directed effect'))


# prior item
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


# subsequent item
dat %>% 
  ggplot(aes(ceiling(as.numeric(postvalue_prioritem)/2), acc, color=group)) +
  stat_summary(fun.data = mean_se, geom="pointrange") +
  stat_summary(fun.data = mean_se, geom="line") +
  theme_classic() +
  scale_x_continuous("Value of preceding study item", labels=c('1-2','3-4','5-6','7-8','9-10')) +
  scale_color_discrete('Participant group', labels=c('Large value-directed effect','Small value-directed effect')) +
  ylab('Accuracy')


# scompare effects of prior and subsequent
dat %>% 
  mutate(postvalue_prioritem = as.numeric(postvalue_prioritem)) %>% 
  gather(position, value, value_prioritem, postvalue_prioritem) %>% 
  mutate(position = ifelse(position == 'value_prioritem', 'Accuracy for following neighbor','Accuracy for preceding neighbor')) %>% 
  ggplot(aes(ceiling(as.numeric(value)/2), acc, color=group)) +
  stat_summary(fun.data = mean_se, geom="point", size=3) + 
  stat_summary(fun.data = mean_se, geom="pointrange") +
  stat_summary(fun.data = mean_se, geom="line") +
  theme_classic() +
  scale_x_continuous("Value of study item", labels=c('1-2','3-4','5-6','7-8','9-10')) +
  scale_color_discrete('Participant group', labels=c('Large value-directed effect','Small value-directed effect')) +
  ylab('Accuracy') +
  facet_wrap(~position)


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
