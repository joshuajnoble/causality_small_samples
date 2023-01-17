library(BayesFactor, quietly = TRUE)
library(brms)
library(olsrr)
library(stargazer)

#load in our experimental data
experiment <- read.csv("data/article_simple_experiment.csv")

# a little bit of feature engineering
experiment$grade_diff <- experiment$post_grade - experiment$prev_grade

experiment$classfactor <- factor(experiment$classroom)
experiment$treatedfactor <- factor(experiment$treated)

#what's our histogram look like?
hist(experiment$grade_diff)

#Freq:  frequentist t-testing
t.test(experiment[experiment$treated == 1,]$grade_diff, experiment[experiment$treated == 0,]$grade_diff)

# Bayes: bayesian t-testing
ttestBF(experiment[experiment$treated == 1,]$grade_diff, experiment[experiment$treated == 0,]$grade_diff)

# Bayes: let's check our variances for a more complex model
anovaBF(grade_diff ~ treatedfactor + classfactor, data = experiment)


#Freq: frequentist analysis of possible model
f_aov <- aov(grade_diff ~ treatedfactor, data = experiment)
summary(f_aov)

f_aov <- aov(grade_diff ~ treatedfactor + classfactor, data = experiment)
summary(f_aov)

#Freq:  a better model
f_aov <- aov(grade_diff ~ classfactor:treatedfactor, data = experiment)
summary(f_aov)

#Freq:  now our linear regression
flm <- lm(grade_diff ~ treatedfactor*classfactor, data = experiment)
stargazer::stargazer(flm, type = "text", single.row=TRUE, ci=TRUE, ci.level=0.95)


#Bayes: let's mcmc up a model
bayes_lr <- brm(grade_diff ~ treatedfactor * classfactor, data = experiment, refresh = 0, warmup = 500, iter = 1000)
summary(bayes_lr)
pp_check(bayes_lr)

#Bayes: check our interactions
conditional_effects(bayes_lr)

#Bayes: we got the priors from our helpful colleague, womp womp
brms_prior <- c(
  prior(normal(0, 1), coef = treatedfactor0:classfactor1),
  prior(normal(0, 1), coef = treatedfactor0:classfactor2),
  prior(normal(0, 1), coef = treatedfactor1:classfactor1),
  prior(normal(0, 1), coef = treatedfactor1:classfactor2)
)

bayes_lr_small_prior <- brm(grade_diff ~ treatedfactor:classfactor, 
                                 prior=brms_prior, data = experiment, refresh = 0, warmup = 500, iter = 1000)

summary(bayes_lr_small_prior)

#Bayes: check our interactions
conditional_effects(bayes_lr_small_prior)

# now we got previous data
prev_data <- read.csv("data/experiment_update.csv")

#make it match our experiment
prev_data$grade_diff <- prev_data$post_grade - prev_data$prev_grade
prev_data$classfactor <- factor(prev_data$classroom)
prev_data$treatedfactor <- factor(prev_data$treated)

# merge the two (if we want to use it) and toss class 1 and 2
merged <- merge(experiment[experiment$classroom > 2,], prev_data, all = TRUE)

#Bayes: build a prior from this previous experiment
bayes_lr_previous <- brm(grade_diff ~ treatedfactor, 
                            data = prev_data, refresh = 0, warmup = 500, iter = 1000)

#Bayes: get a summary to use as a prior
summary(bayes_lr_previous)
pp_check(bayes_lr_previous)

#Bayes: use it to re-evaluate our current experiment
brms_prior <- c(
  prior(normal(7.0, 0.5), coef = treatedfactor1)
)

#Bayes:  using our prior
bayes_lr_prior <- brm(grade_diff ~ treatedfactor, prior = brms_prior,
                         data = experiment, refresh = 0, warmup = 500, iter = 1000)

#Bayes: check our work
summary(bayes_lr_prior)
pp_check(bayes_lr_prior)

#Bayes: look at the conditional effects
conditional_effects(bayes_lr_prior)
pp_check(bayes_lr_prior, type = "dens_overlay", ndraws=20)

#Freq:  the frequentist version with our merged data
flm_merged <- lm(grade_diff ~ treatedfactor*classfactor, data = merged)
stargazer::stargazer(flm_merged, type = "text", single.row=TRUE, ci=TRUE, ci.level=0.95)

#Freq:  check out the residuals plot, no bueno
plot(flm_merged)

#Freq: simplify things
flm_merged_simple <- lm(grade_diff ~ treatedfactor, data = merged2)
stargazer::stargazer(flm_merged_simple, type = "text", single.row=TRUE, ci=TRUE, ci.level=0.95)
plot(flm_merged_simple)
