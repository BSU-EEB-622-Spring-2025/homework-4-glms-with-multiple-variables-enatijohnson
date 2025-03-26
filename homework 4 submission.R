## Homework 4 Submission ## 
library(MASS)
library(performance)
library(marginaleffects)
library(modelr)
library(ggplot2)

mistletoe <- read.csv("mistletoes.csv")
head(mistletoe)


## Question 1:

## 1a) (5 pts)** Fit a glm assessing evidence for the following hypothesis: Seedling density is increased beneath trees experiencing mistletoe infection. Describe the rationale you used in selecting a glm structure and probability distribution. Calculate model fit using MAE.


#seedlings is positively bounded, and discrete, so a poisson or negative binomial distribution is appropriate
#year is irrelevant to this question - no repeat treeIDs between years
#response variable is seedling density and predictor is treatment

#general glm structure: glm(Seedlings ~ Treatment)
pois_mod <- glm(Seedlings~Treatment, data = mistletoe, family = poisson(link = "log"))
summary(pois_mod)

exp(5.73) #predicted number of seedlings when trees are parasitized
exp(5.73-3.16) #predicted number of seedlings when trees are unparasitized

check_overdispersion(pois_mod) #overdispersion is detected

#run negative binomial instead
nb_mod <- glm.nb(Seedlings~Treatment, data = mistletoe)
summary(nb_mod)
#estimates are essentially the same as with the poisson model, but AIC is much lower
mae(pois_mod, data = mistletoe)
mae(nb_mod, data = mistletoe)
#MAE is 158.3636 for both models

## ASW: nice! MAE may not shift between nb and poisson, but if we are violating the assumptions of the poisson, we want to switch regardless because our p-values can be overly confident in the poisson fit.

## 1b) (15 pts)** Use visual (e.g. a marginal effects plot) and written (e.g. effect sizes on scale of response) approaches to interpret the results of your model.

#Does mistletoe infection alter seedling density? How much does seedling recruitment differ beneath parasitized and unparasitized trees? Explain which elements of your glm results informed your conclusions and annotate the steps you needed to take to interpret your parameters. 

plot_predictions(nb_mod, condition = "Treatment") + theme_bw()
#There is more than a 23-fold increase in seedling abundance beneath parasitized trees. The estimates and p-values informed my conclusions. I needed to use the exp() function on the estimates to convert them back from logistic scale.

## ASW: great!


## 1c) (10 pts)** During the course of this study, 2012 was an atypically rainy year, compared to 2011. Fit an additional glm that quantifies how the effect of mistletoe differs between the two years in this study. Write ~2 new sentences that summarize the results of the new model and their biological implications.

nb_mod <- glm.nb(Seedlings~Treatment * Year, data = mistletoe)
summary(nb_mod)
plot_predictions(nb_mod, by =c("Treatment", "Year")) + theme_bw()
#The addition of Year resulted in all of the estimates being not statistically significant, although with p-values only slightly higher than 0.05. Seedling density was higher in 2012, with most of the difference coming from a higher density of seedlings under parasitized trees.

## ASW: great work! 30/30

## Question 2:
treemortality <- read.csv("treemortality.csv")
head(treemortality)

## 2a) (5 pts)** Fit a glm (using a probability distribution of your choice) that reflects the following research question (including thinning as your only predictor and mortality as your response): Do forest thinning treatments reduce the probability of tree mortality? Interpret the results of the glm by writing 2-4 sentences about the biological significance of the effect of thinning, including descriptions of the sizes of the effect on the scale of the response variable, evidence for/against the hypothesis, and a metric of model fit.
library(pROC)
mod <- glm(mortality ~ thinning, data = treemortality, family = binomial(link = "logit"))
summary(mod)
plogis(0.9933)
#tree mortality without thinning is 0.73
plogis(0.9933 - 1.8559)
#tree mortality with thinning is 0.30

test_prob <- predict(mod, type = "response")
test_roc <- roc(treemortality$mortality ~ test_prob, plot = TRUE, print.auc = TRUE)
test_roc

#Forest thinning does appear to reduce the probability of tree mortality based on the model. There is a 58% reduction in tree mortality in the thinned area, and the effect of thinning is highly significant. The AUC of 0.7096 indicates that almost half of the variance in the data is explained by the model. 

##ASW: The AUC/ROC isn't telling us about the proportion explained (like an R2), but rather the model's ability to classify "0s" and "1s" correctly.  AUC of 1.0 indicates perfect discrimination, while 0.5 indicates that the model isn't classifying values any better than a random guess would.
 


## 2b)(2 pts)** The researchers explicitly considered the potential for confounding relationships related to tree size in their design and randomized their post-fire sampling by tree size. Given this information, do the researchers need to incorporate tree size into their glm to accurately estimate the effect of thinning? Why or why not?

#No, they do not need to incorporate tree size into the GLM. Because tree size was randomized between treatments, the effect of thinning is not biased by tree size. 

## ASW: lovely!


## 2c) Refit the model from 2a to include the necessary variables to minimize bias in our estimation of the “thinning” variable, based on the reviewer’s proposed DAG (above). Does the effect of “thinning” change? If so, describe the degree of change and why the two models may differ in their conclusions. If needed, modify your model interpretation from 2a.
mod2 <- glm(mortality ~ thinning + slope, data = treemortality, family = binomial(link = "logit"))
summary(mod2)
plogis(-15.37)
plogis(-15.37-0.948)
plogis(-15.37 + 0.627)
#"Closing the loop" by including one of the other variables reveals that thinning had basically no effect. The effect of slope and distance to nearest road was being hidden within the thinning variable in the previous model.


## ASW: you're close here! two quick things. Firstly, this model should also include road distance as a covariate, according to the DAG, since that 'backdoor' path is not closed.

## Secondly, Once the model looks like: glm(mortality~ slope + distroad + thinning…), your predictions need to include a 'baseline' for those other variables. For instance, a common practice is to hold all the other values at their means:

# plogis(int + Bthinning*X +	Broad*(mean(treemortality$roaddist)) +  Bslope*(mean(treemortality$slope)))

## (which is what the predictions() function does automatically.)

## Currently, your predictions are holding slope at 0 and distance from road at 0, making the probabilities very, very small. But you are on the right track! The effect of thinning should be reduced, but not disappear entirely.

plot_predictions(mod2, condition="thinning")

## 17/20

## ASW: nice work! 47/50
