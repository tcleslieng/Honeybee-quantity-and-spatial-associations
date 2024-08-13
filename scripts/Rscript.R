### Packages

library(dplyr)
library(ggplot2)
library(tidyverse)
library(stringr)
library(wesanderson)
library(lme4)
library(scales)
library(simr)


########################## Experiment 1 ##################################

####### Data importing

X1_42_LR <- read.csv("../Data/1-42 LR.csv") # Import data

X1_42_LR$bee_id <- as.factor(X1_42_LR$bee_id) # treat as a categorical factor
summary(X1_42_LR)


small_X1_42_LR <-subset(X1_42_LR, number == "small") #subset original data sets into data for the Small Number and Large number tests
mean(small_X1_42_LR$choice)
large_X1_42_LR <-subset(X1_42_LR, number == "large")
mean(large_X1_42_LR$choice)

X1_42_UD <- read.csv("../Data/1-42_UD.csv")

X1_42_UD$BEEID <- as.factor(X1_42_UD$BEEID) # treat as a categorical factor
summary(X1_42_UD)
glimpse(X1_42_UD)
X1_42_UD$CHOICE <- as.factor(X1_42_UD$CHOICE)
small_X1_42_UD <-subset(X1_42_UD, NUMBER == "small") #subset original data sets into data for the Small Number and Large number tests
mean(small_X1_42_UD$CHOICE)
summary(small_X1_42_UD)

large_X1_42_UD <-subset(X1_42_UD, NUMBER == "large")
summary(large_X1_42_UD)
mean(large_X1_42_UD$CHOICE)


##### Regression analysis for testing horizontal spatial-quantity associations

model_small_LR <- glmer(choice~ 1 + (1|bee_id), family=binomial(link="logit"), small_X1_42_LR) # Regression model for the Small Number test
summary(model_small_LR)

confint(model_small_LR) #confidence interval
smallLRconfintlow<-(exp(-0.6466950)/(1 + exp(-0.6466950))) #reverse link function to extract upper and lower bounds of CI
smallLRconfinthigh<-(exp(0.4573276)/(1 + exp(0.4573276)))

model_large_LR <- glmer(choice~ 1 + (1|bee_id), family=binomial(link="logit"), large_X1_42_LR) # Regression model for the Large number test
summary(model_large_LR)

confint(model_large_LR) #confidence interval
largeLRconfintlow<-(exp(-0.8955361)/(1 + exp(-0.8955361))) #reverse link function
largeLRconfinthigh<-(exp(-0.4451618)/(1 + exp(-0.4451618)))


##### Regression models for testing of vertical spatial-quantity associations


model_small_UD <- glmer(CHOICE~ 1 + (1|BEEID), family=binomial(link="logit"), small_X1_42_UD)
summary(model_small_UD)

confint(model_small_UD) #confidence interval
smallUDconfintlow<-(exp(-1.466148)/(1 + exp(-1.466148))) #reverse link function
smallUDconfinthigh<-(exp(-0.3185877)/(1 + exp(-0.3185877)))

model_large_UD <- glmer(CHOICE~ 1 + (1|BEEID), family=binomial(link="logit"), large_X1_42_UD)
summary(model_large_UD)

confint(model_large_UD)

largeUDconfintlow<-(exp(-1.7406589)/(1 + exp(-1.7406589))) #reverse link function
largeUDconfinthigh<-(exp(-0.7373596)/(1 + exp(-0.7373596)))




########################## Experiment 2 ##################################


####### Data importing

testing_data <- read.csv("../Data/part 2_testing.csv") # data from all non-rewarded tests
testing_data$bee_id<-factor(testing_data$bee_id)
summary (testing_data)
LR_test <-subset(testing_data, group == "LR") #Subset bees in the Left to right group
RL_test <-subset(testing_data, group == "RL") #Subset bees in the Right to Left group

part_2_training <- read_csv("../Data/part 2_training.csv") # data from only training data


############ Regression analyses for Left to right tests

# LR small number test

LTS_test <- subset(LR_test, test_type == "LTS") #subset data into the Left to right small number test
summary(LTS_test)

testing_LR_S <- glmer(choice ~ 1 + (1|bee_id), LTS_test, family = binomial) # generalised linear mixed model
summary(testing_LR_S)
mean(LTS_test$choice)
confint(testing_LR_S) # confidence interval
testing_LR_S_confintlow<-(exp(-0.2484372)/(1 + exp(-0.2484372))) #reverse link function
testing_LR_S_confinthigh<-(exp(0.8311611)/(1 + exp(0.8311611)))  #reverse link function

# LR large number test

LTB_test <- subset(LR_test, test_type == "LTB") #subset data into the Left to right large number test
summary(LTB_test)
testing_LR_B <- glmer(choice ~ 1 + (1|bee_id), LTB_test, family = binomial) # generalised linear mixed model
summary(testing_LR_B)
mean(LTB_test$choice)
confint(testing_LR_B) #confidence interval
testing_LR_L_confintlow<-(exp(0.1393019)/(1 + exp(0.1393019))) #reverse link function
testing_LR_L_confinthigh<-(exp(1.049324)/(1 + exp(1.049324))) #reverse link function


# LR small number transfer test
TTS_test <- subset(LR_test, test_type == "TTS") #subset data into the Left to right small number transfer test
summary(TTS_test)
testing_LR_TTS <- glmer(choice ~ 1 + (1|bee_id), TTS_test, family = binomial) # generalised linear mixed model
summary(testing_LR_TTS)
confint(testing_LR_TTS) #confidence interval
mean(TTS_test$choice)
testing_LR_TTS_confintlow<-(exp(-0.5415068)/(1 + exp(-0.5415068))) #reverse link function
testing_LR_TTS_confinthigh<-(exp(0.06512261)/(1 + exp(0.065122614))) #reverse link function


# LR large/big number transfer test

TTB_test <- subset(LR_test, test_type == "TTB") #subset data into the Left to right larger number transfer test
summary(TTB_test)
testing_LR_TTB <- glmer(choice ~ 1 + (1|bee_id), data = TTB_test, family = binomial) # generalised linear mixed model
summary(testing_LR_TTB)
mean(TTB_test$choice)
confint(testing_LR_TTB) #confidence interval
testing_LR_TTB_confintlow<-(exp(-0.08294314)/(1 + exp(-0.08294314))) #reverse link function
testing_LR_TTB_confinthigh<-(exp(0.6959484)/(1 + exp(0.6959484))) #reverse link function



############ Regression analyses for Right to left tests

# RL small number test
RL_LTS <- subset(RL_test, test_type == "LTS") #subset data into the Right to left small number transfer test
summary(RL_LTS)
mean(RL_LTS$choice)
testing_RL_S <- glmer(choice ~ 1 + (1|bee_id), RL_LTS, family = binomial) # generalised linear mixed model
summary(testing_RL_S)
confint(testing_RL_S)  #confidence interval
testing_RL_S_confintlow<-(exp(-0.1594488)/(1 + exp(-0.1594488))) #reverse link function
testing_RL_S_confinthigh<-(exp(0.4445320)/(1 + exp(0.4445320))) #reverse link function


# RL large
RL_LTB<- subset(RL_test, test_type == "LTB") #subset data into the Right to left large number transfer test
mean(RL_LTB$choice)
summary(RL_LTB)
testing_RL_B <- glmer(choice ~ 1 + (1|bee_id), RL_LTB, family = binomial) # generalised linear mixed model
summary(testing_RL_B)
confint(testing_RL_B) #confidence interval
testing_RL_B_confintlow<-(exp(-0.4204101)/(1 + exp(-0.4204101))) #reverse link function
testing_RL_B_confinthigh<-(exp(0.1830620)/(1 + exp(0.1830620))) #reverse link function


# RL small number transfer test
RL_TTS <- subset(RL_test, test_type == "TTS") #subset data into the Right to left small number transfer test
summary(RL_TTS)
testing_RL_TTS <- glmer(choice ~ 1 + (1|bee_id), RL_TTS, family = binomial) # generalised linear mixed model
mean(RL_TTS$choice)
summary(testing_RL_TTS)
confint(testing_RL_TTS) #confidence interval
testing_RL_TTS_confintlow<-(exp(-0.1359353)/(1 + exp(-0.1359353))) #reverse link function
testing_RL_TTS_confinthigh<-(exp(0.4685901)/(1 + exp(0.4685901))) #reverse link function

# RL big/large number transfer test
RL_TTB <- subset(RL_test, test_type == "TTB") #subset data into the Right to left large number transfer test
summary(RL_TTB)
testing_RL_TTB <- glmer(choice ~ 1 + (1|bee_id), RL_TTB, family = binomial) # generalised linear mixed model
mean(RL_TTB$choice)
summary(testing_RL_TTB)
confint(testing_RL_TTB) #confidence interval
testing_RL_TTB_confintlow<-(exp(-0.2880571)/(1 + exp(-0.2880571))) #reverse link function
testing_RL_TTB_confinthigh<-(exp(0.3866840)/(1 + exp(0.3866840))) #reverse link function



#### Regression model comparing the performance of bees trained to left-to-right with bees trained to right-to-left

Trainingcompar_lm <- glmer(choice ~ group + (1|bee_id), part_2_training, family = binomial) # generalised linear mixed model 
summary(Trainingcompar_lm)




########################## Figures ##################################

cbPalette <- c("#C6CDF7", "#E6A0C4") # Set colours for graphs

######## Figure 5 A

# First we need to calculate the confidence intervals of the R direction bars in the graphs (the chance of selecting R), which is just the reverse of the choices made towards L
# Because of this, we can calculate the confidence intervals using the CIs from the choices towards L

largeRLconfintlow=(0.5-largeLRconfinthigh)+0.5 # Calculating CIs
largeRLconfinthigh=(0.5-largeLRconfintlow)+0.5

smallRLconfintlow=(0.5-smallLRconfinthigh)+0.5
smallRLconfinthigh=(0.5-smallLRconfintlow)+0.5


R <- data.frame(number = rep(c("Large","Small"), each = 2),   # Create dataframe including information for the graph 
                Direction = rep(c("L","R"), times = 2),
                proportion = c(0.34,0.66, 0.47,0.53),
                confintlow = c(largeLRconfintlow,largeRLconfintlow, smallLRconfintlow,smallRLconfintlow),
                confinthigh = c(largeLRconfinthigh,largeRLconfinthigh,smallLRconfinthigh,smallRLconfinthigh))


X1_42_LR$NUMBER <- as.factor(X1_42_LR$number) 
X1_42_LR$BEEID <- as.factor(X1_42_LR$bee_id)

meanpoints <- X1_42_LR %>%   # Calculate mean of each block for each bee
  group_by(BEEID, NUMBER) %>% 
  summarize(proportion = mean(choice))
levels(meanpoints$NUMBER) <- str_to_title(levels(meanpoints$NUMBER)) # recapitalize condition names
colnames(meanpoints)[2] ="number" # rename column to match dframe, because the column name needs to match the dframe as we want to plot both in the same graph
meanpoints$Direction <- "L"

meanpoints$proportion[meanpoints$proportion == 0] <- 0.02 # Change all 0 values to 0.02 so they can be seen on the graph
meanpoints$position <- ifelse(meanpoints$number == "Large", 0.8, 1.8)  # Set the exact position of geompoints to be overlayed on the bar graphs


exp1.LR <- ggplot(R, aes(x = number, y = proportion, fill = Direction)) +  # ggplot structure
  geom_bar(stat = "identity", position = "dodge") + 
  geom_errorbar(data = R, aes(x = number, y = proportion, ymin = confintlow, ymax = confinthigh, group = Direction), width = .2, size = 1, position = position_dodge(.9)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1, suffix = NULL), limits = c(0, 1)) +
  geom_hline(yintercept = 0.5, linetype = "dashed", colour = "grey", size = 1) +
  labs(x = "Number Tests", y = "Mean proportion of choosing L or R in two number tests") +
  scale_fill_manual(values = cbPalette) +
  geom_point(data = meanpoints, aes(x = position, y = proportion, group = number), position = position_jitterdodge(jitter.width = 0.2, jitter.height = 0.02, dodge.width = 0.9), show.legend = FALSE, size = 2.2, colour = "azure4", na.rm = FALSE) + 
  theme(panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 13),
        axis.title = element_text(size = 17),
        legend.key.size = unit(1.2, 'cm'))
print(exp1.LR)



######## Figure 5 B

largeDUconfintlow=(0.5-largeUDconfinthigh)+0.5 # Calculating CIs
largeDUconfinthigh=(0.5-largeUDconfintlow)+0.5

smallDUconfintlow=(0.5-smallUDconfinthigh)+0.5
smallDUconfinthigh=(0.5-smallUDconfintlow)+0.5


R2 <- data.frame(number = rep(c("Large","Small"), each = 2),  # Create dataframe including information for the graph 
                 Direction = rep(c("D","U"), times = 2),
                 proportion = c(0.71,0.29, 0.64,0.36),
                 confintlow = c(largeDUconfintlow,largeUDconfintlow, smallDUconfintlow,smallUDconfintlow),
                 confinthigh = c(largeDUconfinthigh,largeUDconfinthigh,smallDUconfinthigh,smallUDconfinthigh))

X1_42_UD$NUMBER <- as.factor(X1_42_LR$number) 
X1_42_UD$BEEID <- as.factor(X1_42_LR$bee_id)

meanpoints2 <- X1_42_UD %>%   # Calculate mean of each block for each bee
  group_by(BEEID, NUMBER) %>% 
  summarize(proportion = mean(CHOICE))

levels(meanpoints2$NUMBER) <- str_to_title(levels(meanpoints2$NUMBER)) # recapitalize condition names
colnames(meanpoints2)[2] ="number" # rename column to match dframe, because the column name needs to match the dframe as we want to plot both in the same graph
levels(meanpoints2$number) <- str_to_title(levels(meanpoints2$number)) # recapitalize condition names
colnames(meanpoints2)[2] ="number" # rename column to match dframe, because the column name needs to match the dframe as we want to plot both in the same graph
meanpoints2$Direction <- "U" # Adds a column to be consistent with the structure of R2
meanpoints2$proportion[meanpoints2$proportion == 0] <- 0.02 # Change all 0 values to 0.02 so they can be seen on the graph
meanpoints2$position <- ifelse(meanpoints2$number == "Large", 0.8, 1.8)  # Set the exact position of geompoints to be overlayed on the bar graphs

exp1.UD <- ggplot(R2, aes(x = number, y = proportion, fill = Direction)) + # ggplot structure
  geom_bar(stat = "identity", position = "dodge") + 
  geom_errorbar(data = R2, aes(x = number, y = proportion, ymin = confintlow, ymax = confinthigh, group = Direction), width = .2, position = position_dodge(.9), size = 1) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1, suffix = NULL), limits = c(0, 1)) +
  geom_hline(yintercept = 0.5, linetype = "dashed", colour = "grey", size = 1) +
  labs(x = "Number Tests", y = "Mean proportion of choosing U or D in two number tests") +
  scale_fill_manual(values = cbPalette) +
  geom_point(data = meanpoints2, aes(x = position, y = proportion, group = Direction), position = position_jitterdodge(jitter.width = 0.2, jitter.height = 0.02, dodge.width = 0.9), show.legend = FALSE, size = 2.2, colour = "azure4", na.rm = FALSE) + 
  theme(panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 13),
        axis.title = element_text(size = 17),
        legend.key.size = unit(1.2, 'cm'))
print(exp1.UD)

######## Figure 6 A

dat <- part_2_training

# Convert relevant columns to factors
dat$bee_id <- as.factor(dat$bee_id) 
dat$trial <- as.factor(dat$trial)
dat$block <- as.factor(dat$block)

# Filter for LR group
dat.lr <- dat %>% filter(group == "LR") 

# Calculate mean choice per bee_id and block
dat.lr.mean <- dat.lr %>% 
  group_by(bee_id, block) %>% 
  summarize(mean = mean(choice), .groups = 'drop')

# Calculate mean, standard error, and confidence intervals per block
dat.lr.trial.mean <- dat.lr.mean %>% 
  group_by(block) %>% 
  summarise(
    mean.trial = mean(mean), 
    sd = sd(mean),
    n = n(),
    sem = sd / sqrt(n), 
    ci_lower = mean.trial - qt(0.975, df=n-1) * sem,
    ci_upper = mean.trial + qt(0.975, df=n-1) * sem
  )


learning.curve.lr <- ggplot(dat.lr.mean, # ggplot structure
                            aes(x = block, 
                                y = mean,
                                group = factor(bee_id)))+ # to group the same flower in different visual systems into one group, so the lines will connect them together.
  geom_hline(yintercept = 0.5, linetype = "dashed", colour = "grey") + 
  geom_jitter(size = 1, alpha = 0.5, width = 0.05, height = 0.005, colour = dat.lr.mean$bee_id) + # optional geom_jitter()
  geom_line(size = 0.5, alpha = 0.2, colour = dat.lr.mean$bee_id)+
  geom_point(data = dat.lr.mean,
             aes(x = "10",
                 y = dat.lr.trial.mean$mean.trial[1]), size = 2) +
  geom_point(data = dat.lr.mean,
             aes(x = "20",
                 y = dat.lr.trial.mean$mean.trial[2]), size = 2) +
  geom_point(data = dat.lr.mean,
             aes(x = "30",
                 y = dat.lr.trial.mean$mean.trial[3]), size = 2) +
  geom_point(data = dat.lr.mean,
             aes(x = "40",
                 y = dat.lr.trial.mean$mean.trial[4]), size = 2) +
  geom_errorbar(data = dat.lr.mean,
                aes( x = "10", # specify it is for block 10
                     ymin = 0.4265905, 
                     ymax = 0.5263507), width = .2) +
  geom_errorbar(data = dat.lr.mean,
                aes( x = "20", # specify it is for block 10
                     ymin = 0.4391837, 
                     ymax = 0.6078751), width = .2) +
  geom_errorbar(data = dat.lr.mean,
                aes( x = "30", # specify it is for block 10
                     ymin = 0.5113649, 
                     ymax = 0.6651057), width = .2) +
  geom_errorbar(data = dat.lr.mean,
                aes( x = "40", # specify it is for block 10
                     ymin = 0.5846400, 
                     ymax = 0.7212424), width = .2) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1, suffix = NULL), limits = c(0,1.1), expand = c(0, 0)) + # suffix = NULL to remove % on the y labels
  xlab("Trial") +     
  ylab("Mean proportion of correct choice") +
  theme(axis.title.x = element_text(size = 17),  
        axis.text.x  = element_text(size = 14, colour = "black"),
        axis.title.y = element_text(size = 17, vjust = 1),
        axis.text.y = element_text(size = 14, colour = "black")) +
  scale_color_gradient() +
  theme_classic()
print(learning.curve.lr)
ggsave(learning.curve.lr, width = 5, height = 5, dpi = 300, filename = "LRline.pdf")




######## Figure 6 B


R3 <- data.frame(Number = rep(c("Large number","Small number"), each = 2), # Create dataframe for graph
                 test = rep(c("Learning test","Transfer test"), times = 2),
                 proportion = c(0.63,0.57, 0.56,0.44),
                 confintlow = c(testing_LR_L_confintlow,testing_LR_TTB_confintlow, testing_LR_S_confintlow,testing_LR_TTS_confintlow),
                 confinthigh = c(testing_LR_L_confinthigh,testing_LR_TTB_confinthigh, testing_LR_S_confinthigh,testing_LR_TTS_confinthigh))



LR_test$test <- as.factor(LR_test$test)
LR_test$BEEID <- as.factor(LR_test$bee_id)



meanpoints3 <- LR_test %>%   # Calculate mean of each block for each bee
  group_by(bee_id, test, test_type) %>% 
  summarize(proportion = mean(choice))
meanpoints3$number <- meanpoints3$test_type
colnames(meanpoints3)[5] ="Number"

meanpoints3$proportion[meanpoints3$proportion == 0] <- 0.02 # Set all 0 values to 0.02
meanpoints3$proportion[meanpoints3$proportion == 1] <- 0.98 # Set all 1 values to 0.98
meanpoints3$Number[meanpoints3$test_type == "LTB"] <- "Large number" # Adding relevant columns to dataframe
meanpoints3$Number[meanpoints3$test_type == "LTS"] <- "Small number"
meanpoints3$Number[meanpoints3$test_type == "TTB"] <- "Large number"
meanpoints3$Number[meanpoints3$test_type == "TTS"] <- "Small number"

meanpoints3$test2[meanpoints3$test == "LT"] <- "Learning test" # Adding relevant columns to dataframe
meanpoints3$test2[meanpoints3$test == "TT"] <- "Transfer test"
meanpoints3$test <- meanpoints3$test2



exp2.LR <- ggplot(R3, aes(x = test, y = proportion, fill = Number)) +
  geom_bar(stat = "identity", position = "dodge") + 
  geom_errorbar(data = R3, aes(x = test, y = proportion, ymin = confintlow, ymax = confinthigh, group = Number), width = .2, size = 1, position = position_dodge(.9)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1, suffix = NULL), limits = c(0, 1)) +
  geom_hline(yintercept = 0.5, linetype = "dashed", colour = "grey", size = 1) +
  labs(x = "Number Tests", y = "Mean proportion of correct choices") +
  scale_fill_manual(values = cbPalette, name = "Number test") +
  geom_point(data = meanpoints3, aes(x = test, y = proportion, fill = Number), position = position_jitterdodge(jitter.width = 0.1, jitter.height = 0.02, dodge.width = 0.9), show.legend = FALSE, size = 2.2, colour = "azure4", na.rm = FALSE) + 
  geom_errorbar(data = R3, aes(x = test, y = proportion, ymin = confintlow, ymax = confinthigh, group = Number), width = .2, position = position_dodge(.9)) +
  theme(panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 13),
        axis.title = element_text(size = 17),
        legend.key.size = unit(0.9, 'cm'))

print(exp2.LR)



######## Figure 6 A

# Convert relevant columns to factors
dat$bee_id <- as.factor(dat$bee_id)
dat$trial <- as.factor(dat$trial)
dat$block <- as.factor(dat$block)

# Filter for RL group
dat.rl <- dat %>% filter(group == "RL") 

# Calculate mean choice per bee_id and block
dat.rl.mean <- dat.rl %>% 
  group_by(bee_id, block) %>% 
  summarize(mean = mean(choice), .groups = 'drop')

# Calculate mean, standard error, and confidence intervals per block
dat.rl.trial.mean <- dat.rl.mean %>% 
  group_by(block) %>% 
  summarise(
    mean.trial = mean(mean), 
    sd = sd(mean),
    n = n(),
    sem = sd / sqrt(n), 
    ci_lower = mean.trial - qt(0.975, df=n-1) * sem,
    ci_upper = mean.trial + qt(0.975, df=n-1) * sem
  )

learning.curve.rl <- ggplot(dat.rl.mean,  # ggplot structure
                            aes(x = block, 
                                y = mean,
                                group = factor(bee_id)))+ # to group the same flower in different visual systems into one group, so the lines will connect them together.
  geom_hline(yintercept = 0.5, linetype = "dashed", colour = "grey") + 
  geom_jitter(size = 1, alpha = 0.5, width = 0.05, height = 0.005, colour = dat.rl.mean$bee_id) + # optional geom_jitter()
  geom_line(size = 0.5, alpha = 0.2, colour = dat.lr.mean$bee_id)+
  geom_point(data = dat.rl.mean,
             aes(x = "10",
                 y = dat.rl.trial.mean$mean.trial[1]), size = 2) +
  geom_point(data = dat.rl.mean,
             aes(x = "20",
                 y = dat.rl.trial.mean$mean.trial[2]), size = 2) +
  geom_point(data = dat.rl.mean,
             aes(x = "30",
                 y = dat.rl.trial.mean$mean.trial[3]), size = 2) +
  geom_point(data = dat.rl.mean,
             aes(x = "40",
                 y = dat.rl.trial.mean$mean.trial[4]), size = 2) +
  geom_errorbar(data = dat.rl.mean,
                aes( x = "10", # specify it is for block 10
                     ymin = 0.4465775, 
                     ymax = 0.6122460), width = .2) +
  geom_errorbar(data = dat.lr.mean,
                aes( x = "20", # specify it is for block 10
                     ymin = 0.4490619, 
                     ymax = 0.5862322), width = .2) +
  geom_errorbar(data = dat.lr.mean,
                aes( x = "30", # specify it is for block 10
                     ymin = 0.4921413, 
                     ymax = 0.6960940), width = .2) +
  geom_errorbar(data = dat.lr.mean,
                aes( x = "40", # specify it is for block 10
                     ymin = 0.5618374, 
                     ymax = 0.7440450), width = .2) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1, suffix = NULL), limits = c(0,1.1),  expand = c(0, 0)) + # suffix = NULL to remove % on the y labels
  xlab("Trial") +     
  ylab("Mean proportion of correct choice") +
  scale_color_gradient() +
  theme_classic()+
  theme(axis.title.x = element_text(size = 17),  
        axis.text.x  = element_text(size = 14, colour = "black"),
        axis.title.y = element_text(size = 17, vjust = 1),
        axis.text.y = element_text(size = 14, colour = "black")) 
print(learning.curve.rl)
ggsave(learning.curve.rl, width = 5, height = 5, dpi = 300, filename = "RLline.pdf")



######## Figure 6 B

R4 <- data.frame(Number = rep(c("Large number","Small number"), each = 2), # Create dataframe for bargraph
                 test = rep(c("Learning test","Transfer test"), times = 2),
                 proportion = c(0.46,0.51, 0.54,0.54),
                 confintlow = c(testing_RL_B_confintlow,testing_RL_TTB_confintlow, testing_RL_S_confintlow,testing_RL_TTS_confintlow),
                 confinthigh = c(testing_RL_B_confinthigh,testing_RL_TTB_confinthigh, testing_RL_S_confinthigh,testing_RL_TTS_confinthigh))


RL_test$test <- as.factor(RL_test$test)
RL_test$BEEID <- as.factor(RL_test$bee_id)



meanpoints4 <- RL_test %>%   # Calculate mean of each block for each bee
  group_by(bee_id, test, test_type) %>% 
  summarize(proportion = mean(choice))
meanpoints4$number <- meanpoints4$test_type
colnames(meanpoints3)[5] ="Number"

meanpoints4$proportion[meanpoints4$proportion == 0] <- 0.02 # Set 0 value to 0.02
meanpoints4$proportion[meanpoints4$proportion == 1] <- 0.98 # Set 1 value to 0.98
meanpoints4$Number[meanpoints4$test_type == "LTB"] <- "Large number" # Adds relevant information to dataframe
meanpoints4$Number[meanpoints4$test_type == "LTS"] <- "Small number"
meanpoints4$Number[meanpoints4$test_type == "TTB"] <- "Large number"
meanpoints4$Number[meanpoints4$test_type == "TTS"] <- "Small number"

meanpoints4$test2[meanpoints4$test == "LT"] <- "Learning test" # Adds relevant information to dataframe
meanpoints4$test2[meanpoints4$test == "TT"] <- "Transfer test"
meanpoints4$test <- meanpoints4$test2



exp2.RL <- ggplot(R4, aes(x = test, y = proportion, fill = Number)) + #ggplot structure
  geom_bar(stat = "identity", position = "dodge") + 
  geom_errorbar(data = R4, aes(x = test, y = proportion, ymin = confintlow, ymax = confinthigh, group = Number), size= 1, width = .2, position = position_dodge(.9)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1, suffix = NULL), limits = c(0, 1)) +
  geom_hline(yintercept = 0.5, linetype = "dashed", colour = "grey", size = 1) +
  labs(x = "Test", y = "Mean proportion of correct choices") +
  scale_fill_manual(values = cbPalette, name = "Number test") +
  geom_point(data = meanpoints4, aes(x = test, y = proportion, fill = Number), position = position_jitterdodge(jitter.width = 0.1, jitter.height = 0.02, dodge.width = 0.9), show.legend = FALSE, size = 2.2, colour = "azure4", na.rm = FALSE) + 
  geom_errorbar(data = R4, aes(x = test, y = proportion, ymin = confintlow, ymax = confinthigh, group = Number), width = .2, position = position_dodge(.9)) +
  theme(panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 13),
        axis.title = element_text(size = 17),
        legend.key.size = unit(0.9, 'cm'))

print(exp2.RL)

#################### Power analysis by simulation for an effect size of interest = 0.64 ####################


# Parameters
n_bees <- 42
n_data_points <- 10
n_total <- n_bees * n_data_points
p1 <- 0.64  # Alternative hypothesis proportion
alpha <- 0.05
nsim <- 1000  # Number of simulations

# Function to simulate data with random effects
simulate_data <- function(n_bees, n_data_points, p, random_effect_sd) {
  bee_ids <- rep(1:n_bees, each = n_data_points)
  random_effects <- rnorm(n_bees, 0, random_effect_sd)
  choices <- sapply(1:n_total, function(i) {
    prob <- plogis(qlogis(p) + random_effects[bee_ids[i]])
    rbinom(1, 1, prob)
  })
  data.frame(bee_id = factor(bee_ids), choice = choices)
}

# General assumptions for random effect standard deviation
random_effect_sd <- 0.4418846

# Simulate data with the specified effect size
set.seed(123)  # For reproducibility
simulated_data <- simulate_data(n_bees, n_data_points, p1, random_effect_sd)

# Print the simulated dataframe
print(head(simulated_data))
summary(simulated_data)

# Initialize counter for significant results
significant_results <- 0

# Run simulations
for (i in 1:nsim) {
  # Simulate data under the alternative hypothesis
  simulated_data <- simulate_data(n_bees, n_data_points, p1, random_effect_sd)
  
  # Fit the model to the simulated data
  model <- tryCatch({
    glmer(choice ~ 1 + (1 | bee_id), family = binomial(link = "logit"), data = simulated_data)
  }, error = function(e) {
    message("Error fitting model: ", e)
    return(NULL)
  })
  
  if (!is.null(model)) {
    # Extract p-value for the intercept
    p_value <- summary(model)$coefficients[1, "Pr(>|z|)"]
    
    # Check if p-value is below the significance level
    if (p_value < alpha) {
      significant_results <- significant_results + 1
    }
  }
}

# Calculate power
power_estimate <- significant_results / nsim
print(paste("Estimated Power: ", round(power_estimate, 3)))



################# Power analysis across multiple effect sizes ###########################

# Parameters
n_bees <- 17
n_data_points <- 10
n_total <- n_bees * n_data_points
p0 <- 0.5  # Baseline proportion
alpha <- 0.05
nsim <- 1000  # Number of simulations

# Function to simulate data with random effects
simulate_data <- function(n_bees, n_data_points, p, random_effect_sd) {
  bee_ids <- rep(1:n_bees, each = n_data_points)
  random_effects <- rnorm(n_bees, 0, random_effect_sd)
  choices <- sapply(1:n_total, function(i) {
    prob <- plogis(qlogis(p) + random_effects[bee_ids[i]])
    rbinom(1, 1, prob)
  })
  data.frame(bee_id = factor(bee_ids), choice = choices)
}

# General assumptions for random effect standard deviation
random_effect_sd <- 0.4418846
set.seed(123) 

# Function to calculate power for a given effect size
calculate_power <- function(p1) {
  significant_results <- 0
  
  for (i in 1:nsim) {
    # Simulate data under the alternative hypothesis
    simulated_data <- simulate_data(n_bees, n_data_points, p1, random_effect_sd)
    
    # Fit the model to the simulated data
    model <- tryCatch({
      glmer(choice ~ 1 + (1 | bee_id), family = binomial(link = "logit"), data = simulated_data)
    }, error = function(e) {
      message("Error fitting model: ", e)
      return(NULL)
    })
    
    if (!is.null(model)) {
      # Extract p-value for the intercept
      p_value <- summary(model)$coefficients[1, "Pr(>|z|)"]
      
      # Check if p-value is below the significance level
      if (p_value < alpha) {
        significant_results <- significant_results + 1
      }
    }
  }
  
  # Calculate power
  power_estimate <- significant_results / nsim
  return(power_estimate)
}

# Effect sizes to test (different values of p1)
effect_sizes <- seq(0.5, 0.8, by = 0.05)

# Calculate power for each effect size
power_results <- sapply(effect_sizes, calculate_power)

# Print power results for different values of p1
print(data.frame(p1 = effect_sizes, power = round(power_results, 3)))



##### Check simulated dataframes


library(lme4)

# Function to simulate data with random effects
simulate_data <- function(n_bees, n_data_points, p, random_effect_sd) {
  bee_ids <- rep(1:n_bees, each = n_data_points)
  random_effects <- rnorm(n_bees, 0, random_effect_sd)
  choices <- sapply(1:n_total, function(i) {
    prob <- plogis(qlogis(p) + random_effects[bee_ids[i]])
    rbinom(1, 1, prob)
  })
  data.frame(bee_id = factor(bee_ids), choice = choices)
}

# Function to compute summary statistics
compute_summary <- function(data) {
  choice_summary <- summary(data$choice)
  choice_mean <- mean(data$choice)
  choice_sd <- sd(data$choice)
  list(Choice_Summary = choice_summary, Mean = choice_mean, SD = choice_sd)
}

# Generate and print five dataframes with their summary statistics
set.seed(123)  # For reproducibility
data_list <- lapply(1:5, function(x) simulate_data(n_bees, n_data_points, p1, random_effect_sd))

for (i in 1:5) {
  cat(paste("Dataframe", i, ":\n"))
  print(head(data_list[[i]], 10))  # Print first 10 rows for brevity
  cat("\nSummary Statistics:\n")
  summary_stats <- compute_summary(data_list[[i]])
  print(summary_stats$Choice_Summary)
  cat("Mean:", summary_stats$Mean, "\n")
  cat("SD:", summary_stats$SD, "\n\n")
}
