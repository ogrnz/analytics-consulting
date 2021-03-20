# Prepare session ---------------------------------------------------------

#HV
 setwd("E:/Universite de Genève/Faculté d'économie/Mon Master en statistique/A1S2/Analytics Consulting/analytics-consulting/A")

library(readxl)
library(tidyverse)
library(ggmosaic)
library(gghalves)
library(tidyverse)
library(mgcv) 
library(MASS)
library(nnet)
library(lme4) 
library(gee)
library(robustbase) 
library(brglm)
library(ggplot2) 
library(aod) 
library(pscl) 
library(MuMIn)
library(janitor)


# Data import&cleaning -----------------------------------------------------------

clothing <- read_excel("Aclothing.xls")
# I rename it because I got too much headaches (same name :p )
clothing <- clothing %>% 
  rename(
    ClothingType = Clothing
  )

clothing$ClothingType <- as.factor(clothing$ClothingType)
clothing$Gender <- as.factor(clothing$Gender)
clothing$InteractionType <- as.factor(clothing$InteractionType)

str(clothing)
head(clothing)

attach(clothing)

# EDA (Exploratory Data Analysis) -----------------------------------------

boxplot(clothing)



# Model selection ---------------------------------------------------------
#HV: I will change my model depending on Sneha findings
# First I show the possible relationship

clothing %>% ggplot(aes(x=ClothingType,
                        y=Count,
                        color=InteractionType)) +
  geom_jitter(height = 0.25,width = 0.25, alpha = 0.4) +
  geom_smooth(se = FALSE) +
  theme_minimal() +
  labs(title="Relation between clothing and number of interactions",
       x="Clothing Type",
       y="Number of interactions",
       caption="Figure 1") +
  ggsave("figures/fig1.png")

clothing %>% ggplot(aes(x=ClothingType,
                        y=Count,
                        fill=ClothingType)) +
  geom_half_boxplot(alpha = 0.6) +
  geom_half_violin(side="r", alpha=0.4, bw=0.45) +
  theme(legend.position = "none") +
  theme_minimal() +
  labs(title="Relation between clothing and number of interactions",
       x="Clothing Type",
       y="Number of interactions",
       caption="Figure 2") +
  guides(fill = FALSE)+
  coord_flip()+
  scale_fill_viridis_d(begin = 0.1, end = 0.8) +
  ggsave("figures/fig2.png")

clothing %>% ggplot(aes(x=Gender,
                        y=Count,
                        color=InteractionType)) +
  geom_jitter(height = 0.25,width = 0.25, alpha = 0.4) +
  geom_smooth(se = FALSE) +
  theme_minimal() +
  labs(title="Relation between gender and number of interactions",
       x="Gender",
       y="Number of interactions",
       caption="Figure 3")+
  scale_fill_viridis_d(begin = 0.1, end = 0.8)+
  ggsave("figures/fig3.png")

clothing %>% ggplot(aes(x=Gender,
                        y=Count,
                        fill=Gender)) +
  geom_half_boxplot(alpha = 0.6) +
  geom_half_violin(side="r", alpha=0.4, bw=0.45) +
  theme(legend.position = "none") +
  theme_minimal() +
  labs(title="Relation between gender and number of interactions",
       x="Gender",
       y="Number of interactions",
       caption="Figure 4") +
  guides(fill = FALSE)+
  coord_flip()+
  scale_fill_viridis_d(begin = 0.1, end = 0.8)+
  ggsave("figures/fig4.png")


clothing %>% ggplot(aes(x=ClothingType,
                        y=InteractionType,
                        color=InteractionType)) +
  geom_jitter(height = 0.25,width = 0.25, alpha = 0.4) +
  geom_smooth(se = FALSE) +
  theme_minimal() +
  labs(title="Relation between clothing and type of interactions",
       x="Clothing Type",
       y="Type of Interactions",
       caption="Figure 5") +
  ggsave("figures/fig5.png")

clothing %>% ggplot(aes(x=Gender,
                        y=InteractionType,
                        color=InteractionType)) +
  geom_jitter(height = 0.25,width = 0.25, alpha = 0.35) +
  geom_smooth(se = FALSE) +
  theme_minimal() +
  labs(title="Relation between gender and type of interactions",
       x="Gender",
       y="Type of Interactions",
       caption="Figure 6")+
  ggsave("figures/fig6.png")

# CONCLUSION: no effect on gender or clothing type on number/type of interaction


# Basic Model -------------------------------------------------------------

clothing.lme <- lm(Count ~ Gender + ClothingType + Gender*ClothingType)
summary(clothing.lme)
# T-test non-significant, adjusted R-squared at 1.7%
# BUT
# Fisher test significant -> overall model significant

plot(clothing.lme)
# Non-normal data, heavy outliers, non-significant fitted values

plot(resid(clothing.lme)~Gender)
abline(h=0,col=2)

# HELP!
# try it with GLM
clothing.glm <- glm(Count ~ Gender + ClothingType + Gender*ClothingType, family="logit")


