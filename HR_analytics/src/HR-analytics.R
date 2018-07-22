library(reshape)
library(ggplot2)
library(ggthemes)
library(glmnet)
library(ModelGood)

# load data
s1 <- read.csv ("../data/HR_comma_sep.csv",header=TRUE) 
# categorical variable: sales, salary

s2<-s1[,c(1:8)]

cor.matrix <- round(cor(s2, use = "pairwise.complete.obs",
                        method = "spearman"), digits = 2)

## Turning it all into a dataframe

cor.dat <- melt(cor.matrix)
cor.dat <- data.frame(cor.dat)

## Plotting

theme_set(theme_solarized())

ggplot(cor.dat, aes(X2, X1, fill = value)) + 
  geom_tile() + 
  geom_text(aes(X2, X1, label = value), color = "#073642", size = 4) +
  scale_fill_gradient(name=expression("Spearman" * ~ rho), low = "#fdf6e3", high = "steelblue", 
                      breaks=seq(0, 1, by = 0.2), limits = c(-0.4, 0.4)) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(x = "", y = "") + 
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1, title.position = "top",
                               title.hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1), 
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        legend.justification = c(1, 0),
        legend.position = c(-2, 0),
        legend.direction = "horizontal") +
  guides(fill = guide_colorbar(barwidth = 7, 
                               barheight = 1, 
                               title.position = "top", 
                               title.hjust = 0.5))

s1$left.f=factor(s1$left)
qplot(satisfaction_level, data = s1, 
      color = left.f, fill = left.f,
      geom = "histogram", 
      main = "histogram: satisfaction level vs left or not")

s1$left.f=factor(s1$left)
qplot(time_spend_company, data = s1, 
      color = left.f, fill = left.f,
      geom = "histogram",
      main = "histogram: time spend company vs left or not")

# model

x <- model.matrix(left ~ satisfaction_level + last_evaluation + number_project + average_montly_hours	+ time_spend_company + Work_accident + promotion_last_5years + sales + salary,s1)
y <- s1$left
out <- cv.glmnet(x,y,alpha=1,family="binomial",type.measure = "mse")
plot(out)

# minimize value of lambda
min_lambda <- out$lambda.min

# regression coeffficients
coef(out)

# model 2

fit.single <-glm(left ~ satisfaction_level, data=s1, family="binomial")
# summary(fit.single)
roc.single<- Roc(fit.single)

fit.twovariable <-glm(left ~ satisfaction_level+time_spend_company, data=s1, family="binomial")
# summary(fit.twovariable)

fit.fullmodel<-glm(left ~ satisfaction_level + last_evaluation + number_project + average_montly_hours + time_spend_company + Work_accident + promotion_last_5years + sales + salary, family = "binomial", data = s1)
# summary(fit.fullmodel)

plot(Roc(list("Full" = fit.fullmodel,"Single" = fit.single, "Two" = fit.twovariable)),legend = TRUE, auc = TRUE)
