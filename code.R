#Read data
df <- read.csv("Ptdata.csv")
df <- subset(df, select = - c(PtID, P3, P4, P5, P9, P10))

colnames(df)[2:6] <- c("Age", "AdmType", "SOFA", "Gender", "DisStatus")
#Tidy data
df <- df[df$DocID %in% c('doc-01', 'doc-02', 'doc-03', 'doc-04', 'doc-05'), ]
df$y <- ifelse(df$DisStatus == 'A', 1, 0)
df$DocID[1:79] <- paste0("Phys",1)
df$DocID[80:174] <- paste0("Phys",2)
df$DocID[175:264] <- paste0("Phys",3)
df$DocID[265:365] <- paste0("Phys",4)
df$DocID[366:489] <- paste0("Phys",5)
df$Age <- factor(df$Age)
df$AdmType <- factor(df$AdmType)
df$Gender <- factor(df$Gender)
df$DocID <- factor(df$DocID)

############################Using Propensity Weighting with Parametric Method#######################################
# install.packages(PSweight)
library(PSweight)
ps.mult <- DocID ~ Age + AdmType + SOFA + Gender
bal.mult <- SumStat(ps.formula = ps.mult, weight = c('IPW', 'overlap'), data = df)

ps.p <- propensity<-bal.mult$propensity

plot(bal.mult, type = "density")
plot(bal.mult, metric = "ASD")
plot(bal.mult, metric = "PSD")

#IPW
ate.mult <- PSweight(ps.formula = ps.mult, yname = 'y', data = df, weight = 'IPW')
contrasts.mult <- rbind(c(1, -1, 0, 0, 0), c(1, 0, -1, 0, 0), c(1, 0, 0, -1, 0), c(1, 0, 0, 0, -1), 
                         c(0, 1, -1, 0, 0), c(0, 1, 0, -1, 0), c(0, 1, 0, 0, -1), c(0, 0, 1, -1, 0),
                         c(0, 0, 1, 0, -1), c(0, 0, 0, 1, -1))
sum.ate.mult.rr <- summary(ate.mult, type = 'RR', contrast = contrasts.mult)
exp(sum.ate.mult.rr$estimates[, c(1, 4, 5)])
exp(sum.ate.mult.rr$estimates[, 5])-exp(sum.ate.mult.rr$estimates[, 4])

#GOW
ato.mult <- PSweight(ps.formula = ps.mult, yname = 'y', data = df, weight = 'overlap')
sum.ato.mult.rr <- summary(ato.mult, type = 'RR', contrast = contrasts.mult)
exp(sum.ato.mult.rr$estimates[, c(1, 4, 5)])
exp(sum.ato.mult.rr$estimates[, 5])-exp(sum.ato.mult.rr$estimates[, 4])

#IPW-aug
out.y <- y ~ Age + AdmType + SOFA + Gender
ate.mult.aug <- PSweight(ps.formula = ps.mult, yname = 'y', data = df, augmentation = T, 
                         out.formula = out.y, family = 'binomial', weight='IPW')
sum.ate.mult.aug.rr <- summary(ate.mult.aug, type='RR', contrast=contrasts.mult)
exp(sum.ate.mult.aug.rr$estimates[, c(1, 4, 5)])
exp(sum.ate.mult.aug.rr$estimates[, 5])-exp(sum.ate.mult.aug.rr$estimates[, 4])

#GOW-aug
ato.mult.aug <- PSweight(ps.formula = ps.mult, yname = 'y', data = df, augmentation = T, 
                         out.formula = out.y, family = 'binomial')
sum.ato.mult.aug.rr <- summary(ato.mult.aug, type='RR', contrast=contrasts.mult)
exp(sum.ato.mult.aug.rr$estimates[, c(1, 4, 5)])
exp(sum.ato.mult.aug.rr$estimates[, 5])-exp(sum.ato.mult.aug.rr$estimates[, 4])

############################Using Propensity Weighting with Super Learning Method#######################################
# remotes::install_github("tlverse/sl3")
library(sl3)

sl3_list_properties()
sl3_list_learners(properties = 'categorical')

# https://tlverse.org/sl3/reference/index.html#sl-learners
#2 XGBoost models
lrn_xgb1 <- Lrnr_xgboost$new(nrounds=20000, max_depth=2, eta=0.001, subsample=0.9)
lrn_xgb2 <- Lrnr_xgboost$new(nrounds=20000, max_depth=1, eta=0.001, subsample=0.9)
#2 random forest models
lrn_rf1 <- Lrnr_ranger$new(mtry=3, num.trees = 200, sample.fraction=0.9)
lrn_rf2 <- Lrnr_ranger$new(mtry=3, num.trees = 100, sample.fraction=0.9)

Pstack <- Stack$new(lrn_xgb1, lrn_xgb2, lrn_rf1, lrn_rf2)
Psl <- Lrnr_sl$new(learners = Pstack)

# could automatically done by make_sl3_Task
df$Age <- factor_to_indicators(df$Age)
df$AdmType <- factor_to_indicators(df$AdmType)
df$Gender <- factor_to_indicators(df$Gender)

df$DocID <- factor(df$DocID)
Ptask <- make_sl3_Task(
  data = df,
  outcome = "DocID",
  covariates = c("Age", "AdmType", "SOFA", "Gender"),
  folds = 5L
)

set.seed(2023)
Psl_fit <- Psl$train(task = Ptask)

Psl_fit$coefficients

ps.sl.preds <- Psl_fit$predict(task = Ptask)

ps.sl <- matrix(unlist(ps.sl.preds), ncol=5, byrow = T)

#Draw catterplot of propensity scores 
# install.packages(ggplot2)
library(ggplot2)
dfplot<-data.frame(x = ps.p[, 1], y = ps.sl[, 1])
pt1<-ggplot(data = dfplot, mapping = aes(x = x, y = y)) + geom_point()+ geom_abline() + lims(x = c(0.05, 0.4), y = c(0.05, 0.4))+
  theme_bw()+ xlab("Parametric propensity score estimates") +
  ylab("Super learning propensity score estimates")
  
dfplot<-data.frame(x = ps.p[, 2], y = ps.sl[, 2])
pt2<-ggplot(data = dfplot, mapping = aes(x = x, y = y)) + geom_point()+ geom_abline() + lims(x = c(0.05, 0.4), y = c(0.05, 0.4))+
  theme_bw()+ xlab("Parametric propensity score estimates") +
  ylab("Super learning propensity score estimates")  
  
dfplot<-data.frame(x = ps.p[, 3], y = ps.sl[, 3])
pt3<-ggplot(data = dfplot, mapping = aes(x = x, y = y)) + geom_point()+ geom_abline() + lims(x = c(0.05, 0.4), y = c(0.05, 0.4))+
  theme_bw()+ xlab("Parametric propensity score estimates") +
  ylab("Super learning propensity score estimates")

dfplot<-data.frame(x = ps.p[, 4], y = ps.sl[, 4])
pt4<-ggplot(data = dfplot, mapping = aes(x = x, y = y)) + geom_point()+ geom_abline() + lims(x = c(0.05, 0.4), y = c(0.05, 0.4))+
  theme_bw()+ xlab("Parametric propensity score estimates") +
  ylab("Super learning propensity score estimates")

dfplot<-data.frame(x = ps.p[, 5], y = ps.sl[, 5])
pt5<-ggplot(data = dfplot, mapping = aes(x = x, y = y)) + geom_point()+ geom_abline() + lims(x = c(0.05, 0.4), y = c(0.05, 0.4))+
  theme_bw()+ xlab("Parametric propensity score estimates") +
  ylab("Super learning propensity score estimates")

bal.mult.sl <- SumStat(ps.estimate = ps.sl, zname ="DocID", xname = c("Age", "AdmType", "SOFA", "Gender"), weight = c('IPW', 'overlap'), data=df)

plot(bal.mult.sl, type='density')

plot(bal.mult.sl, metric='ASD')

plot(bal.mult.sl, metric='PSD')

#IPW-SL
colnames(ps.sl) <- c("Phys1", "Phys2", "Phys3", "Phys4", "Phys5")
ate.mult.sl <- PSweight(ps.estimate = ps.sl, yname = 'y', zname ="DocID", data=df, weight = 'IPW')
sum.ate.mult.sl.rr <- summary(ate.mult.sl, type = 'RR', contrast = contrasts.mult)
exp(sum.ate.mult.sl.rr$estimates[, c(1, 4, 5)])
exp(sum.ate.mult.sl.rr$estimates[, 5])-exp(sum.ate.mult.sl.rr$estimates[, 4])

#GOW-SL
ato.mult.sl <- PSweight(ps.estimate = ps.sl, yname = 'y', zname ="DocID", data=df, weight = 'overlap')
sum.ato.mult.sl.rr <- summary(ato.mult.sl, type = 'RR', contrast = contrasts.mult)
exp(sum.ato.mult.sl.rr$estimates[, c(1, 4, 5)])
exp(sum.ato.mult.sl.rr$estimates[, 5])-exp(sum.ato.mult.sl.rr$estimates[, 4])

df$y.sl <- factor(df$DisStatus)
#DocID <- factor_to_indicators(df$DocID)
#DocID automatically to one-hot encoding by make_sl3_Task

Qtask <- make_sl3_Task(
  data = df,
  outcome = "y.sl",
  covariates = c("Age", "AdmType", "SOFA", "Gender"),
  folds = 5L
)

Qtask1 <- make_sl3_Task(
  data = df[df$DocID=='Phys1',],
  outcome = "y.sl",
  covariates = c("Age", "AdmType", "SOFA", "Gender"),
  folds = 5L
)

Qtask2 <- make_sl3_Task(
  data = df[df$DocID=='Phys2',],
  outcome = "y.sl",
  covariates = c("Age", "AdmType", "SOFA", "Gender"),
  folds = 5L
)

Qtask3 <- make_sl3_Task(
  data = df[df$DocID=='Phys3',],
  outcome = "y.sl",
  covariates = c("Age", "AdmType", "SOFA", "Gender"),
  folds = 5L
)

Qtask4 <- make_sl3_Task(
  data = df[df$DocID=='Phys4',],
  outcome = "y.sl",
  covariates = c("Age", "AdmType", "SOFA", "Gender"),
  folds = 5L
)

Qtask5 <- make_sl3_Task(
  data = df[df$DocID=='Phys5',],
  outcome = "y.sl",
  covariates = c("Age", "AdmType", "SOFA", "Gender"),
  folds = 5L
)

# adding lrn_xgb3, error
Qstack <- Stack$new(lrn_xgb1, lrn_xgb2, lrn_rf1, lrn_rf2)
Qsl <- Lrnr_sl$new(learners = Qstack)

set.seed(2023)
Qsl_fit1 <- Qsl$train(task = Qtask1)
Qsl_fit1$coefficients
set.seed(2023)
Qsl_fit2 <- Qsl$train(task = Qtask2)
Qsl_fit2$coefficients
set.seed(2023)
Qsl_fit3 <- Qsl$train(task = Qtask3)
Qsl_fit3$coefficients
set.seed(2023)
Qsl_fit4 <- Qsl$train(task = Qtask4)
Qsl_fit4$coefficients
set.seed(2023)
Qsl_fit5 <- Qsl$train(task = Qtask5)
Qsl_fit5$coefficients

q.sl <- matrix(NA, nrow=489, ncol=5)
q.sl[, 1] <- Qsl_fit1$predict(task = Qtask)
q.sl[, 2] <- Qsl_fit2$predict(task = Qtask)
q.sl[, 3] <- Qsl_fit3$predict(task = Qtask)
q.sl[, 4] <- Qsl_fit4$predict(task = Qtask)
q.sl[, 5] <- Qsl_fit5$predict(task = Qtask)
colnames(q.sl) <- c("Phys1", "Phys2", "Phys3", "Phys4", "Phys5")

#IPW-aug-SL
ate.mult.aug.sl <- PSweight(ps.estimate = ps.sl, yname = 'y', zname ="DocID", data = df, 
                            augmentation = T, out.estimate=q.sl, weight='IPW')
sum.ate.mult.aug.sl.rr <- summary(ate.mult.aug.sl, type='RR', contrast=contrasts.mult)
exp(sum.ate.mult.aug.sl.rr$estimates[, c(1, 4, 5)])
exp(sum.ate.mult.aug.sl.rr$estimates[, 5])-exp(sum.ate.mult.aug.sl.rr$estimates[, 4])

# GOW-aug-SL
ato.mult.aug.sl <- PSweight(ps.estimate = ps.sl, yname = 'y', zname ="DocID", data = df, 
                            augmentation = T, out.estimate=q.sl)
sum.ato.mult.aug.sl.rr <- summary(ato.mult.aug.sl, type='RR', contrast=contrasts.mult)
exp(sum.ato.mult.aug.sl.rr$estimates[, c(1, 4, 5)])
exp(sum.ato.mult.aug.sl.rr$estimates[, 5])-exp(sum.ato.mult.aug.sl.rr$estimates[, 4])



########################bootstrap for super learning method to calculate confidence intervals###############################
IPW_est<-matrix(NA,10,25)
GOW_est<-matrix(NA,10,25)
IPWaug_est<-matrix(NA,10,25)
GOWaug_est<-matrix(NA,10,25)
IPW_est[,1]<-sum.ate.mult.sl.rr$estimates[, c(1)]
GOW_est[,1]<-sum.ato.mult.sl.rr$estimates[, c(1)]
IPWaug_est[,1]<-sum.ate.mult.aug.sl.rr$estimates[, c(1)]
GOWaug_est[,1]<-sum.ato.mult.aug.sl.rr$estimates[, c(1)]


sl3_list_properties()
sl3_list_learners(properties = 'categorical')

# https://tlverse.org/sl3/reference/index.html#sl-learners
lrn_xgb1 <- Lrnr_xgboost$new(nrounds=20000, max_depth=2, eta=0.001, subsample=0.9)
lrn_xgb2 <- Lrnr_xgboost$new(nrounds=20000, max_depth=1, eta=0.001, subsample=0.9)
lrn_rf1 <- Lrnr_ranger$new(mtry=3, num.trees = 200, sample.fraction=0.9)
lrn_rf2 <- Lrnr_ranger$new(mtry=3, num.trees = 100, sample.fraction=0.9)

contrasts.mult <- rbind(c(1, -1, 0, 0, 0), c(1, 0, -1, 0, 0), c(1, 0, 0, -1, 0), c(1, 0, 0, 0, -1), 
                        c(0, 1, -1, 0, 0), c(0, 1, 0, -1, 0), c(0, 1, 0, 0, -1), c(0, 0, 1, -1, 0),
                        c(0, 0, 1, 0, -1), c(0, 0, 0, 1, -1))
q.sl <- matrix(NA, nrow=489, ncol=5)
for (i in 1:24) {
#bootstrap sample dfx  
  set.seed(i)
  index<-sample(x=nrow(df),dim(df)[1],replace = TRUE)
  dfx=df[index,]
  rownames(dfx)<-1:dim(df)[1]
 
# could automatically done by make_sl3_Task
dfx$Age <- factor_to_indicators(df$Age)
dfx$AdmType <- factor_to_indicators(df$AdmType)
dfx$Gender <- factor_to_indicators(df$Gender)

dfx$DocID <- factor(df$DocID)


Pstack <- Stack$new(lrn_xgb1, lrn_xgb2, lrn_rf1, lrn_rf2)
Psl <- Lrnr_sl$new(learners = Pstack)


Ptask <- make_sl3_Task(
  data = dfx,
  outcome = "DocID",
  covariates = c("Age", "AdmType", "SOFA", "Gender"),
  folds = 5L
)

set.seed(2023)
Psl_fit <- Psl$train(task = Ptask)

Psl_fit$coefficients

ps.sl.preds <- Psl_fit$predict(task = Ptask)

ps.sl <- matrix(unlist(ps.sl.preds), ncol=5, byrow = T)

#IPW-SL
colnames(ps.sl) <- c("Phys1", "Phys2", "Phys3", "Phys4", "Phys5")
ate.mult.sl <- PSweight(ps.estimate = ps.sl, yname = 'y', zname ="DocID", data=dfx, weight = 'IPW')
sum.ate.mult.sl.rr <- summary(ate.mult.sl, type = 'RR', contrast = contrasts.mult)
IPW_est[,i+1]<-sum.ate.mult.sl.rr$estimates[, c(1)]
ato.mult.sl <- PSweight(ps.estimate = ps.sl, yname = 'y', zname ="DocID", data=dfx, weight = 'overlap')
sum.ato.mult.sl.rr <- summary(ato.mult.sl, type = 'RR', contrast = contrasts.mult)
GOW_est[,i+1]<-sum.ato.mult.sl.rr$estimates[, c(1)]

dfx$y.sl <- factor(dfx$DisStatus)


Qtask <- make_sl3_Task(
  data = dfx,
  outcome = "y.sl",
  covariates = c("Age", "AdmType", "SOFA", "Gender"),
  folds = 5L
)

Qtask1 <- make_sl3_Task(
  data = dfx[dfx$DocID=='Phys1',],
  outcome = "y.sl",
  covariates = c("Age", "AdmType", "SOFA", "Gender"),
  folds = 5L
)

Qtask2 <- make_sl3_Task(
  data = dfx[dfx$DocID=='Phys2',],
  outcome = "y.sl",
  covariates = c("Age", "AdmType", "SOFA", "Gender"),
  folds = 5L
)

Qtask3 <- make_sl3_Task(
  data = dfx[dfx$DocID=='Phys3',],
  outcome = "y.sl",
  covariates = c("Age", "AdmType", "SOFA", "Gender"),
  folds = 5L
)

Qtask4 <- make_sl3_Task(
  data = dfx[dfx$DocID=='Phys4',],
  outcome = "y.sl",
  covariates = c("Age", "AdmType", "SOFA", "Gender"),
  folds = 5L
)

Qtask5 <- make_sl3_Task(
  data = dfx[dfx$DocID=='Phys5',],
  outcome = "y.sl",
  covariates = c("Age", "AdmType", "SOFA", "Gender"),
  folds = 5L
)

# adding lrn_xgb3, error
Qstack <- Stack$new(lrn_xgb1, lrn_xgb2, lrn_rf1, lrn_rf2)
Qsl <- Lrnr_sl$new(learners = Qstack)

set.seed(2023)
Qsl_fit1 <- Qsl$train(task = Qtask1)
Qsl_fit1$coefficients
set.seed(2023)
Qsl_fit2 <- Qsl$train(task = Qtask2)
Qsl_fit2$coefficients
set.seed(2023)
Qsl_fit3 <- Qsl$train(task = Qtask3)
Qsl_fit3$coefficients
set.seed(2023)
Qsl_fit4 <- Qsl$train(task = Qtask4)
Qsl_fit4$coefficients
set.seed(2023)
Qsl_fit5 <- Qsl$train(task = Qtask5)
Qsl_fit5$coefficients


q.sl[, 1] <- Qsl_fit1$predict(task = Qtask)
q.sl[, 2] <- Qsl_fit2$predict(task = Qtask)
q.sl[, 3] <- Qsl_fit3$predict(task = Qtask)
q.sl[, 4] <- Qsl_fit4$predict(task = Qtask)
q.sl[, 5] <- Qsl_fit5$predict(task = Qtask)
colnames(q.sl) <- c("Phys1", "Phys2", "Phys3", "Phys4", "Phys5")

#IPW-aug-SL
ate.mult.aug.sl <- PSweight(ps.estimate = ps.sl, yname = 'y', zname ="DocID", data = df, 
                            augmentation = T, out.estimate=q.sl, weight='IPW')
sum.ate.mult.aug.sl.rr <- summary(ate.mult.aug.sl, type='RR', contrast=contrasts.mult)
IPWaug_est[,i+1]<-sum.ate.mult.aug.sl.rr$estimates[, c(1)]

# GOW-aug-SL
ato.mult.aug.sl <- PSweight(ps.estimate = ps.sl, yname = 'y', zname ="DocID", data = df, 
                            augmentation = T, out.estimate=q.sl)
sum.ato.mult.aug.sl.rr <- summary(ato.mult.aug.sl, type='RR', contrast=contrasts.mult)
GOWaug_est[,i+1]<-sum.ato.mult.aug.sl.rr$estimates[, c(1)]

print(i)
}

# install.packages(matrixStats)
library(matrixStats)

exp(IPW_est[,1]-1.96*rowSds(IPW_est))
exp(IPW_est[,1]+1.96*rowSds(IPW_est))
exp(IPW_est[,1]+1.96*rowSds(IPW_est))-exp(IPW_est[,1]-1.96*rowSds(IPW_est))

exp(GOW_est[,1]-1.96*rowSds(GOW_est))
exp(GOW_est[,1]+1.96*rowSds(GOW_est))
exp(GOW_est[,1]+1.96*rowSds(GOW_est))-exp(GOW_est[,1]-1.96*rowSds(GOW_est))

exp(IPWaug_est[,1]-1.96*rowSds(IPWaug_est))
exp(IPWaug_est[,1]+1.96*rowSds(IPWaug_est))
exp(IPWaug_est[,1]+1.96*rowSds(IPWaug_est))-exp(IPWaug_est[,1]-1.96*rowSds(IPWaug_est))

exp(GOWaug_est[,1]-1.96*rowSds(GOWaug_est))
exp(GOWaug_est[,1]+1.96*rowSds(GOWaug_est))
exp(GOWaug_est[,1]+1.96*rowSds(GOWaug_est))-exp(GOWaug_est[,1]-1.96*rowSds(GOWaug_est))

#####################bootstrap for parametric method to calculate confidence intervals###########################

P_IPW_est<-matrix(NA,10,25)
P_GOW_est<-matrix(NA,10,25)
P_IPWaug_est<-matrix(NA,10,25)
P_GOWaug_est<-matrix(NA,10,25)
P_IPW_est[,1]<-sum.ate.mult.rr$estimates[, c(1)]
P_GOW_est[,1]<-sum.ato.mult.rr$estimates[, c(1)]
P_IPWaug_est[,1]<-sum.ate.mult.aug.rr$estimates[, c(1)]
P_GOWaug_est[,1]<-sum.ato.mult.aug.rr$estimates[, c(1)]

ps.mult <- DocID ~ Age + AdmType + SOFA + Gender
out.y <- y ~ Age + AdmType + SOFA + Gender
for (i in 1:24) {
  #bootstrap sample dfx  
  set.seed(i)
  index<-sample(x=nrow(df),dim(df)[1],replace = TRUE)
  dfx=df[index,]
  rownames(dfx)<-1:dim(df)[1]
  
  bal.mult <- SumStat(ps.formula = ps.mult, weight = c('IPW', 'overlap'), data = dfx)
  
  ps.p <- propensity<-bal.mult$propensity
  
  #IPW
  ate.mult <- PSweight(ps.formula = ps.mult, yname = 'y', data = dfx, weight = 'IPW')

  sum.ate.mult.rr <- summary(ate.mult, type = 'RR', contrast = contrasts.mult)
  P_IPW_est[,i+1]<-sum.ate.mult.rr$estimates[, c(1)]
  
  #GOW
  ato.mult <- PSweight(ps.formula = ps.mult, yname = 'y', data = dfx, weight = 'overlap')
  sum.ato.mult.rr <- summary(ato.mult, type = 'RR', contrast = contrasts.mult)
  P_GOW_est[,i+1]<-sum.ato.mult.rr$estimates[, c(1)]
  
  #IPW-aug
  
  ate.mult.aug <- PSweight(ps.formula = ps.mult, yname = 'y', data = dfx, augmentation = T, 
                           out.formula = out.y, family = 'binomial', weight='IPW')
  sum.ate.mult.aug.rr <- summary(ate.mult.aug, type='RR', contrast=contrasts.mult)
  P_IPWaug_est[,i+1]<-sum.ate.mult.aug.rr$estimates[, c(1)]
  
  #GOW-aug
  ato.mult.aug <- PSweight(ps.formula = ps.mult, yname = 'y', data = dfx, augmentation = T, 
                           out.formula = out.y, family = 'binomial')
  sum.ato.mult.aug.rr <- summary(ato.mult.aug, type='RR', contrast=contrasts.mult)
  P_GOWaug_est[,i+1]<-sum.ato.mult.aug.rr$estimates[, c(1)]
}


exp(P_IPW_est[,1]-1.96*rowSds(P_IPW_est))
exp(P_IPW_est[,1]+1.96*rowSds(P_IPW_est))
exp(P_IPW_est[,1]+1.96*rowSds(P_IPW_est))-exp(P_IPW_est[,1]-1.96*rowSds(P_IPW_est))

exp(P_GOW_est[,1]-1.96*rowSds(P_GOW_est))
exp(P_GOW_est[,1]+1.96*rowSds(P_GOW_est))
exp(P_GOW_est[,1]+1.96*rowSds(P_GOW_est))-exp(P_GOW_est[,1]-1.96*rowSds(P_GOW_est))

exp(P_IPWaug_est[,1]-1.96*rowSds(P_IPWaug_est))
exp(P_IPWaug_est[,1]+1.96*rowSds(P_IPWaug_est))
exp(P_IPWaug_est[,1]+1.96*rowSds(P_IPWaug_est))-exp(P_IPWaug_est[,1]-1.96*rowSds(P_IPWaug_est))

exp(P_GOWaug_est[,1]-1.96*rowSds(P_GOWaug_est))
exp(P_GOWaug_est[,1]+1.96*rowSds(P_GOWaug_est))
exp(P_GOWaug_est[,1]+1.96*rowSds(P_GOWaug_est))-exp(P_GOWaug_est[,1]-1.96*rowSds(P_GOWaug_est))