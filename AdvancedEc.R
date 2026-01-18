library(haven)
library(dplyr)
library(tidyverse)
library(rddapp)
library(ggplot2)
library(vtable)
library(ggeasy)
#######
# patchwork for plots' arrangements:
#install.packages("patchwork")
library(patchwork)
# install.packages("rddensity")
# install.packages("rdd")
library(rddensity)
library(rdd)
# install.packages("officer")
#install.packages("flextable")
library(officer)
library(flextable)
#######
'“amazon_data.dta”, provides information for each collected edition on Amazon 
and Project Gutenberg (if available) on a monthly level (from April 2011 to
April 2012), including information on prices, availability, demand, and other
Amazon-specific characteristics, as well as information on the title-level, 
including the original year of publication, prizes won, British library checkouts,
and membership in Harry Bloom’s Western Canon.'

data_amazon = read_dta('amazon_data.dta')

data_edition_det = read_dta('edition_details.dta')

'dataset, “Gutenberg_downloads.dta” provides Project Gutenberg download counts
over a 30-day period (April 2014) on 29,290 documents
that are available on Project Gutenberg.'
# Can we use the newest data? From 2014 to 2022 there might be a change:
data_gtb_dwnlds = read_dta('gutenberg_downloads.dta')
str(data_amazon)
# Summary for the data:
summary(data_amazon)
head(data_amazon)
data_amazon$format
data_amazon$format2 <- data_amazon$format
data_amazon$format2[data_amazon$format2 != 'Hardcover' & data_amazon$format2 != 'Paperback'] = 'other'
data_amazon$format2


#Create dummy variables:
library(caret)
dummy <- dummyVars("~ format2", data=data_amazon, sep = ".")
dummy
my_onehot <- data.frame(predict(dummy, newdata = data_amazon)) %>% as_tibble()
colnames(my_onehot) = c('hc','other', 'pb')
my_onehot
summary(my_onehot)
data_amazon_dv <- data_amazon %>% cbind(my_onehot)
summary(data_amazon_dv)

######
#sum hc pb other age new used major prize price sales gb demand if t==0 & gb==0
df_t_gb_0 <- data_amazon_dv %>% filter(t==0 & gb==0) %>% select(hc,pb,other,age,new,used,major, prize, price, sales, gb,demand)
#Why? What do we see here?
summary(df_t_gb_0)
#Why? What do we see here?
#sum hc pb other age new used major prize price sales gb demand if t==0 & gb==0 & post==0:
df_t_gb_post_0 <- data_amazon_dv %>% filter(t==0 & gb==0 & post1923==0) %>% select(hc,pb,other,age,new,used,major, prize, price, sales, gb,demand)
#Why? What do we see here?
summary(df_t_gb_post_0)
df_t_gb_0_post_1 <- data_amazon_dv %>% filter(t==0 & gb==0 & post1923==1) %>% select(hc,pb,other,age,new,used,major, prize, price, sales, gb,demand)
#Why? What do we see here?
summary(df_t_gb_0_post_1)

#t-test:
#hc pb other age new used picture major prize price sales gb demand ntitle ntitleformat
# 
df_t_gb_0_for_ttest <- data_amazon_dv %>% filter(t==0 & gb==0) %>% select(post1923,hc,pb,other,age,new,used,major, prize, price, sales, gb,demand, ntitle, ntitleformat)
lapply(df_t_gb_0_for_ttest[-1], function(x) t.test(x ~ df_t_gb_0_for_ttest$post1923))

summary(data_amazon_dv)
for (i in c('Hardcover', 'Paperback', 'E-book')) {
  temp_df <- data_amazon %>% filter(format==i & t==1) %>% select(ntitleformat, post1923)
  print(i)
  temp_ttest<-t.test(temp_df$ntitleformat ~ temp_df$post1923)
  print(temp_ttest)
}

# sum gb if t==0 & gb>0
summary(data_amazon_dv$gb[data_amazon_dv$t==0 & data_amazon_dv$gb>0])

#st(data_amazon)
st(data_amazon_dv, group = 'post1923', group.test = TRUE, vars=c('new','used','ntitleformat'))

#drop t==0
data_amazon_dv_no_t0 <- data_amazon_dv %>% filter(t!=0)

##** RDD: Price (Table 3)
#A sharp regression discontinuity design (RDD).

#replace year=year-1923
data_amazon_RDD_price <- data_amazon_dv_no_t0 %>% mutate(year=year-1923)
data_amazon_RDD_price$logprice = log(data_amazon_RDD_price$amazonprice)
data_amazon_RDD_price$amazonprice[data_amazon_RDD_price$format=="Gutenberg"] = 0
#data_amazon_RDD_price
head(data_amazon_RDD_price)
summary(data_amazon_RDD_price)

#mean log price value in the period < 1923:
mean(is.na(data_amazon_RDD_price$logprice[data_amazon_RDD_price$post1923 == 0]))
#mean log price value in the period >= 1923:
mean(is.na(data_amazon_RDD_price$logprice[data_amazon_RDD_price$post1923 == 1]))
# It could be interpreted as a discontinuity, which supports the thesis of the author.

#logprice ~ post plr year* prize_author canon* new age major i.formats i.t if year>=-8 & year<=7
data_for_reg_price <- data_amazon_RDD_price %>% filter(year>=-8 & year<=7)
summary(data_for_reg_price)

# encode string variables (format) to numeric:
data_for_reg_price$formatNum = as.numeric(as.factor(data_for_reg_price$format))
summary(data_for_reg_price$formatNum)

library(DataExplorer)
plot_density(data_amazon_RDD_price)

# density (McCrary test):
density <- rddensity(data_amazon_RDD_price$year, c = 0.0)
density
#plots and at the p-values of the McCrary tests: and if the p-values are at or below .05, then we can be 95% confident that manipulation has occurred.
#p-value 0.2136572
print(DCdensity(data_amazon_RDD_price$year,c = 0.0,plot=TRUE))

plot_desity_2 <- rdplotdensity(density, data_amazon_RDD_price$year)
plot_desity_2

#Visualization of the data and the cliff year:
plot_my_model_reg_price <- ggplot(data_amazon_RDD_price, aes(x=year, y = post1923, color = factor(post1923))) + geom_point(alpha = 0.4)+ggtitle("Sonny Bono Copyright extension act validity - post1923") + ggeasy::easy_center_title()
plot_my_model_reg_price
plot_my_model_reg_price + geom_vline(xintercept = 0, color = 'black', size = 1, linetype = "dotted")+ theme_minimal()

# We use a treatment variable post1923: 0 indicates a year before 1923 (a title is a public one in this case); 1 indicates that the title is under copyright.

plot_my_model_reg_price_2 = ggplot(data_amazon_RDD_price, aes(x=year, y = logprice, color = factor(post1923))) + geom_point(alpha = 0.4) + ggtitle("Sonny Bono Copyright extension act validity - post1923") + ggeasy::easy_center_title()
plot_my_model_reg_price_2 + geom_vline(xintercept = 0, color = 'black', size = 1, linetype = "dotted") + geom_smooth(method = 'lm') + theme_minimal()
# Loess smoothing (just for an experiment):
plot_my_model_reg_price_2 + geom_vline(xintercept = 0, color = 'black', size = 1, linetype = "dotted") + geom_smooth(method = 'loess') + theme_minimal()
# 3rd order polynomial:
plot_my_model_reg_price_2 + geom_vline(xintercept = 0, color = 'black', size = 1, linetype = "dotted") + stat_smooth(aes(year, logprice, group = post1923), method = "lm", formula = y ~ poly(x,3))

### Linear regressions.

#We estimate the effect setting the copyright extension threshold year on 1923 on log price using a cubic polynomial in distance to the copyright validity threshold
# and number of terms including copyright validity dummy post1923. Subsequent regressions based on different setups for terms used:


# linear regression:
# 1 intercept + 10 convariates = 11
my_model_reg_price <- lm(logprice ~ post1923 + plr + year + year^2 + year^3 + prize_author + canon_title + canon_author + new + age + major + formatNum, data_for_reg_price)

summary(my_model_reg_price)

# We use robust standard errors for result interpretation:

#Robust and clustered standard errors with R
#we want accurate standard errors to focus on confidence intervals when reporting findings.
#standard lm() command doesn’t have built-in support for robust or clustered standard errors:
#install.packages("sandwich")
library(sandwich)
library(lmtest)

# Clustered robust standard errors with lm()
# The standard errors allow for intragroup correlation, relaxing the usual requirement that the observations be independent. The observations are independent across groups (clusters) but not necessarily within groups. cluster specifies to which
# group each observation belongs, in our case - distance (year) from 1923. This affects the standard errors and variance – covariance matrix of the estimators but not the estimated coefficients.
my_model_reg_price_robust_clustered <- coeftest(my_model_reg_price, vcov = vcovCL, cluster = ~year)
my_model_reg_price_robust_clustered

# We reproduce calculations of author keeping initial bandwidth [-8,7]: eight years before 1923 and eight years after (including 1923):

# Regression 2 (only hardcovers)

#logprice <- post year* plr prize_author canon* new age major i.t if year>=-8 & year<=7 & format=="Hardcover"
data_for_reg_price_2 <- data_amazon_RDD_price %>% filter(year>=-8 & year<=7 & format=="Hardcover")

# Graphical analysis

#Linear regression: one slope for non-treateted (w/o copyright) and treated (with copyright).
lm_tmp_lin <- lm(logprice ~ year + I(year>=0), data = data_for_reg_price_2)
lm_fun_lin <- function(x) predict(lm_tmp_lin, data.frame(year = x)) #log price prediction

plot_1rd_ord_p_m2 <- plot_my_model_reg_price_2 + 
  stat_function(
    data = data.frame(x = c(-8, 7),y = c(-3, 5)),aes(x = x,y=y),
    fun = lm_fun_lin,xlim = c(-8,-0.0001),
    col="red",size = 1.5) + geom_vline(xintercept = 0, color = 'black', size = 1, linetype = "dotted") +
  stat_function(
    data = data.frame(x = c(-8, 7),y = c(-3, 5)),aes(x = x,y=y),
    fun = lm_fun_lin,xlim = c(0.0001,7),
    col="red", size = 1.5 ) + theme_minimal()
plot_1rd_ord_p_m2

#Lm: 3rd order polynomial:
lm_tmp <- lm(logprice ~ poly(year,3) + I(year>=0), data = data_for_reg_price_2)
lm_fun <- function(x) predict(lm_tmp, data.frame(year = x)) #log price prediction
plot_3rd_ord_p_m2 <- plot_my_model_reg_price_2 + 
  stat_function(
    data = data.frame(x = c(-8, 7),y = c(0, 1)),aes(x = x,y=y),
    fun = lm_fun,xlim = c(-8,-0.0001),
    col="red",size = 1.5) + geom_vline(xintercept = 0, color = 'black', size = 1, linetype = "dotted") +
  stat_function(
    data = data.frame(x = c(-8, 7),y = c(-3, 5)),aes(x = x,y=y),
    fun = lm_fun,xlim = c(0.0001,7),
    col="red", size = 1.5 )  + theme_minimal()

plot_1rd_ord_p_m2 + plot_3rd_ord_p_m2 + plot_layout(ncol =2)


# Model fit with config: year>=-8 & year<=7 & format=="Hardcover".
my_model_reg_price_2 <- lm(logprice ~ post1923 + plr + year + year^2 + year^3 + prize_author + canon_title + canon_author + new + age + major, data_for_reg_price_2)
summary(my_model_reg_price_2)
my_model_reg_price_2_robust_clustered <- coeftest(my_model_reg_price_2, vcov = vcovCL, cluster = ~year)
my_model_reg_price_2_robust_clustered


##### visualization of the second linear regression with RDD:
#different slopes for intervals pre-1923 and post-1923
plot_my_model_reg_price_2 = ggplot(data_for_reg_price_2, aes(x=year, y = logprice, color = factor(post1923))) + geom_point(alpha = 0.4) + ggtitle("Model2: Sonny Bono Copyright extension act validity - post1923") + ggeasy::easy_center_title()
#+ geom_smooth(method = 'lm')
plot_my_model_reg_price_2 + geom_vline(xintercept = 0, color = 'black', size = 1, linetype = "dotted") + stat_smooth(aes(year, logprice, group = post1923), method = "lm", formula = y ~ x + I(x>=0) )
plot_my_model_reg_price_2 + geom_vline(xintercept = 0, color = 'black', size = 1, linetype = "dotted") + geom_smooth(method = 'lm') + theme_minimal()


#RDD (only paperbacks).

#logprice ~ post year* plr prize_author canon* new age major i.t if year>=-8 & year<=7 & format=="Paperback"
data_for_reg_price_3 <- data_amazon_RDD_price %>% filter(year>=-8 & year<=7 & format=="Paperback")

# Graphical analysis
plot_my_model_reg_price_3 = ggplot(data_for_reg_price_3, aes(x=year, y = logprice, color = factor(post1923))) + geom_point(alpha = 0.4) + ggtitle("Model 3 (only paperbacks): Sonny Bono Copyright extension act validity - post1923") + ggeasy::easy_center_title() + theme_minimal() + geom_vline(xintercept = 0, color = 'black', size = 1, linetype = "dotted")
plot_my_model_reg_price_3
#Linear regression: one slope for non-treateted (w/o copyright) and treated (with copyright).
lm_tmp <- lm(logprice ~ year + I(year>=0), data = data_for_reg_price_3)
lm_fun <- function(x) predict(lm_tmp, data.frame(year = x)) #log price prediction

plot_my_model_reg_price_3 + 
  stat_function(
    data = data.frame(x = c(-8, 7),y = c(-3, 5)),aes(x = x,y=y),
    fun = lm_fun,xlim = c(-8,-0.0001),
    col="red",size = 1.5) + geom_vline(xintercept = 0, color = 'black', size = 1, linetype = "dotted") +
  stat_function(
    data = data.frame(x = c(-8, 7),y = c(-3, 5)),aes(x = x,y=y),
    fun = lm_fun,xlim = c(0.0001,7),
    col="red", size = 1.5 ) + theme_minimal()


#Lm: 3rd order polynomial:
lm_tmp <- lm(logprice ~ poly(year,3) + I(year>=0), data = data_for_reg_price_3)
lm_fun <- function(x) predict(lm_tmp, data.frame(year = x)) #log price prediction
plot_my_model_reg_price_3 + 
  stat_function(
    data = data.frame(x = c(-8, 7),y = c(0, 1)),aes(x = x,y=y),
    fun = lm_fun,xlim = c(-8,-0.0001),
    col="red",size = 1.5) + geom_vline(xintercept = 0, color = 'black', size = 1, linetype = "dotted") +
  stat_function(
    data = data.frame(x = c(-8, 7),y = c(-3, 5)),aes(x = x,y=y),
    fun = lm_fun,xlim = c(0.0001,7),
    col="red", size = 1.5 )  + theme_minimal()



# RDD (only paperbacks)
my_model_reg_price_3 <- lm(logprice ~ post1923 + plr + year + year^2 + year^3 + prize_author + canon_title + canon_author + new + age + major, data_for_reg_price_3)
summary(my_model_reg_price_3)
my_model_reg_price_3_robust_clustered <- coeftest(my_model_reg_price_3, vcov = vcovCL, cluster = ~year)
my_model_reg_price_3_robust_clustered

# RDD
# Introduction of new variables - formatNum:
#logprice ~ post plr year* prize_author canon* ntitleformat new age major i.formats i.t if year>=-8 & year<=7
my_model_reg_price_4 <- lm(logprice ~ post1923 + plr + year + year^2 + year^3 + prize_author + canon_title + canon_author + new + age + major + ntitleformat + formatNum, data_for_reg_price)
summary(my_model_reg_price_4)
my_model_reg_price_4_robust_clustered <- coeftest(my_model_reg_price_4, vcov = vcovCL, cluster = ~year)
my_model_reg_price_4_robust_clustered

# added ntitleformat and using only hardcovers:
#logprice ~ post year* plr prize_author canon* ntitleformat new age major i.t if year>=-8 & year<=7 & format=="Hardcover", vce(cluster year)
# year>=-8 & year<=7 & format=="Hardcover"
my_model_reg_price_5 <- lm(logprice ~ post1923 + plr + year + year^2 + year^3 + prize_author + canon_title + canon_author + new + age + major + ntitleformat , data_for_reg_price_2)
summary(my_model_reg_price_5)
my_model_reg_price_5_robust_clustered <- coeftest(my_model_reg_price_5, vcov = vcovCL, cluster = ~year)
my_model_reg_price_5_robust_clustered

#logprice ~ post year* plr prize_author canon* ntitleformat new age major i.t if year>=-8 & year<=7 & format=="Paperback", vce(cluster year)
my_model_reg_price_6 <- lm(logprice ~ post1923 + plr + year + year^2 + year^3 + prize_author + canon_title + canon_author + new + age + major + ntitleformat, data_for_reg_price_3)
summary(my_model_reg_price_6)
my_model_reg_price_6_robust_clustered <- coeftest(my_model_reg_price_6, vcov = vcovCL, cluster = ~year)
my_model_reg_price_6_robust_clustered

est = rbind(summary(my_model_reg_price)$coefficients,
            summary(my_model_reg_price_2)$coefficients,
            summary(my_model_reg_price_3)$coefficients,
            summary(my_model_reg_price_4)$coefficients,
            summary(my_model_reg_price_5)$coefficients,
            summary(my_model_reg_price_6)$coefficients)

# overwriting std error:
est[, 2] = c(sqrt(diag(vcovCL(my_model_reg_price, cluster= ~ year))),
             sqrt(diag(vcovCL(my_model_reg_price_2, cluster= ~ year))),
             sqrt(diag(vcovCL(my_model_reg_price_3, cluster= ~ year))),
             sqrt(diag(vcovCL(my_model_reg_price_4, cluster= ~ year))),
             sqrt(diag(vcovCL(my_model_reg_price_5, cluster= ~ year))),
             sqrt(diag(vcovCL(my_model_reg_price_6, cluster= ~ year)))
             )
est[, 3] = est[, 1] / est[, 2]

est[, 4] = 2 * (1 - pt(abs(est[, 3]), df = c(rep(my_model_reg_price$df.residual, 11), 
                                             rep(my_model_reg_price_2$df.residual,10), 
                                             rep(my_model_reg_price_3$df.residual, 10),
                                             rep(my_model_reg_price_4$df.residual,12), 
                                             rep(my_model_reg_price_5$df.residual, 11),                                             
                                             rep(my_model_reg_price_6$df.residual, 11))))
est


#install.packages("jtools")
library(jtools)
#install.packages("ggstance")
#install.packages("huxtable")
library(ggstance)
library(huxtable)
# here can be more models:
plot_summs(my_model_reg_price, my_model_reg_price_2, my_model_reg_price_3, my_model_reg_price_4, my_model_reg_price_5, my_model_reg_price_6)
#export_summs(my_model_reg_price, my_model_reg_price_2, my_model_reg_price_3, my_model_reg_price_4, my_model_reg_price_5, my_model_reg_price_6, scale = TRUE)
export_summs(my_model_reg_price_robust_clustered, my_model_reg_price_2_robust_clustered, my_model_reg_price_3_robust_clustered, my_model_reg_price_4_robust_clustered, my_model_reg_price_5_robust_clustered, my_model_reg_price_6_robust_clustered, scale = TRUE, to.file="docx", file.name = "E:/nosave/aE/table_price.docx", number_format = "%.4f")
#export_summs(my_model_rdd_by_editions_robust_clustered, my_model_rdd_by_editions_hc_robust_clustered, my_model_rdd_by_editions_pb_robust_clustered, my_model_rdd_by_editions_eb_robust_clustered, scale = TRUE, to.file="docx", file.name = "E:/nosave/aE/table_2.docx", model.names = c('#Titles (All)', '#Titles (Hardcover)','#Titles (Paperback)','#Titles (E-Book)' ))



summary(my_model_reg_price_robust_clustered)
# We use the package rddapp to change bandwidths and evaluate these modifications.
#NB: To estimate the effect of a copyright more precisely, the author tighten the bound around 1923 to include only
#those bestsellers which were published between 1915 and 1930—seven years before
#and after the cliff year.

summary(data_amazon_RDD_price)
#Tuning of bandwidths:
#
#simple sharp RDD:
my_fitLLR = rd_est(logprice ~ year,
                cutpoint = 0,
                data = data_amazon_RDD_price,
                kernel = 'rectangular',
                t.design = "geq"
)
my_fitLLRSens = rd_sens_bw(my_fitLLR, bws = seq(1, 13, by = 1))
my_fitLLRSens
#plot:
sens_plot(my_fitLLRSens, level = 0.95, x = "bw", yrange = c(-0.15, 0.6))
# Based on the tuning 9.13 years would be an optimal bandwidth.


#Including covariates and treatment variable (post1923):
# Efficiency gains?
temp_data_rddapp <- data_amazon_RDD_price
temp_data_rddapp $formatNum = as.numeric(as.factor(temp_data_rddapp$format))
summary(temp_data_rddapp$formatNum)
my_fitLLR_cov = rd_est(logprice ~ year + year^2 + year^3 |plr + prize_author + canon_title + canon_author + new + age + major + ntitleformat + formatNum + picture,
                   cutpoint = 0,
                   data = temp_data_rddapp,
                   kernel = 'rectangular',
                   t.design = "geq"
)
my_fitLLRSens_cov = rd_sens_bw(my_fitLLR_cov, bws = seq(1, 13, by = 1))
my_fitLLRSens_cov
sens_plot(my_fitLLRSens_cov, level = 0.95, x = "bw", yrange = c(-0.35, 0.2))
#Optimum based on the regression discontinuity estimation with covariates.
my_fitLLR_cov$se
my_fitLLR_cov$cov
summary(my_fitLLR_cov)

# triangular kernel:
my_fitLLR_cov_2 = rd_est(logprice ~ year + year^2 + year^3 |plr + prize_author + canon_title + canon_author + new + age + major + ntitleformat + formatNum + picture,
                       cutpoint = 0,
                       data = temp_data_rddapp,
                       kernel = 'triangular',
                       t.design = "geq",
                       
)
my_fitLLRSens_cov_2 = rd_sens_bw(my_fitLLR_cov_2, bws = seq(1, 13, by = 1))
my_fitLLRSens_cov_2
sens_plot(my_fitLLRSens_cov_2, level = 0.95, x = "bw", yrange = c(-0.35, 0.2))



# As next we re-run the model 4 with the modified bandwidth:
# creation of a new data set:
data_for_reg_price_mod <- data_amazon_RDD_price %>% filter(year>=-9 & year<=8)
# encode string variables (format) to numeric:
data_for_reg_price_mod$formatNum = as.numeric(as.factor(data_for_reg_price_mod$format))
summary(data_for_reg_price_mod$formatNum)

my_model_reg_price_4_mod <- lm(logprice ~ post1923 + plr + year + year^2 + year^3 + prize_author + canon_title + canon_author + new + age + major + ntitleformat + formatNum, data_for_reg_price_mod)
summary(my_model_reg_price_4)
my_model_reg_price_4_mod_robust_clustered <- coeftest(my_model_reg_price_4_mod, vcov = vcovCL, cluster = ~year)
my_model_reg_price_4_mod_robust_clustered
my_model_reg_price_4_robust_clustered

my_model_reg_price_4_mod$df.residual
my_model_reg_price_4$df.residual
#Observation: 


## Setting up for the RDD on editions

##Gutenberg editions are mechanical, so drop these:
##drop if format=="Gutenberg"
#temp_RDD_editions <- data_amazon %>% filter(!format=="Gutenberg")
#data_for_reg_price
temp_RDD_editions <- data_for_reg_price %>% filter(!format=="Gutenberg")
#collapse (sum) sales* (mean) canon* prize_author amazonprice price picture major age plr ntitle* post year*, by(title format)

df_sales_for_sum <- temp_RDD_editions %>% select(sales,title, format)
df_vars_for_means <- temp_RDD_editions %>% select(canon_title,canon_author,prize_author,amazonprice,price,picture,major,age,plr,post1923,year,title, format,ntitle, ntitleformat, pages)

agg_df_RDD_editions_means <- aggregate(cbind(canon_title,ntitle, ntitleformat, canon_author,prize_author,amazonprice,price,picture, major, age,plr,post1923,year,pages) ~ title + format, data = df_vars_for_means, FUN = mean, na.rm=TRUE, na.action=NULL)
agg_df_RDD_editions_means
agg_df_RDD_editions_sum <- aggregate(sales ~ title + format, data = df_sales_for_sum, FUN = sum, na.rm = FALSE)
agg_df_RDD_editions_sum

#summary(df_sales_for_sum)
#summary(df_vars_for_means)
#summary(agg_df_RDD_editions_sum)
#summary(agg_df_RDD_editions_means)
#agg_df_RDD_editions_sum %>% left_join
aggregated_df <- agg_df_RDD_editions_sum %>% left_join(agg_df_RDD_editions_means)
summary(aggregated_df)


#Expand to include 0s for unavailable title-editions 
dummy_for_editions <- dummyVars("~ format", data=aggregated_df, sep = ".")
my_onehot_2 <- data.frame(predict(dummy_for_editions, newdata = aggregated_df)) %>% as_tibble()
my_onehot_2 
colnames(my_onehot_2) = c('eb','hc', 'pb')
my_onehot_2
aggregated_df_dv <- aggregated_df %>% cbind(my_onehot_2)
head(aggregated_df_dv)
df_hc_title <- aggregated_df_dv %>% group_by(title) %>% summarise(hc_title=sum(hc)) %>% ungroup()
df_pb_title <- aggregated_df_dv %>% group_by(title) %>% summarise(pb_title=sum(pb)) %>% ungroup()
df_eb_title <- aggregated_df_dv %>% group_by(title) %>% summarise(eb_title=sum(eb)) %>% ungroup()
summary(df_eb_title)
df_titles <- df_hc_title %>% left_join(df_pb_title) %>% left_join(df_eb_title)
head(df_titles)

df_titles$all_title <- df_hc_title$hc_title + df_pb_title$pb_title + df_eb_title$eb_title
head(df_titles)

df_titles$b <- 4 - df_titles$all_title
head(df_titles)
df_titles %>% rep(2)

# RDD by editions.

# Graphical analysis
# post1923 = 0 and year = 1 (unclear, why this is the case)
new_aggregated_df <- aggregated_df %>% filter(title!='THE CALL OF THE CANYON')
plot_rdd_by_editions = ggplot(new_aggregated_df, aes(x=year, y = ntitle, color = factor(post1923))) + geom_point(alpha = 0.4) + ggtitle("RDD : in-print editions per title") + ggeasy::easy_center_title() + theme_minimal() + geom_vline(xintercept = 0, color = 'black', size = 1, linetype = "dotted")
plot_rdd_by_editions
#Linear regression: one slope for non-treated (w/o copyright) and treated (with copyright).
lm_tmp <- lm(ntitle ~ year + I(year>=0), data = aggregated_df)
lm_fun <- function(x) predict(lm_tmp, data.frame(year = x)) #log price prediction

plot_rdd_by_editions + 
  stat_function(
    data = data.frame(x = c(-8, 7),y = c(-3, 5)),aes(x = x,y=y),
    fun = lm_fun,xlim = c(-8,-0.0001),
    col="red",size = 1.5) + geom_vline(xintercept = 0, color = 'black', size = 1, linetype = "dotted") +
  stat_function(
    data = data.frame(x = c(-8, 7),y = c(-3, 5)),aes(x = x,y=y),
    fun = lm_fun,xlim = c(0.0001,7),
    col="red", size = 1.5 ) + theme_minimal()


#Lm: 3rd order polynomial:
# one slope:
c_aggregated_df <- aggregated_df %>% filter(year<0)
t_aggregated_df <- aggregated_df %>% filter(year>=0)
lm_tmp_c <- lm(ntitle ~ poly(year,3), data = c_aggregated_df)
lm_tmp_t <- lm(ntitle ~ poly(year,3), data = t_aggregated_df)

lm_fun_c <- function(x) predict(lm_tmp_c, data.frame(year = x)) # ntitle prediction
lm_fun_t <- function(x) predict(lm_tmp_t, data.frame(year = x)) # ntitle prediction
plot_rdd_by_editions + 
  stat_function(
    data = data.frame(x = c(-8, -0.0001),y = c(0, 150)),aes(x = x,y=y),
    fun = lm_fun_c,xlim = c(-8,-0.0001),
    col="red",size = 1.5) + geom_vline(xintercept = 0, color = 'black', size = 1, linetype = "dotted") +
  stat_function(
    data = data.frame(x = c(0.0001,7),y = c(0, 150)),aes(x = x,y=y),
    fun = lm_fun_t,xlim = c(0.0001,7),
    col="red", size = 1.5 )  + theme_minimal()

plot_rdd_by_editions + geom_vline(xintercept = 0, color = 'black', size = 1, linetype = "dotted") + stat_smooth(aes(year, ntitle, group = post1923), method = "lm", formula = y ~ poly(x,3))
plot_rdd_by_editions
# RDD
#eststo: reg ntitle post plr prize_author canon* year* if year>=-8 & year<=7 & format=="Hardcover", vce(cluster year) 
# all titles:
new_aggregated_df <- aggregated_df %>% group_by(title) %>% select(title, ntitle,post1923,plr,year,prize_author,canon_title,canon_author, price, pages) %>% summarise_all(funs(mean))

my_model_rdd_by_editions <- lm(ntitle ~ post1923 + plr + year + year^2 + year^3 + prize_author + canon_title + canon_author, new_aggregated_df)
summary(my_model_rdd_by_editions)
my_model_rdd_by_editions_robust_clustered <- coeftest(my_model_rdd_by_editions, vcov = vcovCL, cluster = ~year)
my_model_rdd_by_editions_robust_clustered

#hardcovers:
aggregated_df_hc <- aggregated_df %>% filter(format=='Hardcover')
my_model_rdd_by_editions_hc <- lm(ntitle ~ post1923 + plr + year + year^2 + year^3 + prize_author + canon_title + canon_author, aggregated_df_hc)
my_model_rdd_by_editions_hc
my_model_rdd_by_editions_hc_robust_clustered <- coeftest(my_model_rdd_by_editions_hc, vcov = vcovCL, cluster = ~year)
my_model_rdd_by_editions_hc_robust_clustered

aggregated_df_pb <- aggregated_df %>% filter(format=='Paperback')
my_model_rdd_by_editions_pb <- lm(ntitle ~ post1923 + plr + year + year^2 + year^3 + prize_author + canon_title + canon_author, aggregated_df_pb)
my_model_rdd_by_editions_pb
my_model_rdd_by_editions_pb_robust_clustered <- coeftest(my_model_rdd_by_editions_pb, vcov = vcovCL, cluster = ~year)
my_model_rdd_by_editions_pb_robust_clustered

aggregated_df_eb <- aggregated_df %>% filter(format=='E-book')
my_model_rdd_by_editions_eb <- lm(ntitle ~ post1923 + plr + year + year^2 + year^3 + prize_author + canon_title + canon_author, aggregated_df_eb)
my_model_rdd_by_editions_eb
my_model_rdd_by_editions_eb_robust_clustered <- coeftest(my_model_rdd_by_editions_pb, vcov = vcovCL, cluster = ~year)
my_model_rdd_by_editions_eb_robust_clustered

plot_summs(my_model_rdd_by_editions_robust_clustered, my_model_rdd_by_editions_hc_robust_clustered, my_model_rdd_by_editions_pb_robust_clustered, my_model_rdd_by_editions_eb_robust_clustered)
#
export_summs(my_model_rdd_by_editions_robust_clustered, my_model_rdd_by_editions_hc_robust_clustered, my_model_rdd_by_editions_pb_robust_clustered, my_model_rdd_by_editions_eb_robust_clustered, scale = TRUE, to.file="docx", file.name = "E:/nosave/aE/table_2.docx", model.names = c('#Titles (All)', '#Titles (Hardcover)','#Titles (Paperback)','#Titles (E-Book)' ))

#Figure 2: Panel A-D
# we use data from edition_details.dta to illustrate no discontinuity at 1923:
plot_plr_year <- ggplot(new_aggregated_df, aes(x=year, y = plr, color = factor(post1923))) + geom_point(alpha = 0.4)+ggtitle("Average annual British library checkouts per title") + ggeasy::easy_center_title() + geom_vline(xintercept = 0, color = 'black', size = 1, linetype = "dotted")+ theme_minimal()
plot_plr_year

hc_editions <- data_edition_det %>% filter(format == 'Hardcover')

#** look only at editions within 74 years of original publication:
#  gen age = pubyear - year
#keep if age<75
hc_editions$age <- hc_editions$pubyear - hc_editions$year

data_edition_det$age <- data_edition_det$pubyear - data_edition_det$year
data_edition_75y <- data_edition_det %>% filter(age<75)
#%>%  group_by(title, year) %>% summarize(n = n())
  
df_hc_editions <- hc_editions %>% filter(age<75) %>%  group_by(title, year) %>% summarize(n = n())

plot_hc_year <- ggplot(df_hc_editions, aes(x=year, y = n)) + geom_point(alpha = 0.4)+ggtitle("Average annual British library checkouts per title") + ggeasy::easy_center_title() + geom_vline(xintercept = 1923, color = 'black', size = 1, linetype = "dotted")+ theme_minimal()
plot_hc_year

#** b) prices of editions
#egen price75_py = mean(bowkerprice), by(year)
#** c) number of pages
#egen pagelength75_py = mean(pages), by(year)

df_price_bowker_75y <- data_edition_75y %>% group_by(year) %>% summarize(mean_price = mean(bowkerprice))
df_pages_75y <- data_edition_75y %>% filter(!is.na(pages)) %>% group_by(year) %>%  summarize(mean_pages = mean(pages))

plot_price_year <- ggplot(df_price_bowker_75y, aes(x=year, y = mean_price)) + geom_point(alpha = 0.4)+ggtitle("Average price per edition") + ggeasy::easy_center_title() + geom_vline(xintercept = 1923, color = 'black', size = 1, linetype = "dotted")+ theme_minimal()
plot_price_year

plot_pages_year <- ggplot(df_pages_75y, aes(x=year, y = mean_pages )) + geom_point(alpha = 0.4)+ggtitle("Average price per edition") + ggeasy::easy_center_title() + geom_vline(xintercept = 1923, color = 'black', size = 1, linetype = "dotted")+ theme_minimal()
plot_pages_year

plot_plr_year + plot_hc_year+plot_price_year + plot_pages_year + plot_layout(ncol =2)

### Bandwidth of 10 years, model 1 and 3:
data_for_reg_price_bw10 <- data_amazon_RDD_price %>% filter(year>=-10 & year<=9)
# encode string variables (format) to numeric:
data_for_reg_price_bw10$formatNum = as.numeric(as.factor(data_for_reg_price_bw10$format))
summary(data_for_reg_price_bw10$formatNum)
summary(data_for_reg_price_bw10)

# linear regression:
# 1 intercept + 10 convariates = 11

my_model_reg_price_bw10 <- lm(logprice ~ post1923 + plr + year + year^2 + year^3 + prize_author + canon_title + canon_author + new + age + major + formatNum, data_for_reg_price_bw10)
summary(my_model_reg_price_bw10)
my_model_reg_price_robust_clustered_bw10 <- coeftest(my_model_reg_price_bw10, vcov = vcovCL, cluster = ~year)
my_model_reg_price_robust_clustered_bw10

data_for_reg_price_3_bw10 <- data_amazon_RDD_price %>% filter(year>=-10 & year<=9 & format=="Paperback")
# RDD (only paperbacks)
my_model_reg_price_3_bw10 <- lm(logprice ~ post1923 + plr + year + year^2 + year^3 + prize_author + canon_title + canon_author + new + age + major, data_for_reg_price_3_bw10)
summary(my_model_reg_price_3_bw10)
my_model_reg_price_3_robust_clustered_bw10 <- coeftest(my_model_reg_price_3_bw10, vcov = vcovCL, cluster = ~year)
my_model_reg_price_3_robust_clustered_bw10

export_summs(my_model_reg_price_robust_clustered_bw10, my_model_reg_price_3_robust_clustered_bw10, scale = TRUE, to.file="docx", file.name = "E:/nosave/aE/table_2_updated.docx", model.names = c('Model 1 - updated bandwidth', 'Model 3 - updated bandwidth' ))

#end

