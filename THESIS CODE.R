library(readxl)
Dataset_Thesis_Raw <- read_excel("C:/Users/Utente/Downloads/Dataset Thesis Raw.xlsx")
View(Dataset_Thesis_Raw)


library(ggplot2)

ggplot(Dataset_Thesis_Raw, aes(y=age_beginning))+
  geom_boxplot() +
  labs(title="Histogram for Age at the beginning of the investment", y="Age of the investors")

# Counting the people with respect to their field of study

sum(Dataset_Thesis_Raw$field_of_study=="ECONOMICS AND FINANCE")

sum(Dataset_Thesis_Raw$field_of_study=="ENGINEERING")
 
sum(Dataset_Thesis_Raw$field_of_study=="LANGUAGES AND TOURISM")

sum(Dataset_Thesis_Raw$field_of_study=="MEDICAL SCIENCES")

sum(Dataset_Thesis_Raw$field_of_study=="MODERN LITERATURE")

sum(Dataset_Thesis_Raw$field_of_study=="UNKNOWN")

sum(Dataset_Thesis_Raw$field_of_study=="SCIENTIFIC HIGH SCHOOL")



# Statistics of the experience estimators

max(Dataset_Thesis_Raw$n_roles_beginning)

mean(Dataset_Thesis_Raw$n_failed_inv)

median(Dataset_Thesis_Raw$n_failed_inv)

max(Dataset_Thesis_Raw$n_failed_inv)

sum(Dataset_Thesis_Raw$n_failed_inv==0)

sd(Dataset_Thesis_Raw$value_variation)

max(Dataset_Thesis_Raw$value_variation)

# Values in percentage

variation<-Dataset_Thesis_Raw$value_variation*100
sd(variation)

max(Dataset_Thesis_Raw$value_variation)*100

mean(Dataset_Thesis_Raw$value_variation)*100

median(Dataset_Thesis_Raw$value_variation)*100

# Summary of all variables of the dataset
summary(Dataset_Thesis_Raw)

# Standard deviations for the table 1
VariationShare<-Dataset_Thesis_Raw$difference_share
sd(VariationShare)

logvar<-Dataset_Thesis_Raw$log_variation
sd(logvar)

rolesbeg<-Dataset_Thesis_Raw$n_roles_beginning
sd(rolesbeg)

failed<-Dataset_Thesis_Raw$n_failed_inv
sd(failed)


agetoday<-Dataset_Thesis_Raw$age_today
sd(agetoday)

agebeg<-Dataset_Thesis_Raw$age_beginning
sd(agebeg)


durat<-Dataset_Thesis_Raw$duration
sd(durat)


library(ggplot2)

# Histogram for Years of beginning of investment

ggplot(data=Dataset_Thesis_Raw, aes(x=Dataset_Thesis_Raw$foundation_year)) + 
  geom_histogram(breaks=seq(2000, 2019, by=1),
                 col="red", 
                 fill="green", 
                 alpha = .2) + 
  labs(x="Years of constitution", y="Count") + 
  ylim(c(0,13))


sum(Dataset_Thesis_Raw$foundation_year==2017)
sum(Dataset_Thesis_Raw$foundation_year==2018)
sum(Dataset_Thesis_Raw$foundation_year==2016)
sum(Dataset_Thesis_Raw$foundation_year>=2011)/91 #share of investments that begun in 2011



# Distribution of the log growth rates

ggplot(data=Dataset_Thesis_Raw, aes(log_variation)) + 
  geom_histogram(aes(y =..density..), 
                 col="red", 
                 fill="green", 
                 alpha=.2) + 
  geom_density(col=2) + 
  labs(x="log-variation")

mean(Dataset_Thesis_Raw$log_variation)
sd(Dataset_Thesis_Raw$log_variation)
median(Dataset_Thesis_Raw$log_variation)

# Scatterplot age investors vs growth rate

ggplot(Dataset_Thesis_Raw, aes(x=age_beginning, y=log_variation)) + 
  geom_point(aes(col=gender))+
  geom_smooth(method = "lm", se = TRUE)+
  labs(x="Age at the beginning of the investement", y="log-variation")
# Linear regression

Age<-as.matrix(Dataset_Thesis_Raw$age_beginning)
logvariation <- as.matrix(Dataset_Thesis_Raw$log_variation)
reg1 <-lm(logvariation ~ Age)
summary(reg1) 


# Other types of scatterplots:

# Scatterplot experience vs growth rate

ggplot(Dataset_Thesis_Raw, aes(x=n_roles_beginning, y=log_variation)) + 
  geom_point(aes(col=gender))+
  geom_smooth(method = "lm", se = TRUE)



#Scatterplot (at least one failure) vs growth rate 

ggplot(Dataset_Thesis_Raw[Dataset_Thesis_Raw$n_failed_inv>=1,], aes(x=n_failed_inv, y=log_variation)) + 
  geom_point(aes(col=gender))+
  geom_smooth(method = "lm", se = TRUE) +
labs(x="Number of failed investments", y="log-variation")



#Boxplot for the log-variation

ggplot(data=Dataset_Thesis_Raw, aes(y=log_variation)) + 
  geom_boxplot() + 
  stat_boxplot(geom ='errorbar') +
  labs(y="Log - variation")+
  xlim(c(-0.5,0.5))

# Men vs Women

ggplot(data=Dataset_Thesis_Raw, aes(x=gender, y=log_variation)) + 
  geom_boxplot() + 
  stat_boxplot(geom ='errorbar') +
  labs(x="Gender" ,y="Log - variation")

mean(Dataset_Thesis_Raw$log_variation[Dataset_Thesis_Raw$gender=="M"])
mean(Dataset_Thesis_Raw$log_variation[Dataset_Thesis_Raw$gender=="F"])

sd(Dataset_Thesis_Raw$log_variation[Dataset_Thesis_Raw$gender=="M"])
sd(Dataset_Thesis_Raw$log_variation[Dataset_Thesis_Raw$gender=="F"])

max(Dataset_Thesis_Raw$log_variation[Dataset_Thesis_Raw$gender=="M"])

min(Dataset_Thesis_Raw$log_variation[Dataset_Thesis_Raw$gender=="M"])


# ECONOMICS vs others


mean(Dataset_Thesis_Raw$log_variation[Dataset_Thesis_Raw$field_of_study=="ECONOMICS AND FINANCE"])
mean(Dataset_Thesis_Raw$log_variation[Dataset_Thesis_Raw$field_of_study!="ECONOMICS AND FINANCE"])


  
ggplot(data=Dataset_Thesis_Raw, aes(field_of_study=="ECONOMICS AND FINANCE", y=log_variation)) + 
  geom_boxplot() + 
  stat_boxplot(geom ='errorbar') +
  labs(x="Economics and Finance" ,y="Log - variation")


# ECONOMICS vs ENGINEERING

# Vectors
Eco<-Dataset_Thesis_Raw$field_of_study=="ECONOMICS AND FINANCE"  

Eng<-Dataset_Thesis_Raw$field_of_study=="ENGINEERING"

# Dataset containing only people with Eco & Fin or Engineering study background
EcovsEng<-Dataset_Thesis_Raw[Eco | Eng,]

ggplot(data=EcovsEng, aes(x=field_of_study=="ECONOMICS AND FINANCE", y=log_variation)) + 
  geom_boxplot() + 
  stat_boxplot(geom ='errorbar') +
  labs(x="Economics and Finance", y="Log - variation")

mean(Dataset_Thesis_Raw$log_variation[Dataset_Thesis_Raw$field_of_study=="ECONOMICS AND FINANCE"])
mean(Dataset_Thesis_Raw$log_variation[Dataset_Thesis_Raw$field_of_study!="ENGINEERING"])

min(EcovsEng$log_variation[EcovsEng$field_of_study=="ENGINEERING"])
max(EcovsEng$log_variation[EcovsEng$field_of_study=="ENGINEERING"])
sd(EcovsEng$log_variation[EcovsEng$field_of_study=="ENGINEERING"])

#	Who experienced the best variation in the involvement of shareholding of an enterprise? 

ggplot(data=Dataset_Thesis_Raw, aes(x=age_beginning>=43, y=log_variation)) + 
  geom_boxplot() + 
  stat_boxplot(geom ='errorbar') +
  labs(x="People less than 43 years old",y="Log - variation")


diffshare_pos<-Dataset_Thesis_Raw[Dataset_Thesis_Raw$difference_share>0,]

ggplot(data=diffshare_pos, aes(x=age_beginning>43, y=difference_share)) + 
  geom_boxplot() + 
  stat_boxplot(geom ='errorbar') +
  labs(x="People more than 43 years old",y="Share variation in percentage")



mean(diffshare_pos$difference_share[diffshare_pos$age_beginning>43])
mean(diffshare_pos$difference_share[diffshare_pos$age_beginning<=43])

diffshare_neg<-Dataset_Thesis_Raw[Dataset_Thesis_Raw$difference_share<0,]

ggplot(data=diffshare_neg, aes(x=age_beginning>43, y=difference_share)) + 
  geom_boxplot() + 
  stat_boxplot(geom ='errorbar') +
  labs(x="People more than 43 years old",y="Share variation in percentage")

mean(diffshare_neg$difference_share[diffshare_neg$age_beginning>43])
mean(diffshare_neg$difference_share[diffshare_neg$age_beginning<=43])



# Do people who have experienced more than 8 (mean) investments have a higher return than those with less experience?

ggplot(data=Dataset_Thesis_Raw, aes(x=n_roles_beginning>=8, y=log_variation)) + 
  geom_boxplot() + 
  stat_boxplot(geom ='errorbar') +
  labs(x="People with more than 8 roles", y="Log - variation")

mean(Dataset_Thesis_Raw$log_variation[Dataset_Thesis_Raw$n_roles_beginning>=8])
min(Dataset_Thesis_Raw$log_variation[Dataset_Thesis_Raw$n_roles_beginning>=8])
max(Dataset_Thesis_Raw$log_variation[Dataset_Thesis_Raw$n_roles_beginning>=8])
sd(Dataset_Thesis_Raw$log_variation[Dataset_Thesis_Raw$n_roles_beginning>=8])


mean(Dataset_Thesis_Raw$log_variation[Dataset_Thesis_Raw$n_roles_beginning<8])
min(Dataset_Thesis_Raw$log_variation[Dataset_Thesis_Raw$n_roles_beginning<8])
max(Dataset_Thesis_Raw$log_variation[Dataset_Thesis_Raw$n_roles_beginning<8])
sd(Dataset_Thesis_Raw$log_variation[Dataset_Thesis_Raw$n_roles_beginning<8])

Dataset_Thesis_Raw$log_variation[Dataset_Thesis_Raw$n_roles_beginning>=8]

#  Do people who have at least one failure  investments have a higher return than those with less experience?
ggplot(data=Dataset_Thesis_Raw, aes(x=n_failed_inv>=1, y=log_variation)) + 
  geom_boxplot() + 
  stat_boxplot(geom ='errorbar') +
  labs(x="People with at least one failed investment",y="Log - variation")

mean(Dataset_Thesis_Raw$log_variation[Dataset_Thesis_Raw$n_failed_inv>=1])
min(Dataset_Thesis_Raw$log_variation[Dataset_Thesis_Raw$n_failed_inv>=1])
max(Dataset_Thesis_Raw$log_variation[Dataset_Thesis_Raw$n_failed_inv>=1])
sd(Dataset_Thesis_Raw$log_variation[Dataset_Thesis_Raw$n_failed_inv>=1])


mean(Dataset_Thesis_Raw$log_variation[Dataset_Thesis_Raw$n_failed_inv<1])
min(Dataset_Thesis_Raw$log_variation[Dataset_Thesis_Raw$n_failed_inv<1])
max(Dataset_Thesis_Raw$log_variation[Dataset_Thesis_Raw$n_failed_inv<1])
sd(Dataset_Thesis_Raw$log_variation[Dataset_Thesis_Raw$n_failed_inv<1])


# Did people with multiple investments improved the quality of investments (growth of the company) over the time?

Multiple_investors<-Dataset_Thesis_Raw[c(4:24,26:50,53:64,66:69,76:82,87,88,90,91),]

ggplot(Multiple_investors, aes(x=year_beg, y=log_variation, color=gender)) +
  geom_point() + 
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
  labs(x="Initial year",y="Log - variation")


# Is there a correlation between the rate of involvement and the performance of the enterprise?

share5<-Dataset_Thesis_Raw[Dataset_Thesis_Raw$share_beginning>8.36,]

ggplot(share5, aes(x=share_beginning, y=log_variation)) +
  geom_point() + 
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
labs(x="Share owned at the beginning",y="Log - variation")




share<-as.matrix(share5$share_beginning)
logvariation <- as.matrix(share5$log_variation)
reg2 <-lm(logvariation ~ share)
summary(reg2)

# Did the companies founded before 2010 grew more than the others

median(Dataset_Thesis_Raw$foundation_year)

ggplot(data=Dataset_Thesis_Raw, aes(x=foundation_year<=2010, y=log_variation)) + 
  geom_boxplot() + 
  stat_boxplot(geom ='errorbar') +
  labs(x="Companies founded in 2010 or before",y="Log - variation")

mean(Dataset_Thesis_Raw$log_variation[Dataset_Thesis_Raw$foundation_year<=2010])
mean(Dataset_Thesis_Raw$log_variation[Dataset_Thesis_Raw$foundation_year>2010])

min(Dataset_Thesis_Raw$log_variation[Dataset_Thesis_Raw$foundation_year<=2010])
max(Dataset_Thesis_Raw$log_variation[Dataset_Thesis_Raw$foundation_year<=2010])
min(Dataset_Thesis_Raw$log_variation[Dataset_Thesis_Raw$foundation_year>2010])
max(Dataset_Thesis_Raw$log_variation[Dataset_Thesis_Raw$foundation_year>2010])



# generate a table (for the regression) in latex programming language

install.packages("stargazer")
library(stargazer)
stargazer(reg1,title="Regression Results")

