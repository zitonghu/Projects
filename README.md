# zitong-projects

---
  title: 'Lender Classification with Logistic Regression'
author: "Zitong Hu"
output:
  pdf_document:
  toc: yes
word_document:
  toc: yes
html_document:
  theme: journal
highlight: tango
number_sections: yes
toc: yes
toc_float: no
---
  
  
  ```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```


```{r, load_libraries, include = FALSE}
#Load required libraries
library(tidyverse) 
library(lubridate)
library(GGally) 
library(ggfortify) 
library(rsample) 
library(janitor)
library(broom) 
library(huxtable) 
library(caret) 
library(nnet) 
library(pROC) 
library(MLmetrics)
```


## Load the data

```{r, load_data, warning=FALSE, message=FALSE}

lc_raw <- read_csv("LendingClub Data.csv",  skip=1) %>%  #since the first row is a title we want to skip it. 
  clean_names() # use janitor::clean_names()
```

```{r, Inspect}
#have an overview of the data
glimpse(lc_raw)
```

## Clean the data

```{r, clean data}
lc_clean<- lc_raw %>%
  dplyr::select(-x20:-x80) %>% #delete empty columns
  filter(!is.na(int_rate)) %>%   #delete empty rows
  mutate(
    issue_d = mdy(issue_d),  #  fix date format
    term = factor(term_months),     # turn 'term' into a categorical variable
    delinq_2yrs = factor(delinq_2yrs) # turn 'delinq_2yrs' into a categorical variable
  ) %>% 
  mutate(default = dplyr::recode(loan_status, 
                                 "Charged Off" = "1", 
                                 "Fully Paid" = "0"))%>%
  mutate(default = as.factor(default)) %>%
  dplyr::select(-emp_title,-installment, -term_months, everything()) #move some not-so-important variables to the end. 

```

## Explore the data

```{r, visualization of defaults, warning=FALSE}
#bar chart of defaults
def_vis1<-ggplot(data=lc_clean, aes(x=default)) +geom_bar(aes(y = (..count..)/sum(..count..))) + labs(x="Default, 1=Yes, 0=No", y="relative frequencies") +scale_y_continuous(labels=scales::percent) +geom_text(aes( label = scales::percent((..count..)/sum(..count..) ),y=(..count..)/sum(..count..) ), stat= "count",vjust=-0.5) 
def_vis1

#bar chart of defaults per loan grade
def_vis2<-ggplot(data=lc_clean, aes(x=default), group=grade) +geom_bar(aes(y = (..count..)/sum(..count..), fill = factor(..x..)), stat="count")  + labs(title="Defaults by Grade", x="Default, 1=Yes, 0=No", y="relative frequencies") +scale_y_continuous(labels=scales::percent) +facet_grid(~grade) + theme(legend.position = "none") +geom_text(aes( label = scales::percent((..count..)/sum(..count..) ),y=(..count..)/sum(..count..) ), stat= "count",vjust=-0.5) 
def_vis2

#bar chart of defaults per number of Delinquencies
def_vis3<-lc_clean %>%
  filter(as.numeric(delinq_2yrs)<4) %>%
  ggplot(aes(x=default), group=delinq_2yrs) +geom_bar(aes(y = (..count..)/sum(..count..), fill = factor(..x..)), stat="count")  + labs(title="Defaults by Number of Delinquencies", x="Default, 1=Yes, 0=No", y="relative frequencies")  +scale_y_continuous(labels=scales::percent) +facet_grid(~delinq_2yrs) + theme(legend.position = "none") +geom_text(aes( label = scales::percent((..count..)/sum(..count..) ),y=(..count..)/sum(..count..) ), stat= "count",vjust=-0.5)

def_vis3

#scatter plots 

#select 2000 random loans to display only to make the display less busy. 
set.seed(1234)
reduced<-lc_clean[sample(0:nrow(lc_clean), 2000, replace = FALSE),]%>%
  mutate(default=as.numeric(default)-1) # also convert default to a numeric {0,1} to make it easier to plot.


# scatter plot of defaults against loan amount                         
def_vis4<-ggplot(data=reduced, aes(y=default,x=I(loan_amnt/1000)))  + labs(y="Default, 1=Yes, 0=No", x="Loan Amnt (1000 $)") +geom_jitter(width=0, height=0.05, alpha=0.7) #We use jitter to offset the display of defaults/non-defaults to make the data easier to interpert. We have also changed the amount to 1000$ to reduce the number of zeros on the horizontal axis.

def_vis4

#scatter plot of defaults against loan amount.
def_vis5<-ggplot(data=reduced, aes(y=default,x=I(annual_inc/1000)))   + labs(y="Default, 1=Yes, 0=No", x="Annual Income(1000 $)") +geom_jitter(width=0, height=0.05, alpha=0.7) +  xlim(0,400)

def_vis5

```

Estimate a correlation table between defaults and other continuous variables.

```{r, correlation table, warning=FALSE, message=FALSE}

lc_clean %>% 
  mutate(default=as.numeric(default)-1)%>%
  select(loan_amnt, dti, annual_inc, default) %>% #keep Y variable last
  ggcorr(method = c("pairwise", "pearson"), label_round=2, label = TRUE)

```

```{r}
# more visualizations of the variables

def_vis6<-ggplot(data=lc_clean, aes(x=default), group=home_ownership) +geom_bar(aes(y = (..count..)/sum(..count..), fill = factor(..x..)), stat="count")  + labs(title="Defaults by home_ownerahip", x="Default, 1=Yes, 0=No", y="relative frequencies") +scale_y_continuous(labels=scales::percent) +facet_grid(~home_ownership) + theme(legend.position = "none") +geom_text(aes( label = scales::percent((..count..)/sum(..count..) ),y=(..count..)/sum(..count..) ), stat= "count",vjust=-0.5) 
def_vis6



def_vis7<-ggplot(data=lc_clean, aes(x=default), group=purpose) +geom_bar(aes(y = (..count..)/sum(..count..), fill = factor(..x..)), stat="count")  + labs(title="Defaults by loan purpose", x="Default, 1=Yes, 0=No", y="relative frequencies") +scale_y_continuous(labels=scales::percent) +facet_grid(~purpose) + theme(legend.position = "none") +geom_text(aes( label = scales::percent((..count..)/sum(..count..) ),y=(..count..)/sum(..count..) ), stat= "count",vjust=-0.5) 
def_vis7
unique(lc_clean$purpose)
```

Default type 0 has the highest relative frequency in lenders with mortgage home and 
rent home. Default type 1 also has the highest relative frequency in these two categories

Default type 0 has the highest relative frequency in lenders with the purpose of consolidating
debts, the second and third highest relative frequency are found with the purpose of credit card
and other. Default type 1 also has the highest relative frequency in lenders with the purpose
of consolidating
debts


# Linear vs. logistic regression for binary response variables

```{r, linear and logisitc regression with binary response variable, warning=FALSE}

model_lm<-lm(as.numeric(default)~I(annual_inc/1000), lc_clean)
summary(model_lm)


logistic1<-glm(default~I(annual_inc/1000), family="binomial", lc_clean)
summary(logistic1)


ggplot(data=reduced, aes(x=I(annual_inc/1000), y=default)) + geom_smooth(method="lm", se=0, aes(color="OLS"))+ geom_smooth(method = "glm", method.args = list(family = "binomial"),  se=0, aes(color="Logistic"))+ labs(y="Prob of Default", x="Annual Income(1000 $)")+  xlim(0,450)+scale_y_continuous(labels=scales::percent)+geom_jitter(width=0, height=0.05, alpha=0.7) + scale_colour_manual(name="Fitted Model", values=c("blue", "red"))


```

The logistic regression is more suitable for predicting probability as the predicted probability of 
linear regression drops below 0% when the annual income exceed 350,000 dollars which makes it unsuitable 
for predicting lenders with higher income. The logistic regression does not have such concern which makes
it more suitable for prediction. 

# Multivariate logistic regression
annual_inc, term, grade, and loan amount are used as features

```{r, multivariate logistic regression, warning=F}
logistic2<- glm(default~annual_inc + term + grade + loan_amnt, family="binomial", lc_clean)
summary(logistic2)

#compare the fit of logistic 1 and logistic 2
anova(logistic1,logistic2)

```

```{r, error = TRUE}
#Predict the probability of default
prob_default2<-predict(logistic2,lc_clean,type="response")
#plot 1: Density of predictions
g2<-ggplot( lc_clean, aes( prob_default2 ) )+
  geom_density( size=1)+
  ggtitle( "Predicted Probability with Logistic 2" )+  xlab("Estimated Probability")
g2
#plot 2: Density of predictions by default
g3<-ggplot( lc_clean, aes( prob_default2, color=default) ) +
  geom_density( size=1)+
  ggtitle( "Predicted Probability with Logistic 2" )+
  xlab("Estimated Probability")

g3
```

## From probability to classification

Predicted probability threshold: 19.5%

```{r, From probability to classification, error = TRUE}
#using the logistic 2 model predict default probabilities
prob_default2<- predict(logistic2,lc_clean,type="response")

one_or_zero<-ifelse(prob_default2>0.195,"1","0")
p_class<- factor(one_or_zero,levels=levels(lc_clean$default))

#produce the confusion matrix and set default as the positive outcome
con2<-confusionMatrix(p_class,lc_clean$default,positive="1")

#print the confusion matrix
con2

```

## Produce ROC curve and calculate AUC with logistic 2 model

```{r, ROC curves, warning=FALSE, error = TRUE}
#estimate the ROC curve for Logistic 2
ROC_logistic2 <-roc(lc_clean$default,prob_default2)

#estimate the AUC for Logistic 2 and round it to two decimal places
AUC2<-  round(auc(lc_clean$default,prob_default2)*100, digits=2)

#Plot the ROC curve and display the AUC in the title
ROC2<-ggroc(ROC_logistic2,  alpha = 0.5)+ ggtitle(paste("Model Logistic 2: AUC=",AUC2,"%"))  +
  geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1), color="grey", linetype="dashed")+geom_segment(aes(x = 1, xend = 1, y = 0, yend = 1), color="black", linetype="dashed")+geom_segment(aes(x = 1, xend = 0, y = 1, yend = 1), color="black", linetype="dashed")
ROC2
```

##Split data into testing and training

```{r, out-of-sample ROC curve}
# splitting the data into training and testing
set.seed(1234)
train_test_split <- initial_split(lc_clean, prop = 0.8)
testing <- testing(train_test_split) #20% of the data is set aside for testing
training <- training(train_test_split) #80% of the data is set aside for training

# run logistic 2 on the training set 
logistic2<-glm(default~annual_inc + term + grade + loan_amnt, family="binomial", training)

#calculate probability of default in the training sample 
p_in<-predict(logistic2, training, type = "response")
#ROC curve using in-sample predictions
ROC_logistic2_in <- roc(training$default,p_in)

#AUC using in-sample predictions
AUC_logistic2_in<-round(auc(training$default,p_in)*100, digits=2)

#plot ROC curve
ROC3<-ggroc(ROC_logistic2_in,  alpha = 0.5)+ ggtitle(paste("Model Logistic 2 in sample: AUC=",AUC_logistic2_in,"%"))  + geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1), color="grey", linetype="dashed")+geom_segment(aes(x = 1, xend = 1, y = 0, yend = 1), color="black", linetype="dashed")+geom_segment(aes(x = 1, xend = 0, y = 1, yend = 1), color="black", linetype="dashed")
ROC3
#calculate probability of default out of sample 
p_out<- predict(logistic2, testing, type = "response")

#ROC curve using out-of-sample predictions
ROC_logistic2_out <- roc(testing$default,p_out)
#AUC using out-of-sample predictions
AUC_logistic2_out <- round(auc(testing$default,p_out)*100, digits=2)

#plot in the same figure both ROC curves and print the AUC of both curves in the title
ROC4<-ggroc(list("Logistic 2 in-sample"=ROC_logistic2_in, "Logistic 2 out-of-sample"=ROC_logistic2_out),  alpha = 0.5)+ 
  ggtitle(paste("Model Logistic 2 in sample: AUC=",AUC_logistic2_in, 
                "%\nModel Logistic 2 out of sample: AUC=",AUC_logistic2_out,"%"))  + 
  geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1), color="grey", linetype="dashed")+
  geom_segment(aes(x = 1, xend = 1, y = 0, yend = 1), color="black", linetype="dashed")+
  geom_segment(aes(x = 1, xend = 0, y = 1, yend = 1), color="black", linetype="dashed")

ROC4

```

There is over-fitting for the model as the AUC for the in-sample model is higher than the out-of-sample model, indicating that the classification does not perform as well for out-of-sample data. 

## Selecting loans to invest in using the model Logistic 2.

```{r}
# splitting the data into training and testing
set.seed(1234)
train_test_split <- initial_split(lc_clean, prop = 0.6)
training <- training(train_test_split) #60% of the data is set aside for training
remaining <- testing(train_test_split) #40% of the data is set aside for validation & testing
set.seed(4321)
train_test_split <- initial_split(remaining, prop = 0.5)
validation<-training(train_test_split) #50% of the remaining data (20% of total data) will be used for validation
testing<-testing(train_test_split) #50% of the remaining data (20% of total data) will be used for testing
```


>Q9. Train logistic 2 on the training set above. Use the trained model to determine the optimal cut-off threshold based on the validation test. 

Insert your code here:
```{r}

logistic2<-glm(default~annual_inc + term + grade + loan_amnt, family="binomial", training)
p_val<-predict(logistic2, validation, type = "response") 

profit=0
threshold=0
for(i in 1:100) {
  threshold[i]=i/400
  one_or_zero_search<-ifelse(p_val>threshold[i],"1","0")
  p_class_search<-factor(one_or_zero_search,levels=levels(validation$default))
  
  con_search<-confusionMatrix(p_class_search,validation$default,positive="1")
  profit[i]=con_search$table[1,1]*18-con_search$table[1,2]*85
}

ggplot(as.data.frame(threshold), aes(x=threshold,y=profit)) + geom_smooth(method = 'loess', se=0) +labs(title="Profit curve with logistic 2 based on validation set")

paste0("Based on the validation set: Maximum profit per loan is $", round(max(profit)/nrow(validation),2), " achieved at a threshold of ", threshold[which.is.max(profit)]*100,"%.")

#optimal threshold based on the validation set
threshold=threshold[which.is.max(profit)]

#Use the model estimated on the training set to predict probabilities of default on the testing set
p_test<-predict(logistic2, testing, type = "response")

#use the threshold estimated using the validation set to estimate the profits on the testing set
one_or_zero<-ifelse(p_test>threshold,"1","0")
p_class<-factor(one_or_zero,levels=levels(testing$default))
con<-confusionMatrix(p_class,testing$default,positive="1")

profit=con$table[1,1]*18-con$table[1,2]*85
paste0("Based on the testing set the actual profit per loan is: $", round(profit/nrow(testing),2))

```


The optimal cutoff threshold is 17.75% and it generate $5.9 as the maximum profit per loan. The actual profit per loan associated with the cutoff is $5.2


# More realistic revenue model
Each loan has different terms (e.g., different interest rate and different duration) and therefore a different return if fully paid. For example, a 36 month loan of \$5000 with installment of \$163 per month would generate a return of `163*36/5000-1` if there was no default. The lending would generate a loss of -65% if there was a default (the loss is not 100% because the loan may not default immediately and/or the lending club may be able to recover part of the loan). 

## Calculate the percentage return of \$1 investment in each loan in the validation set

```{r}
summary(validation$default)
default_ppl<- validation %>% filter(default == 1) 
good_ppl<- validation %>% filter(default == 0) 

default_loss<- nrow(default_ppl)*0.65

invest_profit<- sum(good_ppl$term_months * good_ppl$installment/good_ppl$loan_amnt-1)

profit<- (invest_profit- default_loss)/nrow(validation)

profit


```
The return is 10.92%



## Expected return in logistic 2 model under the assumption:

expected return = return if not default * (1-prob(default)) + return if default * prob(default). 


```{r}

summary(validation$default)
default_ppl<- validation %>% filter(default == 1) 
good_ppl<- validation %>% filter(default == 0) 

default_loss<- nrow(default_ppl)*0.65

invest_profit<- sum(good_ppl$term_months * good_ppl$installment/good_ppl$loan_amnt-1)

profit<- (invest_profit- default_loss)/nrow(validation)

profit
logistic2$table



validation$pred<- p_test
#expected return = return if not default * (1-prob(default)) + return if default * prob(default). 

exp_return<- (validation$term_months * validation$installment/validation$loan_amnt-1) * (1-validation$pred) -
  0.65 *validation$pred

total_exp_return<- sum(exp_return)
#The total expected return is 879
validation$exp_return<- exp_return

top_750<- validation %>% arrange(desc(exp_return)) %>% slice_max(exp_return, n = 750)

sum(top_750$exp_return)
```
The total expected return is 879. As I change $n$, the realized return will increase as it would consider the 
most promising loan as non-default. the profit for $n=750$ is 304.4


##Prediction experimentation with different models and features

```{r}

#logistic2<- glm(default~annual_inc + term + grade + loan_amnt, family="binomial", lc_clean)
#summary(logistic2)


logistic3<- glm(default~term + grade+ annual_inc * int_rate + term * verification_status, family="binomial", lc_clean)
summary(logistic3)


prob_default3<- predict(logistic3,lc_clean,type="response")

#Call any loan with probability more than 19.5% as default and any loan with lower probability as non-default. 
one_or_zero<-ifelse(prob_default3>0.195,"1","0")
p_class<- factor(one_or_zero,levels=levels(lc_clean$default))

#produce the confusion matrix and set default as the positive outcome
con3<-confusionMatrix(p_class,lc_clean$default,positive="1")

#print the confusion matrix
con3

#estimate the ROC curve for Logistic 2
ROC_logistic3 <-roc(lc_clean$default,prob_default3)

#estimate the AUC for Logistic 2 and round it to two decimal places
AUC3<-  round(auc(lc_clean$default,prob_default3)*100, digits=2)

#Plot the ROC curve and display the AUC in the title
ROC3<-ggroc(ROC_logistic3,  alpha = 0.5)+ ggtitle(paste("Model Logistic 3: AUC=",AUC3,"%"))  +
  geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1), color="grey", linetype="dashed")+geom_segment(aes(x = 1, xend = 1, y = 0, yend = 1), color="black", linetype="dashed")+geom_segment(aes(x = 1, xend = 0, y = 1, yend = 1), color="black", linetype="dashed")

ROC3

```

The new model removes the statistically insignificant term loan amount and adds two interation term between annual income & interest rate, and term & verification status. The model reached an improvement compare to logistic 2 as the AIC reduces from 29306 to 29243 and the AUC increases from 68.01% to 68.23%.



## Use Logistic 3 to predict 180 best loans to invest in 

```{r}
lc_assessment<- read_csv("./Assessment Data_2022.csv") %>%  #load the data 
  clean_names() %>% # use janitor::clean_names() 
  mutate(
    issue_d = mdy(issue_d),  # lubridate::mdy() to fix date format
    term = factor(term_months),     # turn 'term' into a categorical variable
    delinq_2yrs = factor(delinq_2yrs)) # turn 'delinq_2yrs' into a categorical variable


p_test<-predict(logistic3, lc_assessment, type = "response")
lc_assessment$pred<-p_test


#expected return = return if not default * (1-prob(default)) + return if default * prob(default). 

exp_return<- (lc_assessment$term_months * lc_assessment$installment/lc_assessment$loan_amnt-1) * (1-lc_assessment$pred) - 0.65 *lc_assessment$pred


#The total expected return is 879
lc_assessment$exp_return<- exp_return

top_180<- lc_assessment %>% arrange(desc(exp_return)) %>% slice_max(exp_return, n =180)


#write.csv(top_180, "Zitong_Hu_DS")



```

