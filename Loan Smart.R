ld=read.csv(file="loans data.csv",stringsAsFactors = FALSE)

install.packages("dplyr")

library(dplyr)

glimpse(ld)

ld=ld %>% select(-Amount.Funded.By.Investors)

ld=ld %>% mutate(Amount.Requested=as.numeric(Amount.Requested))

glimpse(ld)

ld=ld %>% mutate(Open.CREDIT.Lines=as.numeric(Open.CREDIT.Lines))

glimpse(ld)

ld=ld %>% mutate(Revolving.CREDIT.Balance=as.numeric(Revolving.CREDIT.Balance))

glimpse(ld)

ld=ld %>% mutate(Interest.Rate=as.numeric(gsub("%","",Interest.Rate)))

glimpse(ld)

ld=ld %>%mutate(f1=as.numeric(substr(FICO.Range,1,3)),
                f2=as.numeric(substr(FICO.Range,5,7)),
                fico=0.5*(f1+f2)
) %>%
  select(-FICO.Range,-f1,-f2)

glimpse(ld)

ld=ld %>%
  mutate(el=ifelse(substr(Employment.Length,1,2)=="10",10,Employment.Length),
         el=ifelse(substr(Employment.Length,2,1)=="<",0,el),
         el=gsub("years","",el),
         el=gsub("year","",el),
         el=as.numeric(el)
  ) %>%
  select(-Employment.Length) %>%
  na.omit()

glimpse(ld)

table(ld$Home.Ownership)

ld=ld %>%
  mutate(HW_RENT=as.numeric(Home.Ownership=="RENT"),
         HW_OWN=as.numeric(Home.Ownership=="OWN"),
         HW_MORT=as.numeric(Home.Ownership=="MORTGAGE")) %>%
  
  select(-Home.Ownership)

glimpse(ld)

table(ld$Loan.Purpose)

ld=ld %>%
  mutate(LP_cc=as.numeric(Loan.Purpose=="credit_card"),
         LP_dc=as.numeric(Loan.Purpose=="debt_consolidation"),
         LP_other=as.numeric(Loan.Purpose=="other"))%>%
  select(-Loan.Purpose)

table(ld$Loan.Length)

ld=ld %>%
  mutate(LL_36=as.numeric(Loan.Length=="36 months")) %>%
  select(-Loan.Length)

glimpse(ld)

table(ld$State)

ld=ld %>%
  select(-state)

glimpse(ld)

set.seed(2)
s=sample(1:nrow(ld),0.7*nrow(ld))
ld_train=ld[s,]
ld_test=ld[-s,]

glimpse(ld_train)

fit=lm(Interest.Rate~.-ID,data=ld_train)

install.packages("car")

library(car)

vif(fit)

fit=lm(Interest.Rate~.-ID -HW_MORT,data=ld_train)
vif(fit)

summary(fit)

fit=lm(Interest.Rate~.-ID -HW_MORT -Debt.To.Income.Ratio,data=ld_train)
summary(fit)

fit=lm(Interest.Rate~.-ID -HW_MORT -Debt.To.Income.Ratio -el,data=ld_train)
summary(fit)

fit=lm(Interest.Rate~.-ID -HW_MORT -Debt.To.Income.Ratio -el -Revolving.CREDIT.Balance,data=ld_train)
summary(fit)

fit=lm(Interest.Rate~.-ID -HW_MORT -Debt.To.Income.Ratio -el -Revolving.CREDIT.Balance -HW_OWN,data=ld_train)
summary(fit)

fit=lm(Interest.Rate~.-ID -HW_MORT -Debt.To.Income.Ratio -el -Revolving.CREDIT.Balance -HW_OWN -LP_other,data=ld_train)
summary(fit)

install.packages("ggplot2")
library(ggplot2)

ld_train %>%
  mutate(pred_IR=predict(fit,newdata=ld_train)) %>%
  ggplot(aes(x=Interest.Rate,y=pred_IR))+geom_point(alpha=0.6)

plot(fit,which =1)

plot(fit,which =2)

df=data.frame(res=fit$residual)

ggplot(df,aes(x=res))+geom_density(color="red")+
  stat_function(fun=dnorm,args=list(mean=mean(df$res),sd=sd(df$res)),color="green")

shapiro.test(fit$residuals)

plot(fit,which =3)

plot(fit,which =4)

mean((ld_test$Interest.Rate-predict(fit,newdata = ld_test))** 1) %>%
  sqrt()
