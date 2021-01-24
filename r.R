library(haven)
??haven
library(sjmisc)
library('sjlabelled')
library(tidyverse)
cgss2017 <- read_dta("cgss2017/cgss2017.dta")
cgss2017backup <- cgss2017
descr(cgss2017backup$a31)
descr(cgss2017backup$a285)
descr(cgss2017backup$a311)

cgss2017[cgss2017 >= 9999997] <- NA
cgss2017[cgss2017 == 9999996] <- 999999
cgss2017[cgss2017 == 98] <- NA
cgss2017[cgss2017 == 99] <- NA
cgss2017 <- sjlabelled::drop_labels(cgss2017)
cgss2017 <- sjmisc::to_label(cgss2017)
descr(cgss2017backup$a31)
descr(cgss2017backup$a285)
descr(cgss2017backup$a311)
table(cgss2017$a311)
table(cgss2017$a285)
cgss2017 <- droplevels(cgss2017)
library(tidyverse)
cgssx <-
  cgss2017 %>% select(
    birth = a31,
    surinternet = a285,
    socontact = a311,
  )
cgssx <- rename(cgssx)
cgssx$age <- 2017 - cgssx$birth
cgssx<-filter(cgssx,cgssx$age>18)
table(cgssx$age)

#library(carData)
#library('car')
#cgssx$age <- as.numeric(cgssx$age)
#cgssx$age <=car::recode(cgssx$age,"19:27='90��';28��37='80��';38��47='70��';48��57='60��';58:67='50��';68:77='40��';78:87='30��';88:97='20��';98:103='1900��'")
#table(cgssx$age)


cgssx$age <- as.numeric(cgssx$age)
cgssx$age.cut=cut(cgssx$age,breaks = c(18,28,38,48,58,68,78,88,98,108,117),labels = c('90��','80��','70��','60��','50��','40��','30��','20��','10��','1900��'))
cgssx$age.cut
table(cgssx$age.cut)

qplot(cgssx$age.cut)
plot(cgssx$age.cut)
str(cgssx$age.cut)

cgssx <- droplevels(cgssx)

#install.packages("gtsummary")
library("gtsummary")
tbl_summary(cgssx)#??tbl
print(cgssx)#head(trial)
#tbl_summary(cgssx,statistic = list(all_continuous() ~ "{mean}({sd}) {median} ({min}, {p25}, {p75},{max})"),label = list(age = "age"),digits = list(age ~ c(2, 2), c(surinternet) ~ 1))
cgssx %>%
  select(socontact, surinternet, age.cut) %>% tbl_summary(
    statistic = list(
      all_continuous() ~ "{mean}({sd}) {median} ({min}, {p25}, {p75},{max})"
    ),
    label = list(age.cut = "age"),
    digits = list(socontact ~ c(2, 2), c(surinternet) ~ 1)
  )

cgssx %>%
  select(socontact, surinternet, age.cut) %>% tbl_summary(
    by = age.cut, missing = "no",
    type = all_continuous() ~ "continuous2",
    statistic = list(
      all_continuous() ~ c("{N_nonmiss}",
                           "{median} ({p25}, {p75})", 
                           "{min}, {max}")
    ),
    label = list(age.cut = "age"),
    digits = list(socontact ~ c(2, 2), c(surinternet) ~ 1)
  )  # %>%
#add_n() %>% # add column with total number of non-missing observations
  #add_p() %>% # test for a difference between groups
  #modify_header(label = "**Variable**") %>% # update the column header
  #bold_labels() 
#age_surinternet_and_socontact
#install.packages('ggstatsplot')
library(ggstatsplot)
library(ggplot2)
ggplot(cgssx[is.na(cgssx$socontact) == F,], aes(x=socontact,fill=age.cut,na.rm = TRUE)) +
  geom_bar(stat="count",na.rm = TRUE)+geom_text(stat='count',aes(label=..count..),position=position_stack(vjust=0.5))+
  facet_wrap(~age.cut)

library(ggplot2)
ggplot(cgssx[is.na(cgssx$socontact) == F,], aes(x=socontact,fill=surinternet,na.rm = TRUE)) +
  geom_bar(stat="count",na.rm = TRUE)+geom_text(stat='count',aes(label=..count..),position=position_stack(vjust=0.5))+
  facet_wrap(~surinternet)

library(ggplot2)
ggplot(cgssx[is.na(cgssx$surinternet) == F,], aes(x=surinternet,fill=age.cut,na.rm = TRUE)) +
  geom_bar(stat="count",na.rm = TRUE)+geom_text(stat='count',aes(label=..count..),position=position_stack(vjust=0.5))+
  facet_wrap(~age.cut)

ggplot(cgssx[is.na(cgssx$socontact) == F,], aes(x=socontact,fill=age.cut)) +geom_bar(stat="count",position = "dodge")+geom_text(stat='count',aes(label=..count..),vjust=-1,position=position_dodge(width=0.9))
ggplot(cgssx[is.na(cgssx$surinternet) == F,], aes(x=surinternet,fill=age.cut)) +geom_bar(stat="count",position = "dodge")+geom_text(stat='count',aes(label=..count..),vjust=-1,position=position_dodge(width=0.9))

library(insight)
library(Matrix)
library(lme4)
ggstatsplot::ggpiestats(
  data = cgssx,
  x = age.cut,
  y = surinternet,
  title = "��ͬ�����Ի�������ʹ���������ͳ��ͼ", # title for the plot
  legend.title = "����ֲ�", # title for the legend
  caption = substitute(paste(italic("Source"), ": 1974 Motor Trend US magazine"))
)
??ggpiestats
??insight
ggstatsplot::ggpiestats(
  data = cgssx,
  x = age.cut,
  y = socontact,
  title = "��ͬ�������罻Ƶ�ʱ�ͼ", # title for the plot
  legend.title = "����ֲ�", # title for the legend
  caption = substitute(paste(italic("Source"), ": 1974 Motor Trend US magazine"))
)

ggstatsplot::ggpiestats(
  data = cgssx,
  x = surinternet,
  y = socontact,
  title = "�������罻Ƶ�ʱ�ͼ", # title for the plot
  legend.title = "����Ƶ��", # title for the legend
  caption = substitute(paste(italic("Source"), ": 1974 Motor Trend US magazine"))
)
ggstatsplot::ggbarstats(
  data = cgssx,
  x = surinternet,
  y = socontact,
  title = "�������罻Ƶ������ͼ",
  xlab = "����Ƶ��",
  legend.title = "�罻Ƶ��"
  #ggtheme = hrbrthemes::theme_ipsum_pub(),
)
library(emmeans)
library(effects)
library(ggeffects)
names(cgssx)

#cgssx$surinternet <-
  #factor(cgssx$surinternet,
         #levels = c(1, 2, 3, 4, 5),
        # labels = c("never", "seldom", "sometimes","ofen","very_ofen"))
#table(cgssx$surinternet)
#levels(socontect)

cgssx$socontact <- as.numeric(cgssx$socontact)#��ֵ����ת��
cgssx$surinternet <- as.numeric(cgssx$surinternet)
cgssx$socontact <- as.factor(cgssx$socontact)
cgssx$surinternet <- as.factor(cgssx$surinternet)
tbl_summary(cgssx)

#linear models
sat_mod1 <- lm(socontact ~ age.cut, # regression formula
               data = cgssx) # data 
sat_mod2<-lm(socontact ~ surinternet, data = cgssx) 
sat_mod3<-lm(socontact ~ age.cut+surinternet+age.cut*surinternet, data = cgssx) 
# model marginsplot ggeffects
mydf<-ggpredict(sat_mod3, terms = c("socontact", "surinternet"),data = cgssx)
ggplot(mydf, aes(x = x, y = predicted, colour = group)) +
  geom_point() +
  stat_smooth(method = "lm", se = FALSE)

mydf2<-ggpredict(sat_mod3, terms = c("surinternet","age.cut"),data = cgssx)
ggplot(mydf2, aes(x = x, y = predicted, colour = group)) +
  geom_line() +
  stat_smooth(method = "lm", se = FALSE)

cgssx$socontact <- as.factor(cgssx$socontact)
cgssx$surinternet <- as.factor(cgssx$surinternet)

plot(cgssx$surinternet,cgssx$socontact,pch=20,xlab="����Ƶ��",ylab="�罻Ƶ��")
model1 <- lm(cgssx$surinternet ~cgssx$socontact, data = cgssx) #�ع����
summary(model1) #�ع������
anova(model1) #���������
abline()  #���ֱ��,����Ҳ������lines(x$X1,fitted(model1))
#hist(cgssx$age.cut,breaks = 10)