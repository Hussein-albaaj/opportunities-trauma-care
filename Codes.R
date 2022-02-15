xy<-read.csv('swetrau-scrambled.csv')

# Gör en kolumn med intub: 1, intub ED, 2. Ej intub. 3. Intub prehosp.
xy$intub <- with(xy, ifelse(`pre_intubated` == 1 & is.na(xy$pre_intubated) == FALSE, 3, `ed_intubated`))
# Förklaring: tub<- om pre inte är Na och är 1(ja), sant: konvertera till 3, Falskt: använd ed_intubated (1 är ja och 2 är nej)

install.packages("naniar")
library(naniar)
ab<-xy %>% replace_with_na(replace = list( "res_survival" = 999))
ac<-ab %>% replace_with_na(replace = list( "intub" = 999))
swetrau<-ac %>% replace_with_na(replace = list( "host_care_level" = 999))

# Egen datafram för räkna missing data
library(dplyr)
rak.var<-c("Gender","res_survival","intub","host_care_level","rr_rts","sbp_rts","gcs_sum")
cont.var<-c("ISS","dt_ed_first_ct","dt_ed_emerg_proc","pt_age_yrs")


ba<-select(swetrau,rak.var,cont.var)

# Gör om blodtryck till RTS, måste klickas i ordning.
library(dplyr)

swetrau$ed_sbp_value[is.na(swetrau$ed_sbp_value)]<-350

swetrau$ed_sbp_value[swetrau$ed_sbp_value>=1 & swetrau$ed_sbp_value<=49]<-1
swetrau$ed_sbp_value[swetrau$ed_sbp_value>=50 & swetrau$ed_sbp_value<=75]<-2
swetrau$ed_sbp_value[swetrau$ed_sbp_value>=76 & swetrau$ed_sbp_value<=89]<-3
swetrau$ed_sbp_value[swetrau$ed_sbp_value>89 & swetrau$ed_sbp_value<=298]<-4
swetrau$ed_sbp_value[swetrau$ed_sbp_value==350]<-5
swetrau$ed_sbp_value[swetrau$ed_sbp_value==999]<-5

# Tar RTS värdet från ed, om det inte finns så använder den vanliga blodtrycket, som är grupperat enligt RTS.
swetrau$sbp_rts<-with(swetrau, ifelse(ed_sbp_rtscat!=999 & is.na(swetrau$ed_sbp_rtscat)==FALSE,ed_sbp_rtscat,ed_sbp_value))

nrow(swetrau[is.na(swetrau$inj_dominant),]+(subset(swetrau,inj_dominant=='999')))
summary(swetrau$inj_dominant)
nrow(subset(swetrau,inj_dominant=='999'|is.na(swetrau$inj_dominant)))
data.frame(table(swetrau$inj_dominant))

# Ta bort missing data från intub
ki<-swetrau[!is.na(swetrau$intub),]


# Samma med ed_RR
ki$ed_rr_value[is.na(ki$ed_rr_value)]<-350

ki$ed_rr_value[ki$ed_rr_value>=1 & ki$ed_rr_value<=5]<-1
ki$ed_rr_value[ki$ed_rr_value>=6 & ki$ed_rr_value<=9]<-2
ki$ed_rr_value[ki$ed_rr_value>=10 & ki$ed_rr_value<=29]<-4
ki$ed_rr_value[ki$ed_rr_value>29 & ki$ed_rr_value<=70]<-3
ki$ed_rr_value[ki$ed_rr_value==99]<-5
ki$ed_rr_value[ki$ed_rr_value==350]<-5
ki$ed_rr_value[ki$ed_rr_value==999]<-5

ki$ed_rr_rts<-with(ki, ifelse(is.na(ki$ed_rr_rtscat)==FALSE & ed_rr_rtscat != 999,ed_rr_rtscat,ed_rr_value))


# Samma med pre_RR
ki$pre_rr_value[ki$pre_rr_value>=1 & ki$pre_rr_value<=5]<-1
ki$pre_rr_value[ki$pre_rr_value>=6 & ki$pre_rr_value<=9]<-2
ki$pre_rr_value[ki$pre_rr_value>=10 & ki$pre_rr_value<=29]<-4
ki$pre_rr_value[ki$pre_rr_value>29 & ki$pre_rr_value<=70]<-3
ki$pre_rr_value[ki$pre_rr_value==99]<-5
ki$pre_rr_value[ki$pre_rr_value==350]<-5
ki$pre_rr_value[ki$pre_rr_value==999]<-5

ki$pre_rr_rts<-with(ki, ifelse(is.na(ki$pre_rr_rtscat)==FALSE & pre_rr_rtscat != 999,pre_rr_rtscat,pre_rr_value))

ki$rr_rts<-with(ki, ifelse(intub == 3 & pre_rr_rts !=5 & pre_rr_rtscat != 999,pre_rr_rts,ed_rr_rts))

ki$rr_rts[ki$rr_rts==999]<-5

# Konvertera GCS till mild 13-15 (3), moderate 9-12 (2), severe 3-8. (1). 5 = MISSING
ki$ed_gcs_sum[is.na(ki$ed_gcs_sum)]<-350

ki$ed_gcs_sum[ki$ed_gcs_sum>=3 & ki$ed_gcs_sum<=8]<-1
ki$ed_gcs_sum[ki$ed_gcs_sum>=9 & ki$ed_gcs_sum<=12]<-2
ki$ed_gcs_sum[ki$ed_gcs_sum>=13 & ki$ed_gcs_sum<=15]<-3
ki$ed_gcs_sum[ki$ed_gcs_sum==99]<-5
ki$ed_gcs_sum[ki$ed_gcs_sum==350]<-5
ki$ed_gcs_sum[ki$ed_gcs_sum==999]<-5


# Gör samma med pre_GCS
ki$pre_gcs_sum[is.na(ki$pre_gcs_sum)]<-350

ki$pre_gcs_sum[ki$pre_gcs_sum>=3 & ki$pre_gcs_sum<=8]<-1
ki$pre_gcs_sum[ki$pre_gcs_sum>=9 & ki$pre_gcs_sum<=12]<-2
ki$pre_gcs_sum[ki$pre_gcs_sum>=13 & ki$pre_gcs_sum<=15]<-3
ki$pre_gcs_sum[ki$pre_gcs_sum==99]<-5
ki$pre_gcs_sum[ki$pre_gcs_sum==350]<-5
ki$pre_gcs_sum[ki$pre_gcs_sum==999]<-5


# Skapa gcs_sum som blir vår nya gcs, 5=missing
ki$gc<-with(ki, ifelse(intub == 3 & pre_gcs_sum !=5,pre_gcs_sum,ed_gcs_sum))
ki$gcs<-with(ki, ifelse(intub == 1 & gc ==5,99,gc))
ki$gcs_sum<-with(ki, ifelse(intub == 3 & gcs ==5,99,gcs))


# Lägg till problemområde, Lägg till för hand funkar om koden inte gör det.

library(readr)
problem_scrambled<-read.csv('problem-scrambled.csv')

problem_scrambled$`Problemomrade_ FMP`[problem_scrambled$`Problemomrade_ FMP`=='ok'|problem_scrambled$`Problemomrade_ FMP`=='Ok'|problem_scrambled$`Problemomrade_ FMP`=='OK'|problem_scrambled$`Problemomrade_ FMP`=='Föredömligt handlagd']<-'No'
problem_scrambled$`Problemomrade_ FMP`[problem_scrambled$`Problemomrade_ FMP`=='bristande rutin'|problem_scrambled$`Problemomrade_ FMP`=='Dokumentation'|
                                         problem_scrambled$`Problemomrade_ FMP`=='Dokumetation'|problem_scrambled$`Problemomrade_ FMP`=='Handläggning'|
                                         problem_scrambled$`Problemomrade_ FMP`=='Kommunikation'|problem_scrambled$`Problemomrade_ FMP`=='kompetens brist'|
                                         problem_scrambled$`Problemomrade_ FMP`=='Logistik/teknik'|problem_scrambled$`Problemomrade_ FMP`=='Lång tid till DT'|
                                         problem_scrambled$`Problemomrade_ FMP`=='Lång tid till op'|problem_scrambled$`Problemomrade_ FMP`=='Missad skada'|
                                         problem_scrambled$`Problemomrade_ FMP`=='Neurokirurg'|problem_scrambled$`Problemomrade_ FMP`=='Tertiär survey'|
                                         problem_scrambled$`Problemomrade_ FMP`=='Resurs'|problem_scrambled$`Problemomrade_ FMP`=='Traumakriterier/styrning'|
                                         problem_scrambled$`Problemomrade_ FMP`=='Triage på akutmottagningen'|problem_scrambled$`Problemomrade_ FMP`=='Vårdnivå']<-'Yes'

rename(problem_scrambled,probYN=`Problemomrade_ FMP`)


# Sätt ihop databaser mha id
nk<-merge(ki,problem_scrambled,by='id')


# Välj variabler
cat.var<-c("id","probYN","Gender","res_survival","intub","host_care_level","rr_rts","sbp_rts","gcs_sum")
cont.var<-c("ISS","dt_ed_first_ct","dt_ed_emerg_proc","pt_age_yrs")

nk$probYN<-nk$`Problemomrade_ FMP`

# Gör en en dataframe (nks) med endast våra variabler.
library(dplyr)
nks<-select(nk,cat.var,cont.var)

nkk<-subset(nks,!(pt_age_yrs%in%c(1,2,3,4,5,6,7,8,9,10,11,12,13,14)))

# Se alla missing values
summary(nkk)

aa<-na.omit(nkk)
data.frame(table(aa$probYN))

ae<-aa


# Logistic regression

  # Sätt alla cat.var som factor variables
str(aa)
aa$probYN<-as.factor(aa$probYN)
aa$Gender<-as.factor(aa$Gender)
aa$host_care_level<-as.factor(aa$host_care_level)
aa$intub<-as.factor(aa$intub)
aa$res_survival<-as.factor(aa$res_survival)
aa$rr_rts<-as.factor(aa$rr_rts)
aa$sbp_rts<-as.factor(aa$sbp_rts)
aa$gcs_sum<-as.factor(aa$gcs_sum)


pairs(aa,col=aa$probYN)

sex<-glm(probYN~Gender,data = aa,family = 'binomial')
summary(sex)

surv<-glm(probYN~res_survival,data = aa,family = 'binomial')
summary(surv)

tub<-glm(probYN~intub,data = aa,family = 'binomial')
summary(tub)

care<-glm(probYN~host_care_level,data = aa,family = 'binomial')
summary(care)

surv<-glm(probYN~res_survival,data = aa,family = 'binomial')
summary(surv)


logistic<-glm(probYN~.,data = aa,family = 'binomial')
summary(logistic)
plot(logistic)

glm.fit=glm(probYN~.,data = aa,family = binomial)

summary(glm.fit)
plot(glm.fit)
pairs(aa,col=aa$probYN)


# Table 1

install.packages("table1")
library(table1)

aa$probYN <-
  factor(aa$probYN,
         levels=c("Yes","No"),
         labels=c("Opportunity for improvement",
                  "No opportunity for improvement"))

aa$Gender <-
  factor(aa$Gender, levels=c("M","K"),
         labels=c("Male",
                  "Female"))

aa$res_survival <-
  factor(aa$res_survival, levels=c("1","2"),
         labels=c("Dead",
                  "Alive"))

aa$intub <-
  factor(aa$intub, levels=c("3","1","2"),
         labels=c("Pre-hospital Intubation",
                  "ED Intubation","Not Intubated"))

aa$host_care_level <-
  factor(aa$host_care_level, levels=c("1","2","3","4","5"),
         labels=c("ED","Admissioned","Surgical Ward","Specialized ward","ICU"))

aa$sbp_rts <-
  factor(aa$sbp_rts, levels=c("4","3","2","1","0","5"),
         labels=c(">89","76-89","50-75","1-49","0","Missing"))

aa$gcs_sum <-
  factor(aa$gcs_sum, levels=c("3","2","1","5","99"),
         labels=c("Mild: 13-15","Moderate: 9-12","Severe: 3-8","Missing","Pre-intubated"))

aa$rr_rts <-
  factor(aa$rr_rts, levels=c("3","4","2","1","0","5"),
         labels=c(">29","10-29","6-9","1-5","0","Missing"))



# Döp om variablerna

label(aa$ISS)      <- "Injury Severity Score"
label(aa$Gender)       <- "Gender"
label(aa$intub)     <- "Intubation"
label(aa$pt_age_yrs)       <- "Age"
label(aa$res_survival)       <- "Survival after 30 days"
label(aa$host_care_level)       <- "Highest hospital care level"
label(aa$intub)       <- "Intubation"
label(aa$rr_rts)       <- "Respiratory rate"
label(aa$sbp_rts)       <- "Systolic Blood Pressure"
label(aa$gcs_sum)       <- "GCS"
label(aa$dt_ed_first_ct)       <- "Time to first CT"
label(aa$dt_ed_emerg_proc)       <- "Time to intervention"

# Ange enhet där det krävs

units(aa$pt_age_yrs)       <- "Years"
units(aa$dt_ed_first_ct)   <- "minutes"
units(aa$dt_ed_emerg_proc)   <- "minutes"
units(aa$sbp_rts)       <- "RTS"
units(aa$rr_rts)       <- "RTS"

table1(~ Gender + pt_age_yrs + res_survival + host_care_level + intub + ISS + rr_rts + sbp_rts +
         gcs_sum + dt_ed_first_ct + dt_ed_emerg_proc| probYN, data=aa)




# Flow chart

install.packages("PRISMAstatement")
library(PRISMAstatement)

flow_exclusions(
  incl_counts = c(nrow(xy),nrow(ki), nrow(nks), nrow(nkk),nrow(aa)),
  total_label = "Patients in the trauma registry",
  incl_labels = c("Intubation-status known", "Outcome identified", "Matching set criteria","Included in the study"),
  excl_labels = c("Intubation-status unknown", "Outcome unidentified", "Not meeting age criteria", "Missing value")
)
