

DETTA ÄR EN GAMMAL VERSION AV MIN KOD.
VG SE PUBLICATION FÖR ALL KOD OCH TEXT.








xy<-read.csv('swetrau-scrambled.csv')

# Gör en kolumn med intub: 1, intub ED, 2. Ej intub. 3. Intub prehosp.
xy$intub <- with(xy, ifelse(`pre_intubated` == 1 & is.na(xy$pre_intubated) == FALSE, 3, `ed_intubated`))
# Förklaring: tub<- om pre inte är Na och är 1(ja), sant: konvertera till 3, Falskt: använd ed_intubated (1 är ja och 2 är nej)

# JA: Även övriga variabler behöver "städas" EXV kan GCS inte vara 1-2, 15+. RR kan max 70, ISS kan ej vara 0,1,2 osv. använd unique / summary osv för att hitta fel.
# Förslag är att göra detta tidigt i koden så att du senare kan släppa den sorteringen.

install.packages("naniar")
install.packages('gtsummary')
library(naniar)
ab<-xy %>% replace_with_na(replace = list( "res_survival" = 999))
ac<-ab %>% replace_with_na(replace = list( "intub" = 999))
swetrau<-ac %>% replace_with_na(replace = list( "host_care_level" = 999))

# Gör om blodtryck till RTS, måste klickas i ordning.
library(dplyr)

# JA: Överväg att spara dina RTS konverterade variabler direkt i en ny collumn istället för senare alternativt städa datan först.
# Varför 350 btw?.
swetrau$ed_sbp_value[is.na(swetrau$ed_sbp_value)]<-350

swetrau$ed_sbp_value[swetrau$ed_sbp_value>=1 & swetrau$ed_sbp_value<=49]<-1
swetrau$ed_sbp_value[swetrau$ed_sbp_value>=50 & swetrau$ed_sbp_value<=75]<-2
swetrau$ed_sbp_value[swetrau$ed_sbp_value>=76 & swetrau$ed_sbp_value<=89]<-3
swetrau$ed_sbp_value[swetrau$ed_sbp_value>89 & swetrau$ed_sbp_value<=298]<-4
swetrau$ed_sbp_value[swetrau$ed_sbp_value==350]<-5
swetrau$ed_sbp_value[swetrau$ed_sbp_value==999]<-5

# Tar RTS värdet från ed, om det inte finns så använder den vanliga blodtrycket, som är grupperat enligt RTS.
swetrau$sbp_rts<-with(swetrau, ifelse(ed_sbp_rtscat!=999 & is.na(swetrau$ed_sbp_rtscat)==FALSE,ed_sbp_rtscat,ed_sbp_value))

# JA: Vad gör du här?

nrow(swetrau[is.na(swetrau$inj_dominant),]+(subset(swetrau,inj_dominant=='999')))
summary(swetrau$inj_dominant)
nrow(subset(swetrau,inj_dominant=='999'|is.na(swetrau$inj_dominant)))
data.frame(table(swetrau$inj_dominant))

# Samma med ed_RR
swetrau$ed_rr_value[is.na(swetrau$ed_rr_value)]<-350

swetrau$ed_rr_value[swetrau$ed_rr_value>=1 & swetrau$ed_rr_value<=5]<-1
swetrau$ed_rr_value[swetrau$ed_rr_value>=6 & swetrau$ed_rr_value<=9]<-2
swetrau$ed_rr_value[swetrau$ed_rr_value>=10 & swetrau$ed_rr_value<=29]<-4
swetrau$ed_rr_value[swetrau$ed_rr_value>29 & swetrau$ed_rr_value<=70]<-3
swetrau$ed_rr_value[swetrau$ed_rr_value==99]<-5
swetrau$ed_rr_value[swetrau$ed_rr_value==350]<-5
swetrau$ed_rr_value[swetrau$ed_rr_value==999]<-5

swetrau$ed_rr_rts<-with(swetrau, ifelse(is.na(swetrau$ed_rr_rtscat)==FALSE & ed_rr_rtscat != 999,ed_rr_rtscat,ed_rr_value))


# Samma med pre_RR
swetrau$pre_rr_value[swetrau$pre_rr_value>=1 & swetrau$pre_rr_value<=5]<-1
swetrau$pre_rr_value[swetrau$pre_rr_value>=6 & swetrau$pre_rr_value<=9]<-2
swetrau$pre_rr_value[swetrau$pre_rr_value>=10 & swetrau$pre_rr_value<=29]<-4
swetrau$pre_rr_value[swetrau$pre_rr_value>29 & swetrau$pre_rr_value<=70]<-3
swetrau$pre_rr_value[swetrau$pre_rr_value==99]<-5
swetrau$pre_rr_value[swetrau$pre_rr_value==350]<-5
swetrau$pre_rr_value[swetrau$pre_rr_value==999]<-5

swetrau$pre_rr_rts<-with(swetrau, ifelse(is.na(swetrau$pre_rr_rtscat)==FALSE & pre_rr_rtscat != 999,pre_rr_rtscat,pre_rr_value))

swetrau$rr_rts<-with(swetrau, ifelse(intub == 3 & pre_rr_rts !=5 & pre_rr_rtscat != 999,pre_rr_rts,ed_rr_rts))

swetrau$rr_rts[swetrau$rr_rts==999]<-5

# Konvertera GCS till mild 13-15 (3), moderate 9-12 (2), severe 3-8. (1). 5 = MISSING
swetrau$ed_gcs_sum[is.na(swetrau$ed_gcs_sum)]<-350

swetrau$ed_gcs_sum[swetrau$ed_gcs_sum>=3 & swetrau$ed_gcs_sum<=8]<-1
swetrau$ed_gcs_sum[swetrau$ed_gcs_sum>=9 & swetrau$ed_gcs_sum<=12]<-2
swetrau$ed_gcs_sum[swetrau$ed_gcs_sum>=13 & swetrau$ed_gcs_sum<=15]<-3
swetrau$ed_gcs_sum[swetrau$ed_gcs_sum==99]<-5
swetrau$ed_gcs_sum[swetrau$ed_gcs_sum==350]<-5
swetrau$ed_gcs_sum[swetrau$ed_gcs_sum==999]<-5


# Gör samma med pre_GCS
swetrau$pre_gcs_sum[is.na(swetrau$pre_gcs_sum)]<-350

swetrau$pre_gcs_sum[swetrau$pre_gcs_sum>=3 & swetrau$pre_gcs_sum<=8]<-1
swetrau$pre_gcs_sum[swetrau$pre_gcs_sum>=9 & swetrau$pre_gcs_sum<=12]<-2
swetrau$pre_gcs_sum[swetrau$pre_gcs_sum>=13 & swetrau$pre_gcs_sum<=15]<-3
swetrau$pre_gcs_sum[swetrau$pre_gcs_sum==99]<-5
swetrau$pre_gcs_sum[swetrau$pre_gcs_sum==350]<-5
swetrau$pre_gcs_sum[swetrau$pre_gcs_sum==999]<-5

# Skapa gcs_sum som blir vår nya gcs, 5=missing
swetrau$gc<-with(swetrau, ifelse(intub == 3 & pre_gcs_sum !=5,pre_gcs_sum,ed_gcs_sum))
swetrau$gcs<-with(swetrau, ifelse(intub == 1 & gc ==5,99,gc))
swetrau$gcs_sum<-with(swetrau, ifelse(intub == 3 & gcs ==5,99,gcs))


# Egen datafram för räkna missing data
library(dplyr)
rak.var<-c("Gender","intub","res_survival","host_care_level","rr_rts","sbp_rts","gcs_sum")
cont.var<-c("ISS","dt_ed_first_ct","dt_ed_emerg_proc","pt_age_yrs")

#JA: Du redovisar missing nedan genom att använda ba?

ba<-select(swetrau,rak.var,cont.var)

with(ba,sum(is.na(res_survival)))

# Ta bort missing data från intub
ki<-swetrau[!is.na(swetrau$intub),]

# Lägg till problemområde, Lägg till för hand funkar om koden inte gör det.

library(readr)
##### JA: Var tvungen att ändra `Problemomrade_.FMP` -> `Problemomrade_.FMP` Var det en tanke med det? ändra tillbaka om du behöver!

problem_scrambled<-read.csv('problem-scrambled.csv')

problem_scrambled$`Problemomrade_.FMP`[problem_scrambled$`Problemomrade_.FMP`=='ok'|problem_scrambled$`Problemomrade_.FMP`=='Ok'|problem_scrambled$`Problemomrade_.FMP`=='OK'|problem_scrambled$`Problemomrade_.FMP`=='Föredömligt handlagd']<-'No'
problem_scrambled$`Problemomrade_.FMP`[problem_scrambled$`Problemomrade_.FMP`=='bristande rutin'|problem_scrambled$`Problemomrade_.FMP`=='Dokumentation'|
                                         problem_scrambled$`Problemomrade_.FMP`=='Dokumetation'|problem_scrambled$`Problemomrade_.FMP`=='Handläggning'|
                                         problem_scrambled$`Problemomrade_.FMP`=='Kommunikation'|problem_scrambled$`Problemomrade_.FMP`=='kompetens brist'|
                                         problem_scrambled$`Problemomrade_.FMP`=='Logistik/teknik'|problem_scrambled$`Problemomrade_.FMP`=='Lång tid till DT'|
                                         problem_scrambled$`Problemomrade_.FMP`=='Lång tid till op'|problem_scrambled$`Problemomrade_.FMP`=='Missad skada'|
                                         problem_scrambled$`Problemomrade_.FMP`=='Neurokirurg'|problem_scrambled$`Problemomrade_.FMP`=='Tertiär survey'|
                                         problem_scrambled$`Problemomrade_.FMP`=='Resurs'|problem_scrambled$`Problemomrade_.FMP`=='Traumakriterier/styrning'|
                                         problem_scrambled$`Problemomrade_.FMP`=='Triage på akutmottagningen'|problem_scrambled$`Problemomrade_.FMP`=='Vårdnivå']<-'Yes'

rename(problem_scrambled,probYN=`Problemomrade_.FMP`)


# Sätt ihop databaser mha id
nk<-merge(ki,problem_scrambled,by='id')


# Välj variabler
cat.var<-c("id","probYN","Gender","res_survival","intub","host_care_level","rr_rts","sbp_rts","gcs_sum")
cont.var<-c("ISS","dt_ed_first_ct","dt_ed_emerg_proc","pt_age_yrs")

nk$probYN<-nk$`Problemomrade_.FMP`

# Gör en en dataframe (nks) med endast våra variabler.
library(dplyr)
nks<-select(nk,cat.var,cont.var)

# JA: Är detta för att ta bort barn från swetrau?

nkk<-subset(nks,!(pt_age_yrs%in%c(1,2,3,4,5,6,7,8,9,10,11,12,13,14)))

# Se alla missing values
summary(nkk)
# JA: Du kommer behöva hänvisa till antalet i texten. Extractionen kan vara exv vara Missing.RR <- sum(is.na(nkk$rr_rts)).
aa<-na.omit(nkk)
data.frame(table(aa$probYN))

ae<-aa


# Logistic regression


  # Sätt alla cat.var som factor variables

#JA: Tror det går att göra as.factor på delar av dataset om du villa slippa köra collumn för collumn. men fungerar såklart.
str(ae)
ae$probYN<-as.factor(ae$probYN)
ae$Gender<-as.factor(ae$Gender)
ae$host_care_level<-as.factor(ae$host_care_level)
ae$intub<-as.factor(ae$intub)
ae$res_survival<-as.factor(ae$res_survival)
ae$rr_rts<-as.factor(ae$rr_rts)
ae$sbp_rts<-as.factor(ae$sbp_rts)
ae$gcs_sum<-as.factor(ae$gcs_sum)


# Rita ut alla grafer
pairs(ae,col=ae$probYN)

# Logistic regression på variabler för sig
sex<-glm(probYN~Gender,data = ae,family = 'binomial')
summary(sex)

surv<-glm(probYN~res_survival,data = ae,family = 'binomial')
summary(surv)

tub<-glm(probYN~intub,data = ae,family = 'binomial')
summary(tub)

care<-glm(probYN~host_care_level,data = ae,family = 'binomial')
summary(care)

resp<-glm(probYN~rr_rts,data = ae,family = 'binomial')
summary(resp)

sysbp<-glm(probYN~sbp_rts,data = ae,family = 'binomial')
summary(sysbp)

gcs_var<-glm(probYN~gcs_sum,data = ae,family = 'binomial')
summary(gcs_var)

injury<-glm(probYN~ISS,data = ae,family = 'binomial')
summary(injury)

first_ct<-glm(probYN~dt_ed_first_ct,data = ae,family = 'binomial')
summary(first_ct)

first_proc<-glm(probYN~dt_ed_emerg_proc,data = ae,family = 'binomial')
summary(first_proc)

age<-glm(probYN~pt_age_yrs,data = ae,family = 'binomial')
summary(age)

logistic<-glm(probYN~.,data = ae,family = 'binomial')
summary(logistic)
plot(logistic)

glm.fit=glm(probYN~.,data = ae,family = binomial)

summary(glm.fit)
plot(glm.fit)
pairs(ae,col=ae$probYN)


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

#JA: Finns det en anledning till atu valt att lägga intubations status som en separat flik innan och utanför övriga missing values?
# Förslag är att intubation ingår i missing values och att du även presenterar en tabell med de olika variablernas NA.
# Vi får klura på exakt hur vi presenterar att en patient kan ha flera missing: Prel med en kort deskriptiv text kring det och sedan bara total missing.

flow_exclusions(
  incl_counts = c(nrow(xy),nrow(ki), nrow(nks), nrow(nkk),nrow(aa)),
  total_label = "Patients in the trauma registry",
  incl_labels = c("Intubation-status known", "Outcome identified", "Matching set criteria","Included in the study"),
  excl_labels = c("Intubation-status unknown", "Outcome unidentified", "Not meeting age criteria", "Missing value")
)


a <- c(0,1)
a$a <- nrow(problem_scrambled)
a <- as.data.frame(a)
a$b <- nrow(known_problem_area)
a$c <- nrow(fewer_variables)
a$d <- nrow(blunt_multisystem_cohort)
a$e <- nrow(penetrating_cohort)
a$f <- nrow(severe_tbi_cohort)
a$g <- nrow(shock_cohort)
a$h <- nrow(geriatric_cohort)
#df with nrow of relevant df:s to be used as reference in code below

