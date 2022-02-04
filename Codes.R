swetrau<-read.csv('swetrau-scrambled.csv')

# Gör en kolumn med intub: 1, intub ED, 2. Ej intub. 3. Intub prehosp.
swetrau$intub <- with(swetrau, ifelse(`pre_intubated` == 1 & is.na(swetrau$pre_intubated) == FALSE, 3, `ed_intubated`))
# Förklaring: tub<- om pre inte är Na och är 1(ja), sant: konvertera till 3, Falskt: använd ed_intubated (1 är ja och 2 är nej)



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


# Ta bort missing data från intub
dat<-swetrau[!is.na(swetrau$intub),]
ki<-subset(dat,intub!=999)


# Samma med ed_RR
ki$ed_rr_value[is.na(ki$ed_rr_value)]<-350

ki$ed_rr_value[ki$ed_rr_value>=1 & ki$ed_rr_value<=5]<-1
ki$ed_rr_value[ki$ed_rr_value>=6 & ki$ed_rr_value<=9]<-2
ki$ed_rr_value[ki$ed_rr_value>=10 & ki$ed_rr_value<=29]<-4
ki$ed_rr_value[ki$ed_rr_value>29 & ki$ed_rr_value<=70]<-3
ki$ed_rr_value[ki$ed_rr_value==99]<-5
ki$ed_rr_value[ki$ed_rr_value==350]<-5
ki$ed_rr_value[ki$ed_rr_value==999]<-5

ki$ed_rr_rts<-with(ki, ifelse(is.na(ki$ed_rr_rtscat)==FALSE,ed_rr_rtscat,ed_rr_value))


# Samma med pre_RR
ki$pre_rr_value[ki$pre_rr_value>=1 & ki$pre_rr_value<=5]<-1
ki$pre_rr_value[ki$pre_rr_value>=6 & ki$pre_rr_value<=9]<-2
ki$pre_rr_value[ki$pre_rr_value>=10 & ki$pre_rr_value<=29]<-4
ki$pre_rr_value[ki$pre_rr_value>29 & ki$pre_rr_value<=70]<-3
ki$pre_rr_value[ki$pre_rr_value==99]<-5
ki$pre_rr_value[ki$pre_rr_value==350]<-5
ki$pre_rr_value[ki$pre_rr_value==999]<-5

ki$pre_rr_rts<-with(ki, ifelse(is.na(ki$pre_rr_rtscat)==FALSE,pre_rr_rtscat,pre_rr_value))

ki$rr_rts<-with(ki, ifelse(intub == 3 & pre_rr_rts !=5,pre_rr_rts,ed_rr_rts))


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
ki$gcs<-with(ki, ifelse(intub == 1 | intub == 3 & gc ==5,99,gc))

# Kanske inte ska användas????? ifall intub har na gcs så använder den pre_gcs ki$gcs_sum<-with(ki, ifelse(intub == 1 & gcs ==5,pre_gcs_sum,gcs))


# Lägg till problemområde

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

names(nk)[208] <- 'probYN'

# Gör en en dataframe (nks) med endast våra variabler.
library(dplyr)
nks<-select(nk,cat.var,cont.var)
summary(nks)
data.frame(table(nks$probYN))
with(nks,sum(is.na(nks$probYN)))


