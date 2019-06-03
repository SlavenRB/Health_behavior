#############################################################################################################################.
#	Psihološki cinioci zdravstvenog ponašanja mladih - analiza podataka.
#############################################################################################################################.

# Instalacija paketa 
install.packages('psych',repos='http://cran.us.r-project.org')
install.packages('ggplot2',repos='http://cran.us.r-project.org')
install.packages('nlme',repos='http://cran.us.r-project.org')
install.packages('effects',repos='http://cran.us.r-project.org')
install.packages('Rcpp',repos='http://cran.us.r-project.org')
install.packages('reshape2',repos='http://cran.us.r-project.org')
install.packages('car',repos='http://cran.us.r-project.org')
install.packages('descr',repos='http://cran.us.r-project.org')
install.packages('robustbase',repos='http://cran.us.r-project.org')
install.packages('psychometric',repos='http://cran.us.r-project.org')
install.packages('lme4',repos='http://cran.us.r-project.org')

# Ucitavanje paketa
library(foreign) # za ucitavanje .sav fajlova
library(psych) # za deskriptivnu statistiku
library(ggplot2) # za grafiku
library(nlme) # za multilevel 
library(effects) # za moderaciju
library(reshape2) # za restrukturaciju baze
library(car) # za rekodiranje baze
library(descr) # za tabele
library(robustbase) # za robusnu regresiju
library(psychometric) # za proveru psihometrijskkjh karakteristika


# Ucitavanje podataka (cela final baza)
podaci <- read.spss("C:\\!Slaven\\1 TEZA\\4 baza podataka\\!Zdravstveno ponasanje - final.sav", to.data.frame=T)

# Selektovanje poduzorka ispitanika za koje imamo podatke na svim instrumentima (n=98)
podaci <- podaci[which(podaci$PunSet == "Da"), ]

# Definisanje vrednosti planiranih nedostajucih podataka

podaci$unap[podaci$unap==9999]<- NA
podaci$unap_koliko[podaci$unap_koliko==9999]<- NA
podaci$unap_koliko_ic[podaci$unap_koliko_ic==9999]<- NA
podaci$unap_trebalo[podaci$unap_trebalo==9999]<- NA
podaci$unap_da_afek[podaci$unap_da_afek==9999]<- NA
podaci$unap_da_afek_ic[podaci$unap_da_afek_ic==9999]<- NA
podaci$unap_da_afek_lag[podaci$unap_da_afek_lag==9999]<- NA
podaci$unap_da_afek_lag_ic[podaci$unap_da_afek_lag_ic==9999]<- NA
podaci$unap_ne_afek[podaci$unap_ne_afek==9999]<- NA
podaci$unap_ne_afek_c[podaci$unap_ne_afek_c==9999]<- NA
podaci$unap_ne_afek_c_ic[podaci$unap_ne_afek_c_ic==9999]<- NA
podaci$unap_ne_afek_c_lag[podaci$unap_ne_afek_c_lag==9999]<- NA
podaci$unap_ne_afek_c_lag_ic[podaci$unap_ne_afek_c_lag_ic==9999]<- NA
podaci$poz_afek[podaci$poz_afek==9999]<- NA
podaci$poz_afek_ic[podaci$poz_afek_ic==9999]<- NA
podaci$poz_afek_lag[podaci$poz_afek_lag==9999]<- NA
podaci$poz_afek_lag_ic[podaci$poz_afek_lag_ic==9999]<- NA
podaci$unap_int[podaci$unap_int==9999]<- NA
podaci$unap_int_gc[podaci$unap_int_gc==9999]<- NA
podaci$ugr[podaci$ugr==9999]<- NA
podaci$ugr_koliko[podaci$ugr_koliko==9999]<- NA
podaci$ugr_koliko_ic[podaci$ugr_koliko_ic==9999]<- NA
podaci$ugr_iskusenje[podaci$ugr_iskusenje==9999]<- NA
podaci$ugr_da_afek[podaci$ugr_da_afek==9999]<- NA
podaci$ugr_da_afek_ic[podaci$ugr_da_afek_ic==9999]<- NA
podaci$ugr_da_afek_lag[podaci$ugr_da_afek_lag==9999]<- NA
podaci$ugr_da_afek_lag_ic[podaci$ugr_da_afek_lag_ic==9999]<- NA
podaci$ugr_ne_afek[podaci$ugr_ne_afek==9999]<- NA
podaci$ugr_ne_afek_c[podaci$ugr_ne_afek_c==9999]<- NA
podaci$ugr_ne_afek_c_ic[podaci$ugr_ne_afek_c_ic==9999]<- NA
podaci$ugr_ne_afek_c_lag[podaci$ugr_ne_afek_c_lag==9999]<- NA
podaci$ugr_ne_afek_c_lag_ic[podaci$ugr_ne_afek_c_lag_ic==9999]<- NA
podaci$neg_afek[podaci$neg_afek==9999]<- NA
podaci$neg_afek_ic[podaci$neg_afek_ic==9999]<- NA
podaci$neg_afek_lag[podaci$neg_afek_lag==9999]<- NA
podaci$neg_afek_lag_ic[podaci$neg_afek_lag_ic==9999]<- NA
podaci$ugr_int[podaci$ugr_int==9999]<- NA
podaci$ugr_int_gc[podaci$ugr_int_gc==9999]<- NA
podaci$ugr_iskusenje_ic[podaci$ugr_iskusenje_ic==9999]<- NA
podaci$total_afek[podaci$total_afek==9999]<- NA
podaci$total_afek_ic[podaci$total_afek_ic==9999]<- NA
podaci$total_afek_lag[podaci$total_afek_lag==9999]<- NA
podaci$total_afek_lag_ic[podaci$total_afek_lag_ic==9999]<- NA
podaci$Desavanja_ic[podaci$Desavanja_ic==9999]<- NA
podaci$Umor_ic[podaci$Umor_ic==9999]<- NA
podaci$Desavanja_lag_ic[podaci$Desavanja_lag_ic==9999]<- NA
podaci$Umor_lag_ic[podaci$Umor_lag_ic==9999]<- NA
podaci$HonestyHumility_gc[podaci$HonestyHumility_gc==9999]<- NA
podaci$Emotionality_gc[podaci$Emotionality_gc==9999]<- NA
podaci$Extraversion_gc[podaci$Extraversion_gc==9999]<- NA
podaci$Agreeableness_gc[podaci$Agreeableness_gc==9999]<- NA
podaci$Conscientiousness_gc[podaci$Conscientiousness_gc==9999]<- NA
podaci$Openness_gc[podaci$Openness_gc==9999]<- NA
podaci$SDQPhAbilities_gc[podaci$SDQPhAbilities_gc==9999]<- NA
podaci$SDQPhAppereance_gc[podaci$SDQPhAppereance_gc==9999]<- NA
podaci$SDQGenEsteem_gc[podaci$SDQGenEsteem_gc==9999]<- NA
podaci$PrevFokus_gc[podaci$PrevFokus_gc==9999]<- NA
podaci$PromoFokus_gc[podaci$PromoFokus_gc==9999]<- NA
podaci$Voda[podaci$Voda==9999]<- NA
podaci$Voda_ikad[podaci$Voda_ikad==9999]<- NA
podaci$Voda_koliko[podaci$Voda_koliko==9999]<- NA
podaci$Voda_koliko_ic[podaci$Voda_koliko_ic==9999]<- NA
podaci$Voda_trebalo[podaci$Voda_trebalo==9999]<- NA
podaci$Voda_da_afek[podaci$Voda_da_afek==9999]<- NA
podaci$Voda_da_afek_ic[podaci$Voda_da_afek_ic==9999]<- NA
podaci$Voda_da_afek_lag[podaci$Voda_da_afek_lag==9999]<- NA
podaci$Voda_da_afek_lag_ic[podaci$Voda_da_afek_lag_ic==9999]<- NA
podaci$Voda_ne_afek[podaci$Voda_ne_afek==9999]<- NA
podaci$Voda_ne_afek_c[podaci$Voda_ne_afek_c==9999]<- NA
podaci$Voda_ne_afek_c_ic[podaci$Voda_ne_afek_c_ic==9999]<- NA
podaci$Voda_ne_afek_c_lag[podaci$Voda_ne_afek_c_lag==9999]<- NA
podaci$Voda_ne_afek_c_lag_ic[podaci$Voda_ne_afek_c_lag_ic==9999]<- NA
podaci$Kafa[podaci$Kafa==9999]<- NA
podaci$Kafa_ikad[podaci$Kafa_ikad==9999]<- NA
podaci$Kafa_koliko[podaci$Kafa_koliko==9999]<- NA
podaci$Kafa_koliko_ic[podaci$Kafa_koliko_ic==9999]<- NA
podaci$Kafa_iskusenje[podaci$Kafa_iskusenje==9999]<- NA
podaci$Kafa_iskusenje_b[podaci$Kafa_iskusenje_b==9999]<- NA
podaci$Kafa_da_afek[podaci$Kafa_da_afek==9999]<- NA
podaci$Kafa_da_afek_ic[podaci$Kafa_da_afek_ic==9999]<- NA
podaci$Kafa_da_afek_lag[podaci$Kafa_da_afek_lag==9999]<- NA
podaci$Kafa_da_afek_lag_ic[podaci$Kafa_da_afek_lag_ic==9999]<- NA
podaci$Kafa_ne_afek[podaci$Kafa_ne_afek==9999]<- NA
podaci$Kafa_ne_afek_c[podaci$Kafa_ne_afek_c==9999]<- NA
podaci$Kafa_ne_afek_c_ic[podaci$Kafa_ne_afek_c_ic==9999]<- NA
podaci$Kafa_ne_afek_c_lag[podaci$Kafa_ne_afek_c_lag==9999]<- NA
podaci$Kafa_ne_afek_c_lag_ic[podaci$Kafa_ne_afek_c_lag_ic==9999]<- NA
podaci$Kafa_iskusenje_ic[podaci$Kafa_iskusenje_ic==9999]<- NA
podaci$Alkohol[podaci$Alkohol==9999]<- NA
podaci$Alkohol_ikad[podaci$Alkohol_ikad==9999]<- NA
podaci$Alkohol_koliko[podaci$Alkohol_koliko==9999]<- NA
podaci$Alkohol_koliko_ic[podaci$Alkohol_koliko_ic==9999]<- NA
podaci$Alkohol_iskusenje[podaci$Alkohol_iskusenje==9999]<- NA
podaci$Alkohol_iskusenje_b[podaci$Alkohol_iskusenje_b==9999]<- NA
podaci$Alkohol_da_afek[podaci$Alkohol_da_afek==9999]<- NA
podaci$Alkohol_da_afek_ic[podaci$Alkohol_da_afek_ic==9999]<- NA
podaci$Alkohol_da_afek_lag[podaci$Alkohol_da_afek_lag==9999]<- NA
podaci$Alkohol_da_afek_lag_ic[podaci$Alkohol_da_afek_lag_ic==9999]<- NA
podaci$Alkohol_ne_afek[podaci$Alkohol_ne_afek==9999]<- NA
podaci$Alkohol_ne_afek_c[podaci$Alkohol_ne_afek_c==9999]<- NA
podaci$Alkohol_ne_afek_c_ic[podaci$Alkohol_ne_afek_c_ic==9999]<- NA
podaci$Alkohol_ne_afek_c_lag[podaci$Alkohol_ne_afek_c_lag==9999]<- NA
podaci$Alkohol_ne_afek_c_lag_ic[podaci$Alkohol_ne_afek_c_lag_ic==9999]<- NA
podaci$Alkohol_iskusenje_ic[podaci$Alkohol_iskusenje_ic==9999]<- NA
podaci$Cigarete[podaci$Cigarete==9999]<- NA
podaci$Cigarete_ikad[podaci$Cigarete_ikad==9999]<- NA
podaci$Cigarete_koliko[podaci$Cigarete_koliko==9999]<- NA
podaci$Cigarete_koliko_ic[podaci$Cigarete_koliko_ic==9999]<- NA
podaci$Cigarete_iskusenje[podaci$Cigarete_iskusenje==9999]<- NA
podaci$Cigarete_iskusenje_b[podaci$Cigarete_iskusenje_b==9999]<- NA
podaci$Cigarete_da_afek[podaci$Cigarete_da_afek==9999]<- NA
podaci$Cigarete_da_afek_ic[podaci$Cigarete_da_afek_ic==9999]<- NA
podaci$Cigarete_da_afek_lag[podaci$Cigarete_da_afek_lag==9999]<- NA
podaci$Cigarete_da_afek_lag_ic[podaci$Cigarete_da_afek_lag_ic==9999]<- NA
podaci$Cigarete_ne_afek[podaci$Cigarete_ne_afek==9999]<- NA
podaci$Cigarete_ne_afek_c[podaci$Cigarete_ne_afek_c==9999]<- NA
podaci$Cigarete_ne_afek_c_ic[podaci$Cigarete_ne_afek_c_ic==9999]<- NA
podaci$Cigarete_ne_afek_c_lag[podaci$Cigarete_ne_afek_c_lag==9999]<- NA
podaci$Cigarete_ne_afek_c_lag_ic[podaci$Cigarete_ne_afek_c_lag_ic==9999]<- NA
podaci$Cigarete_iskusenje_ic[podaci$Cigarete_iskusenje_ic==9999]<- NA
podaci$BrzaHrana[podaci$BrzaHrana==9999]<- NA
podaci$BrzaHrana_ikad[podaci$BrzaHrana_ikad==9999]<- NA
podaci$BrzaHrana_koliko[podaci$BrzaHrana_koliko==9999]<- NA
podaci$BrzaHrana_koliko_ic[podaci$BrzaHrana_koliko_ic==9999]<- NA
podaci$BrzaHrana_iskusenje[podaci$BrzaHrana_iskusenje==9999]<- NA
podaci$BrzaHrana_iskusenje_b[podaci$BrzaHrana_iskusenje_b==9999]<- NA
podaci$BrzaHrana_da_afek[podaci$BrzaHrana_da_afek==9999]<- NA
podaci$BrzaHrana_da_afek_ic[podaci$BrzaHrana_da_afek_ic==9999]<- NA
podaci$BrzaHrana_da_afek_lag[podaci$BrzaHrana_da_afek_lag==9999]<- NA
podaci$BrzaHrana_da_afek_lag_ic[podaci$BrzaHrana_da_afek_lag_ic==9999]<- NA
podaci$BrzaHrana_ne_afek[podaci$BrzaHrana_ne_afek==9999]<- NA
podaci$BrzaHrana_ne_afek_c[podaci$BrzaHrana_ne_afek_c==9999]<- NA
podaci$BrzaHrana_ne_afek_c_ic[podaci$BrzaHrana_ne_afek_c_ic==9999]<- NA
podaci$BrzaHrana_ne_afek_c_lag[podaci$BrzaHrana_ne_afek_c_lag==9999]<- NA
podaci$BrzaHrana_ne_afek_c_lag_ic[podaci$BrzaHrana_ne_afek_c_lag_ic==9999]<- NA
podaci$BrzaHrana_iskusenje_ic[podaci$BrzaHrana_iskusenje_ic==9999]<- NA
podaci$Slatkisi[podaci$Slatkisi==9999]<- NA
podaci$Slatkisi_ikad[podaci$Slatkisi_ikad==9999]<- NA
podaci$Slatkisi_koliko[podaci$Slatkisi_koliko==9999]<- NA
podaci$Slatkisi_koliko_ic[podaci$Slatkisi_koliko_ic==9999]<- NA
podaci$Slatkisi_iskusenje[podaci$Slatkisi_iskusenje==9999]<- NA
podaci$Slatkisi_iskusenje_b[podaci$Slatkisi_iskusenje_b==9999]<- NA
podaci$Slatkisi_da_afek[podaci$Slatkisi_da_afek==9999]<- NA
podaci$Slatkisi_da_afek_ic[podaci$Slatkisi_da_afek_ic==9999]<- NA
podaci$Slatkisi_da_afek_lag[podaci$Slatkisi_da_afek_lag==9999]<- NA
podaci$Slatkisi_da_afek_lag_ic[podaci$Slatkisi_da_afek_lag_ic==9999]<- NA
podaci$Slatkisi_ne_afek[podaci$Slatkisi_ne_afek==9999]<- NA
podaci$Slatkisi_ne_afek_c[podaci$Slatkisi_ne_afek_c==9999]<- NA
podaci$Slatkisi_ne_afek_c_ic[podaci$Slatkisi_ne_afek_c_ic==9999]<- NA
podaci$Slatkisi_ne_afek_c_lag[podaci$Slatkisi_ne_afek_c_lag==9999]<- NA
podaci$Slatkisi_ne_afek_c_lag_ic[podaci$Slatkisi_ne_afek_c_lag_ic==9999]<- NA
podaci$Slatkisi_iskusenje_ic[podaci$Slatkisi_iskusenje_ic==9999]<- NA
podaci$Voce[podaci$Voce==9999]<- NA
podaci$Voce_ikad[podaci$Voce_ikad==9999]<- NA
podaci$Voce_koliko[podaci$Voce_koliko==9999]<- NA
podaci$Voce_koliko_ic[podaci$Voce_koliko_ic==9999]<- NA
podaci$Voce_trebalo[podaci$Voce_trebalo==9999]<- NA
podaci$Voce_da_afek[podaci$Voce_da_afek==9999]<- NA
podaci$Voce_da_afek_ic[podaci$Voce_da_afek_ic==9999]<- NA
podaci$Voce_da_afek_lag[podaci$Voce_da_afek_lag==9999]<- NA
podaci$Voce_da_afek_lag_ic[podaci$Voce_da_afek_lag_ic==9999]<- NA
podaci$Voce_ne_afek[podaci$Voce_ne_afek==9999]<- NA
podaci$Voce_ne_afek_c[podaci$Voce_ne_afek_c==9999]<- NA
podaci$Voce_ne_afek_c_ic[podaci$Voce_ne_afek_c_ic==9999]<- NA
podaci$Voce_ne_afek_c_lag[podaci$Voce_ne_afek_c_lag==9999]<- NA
podaci$Voce_ne_afek_c_lag_ic[podaci$Voce_ne_afek_c_lag_ic==9999]<- NA
podaci$Povrce[podaci$Povrce==9999]<- NA
podaci$Povrce_ikad[podaci$Povrce_ikad==9999]<- NA
podaci$Povrce_koliko[podaci$Povrce_koliko==9999]<- NA
podaci$Povrce_koliko_ic[podaci$Povrce_koliko_ic==9999]<- NA
podaci$Povrce_trebalo[podaci$Povrce_trebalo==9999]<- NA
podaci$Povrce_da_afek[podaci$Povrce_da_afek==9999]<- NA
podaci$Povrce_da_afek_ic[podaci$Povrce_da_afek_ic==9999]<- NA
podaci$Povrce_da_afek_lag[podaci$Povrce_da_afek_lag==9999]<- NA
podaci$Povrce_da_afek_lag_ic[podaci$Povrce_da_afek_lag_ic==9999]<- NA
podaci$Povrce_ne_afek[podaci$Povrce_ne_afek==9999]<- NA
podaci$Povrce_ne_afek_c[podaci$Povrce_ne_afek_c==9999]<- NA
podaci$Povrce_ne_afek_c_ic[podaci$Povrce_ne_afek_c_ic==9999]<- NA
podaci$Povrce_ne_afek_c_lag[podaci$Povrce_ne_afek_c_lag==9999]<- NA
podaci$Povrce_ne_afek_c_lag_ic[podaci$Povrce_ne_afek_c_lag_ic==9999]<- NA
podaci$Vezbanje[podaci$Vezbanje==9999]<- NA
podaci$Vezbanje_ikad[podaci$Vezbanje_ikad==9999]<- NA
podaci$Vezbanje_koliko[podaci$Vezbanje_koliko==9999]<- NA
podaci$Vezbanje_koliko_ic[podaci$Vezbanje_koliko_ic==9999]<- NA
podaci$Vezbanje_trebalo[podaci$Vezbanje_trebalo==9999]<- NA
podaci$Vezbanje_da_afek[podaci$Vezbanje_da_afek==9999]<- NA
podaci$Vezbanje_da_afek_ic[podaci$Vezbanje_da_afek_ic==9999]<- NA
podaci$Vezbanje_da_afek_lag[podaci$Vezbanje_da_afek_lag==9999]<- NA
podaci$Vezbanje_da_afek_lag_ic[podaci$Vezbanje_da_afek_lag_ic==9999]<- NA
podaci$Vezbanje_ne_afek[podaci$Vezbanje_ne_afek==9999]<- NA
podaci$Vezbanje_ne_afek_c[podaci$Vezbanje_ne_afek_c==9999]<- NA
podaci$Vezbanje_ne_afek_c_ic[podaci$Vezbanje_ne_afek_c_ic==9999]<- NA
podaci$Vezbanje_ne_afek_c_lag[podaci$Vezbanje_ne_afek_c_lag==9999]<- NA
podaci$Vezbanje_ne_afek_c_lag_ic[podaci$Vezbanje_ne_afek_c_lag_ic==9999]<- NA
podaci$DodaciIshrani[podaci$DodaciIshrani==9999]<- NA
podaci$DodaciIshrani_ikad[podaci$DodaciIshrani_ikad==9999]<- NA
podaci$DodaciIshrani_koliko[podaci$DodaciIshrani_koliko==9999]<- NA
podaci$DodaciIshrani_koliko_ic[podaci$DodaciIshrani_koliko_ic==9999]<- NA
podaci$DodaciIshrani_trebalo[podaci$DodaciIshrani_trebalo==9999]<- NA
podaci$DodaciIshrani_da_afek[podaci$DodaciIshrani_da_afek==9999]<- NA
podaci$DodaciIshrani_da_afek_ic[podaci$DodaciIshrani_da_afek_ic==9999]<- NA
podaci$DodaciIshrani_da_afek_lag[podaci$DodaciIshrani_da_afek_lag==9999]<- NA
podaci$DodaciIshrani_da_afek_lag_ic[podaci$DodaciIshrani_da_afek_lag_ic==9999]<- NA
podaci$DodaciIshrani_ne_afek[podaci$DodaciIshrani_ne_afek==9999]<- NA
podaci$DodaciIshrani_ne_afek_c[podaci$DodaciIshrani_ne_afek_c==9999]<- NA
podaci$DodaciIshrani_ne_afek_c_ic[podaci$DodaciIshrani_ne_afek_c_ic==9999]<- NA
podaci$DodaciIshrani_ne_afek_c_lag[podaci$DodaciIshrani_ne_afek_c_lag==9999]<- NA
podaci$DodaciIshrani_ne_afek_c_lag_ic[podaci$DodaciIshrani_ne_afek_c_lag_ic==9999]<- NA

# Prebacivanje varijabli iz kategorickog u numericki format.

podaci$unap_trebalo<- as.numeric(podaci$unap_trebalo)
podaci$ugr_iskusenje<- as.numeric(podaci$ugr_iskusenje)
podaci$Desavanja<- as.numeric(podaci$Desavanja)
podaci$Umor<- as.numeric(podaci$Umor)
podaci$Voda<- as.numeric(podaci$Voda)
podaci$Voda_ikad<- as.numeric(podaci$Voda_ikad)
podaci$Voda_koliko<- as.numeric(podaci$Voda_koliko)
podaci$Voda_trebalo<- as.numeric(podaci$Voda_trebalo)
podaci$Voda_da_afek<- as.numeric(podaci$Voda_da_afek)
podaci$Voda_ne_afek<- as.numeric(podaci$Voda_ne_afek)
podaci$Kafa<- as.numeric(podaci$Kafa)
podaci$Kafa_ikad<- as.numeric(podaci$Kafa_ikad)
podaci$Kafa_koliko<- as.numeric(podaci$Kafa_koliko)
podaci$Kafa_iskusenje<- as.numeric(podaci$Kafa_iskusenje)
podaci$Kafa_da_afek<- as.numeric(podaci$Kafa_da_afek)
podaci$Kafa_ne_afek<- as.numeric(podaci$Kafa_ne_afek)
podaci$Alkohol<- as.numeric(podaci$Alkohol)
podaci$Alkohol_ikad<- as.numeric(podaci$Alkohol_ikad)
podaci$Alkohol_koliko<- as.numeric(podaci$Alkohol_koliko)
podaci$Alkohol_iskusenje<- as.numeric(podaci$Alkohol_iskusenje)
podaci$Alkohol_da_afek<- as.numeric(podaci$Alkohol_da_afek)
podaci$Alkohol_ne_afek<- as.numeric(podaci$Alkohol_ne_afek)
podaci$Cigarete<- as.numeric(podaci$Cigarete)
podaci$Cigarete_ikad<- as.numeric(podaci$Cigarete_ikad)
podaci$Cigarete_koliko<- as.numeric(podaci$Cigarete_koliko)
podaci$Cigarete_iskusenje<- as.numeric(podaci$Cigarete_iskusenje)
podaci$Cigarete_da_afek<- as.numeric(podaci$Cigarete_da_afek)
podaci$Cigarete_ne_afek<- as.numeric(podaci$Cigarete_ne_afek)
podaci$BrzaHrana<- as.numeric(podaci$BrzaHrana)
podaci$BrzaHrana_ikad<- as.numeric(podaci$BrzaHrana_ikad)
podaci$BrzaHrana_koliko<- as.numeric(podaci$BrzaHrana_koliko)
podaci$BrzaHrana_iskusenje<- as.numeric(podaci$BrzaHrana_iskusenje)
podaci$BrzaHrana_da_afek<- as.numeric(podaci$BrzaHrana_da_afek)
podaci$BrzaHrana_ne_afek<- as.numeric(podaci$BrzaHrana_ne_afek)
podaci$Slatkisi<- as.numeric(podaci$Slatkisi)
podaci$Slatkisi_ikad<- as.numeric(podaci$Slatkisi_ikad)
podaci$Slatkisi_koliko<- as.numeric(podaci$Slatkisi_koliko)
podaci$Slatkisi_iskusenje<- as.numeric(podaci$Slatkisi_iskusenje)
podaci$Slatkisi_da_afek<- as.numeric(podaci$Slatkisi_da_afek)
podaci$Slatkisi_ne_afek<- as.numeric(podaci$Slatkisi_ne_afek)
podaci$Voce<- as.numeric(podaci$Voce)
podaci$Voce_ikad<- as.numeric(podaci$Voce_ikad)
podaci$Voce_koliko<- as.numeric(podaci$Voce_koliko)
podaci$Voce_trebalo<- as.numeric(podaci$Voce_trebalo)
podaci$Voce_da_afek<- as.numeric(podaci$Voce_da_afek)
podaci$Voce_ne_afek<- as.numeric(podaci$Voce_ne_afek)
podaci$Povrce<- as.numeric(podaci$Povrce)
podaci$Povrce_ikad<- as.numeric(podaci$Povrce_ikad)
podaci$Povrce_koliko<- as.numeric(podaci$Povrce_koliko)
podaci$Povrce_trebalo<- as.numeric(podaci$Povrce_trebalo)
podaci$Povrce_da_afek<- as.numeric(podaci$Povrce_da_afek)
podaci$Povrce_ne_afek<- as.numeric(podaci$Povrce_ne_afek)
podaci$Vezbanje<- as.numeric(podaci$Vezbanje)
podaci$Vezbanje_ikad<- as.numeric(podaci$Vezbanje_ikad)
podaci$Vezbanje_koliko<- as.numeric(podaci$Vezbanje_koliko)
podaci$Vezbanje_trebalo<- as.numeric(podaci$Vezbanje_trebalo)
podaci$Vezbanje_da_afek<- as.numeric(podaci$Vezbanje_da_afek)
podaci$Vezbanje_ne_afek<- as.numeric(podaci$Vezbanje_ne_afek)
podaci$DodaciIshrani<- as.numeric(podaci$DodaciIshrani)
podaci$DodaciIshrani_ikad<- as.numeric(podaci$DodaciIshrani_ikad)
podaci$DodaciIshrani_koliko<- as.numeric(podaci$DodaciIshrani_koliko)
podaci$DodaciIshrani_trebalo<- as.numeric(podaci$DodaciIshrani_trebalo)
podaci$DodaciIshrani_da_afek<- as.numeric(podaci$DodaciIshrani_da_afek)
podaci$DodaciIshrani_ne_afek<- as.numeric(podaci$DodaciIshrani_ne_afek)
podaci$VT_3nr<- as.numeric(podaci$VT_3nr)
podaci$VT_4nr<- as.numeric(podaci$VT_4nr)
podaci$I_1nr<- as.numeric(podaci$I_1nr)
podaci$I_2nr<- as.numeric(podaci$I_2nr)
podaci$I_3nr<- as.numeric(podaci$I_3nr)
podaci$I_7nr<- as.numeric(podaci$I_7nr)
podaci$PU_1nr<- as.numeric(podaci$PU_1nr)
podaci$PU_2nr<- as.numeric(podaci$PU_2nr)
podaci$PU_3nr<- as.numeric(podaci$PU_3nr)
podaci$PU_4nr<- as.numeric(podaci$PU_4nr)
podaci$PolNR<- as.numeric(podaci$PolNR)


# Pozivanje funkcije lmeControl za optimizaciju, odn. povecanje broja pokušaja da se postigne konvergencija modela, koja ce biti ukljucena u sve HLM modele.
ctrl <- lmeControl(opt='optim') 

###########################################################################################################################################.
# H1: Veci procenjeni intenzitet sprovedenog zdravstveno unapredujuceg ponašanja ce biti pozitivno povezan sa intenzitetom prijatnosti afekta.
###########################################################################################################################################.

# Kriterijum: "unap_da_afek"  Afekat povodom izvedenog unapredujuceg ponašanja.
# Prediktor "unap_koliko_ic"  Intenzitet izvedenog unapredujuceg ponašanja, centrirano prema ispitaniku

# Proveravamo da li je potreban HLM tako što se postavlja nulti model (samo intercept) u jednostepenu analizu koji se nakon toga poredi sa HLM-om 
# bira se funkcija gls generalised least squares jer sadrži maximum likelihood koji koristi HLM-u, pa ce analize biti uporedive.

H1_00 <- gls(unap_da_afek ~ 1, # zavisna varijabla je afekat povodom unapredujuceg ponašanja, a prediktora još nema zato stoji 1
			method ="ML", # maximum likelihood metod, da bi bio uporediv sa HLM-om
			na.action=na.exclude, # sve što smo definisali kao ocekivani nedostajuci podatak (dakle, 9999) iskljucujemo iz analize
			data=podaci) # navodi se ime objekta u koji su smešteni podaci
summary(H1_00) # prikaz rezultata

# Postavljanje HLM nultog modela. Takode bez prediktora, samo što je dozvoljeno da intercepti variraju unutar ispitanika.  
H1_0 <- lme(unap_da_afek ~ 1, # zavisna var. je afekat povodom izvedenog unap. ponašanja, a prediktora još nema 
	data=podaci, # set podataka
	method ="ML", # maximum likelihood metod, ako se ovo ne definiše nlme podrazumeva restriktivni maximum likelihood (REML). S obzirom na to da hipoteze prvenstveno govore o direktnim efektima, a ne o proceni udela varijanse, autorei na koje se pozivam prepolucuju LM a ne RELM. 
      na.action=na.exclude, # iskljucujem iz analize sve što je prethodno definisano kao missing (dakle 9999)
	control=ctrl, # prethodno definisana funkcija za optimizaciju
	correlation = corAR1(), # definisanje strukture matrice kovarijansi za autoregresivne korelacije prvog nivoa. Bez ovoga, nlme bi podrazumevao jednostavnu matricu strukture (odn. da su merenja unutar ispitanika nezavisna) i time bi bila povecana mogucnost greške prvog tipa. 
	random = ~ 1 | IdIspitanika) # dozvoljava se da intercepti variraju unutar drugog nivoa, odnosno unutar ispitanika.
summary(H1_0) # štampam rezultat

# Poredenje jednostepenog modela sa HLM-om pokazuje da HLM ima znacajno bolji fit. Znacajna promena u Log Liklihood-u -2LL X2(1)=858.676, p <.0001, pored toga HLM model ima niže i AIC i BIC.
anova(H1_00, H1_0)

# Racunanjem intraklasnih korelacija se dobija da ICC_between = 0.46 i ICC_within= 0.54. 
VarCorr(H1_0) # funkcijom VarCorr iz paketa nlme se racuna varijansa intercepata unutar i izmedu ispitanika
RandomEffects <- as.numeric(VarCorr(H1_0)[,1]) # ispis daje i varijanse i standardne devijacije, ali selektujemo samo varijanse
ICC_between <- RandomEffects[1]/(RandomEffects[1]+RandomEffects[2]) # racuna se udeo varijanse medu ispitanicima u ukupnoj varijansi 
ICC_within <- 1 - ICC_between # procenat varijanse unutar ispitanika se dobija kao komplementarna vrednost
ICC_between
ICC_within

# Uvodimo prediktore: prvo varijablu "Merenje" (redni broj situacije merenja) da bismo kontrolisali varijasu koja je u vezi sa protokom vremena, 
# zatim glavnu prediktorsku varijablu "unap_koliko" koja se odnosi na procenu intenziteta izvedenog unap. ponašanja tokom poslednjih 2 sata, koja je centrirana na nivou ispitanika.
# Prvo postavljam model sa varijabilnim interceptima i dobijam da je intenzitet unapredujuceg ponašanja znacajan prediktor afekta b=0.51, t(1649)= 24.83, p<0,0001 
H1_1 <- lme(unap_da_afek ~ 1 + Merenje + unap_koliko_ic, 
	data=podaci, 
	method ="ML",
      na.action=na.exclude,
	control=ctrl, 
	correlation = corAR1(),
	random = ~ 1  + Merenje | IdIspitanika)
summary(H1_1)

# a zatim, u narednom modelu dozvoljavam da i intercepti i nagibi variraju i takodje dobijam da je glavni prediktor znacajan  b=0.48, t(1649)= 14.08, p<0,0001
H1_2 <- lme(unap_da_afek ~ 1 + Merenje + unap_koliko_ic, 
	data=podaci, 
	method ="ML",
      na.action=na.exclude,
	control=ctrl, 
	correlation = corAR1(),
	random = ~ 1  + Merenje + unap_koliko_ic | IdIspitanika) # u random delu, uvodim prediktore
summary(H1_2)

# poredenje ukazuje da model sa random nagibima ima znacajno bolji fit -2LL X2(1)= 126.811, p <.0001) (i manji AIC i BIC). 
anova(H1_1,H1_2)  

# pravim graficki prikaz povezanosti
ggplot(data=podaci, aes(x=unap_koliko_ic, y=unap_da_afek, group=IdIspitanika , color="gray"), legend=FALSE) + 
  geom_point(color="gray") + guides(color=FALSE) +
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE, lty=1, size=.5, color="gray40") +
  geom_smooth(aes(group=1), method=lm, se=FALSE, fullrange=FALSE, lty=1, size=2, color="red") +
  xlab("Zdravstveno unapredujuce ponašanje") + scale_x_continuous(breaks=seq(0,7,by=1)) +
  ylab("Afekat povodom zdravstveno unapredujuceg ponašanja")  + ylim(1,7) +
  ggtitle("Zdravstveno unapredujuce ponašanje i afekat povodom istog")

# Verovatnoca da na osnovu slucaja dobijemo ovoliki ili veci t statistik je p<0,0001, tako da imamo dovoljno osnova da odbacimo nultu hipotezu i prihvatimo istraživacku hipotezu 
# koja govori da je veci procenjeni intenzitet zdravstveno unapredujuceg ponašanja u prediktor prijatnosti afekta.

#############################	dodatne analize u vezi sa H1.	#################################################################.

# H1. ispitivanje moderatora. 

# Hipotezama nisu eksplicirani rezultati moderacije, ali je u nacrtu navedeno ce biti provereno postojanje efekata efekata moderacije glavnih prediktora sa:
# uobicajenim intenzitetom ponašanja (mereno upitnicima samoprocene)
# procenom umora i povoljnosti dešavanja (“Ono što Vam se dešavalo tokom prethodnih dva sata doživljavate kao...1 Veoma negativno 5 Veoma pozitivno”)
# bazicnim crama licnosti (HEXACO)
# samopoimanjem (SDQ)
# regulatornim fokusom
# napomena: u multilevel analizama smo usmereni na efekte moderacije, a direktna povezanost trajnijih dispozicija sa zdravstvenim ponašanjem su ostavljene za analize na nivou ispitanika (H9 i H10)).

# Prvo je ispitana interakcija procene uobicajenog intenizteta unap. ponašanja (upitnik) centriranog na nivou uzorka (grand mean) i glavnog prediktora (procene intenziteta unap. ponašanja u situaciji - ESM)
# pokazuje se da ova interakcija nije znacajna pa je iskljucujem iz modela. (po istom principu su formirani i naredni modeli. Dakle, interakcije koje se pokažu znacajnim se zadržavaju a ostale se iskljucuju iz modela)
# moderatori se uvode u blokovima i uvek istim redosledom 
# 1. uobicajeno ponašanje, 2. umor i dešavanja, 3. bazicne crte licnosti, 4. samopoimanje i 5. regulatorni fokus.

H1_3 <- lme(unap_da_afek ~ 1 + Merenje + 
	unap_koliko_ic * unap_int_gc, 
	data=podaci, 
	method ="ML",
      na.action=na.exclude,
	control=ctrl, 
	correlation = corAR1(),
	random = ~ 1  + Merenje + unap_koliko_ic | IdIspitanika)
summary(H1_3)

# uvodenje procene stanja umora i pogodnosti dešavanja (centrirane na nivou ispitanika) pokazuju znacajnu interkaciju percepcije dešavanja sa glavnim prediktorom  b=0.07, t(1645)= 3.32, p=0,0009 
# Dakle, što su dešavanja u širem kontekstu pozitivnija, to je izraženija povezanst unap. ponašanja sa pozitivnim afektom.
H1_4 <- lme(unap_da_afek ~ 1 + Merenje + 
	unap_koliko_ic * Desavanja_ic + 
	unap_koliko_ic * Umor_ic, 
	data=podaci, 
	method ="ML",
      na.action=na.exclude,
	control=ctrl, 
	correlation = corAR1(),
	random = ~ 1  + Merenje + unap_koliko_ic | IdIspitanika)
summary(H1_4)

# fukcija describe paketa psych je upotrebljena za dekriptivnu statistiku varijabli
describe(podaci$unap_koliko_ic) 
describe(podaci$Desavanja_ic)

# Funkcijom effect paketa effects ispitujemo moderaciju
ef.H1_4 <- effect(term="unap_koliko_ic*Desavanja_ic", mod=H1_4,  
              xlevels=list(unap_koliko_ic=c(-0.85, +0.85), Desavanja_ic=c(-1.06,+1.06)))  
summary(ef.H1_4)

# ispis funkcije effect je smešten u data freame format da bi dalje mogogao da se koristi za graficki prikaz moderacije
ef.H1_4 <- as.data.frame(ef.H1_4)                   

ef.H1_4

# graficki prikaz moderacije
ggplot(data=ef.H1_4, aes(x=unap_koliko_ic, y=fit, group=Desavanja_ic), legend=TRUE) + 
  geom_point() +
  geom_line() +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.15) +
  xlab("Unapedujuce ponašanje") + xlim(-1,1) +
  ylab("Predideni afekat povodom zdravstveno unapredujuceg ponašanja") + ylim(4.25,5.75) +
  ggtitle("Predikcija afekta na osnovu izvedenog ponašanja - moderirana procenom povoljnosti dešavanja")

# u modelu su zadržana dešavanja (jer su se pokazala znacajnim moderatorom) i dodajem bazicne crte licnosti
H1_5 <- lme(unap_da_afek ~ 1 + Merenje +  
	unap_koliko_ic*Desavanja_ic+ 
	unap_koliko_ic*HonestyHumility_gc +                                                                                                                                                 
	unap_koliko_ic*Emotionality_gc + 
	unap_koliko_ic*Extraversion_gc +                                                                                                                                 
	unap_koliko_ic*Agreeableness_gc + 
	unap_koliko_ic*Conscientiousness_gc + 
	unap_koliko_ic*Openness_gc,
	data=podaci, 
	method ="ML",
      na.action=na.exclude,
	control=ctrl, 
	correlation = corAR1(),
	random = ~ 1  + Merenje + unap_koliko_ic | IdIspitanika)
summary(H1_5)

# Pokazuje se znacajna interakcija glavnog prediktora sa saradljivošcu. b=-0.13, t(1641)= -2.74, p=0,0061
# Povezanost izmedu unapredujuceg ponašanja i afekta je izraženija kod onih sa nižim skoroivma saradjivosti 
# Na niskom nivou izvedenog unapredujuceg ponašanja visoko saradljivi raportiraju znatno viši nivo zadovoljstva povodom tog ponašanja od nisko saradljivih. 

describe(podaci$unap_koliko_ic)
describe(podaci$Agreeableness_gc)

ef.H1_5 <- effect(term="unap_koliko_ic*Agreeableness_gc", mod=H1_5, 
              xlevels=list(unap_koliko_ic=c(-0.85, +0.85), Agreeableness_gc=c(-0.71, +0.71)))
summary(ef.H1_5)

ef.H1_5 <- as.data.frame(ef.H1_5)
ef.H1_5

# graficki prikazujem moderacije
ggplot(data=ef.H1_5, aes(x=as.numeric(unap_koliko_ic), y=fit, group=Agreeableness_gc), legend=TRUE) + 
  geom_point() +
  geom_line() +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.15) +
  xlab("Unapedujuce ponašanje") + xlim(-1,1) +
  ylab("Predvideni afekat povodom zdravstveno unapredujuceg ponašanja") + ylim(4.25,5.75) +
  ggtitle("Predikcija afekta na osnovu izvedenog ponašanja - moderirana saradljivošcu")

H1_6 <- lme(unap_da_afek ~ 1 + Merenje +  
	unap_koliko_ic*Desavanja_ic+ 
	unap_koliko_ic*Agreeableness_gc +
	unap_koliko_ic*SDQPhAbilities_gc + 
	unap_koliko_ic*SDQPhAppereance_gc + 
	unap_koliko_ic*SDQGenEsteem_gc,
	data=podaci, 
	method ="ML",
      na.action=na.exclude,
	control=ctrl, 
	correlation = corAR1(),
	random = ~ 1  + Merenje + unap_koliko_ic | IdIspitanika)
summary(H1_6)

H1_7 <- lme(unap_da_afek ~ 1 + Merenje +  
	unap_koliko_ic*Desavanja_ic+ 
	unap_koliko_ic*Agreeableness_gc +
	unap_koliko_ic*PrevFokus_gc + 
	unap_koliko_ic*PromoFokus_gc,
	data=podaci, 
	method ="ML",
      na.action=na.exclude,
	control=ctrl, 
	correlation = corAR1(),
	random = ~ 1  + Merenje + unap_koliko_ic | IdIspitanika)
summary(H1_7)

H1_final <- lme(unap_da_afek ~ 1 + Merenje +  
	unap_koliko_ic*Desavanja_ic+ 
	unap_koliko_ic*Agreeableness_gc,
	data=podaci, 
	method ="ML",
      na.action=na.exclude,
	control=ctrl, 
	correlation = corAR1(),
	random = ~ 1  + Merenje + unap_koliko_ic | IdIspitanika)
summary(H1_final)

# H1. ispitivanje pojedinacnih ponašanja. 

# Napomena: To je uradeno samo sa osnovnim modelom, bez modracije (S obzirom na obim analiza, misam siguran da li ima potrebe ispitivati moderaciju i za pojedinacna ponašanja) 
# osnovni model se pokazuje znacajnim za sva ispitivana unap. ponašanja, osim za dodatke ishrani gde model ne postiže konvergenciju
# voda b=0.51, t(1499)= 12.74, p<0,0001
# voce b=0.26, t(495)= 5.04, p<0,0001
# povrce b=0.35, t(500)= 7.40, p<0,0001
# vežbanje b=0.32, t(282)= 5.33, p<0,0001

H1_8 <- lme(Voda_da_afek ~ 1 + Merenje + Voda_koliko_ic, 
	data=podaci, 
	method ="ML",
      na.action=na.exclude,
	control=ctrl, 
	correlation = corAR1(),
	random = ~ 1  + Merenje + Voda_koliko_ic | IdIspitanika)
summary(H1_8)

H1_9 <- lme(Voce_da_afek ~ 1 + Merenje + Voce_koliko_ic, 
	data=podaci, 
	method ="ML",
      na.action=na.exclude,
	control=ctrl, 
	correlation = corAR1(),
	random = ~ 1  + Merenje + Voce_koliko_ic | IdIspitanika)
summary(H1_9)

H1_10 <- lme(Povrce_da_afek ~ 1 + Merenje + Povrce_koliko_ic, 
	data=podaci, 
	method ="ML",
      na.action=na.exclude,
	control=ctrl, 
	correlation = corAR1(),
	random = ~ 1  + Merenje + Povrce_koliko_ic | IdIspitanika)
summary(H1_10)

H1_11 <- lme(Vezbanje_da_afek ~ 1 + Merenje + Vezbanje_koliko_ic, 
	data=podaci, 
	method ="ML",
      na.action=na.exclude,
	control=ctrl, 
	correlation = corAR1(),
	random = ~ 1  + Merenje + Vezbanje_koliko_ic | IdIspitanika)
summary(H1_11)

H1_12 <- lme(DodaciIshrani_da_afek ~ 1 + Merenje + DodaciIshrani_koliko_ic, 
	data=podaci, 
	method ="ML",
      na.action=na.exclude,
	control=ctrl, 
	correlation = corAR1(),
	random = ~ 1 + Merenje + DodaciIshrani_koliko_ic| IdIspitanika)
summary(H1_12)

#####################################################################################################################################################.
# H2: Zdravstveno unapredujuce ponašanje koje nije izvedeno, a procenjuje se da je trebalo da bude izvedeno, bice povezano sa neprijatnim afektom.
#####################################################################################################################################################.

# Kriterijum: "unap_ne_afek_c"  Afekat povodom neizvedenog unapredujuceg, ako to ikad rade (oznaka "c" je od cisto, jer sam za one koji se nikada ne bave nekom aktivnošcu odgovor zamenio system missing-om).
# Prediktor: "unap_trebalo" Da li osoba smatra da je trebalo da izvede odredeno zdravstveno unapredujuce ponašanje.
 
# Proveravam da li mi je potreban HLM poredeci jednostepeni nulti model sa HLM nultim modelom, i dobijam da HLM ima bolji fit (-2LL X2(1)=1580.364, p <.0001) 

H2_00 <- gls(unap_ne_afek_c ~ 1, 
			method ="ML", 
			na.action=na.exclude, 
			data=podaci) 
summary(H2_00)

H2_0 <- lme(unap_ne_afek_c ~ 1, 
	data=podaci, 
	method ="ML",
      na.action=na.exclude,
	control=ctrl, 
	correlation = corAR1(),
	random = ~ 1 | IdIspitanika)
summary(H2_0)
anova(H2_00, H2_0)

# Ispitujem intraklasne korelacije i dobijam da je ICC_between= 0.60, a ICC_within= 0.40

VarCorr(H2_0)
RandomEffects <- as.numeric(VarCorr(H2_0)[,1])
ICC_between <- RandomEffects[1]/(RandomEffects[1]+RandomEffects[2]) 
ICC_within <- 1 - ICC_between
ICC_between
ICC_within

# Uvodim  kontrolnu varijablu "merenje" i kao glavnu prediktorsku binarnu varijablu "unap_trebalo" i dobijam da i model sa slobodnim interceptom i model sa slobodnim interceptom i nagibom, glavni prediktor pokazuju znacajnim, 
# Model sa slobodnim intrceptima i nagibima ima bolji fit (-2LL X2(1)= 104.43, p <.0001) pa uzimam njegovu vrednost kao relevantnu b=-0.56, t(1862)= -9.49, p<0,0001 . 

H2_1 <- lme(unap_ne_afek_c ~ 1 + Merenje + unap_trebalo, 
	data=podaci, 
	method ="ML",
      na.action=na.exclude,
	control=ctrl, 
	correlation = corAR1(),
	random = ~ 1 | IdIspitanika)
summary(H2_1)

H2_2 <- lme(unap_ne_afek_c ~ 1 + Merenje + unap_trebalo, 
	data=podaci, 
	method ="ML",
      na.action=na.exclude,
	control=ctrl, 
	correlation = corAR1(),
	random = ~ 1  + Merenje + unap_trebalo | IdIspitanika)
summary(H2_2)
anova(H2_1, H2_2)

# graficki prikazujem povezanost
ggplot(data=podaci, aes(x=unap_trebalo, y=unap_ne_afek_c, group=IdIspitanika , color="gray"), legend=FALSE) + 
  geom_point(color="gray") + guides(color=FALSE) +
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE, lty=1, size=.5, color="gray40") +
  geom_smooth(aes(group=1), method=lm, se=FALSE, fullrange=FALSE, lty=1, size=2, color="red") +
  xlab("Da li je bilo potrebno da se izvede zdravstveno unapredujuce ponašanje?") + scale_x_continuous(breaks=seq(1,2,by=1)) +
  ylab("Afekat povodom neizvodenja unapredujuceg ponašanja")  + ylim(1,7) +
  ggtitle("Neizvedeno zdravstveno unapredujuce ponašanje i afekat povodom istog")

# Verovatnoca da na osnovu slucaja dobijemo ovoliki ili veci t statistik je p<0,0001, tako da imamo dovoljno osnova da odbacimo nultu hipotezu i prihvatimo istraživacku hipotezu 
# koja govori da je zdravstveno unapredujuce ponašanje koje nije izvedeno, a procenjuje se da je trebalo da bude izvedeno, povezano sa neprijatnim afektom.

#############################	dodatne analize u vezi sa H2.	#################################################################.

# H2 ispitivanje moderatora. 

# Interakcija glavnog prediktora i uobicajenog unapredjujuceg ponašanja, centriramo prema grupi (grand mean) se pokazuje znacajnim 
# b=-0.25, t(1862)= -2.26, p<0,0235 i zakljucujemo da oni koji inace sprovode više zdravstveno unapredujuceg ponašanja, u situaciji kada ne izvedu unapredujuce ponašanje
# a smatraju da je trebalo, osecaju se gore nego oni koji uobicajeno sprovode manje zdravstveno unapredujuceg ponašanja.

H2_3 <- lme(unap_ne_afek_c ~ 1 + Merenje + 
	unap_trebalo*unap_int_gc, 
	data=podaci, 
	method ="ML",
      na.action=na.exclude,
	control=ctrl, 
	correlation = corAR1(),
	random = ~ 1  + Merenje + unap_trebalo | IdIspitanika)
summary(H2_3)

describe(podaci$unap_trebalo)
describe(podaci$unap_int_gc)

ef.H2_3 <- effect(term="unap_trebalo*unap_int_gc", mod=H2_3, 
              xlevels=list(unap_trebalo=c(-0.49, +0.49), unap_int_gc=c(-0.52,+0.52)))
summary(ef.H2_3)

ef.H2_3 <- as.data.frame(ef.H2_3)
ef.H2_3

# graficki prikaz moderatora
ggplot(data=ef.H2_3, aes(x=unap_trebalo, y=fit, group=unap_int_gc), legend=TRUE) + 
  geom_point() +
  geom_line() +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.15) +
  xlab("Neizvedeno unapedujuce ponašanje") + xlim(-1,1)  + 
  ylab("Predvideni afekat povodom neizvedenog zdravstveno unapredujuceg ponašanja") + ylim(4,6) +
  ggtitle("Predikcija afekta na osnovu neizvedenog ponašanja - moderiran uobicajenim unapredujucim ponašanem")

H2_4 <- lme(unap_ne_afek_c ~ 1 + Merenje + 
	unap_trebalo*unap_int_gc+
	unap_trebalo*Desavanja_ic+
	unap_trebalo*Umor_ic, 
	data=podaci, 
	method ="ML",
      na.action=na.exclude,
	control=ctrl, 
	correlation = corAR1(),
	random = ~ 1  + Merenje + unap_trebalo | IdIspitanika)
summary(H2_4)

H2_5 <- lme(unap_ne_afek_c ~ 1 + Merenje + 
	unap_trebalo*unap_int_gc+ 
	unap_trebalo*HonestyHumility_gc +                                                                                                                                                 
	unap_trebalo*Emotionality_gc + 
	unap_trebalo*Extraversion_gc +                                                                                                                                 
	unap_trebalo*Agreeableness_gc + 
	unap_trebalo*Conscientiousness_gc + 
	unap_trebalo*Openness_gc, 
	data=podaci, 
	method ="ML",
      na.action=na.exclude,
	control=ctrl, 
	correlation = corAR1(),
	random = ~ 1  + Merenje + unap_trebalo | IdIspitanika)
summary(H2_5)

H2_6 <- lme(unap_ne_afek_c ~ 1 + Merenje + 
	unap_trebalo*unap_int_gc+                                                                                                                                                  
	unap_trebalo*SDQPhAbilities_gc +                                                                                                                                 
	unap_trebalo*SDQPhAppereance_gc + 
	unap_trebalo*SDQGenEsteem_gc, 
	data=podaci, 
	method ="ML",
      na.action=na.exclude,
	control=ctrl, 
	correlation = corAR1(),
	random = ~ 1  + Merenje + unap_trebalo | IdIspitanika)
summary(H2_6)

H2_7 <- lme(unap_ne_afek_c ~ 1 + Merenje +   
	unap_trebalo*unap_int_gc +                                                                                                                                                 
	unap_trebalo*PrevFokus_gc +                                                                                                                                 
	unap_trebalo*PromoFokus_gc, 
	data=podaci, 
	method ="ML",
      na.action=na.exclude,
	control=ctrl, 
	correlation = corAR1(),
	random = ~ 1  + Merenje + unap_trebalo | IdIspitanika)
summary(H2_7)

H2_final <- lme(unap_ne_afek_c ~ 1 + Merenje +   
	unap_trebalo*unap_int_gc, 
	data=podaci, 
	method ="ML",
      na.action=na.exclude,
	control=ctrl, 
	correlation = corAR1(),
	random = ~ 1  + Merenje + unap_trebalo | IdIspitanika)
summary(H2_final)

# H2 Ispitivanje pojedinacnih ponašanja	

# Istraživacka H2 se potvrduje i na svim pojedinacnim zdravstveno unapredujucim ponašanjima.

# Voda b=-1.01, t(340)= -6.18, p<0,0001
# Voce b=-0.96, t(1207)= -11.50, p<0,0001
# Povrce b=-0.81, t(1231)= -9.48, p<0,0001
# Vežbanje b=-1.13, t(1285)= -10.55, p<0,0001
# Dodaci ishrani b=-1.26, t(714)= -6.30, p<0,0001

H2_8 <- lme(Voda_ne_afek_c ~ 1 + Merenje + Voda_trebalo, 
	data=podaci, 
	method ="ML",
      na.action=na.exclude,
	control=ctrl, 
	correlation = corAR1(),
	random = ~ 1  + Merenje + Voda_trebalo | IdIspitanika)
summary(H2_8)

H2_9 <- lme(Voce_ne_afek_c ~ 1 + Merenje + Voce_trebalo, 
	data=podaci, 
	method ="ML",
      na.action=na.exclude,
	control=ctrl, 
	correlation = corAR1(),
	random = ~ 1  + Merenje + Voce_trebalo | IdIspitanika)
summary(H2_9)

H2_10 <- lme(Povrce_ne_afek_c ~ 1 + Merenje + Povrce_trebalo, 
	data=podaci, 
	method ="ML",
      na.action=na.exclude,
	control=ctrl, 
	correlation = corAR1(),
	random = ~ 1  + Merenje + Povrce_trebalo | IdIspitanika)
summary(H2_10)

H2_11 <- lme(Vezbanje_ne_afek_c ~ 1 + Merenje + Vezbanje_trebalo, 
	data=podaci, 
	method ="ML",
      na.action=na.exclude,
	control=ctrl, 
	correlation = corAR1(),
	random = ~ 1  + Merenje + Vezbanje_trebalo | IdIspitanika)
summary(H2_11)-10.

H2_12 <- lme(DodaciIshrani_ne_afek_c ~ 1 + Merenje + DodaciIshrani_trebalo, 
	data=podaci, 
	method ="ML",
  	na.action=na.exclude,
	control=ctrl, 
	correlation = corAR1(),
	random = ~ 1  + Merenje + DodaciIshrani_trebalo | IdIspitanika)
summary(H2_12)

# Eksplorativne analize u vezi sa H2.
# želim da isptam koje karakteristike ispitanika i situacije predvidaju intenzitet afekta povodom propuštenog unapredujuceg ponašanja
# Selektujem samo one koji smatraju da je trebalo da izvedu odredeno ponašanje i ispitujem prediktore. Uvodim ih istim redosledom kako kada testiram moderatore.

podaci_unap_trebalo <- podaci[which(podaci$unap_trebalo_lag == 1), ]

H2a_1 <- lme(unap_ne_afek_c ~ 1 + Merenje + unap_int_gc, 
	data=podaci_unap_trebalo, 
	method ="ML",
      na.action=na.exclude,
	control=ctrl, 
	correlation = corAR1(),
	random = ~ 1  + Merenje | IdIspitanika)
summary(H2a_1)

# Procena povoljnosti dešavanja se pokazuje znacajnim prediktorom (u modelu sa varijabilnim interceptom i nagibom, koji ima bolji fit -2LL X2(1)=14.959, p =.0365) 
# tako da, što su povoljnije ocenjene okolnosti u kojima smo propustili da izvedemo unapredujuce ponašanje to cemo se povodom toga osecati lošije b=-0.11, t(1009)= -4.92, p<0,0001

H2a_2 <- lme(unap_ne_afek_c ~ 1 + Merenje + Desavanja_ic + Umor_ic, 
	data=podaci_unap_trebalo, 
	method ="ML",
      na.action=na.exclude,
	control=ctrl, 
	correlation = corAR1(),
	random = ~ 1  + Merenje | IdIspitanika)
summary(H2a_2)

H2a_2r <- lme(unap_ne_afek_c ~ 1 + Merenje + Desavanja_ic + Umor_ic, 
	data=podaci_unap_trebalo, 
	method ="ML",
      na.action=na.exclude,
	control=ctrl, 
	correlation = corAR1(),
	random = ~ 1  + Merenje + Desavanja_ic + Umor_ic | IdIspitanika)
summary(H2a_2r)
anova(H2a_2,H2a_2r)

# Kada uvedem crte licnosti dobijam da su ekstraverzija b=0.27, t(89)= 2.75, p<0,0073 i savestnost b=0.27, t(89)= 2.46, p<0,0158 znacajni pozitivni prediktori toga 
# koliko ce se neko dobro osecati u situaciji kada je propustio da uradi nešto što je trebalo.

H2a_3 <- lme(unap_ne_afek_c ~ 1 + Merenje + Desavanja_ic + HonestyHumility_gc + Emotionality_gc + Extraversion_gc + Agreeableness_gc + Conscientiousness_gc + Openness_gc, 
	data=podaci_unap_trebalo, 
	method ="ML",
      na.action=na.exclude,
	control=ctrl, 
	correlation = corAR1(),
	random = ~ 1  + Merenje + Desavanja_ic | IdIspitanika)
summary(H2a_3)

H2a_4 <- lme(unap_ne_afek_c ~ 1 + Merenje + Desavanja_ic + Extraversion_gc + Conscientiousness_gc + SDQPhAbilities_gc + SDQPhAppereance_gc + SDQGenEsteem_gc, 
	data=podaci_unap_trebalo, 
	method ="ML",
      na.action=na.exclude,
	control=ctrl, 
	correlation = corAR1(),
	random = ~ 1  + Merenje + Desavanja_ic | IdIspitanika)
summary(H2a_4)

H2a_5 <- lme(unap_ne_afek_c ~ 1 + Merenje + Desavanja_ic + Extraversion_gc + Conscientiousness_gc + PrevFokus_gc + PromoFokus_gc, 
	data=podaci_unap_trebalo, 
	method ="ML",
      na.action=na.exclude,
	control=ctrl, 
	correlation = corAR1(),
	random = ~ 1  + Merenje + Desavanja_ic | IdIspitanika)
summary(H2a_5)

H2a_final <- lme(unap_ne_afek_c ~ 1 + Merenje + Desavanja_ic + Extraversion_gc + Conscientiousness_gc, 
	data=podaci_unap_trebalo, 
	method ="ML",
      na.action=na.exclude,
	control=ctrl, 
	correlation = corAR1(),
	random = ~ 1  + Merenje + Desavanja_ic | IdIspitanika)
summary(H2a_final)

ggplot(data=podaci_unap_trebalo, aes(x=Desavanja_ic, y=unap_ne_afek_c, group=IdIspitanika , color="gray"), legend=FALSE) + 
  geom_point(color="gray") + guides(color=FALSE) +
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE, lty=1, size=.5, color="gray40") +
  geom_smooth(aes(group=1), method=lm, se=FALSE, fullrange=FALSE, lty=1, size=2, color="red") +
  xlab("Procena povoljnosti dešavanja") + scale_x_continuous(breaks=seq(-4,4,by=1)) +
  ylab("Afekat povodom neizvodenja unapredujuceg ponašanja")  + ylim(1,7) +
  ggtitle("Afekat povodom neizvedenog unapredujuceg ponašanja i procena povoljnosti dešavanja")

ggplot(data=podaci_unap_trebalo, aes(x=Extraversion_gc, y=unap_ne_afek_c, color="gray"), legend=FALSE) + 
  geom_point(color="gray") + guides(color=FALSE) +
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE, lty=1, size=.5, color="gray40") +
  geom_smooth(aes(group=1), method=lm, se=FALSE, fullrange=FALSE, lty=1, size=2, color="red") +
  xlab("Ekstraverzija") + scale_x_continuous(breaks=seq(-2,2,by=1)) +
  ylab("Afekat povodom neizvodenja unapredujuceg ponašanja")  + ylim(1,7) +
  ggtitle("Afekat povodom neizvedenog unapredujuceg ponašanja i ekstraverzija")

ggplot(data=podaci_unap_trebalo, aes(x=Conscientiousness_gc, y=unap_ne_afek_c, color="gray"), legend=FALSE) + 
  geom_point(color="gray") + guides(color=FALSE) +
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE, lty=1, size=.5, color="gray40") +
  geom_smooth(aes(group=1), method=lm, se=FALSE, fullrange=FALSE, lty=1, size=2, color="red") +
  xlab("Savesnost") + scale_x_continuous(breaks=seq(-2,2,by=1)) +
  ylab("Afekat povodom neizvodenja unapredujuceg ponašanja")  + ylim(1,7) +
  ggtitle("Afekat povodom neizvedenog unapredujuceg ponašanja i savesnost")


#############################################################################################################################.
# H3: Veci procenjeni intenzitet sprovedenog zdravstveno ugrožavajuceg ponašanja ce biti povezan sa neprijatnim afektom.
#############################################################################################################################.

# Glavna zavisna "ugr_da_afek" Afekat povodom izvedenog ugrožavajuceg  ponašanja.
# Glavni prediktor "ugr_koliko_ic" Intenzitet izvedenog ugrožavajuceg ponašanja, centrirano prema ispitaniku

# Proveravam da li mi je potreban HLM, i dobijam rezultat koji ukazuje na bolji fit HLM modela -2LL X2(1)=605.212, p <.0001
 
H3_00 <- gls(ugr_da_afek ~ 1,  
	method ="ML",
	na.action=na.exclude,
      data=podaci)
summary(H3_00)

H3_0 <- lme(ugr_da_afek ~ 1, 
	data=podaci, 
	method ="ML",
      na.action=na.exclude,
	control=ctrl, 
	correlation = corAR1(),
	random = ~ 1 | IdIspitanika)
summary(H3_0)
anova(H3_00, H3_0)

# Delim varijansu zavisne varijable i dobijam  ICC_between=0.47 a ICC_within=0.53, što takode ukazuje na potrebu za HLM-om. 

VarCorr(H3_0)
RandomEffects <- as.numeric(VarCorr(H3_0)[,1])
ICC_between <- RandomEffects[1]/(RandomEffects[1]+RandomEffects[2]) 
ICC_within <- 1 - ICC_between
ICC_between
ICC_within

# Model sa varijabilnim interceptom pokazuje glavnu prediktorsku varijablu znacajnom b=-0.09, t(1187)= -3.30, p=0,0010, 
# ali u modelu sa varijabilnim interceptom i nagibom se ne pokazuje znacajnim.
# Poredenje modela ukazuje na to da kompleksniji model (varijabilni nagib i intercept) ima bolji fit -2LL X2(1)=30.600, p <.0001
# tako da zakljucujem da H3 nije potvrdena i ne idem dalje sa ispitivanjem moderacije, ali ispitujem hipotezu na pojedinacnim ponašanjima.
   
H3_1 <- lme(ugr_da_afek ~ 1 + Merenje + ugr_koliko_ic, 
	data=podaci, 
	method ="ML",
      na.action=na.exclude,
	control=ctrl, 
	correlation = corAR1(),
	random = ~ 1  + Merenje | IdIspitanika)
summary(H3_1)

H3_2 <- lme(ugr_da_afek ~ 1 + Merenje + ugr_koliko_ic, 
	data=podaci, 
	method ="ML",
      na.action=na.exclude,
	control=ctrl, 
	correlation = corAR1(),
	random = ~ 1  + Merenje + ugr_koliko_ic | IdIspitanika)
summary(H3_2)
anova(H3_1, H3_2)

#############################	 dodatne analize u vezi sa H3. #################################################################.

# ispitivanje pojedinacnih ponašanja.

# kafa - nije znacajna
# alkohol - nije znacajan
# cigarete b=-0.27, t(354)= -3.89, p<0,0001
# brza hrana - nije znacajna
# slatkiši b=-0.08, t(522)= -2.25, p=0,0248

H3_8 <- lme(Kafa_da_afek ~ 1 + Merenje + Kafa_koliko_ic, 
	data=podaci, 
	method ="ML",
      na.action=na.exclude,
	control=ctrl, 
	correlation = corAR1(),
	random = ~ 1  + Merenje + Kafa_koliko_ic | IdIspitanika)
summary(H3_8)

H3_9 <- lme(Alkohol_da_afek ~ 1 + Merenje + Alkohol_koliko_ic, 
	data=podaci, 
	method ="ML",
      na.action=na.exclude,
	control=ctrl, 
	correlation = corAR1(),
	random = ~ 1  + Merenje + Alkohol_koliko_ic | IdIspitanika)
summary(H3_9)

H3_10 <- lme(Cigarete_da_afek ~ 1 + Merenje + Cigarete_koliko_ic, 
	data=podaci, 
	method ="ML",
      na.action=na.exclude,
	control=ctrl, 
	correlation = corAR1(),
	random = ~ 1  + Merenje + Cigarete_koliko_ic | IdIspitanika)
summary(H3_10)
‚

# graficki prikazujem povezanost
ggplot(data=podaci, aes(x=Cigarete_koliko_ic, y=Cigarete_da_afek, group=IdIspitanika , color="gray"), legend=FALSE) + 
  geom_point(color="gray") + guides(color=FALSE) +
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE, lty=1, size=.5, color="gray40") +
  geom_smooth(aes(group=1), method=lm, se=FALSE, fullrange=FALSE, lty=1, size=2, color="red") +
  xlab("Subjektivna procena inenziteta pušenja") + scale_x_continuous(breaks=seq(-3,3,by=1)) +
  ylab("Afekat povodom pušenja")  + ylim(1,7) +
  ggtitle("Povezanost pušenja i afekta povodom pušenja")

H3_10a <- lme(Cigarete_da_afek ~ 1 + Merenje +  
	Cigarete_koliko_ic * Desavanja_ic + 
	Cigarete_koliko_ic * Umor_ic,
	data=podaci, 
	method ="ML",
      na.action=na.exclude,
	control=ctrl, 
	correlation = corAR1(),
	random = ~ 1  + Merenje + Cigarete_koliko_ic | IdIspitanika)
summary(H3_10a)

H3_10b <- lme(Cigarete_da_afek ~ 1 + Merenje +  
	Cigarete_koliko_ic * HonestyHumility_gc + 
	Cigarete_koliko_ic * Emotionality_gc + 
	Cigarete_koliko_ic * Extraversion_gc + 
	Cigarete_koliko_ic * Agreeableness_gc + 
	Cigarete_koliko_ic * Conscientiousness_gc + 
	Cigarete_koliko_ic * Openness_gc,
	data=podaci, 
	method ="ML",
      na.action=na.exclude,
	control=ctrl, 
	correlation = corAR1(),
	random = ~ 1  + Merenje + Cigarete_koliko_ic | IdIspitanika)
summary(H3_10b)




H3_11 <- lme(BrzaHrana_da_afek ~ 1 + Merenje + BrzaHrana_koliko_ic, 
	data=podaci, 
	method ="ML",
      na.action=na.exclude,
	control=ctrl, 
	correlation = corAR1(),
	random = ~ 1  + Merenje + BrzaHrana_koliko_ic | IdIspitanika)
summary(H3_11)

H3_12 <- lme(Slatkisi_da_afek ~ 1 + Merenje + Slatkisi_koliko_ic, 
	data=podaci, 
	method ="ML",
      na.action=na.exclude,
	control=ctrl, 
	correlation = corAR1(),
	random = ~ 1  + Merenje + Slatkisi_koliko_ic | IdIspitanika)
summary(H3_12)

ggplot(data=podaci, aes(x=Slatkisi_koliko_ic, y=Slatkisi_da_afek, group=IdIspitanika , color="gray"), legend=FALSE) + 
  geom_point(color="gray") + guides(color=FALSE) +
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE, lty=1, size=.5, color="gray40") +
  geom_smooth(aes(group=1), method=lm, se=FALSE, fullrange=FALSE, lty=1, size=2, color="red") +
  xlab("Subjektivna procena inenziteta konzumacije slatkiša") + scale_x_continuous(breaks=seq(-3,3,by=1)) +
  ylab("Afekat povodom konzumacije slatkiša")  + ylim(1,7) +
  ggtitle("Povezanost konzumacije slatkiša i afekta povodom konzumacije slatkiša")

#############################################################################################################################.
# H4: Stepen iskušenja pod kojim zdravstveno ugrožavajuce ponašanje nije izvedeno bice povezano sa pozitivnim afektom.
#############################################################################################################################.

# Glavna zavisna "ugr_ne_afek_c" Afekat povodom neizvedenog ugrožavajuceg ponašanja, kod osoba za koje smo evidentirali da ikada upražnjavaju ta ponašanja.
# Glavni prediktor "ugr_iskusenje_b" Da li je postojalo iskušenje?.

# proveravam da li je potreban HLM, dobijam da jeste jer model pokazuje bolji fit -2LL X2(1)=2140.903, p <.0001

H4_00 <- gls(ugr_ne_afek_c ~ 1, 
			method ="ML", 
			na.action=na.exclude, 
			data=podaci) 
summary(H4_00)

H4_0 <- lme(ugr_ne_afek_c ~ 1, 
	data=podaci, 
	method ="ML",
      na.action=na.exclude,
	control=ctrl, 
	correlation = corAR1(),
	random = ~ 1 | IdIspitanika)
summary(H4_0)
anova(H4_00, H4_0)

# racunam intraklasne korelacije, i dobijam da  ICC_between = 0.68 i ICC_within= 0.32
VarCorr(H4_0) 
RandomEffects <- as.numeric(VarCorr(H4_0)[,1]) 
ICC_between <- RandomEffects[1]/(RandomEffects[1]+RandomEffects[2]) 
ICC_within <- 1 - ICC_between 
ICC_between
ICC_within

# oba modela pokazuju da je stepen iskušenja znacajan prediktor afekta, ali u suprotnom smeru od onog koji je ocekivan sa H4
# Kompleksniji model (varijabilni intercept i nagib) ne pokazuje unapredenje fita, tako da prihvatam jednostavniji (varijabilni intercept) b=-0.18, t(577)= -3.47, p=0,0006

H4_1 <- lme(ugr_ne_afek_c ~ 1 + Merenje + ugr_iskusenje_b, 
	data=podaci, 
	method ="ML",
      na.action=na.exclude,
	control=ctrl, 
	correlation = corAR1(),
	random = ~ 1 + Merenje | IdIspitanika)
summary(H4_1)

H4_2 <- lme(ugr_ne_afek_c ~ 1 + Merenje + ugr_iskusenje_b, 
	data=podaci, 
	method ="ML",
      na.action=na.exclude,
	control=ctrl, 
	correlation = corAR1(),
	random = ~ 1  + Merenje + ugr_iskusenje_b | IdIspitanika)
summary(H4_2)
anova(H4_1, H4_2)

# graficki prikaz povezanosti
ggplot(data=podaci, aes(x=ugr_iskusenje_b, y=ugr_ne_afek_c, group=IdIspitanika , color="gray"), legend=FALSE) + 
  geom_point(color="gray") + guides(color=FALSE) +
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE, lty=1, size=.5, color="gray40") +
  geom_smooth(aes(group=1), method=lm, se=FALSE, fullrange=FALSE, lty=1, size=2, color="red") +
  xlab("Postojanje iskušenja") + scale_x_continuous(breaks=seq(-2,2,by=1)) +
  ylab("Afekat povodom neizvedenog ugrožavajuceg ponašanja")  + ylim(1,7) +
  ggtitle("Postojanje iskušenja i afekat povodom neizvedenog ponašanja")

#############################	 dodatne analize u vezi sa H4. #################################################################.

# Ispitianje moderatora.

H4_3 <- lme(ugr_ne_afek_c ~ 1 + Merenje + ugr_iskusenje_b +
	ugr_iskusenje_b*ugr_int_gc, 
	data=podaci, 
	method ="ML",
      na.action=na.exclude,
	control=ctrl, 
	correlation = corAR1(),
	random = ~ 1  + Merenje | IdIspitanika)
summary(H4_3)

H4_4 <- lme(ugr_ne_afek_c ~ 1 + Merenje + ugr_iskusenje_b +
	ugr_iskusenje_b*Desavanja_ic +
	ugr_iskusenje_b*Umor_ic,  
	data=podaci, 
	method ="ML",
      na.action=na.exclude,
	control=ctrl, 
	correlation = corAR1(),
	random = ~ 1  + Merenje | IdIspitanika)
summary(H4_4)

# Uvodenje bazicnih crta licnosti otkriva da Emocionalnost u statisticki znacajnoj meri b=0.17, t(571)= 2.02, p=0,0437
# moderira odnos izmedu stepena izloženosti iskušenju i afekta povodom toga što ponašanje nije izvedeno. 
# Naime, kod onih sa niskim skorovima na emocionalnosti, u situaciji iskušenja se beleži strmiji pad prijatnosti afekta nego kod onih sa višim skorovima emocionalnosti.

H4_5 <- lme(ugr_ne_afek_c ~ 1 + Merenje + ugr_iskusenje_b +
	ugr_iskusenje_b*HonestyHumility_gc +
	ugr_iskusenje_b*Emotionality_gc +
	ugr_iskusenje_b*Extraversion_gc +
	ugr_iskusenje_b*Agreeableness_gc +
	ugr_iskusenje_b*Conscientiousness_gc +
	ugr_iskusenje_b*Openness_gc,  
	data=podaci, 
	method ="ML",
      na.action=na.exclude,
	control=ctrl, 
	correlation = corAR1(),
	random = ~ 1  + Merenje | IdIspitanika)
summary(H4_5)

describe(podaci$ugr_iskusenje_b)
describe(podaci$Agreeableness_gc)

ef.H4_5 <- effect(term="ugr_iskusenje_b*Agreeableness_gc", mod=H4_5, 
              xlevels=list(ugr_iskusenje_b=c(-0.47, +0.47), Agreeableness_gc=c(-0.71, +0.71)))
summary(ef.H4_5)

ef.H4_5 <- as.data.frame(ef.H4_5)
ef.H4_5

# graficki prikaz moderacije
ggplot(data=ef.H4_5, aes(x=as.numeric(ugr_iskusenje_b), y=fit, group=Agreeableness_gc), legend=TRUE) + 
  geom_point() +
  geom_line() +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.15) +
  xlab("Iskušenje da se izvede ugrožavajuce ponašanje") + xlim(-1,1) +
  ylab("Afekat povodom neizvodenja ugrožavajuceg ponašanja") + ylim(5,6) +
  ggtitle("Predikcija afekta na osnovu ponašanja - moderirana crtom saradljivosti")


H4_6 <- lme(ugr_ne_afek_c ~ 1 + Merenje + ugr_iskusenje_b +
	ugr_iskusenje_b*Agreeableness_gc +
	ugr_iskusenje_b*SDQPhAbilities_gc +
	ugr_iskusenje_b*SDQPhAppereance_gc +
	ugr_iskusenje_b*SDQGenEsteem_gc, 
	data=podaci, 
	method ="ML",
      na.action=na.exclude,
	control=ctrl, 
	correlation = corAR1(),
	random = ~ 1  + Merenje + ugr_iskusenje_b | IdIspitanika)
summary(H4_6)

H4_7 <- lme(ugr_ne_afek_c ~ 1 + Merenje + ugr_iskusenje_b +
	ugr_iskusenje_b*Agreeableness_gc +
	ugr_iskusenje_b*PrevFokus_gc +
	ugr_iskusenje_b*PromoFokus_gc, 
	data=podaci, 
	method ="ML",
      na.action=na.exclude,
	control=ctrl, 
	correlation = corAR1(),
	random = ~ 1  + Merenje + ugr_iskusenje_b | IdIspitanika)
summary(H4_7)

H4_final <- lme(ugr_ne_afek_c ~ 1 + Merenje + ugr_iskusenje_b +
	ugr_iskusenje_b*Agreeableness_gc, 
	data=podaci, 
	method ="ML",
      na.action=na.exclude,
	control=ctrl, 
	correlation = corAR1(),
	random = ~ 1  + Merenje + ugr_iskusenje_b | IdIspitanika)
summary(H4_final)

# Ispitianje pojedinacnih ponašanja.

# pokazuje znacajnim prediktore u slucaju:
# kafe b=-0.24, t(211)= -2.38, p=0,0180
# brze hrane b=-0.58, t(159)= -2.89, p=0,0043
# slatkiše b=-0.31, t(205)= -2.18, p=0,0304
# alkohol - ne postiže konvergenciju
# cigarete - nije znacajno

H4_8 <- lme(Kafa_ne_afek_c ~ 1 + Merenje + Kafa_iskusenje_b, 
	data=podaci, 
	method ="ML",
      na.action=na.exclude,
	control=ctrl, 
	correlation = corAR1(),
	random = ~ 1  + Merenje + Kafa_iskusenje_b | IdIspitanika)
summary(H4_8)

H4_9 <- lme(Alkohol_ne_afek_c ~ 1 + Merenje + Alkohol_iskusenje_b, 
	data=podaci, 
	method ="ML",
      na.action=na.exclude,
	control=ctrl, 
	correlation = corAR1(),
	random = ~ 1  + Merenje + Alkohol_iskusenje_b | IdIspitanika)
summary(H4_9)

H4_10 <- lme(Cigarete_ne_afek_c ~ 1 + Merenje + Cigarete_iskusenje_b, 
	data=podaci, 
	method ="ML",
      na.action=na.exclude,
	control=ctrl, 
	correlation = corAR1(),
	random = ~ 1  + Merenje + Cigarete_iskusenje_b | IdIspitanika)
summary(H4_10)

H4_11 <- lme(BrzaHrana_ne_afek_c ~ 1 + Merenje + BrzaHrana_iskusenje_b, 
	data=podaci, 
	method ="ML",
      na.action=na.exclude,
	control=ctrl, 
	correlation = corAR1(),
	random = ~ 1  + Merenje + BrzaHrana_iskusenje_b | IdIspitanika)
summary(H4_11)

H4_12 <- lme(Slatkisi_ne_afek_c ~ 1 + Merenje + Slatkisi_iskusenje_b, 
	data=podaci, 
	method ="ML",
      na.action=na.exclude,
	control=ctrl, 
	correlation = corAR1(),
	random = ~ 1  + Merenje + Slatkisi_iskusenje_b | IdIspitanika)
summary(H4_12)

# Eksplorativne analize u vezi sa H4.
# želimo da isptamo koje karakteristike ispitanika i situacije predvidaju intenzitet afekta povodom neizvoðenja ponašanja u situaciji iskušenja.
# Selektujemo samo one koji su bili u iskušenju i ispitujem prediktore. Uvodimo ih istim redosledom kako kada testiram moderatore.
# Za prediktore na prvom nivou merenja testiramo model varijabilnog intercepta nasuprot modelu varijabilnog intercepta i nagiba.

podaci_ugr_iskusenje <- podaci[which(podaci$ugr_iskusenje_b == 1), ]
dim (podaci_ugr_iskusenje)

H4a_1 <- lme(ugr_ne_afek_c ~ 1 + Merenje + ugr_int_gc, 
	data=podaci_ugr_iskusenje, 
	method ="ML",
      na.action=na.exclude,
	control=ctrl, 
	correlation = corAR1(),
	random = ~ 1  + Merenje | IdIspitanika)
summary(H4a_1)

# Procena povoljnosti dešavanja se pokazuje znacajnim prediktorom u oba modela, ali usložnjavanje modela (dopuštanje variranja nagiba) ne poboljšava znacajno fit 
# tako da se zadržavam na modelu sa varijabilnim interceptima koji sugeriše da što su dešavanja pozitivnija to ce afekat povodom neizvodenja zdravstveno ugrožavajuceg ponašanja biti lošiji
# b=-0.10, t(576)= -4.09, p<0,0001

H4a_2 <- lme(ugr_ne_afek_c ~ 1 + Merenje + Desavanja_ic + Umor_ic, 
	data=podaci_ugr_iskusenje, 
	method ="ML",
      na.action=na.exclude,
	control=ctrl, 
	correlation = corAR1(),
	random = ~ 1  + Merenje | IdIspitanika)
summary(H4a_2)

H4a_2r <- lme(ugr_ne_afek_c ~ 1 + Merenje + Desavanja_ic + Umor_ic, 
	data=podaci_ugr_iskusenje, 
	method ="ML",
      na.action=na.exclude,
	control=ctrl, 
	correlation = corAR1(),
	random = ~ 1  + Merenje + Desavanja_ic + Umor_ic | IdIspitanika)
summary(H4a_2r)
anova (H4a_2, H4a_2r)

# Ispitivanje bazicnih crta licnosti otkriva da su Savesnost  b=0.31, t(77)= 2.13, p=0,0367 i Emocionalnost b=0.30, t(77)= 2.01, p=0,0481 znacajni prediktori prijatnosti afekta u situaciji kada osoba odoli iskušenju.

H4a_3 <- lme(ugr_ne_afek_c ~ 1 + Merenje + Desavanja_ic +  HonestyHumility_gc + Emotionality_gc + Extraversion_gc + Agreeableness_gc + Conscientiousness_gc + Openness_gc, 
	data=podaci_ugr_iskusenje, 
	method ="ML",
      na.action=na.exclude,
	control=ctrl, 
	correlation = corAR1(),
	random = ~ 1  + Merenje | IdIspitanika)
summary(H4a_3)

# Uvodenje karakteristika samopoimanja otkriva da je prercepcija fizickih sposobnosti b=0.26, t(78)= 2.58, p=0,0117 prediktor pozitivnog afekta povodom neizvodenja ugrožavajuceg ponašanja.

H4a_4 <- lme(ugr_ne_afek_c ~ 1 + Merenje + Desavanja_ic + Emotionality_gc + Conscientiousness_gc + SDQPhAbilities_gc + SDQPhAppereance_gc + SDQGenEsteem_gc, 
	data=podaci_ugr_iskusenje, 
	method ="ML",
      na.action=na.exclude,
	control=ctrl, 
	correlation = corAR1(),
	random = ~ 1  + Merenje | IdIspitanika)
summary(H4a_4)

H4a_5 <- lme(ugr_ne_afek_c ~ 1 + Merenje + Desavanja_ic + Emotionality_gc + Conscientiousness_gc + SDQPhAbilities_gc + PrevFokus_gc + PromoFokus_gc, 
	data=podaci_ugr_iskusenje, 
	method ="ML",
      na.action=na.exclude,
	control=ctrl, 
	correlation = corAR1(),
	random = ~ 1  + Merenje | IdIspitanika)
summary(H4a_5)

H4a_final <- lme(ugr_ne_afek_c ~ 1 + Merenje + Desavanja_ic + Emotionality_gc + Conscientiousness_gc + SDQPhAbilities_gc, 
	data=podaci_ugr_iskusenje, 
	method ="ML",
      na.action=na.exclude,
	control=ctrl, 
	correlation = corAR1(),
	random = ~ 1  + Merenje | IdIspitanika)
summary(H4a_final)

ggplot(data=podaci_ugr_iskusenje, aes(x=Desavanja_ic, y=ugr_ne_afek_c, group=IdIspitanika, color="gray"), legend=FALSE) + 
  geom_point(color="gray") + guides(color=FALSE) +
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE, lty=1, size=.5, color="gray40") +
  geom_smooth(aes(group=1), method=lm, se=FALSE, fullrange=FALSE, lty=1, size=2, color="red") +
  xlab("Povoljnost dešavanja") + scale_x_continuous(breaks=seq(-3,4,by=1)) +
  ylab("Afekat povodom neizvodenja ugrožavajuceg ponašanja")  + ylim(1,7) +
  ggtitle("Afekat povodom neizvodenja ugrožavajuceg ponašanja i povoljnost dešavanja")

ggplot(data=podaci_ugr_iskusenje, aes(x=Emotionality_gc, y=ugr_ne_afek_c, color="gray"), legend=FALSE) + 
  geom_point(color="gray") + guides(color=FALSE) +
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE, lty=1, size=.5, color="gray40") +
  geom_smooth(aes(group=1), method=lm, se=FALSE, fullrange=FALSE, lty=1, size=2, color="red") +
  xlab("Emocionalnost") + scale_x_continuous(breaks=seq(-3,3,by=1)) +
  ylab("Afekat povodom neizvodenja ugrožavajuceg ponašanja")  + ylim(1,7) +
  ggtitle("Afekat povodom neizvodenja ugrožavajuceg ponašanja i emocionalnost")

ggplot(data=podaci_ugr_iskusenje, aes(x=Conscientiousness_gc, y=ugr_ne_afek_c, color="gray"), legend=FALSE) + 
  geom_point(color="gray") + guides(color=FALSE) +
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE, lty=1, size=.5, color="gray40") +
  geom_smooth(aes(group=1), method=lm, se=FALSE, fullrange=FALSE, lty=1, size=2, color="red") +
  xlab("Savesnost") + scale_x_continuous(breaks=seq(-3,3,by=1)) +
  ylab("Afekat povodom neizvodenja ugrožavajuceg ponašanja")  + ylim(1,7) +
  ggtitle("Afekat povodom neizvodenja ugrožavajuceg ponašanja i savesnost")

ggplot(data=podaci_ugr_iskusenje, aes(x=SDQPhAbilities_gc, y=ugr_ne_afek_c, color="gray"), legend=FALSE) + 
  geom_point(color="gray") + guides(color=FALSE) +
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE, lty=1, size=.5, color="gray40") +
  geom_smooth(aes(group=1), method=lm, se=FALSE, fullrange=FALSE, lty=1, size=2, color="red") +
  xlab("Percepcija fizicke sposobnosti") + scale_x_continuous(breaks=seq(-3,3,by=1)) +
  ylab("Afekat povodom neizvodenja ugrožavajuceg ponašanja")  + ylim(1,7) +
  ggtitle("Afekat povodom neizvodenja ugrožavajuceg ponašanja i percepcija fizicke sposobnosti")

###################################################################################################################################################################.
# H5: Prijatnost povodom izvedenog zdravstveno unapredujuceg ponašanja bice prediktor povecanja procenjenog zdravstveno unapredujuceg ponašanja u narednoj situaciji.
###################################################################################################################################################################.

# glavni kriterijum "unap_koliko" procenjeni intenzitet izvedenog unapredujuceg
# glavni prediktor "unap_da_afek_lag_ic" -  Afekat povodom izvedenog unapredujuceg ponašanja u prethodnoj situaciji, centrirano prema ispitaniku

# proveravam da li je neophodan HLM i dobijam da HLM model ima znacajno bolji fit -2LL X2(1)= 170.945, p <.0001

H5_00 <- gls(unap_koliko ~ 1, 
		  method ="ML", 
              data=podaci,
              na.action=na.exclude)
summary(H5_00)

H5_0 <- lme(unap_koliko ~ 1,
		  method ="ML", 
              data=podaci,
              random= ~ 1|IdIspitanika, 
              na.action=na.exclude)
summary(H5_0)
anova(H5_00, H5_0) 

# racunam intraklasne korelacije i dobijam da je ICC_between = 0.16 a ICC_within = 0.83
VarCorr(H5_0)
RandomEffects <- as.numeric(VarCorr(H5_0)[,1])
ICC_between <- RandomEffects[1]/(RandomEffects[1]+RandomEffects[2]) 
ICC_within <- 1 - ICC_between
ICC_between
ICC_within

# Prediktor se ne pokazuje znacajnim ni u jednom od modela tako da H5 odbacujemo.

H5_1 <- lme(unap_koliko ~ 1 + Merenje + unap_da_afek_lag_ic, 
              random= ~ 1 + Merenje |IdIspitanika, 
		  method ="ML",
              correlation = corAR1(),
              data=podaci,
		  control=ctrl,
              na.action=na.exclude)
summary(H5_1)

H5_2 <- lme(unap_koliko ~ 1 + Merenje + unap_da_afek_lag_ic, 
              random= ~ 1 + Merenje + unap_da_afek_lag_ic |IdIspitanika, 
		  method ="ML",
              correlation = corAR1(),
              data=podaci,
		  control=ctrl,
              na.action=na.exclude)
summary(H5_2)
anova(H5_1, H5_2) 

#############################	 dodatne analize u vezi sa H5. #################################################################.

# H5a predvidam unapredujuce ponašanje na osnovu afekta povodom izbegavanja ugrožavajuceg ponašanja u prethodnoj situaciji
# kriterijum: "unap_koliko" Intenzitet izvedenog unapredujuceg ponašanja.
# prediktor: "ugr_ne_afek_c_lag_ic" Afekat povodom prethodno neizvedenog ugrožavajuceg, ako ikad rade, centrirano prema ispitaniku
# dobijam da ni jedan od modela nije statisticki znacajan.

H5a_1 <- lme(unap_koliko ~ 1 + Merenje + ugr_ne_afek_c_lag_ic, 
              random= ~ 1 + Merenje |IdIspitanika, 
		  method ="ML",
              correlation = corAR1(),
              data=podaci,
		  control=ctrl,
              na.action=na.exclude)
summary(H5a_1)

H5a_2 <- lme(unap_koliko ~ 1 + Merenje + ugr_ne_afek_c_lag_ic, 
              random= ~ 1 + Merenje + ugr_ne_afek_c_lag_ic |IdIspitanika, 
		  method ="ML",
              correlation = corAR1(),
              data=podaci,
		  control=ctrl,
              na.action=na.exclude)
summary(H5a_2)
anova(H5a_1, H5a_2)

# H5b predvidam unapredujuce ponašanje na osnovu afekta povodom pozitivnog ponašanja (izvodenja unapredujuceg i izbegavanja ugrožavajuceg) ponašanja u prethodnoj situaciji
# kriterijum: "unap_koliko" Intenzitet izvedenog unapredujuceg ponašanja.
# prediktor: "poz_afek_lag_ic" Afekat povodom izvodenja unapredujuceg i izbegavanja ugrožavajuceg u prethodnom merenju, centrirano prema ispitaniku
# dobijam da ni jedan od modela nije statisticki znacajan 

H5b_1 <- lme(unap_koliko ~ 1 + Merenje + poz_afek_lag_ic, 
              random= ~ 1 + Merenje |IdIspitanika, 
		  method ="ML",
              correlation = corAR1(),
              data=podaci,
		  control=ctrl,
              na.action=na.exclude)
summary(H5b_1)

H5b_2 <- lme(unap_koliko ~ 1 + Merenje + poz_afek_lag_ic, 
              random= ~ 1 + Merenje + poz_afek_lag_ic |IdIspitanika, 
		  method ="ML",
              correlation = corAR1(),
              data=podaci,
		  control=ctrl,
              na.action=na.exclude)
summary(H5b_2)
anova(H5b_1, H5b_2)

# H5c predvidam unapredujuce ponašanje na osnovu afekta povodom neizvodenja unapredujuceg ponašanja u prethodnoj situaciji
# kriterijum: "unap_koliko" Intenzitet izvedenog unapredujuceg ponašanja.
# prediktor: "unap_ne_afek_c_lag_ic"   Afekat povodom prethodno neizvedenog unapredujuceg, ako to ikad rade, centrirano prema ispitaniku
# dobijam da je model sa varijabilnim interceptom znacajan b=0.08, t(1506)= 2.54, p<0,0111 (istovremeno ima i bolji fit od modela sa varijabilnim nagibom)


podaci_unap_trebalo <- podaci[which(podaci$unap_trebalo_lag == 1), ]


H5c_1 <- lme(unap_koliko ~ 1 + Merenje + unap_ne_afek_c_lag_ic, 
              random= ~ 1 + Merenje |IdIspitanika, 
		  method ="ML",
              correlation = corAR1(),
              data=podaci_unap_trebalo,
		  control=ctrl,
              na.action=na.exclude)
summary(H5c_1)

H5c_2 <- lme(unap_koliko ~ 1 + Merenje + unap_ne_afek_c_lag_ic, 
              random= ~ 1 + Merenje + unap_ne_afek_c_lag_ic |IdIspitanika, 
		  method ="ML",
              correlation = corAR1(),
              data=podaci_unap_trebalo,
		  control=ctrl,
              na.action=na.exclude)
summary(H5c_2)
anova(H5c_1, H5c_2)

ggplot(data=podaci, aes(x=unap_ne_afek_c_lag_ic, y=unap_koliko, group=IdIspitanika, color="gray"), legend=FALSE) + 
  geom_point(color="gray") + guides(color=FALSE) +
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE, lty=1, size=.5, color="gray40") +
  geom_smooth(aes(group=1), method=lm, se=FALSE, fullrange=FALSE, lty=1, size=2, color="red") +
  xlab("Afekat povodom neizvodenja unapredujuceg ponašanja u prethodnoj situaciji") + scale_x_continuous(breaks=seq(-3,4,by=1)) +
  ylab("Unapredujuce ponašanje")  + ylim(1,7) +
  ggtitle("Afekat povodom neizvodenja unapredujuceg u prethodnoj i unapredujuce ponašanje u aktuelnoj situaciji")

# H5d predvidam unapredujuce ponašanje na osnovu afekta povodom izvodenja ugrožavajuceg ponašanja u prethodnoj situaciji
# kriterijum: "unap_koliko" Intenzitet izvedenog unapredujuceg ponašanja.
# prediktor: "ugr_da_afek_lag_ic"   Afekat povodom prethodno izvedenog ugrožavajuceg  ponašanja., centrirano prema ispitaniku
# dobijam da je model sa varijabilnim interceptom znacajan b=0.08, t(935)= 2.66, p<0,0080 

H5d_1 <- lme(unap_koliko ~ 1 + Merenje + ugr_da_afek_lag_ic, 
              random= ~ 1 + Merenje |IdIspitanika, 
		  method ="ML",
              correlation = corAR1(),
              data=podaci,
		  control=ctrl,
              na.action=na.exclude)
summary(H5d_1)

H5d_2 <- lme(unap_koliko ~ 1 + Merenje + ugr_da_afek_lag_ic, 
              random= ~ 1 + Merenje + ugr_da_afek_lag_ic |IdIspitanika, 
		  method ="ML",
              correlation = corAR1(),
              data=podaci,
		  control=ctrl,
              na.action=na.exclude)
summary(H5d_2)
anova(H5d_1, H5d_2)

ggplot(data=podaci, aes(x=ugr_da_afek_lag_ic, y=unap_koliko, group=IdIspitanika, color="gray"), legend=FALSE) + 
  geom_point(color="gray") + guides(color=FALSE) +
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE, lty=1, size=.5, color="gray40") +
  geom_smooth(aes(group=1), method=lm, se=FALSE, fullrange=FALSE, lty=1, size=2, color="red") +
  xlab("Afekat povodom izvodenja ugrožavajuceg ponašanja u prethodnoj situaciji") + scale_x_continuous(breaks=seq(-3,4,by=1)) +
  ylab("Unapredujuce ponašanje")  + ylim(1,7) +
  ggtitle("Afekat povodom izvodenja ugrožavajuceg u prethodnoj i unapredujuce ponašanje u aktuelnoj situaciji")


# H5e predvidam unapredujuce ponašanje na osnovu afekta povodom izvodenja negativnog ponašanja (propuštanja unapredujuceg, izvodenja ugrožavajuceg) u prethodnoj situaciji
# kriterijum: "unap_koliko" Intenzitet izvedenog unapredujuceg ponašanja.
# prediktor: "neg_afek_lag_ic"   Afekat povodom izvodenja ugrožavajuceg i propuštanja unapredujuceg ponašanja u prethodnom merenju, centrirano prema ispitaniku
# dobijam da je model sa varijabilnim interceptom znacajan b=0.10, t(1551)= 3.21, p<0,0014 (takode ima i bolji fit, nego mode sa varijabilnim interceptom i nagibom)

H5e_1 <- lme(unap_koliko ~ 1 + Merenje + neg_afek_lag_ic, 
              random= ~ 1 + Merenje |IdIspitanika, 
		  method ="ML",
              correlation = corAR1(),
              data=podaci,
		  control=ctrl,
              na.action=na.exclude)
summary(H5e_1)

H5e_2 <- lme(unap_koliko ~ 1 + Merenje + neg_afek_lag_ic, 
              random= ~ 1 + Merenje + neg_afek_lag_ic |IdIspitanika, 
		  method ="ML",
              correlation = corAR1(),
              data=podaci,
		  control=ctrl,
              na.action=na.exclude)
summary(H5e_2)
anova(H5e_1, H5e_2)

ggplot(data=podaci, aes(x=neg_afek_lag_ic, y=unap_koliko, group=IdIspitanika, color="gray"), legend=FALSE) + 
  geom_point(color="gray") + guides(color=FALSE) +
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE, lty=1, size=.5, color="gray40") +
  geom_smooth(aes(group=1), method=lm, se=FALSE, fullrange=FALSE, lty=1, size=2, color="red") +
  xlab("Afekat povodom negativnog ponašanja u prethodnoj situaciji") + scale_x_continuous(breaks=seq(-3,4,by=1)) +
  ylab("Unapredujuce ponašanje")  + ylim(1,7) +
  ggtitle("Afekat povodom negativnog ponašanja u prethodnoj i unapredujuce ponašanje u aktuelnoj situaciji")


# H5f predvidam unapredujuce ponašanje na osnovu ukupnog afekta (prosek afekta povodom pozitivnog i negativnog ponašanja) u prethodnoj situaciji
# kriterijum: "unap_koliko" Intenzitet izvedenog unapredujuceg ponašanja.
# prediktor: "total_afek_lag_ic" Ukupan afekat u prethodnom mrenju, centrirano prema ispitaniku
# dobijam da je model sa varijabilnim interceptom znacajan b=0.08, t(1574)= 2.38, p<0,0176 (takode ima i bolji fit)

H5f_1 <- lme(unap_koliko ~ 1 + Merenje + total_afek_lag_ic, 
              random= ~ 1 + Merenje|IdIspitanika, 
		  method ="ML",
              correlation = corAR1(),
              data=podaci,
		  control=ctrl,
              na.action=na.exclude)
summary(H5f_1)

H5f_2 <- lme(unap_koliko ~ 1 + Merenje + total_afek_lag_ic, 
              random= ~ 1 + Merenje + total_afek_lag_ic |IdIspitanika, 
		  method ="ML",
              correlation = corAR1(),
              data=podaci,
		  control=ctrl,
              na.action=na.exclude)
summary(H5f_2)
anova(H5f_1, H5f_2)

ggplot(data=podaci, aes(x=total_afek_lag_ic, y=unap_koliko, group=IdIspitanika, color="gray"), legend=FALSE) + 
  geom_point(color="gray") + guides(color=FALSE) +
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE, lty=1, size=.5, color="gray40") +
  geom_smooth(aes(group=1), method=lm, se=FALSE, fullrange=FALSE, lty=1, size=2, color="red") +
  xlab("Afekat povodom ukupnog ponašanja u prethodnoj situaciji") + scale_x_continuous(breaks=seq(-3,4,by=1)) +
  ylab("Unapredujuce ponašanje")  + ylim(1,7) +
  ggtitle("Afekat povodom ukupnog ponašanja u prethodnoj i unapredujuce ponašanje u aktuelnoj situaciji")

###################################################################################################################################################################.
# H6: Prijatnost povodom izbegavanja zdravstveno ugrožavajuceg ponašanja bice prediktor smanjenja procenjenog zdravstveno ugrožavajuceg ponašanja u narednoj situaciji.
###################################################################################################################################################################.

# glavni kriterijum "unap_koliko" procenjeni intenzitet izvedenog unapredujuceg
# glavni prediktor "unap_da_afek_lag_ic" -  Afekat povodom izvedenog unapredujuceg ponašanja u prethodnoj situaciji, centrirano prema ispitaniku

# proveravam da li je neophodan HLM i dobijam da HLM model ima znacajno bolji fit -2LL X2(1)= 170.945, p <.0001

H6_00 <- gls(ugr_koliko ~ 1, 
		  method ="ML", 
              data=podaci,
              na.action=na.exclude)
summary(H6_00)

H6_0 <- lme(ugr_koliko ~ 1,
		  method ="ML", 
              data=podaci,
              random= ~ 1|IdIspitanika, 
              na.action=na.exclude)
summary(H6_0)
anova(H6_00, H6_0) 

# racunam intraklasne korelacije i dobijam da je ICC_between = 0.28 a ICC_within = 0.72
VarCorr(H6_0)
RandomEffects <- as.numeric(VarCorr(H6_0)[,1])
ICC_between <- RandomEffects[1]/(RandomEffects[1]+RandomEffects[2]) 
ICC_within <- 1 - ICC_between
ICC_between
ICC_within



# kriterijum: "ugr_koliko" Intenzitet izvedenog ugrožavajuceg ponašanja.
# prediktor: "ugr_ne_afek_c_lag_ic" Afekat povodom prethodno neizvedenog ugrožavajuceg, ako ikad rade., centrirano prema ispitaniku
# Ni jedan od modela ne pokazuje prediktor znacajnim, što je i ocekivano s obzirom na to da se prilikom testiranja H4 pokazalo da izbegavanje ugrožavajuceg ponašanja ne vodi prijatnom afektu.

H6_1 <- lme(ugr_koliko ~ 1 + Merenje + ugr_ne_afek_c_lag_ic, 
              random= ~ 1 + Merenje |IdIspitanika, 
		  method ="ML",
              correlation = corAR1(),
              data=podaci,
		  control=ctrl,
              na.action=na.exclude)
summary(H6_1)

H6_2 <- lme(ugr_koliko ~ 1 + Merenje + ugr_ne_afek_c_lag_ic, 
              random= ~ 1 + Merenje + ugr_ne_afek_c_lag_ic |IdIspitanika, 
		  method ="ML",
              correlation = corAR1(),
              data=podaci,
		  control=ctrl,
              na.action=na.exclude)
summary(H6_2)
anova(H6_1, H6_2)

#############################	 dodatne analize u vezi sa H6. #################################################################.

# H6a predvidam ugrozavajuce ponašanje na osnovu afekta povodom izvodenja unapredujuceg ponašanja u prethodnoj situaciji.
# kriterijum:"ugr_koliko" Intenzitet izvedenog ugrožavajuceg ponašanja.
# prediktor: "unap_da_afek_lag_ic"  Afekat povodom prethodno izvedenog unapredujuceg ponašanja, centrirano prema ispitaniku
# Ni jedan od modela ne pokazuje prediktor znacajnim

H6a_1 <- lme(ugr_koliko ~ 1 + Merenje + unap_da_afek_lag_ic, 
              random= ~ 1 + Merenje |IdIspitanika, 
		  method ="ML",
              correlation = corAR1(),
              data=podaci,
		  control=ctrl,
              na.action=na.exclude)
summary(H6a_1)

H6a_2 <- lme(ugr_koliko ~ 1 + Merenje + unap_da_afek_lag_ic, 
              random= ~ 1 + Merenje + unap_da_afek_lag_ic |IdIspitanika, 
		  method ="ML",
              correlation = corAR1(),
              data=podaci,
		  control=ctrl,
              na.action=na.exclude)
summary(H6a_2)
anova(H6a_1, H6a_2)

# H6b predvidam ugrožavajuce ponašanje na osnovu afekta povodom pozitivnog ponašanja (izvodenja unapredujuceg i izbegavanja ugrožavajuceg) ponašanja u prethodnoj situaciji
# kriterijum: "ugr_koliko"  Intenzitet izvedenog ugrožavajuceg ponašanja.
# prediktor:"poz_afek_lag_ic" Afekat povodom izvodenja unapredujuceg i izbegavanja ugrožavajuceg u prethodnom merenju, centrirano prema ispitaniku
# ni jedan od modela nije statisticki znacajan.

H6b_1 <- lme(ugr_koliko ~ 1 + Merenje + poz_afek_lag_ic, 
              random= ~ 1 + Merenje |IdIspitanika, 
		  method ="ML",
              correlation = corAR1(),
              data=podaci,
		  control=ctrl,
              na.action=na.exclude)
summary(H6b_1)

H6b_2 <- lme(ugr_koliko ~ 1 + Merenje + poz_afek_lag_ic, 
              random= ~ 1 + Merenje + poz_afek_lag_ic |IdIspitanika, 
		  method ="ML",
              correlation = corAR1(),
              data=podaci,
		  control=ctrl,
              na.action=na.exclude)
summary(H6b_2)
anova(H6b_1, H6b_2)

# H6c predvidam ugrožavajuce ponašanje na osnovu afekta povodom neizvodenja unapredujuceg ponašanja u prethodnoj situaciji
# kriterijum: "ugr_koliko" Intenzitet izvedenog ugrožavajuceg ponašanja.
# prediktor:"unap_ne_afek_c_lag_ic" Afekat povodom prethodno neizvedenog unapredujuceg, ako to ikad rade, centrirano prema ispitaniku
# ni jedan od modela nije statisticki znacajan.

H6c_1 <- lme(ugr_koliko ~ 1 + Merenje + unap_ne_afek_c_lag_ic, 
              random= ~ 1 + Merenje |IdIspitanika, 
		  method ="ML",
              correlation = corAR1(),
              data=podaci,
		  control=ctrl,
              na.action=na.exclude)
summary(H6c_1)

H6c_2 <- lme(ugr_koliko ~ 1 + Merenje + unap_ne_afek_c_lag_ic, 
              random= ~ 1 + Merenje + unap_ne_afek_c_lag_ic |IdIspitanika, 
		  method ="ML",
              correlation = corAR1(),
              data=podaci,
		  control=ctrl,
              na.action=na.exclude)
summary(H6c_2)
anova(H6c_1, H6c_2)

# H6d predvidam ugrožavajuce ponašanje na osnovu afekta povodom izvodenja ugrozavajuceg ponašanja u prethodnoj situaciji
# kriterijum: "ugr_koliko" Intenzitet izvedenog ugrožavajuceg ponašanja.
# prediktor:"ugr_da_afek_lag_ic" Afekat povodom prethodno izvedenog ugrožavajuceg  ponašanja, centrirano prema ispitaniku
# ni jedan od modela nije statisticki znacajan.

H6d_1 <- lme(ugr_koliko ~ 1 + Merenje + ugr_da_afek_lag_ic, 
              random= ~ 1 + Merenje |IdIspitanika, 
		  method ="ML",
              correlation = corAR1(),
              data=podaci,
		  control=ctrl,
              na.action=na.exclude)
summary(H6d_1)

H6d_2 <- lme(ugr_koliko ~ 1 + Merenje + ugr_da_afek_lag_ic, 
              random= ~ 1 + Merenje + ugr_da_afek_lag_ic |IdIspitanika, 
		  method ="ML",
              correlation = corAR1(),
              data=podaci,
		  control=ctrl,
              na.action=na.exclude)
summary(H6d_2)
anova(H6d_1, H6d_2)

# H6e predvidam ugrožavajuce ponašanje na osnovu afekta povodom izvodenja negativnog ponašanja (propuštanja unapredujuceg, izvodenja ugrožavajuceg) u prethodnoj situaciji
# kriterijum: "ugr_koliko" Intenzitet izvedenog ugrožavajuceg ponašanja.
# prediktor:"neg_afek_lag_ic" Afekat povodom izvodenja ugrožavajuceg i propuštanja unapredujuceg ponašanja u prethodnom merenju., centrirano prema ispitaniku
# ni jedan od modela nije statisticki znacajan.

H6e_1 <- lme(ugr_koliko ~ 1 + Merenje + neg_afek_lag_ic, 
              random= ~ 1 + Merenje |IdIspitanika, 
		  method ="ML",
              correlation = corAR1(),
              data=podaci,
		  control=ctrl,
              na.action=na.exclude)
summary(H6e_1)

H6e_2 <- lme(ugr_koliko ~ 1 + Merenje + neg_afek_lag_ic, 
              random= ~ 1 + Merenje + neg_afek_lag_ic |IdIspitanika, 
		  method ="ML",
              correlation = corAR1(),
              data=podaci,
		  control=ctrl,
              na.action=na.exclude)
summary(H6e_2)
anova(H6e_1, H6e_2)

# H6f predvidam ugrožavajuce ponašanje na osnovu ukupnog afekta (prosek afekta povodom pozitivnog i negativnog ponašanja) u prethodnoj situaciji
# kriterijum: "ugr_koliko" Intenzitet izvedenog ugrožavajuceg ponašanja.
# prediktor:"total_afek_lag_ic" Ukupan afekat u prethodnom mrenju, centrirano prema ispitaniku
# ni jedan od modela nije statisticki znacajan.

H6f_1 <- lme(ugr_koliko ~ 1 + Merenje + total_afek_lag_ic, 
              random= ~ 1 + Merenje|IdIspitanika, 
		  method ="ML",
              correlation = corAR1(),
              data=podaci,
		  control=ctrl,
              na.action=na.exclude)
summary(H6f_1)

H6f_2 <- lme(ugr_koliko ~ 1 + Merenje + total_afek_lag_ic, 
              random= ~ 1 + Merenje + total_afek_lag_ic |IdIspitanika, 
		  method ="ML",
              correlation = corAR1(),
              data=podaci,
		  control=ctrl,
              na.action=na.exclude)
summary(H6f_2)
anova(H6f_1, H6f_2)

###################################################################################################################################################################.
# H7: Neprijatnost povodom neizvodenja zdravstveno unapredujuceg ponašanja bice prediktor smanjenja procenjenog zdravstveno unapredujuceg ponašanja u narednoj situaciji.
###################################################################################################################################################################.

# Kriterijum: "unap_koliko"  Intenzitet izvedenog unapredujuceg ponašanja.
# Prediktor: "unap_ne_afek_c_lag_ic" Afekat povodom prethodno neizvedenog unapredujuceg, (ako to ikad rade - c), centrirano prema ispitaniku (-ic) 

# Selektujemo samo situacije kada je ispitanik mislio da je u prethodnoj situaciji trebalo da izvede unapredujuce ponašanje. 
podaci_unap_trebalo <- podaci[which(podaci$unap_trebalo_lag == 1), ]


# Kada smo testirali H5 proverili smo neophodnost korišcenja HLM-a, za i podelili varijansu zavisne varijable za intenzitet unapredujuceg ponašanja "unap_koliko", tako da nema potrebe da to ponovo radimo.

# Oba modela prediktor pokazuju znacajnim, a poredenje ukazuje na to da treba da se zadržimo na jednostavnijem modelu. b=0.10, t(886)= 2.30, p=0,0216
# Dakle, pozitivniji afekat povodom propuštanja unapredujuceg ponašanja, za koje su smatrali da je trebalo da urade, navodi ispitanike više unapredujuceg ponašanja u narednoj situaciji.
# tako da se zadržavam na modelu sa varijablinim interceptom i koji ukazuje na to da treba prihvatiti H7. 


H7_1 <- lme(unap_koliko ~ 1 + Merenje + unap_ne_afek_c_lag_ic, 
              random= ~ 1 + Merenje |IdIspitanika, 
              correlation = corAR1(),
		  method ="ML",
              data=podaci_unap_trebalo,
		  control=ctrl,
              na.action=na.exclude)
summary(H7_1)

H5c_1 <- lme(unap_koliko ~ 1 + Merenje + unap_ne_afek_c_lag_ic, 
              random= ~ 1 + Merenje |IdIspitanika, 
		  method ="ML",
              correlation = corAR1(),
              data=podaci,
		  control=ctrl,
              na.action=na.exclude)
summary(H5c_1)

H7_2 <- lme(unap_koliko ~ 1 + Merenje + unap_ne_afek_c_lag_ic, 
              random= ~ 1 + Merenje + unap_ne_afek_c_lag_ic |IdIspitanika, 
              correlation = corAR1(),
		  method ="ML",
              data=podaci_unap_trebalo,
		  control=ctrl,
              na.action=na.exclude)
summary(H7_2)
anova(H7_1,H7_2) 

#############################	 dodatne analize u vezi sa H7. #################################################################.

# ispitivanje moderatora 

H7_3 <- lme(unap_koliko ~ 1 + Merenje + unap_ne_afek_c_lag_ic*unap_int_gc, 
              random= ~ 1 + Merenje |IdIspitanika, 
              correlation = corAR1(),
		  method ="ML",
              data=podaci_unap_trebalo,
		  control=ctrl,
              na.action=na.exclude)
summary(H7_3)

# pokazuje znacajnu interakciju sa procenom povoljnosti dešavanja. b=-0.11, t(882)= -2.43, p=0,0153
# Povezanost afekta povodom propuštenog unapredujuceg ponašanja sa povecanjem unapredujuceg ponašanja je izražena samo kada su dogadanja procenjena negativnim.

H7_4 <- lme(unap_koliko ~ 1 + Merenje + 
		  unap_ne_afek_c_lag_ic*Desavanja_ic +
		  unap_ne_afek_c_lag_ic*Umor_ic, 
              random= ~ 1 + Merenje |IdIspitanika, 
              correlation = corAR1(),
		  method ="ML",
              data=podaci_unap_trebalo,
		  control=ctrl,
              na.action=na.exclude)
summary(H7_4)

describe(podaci$unap_ne_afek_c_lag_ic)
describe(podaci$Desavanja_ic)

ef.H7_4 <- effect(term="unap_ne_afek_c_lag_ic*Desavanja_ic", mod=H7_4, 
              xlevels=list(unap_ne_afek_c_lag_ic=c(-0.7, +0.7), Desavanja_ic=c(-1.06, +1.06)))
summary(ef.H7_4)

ef.H7_4 <- as.data.frame(ef.H7_4)
ef.H7_4

# graficko prikazivanje moderatora
ggplot(data=ef.H7_4, aes(x=as.numeric(unap_ne_afek_c_lag_ic), y=fit, group=Desavanja_ic), legend=TRUE) + 
  geom_point() +
  geom_line() +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.15) +
  xlab("Afekat povodom propuštenog unapredujuceg ponašanja u prethodnoj situaciji") + xlim(-1,1) +
  ylab("Unapedujuce ponašanje") + ylim(3.5,4.5) +
  ggtitle("Predikcija ponašanja na osnovu afekta - moderirana procenom povoljnosti dešavanja")

H7_5 <- lme(unap_koliko ~ 1 + Merenje + 
		  unap_ne_afek_c_lag_ic*Desavanja_ic +
		  unap_ne_afek_c_lag_ic*HonestyHumility_gc +
		  unap_ne_afek_c_lag_ic*Emotionality_gc +
		  unap_ne_afek_c_lag_ic*Extraversion_gc +
		  unap_ne_afek_c_lag_ic*Agreeableness_gc +
		  unap_ne_afek_c_lag_ic*Conscientiousness_gc +
		  unap_ne_afek_c_lag_ic*Openness_gc, 
              random= ~ 1 + Merenje |IdIspitanika, 
              correlation = corAR1(),
		  method ="ML",
              data=podaci_unap_trebalo,
		  control=ctrl,
              na.action=na.exclude)
summary(H7_5)

# Takode, detektovana je znacajna interakciju izmedu afekta povodom propuštenog unapredujuceg ponašanja i doživljaja fizickog izgleda b=-0.12, t(881)= -2.12, p=0,0342
# kod onih koji imaju lošiju sliku o svom izgledu, stepen pozitivnosti afekta koji imaju povodom propuštenog ponašanja ce voditi ka više unapredujuceg ponašanja u narednoj situaciji.

H7_6 <- lme(unap_koliko ~ 1 + Merenje + 
		  unap_ne_afek_c_lag_ic*Desavanja_ic +
		  unap_ne_afek_c_lag_ic*SDQPhAbilities_gc +
		  unap_ne_afek_c_lag_ic*SDQPhAppereance_gc +
		  unap_ne_afek_c_lag_ic*SDQGenEsteem_gc, 
              random= ~ 1 + Merenje|IdIspitanika, 
              correlation = corAR1(),
		  method ="ML",
              data=podaci_unap_trebalo,
		  control=ctrl,
              na.action=na.exclude)
summary(H7_6)

describe(podaci_unap_trebalo$unap_ne_afek_c_lag_ic)
describe(podaci_unap_trebalo$SDQPhAppereance_gc)

ef.H7_6 <- effect(term="unap_ne_afek_c_lag_ic*SDQPhAppereance_gc", mod=H7_6, 
              xlevels=list(unap_ne_afek_c_lag_ic=c(-0.68, +0.68), SDQPhAppereance_gc=c(-0.92, +0.92)))
summary(ef.H7_6)

ef.H7_6 <- as.data.frame(ef.H7_6)
ef.H7_6

# graficko prikazivanje moderatora
ggplot(data=ef.H7_6, aes(x=as.numeric(unap_ne_afek_c_lag_ic), y=fit, group=SDQPhAppereance_gc), legend=TRUE) + 
  geom_point() +
  geom_line() +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.15) +
  xlab("Afekat povodom propuštenog unapredujuceg ponašanja u prethodnoj situaciji") + xlim(-1,1) +
  ylab("Unapedujuce ponašanje") + ylim(3.5,4.5) +
  ggtitle("Predikcija ponašanja na osnovu afekta - moderirano percecijom fizickog izgleda")

H7_7 <- lme(unap_koliko ~ 1 + Merenje + 
		  unap_ne_afek_c_lag_ic*Desavanja_ic +
		  unap_ne_afek_c_lag_ic*SDQPhAppereance_gc +
		  unap_ne_afek_c_lag_ic*PrevFokus_gc +
		  unap_ne_afek_c_lag_ic*PromoFokus_gc, 
              random= ~ 1 + Merenje |IdIspitanika, 
              correlation = corAR1(),
		  method ="ML",
              data=podaci_unap_trebalo,
		  control=ctrl,
              na.action=na.exclude)
summary(H7_7)


H7_final <- lme(unap_koliko ~ 1 + Merenje + 
		  unap_ne_afek_c_lag_ic*Desavanja_ic +
		  unap_ne_afek_c_lag_ic*SDQPhAppereance_gc, 
              random= ~ 1 + Merenje |IdIspitanika, 
              correlation = corAR1(),
		  method ="ML",
              data=podaci_unap_trebalo,
		  control=ctrl,
              na.action=na.exclude)
summary(H7_final)

# ispitujem pojedinacna ponašanja. 
# Selektujem samo ona merenja kada su ispitanici procenili da je u prethodnoj situaciji trebalo da izvedu unapredujuce ponašanje.
# Na nivou pojedinacnih ponašanja ne dobijam znacajne efekte (za vodu i povrce modeli ni ne postižu konvergenciju)

podaci_voda_trebalo <- podaci[which(podaci$Voda_trebalo_lag == 1), ]
dim(podaci_voda_trebalo)

H7_8 <- lme(Voda_koliko ~ 1 + Merenje + Voda_ne_afek_c_lag_ic, 
              random= ~ 1 + Merenje |IdIspitanika, 
              correlation = corAR1(),
		  method ="ML",
              data=podaci_voda_trebalo,
		  control=ctrl,
              na.action=na.exclude)
summary(H7_8)

podaci_voce_trebalo <- podaci[which(podaci$Voce_trebalo_lag == 1), ]
dim(podaci_voce_trebalo)

H7_9 <- lme(Voce_koliko ~ 1 + Merenje + Voce_ne_afek_c_lag_ic, 
              random= ~ 1 + Merenje |IdIspitanika, 
              correlation = corAR1(),
		  method ="ML",
              data=podaci_voce_trebalo,
		  control=ctrl,
              na.action=na.exclude)
summary(H7_9)

podaci_povrce_trebalo <- podaci[which(podaci$Povrce_trebalo_lag == 1), ]
dim(podaci_povrce_trebalo)
 
H7_10 <- lme(Povrce_koliko ~ 1 + Merenje + Povrce_ne_afek_c_lag_ic, 
              random= ~ 1 + Merenje |IdIspitanika, 
              correlation = corAR1(),
		  method ="ML",
              data=podaci_povrce_trebalo,
		  control=ctrl,
              na.action=na.exclude)
summary(H7_10)

podaci_vezbanje_trebalo <- podaci[which(podaci$Vezbanje_trebalo_lag == 1), ]
dim(podaci_vezbanje_trebalo)

H7_11 <- lme(Vezbanje_koliko ~ 1 + Merenje + Vezbanje_ne_afek_c_lag_ic, 
              random= ~ 1 + Merenje |IdIspitanika, 
              correlation = corAR1(),
		  method ="ML",
              data=podaci_vezbanje_trebalo,
		  control=ctrl,
              na.action=na.exclude)
summary(H7_11)

podaci_dodaciIshrani_trebalo <- podaci[which(podaci$DodaciIshrani_trebalo_lag == 1), ]
dim(podaci_dodaciIshrani_trebalo)

H7_12 <- lme(DodaciIshrani_koliko ~ 1 + DodaciIshrani_ne_afek_c_lag_ic, 
              random= ~ 1 + Merenje |IdIspitanika, 
              correlation = corAR1(),
		  method ="ML",
              data=podaci_dodaciIshrani_trebalo,
		  control=ctrl,
              na.action=na.exclude)
summary(H7_12)

###################################################################################################################################################################.
# H8: Neprijatnost povodom izvodenja zdravstveno ugrožavajuceg ponašanja bice prediktor povecanja procenjenog zdravstveno ugrožavajuceg ponašanja u narednoj situaciji.
###################################################################################################################################################################.

# Testiranje H3 je pokazalo da stepen izvodenja ugrožavajuceg ponašanja nije u statisticki znajacnoj vezi sa prijatnošcu afekta, tako da H8 nije potrebno testirati.
# Ali, s obzirom na to da za pojedina ponašanja kao što su pušenje b=-0.27, t(354)= -3.89, p<0,0001 i konzumacija slatkiša b=-0.08, t(522)= -2.25, p=0,0248 nalazimo visoke i relativno visoke povezanosti
# H8 je testirana za ova ponapanja u eksplorativne svrhe.

H8_10 <- lme(Cigarete_koliko ~ 1 + Merenje + Cigarete_da_afek_lag_ic, 
	random = ~ 1 | IdIspitanika,
	data=podaci, 
	method ="ML",
      na.action=na.exclude,
	control=ctrl, 
	correlation = corAR1())
summary(H8_10)

H8_10r <- lme(Cigarete_koliko ~ 1 + Merenje + Cigarete_da_afek_lag_ic, 
	random = ~ 1 + Merenje + Cigarete_da_afek_lag_ic | IdIspitanika,
	data=podaci, 
	method ="ML",
      na.action=na.exclude,
	control=ctrl, 
	correlation = corAR1())
summary(H8_10r)

H8_12 <- lme(Slatkisi_koliko ~ 1 + Merenje + Slatkisi_da_afek_lag_ic, 
	random = ~ 1 | IdIspitanika,
	data=podaci, 
	method ="ML",
      na.action=na.exclude,
	control=ctrl, 
	correlation = corAR1())
summary(H8_12)

H8_12r <- lme(Slatkisi_koliko ~ 1 + Merenje + Slatkisi_da_afek_lag_ic, 
	random = ~ 1 + Merenje + Slatkisi_da_afek_lag_ic | IdIspitanika,
	data=podaci, 
	method ="ML",
      na.action=na.exclude,
	control=ctrl, 
	correlation = corAR1())
summary(H8_12r)

###################################################################################################################################################################.
# H9: Savesnost ce biti pozitivno povezana sa zdravstveno unapredujucim ponašanjem.
###################################################################################################################################################################.

# Da bih uradio analize koje su na nivou ispitanika neophodno je da agregiram bazu

total_agg <- aggregate(podaci, by=list(podaci$IdIspitanika), mean, na.rm=TRUE)

# prediktorske var: bazicne crte licnosti (centrirane na nivou grupe - grand mean)
# kriterijumska var: "unap_int" nivo uobicajenog zdravstveno unapredujuceg ponašanja 
# var. je dobijena tako što je intenzitet pojedinacnih ponašanja kategorisan u odnosu na grupu, pa izveden prosek tih rangova

# rezultat višestruke regresione analize pokazuje da savesnost jeste statisticki znacajan prediktor zdravstveno unapredujuceg ponašanja b=0.16, t(91)= 2.038, p=0,0445

H9 <- lm(unap_int ~ HonestyHumility_gc + 
	Emotionality_gc + 
	Extraversion_gc + 
	Agreeableness_gc + 
	Conscientiousness_gc + 
	Openness_gc,
	data=total_agg,
	na.action=na.exclude)
summary(H9)

ggplot(data=total_agg, aes(x=Conscientiousness_gc, y=unap_int, color="gray"), legend=FALSE) + 
  geom_point(color="gray") + guides(color=FALSE) +
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE, lty=1, size=.5, color="gray40") +
  geom_smooth(aes(group=1), method=lm, se=FALSE, fullrange=FALSE, lty=1, size=2, color="red") +
  xlab("Savesnost") + scale_x_continuous(breaks=seq(-2,2,by=1)) +
  ylab("Intenzitet unapredujuceg ponašanja")  + ylim(0,4) +
  ggtitle("Savesnost i intenzitet unapredujuceg ponašanja")

#############################	 dodatne analize u vezi sa H9. #################################################################.

# H9a osim crta licnosti(zadržane sve u modelu), kao preiktore uvodim i karakteristike samopoimanja i dobijam da percepcija fizickih sposobnosti biva znacajnim prediktorom unapredujuceg ponašanja
# b=0.21 t(88)=3.57 p=0,0006

H9a <- lm(unap_int ~ HonestyHumility_gc + 
	Emotionality_gc + 
	Extraversion_gc + 
	Agreeableness_gc + 
	Conscientiousness_gc + 
	Openness_gc + 
	SDQPhAbilities_gc + 
	SDQPhAppereance_gc + 
	SDQGenEsteem_gc,
	data=total_agg,
	na.action=na.exclude)
summary(H9a)


# Ispitujem da li na osnovu savesnosti i procne fizickih sposobnosti mogu da predvidim pojedinacna unapredujuca ponašanja

# kriterijum 1: VO_1 - broj caša vode dnevno
# kriterijum 2: I_3nr_int - broj porcija voca nedeljno 
# kriterijum 3: I_5nr_int - broj porcija povrca nedeljno
# kriterijum 4: DI_1 - broj dana u sedmici kada konzumira dodatke ishrani
# kriterijum 5: TV_4 - broj sati vežbanja sedmicno


H9a_1 <- lm(VO_1 ~ HonestyHumility_gc + 
	Emotionality_gc + 
	Extraversion_gc + 
	Agreeableness_gc + 
	Conscientiousness_gc + 
	Openness_gc + 
	SDQPhAbilities_gc + 
	SDQPhAppereance_gc + 
	SDQGenEsteem_gc,
	data=total_agg,
	na.action=na.exclude)
summary(H9a_1)

H9a_1 <- rlm(VO_1 ~ HonestyHumility_gc + 
	Emotionality_gc + 
	Extraversion_gc + 
	Agreeableness_gc + 
	Conscientiousness_gc + 
	Openness_gc + 
	SDQPhAbilities_gc + 
	SDQPhAppereance_gc + 
	SDQGenEsteem_gc,
	data=total_agg,
	na.action=na.exclude)
summary(H9a_1)

# kada je u pitanju konzumacija voca dobijam da je savesnost negativan prediktor b=-5.60, t(88)= -2.53, p=0,0133
# a percepcija fizickih sposobnosti pozitivan b=3.59, t(88)= 2.15, p=0,0342

H9a_2 <- lm(I_3nr_int ~ HonestyHumility_gc + 
	Emotionality_gc + 
	Extraversion_gc + 
	Agreeableness_gc + 
	Conscientiousness_gc + 
	Openness_gc + 
	SDQPhAbilities_gc + 
	SDQPhAppereance_gc + 
	SDQGenEsteem_gc,
	data=total_agg,
	na.action=na.exclude)
summary(H9a_2)

H9a_2r <- lmrob(I_3nr_int ~ HonestyHumility_gc + 
	Emotionality_gc + 
	Extraversion_gc + 
	Agreeableness_gc + 
	Conscientiousness_gc + 
	Openness_gc + 
	SDQPhAbilities_gc + 
	SDQPhAppereance_gc + 
	SDQGenEsteem_gc,
	data=total_agg,
	na.action=na.exclude)
summary(H9a_2r)

ggplot(data=total_agg, aes(x=Conscientiousness_gc, y=I_3nr_int, color="gray"), legend=FALSE) + 
  geom_point(color="gray") + guides(color=FALSE) +
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE, lty=1, size=.5, color="gray40") +
  geom_smooth(aes(group=1), method=lm, se=FALSE, fullrange=FALSE, lty=1, size=2, color="red") +
  xlab("Savesnost") + scale_x_continuous(breaks=seq(-2,2,by=1)) +
  ylab("Broj porcija voca sedmicno")  + ylim(0,150) +
  ggtitle("Savesnost i kozumacija voca")

ggplot(data=total_agg, aes(x=Conscientiousness_gc, y=I_3nr_int, color="gray"), legend=FALSE) + 
  geom_point(color="gray") + guides(color=FALSE) +
  geom_smooth(method=lmrob, se=FALSE, fullrange=FALSE, lty=1, size=.5, color="gray40") +
  geom_smooth(aes(group=1), method=lmrob, se=FALSE, fullrange=FALSE, lty=1, size=2, color="red") +
  xlab("Savesnost") + scale_x_continuous(breaks=seq(-2,2,by=1)) +
  ylab("Broj porcija voca sedmicno")  + ylim(0,150) +
  ggtitle("Savesnost i kozumacija voca")


# za konzumaciju povrca je savesnost pozitivan prediktor b=5.85, t(88)= 3.64, p=0,0005
# a percepcija fizickog izgleda negativan b=-3.11, t(88)= -2.29, p=0,0243

H9a_3 <- lm(I_5nr_int ~ HonestyHumility_gc + 
	Emotionality_gc + 
	Extraversion_gc + 
	Agreeableness_gc + 
	Conscientiousness_gc + 
	Openness_gc + 
	SDQPhAbilities_gc + 
	SDQPhAppereance_gc + 
	SDQGenEsteem_gc,
	data=total_agg,
	na.action=na.exclude)
summary(H9a_3)

H9a_3r <- lmrob(I_5nr_int ~ HonestyHumility_gc + 
	Emotionality_gc + 
	Extraversion_gc + 
	Agreeableness_gc + 
	Conscientiousness_gc + 
	Openness_gc + 
	SDQPhAbilities_gc + 
	SDQPhAppereance_gc + 
	SDQGenEsteem_gc,
	data=total_agg,
	na.action=na.exclude)
summary(H9a_3r)

ggplot(data=total_agg, aes(x=Conscientiousness_gc, y=I_5nr_int, color="gray"), legend=FALSE) + 
  geom_point(color="gray") + guides(color=FALSE) +
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE, lty=1, size=.5, color="gray40") +
  geom_smooth(aes(group=1), method=lm, se=FALSE, fullrange=FALSE, lty=1, size=2, color="red") +
  xlab("Savesnost") + scale_x_continuous(breaks=seq(-2,2,by=1)) +
  ylab("Broj porcija voca sedmicno")  + ylim(0,150) +
  ggtitle("Savesnost i kozumacija povrca")

ggplot(data=total_agg, aes(x=Conscientiousness_gc, y=I_5nr_int, color="gray"), legend=FALSE) + 
  geom_point(color="gray") + guides(color=FALSE) +
  geom_smooth(method=lmrob, se=FALSE, fullrange=FALSE, lty=1, size=.5, color="gray40") +
  geom_smooth(aes(group=1), method=lmrob, se=FALSE, fullrange=FALSE, lty=1, size=2, color="red") +
  xlab("Savesnost") + scale_x_continuous(breaks=seq(-2,2,by=1)) +
  ylab("Broj porcija voca sedmicno")  + ylim(0,150) +
  ggtitle("Savesnost i kozumacija povrca")

H9a_4 <- lm(DI_1 ~ HonestyHumility_gc + 
	Emotionality_gc + 
	Extraversion_gc + 
	Agreeableness_gc + 
	Conscientiousness_gc + 
	Openness_gc + 
	SDQPhAbilities_gc + 
	SDQPhAppereance_gc + 
	SDQGenEsteem_gc,
	data=total_agg,
	na.action=na.exclude)
summary(H9a_4)

# znacajan prediktor broja sati vežbanja sedmicno je percepcija fizickih sposobnosti b=0.88, t(88)= 2.25, p=0,0267

H9a_5 <- lm(TV_4 ~ HonestyHumility_gc + 
	Emotionality_gc + 
	Extraversion_gc + 
	Agreeableness_gc + 
	Conscientiousness_gc + 
	Openness_gc + 
	SDQPhAbilities_gc + 
	SDQPhAppereance_gc + 
	SDQGenEsteem_gc,
	data=total_agg,
	na.action=na.exclude)
summary(H9a_5)

H9a_5r <- lmrob(TV_4 ~ HonestyHumility_gc + 
	Emotionality_gc + 
	Extraversion_gc + 
	Agreeableness_gc + 
	Conscientiousness_gc + 
	Openness_gc + 
	SDQPhAbilities_gc + 
	SDQPhAppereance_gc + 
	SDQGenEsteem_gc,
	data=total_agg,
	na.action=na.exclude)
summary(H9a_5r)

ggplot(data=total_agg, aes(x=SDQPhAbilities_gc, y=TV_4, color="gray"), legend=FALSE) + 
  geom_point(color="gray") + guides(color=FALSE) +
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE, lty=1, size=.5, color="gray40") +
  geom_smooth(aes(group=1), method=lm, se=FALSE, fullrange=FALSE, lty=1, size=2, color="red") +
  xlab("Percepcija fizickih sposobnosti") + scale_x_continuous(breaks=seq(-2,2,by=1)) +
  ylab("Intenzitet unapredujuceg ponašanja")  + ylim(0,25) +
  ggtitle("Percepcija fizickih sposobnosti i broj sati provedenih u vežbanju")

ggplot(data=total_agg, aes(x=SDQPhAbilities_gc, y=TV_4, color="gray"), legend=FALSE) + 
  geom_point(color="gray") + guides(color=FALSE) +
  geom_smooth(method=lmrob, se=FALSE, fullrange=FALSE, lty=1, size=.5, color="gray40") +
  geom_smooth(aes(group=1), method=lmrob, se=FALSE, fullrange=FALSE, lty=1, size=2, color="red") +
  xlab("Percepcija fizickih sposobnosti") + scale_x_continuous(breaks=seq(-2,2,by=1)) +
  ylab("Intenzitet unapredujuceg ponašanja")  + ylim(0,25) +
  ggtitle("Percepcija fizickih sposobnosti i broj sati provedenih u vežbanju")

###################################################################################################################################################################.
# H10: Neuroticizam ce biti pozitivno povezan sa zdravstveno ugrožavajucim ponašanjem.
###################################################################################################################################################################.

# prediktorske var: bazicne crte licnosti (centrirane na nivou grupe - grand mean)
# kriterijumska var: "ugr_int" nivo uobicajenog zdravstveno ugrožavajuceg ponašanja
# var. je dobijena tako što je intenzitet pojedinacnih ponašanja kategorisan u odnosu na grupu, pa izveden prosek tih rangova

# Emocionalnost (HEXACO ekvivalent neuroticizna) se ne pokazuje znacajnim prediktorom ugrožavajuceg ponašanja i H10 nije potvrdena.
# Savesnost se pokazuje negativnim prediktorom ugrožavajuceg ponašanja b=-0.34, t(91)= -3.32, p=0,0013

H10 <- lm(ugr_int ~ HonestyHumility_gc + 
	Emotionality_gc + 
	Extraversion_gc + 
	Agreeableness_gc + 
	Conscientiousness_gc + 
	Openness_gc,
	data=total_agg,
	na.action=na.exclude)
summary(H10)

ggplot(data=total_agg, aes(x=Conscientiousness_gc, y=ugr_int, color="gray"), legend=FALSE) + 
  geom_point(color="gray") + guides(color=FALSE) +
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE, lty=1, size=.5, color="gray40") +
  geom_smooth(aes(group=1), method=lm, se=FALSE, fullrange=FALSE, lty=1, size=2, color="red") +
  xlab("Savesnost") + scale_x_continuous(breaks=seq(-2,2,by=1)) +
  ylab("Intenzitet ugrožavajucih ponašanja")  + ylim(0,4) +
  ggtitle("Savesnost i intenzitet ugrožavajucih ponašanja")

#############################	 dodatne analize u vezi sa H10. #################################################################.

# Osim bazicnih crta licnosti (sve zadržane u modelu) percepcija fizickih sposobnosti se pokazuje znacajnim prediktorom b=-0.16, t(88)= -2.12, p=0,0369

H10a <- lm(ugr_int ~ HonestyHumility_gc + 
	Emotionality_gc + 
	Extraversion_gc + 
	Agreeableness_gc + 
	Conscientiousness_gc + 
	Openness_gc +
	SDQPhAbilities_gc + 
	SDQPhAppereance_gc + 
	SDQGenEsteem_gc,
	data=total_agg,
	na.action=na.exclude)
summary(H10a)

ggplot(data=total_agg, aes(x=SDQPhAbilities_gc, y=ugr_int, color="gray"), legend=FALSE) + 
  geom_point(color="gray") + guides(color=FALSE) +
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE, lty=1, size=.5, color="gray40") +
  geom_smooth(aes(group=1), method=lm, se=FALSE, fullrange=FALSE, lty=1, size=2, color="red") +
  xlab("Samopoimanje fizicke sposobnosti") + scale_x_continuous(breaks=seq(-2,2,by=1)) +
  ylab("Intenzitet ugrožavajucih ponašanja")  + ylim(0,4) +
  ggtitle("Samopoimanje fizicke sposobnosti i intenzitet ugrožavajucih ponašanja")

# predvidam inenzitet pojedinacnih ponašanja

# PU_int - broj cigareta mesecno
# KF_int - broj kafa/energetskih pica nedeljno
# SL_int - broj konzumacija slatkiša nedeljno
# KA_int - broj alkoholnih pica nedeljno
# BH_1 - broj dana u sedmici kojim osoba kozumira brzu hranu

# ekstraverzija se pokazuje kao pozitivan prediktor broja cigareta koji ce neko popušiti tokom meseca b=80.34, t(88)= 3.224, p=0,00177
# dok su negativni prediktori savesnost (b=-53.18, t(88)= -2.314, p=0,0229) i generalno samopouzdanje (b=-39.148, t(88)= -2.202, p=0,0303) 


total_agg$PU_int<-recode(total_agg$PU_int,"0=NA")
 
H10a_1 <- lm(PU_int ~ HonestyHumility_gc + 
	Emotionality_gc + 
	Extraversion_gc + 
	Agreeableness_gc + 
	Conscientiousness_gc + 
	Openness_gc +
	SDQPhAbilities_gc + 
	SDQPhAppereance_gc + 
	SDQGenEsteem_gc,
	data=total_agg,
	na.action=na.exclude)
summary(H10a_1)

H10a_1r <- lmrob(PU_int ~ HonestyHumility_gc + 
	Emotionality_gc + 
	Extraversion_gc + 
	Agreeableness_gc + 
	Conscientiousness_gc + 
	Openness_gc +
	SDQPhAbilities_gc + 
	SDQPhAppereance_gc + 
	SDQGenEsteem_gc,
	data=total_agg,
	na.action=na.exclude,
      max.it = 1000)
summary(H10a_1r)

ggplot(data=total_agg, aes(x=Extraversion_gc, y=PU_int, color="gray"), legend=FALSE) + 
  geom_point(color="gray") + guides(color=FALSE) +
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE, lty=1, size=.5, color="gray40") +
  geom_smooth(aes(group=1), method=lm, se=FALSE, fullrange=FALSE, lty=1, size=2, color="red") +
  xlab("Ekstraverzija") + scale_x_continuous(breaks=seq(-2,2,by=1)) +
  ylab("Broj cigareta mesecno")  + ylim(0,650) +
  ggtitle("Broj popušenih cigareta mesecno")

ggplot(data=total_agg, aes(x=Extraversion_gc, y=PU_int, color="gray"), legend=FALSE) + 
  geom_point(color="gray") + guides(color=FALSE) +
  geom_smooth(method=lmrob, se=FALSE, fullrange=FALSE, lty=1, size=.5, color="gray40") +
  geom_smooth(aes(group=1), method=lmrob, se=FALSE, fullrange=FALSE, lty=1, size=2, color="red") +
  xlab("Ekstraverzija") + scale_x_continuous(breaks=seq(-2,2,by=1)) +
  ylab("Broj cigareta mesecno")  + ylim(0,650) +
  ggtitle("Broj popušenih cigareta mesecno")


H10a_2 <- lm(KF_int ~ HonestyHumility_gc + 
	Emotionality_gc + 
	Extraversion_gc + 
	Agreeableness_gc + 
	Conscientiousness_gc + 
	Openness_gc +
	SDQPhAbilities_gc + 
	SDQPhAppereance_gc + 
	SDQGenEsteem_gc,
	data=total_agg,
	na.action=na.exclude)
summary(H10a_2)

H10a_3 <- lm(SL_int ~ HonestyHumility_gc + 
	Emotionality_gc + 
	Extraversion_gc + 
	Agreeableness_gc + 
	Conscientiousness_gc + 
	Openness_gc +
	SDQPhAbilities_gc + 
	SDQPhAppereance_gc + 
	SDQGenEsteem_gc,
	data=total_agg,
	na.action=na.exclude)
summary(H10a_3)

# savesnost se pokazuje negativnim prediktorom broja alkoholnih pica koje osoba popije sedmicno b=-5.67, t(88)= -3.609, p=0,00051
# otvorenost za iskustvo se pokazuje pozitivnim prediktorom broja alkoholnih pica koje osoba popije sedmicno b=4.09, t(88)= 2.722, p=0,00782


H10a_4 <- lm(KA_int ~ HonestyHumility_gc + 
	Emotionality_gc + 
	Extraversion_gc + 
	Agreeableness_gc + 
	Conscientiousness_gc + 
	Openness_gc +
	SDQPhAbilities_gc + 
	SDQPhAppereance_gc + 
	SDQGenEsteem_gc,
	data=total_agg,
	na.action=na.exclude)
summary(H10a_4)

H10a_4r <- lmrob(KA_int ~ HonestyHumility_gc + 
	Emotionality_gc + 
	Extraversion_gc + 
	Agreeableness_gc + 
	Conscientiousness_gc + 
	Openness_gc +
	SDQPhAbilities_gc + 
	SDQPhAppereance_gc + 
	SDQGenEsteem_gc,
	data=total_agg,
	na.action=na.exclude)
summary(H10a_4r)

ggplot(data=total_agg, aes(x=Conscientiousness_gc, y=KA_int, color="gray"), legend=FALSE) + 
  geom_point(color="gray") + guides(color=FALSE) +
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE, lty=1, size=.5, color="gray40") +
  geom_smooth(aes(group=1), method=lm, se=FALSE, fullrange=FALSE, lty=1, size=2, color="red") +
  xlab("Savesnost") + scale_x_continuous(breaks=seq(-2,2,by=1)) +
  ylab("Broj popijenih pica sedmicno")  + ylim(0,60) +
  ggtitle("Savesnost i broj popijenih pica sedmicno")

ggplot(data=total_agg, aes(x=Conscientiousness_gc, y=KA_int, color="gray"), legend=FALSE) + 
  geom_point(color="gray") + guides(color=FALSE) +
  geom_smooth(method=lmrob, se=FALSE, fullrange=FALSE, lty=1, size=.5, color="gray40") +
  geom_smooth(aes(group=1), method=lmrob, se=FALSE, fullrange=FALSE, lty=1, size=2, color="red") +
  xlab("Savesnost") + scale_x_continuous(breaks=seq(-2,2,by=1)) +
  ylab("Broj popijenih pica sedmicno")  + ylim(0,60) +
  ggtitle("Savesnost i broj popijenih pica sedmicno")

ggplot(data=total_agg, aes(x=Openness_gc, y=KA_int, color="gray"), legend=FALSE) + 
  geom_point(color="gray") + guides(color=FALSE) +
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE, lty=1, size=.5, color="gray40") +
  geom_smooth(aes(group=1), method=lm, se=FALSE, fullrange=FALSE, lty=1, size=2, color="red") +
  xlab("Otvorenost za iskustvo") + scale_x_continuous(breaks=seq(-2,2,by=1)) +
  ylab("Broj popijenih pica sedmicno")  + ylim(0,60) +
  ggtitle("Otvorenost za iskustvo i broj popijenih pica sedmicno")

ggplot(data=total_agg, aes(x=Openness_gc, y=KA_int, color="gray"), legend=FALSE) + 
  geom_point(color="gray") + guides(color=FALSE) +
  geom_smooth(method=lmrob, se=FALSE, fullrange=FALSE, lty=1, size=.5, color="gray40") +
  geom_smooth(aes(group=1), method=lmrob, se=FALSE, fullrange=FALSE, lty=1, size=2, color="red") +
  xlab("Otvorenost za iskustvo") + scale_x_continuous(breaks=seq(-2,2,by=1)) +
  ylab("Broj popijenih pica sedmicno")  + ylim(0,60) +
  ggtitle("Otvorenost za iskustvo i broj popijenih pica sedmicno")


# savesnost se pokazuje negativnim prediktorom broja dana u sedmici kojim osoba konzumira brzu hranu b=-0.71, t(88)= -2.33, p=0,0221
H10a_5 <- lm(BH_1 ~ HonestyHumility_gc + 
	Emotionality_gc + 
	Extraversion_gc + 
	Agreeableness_gc + 
	Conscientiousness_gc + 
	Openness_gc +
	SDQPhAbilities_gc + 
	SDQPhAppereance_gc + 
	SDQGenEsteem_gc,
	data=total_agg,
	na.action=na.exclude)
summary(H10a_5)

H10a_5r <- lmrob(BH_1 ~ HonestyHumility_gc + 
	Emotionality_gc + 
	Extraversion_gc + 
	Agreeableness_gc + 
	Conscientiousness_gc + 
	Openness_gc +
	SDQPhAbilities_gc + 
	SDQPhAppereance_gc + 
	SDQGenEsteem_gc,
	data=total_agg,
	na.action=na.exclude)
summary(H10a_5r)

ggplot(data=total_agg, aes(x=Conscientiousness_gc, y=BH_1, color="gray"), legend=FALSE) + 
  geom_point(color="gray") + guides(color=FALSE) +
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE, lty=1, size=.5, color="gray40") +
  geom_smooth(aes(group=1), method=lm, se=FALSE, fullrange=FALSE, lty=1, size=2, color="red") +
  xlab("Savesnost") + scale_x_continuous(breaks=seq(-2,2,by=1)) +
  ylab("Konzumacija brze hrane")  + ylim(0,7) +
  ggtitle("Savesnost i konzumacija brze hrane")

ggplot(data=total_agg, aes(x=Conscientiousness_gc, y=BH_1, color="gray"), legend=FALSE) + 
  geom_point(color="gray") + guides(color=FALSE) +
  geom_smooth(method=lmrob, se=FALSE, fullrange=FALSE, lty=1, size=.5, color="gray40") +
  geom_smooth(aes(group=1), method=lmrob, se=FALSE, fullrange=FALSE, lty=1, size=2, color="red") +
  xlab("Savesnost") + scale_x_continuous(breaks=seq(-2,2,by=1)) +
  ylab("Konzumacija brze hrane")  + ylim(0,7) +
  ggtitle("Savesnost i konzumacija brze hrane")




###########################################################################################################################################.
# Deskriptivna statistika.
###########################################################################################################################################.

# agregiramo podatke na nivo ispitanika

total_agg <- aggregate(podaci, by=list(podaci$IdIspitanika), mean, na.rm=TRUE)

# racunamo pouzdanost skala

HonestyHumility_60<-c("HEXACO_60_6", "HEXACO_60_30_R", "HEXACO_60_54", "HEXACO_60_12_R", "HEXACO_60_36", "HEXACO_60_60_R", "HEXACO_60_18", "HEXACO_60_42_R", "HEXACO_60_24_R", "HEXACO_60_48_R")		
Emotionality_60<-c("HEXACO_60_5", "HEXACO_60_29", "HEXACO_60_53_R", "HEXACO_60_11", "HEXACO_60_35_R", "HEXACO_60_17", "HEXACO_60_41_R", "HEXACO_60_23", "HEXACO_60_47", "HEXACO_60_59_R")		
Extraversion_60<-c("HEXACO_60_4", "HEXACO_60_28_R", "HEXACO_60_52_R", "HEXACO_60_10_R", "HEXACO_60_34", "HEXACO_60_58", "HEXACO_60_16", "HEXACO_60_40", "HEXACO_60_22", "HEXACO_60_46_R")		
Agreeableness_60<-c("HEXACO_60_3", "HEXACO_60_27", "HEXACO_60_9_R", "HEXACO_60_33", "HEXACO_60_51", "HEXACO_60_15_R", "HEXACO_60_39", "HEXACO_60_57_R", "HEXACO_60_21_R", "HEXACO_60_45")		
Conscientiousness_60<-c("HEXACO_60_2", "HEXACO_60_26_R", "HEXACO_60_8", "HEXACO_60_32_R", "HEXACO_60_14_R", "HEXACO_60_38", "HEXACO_60_50", "HEXACO_60_20_R", "HEXACO_60_44_R", "HEXACO_60_56_R")		
Openness_60<-c("HEXACO_60_1_R", "HEXACO_60_25", "HEXACO_60_7", "HEXACO_60_31_R", "HEXACO_60_13", "HEXACO_60_37", "HEXACO_60_49_R", "HEXACO_60_19_R", "HEXACO_60_43", "HEXACO_60_55_R")

HonestyHumility_60 <- total_agg [HonestyHumility_60]
Emotionality_60 <- total_agg [Emotionality_60]
Extraversion_60 <- total_agg [Extraversion_60]
Agreeableness_60 <- total_agg [Agreeableness_60]
Conscientiousness_60 <- total_agg [Conscientiousness_60]
Openness_60 <- total_agg [Openness_60]

alpha (HonestyHumility_60)
alpha (Emotionality_60)
alpha (Extraversion_60)
alpha (Agreeableness_60)
alpha (Conscientiousness_60)
alpha (Openness_60)


HonestyHumility_100 <-c("HEXACO_100_6_R", "HEXACO_100_30", "HEXACO_100_54_R", "HEXACO_100_78", "HEXACO_100_12_R", "HEXACO_100_36_R", "HEXACO_100_60", "HEXACO_100_84_R", "HEXACO_100_18", "HEXACO_100_42_R", "HEXACO_100_66_R", "HEXACO_100_90_R", "HEXACO_100_24", "HEXACO_100_48", "HEXACO_100_72_R", "HEXACO_100_96_R")			
Emotionality_100 <-c("HEXACO_100_5", "HEXACO_100_29_R", "HEXACO_100_53", "HEXACO_100_77_R", "HEXACO_100_11", "HEXACO_100_35_R", "HEXACO_100_59_R", "HEXACO_100_83", "HEXACO_100_17", "HEXACO_100_41_R", "HEXACO_100_65", "HEXACO_100_89_R", "HEXACO_100_23", "HEXACO_100_47", "HEXACO_100_71", "HEXACO_100_95_R")			
Extraversion_100 <-c("HEXACO_100_4", "HEXACO_100_28", "HEXACO_100_52_R", "HEXACO_100_76_R", "HEXACO_100_10_R", "HEXACO_100_34", "HEXACO_100_58", "HEXACO_100_82_R", "HEXACO_100_16_R", "HEXACO_100_40", "HEXACO_100_64", "HEXACO_100_88", "HEXACO_100_22", "HEXACO_100_46", "HEXACO_100_70_R", "HEXACO_100_94_R")			
Agreeableness_100 <-c("HEXACO_100_3", "HEXACO_100_27", "HEXACO_100_51_R", "HEXACO_100_75_R", "HEXACO_100_9_R", "HEXACO_100_33", "HEXACO_100_57", "HEXACO_100_81", "HEXACO_100_15_R", "HEXACO_100_39", "HEXACO_100_63_R", "HEXACO_100_87_R", "HEXACO_100_21_R", "HEXACO_100_45", "HEXACO_100_69", "HEXACO_100_93_R")			
Conscientiousness_100 <-c("HEXACO_100_2", "HEXACO_100_26", "HEXACO_100_50_R", "HEXACO_100_74_R", "HEXACO_100_8", "HEXACO_100_32", "HEXACO_100_56_R", "HEXACO_100_80_R", "HEXACO_100_14", "HEXACO_100_38_R", "HEXACO_100_62", "HEXACO_100_86", "HEXACO_100_20_R", "HEXACO_100_44_R", "HEXACO_100_68", "HEXACO_100_92_R")			
Openness_100 <-c("HEXACO_100_1_R", "HEXACO_100_25_R", "HEXACO_100_49", "HEXACO_100_73", "HEXACO_100_7", "HEXACO_100_31", "HEXACO_100_55_R", "HEXACO_100_79_R", "HEXACO_100_13_R", "HEXACO_100_37", "HEXACO_100_61", "HEXACO_100_85_R", "HEXACO_100_19_R", "HEXACO_100_43", "HEXACO_100_67", "HEXACO_100_91_R") 				

HonestyHumility_100 <- total_agg [HonestyHumility_100]
Emotionality_100 <- total_agg [Emotionality_100]
Extraversion_100 <- total_agg [Extraversion_100]
Agreeableness_100 <- total_agg [Agreeableness_100]
Conscientiousness_100 <- total_agg [Conscientiousness_100]
Openness_100 <- total_agg [Openness_100]

alpha (HonestyHumility_100)
alpha (Emotionality_100)
alpha (Extraversion_100)
alpha (Agreeableness_100)
alpha (Conscientiousness_100)
alpha (Openness_100)

SDQPhAbilities <- c("SDQ_13", "SDQ_26_R", "SDQ_39", "SDQ_52_R", "SDQ_65", "SDQ_78_R", "SDQ_91", "SDQ_104_R", "SDQ_117", "SDQ_130")				
SDQPhAppereance <- c("SDQ_11", "SDQ_24_R", "SDQ_37", "SDQ_50_R", "SDQ_63", "SDQ_76_R", "SDQ_89", "SDQ_102_R", "SDQ_115_R", "SDQ_128")				
SDQGenEsteem <- c("SDQ_3", "SDQ_16_R", "SDQ_29", "SDQ_42_R", "SDQ_55", "SDQ_68", "SDQ_81_R", "SDQ_94", "SDQ_107_R", "SDQ_120_R", "SDQ_131", "SDQ_135_R")	

SDQPhAbilities <- total_agg[SDQPhAbilities]
SDQPhAppereance <- total_agg[SDQPhAppereance]
SDQGenEsteem <- total_agg[SDQGenEsteem]

alpha (SDQPhAbilities)
alpha (SDQPhAppereance)
alpha (SDQGenEsteem)

PrevFokus <-c("reg_fit1", "reg_fit10", "reg_fit11", "reg_fit13", "reg_fit15", "reg_fit2", "reg_fit4", "reg_fit7", "reg_fit9")  				
PromoFokus <-c("reg_fit12", "reg_fit14", "reg_fit16", "reg_fit17", "reg_fit18", "reg_fit3", "reg_fit5", "reg_fit6", "reg_fit8")	

PrevFokus <- total_agg[PrevFokus]
PromoFokus <- total_agg[PromoFokus]

alpha (PrevFokus)
alpha (PromoFokus)

#####		stacked charts unap	#####.

unap_stack <-podaci[c("ID","unap", "Voda", "Voce", "Povrce", "Vezbanje", "DodaciIshrani")]

unap_stack$Voda <- factor(unap_stack$Voda, levels = c(1,2), labels = c("Ne", "Da"))
unap_stack$Voce <- factor(unap_stack$Voce, levels = c(1,2), labels = c("Ne", "Da"))
unap_stack$Povrce <- factor(unap_stack$Povrce, levels = c(1,2), labels = c("Ne", "Da"))
unap_stack$Vezbanje <- factor(unap_stack$Vezbanje, levels = c(1,2), labels = c("Ne", "Da"))
unap_stack$DodaciIshrani <- factor(unap_stack$DodaciIshrani, levels = c(1,2), labels = c("Ne", "Da"))

unap_stack <- rename(unap_stack, c(unap ="Ukupno"))
unap_stack <- rename(unap_stack, c(DodaciIshrani ="Dodaci_ishrani"))
unap_stack_m <- melt(unap_stack, id="ID")
unap_stack_m_t <- table(unap_stack_m$variable, unap_stack_m$value)
unap_stack_m_t<-prop.table(unap_stack_m_t, 1) 
unap_stack_m_t_m <- melt(unap_stack_m_t)
unap_stack_m_t_m$value <- unap_stack_m_t_m$value*100
unap_stack_m_t_m$value <- round(unap_stack_m_t_m$value, digits=1)


library(plyr)

p4 <- ggplot() + geom_bar(aes(y = value, x = Var1, fill = forcats::fct_rev(Var2)), data = unap_stack_m_t_m, stat="identity")
p4 <- p4 + geom_text(data=unap_stack_m_t_m, aes(x = Var1, y = value, label = paste0(value, "%")), size=4)
unap_stack_m_t_m <- ddply(unap_stack_m_t_m, .(Var1), transform, pos = cumsum(value) - (0.5 * value))
p4 <- ggplot() + geom_bar(aes(y = value, x = Var1, fill = forcats::fct_rev(Var2)), data = unap_stack_m_t_m, stat="identity")
p4 <- p4 + geom_text(data=unap_stack_m_t_m, aes(x = Var1, y = pos, label = paste0(value,"%")), size=4)
p4 <- p4 + theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank(), 
	axis.title.x=element_blank(), 
	axis.title.y=element_blank(),
	axis.ticks.y=element_blank(),
	axis.text.y=element_blank())
p4

#####		stacked charts unap.nivo ispitanika	#####

Unap_ikad_agg <- aggregate(unap_ikad~IdIspitanika, podaci, mean, na.rm=TRUE)
Voda_ikad_agg <- aggregate(Voda_ikad~IdIspitanika, podaci, mean, na.rm=TRUE)
Voce_ikad_agg <- aggregate(Voce_ikad~IdIspitanika, podaci, mean, na.rm=TRUE)
Povrce_ikad_agg <- aggregate(Povrce_ikad~IdIspitanika, podaci, mean, na.rm=TRUE)
Vezbanje_ikad_agg <- aggregate(Vezbanje_ikad~IdIspitanika, podaci, mean, na.rm=TRUE)
DodaciIshrani_ikad_agg <- aggregate(DodaciIshrani_ikad~IdIspitanika, podaci, mean, na.rm=TRUE)

Unap_ikad_agg <- merge(Unap_ikad_agg,Voda_ikad_agg,by="IdIspitanika")
Unap_ikad_agg <- merge(Unap_ikad_agg,Voce_ikad_agg,by="IdIspitanika")
Unap_ikad_agg <- merge(Unap_ikad_agg,Povrce_ikad_agg,by="IdIspitanika")
Unap_ikad_agg <- merge(Unap_ikad_agg,Vezbanje_ikad_agg,by="IdIspitanika")
Unap_ikad_agg <- merge(Unap_ikad_agg,DodaciIshrani_ikad_agg,by="IdIspitanika")

head(Unap_ikad_agg)

Unap_ikad_agg$unap_ikad <- factor(Unap_ikad_agg$unap_ikad, levels = c(1,2), labels = c("Da", "Ne"))
Unap_ikad_agg$Voda_ikad <- factor(Unap_ikad_agg$Voda_ikad, levels = c(1,2), labels = c("Ne", "Da"))
Unap_ikad_agg$Voce_ikad <- factor(Unap_ikad_agg$Voce_ikad, levels = c(1,2), labels = c("Ne", "Da"))
Unap_ikad_agg$Povrce_ikad <- factor(Unap_ikad_agg$Povrce_ikad, levels = c(1,2), labels = c("Ne", "Da"))
Unap_ikad_agg$Vezbanje_ikad <- factor(Unap_ikad_agg$Vezbanje_ikad, levels = c(1,2), labels = c("Ne", "Da"))
Unap_ikad_agg$DodaciIshrani_ikad <- factor(Unap_ikad_agg$DodaciIshrani_ikad, levels = c(1,2), labels = c("Ne", "Da"))

library(plyr)

Unap_ikad_agg <- rename(Unap_ikad_agg, c(unap_ikad ="Ukupno"))
Unap_ikad_agg <- rename(Unap_ikad_agg, c(Voda_ikad ="Voda"))
Unap_ikad_agg <- rename(Unap_ikad_agg, c(Voce_ikad ="Voce"))
Unap_ikad_agg <- rename(Unap_ikad_agg, c(Povrce_ikad ="Povrce"))
Unap_ikad_agg <- rename(Unap_ikad_agg, c(Vezbanje_ikad ="Vezbanje"))
Unap_ikad_agg <- rename(Unap_ikad_agg, c(DodaciIshrani_ikad ="DodaciIshrani"))

Unap_ikad_agg_m <- melt(Unap_ikad_agg, id="IdIspitanika")
Unap_ikad_agg_m_t <- table(Unap_ikad_agg_m$variable, Unap_ikad_agg_m$value)
Unap_ikad_agg_m_t<-prop.table(Unap_ikad_agg_m_t, 1) 
Unap_ikad_agg_m_t_m <- melt(Unap_ikad_agg_m_t)
Unap_ikad_agg_m_t_m$value <- Unap_ikad_agg_m_t_m$value*100
Unap_ikad_agg_m_t_m$value <- round(Unap_ikad_agg_m_t_m$value, digits=1)

library(plyr)

p4.1 <- ggplot() + geom_bar(aes(y = value, x = Var1, fill = forcats::fct_rev(Var2)), data = Unap_ikad_agg_m_t_m, stat="identity")
p4.1 <- p4.1 + geom_text(data=Unap_ikad_agg_m_t_m, aes(x = Var1, y = value, label = paste0(value, "%")), size=4)
Unap_ikad_agg_m_t_m <- ddply(Unap_ikad_agg_m_t_m, .(Var1), transform, pos = cumsum(value) - (0.5 * value))
p4.1 <- ggplot() + geom_bar(aes(y = value, x = Var1, fill = forcats::fct_rev(Var2)), data = Unap_ikad_agg_m_t_m, stat="identity")
p4.1 <- p4.1 + geom_text(data=Unap_ikad_agg_m_t_m, aes(x = Var1, y = pos, label = paste0(value,"%")), size=4)
p4.1 <- p4.1 + theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank(), 
	axis.title.x=element_blank(), 
	axis.title.y=element_blank(),
	axis.ticks.y=element_blank(),
	axis.text.y=element_blank())
p4.1

#####		stacked charts unap.trebalo	#####

treb_stack <-podaci[c("ID","unap_trebalo", "Voda_trebalo", "Voce_trebalo", "Povrce_trebalo", "Vezbanje_trebalo", "DodaciIshrani_trebalo")] 

treb_stack[is.na(treb_stack)] <- 3

treb_stack$unap_trebalo <- factor(treb_stack$unap_trebalo, levels = c(1,2,3), labels = c("Nije trebalo", "Jeste trebalo", "Izvedeno"))
treb_stack$Voda_trebalo <- factor(treb_stack$Voda_trebalo, levels = c(1,2,3), labels = c("Nije trebalo", "Jeste trebalo", "Izvedeno"))
treb_stack$Voce_trebalo <- factor(treb_stack$Voce_trebalo, levels = c(1,2,3), labels = c("Nije trebalo", "Jeste trebalo", "Izvedeno"))
treb_stack$Povrce_trebalo <- factor(treb_stack$Povrce_trebalo, levels = c(1,2,3), labels = c("Nije trebalo", "Jeste trebalo", "Izvedeno"))
treb_stack$Vezbanje_trebalo <- factor(treb_stack$Vezbanje_trebalo, levels = c(1,2,3), labels = c("Nije trebalo", "Jeste trebalo", "Izvedeno"))
treb_stack$DodaciIshrani_trebalo <- factor(treb_stack$DodaciIshrani_trebalo, levels = c(1,2,3), labels = c("Nije trebalo", "Jeste trebalo", "Izvedeno"))

treb_stack <- rename(treb_stack, c(unap_trebalo ="Ukupno"))
treb_stack <- rename(treb_stack, c(Voda_trebalo ="Voda"))
treb_stack <- rename(treb_stack, c(Voce_trebalo ="Voce"))
treb_stack <- rename(treb_stack, c(Povrce_trebalo ="Povrce"))
treb_stack <- rename(treb_stack, c(Vezbanje_trebalo ="Vezbanje"))
treb_stack <- rename(treb_stack, c(DodaciIshrani_trebalo ="Dodaci_ishrani"))

treb_stack_m <- melt(treb_stack, id="ID")
treb_stack_m_t <- table(treb_stack_m$variable, treb_stack_m$value)
treb_stack_m_t<-prop.table(treb_stack_m_t, 1) 
treb_stack_m_t_m <- melt(treb_stack_m_t)
treb_stack_m_t_m$value <- treb_stack_m_t_m$value*100
treb_stack_m_t_m$value <- round(treb_stack_m_t_m$value, digits=1)

library(plyr)

p6 <- ggplot() + geom_bar(aes(y = value, x = Var1, fill = forcats::fct_rev(Var2)), data = treb_stack_m_t_m, stat="identity")
p6 <- p6 + geom_text(data=treb_stack_m_t_m, aes(x = Var1, y = value, label = paste0(value, "%")), size=4)
treb_stack_m_t_m <- ddply(treb_stack_m_t_m, .(Var1), transform, pos = cumsum(value) - (0.5 * value))
p6 <- ggplot() + geom_bar(aes(y = value, x = Var1, fill = forcats::fct_rev(Var2)), data = treb_stack_m_t_m, stat="identity")
p6 <- p6 + geom_text(data=treb_stack_m_t_m, aes(x = Var1, y = pos, label = paste0(value,"%")), size=4)
p6 <- p6 + theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank(), 
	axis.title.x=element_blank(), 
	axis.title.y=element_blank(),
	axis.ticks.y=element_blank(),
	axis.text.y=element_blank())
p6


ugr_stack <-podaci[c("ID","ugr", "Kafa", "Alkohol", "Cigarete", "BrzaHrana", "Slatkisi")]
head(ugr_stack)
ugr_stack$Kafa <- factor(ugr_stack$Kafa, levels = c(1,2), labels = c("Ne", "Da"))
ugr_stack$Alkohol <- factor(ugr_stack$Alkohol, levels = c(1,2), labels = c("Ne", "Da"))
ugr_stack$Cigarete <- factor(ugr_stack$Cigarete, levels = c(1,2), labels = c("Ne", "Da"))
ugr_stack$BrzaHrana <- factor(ugr_stack$BrzaHrana, levels = c(1,2), labels = c("Ne", "Da"))
ugr_stack$Slatkisi <- factor(ugr_stack$Slatkisi, levels = c(1,2), labels = c("Ne", "Da"))

ugr_stack <- rename(ugr_stack, c(ugr ="Ukupno"))
ugr_stack <- rename(ugr_stack, c(BrzaHrana ="Brza_hrana"))
ugr_stack_m <- melt(ugr_stack, id="ID")
ugr_stack_m_t <- table(ugr_stack_m$variable, ugr_stack_m$value)
ugr_stack_m_t<-prop.table(ugr_stack_m_t, 1) 
ugr_stack_m_t_m <- melt(ugr_stack_m_t)
ugr_stack_m_t_m$value <- ugr_stack_m_t_m$value*100
ugr_stack_m_t_m$value <- round(ugr_stack_m_t_m$value, digits=1)

library(plyr)

p5 <- ggplot() + geom_bar(aes(y = value, x = Var1, fill = forcats::fct_rev(Var2)), data = ugr_stack_m_t_m, stat="identity")
p5 <- p5 + geom_text(data=ugr_stack_m_t_m, aes(x = Var1, y = value, label = paste0(value, "%")), size=4)
ugr_stack_m_t_m <- ddply(ugr_stack_m_t_m, .(Var1), transform, pos = cumsum(value) - (0.5 * value))
p5 <- ggplot() + geom_bar(aes(y = value, x = Var1, fill = forcats::fct_rev(Var2)), data = ugr_stack_m_t_m, stat="identity")
p5 <- p5 + geom_text(data=ugr_stack_m_t_m, aes(x = Var1, y = pos, label = paste0(value,"%")), size=4)
p5 <- p5 + theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank(), 
	axis.title.x=element_blank(), 
	axis.title.y=element_blank(),
	axis.ticks.y=element_blank(),
	axis.text.y=element_blank())
p5

# nivo ispitanika

ugr_ikad_agg <- aggregate(ugr_ikad~IdIspitanika, podaci, mean, na.rm=TRUE)
Kafa_ikad_agg <- aggregate(Kafa_ikad~IdIspitanika, podaci, mean, na.rm=TRUE)
Alkohol_ikad_agg <- aggregate(Alkohol_ikad~IdIspitanika, podaci, mean, na.rm=TRUE)
Cigarete_ikad_agg <- aggregate(Cigarete_ikad~IdIspitanika, podaci, mean, na.rm=TRUE)
BrzaHrana_ikad_agg <- aggregate(BrzaHrana_ikad~IdIspitanika, podaci, mean, na.rm=TRUE)
Slatkisi_ikad_agg <- aggregate(Slatkisi_ikad~IdIspitanika, podaci, mean, na.rm=TRUE)

ugr_ikad_agg <- merge(ugr_ikad_agg,Kafa_ikad_agg,by="IdIspitanika")
ugr_ikad_agg <- merge(ugr_ikad_agg,Alkohol_ikad_agg,by="IdIspitanika")
ugr_ikad_agg <- merge(ugr_ikad_agg,Cigarete_ikad_agg,by="IdIspitanika")
ugr_ikad_agg <- merge(ugr_ikad_agg,BrzaHrana_ikad_agg,by="IdIspitanika")
ugr_ikad_agg <- merge(ugr_ikad_agg,Slatkisi_ikad_agg,by="IdIspitanika")

head(ugr_ikad_agg)

ugr_ikad_agg$ugr_ikad <- factor(ugr_ikad_agg$ugr_ikad, levels = c(1,2), labels = c("Da", "Ne"))
ugr_ikad_agg$Kafa_ikad <- factor(ugr_ikad_agg$Kafa_ikad, levels = c(1,2), labels = c("Ne", "Da"))
ugr_ikad_agg$Alkohol_ikad <- factor(ugr_ikad_agg$Alkohol_ikad, levels = c(1,2), labels = c("Ne", "Da"))
ugr_ikad_agg$Cigarete_ikad <- factor(ugr_ikad_agg$Cigarete_ikad, levels = c(1,2), labels = c("Ne", "Da"))
ugr_ikad_agg$BrzaHrana_ikad <- factor(ugr_ikad_agg$BrzaHrana_ikad, levels = c(1,2), labels = c("Ne", "Da"))
ugr_ikad_agg$Slatkisi_ikad <- factor(ugr_ikad_agg$Slatkisi_ikad, levels = c(1,2), labels = c("Ne", "Da"))

library(plyr)

ugr_ikad_agg <- rename(ugr_ikad_agg, c(ugr_ikad ="Ukupno"))
ugr_ikad_agg <- rename(ugr_ikad_agg, c(Kafa_ikad ="Kafa"))
ugr_ikad_agg <- rename(ugr_ikad_agg, c(Alkohol_ikad ="Alkohol"))
ugr_ikad_agg <- rename(ugr_ikad_agg, c(Cigarete_ikad ="Cigarete"))
ugr_ikad_agg <- rename(ugr_ikad_agg, c(BrzaHrana_ikad ="Brza_Hrana"))
ugr_ikad_agg <- rename(ugr_ikad_agg, c(Slatkisi_ikad ="Slatkisi"))

ugr_ikad_agg_m <- melt(ugr_ikad_agg, id="IdIspitanika")
ugr_ikad_agg_m_t <- table(ugr_ikad_agg_m$variable, ugr_ikad_agg_m$value)
ugr_ikad_agg_m_t<-prop.table(ugr_ikad_agg_m_t, 1) 
ugr_ikad_agg_m_t_m <- melt(ugr_ikad_agg_m_t)
ugr_ikad_agg_m_t_m$value <- ugr_ikad_agg_m_t_m$value*100
ugr_ikad_agg_m_t_m$value <- round(ugr_ikad_agg_m_t_m$value, digits=1)

library(plyr)

p5.1 <- ggplot() + geom_bar(aes(y = value, x = Var1, fill = forcats::fct_rev(Var2)), data = ugr_ikad_agg_m_t_m, stat="identity")
p5.1 <- p5.1 + geom_text(data=ugr_ikad_agg_m_t_m, aes(x = Var1, y = value, label = paste0(value, "%")), size=4)
ugr_ikad_agg_m_t_m <- ddply(ugr_ikad_agg_m_t_m, .(Var1), transform, pos = cumsum(value) - (0.5 * value))
p5.1 <- ggplot() + geom_bar(aes(y = value, x = Var1, fill = forcats::fct_rev(Var2)), data = ugr_ikad_agg_m_t_m, stat="identity")
p5.1 <- p5.1 + geom_text(data=ugr_ikad_agg_m_t_m, aes(x = Var1, y = pos, label = paste0(value,"%")), size=4)
p5.1 <- p5.1 + theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank(), 
	axis.title.x=element_blank(), 
	axis.title.y=element_blank(),
	axis.ticks.y=element_blank(),
	axis.text.y=element_blank())
p5.1

isk_stack <-podaci[c("ID", "ugr_iskusenje_b", "Kafa_iskusenje_b", "Alkohol_iskusenje_b", "Cigarete_iskusenje_b", "BrzaHrana_iskusenje_b", "Slatkisi_iskusenje_b")] 
isk_stack[is.na(isk_stack)] <- 3

str(isk_stack)
head(isk_stack)

isk_stack <- rename(isk_stack, c(ugr_iskusenje_b ="Ukupno"))
isk_stack <- rename(isk_stack, c(Kafa_iskusenje_b ="Kafa"))
isk_stack <- rename(isk_stack, c(Alkohol_iskusenje_b ="Alkohol"))
isk_stack <- rename(isk_stack, c(Cigarete_iskusenje_b ="Cigarete"))
isk_stack <- rename(isk_stack, c(BrzaHrana_iskusenje_b ="Brza_hrana"))
isk_stack <- rename(isk_stack, c(Slatkisi_iskusenje_b ="Slatkisi"))

isk_stack$Ukupno <- factor(isk_stack$Ukupno, levels = c(0,1,3), labels = c("Ne", "Da", "Bez odgovora"))
isk_stack$Kafa <- factor(isk_stack$Kafa, levels = c(0,1,3), labels = c("Ne", "Da", "Bez odgovora"))
isk_stack$Alkohol <- factor(isk_stack$Alkohol, levels = c(0,1,3), labels = c("Ne", "Da", "Bez odgovora"))
isk_stack$Cigarete <- factor(isk_stack$Cigarete, levels = c(0,1,3), labels = c("Ne", "Da", "Bez odgovora"))
isk_stack$Brza_hrana <- factor(isk_stack$Brza_hrana, levels = c(0,1,3), labels = c("Ne", "Da", "Bez odgovora"))
isk_stack$Slatkisi <- factor(isk_stack$Slatkisi, levels = c(0,1,3), labels = c("Ne", "Da", "Bez odgovora"))

isk_stack_m <- melt(isk_stack, id="ID")
isk_stack_m_t <- table(isk_stack_m$variable, isk_stack_m$value)
isk_stack_m_t<-prop.table(isk_stack_m_t, 1) 
isk_stack_m_t_m <- melt(isk_stack_m_t)
isk_stack_m_t_m$value <- isk_stack_m_t_m$value*100
isk_stack_m_t_m$value <- round(isk_stack_m_t_m$value, digits=0)

library(plyr)

p7 <- ggplot() + geom_bar(aes(y = value, x = Var1, fill = forcats::fct_rev(Var2)), data = isk_stack_m_t_m, stat="identity")
p7 <- p7 + geom_text(data=isk_stack_m_t_m, aes(x = Var1, y = value, label = paste0(value, "%")), size=4)
isk_stack_m_t_m <- ddply(isk_stack_m_t_m, .(Var1), transform, pos = cumsum(value) - (0.5 * value))
p7 <- ggplot() + geom_bar(aes(y = value, x = Var1, fill = forcats::fct_rev(Var2)), data = isk_stack_m_t_m, stat="identity")
p7 <- p7 + geom_text(data=isk_stack_m_t_m, aes(x = Var1, y = pos, label = paste0(value,"%")), size=4)
p7 <- p7 + theme(legend.position="bottom", legend.direction="horizontal", 
	legend.title = element_blank(), 
	axis.title.x=element_blank(), 
	axis.title.y=element_blank(),
	axis.ticks.y=element_blank(),
	axis.text.y=element_blank())
p7



############################################.


#####		tabele sa prosecima		#####

# izvodjenje

unap_koliko_agg <- aggregate(unap_koliko~IdIspitanika, podaci, mean, na.rm=TRUE)
Voda_koliko_agg <- aggregate(Voda_koliko~IdIspitanika, podaci, mean, na.rm=TRUE)
Voce_koliko_agg <- aggregate(Voce_koliko~IdIspitanika, podaci, mean, na.rm=TRUE)
Povrce_koliko_agg <- aggregate(Povrce_koliko~IdIspitanika, podaci, mean, na.rm=TRUE)
Vezbanje_koliko_agg <- aggregate(Vezbanje_koliko~IdIspitanika, podaci, mean, na.rm=TRUE)
DodaciIshrani_koliko_agg <- aggregate(DodaciIshrani_koliko~IdIspitanika, podaci, mean, na.rm=TRUE)

describe (unap_koliko_agg)
describe (Voda_koliko_agg)
describe (Voce_koliko_agg)
describe (Povrce_koliko_agg)
describe (Vezbanje_koliko_agg)
describe (DodaciIshrani_koliko_agg)

# da, afekat

unap_da_afek_agg <- aggregate(unap_da_afek~IdIspitanika, podaci, mean, na.rm=TRUE)
Voda_da_afek_agg <- aggregate(Voda_da_afek~IdIspitanika, podaci, mean, na.rm=TRUE)
Voce_da_afek_agg <- aggregate(Voce_da_afek~IdIspitanika, podaci, mean, na.rm=TRUE)
Povrce_da_afek_agg <- aggregate(Povrce_da_afek~IdIspitanika, podaci, mean, na.rm=TRUE)
Vezbanje_da_afek_agg <- aggregate(Vezbanje_da_afek~IdIspitanika, podaci, mean, na.rm=TRUE)
DodaciIshrani_da_afek_agg <- aggregate(DodaciIshrani_da_afek~IdIspitanika, podaci, mean, na.rm=TRUE)

describe (unap_da_afek_agg)
describe (Voda_da_afek_agg)
describe (Voce_da_afek_agg)
describe (Povrce_da_afek_agg)
describe (Vezbanje_da_afek_agg)
describe (DodaciIshrani_da_afek_agg)

# ne, afekat
unap_ne_afek_c_agg <- aggregate(unap_ne_afek_c~IdIspitanika, podaci, mean, na.rm=TRUE)
Voda_ne_afek_c_agg <- aggregate(Voda_ne_afek_c~IdIspitanika, podaci, mean, na.rm=TRUE)
Voce_ne_afek_c_agg <- aggregate(Voce_ne_afek_c~IdIspitanika, podaci, mean, na.rm=TRUE)
Povrce_ne_afek_c_agg <- aggregate(Povrce_ne_afek_c~IdIspitanika, podaci, mean, na.rm=TRUE)
Vezbanje_ne_afek_c_agg <- aggregate(Vezbanje_ne_afek_c~IdIspitanika, podaci, mean, na.rm=TRUE)
DodaciIshrani_ne_afek_c_agg <- aggregate(DodaciIshrani_ne_afek_c~IdIspitanika, podaci, mean, na.rm=TRUE)

describe (unap_ne_afek_c_agg)
describe (Voda_ne_afek_c_agg)
describe (Voce_ne_afek_c_agg)
describe (Povrce_ne_afek_c_agg)
describe (Vezbanje_ne_afek_c_agg)
describe (DodaciIshrani_ne_afek_c_agg)

# izvodjenje

ugr_koliko_agg <- aggregate(ugr_koliko~IdIspitanika, podaci, mean, na.rm=TRUE)
Kafa_koliko_agg <- aggregate(Kafa_koliko~IdIspitanika, podaci, mean, na.rm=TRUE)
Alkohol_koliko_agg <- aggregate(Alkohol_koliko~IdIspitanika, podaci, mean, na.rm=TRUE)
Cigarete_koliko_agg <- aggregate(Cigarete_koliko~IdIspitanika, podaci, mean, na.rm=TRUE)
BrzaHrana_koliko_agg <- aggregate(BrzaHrana_koliko~IdIspitanika, podaci, mean, na.rm=TRUE)
Slatkisi_koliko_agg <- aggregate(Slatkisi_koliko~IdIspitanika, podaci, mean, na.rm=TRUE)

describe (ugr_koliko_agg)
describe (Kafa_koliko_agg)
describe (Alkohol_koliko_agg)
describe (Cigarete_koliko_agg)
describe (BrzaHrana_koliko_agg)
describe (Slatkisi_koliko_agg)

# da, afekat

ugr_da_afek_agg <- aggregate(ugr_da_afek~IdIspitanika, podaci, mean, na.rm=TRUE)
Kafa_da_afek_agg <- aggregate(Kafa_da_afek~IdIspitanika, podaci, mean, na.rm=TRUE)
Alkohol_da_afek_agg <- aggregate(Alkohol_da_afek~IdIspitanika, podaci, mean, na.rm=TRUE)
Cigarete_da_afek_agg <- aggregate(Cigarete_da_afek~IdIspitanika, podaci, mean, na.rm=TRUE)
BrzaHrana_da_afek_agg <- aggregate(BrzaHrana_da_afek~IdIspitanika, podaci, mean, na.rm=TRUE)
Slatkisi_da_afek_agg <- aggregate(Slatkisi_da_afek~IdIspitanika, podaci, mean, na.rm=TRUE)

describe (ugr_da_afek_agg)
describe (Kafa_da_afek_agg)
describe (Alkohol_da_afek_agg)
describe (Cigarete_da_afek_agg)
describe (BrzaHrana_da_afek_agg)
describe (Slatkisi_da_afek_agg)

# ne, afekat
ugr_ne_afek_c_agg <- aggregate(ugr_ne_afek_c~IdIspitanika, podaci, mean, na.rm=TRUE)
Kafa_ne_afek_c_agg <- aggregate(Kafa_ne_afek_c~IdIspitanika, podaci, mean, na.rm=TRUE)
Alkohol_ne_afek_c_agg <- aggregate(Alkohol_ne_afek_c~IdIspitanika, podaci, mean, na.rm=TRUE)
Cigarete_ne_afek_c_agg <- aggregate(Cigarete_ne_afek_c~IdIspitanika, podaci, mean, na.rm=TRUE)
BrzaHrana_ne_afek_c_agg <- aggregate(BrzaHrana_ne_afek_c~IdIspitanika, podaci, mean, na.rm=TRUE)
Slatkisi_ne_afek_c_agg <- aggregate(Slatkisi_ne_afek_c~IdIspitanika, podaci, mean, na.rm=TRUE)

describe (ugr_ne_afek_c_agg)
describe (Kafa_ne_afek_c_agg)
describe (Alkohol_ne_afek_c_agg)
describe (Cigarete_ne_afek_c_agg)
describe (BrzaHrana_ne_afek_c_agg)
describe (Slatkisi_ne_afek_c_agg)

unap_koliko_agg_1 <- aggregate(unap_koliko~DanUSedmici, podaci, mean, na.rm=TRUE)
unap_koliko_agg_2 <- aggregate(unap_koliko~Period, podaci, mean, na.rm=TRUE)

unap_da_afek_agg <- aggregate(unap_da_afek~IdIspitanika, podaci, mean, na.rm=TRUE)
unap_da_afek_agg_1 <- aggregate(unap_da_afek~DanUSedmici, podaci, mean, na.rm=TRUE)
unap_da_afek_agg_2 <- aggregate(unap_da_afek~Period, podaci, mean, na.rm=TRUE)

unap_ne_afek_agg <- aggregate(unap_ne_afek~IdIspitanika, podaci, mean, na.rm=TRUE)
unap_ne_afek_agg_1 <- aggregate(unap_ne_afek~DanUSedmici, podaci, mean, na.rm=TRUE)
unap_ne_afek_agg_2 <- aggregate(unap_ne_afek~Period, podaci, mean, na.rm=TRUE)

poz_afek_agg <- aggregate(poz_afek~IdIspitanika, podaci, mean, na.rm=TRUE)
poz_afek_agg_1 <- aggregate(poz_afek~DanUSedmici, podaci, mean, na.rm=TRUE)
poz_afek_agg_2 <- aggregate(poz_afek~Period, podaci, mean, na.rm=TRUE)

ugr_koliko_agg <- aggregate(ugr_koliko~IdIspitanika, podaci, mean, na.rm=TRUE)
ugr_koliko_agg_1 <- aggregate(ugr_koliko~DanUSedmici, podaci, mean, na.rm=TRUE)
ugr_koliko_agg_2 <- aggregate(ugr_koliko~Period, podaci, mean, na.rm=TRUE)

ugr_da_afek_agg <- aggregate(ugr_da_afek~IdIspitanika, podaci, mean, na.rm=TRUE)
ugr_da_afek_agg_1 <- aggregate(ugr_da_afek~DanUSedmici, podaci, mean, na.rm=TRUE)
ugr_da_afek_agg_2 <- aggregate(ugr_da_afek~Period, podaci, mean, na.rm=TRUE)

ugr_ne_afek_agg <- aggregate(ugr_ne_afek~IdIspitanika, podaci, mean, na.rm=TRUE)
ugr_ne_afek_agg_1 <- aggregate(ugr_ne_afek~DanUSedmici, podaci, mean, na.rm=TRUE)
ugr_ne_afek_agg_2 <- aggregate(ugr_ne_afek~Period, podaci, mean, na.rm=TRUE)

neg_afek_agg <- aggregate(neg_afek~IdIspitanika, podaci, mean, na.rm=TRUE)
neg_afek_agg_1 <- aggregate(neg_afek~DanUSedmici, podaci, mean, na.rm=TRUE)
neg_afek_agg_2 <- aggregate(neg_afek~Period, podaci, mean, na.rm=TRUE)

total_afek_agg <- aggregate(total_afek~IdIspitanika, podaci, mean, na.rm=TRUE)
total_afek_agg_1 <- aggregate(total_afek~DanUSedmici, podaci, mean, na.rm=TRUE)
total_afek_agg_2 <- aggregate(total_afek~Period, podaci, mean, na.rm=TRUE)

Desavanja_agg <- aggregate(Desavanja~IdIspitanika, podaci, mean, na.rm=TRUE)
Desavanja_agg_1 <- aggregate(Desavanja~DanUSedmici, podaci, mean, na.rm=TRUE)
Desavanja_agg_2 <- aggregate(Desavanja~Period, podaci, mean, na.rm=TRUE)

Umor_agg <- aggregate(Umor~IdIspitanika, podaci, mean, na.rm=TRUE)
Umor_agg_1 <- aggregate(Umor~DanUSedmici, podaci, mean, na.rm=TRUE)
Umor_agg_2 <- aggregate(Umor~Period, podaci, mean, na.rm=TRUE)


describe (unap_koliko_agg_1)
describe (unap_koliko_agg_2)
describe (unap_da_afek_agg)
describe (unap_da_afek_agg_1)
describe (unap_da_afek_agg_2)
describe (unap_ne_afek_agg)
describe (unap_ne_afek_agg_1)
describe (unap_ne_afek_agg_2)
describe (poz_afek_agg)
describe (poz_afek_agg_1)
describe (poz_afek_agg_2)
describe (ugr_koliko_agg)
describe (ugr_koliko_agg_1)
describe (ugr_koliko_agg_2)
describe (ugr_da_afek_agg)
describe (ugr_da_afek_agg_1)
describe (ugr_da_afek_agg_2)
describe (ugr_ne_afek_agg)
describe (ugr_ne_afek_agg_1)
describe (ugr_ne_afek_agg_2)
describe (neg_afek_agg)
describe (neg_afek_agg_1)
describe (neg_afek_agg_2)
describe (total_afek_agg)
describe (total_afek_agg_1)
describe (total_afek_agg_2)
describe (Desavanja_agg)
describe (Desavanja_agg_1)
describe (Desavanja_agg_2)
describe (Umor_agg)
describe (Umor_agg_1)
describe (Umor_agg_2)


# ANOVA za ponovljena merenja (dani i intervali)

unap_koliko_ic_m<- c("IdIspitanika", "DanUSedmici","unap_koliko_ic")
unap_koliko_ic_m <-podaci[unap_koliko_ic_m]
unap_koliko_ic_m <- na.omit(unap_koliko_ic_m)
unap_koliko_ic_m$IdIspitanika <- as.factor(unap_koliko_ic_m$IdIspitanika) 


qplot(x=factor(DanUSedmici), y=unap_koliko_ic, data=podaci, geom="boxplot", ylab="Zdravstveno unapredujuce ponašanje")
test1 <- lme(unap_koliko_ic ~ 1 + DanUSedmici, 
              random= ~ 1 |IdIspitanika, 
		  method ="ML",
              data=podaci,
		  control=ctrl,
              na.action=na.exclude)
summary(test1)


qplot(x=factor(Period), y=unap_koliko_ic, data=podaci, geom="boxplot", ylab="Zdravstveno unapredujuce ponašanje")
test2 <- lme(unap_koliko_ic ~ 1 + Period, 
              random= ~ 1 |IdIspitanika, 
		  method ="ML",
              data=podaci,
		  control=ctrl,
              na.action=na.exclude)
summary(test2)

qplot(x=factor(DanUSedmici), y=ugr_koliko_ic, data=podaci, geom="boxplot", ylab="Zdravstveno ugrožavajuce ponašanje")
test3 <- lme(ugr_koliko_ic ~ 1 + DanUSedmici, 
              random= ~ 1 |IdIspitanika, 
		  method ="ML",
              data=podaci,
		  control=ctrl,
              na.action=na.exclude)
summary(test3)

qplot(x=factor(Period), y=ugr_koliko_ic, data=podaci, geom="boxplot", ylab="Zdravstveno ugrožavajuce ponašanje")
test4 <- lme(ugr_koliko_ic ~ 1 + Period, 
              random= ~ 1 |IdIspitanika, 
		  method ="ML",
              data=podaci,
		  control=ctrl,
              na.action=na.exclude)
summary(test4)

qplot(x=factor(DanUSedmici), y=Desavanja_ic, data=podaci, geom="boxplot", ylab="Povoljnost dešavanja")
test5 <- lme(Desavanja_ic ~ 1 + DanUSedmici, 
              random= ~ 1 |IdIspitanika, 
		  method ="ML",
              data=podaci,
		  control=ctrl,
              na.action=na.exclude)
summary(test5)

qplot(x=factor(Period), y=Desavanja_ic, data=podaci, geom="boxplot", ylab="Povoljnost dešavanja")
test6 <- lme(Desavanja_ic ~ 1 + Period, 
              random= ~ 1 |IdIspitanika, 
		  method ="ML",
              data=podaci,
		  control=ctrl,
              na.action=na.exclude)
summary(test6)

qplot(x=factor(DanUSedmici), y=Umor_ic, data=podaci, geom="boxplot", ylab="Stepen umora")
test7 <- lme(Umor_ic ~ 1 + DanUSedmici, 
              random= ~ 1 |IdIspitanika, 
		  method ="ML",
              data=podaci,
		  control=ctrl,
              na.action=na.exclude)
summary(test7)
qplot(x=factor(Period), y=Umor_ic, data=podaci, geom="boxplot", ylab="Stepen umora")
test8 <- lme(Umor_ic ~ 1 + Period, 
              random= ~ 1 |IdIspitanika, 
		  method ="ML",
              data=podaci,
		  control=ctrl,
              na.action=na.exclude)
summary(test8)

# uobicajeno zdravstveno ponašanje

# ishrana

total_agg <- aggregate(podaci, by=list(podaci$IdIspitanika), mean, na.rm=TRUE)

# ISHRANA

# visina težina
describe(total_agg$VT_1)
describeBy(total_agg$VT_1, total_agg$PolNR)
wilcox.test(total_agg$VT_1~total_agg$PolNR)

describe(total_agg$VT_2)
describeBy(total_agg$VT_2, total_agg$PolNR)
wilcox.test(total_agg$VT_2~total_agg$PolNR)


# percepcija i pokusaji
tab.0 <- table(total_agg$VT_3nr)
tab.0 <- prop.table(tab.0)
tab.0 <- melt(tab.0)
tab.0$value <- tab.0$value*100
tab.0$value <- round(tab.0$value, digits=1)
tab.0$Var1 <- factor(tab.0$Var1, levels = c(1,2,3,4,5), labels = c("Veoma neuhranjen/a", "Blago neuhranjen/a", "Približno idealne težine", "Blago gojazan/a", "Veoma gojazan/a"))

ggplot(data=tab.0, aes(x=Var1, y=value)) +
  geom_bar(stat="identity", position="dodge") +
  geom_text(aes(label=value), position = position_dodge(0.9), hjust=-0.2, size=4.8)+
  theme(legend.position="none",  axis.title.x=element_blank(), axis.ticks.y=element_blank(), axis.title.y=element_blank(),text=element_text(size=18), axis.ticks.x=element_blank(), axis.text.x=element_blank())+ 
  ylim(0, 100) +
  coord_flip()

tab.1 <- table(total_agg$VT_3nr, total_agg$PolNR)
tab.1 <- round((prop.table(tab.1,2)*100),1)
rownames(tab.1) <- c("Veoma neuhranjen/a", "Blago neuhranjen/a", "Približno idealne težine", "Blago gojazan/a", "Veoma gojazan/a")
colnames (tab.1)<- c("Muškarci", "Žene")
tab.1 <- melt(tab.1)

prop.test(x = c(7, 17), n = c(27, 71), correct=FALSE)

ggplot(tab.1,aes(x=Var1,y=value,fill=Var2))+
  geom_bar(stat="identity",position="dodge")+ 
  geom_text(aes(label=value), position = position_dodge(0.9), hjust=-0.2, size=4.5) +
  theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(), axis.ticks.y=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(), text=element_text(size=18))  +
  scale_fill_manual(values=c("#0072B2", "#CC79A7"))+
  ylim(0, 100) +
  coord_flip()

print(mc.result$p.value)
tab.3 <- table(total_agg$VT_4nr)
tab.3 <- prop.table(tab.3)
tab.3 <- melt(tab.3)
tab.3$value <- tab.3$value*100
tab.3$value <- round(tab.3$value, digits=1)
tab.3$Var1 <- factor(tab.3$Var1, levels = c(1,2,3,4), labels = c("Ne pokušavam da uradim ništa", "Da izgubim na težini", "Da dobijem na težini", "Da sacuvam trenutnu težinu"))

ggplot(data=tab.3, aes(x=Var1, y=value)) +
  geom_bar(stat="identity", position="dodge") +
  geom_text(aes(label=value), position = position_dodge(0.9), hjust=-0.2, size=4.8)+
  theme(legend.position="none",  axis.title.x=element_blank(), axis.ticks.y=element_blank(), axis.title.y=element_blank(),text=element_text(size=18), axis.ticks.x=element_blank(), axis.text.x=element_blank())+ 
  ylim(0, 100) +
  coord_flip()

tab.4 <- table(total_agg$VT_4nr, total_agg$PolNR)
tab.4 <- round((prop.table(tab.4,2)*100),1)
rownames(tab.4) <- c("Ne pokušavam da uradim ništa", "Da izgubim na težini", "Da dobijem na težini", "Da sacuvam trenutnu težinu")
colnames (tab.4)<- c("Muškarci", "Žene")
tab.4 <- melt(tab.4)

prop.test(x = c(7, 30), n = c(27, 71), correct=TRUE)
prop.test(x = c(8, 5), n = c(27, 71), correct=TRUE)

ggplot(tab.4,aes(x=Var1,y=value,fill=Var2))+
  geom_bar(stat="identity",position="dodge")+ 
  geom_text(aes(label=value), position = position_dodge(0.9), hjust=-0.2, size=4.5) +
  theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(), axis.ticks.y=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(), text=element_text(size=18))  +
  scale_fill_manual(values=c("#0072B2", "#CC79A7"))+
  ylim(0, 100) +
  coord_flip()

tab.5 <- table(total_agg$I_1nr)
tab.5 <- prop.table(tab.5)
tab.5 <- melt(tab.5)
tab.5$value <- tab.5$value*100
tab.5$value <- round(tab.5$value, digits=1)
tab.5$Var1 <- factor(tab.5$Var1, levels = c(1,2), labels = c("Ne", "Da"))

ggplot(data=tab.5, aes(x=Var1, y=value)) +
  geom_bar(stat="identity", position="dodge") +
  geom_text(aes(label=value), position = position_dodge(0.9), hjust=-0.2, size=4.8)+
  theme(legend.position="none",  axis.title.x=element_blank(), axis.ticks.y=element_blank(), axis.title.y=element_blank(),text=element_text(size=18), axis.ticks.x=element_blank(), axis.text.x=element_blank())+ 
  ylim(0, 100) +
  coord_flip()

tab.6 <- table(total_agg$I_1nr, total_agg$PolNR)
tab.6 <- round((prop.table(tab.6,2)*100),1)
rownames(tab.6) <- c("Ne", "Da")
colnames (tab.6)<- c("Muškarci", "Žene")
tab.6 <- melt(tab.6)

prop.test(x = c(24, 42), n = c(27, 71), correct=TRUE)

ggplot(tab.6,aes(x=Var1,y=value,fill=Var2))+
  geom_bar(stat="identity",position="dodge")+ 
  geom_text(aes(label=value), position = position_dodge(0.9), hjust=-0.2, size=4.5) +
  theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(), axis.ticks.y=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(), text=element_text(size=18))  +
  scale_fill_manual(values=c("#0072B2", "#CC79A7"))+
  ylim(0, 100) +
  coord_flip()

tab.7 <- table(total_agg$I_2nr)
tab.7 <- prop.table(tab.7)
tab.7 <- melt(tab.7)
tab.7$value <- tab.7$value*100
tab.7$value <- round(tab.7$value, digits=1)
tab.7$Var1 <- factor(tab.7$Var1, levels = c(1,2), labels = c("Ne", "Da"))

ggplot(data=tab.7, aes(x=Var1, y=value)) +
  geom_bar(stat="identity", position="dodge") +
  geom_text(aes(label=value), position = position_dodge(0.9), hjust=-0.2, size=4.8)+
  theme(legend.position="none",  axis.title.x=element_blank(), axis.ticks.y=element_blank(), axis.title.y=element_blank(),text=element_text(size=18), axis.ticks.x=element_blank(), axis.text.x=element_blank())+ 
  ylim(0, 100) +
  coord_flip()

tab.8 <- table(total_agg$I_2nr, total_agg$PolNR)
tab.8 <- round((prop.table(tab.8,2)*100),1)
rownames(tab.8) <- c("Ne", "Da")
colnames (tab.8)<- c("Muškarci", "Žene")
tab.8 <- melt(tab.8)

prop.test(x = c(7, 5), n = c(27, 71), correct=TRUE)

ggplot(tab.8,aes(x=Var1,y=value,fill=Var2))+
  geom_bar(stat="identity",position="dodge")+ 
  geom_text(aes(label=value), position = position_dodge(0.9), hjust=-0.2, size=4.5) +
  theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(), axis.ticks.y=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(), text=element_text(size=18))  +
  scale_fill_manual(values=c("#0072B2", "#CC79A7"))+
  ylim(0, 100) +
  coord_flip()


# voce
tab.9 <- table(total_agg$I_3nr)
tab.9 <- prop.table(tab.9)
tab.9 <- melt(tab.9)
tab.9$value <- tab.9$value*100
tab.9$value <- round(tab.9$value, digits=1)
tab.9$Var1 <- factor(tab.9$Var1, levels = c(1,2,3,4,5), labels = c("Jednom i više puta dnevno", "4 do 6 puta nedeljno", "1 do 3 puta nedeljno", "Manje od jednom nedeljno", "Nikada"))

ggplot(data=tab.9, aes(x=Var1, y=value)) +
  geom_bar(stat="identity", position="dodge") +
  geom_text(aes(label=value), position = position_dodge(0.9), hjust=-0.2, size=4.8)+
  theme(legend.position="none",  axis.title.x=element_blank(), axis.ticks.y=element_blank(), axis.title.y=element_blank(),text=element_text(size=18), axis.ticks.x=element_blank(), axis.text.x=element_blank())+ 
  ylim(0, 100) +
  coord_flip()

tab.10 <- table(total_agg$I_3nr, total_agg$PolNR)
tab.10 <- round((prop.table(tab.10,2)*100),1)
rownames(tab.10) <- c("Jednom i više puta dnevno", "4 do 6 puta nedeljno", "1 do 3 puta nedeljno", "Manje od jednom nedeljno")
colnames (tab.10)<- c("Muškarci", "Žene")
tab.10 <- melt(tab.10)

prop.test(x = c(3, 3), n = c(27, 71), correct=TRUE)


ggplot(tab.10,aes(x=Var1,y=value,fill=Var2))+
  geom_bar(stat="identity",position="dodge")+ 
  geom_text(aes(label=value), position = position_dodge(0.9), hjust=-0.2, size=4.5) +
  theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(), axis.ticks.y=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(), text=element_text(size=18))  +
  scale_fill_manual(values=c("#0072B2", "#CC79A7"))+
  ylim(0, 100) +
  coord_flip()

describe(total_agg$I_4)
describeBy(total_agg$I_4, total_agg$PolNR)
wilcox.test(total_agg$I_4~total_agg$PolNR)

describe(total_agg$I_3nr_int)
describeBy(total_agg$I_3nr_int, total_agg$PolNR)
wilcox.test(total_agg$I_3nr_int~total_agg$PolNR)

# povrce
tab.11 <- table(total_agg$I_5nr)
tab.11 <- prop.table(tab.11)
tab.11 <- melt(tab.11)
tab.11$value <- tab.11$value*100
tab.11$value <- round(tab.11$value, digits=1)
tab.11$Var1 <- factor(tab.11$Var1, levels = c(1,2,3,4,5), labels = c("Jednom i više puta dnevno", "4 do 6 puta nedeljno", "1 do 3 puta nedeljno", "Manje od jednom nedeljno", "Nikada"))

ggplot(data=tab.11, aes(x=Var1, y=value)) +
  geom_bar(stat="identity", position="dodge") +
  geom_text(aes(label=value), position = position_dodge(0.9), hjust=-0.2, size=4.8)+
  theme(legend.position="none",  axis.title.x=element_blank(), axis.ticks.y=element_blank(), axis.title.y=element_blank(),text=element_text(size=18), axis.ticks.x=element_blank(), axis.text.x=element_blank())+ 
  ylim(0, 100) +
  coord_flip()

tab.12 <- table(total_agg$I_5nr, total_agg$PolNR)
tab.12 <- round((prop.table(tab.12,2)*100),1)
rownames(tab.12) <- c("Jednom i više puta dnevno", "4 do 6 puta nedeljno", "1 do 3 puta nedeljno", "Manje od jednom nedeljno")
colnames (tab.12)<- c("Muškarci", "Žene")
tab.12 <- melt(tab.12)

prop.test(x = c(7, 26), n = c(27, 71), correct=TRUE)

ggplot(tab.12,aes(x=Var1,y=value,fill=Var2))+
  geom_bar(stat="identity",position="dodge")+ 
  geom_text(aes(label=value), position = position_dodge(0.9), hjust=-0.2, size=4.5) +
  theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(), axis.ticks.y=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(), text=element_text(size=18))  +
  scale_fill_manual(values=c("#0072B2", "#CC79A7"))+
  ylim(0, 100) +
  coord_flip()

describe(total_agg$I_6)
describeBy(total_agg$I_6, total_agg$PolNR)
wilcox.test(total_agg$I_6~total_agg$PolNR)

describe(total_agg$I_5nr_int)
describeBy(total_agg$I_5nr_int, total_agg$PolNR)
wilcox.test(total_agg$I_5nr_int~total_agg$PolNR)

# razmisljanje o ishrani
tab.13 <- table(total_agg$I_7nr)
tab.13 <- prop.table(tab.13)
tab.13 <- melt(tab.13)
tab.13$value <- tab.13$value*100
tab.13$value <- round(tab.13$value, digits=1)
tab.13$Var1 <- factor(tab.13$Var1, levels = c(1,2,3,4), labels = c("Uvek", "Cesto", "Ponekad", "Nikad"))

ggplot(data=tab.13, aes(x=Var1, y=value)) +
  geom_bar(stat="identity", position="dodge") +
  geom_text(aes(label=value), position = position_dodge(0.9), hjust=-0.2, size=4.8)+
  theme(legend.position="none",  axis.title.x=element_blank(), axis.ticks.y=element_blank(), axis.title.y=element_blank(),text=element_text(size=18), axis.ticks.x=element_blank(), axis.text.x=element_blank())+ 
  ylim(0, 100) +
  coord_flip()

tab.14 <- table(total_agg$I_7nr, total_agg$PolNR)
tab.14 <- round((prop.table(tab.14,2)*100),1)
rownames(tab.14) <- c("Uvek", "Cesto", "Ponekad", "Nikad")
colnames (tab.14)<- c("Muškarci", "Žene")
tab.14 <- melt(tab.14)

prop.test(x = c(9, 40), n = c(27, 71), correct=TRUE)
prop.test(x = c(5, 6), n = c(27, 71), correct=TRUE)
	
ggplot(tab.14,aes(x=Var1,y=value,fill=Var2))+
  geom_bar(stat="identity",position="dodge")+ 
  geom_text(aes(label=value), position = position_dodge(0.9), hjust=-0.2, size=4.5) +
  theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(), axis.ticks.y=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(), text=element_text(size=18))  +
  scale_fill_manual(values=c("#0072B2", "#CC79A7"))+
  ylim(0, 100) +
  coord_flip()

# Pusenje

tab.15 <- table(total_agg$PU_1nr)
tab.15 <- prop.table(tab.15)
tab.15 <- melt(tab.15)
tab.15$value <- tab.15$value*100
tab.15$value <- round(tab.15$value, digits=1)
tab.15$Var1 <- factor(tab.15$Var1, levels = c(1,2,3,4,5,6,7,8), 
labels = c("Nikada nisam probao cigarete",
"7 ili manje godina",
"8 ili 9 godina",
"10 ili 11 godina",
"12 ili 13 godina",
"14 ili 15 godina",
"16 ili 17 godina",
"18 ili više godina"))

ggplot(data=tab.15, aes(x=Var1, y=value)) +
  geom_bar(stat="identity", position="dodge") +
  geom_text(aes(label=value), position = position_dodge(0.9), hjust=-0.2, size=4.8)+
  theme(legend.position="none",  axis.title.x=element_blank(), axis.ticks.y=element_blank(), axis.title.y=element_blank(),text=element_text(size=18), axis.ticks.x=element_blank(), axis.text.x=element_blank())+ 
  ylim(0, 100) +
  coord_flip()

tab.16 <- table(total_agg$PU_1nr, total_agg$PolNR)
tab.16 <- round((prop.table(tab.16,2)*100),1)
rownames(tab.16) <- c("Nikada nisam probao cigarete",
"7 ili manje godina",
"8 ili 9 godina",
"12 ili 13 godina",
"14 ili 15 godina",
"16 ili 17 godina",
"18 ili više godina")
colnames (tab.16)<- c("Muškarci", "Žene")
tab.16 <- melt(tab.16)

ggplot(tab.16,aes(x=Var1,y=value,fill=Var2))+
  geom_bar(stat="identity",position="dodge")+ 
  geom_text(aes(label=value), position = position_dodge(0.9), hjust=-0.2, size=4.5) +
  theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(), axis.ticks.y=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(), text=element_text(size=18))  +
  scale_fill_manual(values=c("#0072B2", "#CC79A7"))+
  ylim(0, 100) +
  coord_flip()

tab.17 <- table(total_agg$PU_2nr)
tab.17 <- prop.table(tab.17)
tab.17 <- melt(tab.17)
tab.17$value <- tab.17$value*100
tab.17$value <- round(tab.17$value, digits=1)
tab.17$Var1 <- factor(tab.17$Var1, levels = c(1,2,3,4,5,6,7), 
labels = c("0 dana",
"1 ili 2 dana",
"3 do 5 dana",
"6 do 9 dana",
"10 do 19 dana",
"20 do 29 dana",
"Svih 30 dana"))

ggplot(data=tab.17, aes(x=Var1, y=value)) +
  geom_bar(stat="identity", position="dodge") +
  geom_text(aes(label=value), position = position_dodge(0.9), hjust=-0.2, size=4.8)+
  theme(legend.position="none",  axis.title.x=element_blank(), axis.ticks.y=element_blank(), axis.title.y=element_blank(),text=element_text(size=18), axis.ticks.x=element_blank(), axis.text.x=element_blank())+ 
  ylim(0, 100) +
  coord_flip()

tab.18 <- table(total_agg$PU_2nr, total_agg$PolNR)
tab.18 <- round((prop.table(tab.18,2)*100),1)
rownames(tab.18) <- c("0 dana",
"1 ili 2 dana",
"3 do 5 dana",
"6 do 9 dana",
"10 do 19 dana",
"20 do 29 dana",
"Svih 30 dana")
colnames (tab.18)<- c("Muškarci", "Žene")
tab.18 <- melt(tab.18)

ggplot(tab.18,aes(x=Var1,y=value,fill=Var2))+
  geom_bar(stat="identity",position="dodge")+ 
  geom_text(aes(label=value), position = position_dodge(0.9), hjust=-0.2, size=4.5) +
  theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(), axis.ticks.y=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(), text=element_text(size=18))  +
  scale_fill_manual(values=c("#0072B2", "#CC79A7"))+
  ylim(0, 100) +
  coord_flip()

tab.19 <- table(total_agg$PU_3nr)
tab.19 <- prop.table(tab.19)
tab.19 <- melt(tab.19)
tab.19$value <- tab.19$value*100
tab.19$value <- round(tab.19$value, digits=1)
tab.19$Var1 <- factor(tab.19$Var1, levels = c(1,2,3,4,5,6), 
labels = c("Manje od 1 cigarete dnevno",
"1 cigaretu dnevno",
"2-5 cigareta dnevno",
"6-10 cigareta dnevno",
"11-20 cigareta dnevno",
"Više od 20 cigareta dnevno"))

ggplot(data=tab.19, aes(x=Var1, y=value)) +
  geom_bar(stat="identity", position="dodge") +
  geom_text(aes(label=value), position = position_dodge(0.9), hjust=-0.2, size=4.8)+
  theme(legend.position="none",  axis.title.x=element_blank(), axis.ticks.y=element_blank(), axis.title.y=element_blank(),text=element_text(size=18), axis.ticks.x=element_blank(), axis.text.x=element_blank())+ 
  ylim(0, 100) +
  coord_flip()

tab.20 <- table(total_agg$PU_3nr, total_agg$PolNR)
tab.20 <- round((prop.table(tab.20,2)*100),1)
rownames(tab.20) <- c("Manje od 1 cigarete dnevno",
"1 cigaretu dnevno",
"2-5 cigareta dnevno",
"6-10 cigareta dnevno",
"11-20 cigareta dnevno",
"Više od 20 cigareta dnevno")
colnames (tab.20)<- c("Muškarci", "Žene")
tab.20 <- melt(tab.20)

ggplot(tab.20,aes(x=Var1,y=value,fill=Var2))+
  geom_bar(stat="identity",position="dodge")+ 
  geom_text(aes(label=value), position = position_dodge(0.9), hjust=-0.2, size=4.5) +
  theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(), axis.ticks.y=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(), text=element_text(size=18))  +
  scale_fill_manual(values=c("#0072B2", "#CC79A7"))+
  ylim(0, 100) +
  coord_flip()

tab.21 <- table(total_agg$PU_4nr)
tab.21 <- prop.table(tab.21)
tab.21 <- melt(tab.21)
tab.21$value <- tab.21$value*100
tab.21$value <- round(tab.21$value, digits=1)
tab.21$Var1 <- factor(tab.21$Var1, levels = c(2,3), 
labels = c("Da", "Ne"))

ggplot(data=tab.21, aes(x=Var1, y=value)) +
  geom_bar(stat="identity", position="dodge") +
  geom_text(aes(label=value), position = position_dodge(0.9), hjust=-0.2, size=4.8)+
  theme(legend.position="none",  axis.title.x=element_blank(), axis.ticks.y=element_blank(), axis.title.y=element_blank(),text=element_text(size=18), axis.ticks.x=element_blank(), axis.text.x=element_blank())+ 
  ylim(0, 100) +
  coord_flip()

tab.22 <- table(total_agg$PU_4nr, total_agg$PolNR)
tab.22 <- round((prop.table(tab.22,2)*100),1)
rownames(tab.22) <- c("Da",
"Ne")
colnames (tab.22)<- c("Muškarci", "Žene")
tab.22 <- melt(tab.22)

ggplot(tab.22,aes(x=Var1,y=value,fill=Var2))+
  geom_bar(stat="identity",position="dodge")+ 
  geom_text(aes(label=value), position = position_dodge(0.9), hjust=-0.2, size=4.5) +
  theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(), axis.ticks.y=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(), text=element_text(size=18))  +
  scale_fill_manual(values=c("#0072B2", "#CC79A7"))+
  ylim(0, 100) +
  coord_flip()

# alkohol

tab.23 <- table(total_agg$KA_1nr)
tab.23 <- prop.table(tab.23)
tab.23 <- melt(tab.23)
tab.23$value <- tab.23$value*100
tab.23$value <- round(tab.23$value, digits=1)
tab.23$Var1 <- factor(tab.23$Var1, levels = c(0,1), 
labels = c("Ne", "Da"))

ggplot(data=tab.23, aes(x=Var1, y=value)) +
  geom_bar(stat="identity", position="dodge") +
  geom_text(aes(label=value), position = position_dodge(0.9), hjust=-0.2, size=4.8)+
  theme(legend.position="none",  axis.title.x=element_blank(), axis.ticks.y=element_blank(), axis.title.y=element_blank(),text=element_text(size=18), axis.ticks.x=element_blank(), axis.text.x=element_blank())+ 
  ylim(0, 100) +
  coord_flip()

tab.24 <- table(total_agg$KA_1nr, total_agg$PolNR)
tab.24 <- round((prop.table(tab.24,2)*100),1)
rownames(tab.24) <- c("Ne", "Da")
colnames (tab.24)<- c("Muškarci", "Žene")
tab.24 <- melt(tab.24)

prop.test(x = c(8, 13), n = c(27, 71), correct=TRUE)
prop.test(x = c(19, 58), n = c(27, 71), correct=TRUE)

ggplot(tab.24,aes(x=Var1,y=value,fill=Var2))+
  geom_bar(stat="identity",position="dodge")+ 
  geom_text(aes(label=value), position = position_dodge(0.9), hjust=-0.2, size=4.5) +
  theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(), axis.ticks.y=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(), text=element_text(size=18))  +
  scale_fill_manual(values=c("#0072B2", "#CC79A7"))+
  ylim(0, 100) +
  coord_flip()

describe(total_agg$KA_2)
describeBy(total_agg$KA_2, total_agg$PolNR)
wilcox.test(total_agg$KA_2~total_agg$PolNR)

describe(total_agg$KA_3)
describeBy(total_agg$KA_3, total_agg$PolNR)
wilcox.test(total_agg$KA_3~total_agg$PolNR)

describe(total_agg$KA_4)
describeBy(total_agg$KA_4, total_agg$PolNR)
wilcox.test(total_agg$KA_4~total_agg$PolNR)

describe(total_agg$KA_5)
describeBy(total_agg$KA_5, total_agg$PolNR)
wilcox.test(total_agg$KA_5~total_agg$PolNR)
# Vezbanje

tab.25 <- table(total_agg$TV_1nr)
tab.25 <- prop.table(tab.25)
tab.25 <- melt(tab.25)
tab.25$value <- tab.25$value*100
tab.25$value <- round(tab.25$value, digits=1)
tab.25$Var1 <- factor(tab.25$Var1, levels = c(0,1), 
labels = c("Ne","Da"))

ggplot(data=tab.25, aes(x=Var1, y=value)) +
  geom_bar(stat="identity", position="dodge") +
  geom_text(aes(label=value), position = position_dodge(0.9), hjust=-0.2, size=4.8)+
  theme(legend.position="none",  axis.title.x=element_blank(), axis.ticks.y=element_blank(), axis.title.y=element_blank(),text=element_text(size=18), axis.ticks.x=element_blank(), axis.text.x=element_blank())+ 
  ylim(0, 100) +
  coord_flip()

tab.26 <- table(total_agg$TV_1nr, total_agg$PolNR)
tab.26 <- round((prop.table(tab.26,2)*100),1)
rownames(tab.26) <- c("Ne","Da")
colnames (tab.26)<- c("Muškarci", "Žene")
tab.26 <- melt(tab.26)

prop.test(x = c(10, 39), n = c(27, 71), correct=TRUE)

ggplot(tab.26,aes(x=Var1,y=value,fill=Var2))+
  geom_bar(stat="identity",position="dodge")+ 
  geom_text(aes(label=value), position = position_dodge(0.9), hjust=-0.2, size=4.5) +
  theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(), axis.ticks.y=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(), text=element_text(size=18))  +
  scale_fill_manual(values=c("#0072B2", "#CC79A7"))+
  ylim(0, 100) +
  coord_flip()

tab.27 <- table(total_agg$TV_2nr)
tab.27 <- prop.table(tab.27)
tab.27 <- melt(tab.27)
tab.27$value <- tab.27$value*100
tab.27$value <- round(tab.27$value, digits=1)
tab.27$Var1 <- factor(tab.27$Var1, levels = c(0,1), 
labels = c("Ne","Da"))

ggplot(data=tab.27, aes(x=Var1, y=value)) +
  geom_bar(stat="identity", position="dodge") +
  geom_text(aes(label=value), position = position_dodge(0.9), hjust=-0.2, size=4.8)+
  theme(legend.position="none",  axis.title.x=element_blank(), axis.ticks.y=element_blank(), axis.title.y=element_blank(),text=element_text(size=18), axis.ticks.x=element_blank(), axis.text.x=element_blank())+ 
  ylim(0, 100) +
  coord_flip()

tab.28 <- table(total_agg$TV_2nr, total_agg$PolNR)
tab.28 <- round((prop.table(tab.28,2)*100),1)
rownames(tab.28) <- c("Ne","Da")
colnames (tab.28)<- c("Muškarci", "Žene")
tab.28 <- melt(tab.28)

prop.test(x = c(3, 2), n = c(27, 71), correct=TRUE)

ggplot(tab.28,aes(x=Var1,y=value,fill=Var2))+
  geom_bar(stat="identity",position="dodge")+ 
  geom_text(aes(label=value), position = position_dodge(0.9), hjust=-0.2, size=4.5) +
  theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(), axis.ticks.y=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(), text=element_text(size=18))  +
  scale_fill_manual(values=c("#0072B2", "#CC79A7"))+
  ylim(0, 100) +
  coord_flip()


describe(total_agg$TV_3)
describeBy(total_agg$TV_3, total_agg$PolNR)
wilcox.test(total_agg$TV_3~total_agg$PolNR)

describe(total_agg$TV_4)
describeBy(total_agg$TV_4, total_agg$PolNR)
wilcox.test(total_agg$TV_4~total_agg$PolNR)

describe(total_agg$TV_5)
describeBy(total_agg$TV_5, total_agg$PolNR)
wilcox.test(total_agg$TV_5~total_agg$PolNR)


###############################################################################################################################

describe(total_agg$VO_1)
describeBy(total_agg$VO_1, total_agg$PolNR)
wilcox.test(total_agg$VO_1~total_agg$PolNR)

describe(total_agg$DI_1)
describeBy(total_agg$DI_1, total_agg$PolNR)
wilcox.test(total_agg$DI_1~total_agg$PolNR)

describe(total_agg$KF_1)
describeBy(total_agg$KF_1, total_agg$PolNR)
wilcox.test(total_agg$KF_1~total_agg$PolNR)

describe(total_agg$KF_2)
describeBy(total_agg$KF_2, total_agg$PolNR)
wilcox.test(total_agg$KF_2~total_agg$PolNR)

describe(total_agg$SL_1)
describeBy(total_agg$SL_1, total_agg$PolNR)
wilcox.test(total_agg$SL_1~total_agg$PolNR)

describe(total_agg$SL_2)
describeBy(total_agg$SL_2, total_agg$PolNR)
wilcox.test(total_agg$SL_2~total_agg$PolNR)

describe(total_agg$BH_1)
describeBy(total_agg$BH_1, total_agg$PolNR)
wilcox.test(total_agg$BH_1~total_agg$PolNR)

###############################################################################################################################