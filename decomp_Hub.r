#############################################################################################################################################
############# DECOMPOSITION TIME SERIES
#############################################################################################################################################


library(tidyr)
library(dplyr)
library(ggplot2)
#library(Hmisc)
library(scales)
library(ggTimeSeries)
library("xlsx")
library(shiny)
library("gridExtra")
library(grid)
library(forecast)
library(dlm)
library(Metrics)
library(zoo)
library(stringr)
library(purrr)
library(ggpubr)
library(data.table)


#########################################################
### DATA
#########################################################


#Ridership data from 2011-01-01 to 2020-07-31 
dt_Hub = data.frame(...) #Three columns : 'date', 'station', 'ridership'


#Exogenous data
data_calendar = function(date_lim){
  j_feries = c(...) #vector of bank holidays dates
  j_pont = c(...) #vector of extra days off dates
  j_travaux_rer = c(...) #vector of maintenance days dates on the rer line
  j_travaux_m1 = c(...) #vector of maintenance days dates on the metro line
  j_greves = c(...) #vector of strike days dates
  j_gratuite = c(...) # vector of free travels days dates
  j_confinement = c(...) #vector of covid19 lockdown days
  j_deconfinement = c(...) #vector of covid19 unlockdown days dates
  #....
  return(
    data.frame('date' = seq(as.Date("2011-01-01"), as.Date("2020-07-31"), by="days")) %>% 
      mutate(wday = wday(date)) %>% 
      mutate(wday = recode(wday, '1' = "Sunday",'2'="Monday",'3'="Tuesday",'4'="Wednesday",'5'="Thursday",'6'="Friday",'7'="Saturday")) %>%
      mutate(bank = ifelse(date %in% j_feries, 1, 0)) %>%
      mutate(bank_workday = ifelse(date %in% j_feries & !wday %in% c('Saturday', 'Sunday'), 1, 0)) %>%
      mutate(bank_weekend = ifelse(date %in% j_feries & wday %in% c('Saturday', 'Sunday'), 1, 0)) %>%
      mutate(extra = ifelse(date %in% j_pont, 1, 0)) %>%
      mutate(work_rer_workday = ifelse(date %in% j_travaux_rer & (! jour %in% c('Saturday', 'Sunday') & ferie == 0), 1, 0)) %>%
      mutate(work_rer_weekend = ifelse(date %in% j_travaux_rer & (jour %in% c('Saturday', 'Sunday') | ferie == 1), 1, 0)) %>%
      mutate(work_m1_workday = ifelse(date %in% j_travaux_m1 & (! jour %in% c('Saturday', 'Sunday') & ferie == 0), 1, 0)) %>%
      mutate(work_m1_weekend = ifelse(date %in% j_travaux_m1 & (jour %in% c('Saturday', 'Sunday') | ferie == 1), 1, 0)) %>%
      mutate(free = ifelse(date %in% j_gratuite, 1, 0)) %>%
      mutate(strike = ifelse(date %in% j_greves, 1, 0)) %>%
      mutate(lockdown = ifelse(date %in% j_confinement, 1, 0)) %>%
      mutate(unlockdown = ifelse(date %in% j_deconfinement, 1, 0)) %>%
      filter(date <= date_lim)
    )       
}



#############################################################################
## PERFORMANCES
#############################################################################

#Calcul RMSE entre 2 séries
get_RMSE = function(original, predit){
  res = predit - original
  RSS = sum(res^2,na.rm=T)
  MSE = RSS/length(original)
  RMSE = sqrt(MSE)
  return(RMSE)
}







###############################################################################################################################
## VISUALISATION DES DONNEES BRUTES
###############################################################################################################################


#Serie totale
ggplot(dt_Hub, aes(x=date,y=ridership)) + geom_line(size=.9) + 
  theme_bw()  +
  theme( legend.position = "none",legend.direction = "vertical", strip.text.y = element_text(size=30,face='bold'),
         legend.box = "vertical", panel.spacing = unit(2, "lines"),
         legend.justification = c(0, 1),
         legend.title=element_text(size=30, face='bold'),
         axis.title=element_text(size=25,face="bold"), axis.text = element_text(size=20),axis.text.x = element_text( vjust = 0.5),
         legend.text = element_text(size = 20,face='bold'),legend.key.width=unit(4, "cm"),legend.key.height=unit(1, "cm"))+
  ylab('Daily inflow') +
  scale_y_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE)) + facet_grid(factor(Reseau,levels = c('RER A','Metro 1') )~.)



#Une année
ggplot(dt_Hub %>% filter(year(date) == 2011),aes(x=date,y=ridership)) + geom_line(size=.9) + 
  theme_bw()  +
  theme( legend.position = "none",legend.direction = "vertical", strip.text.y = element_text(size=30,face='bold'),
         legend.box = "vertical", panel.spacing = unit(2, "lines"),
         legend.justification = c(0, 1),
         legend.title=element_text(size=30, face='bold'),
         axis.title=element_text(size=25,face="bold"), axis.text = element_text(size=20),axis.text.x = element_text( vjust = 0.5),
         legend.text = element_text(size = 20,face='bold'),legend.key.width=unit(4, "cm"),legend.key.height=unit(1, "cm"))+
  ylab('Daily inflow') +
  scale_y_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE)) + facet_grid(factor(Reseau,levels = c('RER A','Metro 1') )~.)



#Semaine
data_visu_s = dt_Hub %>% mutate(wday = wday(Date)) %>% mutate(wday = recode(wday, '1' = "Sunday",'2'="Monday",'3'="Tuesday",'4'="Wednesday",'5'="Thursday",'6'="Friday",'7'="Saturday"))


ggplot(data_visu_s,aes(x=factor(wday,levels=c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday')),y=ridership,fill=wday)) +geom_boxplot(alpha=0.6) +
  facet_grid(factor(Reseau,levels = c('RER A','Metro 1') )~.)+
  theme_bw()  +
  theme( legend.position = "none",legend.direction = "vertical", strip.text.y = element_text(size=30,face='bold'),
         legend.box = "vertical", panel.spacing = unit(2, "lines"),
         legend.justification = c(0, 1),
         legend.title=element_text(size=30, face='bold'),
         axis.title=element_text(size=25,face="bold"), axis.text = element_text(size=20),axis.text.x = element_text( vjust = 0.5),
         legend.text = element_text(size = 20,face='bold'),legend.key.width=unit(4, "cm"),legend.key.height=unit(1, "cm"))+
  ylab('Daily inflow') +
  scale_y_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE)) + xlab("") +
  scale_fill_brewer(palette="Dark2")






######################################################################################################################
## TEST NUMBER OF HARMONICS (WITH AND WITHOUT EXOGENOUS DATA ON 2,4,6,8,10 AND 12 HARMONICS)
######################################################################################################################

##PREPARE DATA (before covid19 lockdown on 2020-03-15)

#Exogenous data
data_calendar = data_calendar('2020-03-15')
free = matrix(as.numeric(data_calendar$free))
strike =  matrix(as.numeric(data_calendar$strike))
lockdown = matrix(as.numeric(data_calendar$lockdown))
unlockdown = matrix(as.numeric(data_calendar$unlockdown)) 
bank_workday =  matrix(as.numeric(data_calendar$bank_workday))
bank_weekend =  matrix(as.numeric(data_calendar$bank_weekend))
work_rer_worday =  matrix(as.numeric(data_calendar$work_rer_worday))
work_rer_weekend =  matrix(as.numeric(data_calendar$work_rer_weekend))
extra =  matrix(as.numeric(data_calendar$extra))
work_m1_worday =  matrix(as.numeric(data_calendar$work_m1_worday))
work_m1_weekend =  matrix(as.numeric(data_calendar$work_m1_weekend))  


#Ridership data

#RERA
serie_train_rera = dt_Hub %>% filter(station = 'RERA', date < '2016-01-01') %>% pull(ridership)
serie_test_rera = dt_Hub %>% filter(station = 'RERA' & (date >= '2016-01-01' & date <= '2020-03-15')) %>% pull(ridership)
serie_tot_rera = c(serie_train_rera,serie_test_rera)

temps_train =  dt_Hub %>% filter(date <= '2015-12-31') %>% pull(date)
temps_test =  dt_Hub %>% filter(date > '2015-12-31' & date <= '2020-03-15') %>% pull(date)
temps_tot = c(temps_train,temps_test)

serie_gen2 = serie_tot_rera
serie_gen2[serie_gen2 == 0] = 1

serie_train_rera_log = log(serie_gen2[which(temps_tot < '2016-01-01' )])
serie_test_rera_log = log(serie_gen2[which(temps_tot <= '2020-03-15' & temps_tot >= '2016-01-01')])
serie_tot_rera_log = c(serie_train_rera_log,serie_test_rera_log)
rm(serie_gen2)


#M1
serie_train_m1 = dt_Hub %>% filter(station = 'M1', date < '2016-01-01') %>% pull(ridership)
serie_test_m1 = dt_Hub %>% filter(station = 'M1' & (date >= '2016-01-01' & date <= '2020-03-15')) %>% pull(ridership)
serie_tot_m1 = c(serie_train_m1,serie_test_m1)

serie_gen2 = serie_tot_m1
serie_gen2[serie_gen2 == 0] = 1

serie_train_m1_log = log(serie_gen2[which(temps_tot < '2016-01-01')])
serie_test_m1_log = log(serie_gen2[which(temps_tot <= '2020-03-15' & temps_tot >= '2016-01-01')])
serie_tot_m1_log = c(serie_train_m1_log,serie_test_m1_log)
rm(serie_gen2)







#LOOP TO CALCULATE MODELS FOR K BETWEEN 2 TO 12, RESULTS ARE SAVED IN TEXT FILES

for(k in c(0,2,4,6,8,10)){
  
  i = -10 #INITIAL VALUES FOR BFGS
  
  
  #RERA ##########################################################
  
  #MODEL WITH EXOGENOUS VARIABLES
  Mod_mult_rera_exo = function(p){
    return(dlmModPoly(2,dV=exp(p[1]), m0=c(mean(serie_tot_rera_log[1:10]),0), dW=c(exp(p[2]),0), C0 = diag(c(exp(p[3]), exp(p[4])))) + dlmModSeas(7,dW=c(rep(0,6)), m0 = rep(exp(p[5]),6),  C0 = exp(p[6]) * diag(nrow = 6), dV=0) +
             dlmModTrig(s=365, q = k, C0 = (exp(p[7]))*diag(2*k), m0 = rep(c(exp(p[8]), exp(p[9])), each=k), dV=0) +
             dlmModReg(bank_workday, addInt = FALSE, dW = exp(p[10]),dV=0, m0=as.numeric(lm(serie_tot_rera_log[which(wday(temps_tot) %in% c(2,3,4,5,6))] ~ as.factor(bank_workday[which(wday(temps_tot) %in% c(2,3,4,5,6))]))$coefficients[2]), C0 = exp(p[19])* diag(nrow = 1)) +
             dlmModReg(bank_weekend, addInt = FALSE, dW = exp(p[11]),dV=0, m0=as.numeric(lm(serie_tot_rera_log[which(wday(temps_tot) %in% c(1,7))] ~ as.factor(bank_weekend[which(wday(temps_tot) %in% c(1,7))]))$coefficients[2]), C0 = exp(p[20])* diag(nrow = 1)) +
             dlmModReg(extra, addInt = FALSE, dW = exp(p[12]),dV=0, m0=as.numeric(lm(serie_tot_rera_log[which(wday(temps_tot) %in% c(2,3,4,5,6))] ~ as.factor(extra[which(wday(temps_tot) %in% c(2,3,4,5,6))]))$coefficients[2]), C0 = exp(p[21])* diag(nrow = 1)) +
             dlmModReg(work_rer_worday, addInt = FALSE, dW = exp(p[13]),dV=0, m0 = as.numeric(lm(serie_tot_rera_log[which(wday(temps_tot) %in% c(2,3,4,5,6))] ~ as.factor(work_rer_worday[which(wday(temps_tot) %in% c(2,3,4,5,6))]))$coefficients[2]), C0 = exp(p[22])* diag(nrow = 1)) +
             dlmModReg(work_rer_weekend, addInt = FALSE, dW = exp(p[14]),dV=0, m0=as.numeric(lm(serie_tot_rera_log[which(wday(temps_tot) %in% c(1,7))] ~ as.factor(work_rer_weekend[which(wday(temps_tot) %in% c(1,7))]))$coefficients[2]), C0 = exp(p[23])* diag(nrow = 1)) +
             dlmModReg(strike, addInt = FALSE, dW = exp(p[15]),dV=0, m0 = as.numeric(lm(serie_tot_rera_log[which(wday(temps_tot) %in% c(2,3,4,5,6))] ~ as.factor(strike[which(wday(temps_tot) %in% c(2,3,4,5,6))]))$coefficients[2]), C0 = exp(p[24])* diag(nrow = 1)) +
             dlmModReg(free, addInt = FALSE, dW = exp(p[16]),dV=0, m0 = as.numeric(lm(serie_tot_rera_log ~ as.factor(free))$coefficients[2]), C0 = exp(p[25])* diag(nrow = 1)) +
             dlmModReg(work_m1_worday, addInt = FALSE, dW = exp(p[17]),dV=0, m0 = as.numeric(lm(serie_tot_rera_log[which(wday(temps_tot) %in% c(2,3,4,5,6))] ~ as.factor(work_m1_worday[which(wday(temps_tot) %in% c(2,3,4,5,6))]))$coefficients[2]), C0 = exp(p[26])* diag(nrow = 1)) +
             dlmModReg(work_m1_weekend, addInt = FALSE, dW = exp(p[18]),dV=0, m0=as.numeric(lm(serie_tot_rera_log[which(wday(temps_tot) %in% c(1,7))] ~ as.factor(work_m1_weekend[which(wday(temps_tot) %in% c(1,7))]))$coefficients[2]), C0 = exp(p[27])* diag(nrow = 1))
           
    )
    
  }
  
  
  
  
  init = rep(i, 27)
  
  
  
  init[5] = -10
  init[8] = -10
  init[9] = -10
  
  upper_bound = rep(Inf,27)
  upper_bound[2] = -15
  
  
  tryCatch({
    mleMod_mult_rera_exo = dlmMLE(serie_train_rera_log, parm = init, build = Mod_mult_rera_exo, hessian=T, upper = upper_bound, control=list(trace=1,REPORT=1,maxit=1000))
    fwrite(as.list(mleMod_mult_rera_exo$par), file = paste("rera_exo_harm",as.character(k),".txt"))
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}
  )
  
  
  
  
  
  #MODEL WITHOUT EXOGENOUS VARIABLES
  Mod_mult_rera = function(p){
    return(dlmModPoly(2,dV=exp(p[1]), m0=c(mean(serie_tot_rera_log[1:10]),0), dW=c(exp(p[2]),0), C0 = diag(c(exp(p[3]), exp(p[4])))) + dlmModSeas(7,dW=c(rep(0,6)), m0 = rep(exp(p[5]),6),  C0 = exp(p[6]) * diag(nrow = 6), dV=0) +
             dlmModTrig(s=365, q = k, C0 = (exp(p[7]))*diag(2*k), m0 = rep(c(exp(p[8]), exp(p[9])), each=k), dV=0) 
           
    )
    
  }
  
  
  
  
  init = rep(i, 9)
  
  
  
  init[5] = -10
  init[8] = -10
  init[9] = -10
  
  upper_bound = rep(Inf,9)
  upper_bound[2] = -15
  
  
  tryCatch({
    mleMod_mult_rera = dlmMLE(serie_train_rera_log, parm = init, build = Mod_mult_rera, hessian=T, upper = upper_bound, control=list(trace=1,REPORT=1,maxit=1000))
    fwrite(as.list(mleMod_mult_rera$par), file = paste("rera_harm",as.character(k),".txt"))
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}
  )
  
  
  
  
  
  
  
  
  #M1 ##########################################################
  #MODEL WITH EXOGENOUS VARIABLES
  
  Mod_mult_m1_exo = function(p){
    return(dlmModPoly(2,dV=exp(p[1]), m0=c(mean(serie_tot_m1_log[1:10]),0), dW=c(exp(p[2]),0), C0 = diag(c(exp(p[3]), exp(p[4])))) + dlmModSeas(7,dW=c(rep(0,6)), m0 = rep(exp(p[5]),6),  C0 = exp(p[6]) * diag(nrow = 6), dV=0) +
             dlmModTrig(s=365, q = k, C0 = (exp(p[7]))*diag(2*k), m0 = rep(c(exp(p[8]), exp(p[9])), each=k), dV=0) +
             dlmModReg(bank_workday, addInt = FALSE, dW = exp(p[10]),dV=0, m0=as.numeric(lm(serie_tot_m1_log[which(wday(temps_tot) %in% c(2,3,4,5,6))] ~ as.factor(bank_workday[which(wday(temps_tot) %in% c(2,3,4,5,6))]))$coefficients[2]), C0 = exp(p[19])* diag(nrow = 1)) +
             dlmModReg(bank_weekend, addInt = FALSE, dW = exp(p[11]),dV=0, m0=as.numeric(lm(serie_tot_m1_log[which(wday(temps_tot) %in% c(1,7))] ~ as.factor(bank_weekend[which(wday(temps_tot) %in% c(1,7))]))$coefficients[2]), C0 = exp(p[20])* diag(nrow = 1)) +
             dlmModReg(extra, addInt = FALSE, dW = exp(p[12]),dV=0, m0=as.numeric(lm(serie_tot_m1_log[which(wday(temps_tot) %in% c(2,3,4,5,6))] ~ as.factor(extra[which(wday(temps_tot) %in% c(2,3,4,5,6))]))$coefficients[2]), C0 = exp(p[21])* diag(nrow = 1)) +
             dlmModReg(work_rer_worday, addInt = FALSE, dW = exp(p[13]),dV=0, m0 = as.numeric(lm(serie_tot_m1_log[which(wday(temps_tot) %in% c(2,3,4,5,6))] ~ as.factor(work_rer_worday[which(wday(temps_tot) %in% c(2,3,4,5,6))]))$coefficients[2]), C0 = exp(p[22])* diag(nrow = 1)) +
             dlmModReg(work_rer_weekend, addInt = FALSE, dW = exp(p[14]),dV=0, m0=as.numeric(lm(serie_tot_m1_log[which(wday(temps_tot) %in% c(1,7))] ~ as.factor(work_rer_weekend[which(wday(temps_tot) %in% c(1,7))]))$coefficients[2]), C0 = exp(p[23])* diag(nrow = 1)) +
             dlmModReg(strike, addInt = FALSE, dW = exp(p[15]),dV=0, m0 = as.numeric(lm(serie_tot_m1_log[which(wday(temps_tot) %in% c(2,3,4,5,6))] ~ as.factor(strike[which(wday(temps_tot) %in% c(2,3,4,5,6))]))$coefficients[2]), C0 = exp(p[24])* diag(nrow = 1)) +
             dlmModReg(free, addInt = FALSE, dW = exp(p[16]),dV=0, m0 = as.numeric(lm(serie_tot_m1_log ~ as.factor(free))$coefficients[2]), C0 = exp(p[25])* diag(nrow = 1)) +
             dlmModReg(work_m1_worday, addInt = FALSE, dW = exp(p[17]),dV=0, m0 = as.numeric(lm(serie_tot_m1_log[which(wday(temps_tot) %in% c(2,3,4,5,6))] ~ as.factor(work_m1_worday[which(wday(temps_tot) %in% c(2,3,4,5,6))]))$coefficients[2]), C0 = exp(p[26])* diag(nrow = 1)) +
             dlmModReg(work_m1_weekend, addInt = FALSE, dW = exp(p[18]),dV=0, m0=as.numeric(lm(serie_tot_m1_log[which(wday(temps_tot) %in% c(1,7))] ~ as.factor(work_m1_weekend[which(wday(temps_tot) %in% c(1,7))]))$coefficients[2]), C0 = exp(p[27])* diag(nrow = 1))
           
    )
    
  }
  
  
  
  init = rep(i, 27)
  
  
  
  init[5] = -10
  init[8] = -10
  init[9] = -10
  
  upper_bound = rep(Inf,27)
  upper_bound[2] = -15
  
  
  tryCatch({
    mleMod_mult_m1_exo = dlmMLE(serie_train_m1_log, parm = init, build = Mod_mult_m1_exo, hessian=T, upper = upper_bound, control=list(trace=1,REPORT=1,maxit=1000))
    fwrite(as.list(mleMod_mult_m1_exo$par), file = paste("m1_exo_harm",as.character(k),".txt"))
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}
  )
  
  
  
  
  
  
  #Modèle sans variables exogènes
  Mod_mult_m1 = function(p){
    return(dlmModPoly(2,dV=exp(p[1]), m0=c(mean(serie_tot_m1_log[1:10]),0), dW=c(exp(p[2]),0), C0 = diag(c(exp(p[3]), exp(p[4])))) + dlmModSeas(7,dW=c(rep(0,6)), m0 = rep(exp(p[5]),6),  C0 = exp(p[6]) * diag(nrow = 6), dV=0) +
             dlmModTrig(s=365, q = k, C0 = (exp(p[7]))*diag(2*k), m0 = rep(c(exp(p[8]), exp(p[9])), each=k), dV=0) 
           
    )
    
  }
  
  
  
  init = rep(i, 9)
  
  
  
  init[5] = -10
  init[8] = -10
  init[9] = -10
  
  upper_bound = rep(Inf,9)
  upper_bound[2] = -15
  
  
  tryCatch({
    mleMod_mult_m1 = dlmMLE(serie_train_m1_log, parm = init, build = Mod_mult_m1, hessian=T, upper = upper_bound, control=list(trace=1,REPORT=1,maxit=1000))
    fwrite(as.list(mleMod_mult_m1$par), file = paste("m1_harm",as.character(k),".txt"))
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}
  )
  
  
}




#RMSE FOR DIFFERENT FORECAST HORIZONS FOR ALL MODELS TESTED



# Construire tous les modèles

par_rera_exo_2 = unlist(as.list(read.table(read.table("rera_exo_harm 2 .txt",sep=','))))
par_rera_exo_4 = unlist(as.list(read.table(read.table("rera_exo_harm 4 .txt",sep=','))))
par_rera_exo_6 = unlist(as.list(read.table(read.table("rera_exo_harm 6 .txt",sep=','))))
par_rera_exo_8 = unlist(as.list(read.table(read.table("rera_exo_harm 8 .txt",sep=','))))
par_rera_exo_10 = unlist(as.list(read.table(read.table("rera_exo_harm 10 .txt",sep=','))))
par_rera_exo_12 =unlist(as.list(read.table(read.table("rera_exo_harm 12 .txt",sep=','))))

par_rera_2 = unlist(as.list(read.table(read.table("rera_harm 2 .txt",sep=','))))
par_rera_4 = unlist(as.list(read.table(read.table("rera_harm 4 .txt",sep=','))))
par_rera_6 = unlist(as.list(read.table(read.table("rera_harm 6 .txt",sep=','))))
par_rera_8 = unlist(as.list(read.table(read.table("rera_harm 8 .txt",sep=','))))
par_rera_10 = unlist(as.list(read.table(read.table("rera_harm 10 .txt",sep=','))))
par_rera_12 = unlist(as.list(read.table(read.table("rera_harm 12 .txt",sep=','))))

par_m1_exo_2 = unlist(as.list(read.table(read.table("m1_exo_harm 2 .txt",sep=','))))
par_m1_exo_4 = unlist(as.list(read.table(read.table("m1_exo_harm 4 .txt",sep=','))))
par_m1_exo_6 = unlist(as.list(read.table(read.table("m1_exo_harm 6 .txt",sep=','))))
par_m1_exo_8 = unlist(as.list(read.table(read.table("m1_exo_harm 8 .txt",sep=','))))
par_m1_exo_10 = unlist(as.list(read.table(read.table("m1_exo_harm 10 .txt",sep=','))))
par_m1_exo_12 = unlist(as.list(read.table(read.table("m1_exo_harm 12 .txt",sep=','))))

par_m1_2 =  unlist(as.list(read.table(read.table("mult_harm/m1_harm 2 .txt",sep=','))))
par_m1_4 = unlist(as.list(read.table(read.table("mult_harm/m1_harm 4 .txt",sep=','))))
par_m1_6 = unlist(as.list(read.table(read.table("m1_harm 6 .txt",sep=','))))
par_m1_8 = unlist(as.list(read.table(read.table("m1_harm 8 .txt",sep=','))))
par_m1_10 = unlist(as.list(read.table(read.table("mult_harm/m1_harm 10 .txt",sep=','))))
par_m1_12 = unlist(as.list(read.table(read.table("mult_harm/m1_harm 12 .txt",sep=','))))




harm_2 = list(par_rera_exo_2, par_rera_2, par_m1_exo_2, par_m1_2)
harm_4 = list(par_rera_exo_4, par_rera_4, par_m1_exo_4, par_m1_4)
harm_6 = list(par_rera_exo_6, par_rera_6,par_m1_exo_6, par_m1_6)
harm_8 = list(par_rera_exo_8, par_rera_8,par_m1_exo_8, par_m1_8)
harm_10 = list(par_rera_exo_10, par_rera_10,par_m1_exo_10, par_m1_10)
harm_12 = list(par_rera_exo_12, par_rera_12, par_m1_exo_12, par_m1_12)



liste_par = list(harm_2, harm_4, harm_6, harm_8, harm_10, harm_12)

liste_mod = list() #list of all models



k = 0
t = 0
for(par_harm in liste_par){
  k = k + 2
  rera_exo = Mod_mult_rera_exo(par_harm[[1]])
  rera = Mod_mult_rera(par_harm[[2]])
  m1_exo = Mod_mult_m1_exo(par_harm[[3]])
  m1 = Mod_mult_m1(par_harm[[4]])
  t = t+1
  liste_mod[[t]] = rera_exo
  t = t+1
  liste_mod[[t]] = rera
  t = t+1
  liste_mod[[t]] = m1_exo
  t = t+1
  liste_mod[[t]] = m1
}




liste_mod_rera = liste_mod[c(1,2,5,6,9,10,13,14,17,18,21,22)]
liste_mod_m1 = liste_mod[c(3,4,7,8,11,12,15,16,19,20,23,24)]







#Calculate RMSE for different time forecast horizons

q = 0
l_rmse = c()


for(jdd in list(liste_mod_rera2, liste_mod_m12)){
  q = q + 1
  if(q == 1){
    print('On est au RER A')
    serie_train = serie_train_rera_log
    serie_test = serie_test_rera_log
  } else{
    print('On est au M1')
    serie_train = serie_train_m1_log
    serie_test = serie_test_m1_log
  }
  s=0
  for(mod in jdd){
    s = s+1
    print(paste('Calcul RMSE pour jdd ', as.character(q), ' et modele ', as.character(s)))
    mod_built = mod
    
    l_pred1 = c()
    l_pred2 = c()
    l_pred3 = c()
    l_pred4 = c()
    l_pred5 = c()
    l_pred6 = c()
    l_pred7 = c()
    l_pred8 = c()
    l_pred9 = c()
    l_pred10 = c()
    l_pred11 = c()
    l_pred12 = c()
    l_pred13 = c()
    l_pred14 = c()
    
    
    l_or1 =c()
    l_or2 = c()
    l_or3 = c()
    l_or4 = c()
    l_or5 = c()
    l_or6 = c()
    l_or7 = c()
    l_or8 =c()
    l_or9 = c()
    l_or10 = c()
    l_or11 = c()
    l_or12 = c()
    l_or13 = c()
    l_or14 = c()
    
    
    for(j in seq(1,728)){
      print(j)
      if(!is.null(mod_built$X)){
        mod_built2 = mod_built
        lim = 1825 + j + 14
        mod_built2$X = mod_built$X[1:lim,]
        prediction_tot <- tail(exp(dlmFilter(c(serie_train, serie_test[1:j], rep(NA,14)),mod_built2)$f),14)
      }else{
        fenetre = dlmFilter(c(serie_train, serie_test[1:j]), mod_built)
        prediction_tot = exp(dlmForecast(fenetre, nAhead = 14)$f)
      }
      
      
      
      
      
      l_pred1 = c(l_pred1, prediction_tot[1])
      l_pred2 = c(l_pred2, prediction_tot[2])
      l_pred3 = c(l_pred3, prediction_tot[3])
      l_pred4 = c(l_pred4, prediction_tot[4])
      l_pred5 = c(l_pred5, prediction_tot[5])
      l_pred6 = c(l_pred6, prediction_tot[6])
      l_pred7 = c(l_pred7, prediction_tot[7])
      l_pred8 = c(l_pred8, prediction_tot[8])
      l_pred9 = c(l_pred9, prediction_tot[9])
      l_pred10 = c(l_pred10, prediction_tot[10])
      l_pred11= c(l_pred11, prediction_tot[11])
      l_pred12 = c(l_pred12, prediction_tot[12])
      l_pred13 = c(l_pred13, prediction_tot[13])
      l_pred14 = c(l_pred14, prediction_tot[14])
      
      
      l_or1 = c(l_or1, exp(serie_test[j+1]))
      l_or2 = c(l_or2, exp(serie_test[j+2]))
      l_or3 = c(l_or3, exp(serie_test[j+3]))
      l_or4 = c(l_or4, exp(serie_test[j+4]))
      l_or5 = c(l_or5, exp(serie_test[j+5]))
      l_or6 = c(l_or6, exp(serie_test[j+6]))
      l_or7 = c(l_or7, exp(serie_test[j+7]))
      l_or8 = c(l_or8, exp(serie_test[j+8]))
      l_or9 = c(l_or9, exp(serie_test[j+9]))
      l_or10 = c(l_or10, exp(serie_test[j+10]))
      l_or11 = c(l_or11, exp(serie_test[j+11]))
      l_or12 = c(l_or12, exp(serie_test[j+12]))
      l_or13 = c(l_or13, exp(serie_test[j+13]))
      l_or14 = c(l_or14, exp(serie_test[j+14]))
      
    }
    
    
    l_pred2 = l_pred2[1:727]
    l_or2 = l_or2[1:727]
    
    l_pred3 = l_pred3[1:726]
    l_or3 = l_or3[1:726]
    
    l_pred4 = l_pred4[1:725]
    l_or4 = l_or4[1:725]
    
    l_pred5 = l_pred5[1:724]
    l_or5 = l_or5[1:724]
    
    l_pred6 = l_pred6[1:723]
    l_or6 = l_or6[1:723]
    
    l_pred7 = l_pred7[1:722]
    l_or7 = l_or7[1:722]
    
    
    
    l_pred8 = l_pred8[1:721]
    l_or8 = l_or8[1:721]
    
    l_pred9 = l_pred9[1:720]
    l_or9 = l_or9[1:720]
    
    l_pred10 = l_pred10[1:719]
    l_or10 = l_or10[1:719]
    
    l_pred11 = l_pred11[1:718]
    l_or11 = l_or11[1:718]
    
    l_pred12 = l_pred12[1:717]
    l_or12 = l_or12[1:717]
    
    l_pred13 = l_pred13[1:716]
    l_or13 = l_or13[1:716]
    
    l_pred14 = l_pred14[1:715]
    l_or14 = l_or14[1:715]
    
    
    
    
    
    RMSE1 = get_RMSE(l_or1, l_pred1)
    RMSE2 = get_RMSE(l_or2, l_pred2)
    RMSE3 = get_RMSE(l_or3, l_pred3)
    RMSE4 = get_RMSE(l_or4, l_pred4)
    RMSE5 = get_RMSE(l_or5, l_pred5)
    RMSE6 = get_RMSE(l_or6, l_pred6)
    RMSE7 = get_RMSE(l_or7, l_pred7)
    
    RMSE8 = get_RMSE(l_or8, l_pred8)
    RMSE9 = get_RMSE(l_or9, l_pred9)
    RMSE10 = get_RMSE(l_or10, l_pred10)
    RMSE11 = get_RMSE(l_or11, l_pred11)
    RMSE12 = get_RMSE(l_or12, l_pred12)
    RMSE13 = get_RMSE(l_or13, l_pred13)
    RMSE14 = get_RMSE(l_or14, l_pred14)
    
    
    l_rmse = c(l_rmse,RMSE1,RMSE2,RMSE3,RMSE4,RMSE5,RMSE6,RMSE7,RMSE8,RMSE9,RMSE10,RMSE11,RMSE12,RMSE13,RMSE14)
    
  }
  
  
}




df_RMSE_mult =
  data.frame('RMSE' = unlist(l_rmse),
             'Station' = c(rep('RER A', 12*12), rep('M1', 12*12)),
             'Harmoniques' = rep(rep(c('2','4','6','8','10','12','0'),each = 24),2),
             'h' = rep(c(1,2,3,4,5,6,7,8,9,10,11,12,13,14),24),
             'Exo'= rep(rep(c(TRUE,FALSE),each=12),12)
  )



ggplot(df_RMSE_mult2 %>% filter(!Harmoniques %in% c('0','2') & h <= 7),aes(x=h,y=RMSE, colour = factor(Harmoniques))) + geom_line(size=2) + geom_point(size=4) + facet_wrap(Station~Exo, scale='free')










############################################################################################################################################################################
## APPLICATION MULTIPLICATIVE MODEL ON ALL DATA
############################################################################################################################################################################

#PREPARE DATA

#Exogenous data
data_calendar = data_calendar('2020-07-31')
free = matrix(as.numeric(data_calendar$free))
strike =  matrix(as.numeric(data_calendar$strike))
lockdown = matrix(as.numeric(data_calendar$lockdown))
unlockdown = matrix(as.numeric(data_calendar$unlockdown)) 
bank_workday =  matrix(as.numeric(data_calendar$bank_workday))
bank_weekend =  matrix(as.numeric(data_calendar$bank_weekend))
work_rer_worday =  matrix(as.numeric(data_calendar$work_rer_worday))
work_rer_weekend =  matrix(as.numeric(data_calendar$work_rer_weekend))
extra =  matrix(as.numeric(data_calendar$extra))
work_m1_worday =  matrix(as.numeric(data_calendar$work_m1_worday))
work_m1_weekend =  matrix(as.numeric(data_calendar$work_m1_weekend))  


#Ridership data 

#rera
serie_train_gen_rera = dt_Hub %>% filter(station == "RERA") %>% pull(ridership)
temps_tot_gen = dt_Hub %>% filter(station == "RERA") %>% pull(date)
serie_train_gen2 = serie_train_gen_rera
serie_train_gen2[serie_train_gen2 == 0] = 1
serie_train_gen_log_rera = log(serie_train_gen2)
rm(serie_train_gen2)


#m1
serie_train_gen_m1 = dt_Hub %>% filter(station == "M1") %>% pull(ridership)
serie_train_gen2 = serie_train_gen_m1
serie_train_gen2[serie_train_gen2 == 0] = 1
serie_train_gen_log_m1 = log(serie_train_gen2)
rm(serie_train_gen2)




#CALCULATE MODELS, TRY FROM MULTIPLE INITIALISATIONS FOR BFGS

#RERA

for(i in c(-10,-5,0,5,10)){
Mod_rob_mult_tout_rera = function(p){
  return(dlmModPoly(2,dV=exp(p[1]), m0=c(mean(serie_train_gen_log_rera[1:10]),0), dW=c(exp(p[2]),0), C0 = diag(c(exp(p[3]), exp(p[4])))) + dlmModSeas(7,dW=c(rep(0,6)), m0 = rep(exp(p[5]),6),  C0 = exp(p[6]) * diag(nrow = 6), dV=0) +
           dlmModTrig(s=365, q = 6, C0 = (exp(p[7]))*diag(12), m0 = rep(c(exp(p[8]), exp(p[9])), each=6), dV=0) +
           dlmModReg(bank_workday, addInt = FALSE, dW = exp(p[10]),dV=0, m0=as.numeric(lm(serie_train_gen_log_rera[which(wday(temps_tot_gen) %in% c(2,3,4,5,6))] ~ as.factor(bank_workday[which(wday(temps_tot_gen) %in% c(2,3,4,5,6))]))$coefficients[2]), C0 = exp(p[21])* diag(nrow = 1)) +
           dlmModReg(bank_weekend, addInt = FALSE, dW = exp(p[11]),dV=0, m0=as.numeric(lm(serie_train_gen_log_rera[which(wday(temps_tot_gen) %in% c(1,7))] ~ as.factor(bank_weekend[which(wday(temps_tot_gen) %in% c(1,7))]))$coefficients[2]), C0 = exp(p[22])* diag(nrow = 1)) +
           dlmModReg(extra, addInt = FALSE, dW = exp(p[12]),dV=0, m0=as.numeric(lm(serie_train_gen_log_rera[which(wday(temps_tot_gen) %in% c(2,3,4,5,6))] ~ as.factor(extra[which(wday(temps_tot_gen) %in% c(2,3,4,5,6))]))$coefficients[2]), C0 = exp(p[23])* diag(nrow = 1)) +
           dlmModReg(work_rer_worday, addInt = FALSE, dW = exp(p[13]),dV=0, m0 = as.numeric(lm(serie_train_gen_log_rera[which(wday(temps_tot_gen) %in% c(2,3,4,5,6))] ~ as.factor(work_rer_worday[which(wday(temps_tot_gen) %in% c(2,3,4,5,6))]))$coefficients[2]), C0 = exp(p[24])* diag(nrow = 1)) +
           dlmModReg(work_rer_weekend, addInt = FALSE, dW = exp(p[14]),dV=0, m0=as.numeric(lm(serie_train_gen_log_rera[which(wday(temps_tot_gen) %in% c(1,7))] ~ as.factor(work_rer_weekend[which(wday(temps_tot_gen) %in% c(1,7))]))$coefficients[2]), C0 = exp(p[25])* diag(nrow = 1)) +
           dlmModReg(strike, addInt = FALSE, dW = exp(p[15]),dV=0, m0 = as.numeric(lm(serie_train_gen_log_rera[which(wday(temps_tot_gen) %in% c(2,3,4,5,6))] ~ as.factor(strike[which(wday(temps_tot_gen) %in% c(2,3,4,5,6))]))$coefficients[2]), C0 = exp(p[26])* diag(nrow = 1)) +
           dlmModReg(free, addInt = FALSE, dW = exp(p[16]),dV=0, m0 = as.numeric(lm(serie_train_gen_log_rera ~ as.factor(free))$coefficients[2]), C0 = exp(p[27])* diag(nrow = 1)) +
           dlmModReg(lockdown, addInt = FALSE, dW = exp(p[17]),dV=0, m0 = as.numeric(lm(serie_train_gen_log_rera ~ as.factor(lockdown))$coefficients[2]), C0 = exp(p[28])* diag(nrow = 1)) +
           dlmModReg(unlockdown, addInt = FALSE, dW = exp(p[18]),dV=0, m0 = as.numeric(lm(serie_train_gen_log_rera ~ as.factor(unlockdown))$coefficients[2]), C0 = exp(p[29])* diag(nrow = 1)) +
           dlmModReg(work_m1_worday, addInt = FALSE, dW = exp(p[19]),dV=0, m0 = as.numeric(lm(serie_train_gen_log_rera[which(wday(temps_tot_gen) %in% c(2,3,4,5,6))] ~ as.factor(work_m1_worday[which(wday(temps_tot_gen) %in% c(2,3,4,5,6))]))$coefficients[2]), C0 = exp(p[30])* diag(nrow = 1)) +
           dlmModReg(work_m1_weekend, addInt = FALSE, dW = exp(p[20]),dV=0, m0=as.numeric(lm(serie_train_gen_log_rera[which(wday(temps_tot_gen) %in% c(1,7))] ~ as.factor(work_m1_weekend[which(wday(temps_tot_gen) %in% c(1,7))]))$coefficients[2]), C0 = exp(p[31])* diag(nrow = 1))
         
  )
  
}



init = rep(i, 31)


init[5] = 0
init[8] = 0
init[9] = 0

upper_bound = rep(Inf,31)
upper_bound[2] = -15




tryCatch({
  mleMod_rob_mult_tout_rera = dlmMLE(serie_train_gen_log_rera, parm = init, build = Mod_rob_mult_tout_rera, hessian=T, upper = upper_bound, control=list(trace=1,REPORT=1,maxit=1500))
  fwrite(as.list(mleMod_rob_mult_tout_rera$par), file = paste("mleMod_rob_mult_tout_rera_init",as.character(i),".txt"))
}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}
)      


}



#M1
for(i in c(-10,-5,0,5,10)){

Mod_rob_mult_tout_m1 = function(p){
  return(dlmModPoly(2,dV=exp(p[1]), m0=c(mean(serie_train_gen_log_m1[1:10]),0), dW=c(exp(p[2]),0), C0 = diag(c(exp(p[3]), exp(p[4])))) + dlmModSeas(7,dW=c(rep(0,6)), m0 = rep(exp(p[5]),6),  C0 = exp(p[6]) * diag(nrow = 6), dV=0) +
           dlmModTrig(s=365, q = 6, C0 = (exp(p[7]))*diag(12), m0 = rep(c(exp(p[8]), exp(p[9])), each=6), dV=0) +
           dlmModReg(bank_workday, addInt = FALSE, dW = exp(p[10]),dV=0, m0=as.numeric(lm(serie_train_gen_log_m1[which(wday(temps_tot_gen) %in% c(2,3,4,5,6))] ~ as.factor(bank_workday[which(wday(temps_tot_gen) %in% c(2,3,4,5,6))]))$coefficients[2]), C0 = exp(p[21])* diag(nrow = 1)) +
           dlmModReg(bank_weekend, addInt = FALSE, dW = exp(p[11]),dV=0, m0=as.numeric(lm(serie_train_gen_log_m1[which(wday(temps_tot_gen) %in% c(1,7))] ~ as.factor(bank_weekend[which(wday(temps_tot_gen) %in% c(1,7))]))$coefficients[2]), C0 = exp(p[22])* diag(nrow = 1)) +
           dlmModReg(extra, addInt = FALSE, dW = exp(p[12]),dV=0, m0=as.numeric(lm(serie_train_gen_log_m1[which(wday(temps_tot_gen) %in% c(2,3,4,5,6))] ~ as.factor(extra[which(wday(temps_tot_gen) %in% c(2,3,4,5,6))]))$coefficients[2]), C0 = exp(p[23])* diag(nrow = 1)) +
           dlmModReg(work_rer_worday, addInt = FALSE, dW = exp(p[13]),dV=0, m0 = as.numeric(lm(serie_train_gen_log_m1[which(wday(temps_tot_gen) %in% c(2,3,4,5,6))] ~ as.factor(work_rer_worday[which(wday(temps_tot_gen) %in% c(2,3,4,5,6))]))$coefficients[2]), C0 = exp(p[24])* diag(nrow = 1)) +
           dlmModReg(work_rer_weekend, addInt = FALSE, dW = exp(p[14]),dV=0, m0=as.numeric(lm(serie_train_gen_log_m1[which(wday(temps_tot_gen) %in% c(1,7))] ~ as.factor(work_rer_weekend[which(wday(temps_tot_gen) %in% c(1,7))]))$coefficients[2]), C0 = exp(p[25])* diag(nrow = 1)) +
           dlmModReg(strike, addInt = FALSE, dW = exp(p[15]),dV=0, m0 = as.numeric(lm(serie_train_gen_log_m1[which(wday(temps_tot_gen) %in% c(2,3,4,5,6))] ~ as.factor(strike[which(wday(temps_tot_gen) %in% c(2,3,4,5,6))]))$coefficients[2]), C0 = exp(p[26])* diag(nrow = 1)) +
           dlmModReg(free, addInt = FALSE, dW = exp(p[16]),dV=0, m0 = as.numeric(lm(serie_train_gen_log_m1 ~ as.factor(free))$coefficients[2]), C0 = exp(p[27])* diag(nrow = 1)) +
           dlmModReg(lockdown, addInt = FALSE, dW = exp(p[17]),dV=0, m0 = as.numeric(lm(serie_train_gen_log_m1 ~ as.factor(lockdown))$coefficients[2]), C0 = exp(p[28])* diag(nrow = 1)) +
           dlmModReg(unlockdown, addInt = FALSE, dW = exp(p[18]),dV=0, m0 = as.numeric(lm(serie_train_gen_log_m1 ~ as.factor(unlockdown))$coefficients[2]), C0 = exp(p[29])* diag(nrow = 1)) +
           dlmModReg(work_m1_worday, addInt = FALSE, dW = exp(p[19]),dV=0, m0 = as.numeric(lm(serie_train_gen_log_m1[which(wday(temps_tot_gen) %in% c(2,3,4,5,6))] ~ as.factor(work_m1_worday[which(wday(temps_tot_gen) %in% c(2,3,4,5,6))]))$coefficients[2]), C0 = exp(p[30])* diag(nrow = 1)) +
           dlmModReg(work_m1_weekend, addInt = FALSE, dW = exp(p[20]),dV=0, m0=as.numeric(lm(serie_train_gen_log_m1[which(wday(temps_tot_gen) %in% c(1,7))] ~ as.factor(work_m1_weekend[which(wday(temps_tot_gen) %in% c(1,7))]))$coefficients[2]), C0 = exp(p[31])* diag(nrow = 1))
         
  )
  
}





init = rep(i, 31)


init[5] = 0
init[8] = 0
init[9] = 0

upper_bound = rep(Inf,31)
upper_bound[2] = -15




tryCatch({
  mleMod_rob_mult_tout_m1 = dlmMLE(serie_train_gen_log_m1, parm = init, build = Mod_rob_mult_tout_m1, hessian=T, upper = upper_bound, control=list(trace=1,REPORT=1,maxit=1500))
  fwrite(as.list(mleMod_rob_mult_tout_m1$par), file = paste("mleMod_rob_mult_tout_m1_init",as.character(i),".txt"))
}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}
)      
}




##VISUALIZATION OF HIDDEN STATES

#Choose best models
LL_rera = c()
LL_m1 = c()
for(i in c(-10,-5,0,5,10)){
  mod_rera = unlist(as.list(read.table(paste0("mleMod_rob_mult_tout_rera_init",as.character(i))))) 
  mod_m1 = unlist(as.list(read.table(paste0("mleMod_rob_mult_tout_m1_init",as.character(i)))))  
  LL_rera = c(LL_rera, -dlmLL(serie_train_gen_log_rera, Mod_rob_mult_tout_rera(mod_rera)))
  LL_m1 = c(LL_m1, -dlmLL(serie_train_gen_log_m1, Mod_rob_mult_tout_m1(mod_m1)))
}


#M1
i_m1 = (c(-10,-5,0,5,10))[which(LL_m1 == max(LL_m1))]
mleMod_rob_mult_tout = unlist(as.list(read.table(paste0("mleMod_rob_mult_tout_m1_init",as.charcter(i_m1))))) 
mod_built_m1 = Mod_rob_mult_tout_m1(mleMod_rob_mult_tout)
mod.smoothed_m1 = dlmSmooth(serie_train_gen_log_m1, mod_built_m1)



#RERA
i_rera = (c(-10,-5,0,5,10))[which(LL_m1 == max(LL_m1))]
mleMod_rob_mult_tout = unlist(as.list(read.table(paste0("mleMod_rob_mult_tout_rera_init",as.character(i_rera))))) 
mod_built_rera = Mod_rob_mult_tout_rera(mleMod_rob_mult_tout)
mod.smoothed_rera = dlmSmooth(serie_train_gen_log_rera, mod_built_rera)





#Errors 
mse.list_smooth_m1 = dlmSvd2var(mod.smoothed_m1$U.S, mod.smoothed_m1$D.S)
mse.list_smooth_rera = dlmSvd2var(mod.smoothed_rera$U.S, mod.smoothed_rera$D.S)


mod.smoothed_m1$s = mod.smoothed_m1$s[-1,]
mod.smoothed_rera$s = mod.smoothed_rera$s[-1,]



#Smoothed states

#trend
trend_m1 = c()
err_trend_m1 = c()
trend_rera = c()
err_trend_rera = c()


#weekly seasonality
sem_m1 = c()
err_sem_m1 = c()
sem_rera = c()
err_sem_rera = c()




#yearly seasonality
ann_m1 = c()
err_ann_m1 = c()
ann_rera = c()
err_ann_rera = c()



#work days rer line
j_travaux_m1 = c()
err_travaux_m1 = c()
j_travaux_rera = c()
err_travaux_rera = c()


#strike days
j_greves_m1 = c()
err_greves_m1 = c()
j_greves_rera = c()
err_greves_rera = c()




#lockdown
j_confinement_m1 = c()
err_confinement_m1 = c()
j_confinement_rera = c()
err_confinement_rera = c()



#bank holidays
j_ferie_m1 = c()
err_ferie_m1 = c()
j_ferie_rera = c()
err_ferie_rera = c()



#work days m1
j_travaux_m1_m1 = c()
err_travaux_m1_m1 = c()
j_travaux_m1_rera = c()
err_travaux_m1_rera = c()




for (t in seq(1,length(serie_train_gen_rera))){
  XFF <- mod_built_rera$FF
  XFF[mod_built_rera$JFF != 0] <- mod_built_rera$X[t, mod_built_rera$JFF]
  
  #trend
  trend_m1 = c(trend_m1,t(mod.smoothed_m1$s[t,1:2])%*%XFF[1:2])
  err_trend_m1 = c(err_trend_m1,   XFF[1] %*% mse.list_smooth_m1[[t]][1,1] %*% t(XFF[1]))
  
  trend_rera = c(trend_rera, t(mod.smoothed_rera$s[t,1:2])%*%XFF[1:2])
  err_trend_rera = c(err_trend_rera, XFF[1] %*% mse.list_smooth_rera[[t]][1,1] %*% t(XFF[1]))
  
  
  
  #saisonnalité semaine
  sem_m1 = c(sem_m1, t(mod.smoothed_m1$s[t,3:8])%*%XFF[3:8])
  err_sem_m1 = c(err_sem_m1, XFF[3] %*% mse.list_smooth_m1[[t]][3,3] %*% t(XFF[3]))   
  
  sem_rera = c(sem_rera, t(mod.smoothed_rera$s[t,3:8])%*%XFF[3:8])
  err_sem_rera = c(err_sem_rera, XFF[3] %*% mse.list_smooth_rera[[t]][3,3] %*% t(XFF[3])) 
  
  
  #saisonnalité annuelle
  ann_m1 = c(ann_m1, t(mod.smoothed_m1$s[t,9:20])%*%XFF[9:20])
  err_ann_m1 = c(err_ann_m1, XFF[9:20] %*% mse.list_smooth_m1[[t]][9:20,9:20] %*% XFF[9:20])
  
  ann_rera = c(ann_rera, t(mod.smoothed_rera$s[t,9:20])%*%XFF[9:20])
  err_ann_rera = c(err_ann_rera, XFF[9:20] %*% mse.list_smooth_rera[[t]][9:20,9:20] %*% XFF[9:20])
  
  
  
  
  #travaux rer A
  j_travaux_m1 = c(j_travaux_m1, t(mod.smoothed_m1$s[t,24:25])%*%XFF[24:25])
  err_travaux_m1 = c(err_travaux_m1,  XFF[24:25] %*% mse.list_smooth_m1[[t]][24:25,24:25] %*% (XFF[24:25]))
  
  j_travaux_rera = c(j_travaux_rera, t(mod.smoothed_rera$s[t,24:25])%*%XFF[24:25])
  err_travaux_rera = c(err_travaux_rera,  XFF[24:25] %*% mse.list_smooth_rera[[t]][24:25,24:25] %*% (XFF[24:25]))
  
  
  
  
  #greves ratp
  j_greves_m1 = c(j_greves_m1, t(mod.smoothed_m1$s[t,26])%*%XFF[26])
  err_greves_m1 = c(err_greves_m1,  XFF[26] %*% mse.list_smooth_m1[[t]][26,26] %*% (XFF[26]))
  
  j_greves_rera = c(j_greves_rera, t(mod.smoothed_rera$s[t,26])%*%XFF[26])
  err_greves_rera = c(err_greves_rera,  XFF[26] %*% mse.list_smooth_rera[[t]][26,26] %*% (XFF[26]))
  
  
  
  #confinement
  j_confinement_m1 = c(j_confinement_m1, t(mod.smoothed_m1$s[t,28:29])%*%XFF[28:29])
  err_confinement_m1 = c(err_confinement_m1,  XFF[28:29] %*% mse.list_smooth_m1[[t]][28:29,28:29] %*% (XFF[28:29]))
  
  j_confinement_rera = c(j_confinement_rera, t(mod.smoothed_rera$s[t,28:29])%*%XFF[28:29])
  err_confinement_rera = c(err_confinement_rera,  XFF[28:29] %*% mse.list_smooth_rera[[t]][28:29,28:29] %*% (XFF[28:29]))
  
  
  
  
  #jours fériés
  j_ferie_m1 = c(j_ferie_m1, t(mod.smoothed_m1$s[t,21:22])%*%XFF[21:22])
  err_ferie_m1 = c(err_ferie_m1,  XFF[21:22] %*% mse.list_smooth_m1[[t]][21:22,21:22] %*% (XFF[21:22]))
  
  j_ferie_rera = c(j_ferie_rera, t(mod.smoothed_rera$s[t,21:22])%*%XFF[21:22])
  err_ferie_rera = c(err_ferie_rera,  XFF[21:22] %*% mse.list_smooth_rera[[t]][21:22,21:22] %*% (XFF[21:22]))
  
  
  
  #travaux M1
  j_travaux_m1_m1 = c(j_travaux_m1_m1, t(mod.smoothed_m1$s[t,30:31])%*%XFF[30:31])
  err_travaux_m1_m1 = c(err_travaux_m1_m1,  XFF[30:31] %*% mse.list_smooth_m1[[t]][30:31,30:31] %*% (XFF[30:31]))
  
  j_travaux_m1_rera = c(j_travaux_m1_rera, t(mod.smoothed_rera$s[t,30:31])%*%XFF[30:31])
  err_travaux_m1_rera = c(err_travaux_m1_rera,  XFF[30:31] %*% mse.list_smooth_rera[[t]][30:31,30:31] %*% (XFF[30:31]))
}



## Création des figures 

#Tendances et ann

labels_names <- list(
  'Original'=expression(y[t]),
  'Trend'=expression(l[t]),
  'Yearly'=expression(f[t])
)

labeller <- function(variable,value){
  return(labels_names[value])
}



#M1
a = ggplot(data.frame(
  'temps' = c(temps_tot_gen, temps_tot_gen,temps_tot_gen),
  'mu' = c((serie_train_gen_m1),(trend_m1), (ann_m1)),
  'upper' = c(serie_train_gen_m1, (trend_m1 + qnorm(0.025, lower=F) * sqrt(err_trend_m1)), (ann_m1 + qnorm(0.025, lower=F) * sqrt(err_ann_m1))),
  'lower' = c(serie_train_gen_m1,(trend_m1 - qnorm(0.025, lower=F) * sqrt(err_trend_m1)),(ann_m1 - qnorm(0.025, lower=F) * sqrt(err_ann_m1))),
  'serie'= c(rep('Original', length(temps_tot_gen)), rep('Trend', length(temps_tot_gen)), rep('Yearly', length(temps_tot_gen)) )
), aes(x=temps, y=mu, group=factor(serie,levels=c("Originale","Trend","Yearly")))) + 
  #geom_rect( aes(xmin = as.Date('2012-09-30')  , xmax = as.Date('2012-10-14'), 
  #ymin = -Inf, ymax = Inf, colour = serie,fill=NA), size=0.8) +
  geom_line(aes(size=serie)) + 
  geom_ribbon(aes(ymin=lower, ymax=upper,fill=serie), alpha=0.7) +
  facet_grid(serie~., scale='free', labeller=labeller) + theme_bw() +
  theme( legend.position = "none",legend.direction = "vertical",  strip.text.y = element_text(size=30,face='bold'),
         legend.box = "vertical", panel.spacing = unit(2, "lines"),
         legend.justification = c(0, 1),
         legend.title=element_text(size=30, face='bold'),
         axis.title=element_text(size=30,face="bold"), axis.text = element_text(size=25),axis.text.x = element_text( vjust = 0.5),
         legend.text = element_text(size = 25,face='bold'),legend.key.width=unit(4, "cm"),legend.key.height=unit(1, "cm"),
  ) + 
  xlab('Date') +
  scale_y_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE))+
  scale_fill_manual(values = c("Original"="black","Trend"="gray78","Yearly"="gray78")) +
  scale_size_manual(values = c("Original"=0.7,"Trend"=1.2,"Yearly"=1)) + ylab("") + 
  scale_colour_manual(values = c("Original"=NA,"Trend"=NA,"Yearly"='black')) + ylab("") 


plot1 = ggplot(data.frame(
  'temps' = c(temps_tot_gen),
  'mu' = c((ann_m1)),
  'upper' = c((ann_m1 + qnorm(0.025, lower=F) * sqrt(err_ann_m1))),
  'lower' = c((ann_m1 - qnorm(0.025, lower=F) * sqrt(err_ann_m1))),
  'serie'= c(rep('Yearly', length(temps_tot_gen)) ))%>%
    mutate(vacs1 = vacances_semaine, vacs2 = vacances_weekend) %>%
    mutate(vacs = vacs1 + vacs2) %>%
    mutate(vacs = factor(vacs)) %>%
    filter(year(temps)==2012) , 
  aes(x=temps, y=mu)) +
  geom_tile( aes(y=0, fill =vacs ), alpha=.4) +
  geom_line(size=1.2) + 
  geom_rect( aes(xmin = as.Date('2012-05-07')  , xmax = as.Date('2012-06-24'), 
                 ymin = -0.5, ymax = .5),fill=NA, colour = "black", size=0.8) +
  geom_ribbon(aes(ymin=lower, ymax=upper),fill="gray78", alpha=0.7) +
  theme_bw() +
  theme(legend.position = "none",legend.direction = "vertical", , strip.text.y = element_text(size=30,face='bold'),
        legend.box = "vertical", panel.spacing = unit(2, "lines"),
        legend.justification = c(0, 1),
        legend.title=element_text(size=30, face='bold'),
        axis.title=element_text(size=30,face="bold"),axis.text.y = element_text(size=25, vjust = 0.5), axis.ticks.x = element_blank(),
        legend.text = element_text(size = 25,face='bold'),legend.key.width=unit(4, "cm"),legend.key.height=unit(1, "cm"), axis.text.x = element_text(size=15)
  ) + 
  xlab('') +
  scale_y_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE), position = "right")+
  ylab(expression(f[t]) ) +
  scale_fill_brewer('BuGn')  + 
  geom_text(label="February", x=as.Date(c("2012-02-25")),y=.25,color = "black", size=6,angle = 90) +
  geom_text(label="Spring", x=as.Date(c("2012-04-22")),y=.25,color = "black", size=6,angle = 90) +
  geom_text(label="Summer", x=as.Date(c("2012-08-05")),y=.25,color = "black", size=6,angle = 90) +
  geom_text(label="Toussaint", x=as.Date(c("2012-11-04")),y=.25,color = "black", size=6,angle = 90) +
  geom_text(label="Christmas", x=as.Date(c("2012-12-22")),y=.3,color = "black", size=6,angle = 90) +
  scale_x_date(date_breaks = "1.5 month", date_labels = c("Jan","Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))




plot2 =  ggplot(data.frame(
  'temps' = c(temps_tot_gen,temps_tot_gen),
  'sem' = c((sem_m1),(sem_rera)),
  'upper' = c((sem_m1 + qnorm(0.025, lower=F) * sqrt(err_sem_m1)), (sem_rera + qnorm(0.025, lower=F) * sqrt(err_sem_rera))),
  'lower' = c((sem_m1 - qnorm(0.025, lower=F) * sqrt(err_sem_m1)), (sem_rera - qnorm(0.025, lower=F) * sqrt(err_sem_rera))),
  'station'= c(rep('Metro 1', length(temps_tot_gen)), rep('RER A', length(temps_tot_gen)))
) %>% filter(temps >= '2012-05-07' & temps <= '2012-06-24' & station == 'Metro 1'), aes(x=temps, y=sem,group=station,colour=station)) + 
  geom_line(size=1, color="black") +
  ylab(expression(s[t]))+theme_bw() +
  theme( legend.position = "none",legend.direction = "vertical", , strip.text.y = element_text(size=30,face='bold'),
         legend.box = "vertical", panel.spacing = unit(2, "lines"),
         legend.justification = c(0, 1),
         legend.title=element_text(size=30, face='bold'),
         axis.title=element_text(size=25,face="bold"), axis.text.x = element_blank(),axis.text.y = element_text(size=25, vjust = 0.5), axis.ticks.x = element_blank(),
         legend.text = element_text(size = 20,face='bold'),legend.key.width=unit(4, "cm"),legend.key.height=unit(1, "cm"))+
  scale_x_date(labels = date_format("%Y-%m-%d")) + xlab('') +
  scale_y_continuous(, position = "right")





arrowA <- data.frame(x1 = 8.5, x2 = 8.5, y1 = -1.5, y2 = -3)

ggplot() + coord_equal(xlim = c(-20, 20), ylim = c(-10, 10), expand = FALSE) +
  annotation_custom(ggplotGrob(a), xmin = -20, xmax = 1, ymin = -10, 
                    ymax = 10) +
  annotation_custom(ggplotGrob(plot1), xmin = 2, xmax = 20, ymin = -2, 
                    ymax = 8) +
  annotation_custom(ggplotGrob(plot2), xmin = 4, xmax = 16, ymin = -3, 
                    ymax = -7) +
  geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2), data = arrowA, 
               arrow = arrow(), lineend = "round",size=1.5) +
  theme_void() 






#RERA
a= ggplot(data.frame(
  'temps' = c(temps_tot_gen, temps_tot_gen,temps_tot_gen),
  'mu' = c((serie_train_gen_rera),(trend_rera), (ann_rera)),
  'upper' = c(serie_train_gen_rera, (trend_rera + qnorm(0.025, lower=F) * sqrt(err_trend_rera)), (ann_rera + qnorm(0.025, lower=F) * sqrt(err_ann_rera))),
  'lower' = c(serie_train_gen_rera,(trend_rera - qnorm(0.025, lower=F) * sqrt(err_trend_rera)),(ann_rera - qnorm(0.025, lower=F) * sqrt(err_ann_rera))),
  'serie'= c(rep('Original', length(temps_tot_gen)), rep('Trend', length(temps_tot_gen)), rep('Yearly', length(temps_tot_gen)) )
), aes(x=temps, y=mu, group=factor(serie,levels=c("Originale","Trend","Yearly")))) +
  #geom_rect( aes(xmin = as.Date('2012-09-30')  , xmax = as.Date('2012-10-14'), 
  #              ymin = -Inf, ymax = Inf, colour = serie,fill=NA), size=0.8) +
  geom_line(aes(size=serie)) + geom_ribbon(aes(ymin=lower, ymax=upper, fill=serie), alpha=0.7) +
  facet_grid(serie~., scale='free', labeller=labeller) + theme_bw() +
  theme( legend.position = "none",legend.direction = "vertical", , strip.text.y = element_text(size=30,face='bold'),
         legend.box = "vertical", panel.spacing = unit(2, "lines"),
         legend.justification = c(0, 1),
         legend.title=element_text(size=30, face='bold'),
         axis.title=element_text(size=30,face="bold"), axis.text = element_text(size=25),axis.text.x = element_text( vjust = 0.5),
         legend.text = element_text(size = 25,face='bold'),legend.key.width=unit(4, "cm"),legend.key.height=unit(1, "cm")) + xlab('Date') +
  scale_y_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE))+
  scale_fill_manual(values = c("Original"="black","Trend"="gray78","Yearly"="gray78")) +
  scale_size_manual(values = c("Original"=0.7,"Trend"=1.2,"Yearly"=1)) + ylab("") +
  scale_colour_manual(values = c("Original"=NA,"Trend"=NA,"Yearly"='black')) 



plot1 = ggplot(data.frame(
  'temps' = c(temps_tot_gen),
  'mu' = c((ann_rera)),
  'upper' = c((ann_rera + qnorm(0.025, lower=F) * sqrt(err_ann_rera))),
  'lower' = c((ann_rera - qnorm(0.025, lower=F) * sqrt(err_ann_rera))),
  'serie'= c(rep('Yearly', length(temps_tot_gen)) ))%>%
    mutate(vacs1 = vacances_semaine, vacs2 = vacances_weekend) %>%
    mutate(vacs = vacs1 + vacs2) %>%
    mutate(vacs = factor(vacs)) %>%
    filter(year(temps)==2012) , 
  aes(x=temps, y=mu)) +
  geom_tile( aes(y=0, fill =vacs ), alpha=.4) +
  geom_line(size=1.2) + 
  geom_rect( aes(xmin = as.Date('2012-05-07')  , xmax = as.Date('2012-06-24'), 
                 ymin = -0.5, ymax = .5),fill=NA, colour = "black", size=0.8) +
  geom_ribbon(aes(ymin=lower, ymax=upper),fill="gray78", alpha=0.7) +
  theme_bw() +
  theme(legend.position = "none",legend.direction = "vertical", , strip.text.y = element_text(size=30,face='bold'),
        legend.box = "vertical", panel.spacing = unit(2, "lines"),
        legend.justification = c(0, 1),
        legend.title=element_text(size=30, face='bold'),
        axis.title=element_text(size=30,face="bold"), axis.text.x = element_text(size=15),axis.text.y = element_text(size=25, vjust = 0.5), axis.ticks.x = element_blank(),
        legend.text = element_text(size = 25,face='bold'),legend.key.width=unit(4, "cm"),legend.key.height=unit(1, "cm")
  ) + 
  xlab('') +
  scale_y_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE), position = "right")+
  ylab(expression(f[t]) ) +
  scale_fill_brewer('BuGn')  + 
  geom_text(label="February", x=as.Date(c("2012-02-25")),y=.25,color = "black", size=6, angle=90) +
  geom_text(label="Spring", x=as.Date(c("2012-04-22")),y=.25,color = "black", size=6, angle=90) +
  geom_text(label="Summer", x=as.Date(c("2012-08-05")),y=.25,color = "black", size=6, angle=90) +
  geom_text(label="Toussaint", x=as.Date(c("2012-11-04")),y=.25,color = "black", size=6, angle=90) +
  geom_text(label="Christmas", x=as.Date(c("2012-12-25")),y=.3,color = "black", size=6, angle=90) +
  scale_x_date(date_breaks = "1.5 month", date_labels = c("Jan","Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))



plot2 =  ggplot(data.frame(
  'temps' = c(temps_tot_gen,temps_tot_gen),
  'sem' = c((sem_m1),(sem_rera)),
  'upper' = c((sem_m1 + qnorm(0.025, lower=F) * sqrt(err_sem_m1)), (sem_rera + qnorm(0.025, lower=F) * sqrt(err_sem_rera))),
  'lower' = c((sem_m1 - qnorm(0.025, lower=F) * sqrt(err_sem_m1)), (sem_rera - qnorm(0.025, lower=F) * sqrt(err_sem_rera))),
  'station'= c(rep('Metro 1', length(temps_tot_gen)), rep('RER A', length(temps_tot_gen)))
) %>% filter(temps >= '2012-05-07' & temps <= '2012-06-24' & station == 'RER A'), aes(x=temps, y=sem,group=station,colour=station)) + 
  geom_line(size=1, color="black") +
  ylab(expression(s[t]))+theme_bw() +
  theme( legend.position = "none",legend.direction = "vertical", , strip.text.y = element_text(size=30,face='bold'),
         legend.box = "vertical", panel.spacing = unit(2, "lines"),
         legend.justification = c(0, 1),
         legend.title=element_text(size=30, face='bold'),
         axis.title=element_text(size=25,face="bold"), axis.text.x = element_blank(),axis.text.y = element_text(size=25, vjust = 0.5), axis.ticks.x = element_blank(),
         legend.text = element_text(size = 20,face='bold'),legend.key.width=unit(4, "cm"),legend.key.height=unit(1, "cm"))+
  scale_x_date(labels = date_format("%Y-%m-%d")) + xlab('') +
  scale_y_continuous(, position = "right")


arrowA <- data.frame(x1 = 8.5, x2 = 8.5, y1 = -1.5, y2 = -3)

ggplot() + coord_equal(xlim = c(-20, 20), ylim = c(-10, 10), expand = FALSE) +
  annotation_custom(ggplotGrob(a), xmin = -20, xmax = 1, ymin = -10, 
                    ymax = 10) +
  annotation_custom(ggplotGrob(plot1), xmin = 2, xmax = 20, ymin = -2, 
                    ymax = 8) +
  annotation_custom(ggplotGrob(plot2), xmin = 4, xmax = 16, ymin = -3, 
                    ymax = -7) +
  geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2), data = arrowA, 
               arrow = arrow(), lineend = "round",size=1.5) +
  theme_void() 




#Semaine


#plot saisonnalité semaine
ggplot(data.frame(
  'temps' = c(as.Date(temps_tot_gen),as.Date(temps_tot_gen)),
  'sem' = c((sem_m1),(sem_rera)),
  'upper' = c((sem_m1 + qnorm(0.025, lower=F) * sqrt(err_sem_m1)), (sem_rera + qnorm(0.025, lower=F) * sqrt(err_sem_rera))),
  'lower' = c((sem_m1 - qnorm(0.025, lower=F) * sqrt(err_sem_m1)), (sem_rera - qnorm(0.025, lower=F) * sqrt(err_sem_rera))),
  'station'= c(rep('Metro 1', length(temps_tot_gen)), rep('RER A', length(temps_tot_gen)))
) %>% filter(temps >= as.Date('2019-01-07') & temps < as.Date('2019-01-14')), aes(x=temps, y=sem,group=station,colour=station)) + geom_line(size=1.6) +
  ylab(expression(s[1][','][t])) + theme_bw() +
  geom_errorbar(aes(ymin= lower, ymax=upper, colour=station), width=.1,size=1)+ 
  theme( legend.direction = "horizontal",
         legend.box = "vertical",
         legend.position="top",
         legend.justification = c(0, 1),
         legend.title=element_text(size=35, face='bold'),
         axis.title=element_text(size=30,face="bold"), axis.text = element_text(size=15),axis.text.x = element_text(angle = 50, vjust = 0.5),
         legend.text = element_text(size = 30,face='bold'),legend.key.width=unit(4, "cm"),legend.key.height=unit(1, "cm"),panel.grid.major =  element_line(size=1.8),
         panel.grid.minor =  element_line(size=1.8), axis.ticks.x=element_blank()) +
  scale_x_date(breaks = seq(as.Date("2019-01-07"), as.Date("2019-01-13"), by="1 day"),
               labels= c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday')) +
  scale_colour_brewer(name = "", labels = c("Metro 1", "RER A"),palette = 'Accent') + 
  xlab('')




#plot jours travaux
ggplot(data.frame(
  'temps' = c(temps_tot_gen,temps_tot_gen),
  'travaux' = c(j_travaux_m1,j_travaux_rera),
  'upper' = c(j_travaux_m1 + qnorm(0.025, lower=F) * sqrt(err_travaux_m1), j_travaux_rera + qnorm(0.025, lower=F) * sqrt(err_travaux_rera)),
  'lower' = c(j_travaux_m1 - qnorm(0.025, lower=F) * sqrt(err_travaux_m1), j_travaux_rera - qnorm(0.025, lower=F) * sqrt(err_travaux_rera)),
  'station'= c(rep('Metro 1', length(temps_tot_gen)), rep('RER A', length(temps_tot_gen))),
  'ferie'=c(ferie)
) %>% filter(month(temps) %in% c(7,8) & year(temps) >= 2015 ) %>% mutate(wday=wday(temps)) %>% mutate(non_work = ifelse(wday %in% c(1,7) | ferie == 1, T,F)) %>%
  mutate(groupe = ifelse(year(temps) %in% c(2015,2016,2017), 'First', 'Second')) %>% 
  filter( (temps > '2015-07-20' & temps < '2015-09-01') | (temps > '2016-07-20' & temps < '2016-09-01') | (temps > '2017-07-20' & temps < '2017-09-01') |
            (temps > '2018-07-20' & temps < '2018-09-01') | temps > '2019-01-01' ) %>%
  filter(station == 'RER A'),
aes(x=temps, y=travaux, group = station)) + 
  geom_line(size=1.2) + 
  geom_ribbon(aes(ymin=lower, ymax=upper, fill=station), alpha=0.7) +
  ylab(expression(sum(beta[t]^{(s )}*X[t]^{(s)}, s %in% {(scriptscriptstyle('Work days RER line'))},))) + 
  facet_wrap(.~year(temps),scale='free')  +  theme_bw() +
  theme( legend.direction = "horizontal",
         legend.box = "vertical",
         legend.position="top",
         legend.justification = c(0, 1),
         title = element_text(size=20), legend.key.size = unit(10,"line"),
         legend.title=element_text(size=30, face='bold'),
         axis.title=element_text(size=30,face="bold"), axis.text.y = element_text(size=30),axis.text.x = element_text(angle=60,size=20, vjust = 0.5), strip.text.x = element_text(size=30),
         legend.text = element_text(size = 25,face='bold'),legend.key.width=unit(4, "cm"),legend.key.height=unit(1, "cm"),strip.text.y = element_text(size=30,face='bold')) +
  xlab('Date') +
  scale_x_date(labels = date_format("%d-%m")) +
  scale_fill_manual(values = c("RER A"="gray78","Metro 1"="gray78"),guide = FALSE)+
  scale_colour_brewer(name = "", labels = c("Working days", "Non working days"),palette = 'Set1') + 
  geom_point(shape=20,size=5,aes( colour=non_work)) + ylim(c(-1.4, 1)) +
  guides(color = guide_legend(override.aes = list(size = 10)))






ggplot(data.frame(
  'temps' = c(temps_tot_gen,temps_tot_gen),
  'travaux' = c(j_travaux_m1,j_travaux_rera),
  'upper' = c(j_travaux_m1 + qnorm(0.025, lower=F) * sqrt(err_travaux_m1), j_travaux_rera + qnorm(0.025, lower=F) * sqrt(err_travaux_rera)),
  'lower' = c(j_travaux_m1 - qnorm(0.025, lower=F) * sqrt(err_travaux_m1), j_travaux_rera - qnorm(0.025, lower=F) * sqrt(err_travaux_rera)),
  'station'= c(rep('Metro 1', length(temps_tot_gen)), rep('RER A', length(temps_tot_gen))),
  'ferie'=c(ferie)
) %>% filter(month(temps) %in% c(7,8) & year(temps) >= 2015 ) %>% mutate(wday=wday(temps)) %>% mutate(non_work = ifelse(wday %in% c(1,7) | ferie == 1, T,F)) %>%
  mutate(groupe = ifelse(year(temps) %in% c(2015,2016,2017), 'First', 'Second')) %>% 
  filter( (temps > '2015-07-20' & temps < '2015-09-01') | (temps > '2016-07-20' & temps < '2016-09-01') | (temps > '2017-07-20' & temps < '2017-09-01') |
            (temps > '2018-07-20' & temps < '2018-09-01') | temps > '2019-01-01' ) %>%
  filter(station == 'Metro 1'),
aes(x=temps, y=travaux, group = station)) + 
  geom_line(size=1.2) + 
  geom_ribbon(aes(ymin=lower, ymax=upper, fill=station), alpha=0.7) +
  ylab(expression(sum(beta[t]^{(s )}*X[t]^{(s)}, s %in% {(scriptscriptstyle('Work days RER line'))},))) + 
  facet_wrap(.~year(temps),scale='free')  + theme_bw() +
  theme( legend.direction = "horizontal",
         legend.box = "vertical",
         legend.position="top",
         legend.justification = c(0, 1),
         title = element_text(size=20), legend.key.size = unit(10,"line"),
         legend.title=element_text(size=30, face='bold'),
         axis.title=element_text(size=30,face="bold"), axis.text.y = element_text(size=30),axis.text.x = element_text(angle=60,size=20, vjust = 0.5), strip.text.x = element_text(size=30),
         legend.text = element_text(size = 25,face='bold'),legend.key.width=unit(4, "cm"),legend.key.height=unit(1, "cm"),strip.text.y = element_text(size=30,face='bold')) +
  xlab('Date') +
  scale_x_date(labels = date_format("%d-%m")) +
  scale_fill_manual(values = c("RER A"="gray78","Metro 1"="gray78"),guide = FALSE)+
  scale_colour_brewer(name = "", labels = c("Working days", "Non working days"),palette = 'Set1') + 
  geom_point(shape=20,size=5,aes( colour=non_work)) + ylim(c(-0.1, 1)) +
  guides(color = guide_legend(override.aes = list(size = 10)))









#plot jours travaux M1
ggplot(data.frame(
  'temps' = c(temps_tot_gen,temps_tot_gen),
  'travaux' = c(j_travaux_m1_m1, j_travaux_m1_rera),
  'upper' = c(j_travaux_m1_m1 + qnorm(0.025, lower=F) * sqrt(err_travaux_m1_m1), j_travaux_m1_rera + qnorm(0.025, lower=F) * sqrt(err_travaux_m1_rera)),
  'lower' = c(j_travaux_m1_m1 - qnorm(0.025, lower=F) * sqrt(err_travaux_m1_m1), j_travaux_m1_rera - qnorm(0.025, lower=F) * sqrt(err_travaux_m1_rera)),
  'station'= c(rep('Metro 1', length(temps_tot_gen)), rep('RER A', length(temps_tot_gen))),
  'ferie'=c(ferie,ferie)
) %>% filter((temps > '2014-08-12' & temps < '2014-08-22') | (temps > '2016-07-10' & temps < '2016-07-21') | 
               (temps > '2017-05-01' & temps < '2017-05-12')  | 
               (temps > '2018-05-01' & temps < '2018-05-13')) %>% mutate(wday=wday(temps)) %>% mutate(non_work = ifelse(wday %in% c(1,7) | ferie == 1, T,F)),
aes(x=temps, y=travaux, group = station)) + 
  geom_line(size=1.2) + 
  geom_ribbon(aes(ymin=lower, ymax=upper, fill=station), alpha=0.7) +
  ylab(expression(sum(beta[t]^{(s )}*X[t]^{(s)}, s %in% {(scriptscriptstyle('Work days metro line'))},))) +
  facet_grid(factor(station, levels=c('RER A', 'Metro 1'))~year(temps),scale='free')  + theme_bw() +
  theme( legend.direction ="horizontal",
         legend.box = "vertical",
         legend.position="top",
         legend.justification = c(0, 1),
         title = element_text(size=20),
         legend.title=element_text(size=30, face='bold'),
         axis.title=element_text(size=30,face="bold"), axis.text.y = element_text(size=30),axis.text.x = element_text(angle=60,size=20, vjust = 0.5), strip.text.x = element_text(size=30),
         legend.text = element_text(size = 25,face='bold'),legend.key.width=unit(4, "cm"),legend.key.height=unit(1, "cm"),strip.text.y = element_text(size=30,face='bold')) +
  xlab('Date') +
  scale_x_date(labels = date_format("%d-%m")) +
  scale_fill_manual(values = c("RER A"="gray78","Metro 1"="gray78"),guide = FALSE)+
  scale_colour_brewer(name = "", labels = c("Working days", "Non working days"),palette = 'Set1') + 
  geom_point(shape=20,size=5,aes( colour=non_work)) +
  guides(color = guide_legend(override.aes = list(size = 10)))





#plot jours greves

zz = data.frame(
  'temps' = c(temps_tot_gen, temps_tot_gen),
  'greves' = c(j_greves_m1, j_greves_rera),
  'upper' = c(j_greves_m1 + qnorm(0.025, lower=F) * sqrt(err_greves_m1), j_greves_rera + qnorm(0.025, lower=F) * sqrt(err_greves_rera)),
  'lower' = c(j_greves_m1 - qnorm(0.025, lower=F) * sqrt(err_greves_m1), j_greves_rera - qnorm(0.025, lower=F) * sqrt(err_greves_rera)),
  'ferie'=c(ferie,ferie),
  'station'= c(rep('Metro 1', length(temps_tot_gen)), rep('RER A', length(temps_tot_gen))),
  'from' = as.Date("2019-12-23"),
  'to' = as.Date("2020-01-03")
) %>% filter(temps >= '2019-12-01' & temps < '2020-01-20') %>% mutate(wday=wday(temps)) %>%
  mutate(non_work = ifelse(wday %in% c(1,7) | ferie == 1, T,F)) %>%
  mutate(sundays = ifelse(temps %in% c(as.Date("2019-12-08"), as.Date("2019-12-15") ), "Yes","No"))

ggplot(zz, 
       aes(x=temps, y=greves, group = station)) + 
  annotate(geom = "rect", xmin =as.Date("2019-12-23"), xmax = as.Date("2020-01-03"),ymin=-Inf,ymax=Inf,
           fill = "#a6bddb", alpha = 0.4) +
  geom_line(size=1.2) + 
  geom_ribbon(aes(ymin=lower, ymax=upper, fill = station), alpha=0.7) +
  ylab(expression(beta[t]^{(scriptscriptstyle('Strike days') )}*X[t]^{(scriptscriptstyle('Strike days'))})) +
  geom_point(shape=20,size=5,aes( colour=non_work))  + theme_bw() +
  theme( legend.direction = "horizontal",
         legend.box = "vertical", strip.text.y = element_text(size=25,face='bold'),
         legend.justification = c(0, 1),
         legend.position="top",
         legend.title=element_text(size=30, face='bold'),
         axis.title=element_text(size=30,face="bold"), axis.text.y = element_text(size=30),axis.text.x = element_text(angle=60,size=25, vjust = 0.5), strip.text.x = element_text(size=30),
         legend.text = element_text(size = 25,face='bold'),legend.key.width=unit(4, "cm"),legend.key.height=unit(1, "cm")) + xlab('Date') +
  scale_x_date(labels = date_format("%d-%m"))  +
  scale_colour_brewer(name = "", labels = c("Working days", "Non working days"),palette = 'Set1') + 
  scale_fill_manual(values = c("RER A"="gray78","Metro 1"="gray78")) + 
  scale_shape_manual(values = c("Yes"=4,"No"=43)) + 
  scale_alpha_manual(values = c("RER A"=0,"Metro 1"=1)) +
  scale_size_manual(values = c("RER A"=0,"Metro 1"=6)) +
  facet_grid(factor(station,levels=c('RER A','Metro 1'))~.,scale='free')+
  guides(fill = FALSE, alpha=FALSE, size=FALSE,shape=FALSE) +
  geom_text(aes(alpha=station, size=station),
            label="Christmas holidays", 
            x=as.Date(c("2019-12-28")),
            y=-.5,
            color = "black"
  ) +
  geom_segment(aes(x=as.Date("2019-12-05"), xend=as.Date("2019-12-05"), y =Inf, yend=-Inf), size=1.1, colour='red2', linetype="dotdash",alpha=0.5) +
  geom_text(aes(alpha=station, size=station),
            label="Start of strike", 
            x=as.Date(c("2019-12-02")),
            y=.7,
            color = "red3"
  ) +
  geom_point(data=zz %>% filter(sundays=="Yes"),
             pch=21, fill=NA, size=9, colour="red", stroke=1) +
  guides(color = guide_legend(override.aes = list(size = 10)))












#plot confinement/deconfinement

zz = data.frame(
  'temps' = c(temps_tot_gen,temps_tot_gen),
  'confinement' = c(j_confinement_m1,j_confinement_rera),
  'upper' = c(j_confinement_m1 + qnorm(0.025, lower=F) * sqrt(err_confinement_m1), j_confinement_rera + qnorm(0.025, lower=F) * sqrt(err_confinement_rera)  ),
  'lower' = c(j_confinement_m1 - qnorm(0.025, lower=F) * sqrt(err_confinement_m1), j_confinement_rera - qnorm(0.025, lower=F) * sqrt(err_confinement_rera)),
  'ferie'=c(ferie,ferie),
  'station'= c(rep('Metro 1', length(temps_tot_gen)), rep('RER A', length(temps_tot_gen))),
  'from' = as.Date("2020-07-06"),
  'to' = as.Date("2020-07-31")
) %>% filter(temps > '2020-03-10') %>% mutate(wday=wday(temps)) %>% mutate(non_work = ifelse(wday %in% c(1,7) | ferie == 1, T,F)) %>% mutate(period = ifelse(temps >= as.Date('2020-05-11'), 'deconfinement','confinement')) %>%
  mutate(travaux = ifelse(temps %in% c(as.Date("2020-07-11"),as.Date("2020-07-12"),
                                       as.Date("2020-07-18"),as.Date("2020-07-19"),
                                       as.Date("2020-07-25"),as.Date("2020-07-26")
  ), 'travaux','non'))




ggplot(zz, aes(x=temps, y=confinement, group=station))+ 
  annotate(geom = "rect", xmin = as.Date("2020-07-06"), xmax = as.Date("2020-07-31"),ymin=-Inf,ymax=Inf,
           fill = "#a6bddb", alpha = 0.4) +
  geom_line(size=1.6) + geom_ribbon(aes(ymin=lower, ymax=upper,fill=station), alpha=0.7) + 
  geom_point(aes(x=temps,y=confinement,color=non_work, size=period, alpha = period),shape=20) +
  ylab(expression(sum(beta[t]^{(s )}*X[t]^{(s)}, s %in% {(scriptscriptstyle('Lockdown, Post-lockdown'))},))) + theme_bw() +
  theme( legend.direction = "horizontal",
         legend.position="top",
         legend.box = "vertical", strip.text.y = element_text(size=25,face='bold'),
         legend.justification = c(0, 1),
         legend.title=element_text(size=30, face='bold'),
         axis.title=element_text(size=30,face="bold"), axis.text.y = element_text(size=30),axis.text.x = element_text(angle=60,size=25, vjust = 0.5), strip.text.x = element_text(size=30),
         legend.text = element_text(size = 25,face='bold'),legend.key.width=unit(4, "cm"),legend.key.height=unit(1, "cm")) + xlab('Time') +
  scale_x_date(labels = date_format("%m-%Y"))  +
  scale_colour_brewer(name = "", labels = c("Working days", "Non working days"),palette = 'Set1') + 
  scale_size_manual(values=c(0,5)) +
  scale_alpha_manual(values=c(0,1)) +
  geom_segment(aes(x = as.Date(c("2020-05-11")), y = -1, xend = as.Date(c("2020-05-11")), yend = -2), arrow = arrow(length = unit(0.9, "cm")), size=2) + 
  geom_text(
    label="Start post-lockdown", 
    x=as.Date(c("2020-05-11")),
    y=-.5,
    color = "black", size=6
  ) +
  scale_fill_manual(values = c("RER A"="gray78","Metro 1"="gray78")) + facet_grid(factor(station,levels=c('RER A','Metro 1'))~.)+
  xlab('Date') +
  guides(fill = FALSE,size=FALSE,alpha=FALSE,colour = guide_legend(override.aes = list(size=5))) +
  geom_text(size = 6,
            label="Summer holidays", 
            x=as.Date(c("2020-07-18")),
            y=-3,
            color = "black"
  ) +
  geom_segment(aes(x = as.Date(c("2020-04-11")), y = -1.5, xend = as.Date(c("2020-03-19")), yend = -2.7), arrow = arrow(length = unit(0.9, "cm")), size=2) + 
  geom_text(
    label="Start lockdown", 
    x=as.Date(c("2020-04-11")),
    y=-1.2,
    color = "black", size=6
  ) +
  geom_point(data=zz %>% filter(travaux=="travaux"),
             pch=21, fill=NA, size=6, colour="darkblue", stroke=1)+
  guides(color = guide_legend(override.aes = list(size = 10)))




