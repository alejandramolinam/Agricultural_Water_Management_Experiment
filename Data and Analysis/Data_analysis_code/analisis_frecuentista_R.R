# Optional generic preliminaries:
graphics.off() # This closes all of R's graphics windows.
rm(list=ls())  # Careful! This clears all of R's memory!
library(stargazer)
library(plm)
#------------------------------------------------------------------------------- 
DF = read.csv( file="Datos_tesis_rondas.csv" )

# INCUMPLIMIENTO EN HORAS
mh0 <- lm(yhoras ~ tto ,data=DF)
summary(mh0)

mh1 <- lm(yhoras ~ Exo + Exo_acu_tot + End_acu_tot + tto ,data=DF)
summary(mh1)
mh2 <- lm(yhoras ~ Exo + Exo_acu_tot + End_acu_tot + dif_beliefs_mas +
                   risk_av + multa_ronda_anterior + tto ,data=DF)
summary(mh2)

stargazer(m0)
stargazer(m1)
# INCUMPLIMIENTO BINARIO
mb0 <- glm(ybinaria ~ tto ,data=DF, family ="binomial")
summary(mb0)

mb1 <- lm(ybinaria ~ Exo + Exo_ant + Exo_acu + End_ant + End_acu  ,data=DF)
summary(mb1)

mb2 <- glm(ybinaria ~ Exo + Exo_ant + Exo_acu + End_ant + End_acu + genero + 
             escolaridad + risk_av + multa_ronda_anterior ,data=DF,family ="binomial")
summary(mb1)


# INCUMPLIMIENTO PORCENTUAL
mp0 <- lm(yporcentual ~ tto ,data=DF)
summary(mp0)

mp1 <- lm(yhoras ~ Exo + Exo_ant + Exo_acu + End_ant + End_acu,data=DF)


mp2 <- lm(yporcentual ~ Exo + Exo_ant + Exo_acu + End_ant + End_acu + genero + edad +
            escolaridad + risk_av + multa_ronda_anterior,data=DF)
summary(mp1)

stargazer(mh0,mb0,mp0)
stargazer(mh1,mb1,mp1)

m2 <- lm(yhoras[tto==0] ~ Exo[tto==0] + Exo_ant[tto==0] + Exo_acu[tto==0] + End_ant[tto==0] + End_acu[tto==0] ,data=DF)
summary(m2)

m3 <- lm(yhoras[tto==1] ~ Exo[tto==1] + 
                          Exo_ant[tto==1] + 
                          Exo_acu[tto==1] + 
                          End_ant[tto==1] + 
                          End_acu[tto==1] + 
                          demanda_vecino[tto==1] +
                          multa_ronda_anterior[tto==1],data=DF)
summary(m3)

# Y BINARIA EN GRUPO CONTROL
glm4 <- glm(ybinaria[tto==0] ~ Exo[tto==0] + 
              Exo_ant[tto==0] + 
              Exo_acu[tto==0] + 
              End_ant[tto==0] + 
              End_acu[tto==0] + 
              multa_ronda_anterior[tto==0],data=DF,family="binomial")
summary(glm4)


# Y BINARIA EN GRUPO TTO
glm3 <- glm(ybinaria[tto==1] ~ Exo[tto==1] + 
           Exo_ant[tto==1] + 
           Exo_acu[tto==1] + 
           End_ant[tto==1] + 
           End_acu[tto==1] + 
           demanda_vecino[tto==1] +
           multa_ronda_anterior[tto==1],data=DF,family="binomial")
summary(glm3)

# EFECTOS FIJOS

ph1 <- plm(yhoras ~ Exo + Exo_acu_tot + End_acu_tot + tto ,data=DF,effect="twoways",index = c("player"))
summary(ph1)
pb1 <- plm(ybinaria ~ Exo + Exo_ant + Exo_acu + End_ant + End_acu + tto ,data=DF,effect="twoways",index = c("ronda"))
summary(pb1)
pp1 <- plm(yporcentual ~ Exo + Exo_ant + Exo_acu + End_ant + End_acu + tto ,data=DF,effect="twoways",index = c("ronda"))
summary(pp1)
stargazer(ph1,pb1,pp1)

dm1 <- plm(yhoras ~ Exo*tto + Exo_acu + End_ant*tto ,data=DF,effect="twoways",index = c("ronda"))
summary(dm1)

#dm1 <- lm(yhoras ~ Exo*tto + Exo_ant*tto  + Exo_acu*tto  + End_ant*tto  + End_acu*tto ,data=DF)
#summary(dm1)



# Solo casos con escasez:
m0 <- lm(yhoras[Exo>0] ~ tto[Exo>0] ,data=DF)
summary(m0)
m1 <- lm(yhoras[Exo>0] ~ Exo[Exo>0] + Exo_ant[Exo>0] + Exo_acu[Exo>0] + End_ant[Exo>0] + End_acu[Exo>0],data=DF)
summary(m1)


m2 <- lm(y[Exo>0 & Tto==0] ~ Exo[Exo>0 & Tto==0] + Exo_ant[Exo>0 & Tto==0] + 
           Exo_acu[Exo>0 & Tto==0] + End_ant[Exo>0 & Tto==0] + End_acu[Exo>0 & Tto==0],data=DF)
summary(m2)

m3 <- lm(y[Exo>0 & Tto==1] ~ Exo[Exo>0 & Tto==1] + Exo_ant[Exo>0 & Tto==1] + 
           Exo_acu[Exo>0 & Tto==1] + End_ant[Exo>0 & Tto==1] + End_acu[Exo>0 & Tto==1],data=DF)
summary(m3)


dm1 <- lm(y[Exo>0] ~ Exo[Exo>0]*Tto[Exo>0] + Exo_ant[Exo>0]*Tto[Exo>0] + Exo_acu[Exo>0]*Tto[Exo>0] + End_ant[Exo>0]*Tto[Exo>0] + End_acu[Exo>0]*Tto[Exo>0],data=DF)
summary(dm1)


