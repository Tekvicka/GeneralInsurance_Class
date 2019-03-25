library(dplyr)
dt_Policy <- read.csv("C:/Users/Zuzana Jankechová/Documents/GeneralInsurance_Class/Data/lesson5_PolicyHistory.csv") %>% distinct(NrPolicy, NrObject, .keep_all = TRUE) 
dt_Claims <- read.csv("C:/Users/Zuzana Jankechová/Documents/GeneralInsurance_Class/Data/lesson5_Claims.csv") %>% distinct(NrClaim, .keep_all = TRUE)

dt_pol_w_claims <- left_join(dt_Policy, 
                             dt_Claims, 
                             by = c("NrPolicy", "NrObject")
)
head(dt_pol_w_claims)
# na analyzu som si zvolila parametre Veh_type1 a D_age_banded
# Veh_type1 = teda druh vozidla (pouzitie?) moze mat potencialny vplyv, kedze urcuje aj to na co je vozislo pouzivane
# D_age_banded - teda vekova kategoria majitela moze mat vyznamny vplyv, kedze s nou suvisia napr skusenosti vodica
library(lubridate)
dt_pol_w_claims <- dt_pol_w_claims %>% 
  mutate(Time_Exposure = lubridate::dmy(Dt_Exp_End) - lubridate::dmy(Dt_Exp_Start),
         Ult_Loss = Paid + Reserves,
         Burning_Cost = ifelse(is.na(Ult_Loss), 0,  Ult_Loss / as.integer(Time_Exposure)))


library(ggplot2)
######### druh vozidla 1 #############
dt_pol_w_claims %>% 
  ggplot(aes(y = Burning_Cost, x = Veh_type1)) + 
  geom_jitter()
# z grafu vidime ze najvacsie skody maju sukromne vozidla (private car) a potom commercial cars 
# je to asi preto, ze tieto druhy su najrozsirenejsie (najviac sa vyuzivaju)
# preto aj predpokladame ze tieto druhy budu mat najvyssi vplyv 

dt_pol_w_claims %>% 
  group_by(Veh_type1) %>% 
  summarise(BC_avg = mean(Burning_Cost, na.rm  = TRUE),
            BC_median = median(Burning_Cost, na.rm = TRUE),
            cnt = n()) %>% 
  arrange(desc(BC_avg))
# vidime ze median je nula, co znamena, ze vacsina poistenych nemala ziadne skody

dt_pol_w_claims %>% 
  ggplot(aes(y = Burning_Cost, x = Veh_type1)) + 
  geom_boxplot() +
  ylim(0, 100)
# z grafu vidime ze je tam vela outlierov

# GML model s parametrom Veh_type1
GLMmodel1 <- glm(data = dt_pol_w_claims %>% filter(Burning_Cost != 0, Burning_Cost < 100),
                 formula = Burning_Cost ~ Veh_type1,
                 family = Gamma())
summary(GLMmodel1)
# tusenie sa potvrdilo a vidime ze najviac signifikankne su prave private car a commercial cars 

######## vekove kategorie ##########
dt_pol_w_claims %>% 
  ggplot(aes(y = Burning_Cost, x = D_age_banded)) + 
  geom_jitter()
# vidime ze najvyssi Burning cost maju tri kategorie a to konkretne: <40,45> , <45,50> , <50,55>
# moze to byt sposobene pravdepodobne tym ze klienti su vo veku kedy auta poziciavaju svojim detom a tie nie su este take skusene
# zaroven vidime aj outlier ktory ma Burning Cost v hodnote > 2000 - obrovska skoda pravdepodobne

dt_pol_w_claims %>% 
  filter(Burning_Cost != 0) %>% 
  group_by(D_age_banded) %>% 
  summarise(BC_avg = mean(Burning_Cost, na.rm  = TRUE),
            BC_median = median(Burning_Cost, na.rm = TRUE),
            cnt = n()) %>% 
  arrange(desc(BC_avg))
# vidime ze naozaj najviac skod sposobila vekova kategoria <40,45> 
# zaujimavostou je najvyssi priemer BC u kategorie <85,90> pricom pocet skor je len 2, takze asi islo o obrovske skody (napr umrtie?)

dt_pol_w_claims[, 8] <- as.factor(dt_pol_w_claims[, 8])
dt_pol_w_claims %>% 
  ggplot(aes(y = Burning_Cost, x = D_age_banded)) + 
  geom_boxplot() +
  ylim(0, 100)
# znova z grafu vidime ze je tam vela outlierov

# GML model s parametrom D_age_banded
GLMmodel2 <- glm(data = dt_pol_w_claims %>% filter(Burning_Cost != 0, Burning_Cost < 100),
                 formula = Burning_Cost ~ D_age_banded,
                 family = Gamma())
summary(GLMmodel2)
# vidime ze tento paramater nie je pre modelovanie signifikantny

#  GML model s parametrami Veh_type1 a D_age_banded
GLMmodel3 <- glm(data = dt_pol_w_claims %>% filter(Burning_Cost != 0, Burning_Cost < 100),
                formula = Burning_Cost ~ Veh_type1 + D_age_banded,
                family = Gamma())
summary(GLMmodel3)
# vidime ze prave parameter Veh_type1 signifikantny je narozdiel od parametra D_age_banded
# rovnako to vyslo aj pri jednotlivych modeloch GLMmodel1 a GMLmodel2
# preto tento model nehodnotim ani najvhodnejsi pri predikovani
# urcite by bolo vyhodnejsie spravit model zaoberajuci sa viacerymi parametrami respektive vyzskusat viacere modely a nasledne ich porovnavat

