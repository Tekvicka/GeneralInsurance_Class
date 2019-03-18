library(dplyr)
library(ggplot2)
library(ChainLadder)

# nacitanie dat
dt_PaidCase <- read.csv("./Data/lesson4_PaidCase.csv")

# pri kazdom pripade je vyuzity hint na preorganizovanie stlpcov

### House & Small ####
Paid_HH_sml <- dt_PaidCase %>% filter(Business == "House" & ClaimSize == "Small" & dataset_type == "PAID" ) 
# ak neuvazujeme len dataset_type == "PAID" , ale zosumovane PAID a CASE:
# Paid_HH_sml  <- dt_PaidCase %>% filter(Business == "House" & ClaimSize == "Small") 
head(Paid_HH_sml)
dTest <- as.triangle(Paid_HH_sml, origin = "ay", dev = "dy", "SumOfamount")
Paid_HH_sml_triangle <- t(triangle( dTest[,10], dTest[,9], dTest[,1], dTest[,2], dTest[,3], dTest[,4], dTest[,5], dTest[,6], dTest[,7], dTest[,8] ))
Paid_HH_sml_triangle
plot(Paid_HH_sml_triangle)
plot(predict(chainladder(Paid_HH_sml_triangle))) 
# z grafu pozorujeme kratke chvosty
ata(Paid_HH_sml_triangle)


### House & Large ####
Paid_HH_lrg <- dt_PaidCase %>% filter(Business == "House" & ClaimSize == "Large" & dataset_type == "PAID" ) 
# ak neuvazujeme len dataset_type == "PAID", ale zosumovane PAID a CASE :
# Paid_HH_lrg <- dt_PaidCase %>% filter(Business == "House" & ClaimSize == "Large") 
head(Paid_HH_lrg)
dTest <- as.triangle(Paid_HH_lrg, origin = "ay", dev = "dy", "SumOfamount")
Paid_HH_lrg_triangle <- t(triangle( dTest[,10], dTest[,9], dTest[,1], dTest[,2], dTest[,3], dTest[,4], dTest[,5], dTest[,6], dTest[,7], dTest[,8] ))
Paid_HH_lrg_triangle
plot(Paid_HH_lrg_triangle)
plot(predict(chainladder(Paid_HH_lrg_triangle))) 
# z grafu pozorujeme kratke chvosty
ata(Paid_HH_lrg_triangle)

# z grafov vidime ze pri House sa straty vyrovnaju po cca 3 az 4 rokoch
# medyi Small a Large nepozorujeme vyrazny roydiel v grafoch


### 3rd Party & Small ####
Paid_3rd_sml <- dt_PaidCase %>% filter(Business == "3rd Party" & ClaimSize == "Small" & dataset_type == "PAID" ) 
# ak neuvazujeme len dataset_type == "PAID" :
# Paid_3rd_sml <- dt_PaidCase %>% filter(Business == "3rd Party" & ClaimSize == "Small")
head(Paid_3rd_sml )
dTest <- as.triangle(Paid_3rd_sml , origin = "ay", dev = "dy", "SumOfamount")
Paid_3rd_sml_triangle <- t(triangle( dTest[,10], dTest[,9], dTest[,1], dTest[,2], dTest[,3], dTest[,4], dTest[,5], dTest[,6], dTest[,7], dTest[,8] ))
Paid_3rd_sml_triangle
plot(Paid_3rd_sml_triangle)
plot(predict(chainladder(Paid_3rd_sml_triangle)))
# z grafu pozorujeme dlhe chvosty
ata(Paid_3rd_sml_triangle)


### 3rd Party & Large ####
Paid_3rd_lrg <- dt_PaidCase %>% filter(Business == "3rd Party" & ClaimSize == "Large" & dataset_type == "PAID" )
# ak neuvazujeme len dataset_type == "PAID" , ale zosumovane PAID a CASE:
# Paid_3rd_lrg <- dt_PaidCase %>% filter(Business == "3rd Party" & ClaimSize == "Large")
head(Paid_3rd_lrg)
dTest <- as.triangle(Paid_3rd_lrg, origin = "ay", dev = "dy", "SumOfamount")
Paid_3rd_lrg_triangle <- t(triangle( dTest[,10], dTest[,9], dTest[,1], dTest[,2], dTest[,3], dTest[,4], dTest[,5], dTest[,6], dTest[,7], dTest[,8] ))
Paid_3rd_lrg_triangle 
plot(Paid_3rd_lrg_triangle)
plot(predict(chainladder(Paid_3rd_lrg_triangle)))
# z grafu pozorujeme dlhe chvosty? je to otazne kedze okrem jednej krivky to vyzera na kratke 
ata(Paid_3rd_lrg_triangle )

# z grafov vidime ze pri 3rd party trva ovela dlhsiu dobu kym sa strata vyrovna, v porovnani s House
# pri porovnani Small a Large pozorujeme jednu nezvycajnost pri Large, kedze 0-ova ciara sa vyrazne rozchadza s ostatnymi

# odlisnosti v House a 3rd Party su kvoli tomu ze ide o dve velmi odlisne druhy poistenia, nakolko ked sa stane nehoda pri autach tak je velmi pravdepodobne, ze aj po dlhsom case budu nahlasene skody, ci uz take, ktore neboli hned zistene pri nehode alebo napr zdravotne tazkosti, ktore mozu nastat neskor - - suvislost s dlhymi chvostmi 
# Narozdiel od domacnosti kedy su skody zrejme po dlhsej dobe nahlasovane nebudu - suvislost s kratkymi chvostmi 

