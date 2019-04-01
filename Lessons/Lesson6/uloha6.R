library(dplyr)
dt_pol_w_claims <- readRDS("C:/Users/Zuzana Jankechová/Documents/GeneralInsurance_Class/Data/lesson6_dt_pol_w_claims.rds")

set.seed(58742)
ind <- sample(2, nrow(dt_pol_w_claims), replace=TRUE, prob=c(0.80, 0.20)) # generate random indicator to split by
dt_pol_w_claims <- mutate(dt_pol_w_claims,
                          data_status = ifelse(ind == 1, 
                                               "Training",
                                               ifelse(ind == 2, 
                                                      "Validation", 
                                                      "Unseen")
                          )
)
train <- dt_pol_w_claims %>% filter(data_status == "Training")
val <- dt_pol_w_claims %>% filter(data_status == "Validation")

mse <- function(prediction, actual){
  return(sum((prediction-actual)^2, na.rm = TRUE)/length(prediction))
}

# moj model na train datach
GLMmodel1 <- glm(data = train,
                 formula = Burning_Cost ~ Veh_type1 + D_age_banded,
                 family = Gamma())
summary(GLMmodel1)
mse(predict(GLMmodel1, train, type = "response"), train$Burning_Cost)
# mse(predict(GLMmodel1, val, type = "response"), val$Burning_Cost)
# mse pre povodny model vychadza 192.786


# do modelu pridame premenne Construct_year a Customer_Type
# ocakavame ze by to malo zlepsit model, kedze bude zaleyat na viacerych relevantnych premennych
GLMmodel2 <- glm(data = train,
                formula = Burning_Cost ~ Veh_type1 + D_age_banded + Construct_year + Customer_Type,
                family = Gamma())
summary(GLMmodel2)
mse(predict(GLMmodel2, train, type = "response"), train$Burning_Cost)
# mse(predict(GLMmodel2, val, type = "response"), val$Burning_Cost)
# mse takehoto modelu je 190.5132, teda sa znizilo oproti povodnemu modelu, co hodnotime pozitivne - model sa zlepsil



source("C:/Users/Zuzana Jankechová/Documents/GeneralInsurance_Class/Lessons/Lesson6/Support/emb_chart.R")
emblem_graph(
  dt.frm = train %>% cbind(data.frame(pred = predict(GLMmodel2, train, type = "response"))),
  x_var =  "Construct_year",
  target = "Burning_Cost",
  prediction =  "pred"
)

# skusime dat vsetky 'stare' vozidla do jednej kategorie
# chceme zistit ci 'outliery' vyrazne oplyvnuju statistiku
train <- train %>% mutate(Construct_year = ifelse(Construct_year <= 2007, 2007, Construct_year))

GLMmodel3<- glm(data = train,
              formula = Burning_Cost ~ Veh_type1 + D_age_banded + Construct_year + Customer_Type,
              family = Gamma())
summary(GLMmodel3)
mse(predict(GLMmodel3, train, type = "response"), train$Burning_Cost)
# mse(predict(GLMmodel3, val, type = "response"), val$Burning_Cost)
# mse je teraz na hodnote 190.8007, co voci povodnemu modelu predstavuje zlepsenie, no voci GLMmodel2 mierny narast a teda zhorsenie
# z toho usudyujeme ze pri nasom modeli nam capping moc nepomoze


# skusime teraz odstranit premennu Customer_Type
# chceme zistit, ci vyznamne ovplyvnuje statistiku
GLMmodel4 <- glm(data = train,
                 formula = Burning_Cost ~ Veh_type1 + D_age_banded + Construct_year,
                 family = Gamma())
summary(GLMmodel4)
mse(predict(GLMmodel4, train, type = "response"), train$Burning_Cost)
# mse(predict(GLMmodel4, val, type = "response"), val$Burning_Cost)
# vidime ze teraz mame dokonca najmensie mse = 190.3154
# z toho usudzujeme, ze premenna Customer_Type nas model vobec nevzlepsuje a preto je v nom zbytocna (prinasa len zbytocny noise)


# ako najlepsi model nam vysiel posledny, no samozrejme by bolo lepsie sa este viac pohrat s roynzmi kombinaciami premennych,
# mozno aj skusit niektore premenne zgrupit 