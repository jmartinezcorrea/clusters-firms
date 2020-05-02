
# intro -------------------------------------------------------------------
source("scr/libraries.R")

# validation with outputs -------------------------------------------------

df.val = readRDS(file="data/working/df.val.rds")
df.val = readRDS(file="data/working/df.val.rds")

## tables
t.size <- df.val %>% 
  group_by(K.cluster, Tam_nue) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n)) %>%
  select(-n) %>%
  spread(., key=K.cluster, value=freq)

write.csv(t.size, "res/tabla-size.csv")

t.age <- df.val %>% 
  group_by(K.cluster, joven) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n)) %>%
  select(-n) %>%
  spread(., key=K.cluster, value=freq)
write.csv(t.age, "res/tabla-age.csv")

t.rama <- df.val %>% 
  group_by(K.cluster, Rama_act) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))%>%
  select(-n) %>%
  spread(., key=K.cluster, value=freq)
write.csv(t.rama, "res/tabla-rama.csv")

t.kinac <- df.val %>% 
  group_by(K.cluster, k.inac) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))%>%
  select(-n) %>%
  spread(., key=K.cluster, value=freq)
write.csv(t.kinac, "res/tabla-kinac.csv")

t.innovativa <- df.val %>% 
  group_by(K.cluster) %>%
  summarize(mean_dai      = mean(dai),
            mean_did      = mean(did),
            mean_aiventas = mean(inno.ventas),
            mean_idventas = mean(id.ventas), 
            mean_innovo   = mean(innovo))
write.csv(t(t.innovativa), "res/tabla-innovativa.csv")


t.ineq <- df.val %>%
  mutate_at(vars(wage.jer,wage.medio, wage.nojer), funs(replace(.,.==0, NA_real_))) %>%
  rowwise() %>% 
  mutate(wage.jefes=mean(c(wage.jer,wage.medio), na.rm=TRUE)) %>% 
  ungroup() %>% 
  mutate(wage.jefes=replace(wage.jefes,is.na(wage.jefes) & (!is.na(wage.nojer) | !is.na(wage.jer)  | !is.na(wage.medio)) , 0),
         wage.ratio.jefes=log(1+wage.jefes)-log(1+wage.nojer)) %>% 
  mutate_at(vars(wage.ratio.jer, wage.ratio.medio, wage.ratio.jefes), funs(replace(.,.<0, 0))) %>% 
  group_by(K.cluster) %>%
  summarize(mean_ratiojer = mean(wage.ratio.jer, na.rm= TRUE),
            mean_ratiomed = mean(wage.ratio.medio, na.rm=TRUE),
            mean_ratiojef = mean(wage.ratio.jefes, na.rm = TRUE)) 
write.csv(t(t.ineq), "res/tabla-ineq.csv")

t.perf <- df.val %>%
  group_by(K.cluster) %>%
  summarize(mean_emp      = mean(empleo),
            mean_exphi    = mean(exp.hincome, na.rm =TRUE),
            mean_expdest  = mean(exp.dest[exporta==1]))
write.csv(t(t.perf), "res/tabla-perf.csv")

## tablas/grafs por rama
# Inno/ventas
t.ramaprod <- df.val %>% 
  group_by(K.cluster, Rama_act) %>%
  summarize(mean_aiventas = mean(inno.ventas)) %>%
  mutate(laiventas = log(mean_aiventas)) %>%
  select(-mean_aiventas) %>%
  spread(., key=K.cluster, value=laiventas) %>%
  mutate(diff = `2` - `1`)

t.ramaprod$Rama_act <- reorder(t.ramaprod$Rama_act,-t.ramaprod$diff)

ggplot(data=t.ramaprod, aes(x=Rama_act, y=diff, group=1)) +
  geom_line()+
  geom_point()+
  xlab("Rama de actividad") + ylab("Diferencia (puntos log)") + 
  ggtitle("Diferencia porcentual AI/ventas entre grupos") +     
  theme_bw() +
  theme(axis.text.x = element_text(size=rel(0.8),angle = 60, hjust = 1))

ggplot(data=t.ramaprod, aes(x=Rama_act, y=diff, fill=Rama_act)) +
  geom_bar(colour="black", stat="identity", width = 0.05) +
  guides(fill=FALSE) +
  coord_flip()


# Innovo
t.ramaprod <- df.val %>% 
  group_by(K.cluster, Rama_act) %>%
  summarize(mean_innovo = mean(innovo)) %>%
  mutate(linnovo = log(mean_innovo)) %>%
  select(-mean_innovo) %>%
  spread(., key=K.cluster, value=linnovo) %>%
  mutate(diff = `2` - `1`)

t.ramaprod$Rama_act <- reorder(t.ramaprod$Rama_act,t.ramaprod$diff)

ggplot(data=t.ramaprod, aes(x=Rama_act, y=diff, fill=Rama_act)) +
  geom_bar(colour="black", stat="identity", width = 0.05) +
  guides(fill=FALSE) +
  coord_flip()

# Empleo
t.ramaprod <- df.val %>% 
  group_by(K.cluster, Rama_act) %>%
  summarize(mean_empleo = mean(empleo)) %>%
  mutate(lempleo = log(mean_empleo)) %>%
  select(-mean_empleo) %>%
  spread(., key=K.cluster, value=lempleo) %>%
  mutate(diff = `2` - `1`)

t.ramaprod$Rama_act <- reorder(t.ramaprod$Rama_act,t.ramaprod$diff)

ggplot(data=t.ramaprod, aes(x=Rama_act, y=diff, fill=Rama_act)) +
  geom_bar(colour="black", stat="identity", width = 0.05) +
  guides(fill=FALSE) +
  coord_flip()

# Prod laboral
t.ramaprod <- df.val %>% 
  group_by(K.cluster, Rama_act) %>%
  summarize(mean_vatr = mean(va.tr)) %>%
  mutate(lmean_vatr = log(mean_vatr)) %>%
  select(-mean_vatr) %>%
  spread(., key=K.cluster, value=lmean_vatr) %>%
  mutate(diff = `2` - `1`)

t.ramaprod$Rama_act <- reorder(t.ramaprod$Rama_act,t.ramaprod$diff)

ggplot(data=t.ramaprod, aes(x=Rama_act, y=diff, fill=Rama_act)) +
  geom_bar(colour="black", stat="identity", width = 0.05) +
  guides(fill=FALSE) +
  coord_flip()

#test medias entre grupos
t<-t.test(va.tr ~ K.cluster, data = df.val)


### Regresion (control por tamaño y sector)
df.val <- df.val %>% 
  mutate_at(vars(Rama_act,Tam_nue,K.cluster), funs(as.factor(.))) %>% 
  mutate_at(vars(empleo,va.tr),funs(l = log(.)))  %>% 
  mutate_at(vars(wage.jer,wage.medio, wage.nojer), funs(replace(.,.==0, NA_real_))) %>%
  rowwise() %>% 
  mutate(wage.jefes=mean(c(wage.jer,wage.medio), na.rm=TRUE)) %>% 
  ungroup() %>% 
  mutate(wage.jefes=replace(wage.jefes,is.na(wage.jefes) & (!is.na(wage.nojer) | !is.na(wage.jer)  | !is.na(wage.medio)) , 0),
         wage.ratio.jefes=log(1+wage.jefes)-log(1+wage.nojer)) %>% 
  mutate_at(vars(wage.ratio.jer, wage.ratio.medio, wage.ratio.jefes), funs(replace(.,.<0, 0))) 

library(sandwich) #para errores estandar robustos
library(lmtest)

"%+%" = function(a,b) paste(a,b,sep="")
modelo = function(rpta) {
  f = as.formula(rpta%+%"~ K.cluster + Tam_nue + Rama_act")
  lm(f, data=df.val)
}

robust = function(mod){
  cov <- vcovHC(mod, type = "HC1")  # robust; HC1 (Stata default)
  sqrt(diag(cov))
}
modelos1 <- tibble(vars = c("dai","did","inno.ventas","id.ventas","innovo"),
                   models= map(vars, modelo),
                   rse=map(models,robust))

modelos2 <- tibble(vars = c("empleo_l", "va.tr_l", "exporta", "exp.dest", "exp.hincome"),
                   models= map(vars, modelo),
                   rse=map(models,robust))

# ineq measures
modelos3 <- tibble(vars = c("wage.ratio.jer","wage.ratio.medio","wage.ratio.jefes"),
                   models= map(vars, modelo),
                   rse=map(models,robust))

# dai
m.did = lm(did ~ K.cluster + Tam_nue + Rama_act, data=df.val)
summary(m.dai)

coeftest(m.did, vcov = vcovHC(m.did, "HC1"))    # robust; HC1 (Stata default)
cov <- vcovHC(m.dai, type = "HC1")
robust.se1 <- sqrt(diag(cov))

## tablas modelos
library(stargazer)
stargazer(modelos1$models,
          title="Medias condicionales de los clusters. Indicadores innovación",
          keep = c("K.cluster2"), type = "latex",
          se=modelos1$rse,
          omit.stat = c("f","rsq", "ser"),
          dep.var.caption = "Variable dependiente:",
          dep.var.labels = c("Gasta en AI", "Gasta en I+D", "AI/ventas (%)", "I+D/ventas (%)", "Innovó"),
          covariate.labels=c("cluster - altas"), 
          df=FALSE, digits=3,
          out = "res/modelos1.html")

stargazer(modelos2$models,
          title="Medias condicionales de los clusters. Desempeño", 
          keep = c("K.cluster2"), type = "html",
          omit.stat = c("f","rsq", "ser"),
          se=modelos2$rse,
          align = TRUE,
          dep.var.caption = "Variable dependiente:",
          dep.var.labels = c("Empleo (en logs)", "Valor agregado (en logs)", "Exporta", "# destinos", "Exporta (altos ingresos)"),
          covariate.labels=c("cluster - altas"), 
          df=FALSE, digits=3,
          out = "res/modelos2.html")

stargazer(modelos3$models,
          title="Medias condicionales de los clusters. Desigualdad salarial.", 
          keep = c("K.cluster2"), type = "html",
          omit.stat = c("f","rsq", "ser"),
          se=modelos3$rse,
          align = TRUE,
          dep.var.caption = "Variable dependiente:",
          dep.var.labels = c("Ratio salario jerárquico-no jerárquico", "Ratio salario medios-no jerárquico", "Ratio salario jefes-no jerárquico"),
          covariate.labels=c("cluster - altas"), 
          df=FALSE, digits=3,
          out = "res/modelos3.html")



# relative importance of variables ----------------------------------------
library(randomForest)
df <- readRDS("data/working/df.rf.rds")

# The default random forest performs 500 trees and p/3 randomly selected 
# predictor variables at each split.  

set.seed(123)
clustering.rf <- randomForest(factor(K.cluster) ~ ., data=df, ntree=1000, keep.forest=FALSE,
                              importance=TRUE)

plot(clustering.rf)
# number of trees with lowest MSE
which.min(clustering.rf$mse)

varImp(clustering.rf)
# 2= mean decrease in node impurity (for classification, the node impurity is measured by the Gini index)
varImpPlot(clustering.rf,type=2, main="Importancia de las variables",
           labels = c("asd"))


dat.gph <- as.data.frame(cbind(rownames(clustering.rf$importance),round(clustering.rf$importance[,"MeanDecreaseGini"],0)))
rownames(dat.gph) = NULL
colnames(dat.gph) <- c("variables","MeanDecreaseGini")

dat.gph$MeanDecreaseGini <- as.numeric(as.character(dat.gph$MeanDecreaseGini))

g <- dat.gph %>%  mutate(variables=fct_reorder(variables, MeanDecreaseGini)) %>%
  ggplot() + geom_point(aes(MeanDecreaseGini,variables)) +
  labs(y="", x="Variable Importance (Mean Decrease in Gini Index)") +
  theme_bw() +
  scale_y_discrete(labels=c("Employee rotation", "Links with other firms", "Specifications",
                            "Extent of personnel participation","Traceability","Technicians %","Professionals %", "R&D formal department",
                            "Teams for solving problems","Project management and design", "Links with the public sector",
                            "Staff incentives","Training activities area","% of trained personnel at non-hierarchical level", 
                            "Standards","% of trained personnel at hierarchical level","Continuous improvement tools"))

ggsave(filename="res/importance-rf.png",g)

# explicacion gini index = https://www.r-bloggers.com/variable-importance-plot-and-variable-selection/


# variable selection ------------------------------------------------------
#### Model-based clustering (diferentes paquetes)

library(clustvarsel) #continuous variable
set.seed(1234)
out1 <- clustvarsel(df.new, G = 2, direction = c("forward"))
summary(out1)
out1$subset

library(SelvarMix) #continuous variable
obj <- SelvarClustLasso(x=df.new, nbcluster=1:5, nbcores=4)

## NOTA: ver paquetes ClustMMDD y LCAvarsel para categorical data.
## http://varsellcm.r-forge.r-project.org/ #mixed data

library(VarSelLCM)
factores <- c('depto.id','dvinc.firmas','dvinc.pub','dnorm.calidad','d.especific','d.gesproydis','d.mejoracont',
              'd.problemas','d.trazabilidad','d.incent','d.rotacion','dcap.func','part.personal')
df.orig[factores] <- lapply(df.orig[factores], factor)
integers <- c('part.personal')
df.orig <- df.orig %>% mutate_at(vars(integers), funs(as.integer(.)))
df.orig <- as.data.frame(df.orig)

# Cluster analysis without variable selection
res_without <- VarSelCluster(df.orig, gvals = 1:3, vbleSelec = FALSE, crit.varsel = "BIC")

# Cluster analysis with variable selection (with parallelisation)
res_with <- VarSelCluster(df.orig, gvals = 1:3, vbleSelec = TRUE, nbcores = 4, crit.varsel = "BIC")

BIC(res_without)
BIC(res_with)

# Summary of the best model
summary(res_with)

# Comparsion con true clusters
ztrue <- K$cluster 
ARI(ztrue, fitted(res_with)) #0.47
#adjustedRandIndex(x, y) 

# Estimated probabilities of classification
head(fitted(res_with, type="probability"))

#Discriminative power of the variables . The greater this index, the more the variable distinguishes the clusters.
plot(res_with)

#Distribution of the most discriminative variable per clusters
plot(x=res_with, y="capacit.nojer")

# Empirical and theoretical distributions (to check that the distribution is well-fitted)
plot(res_with, y="capacit.nojer", type="cdf")

#Distribution of a categorical variable per clusters/Summary of categorical variable
plot(res_with, y="Sex")

VarSelShiny(res_with)



# fuzzy clustering (pending) ----------------------------------------------

# https://cran.r-project.org/web/packages/ppclust/vignettes/fcm.html
