
# intro -------------------------------------------------------------------
source("scr/libraries.R")


# data for clustering and descriptive stats -------------------------------
endei = readRDS(file="data/working/endei.rds")

# seleccion de variables
var.oup <- c('Ide','Tam_nue','Rama_act','calif.ocde','exporta', 'exp.hincome', 'exp.dest','innovo', 'k.inac','ing',
             'empleo','va.tr', 'joven', 'solic.finac', 'inno.ventas', 'id.ventas', 'dai','did', 'inno.tot', 'inno.id',
             'tc.va.tr', 'tc.empleo','tc.ventas','wage.jer', 'wage.medio', 'wage.nojer', 'wage.ratio.jer', 'wage.ratio.medio')


var.inp <- c('Ide','dvinc.firmas','dvinc.pub', 'svinc.firmas', 'svinc.pub', 'depto.id','dcap.func','dorganiza','dcap.cursos', 'dnorm.calidad', 'd.especific', 
             'd.gesproydis','d.diseno','d.mejoracont','d.problemas','d.trazabilidad','d.incent', 'prop.prof', 'prop.tec', 'prop.ing', 'scap.func', 'scap.organiza', 'scap.cursos',
             'solic.finac', 'd.rotacion', 'capacit.jer', 'capacit.sup', 'capacit.nojer', 'part.personal', 'autonom.personal')

df.inp <-  endei %>% 
  select(var.inp)
df.new <- df.inp %>%
  select('Ide','depto.id','prop.prof', 'prop.tec', 'dvinc.firmas','dvinc.pub','dcap.func','dnorm.calidad','d.rotacion', 'capacit.jer', 'capacit.nojer', 'capacit.nojer', 'part.personal','d.especific', 
         'd.gesproydis','d.mejoracont','d.problemas','d.trazabilidad','d.incent')  %>% #'scap.cursos',
  #select(-dvinc.firmas, -dvinc.pub, -dcapacit, -inno.ventas, -id.ventas) %>% 
  drop_na() 


ide <- df.new$Ide

df.new <- df.new %>%
  select(-Ide)

df.oup <- endei %>%
  select(var.oup) %>%
  filter(Ide %in% ide)

### Orden de variables
df.new <- df.new[c("d.especific", "d.trazabilidad", "d.problemas","d.mejoracont","d.gesproydis","dnorm.calidad","d.rotacion","part.personal",
"depto.id","prop.prof","prop.tec","dcap.func","capacit.jer","capacit.nojer","dvinc.firmas", "dvinc.pub","d.incent")]

### Estadisticas descrpitvas (pre-norm) 
desc <- do.call(data.frame, 
               list(mean = apply(df.new, 2, mean),
                    sd = apply(df.new, 2, sd),
                    median = apply(df.new, 2, median),
                    min = apply(df.new, 2, min),
                    max = apply(df.new, 2, max),
                    n = apply(df.new, 2, length)))

write.csv(desc, "res/Tabla-estad-desc.csv")
#stargazer(as.data.frame(df.new), digits = 3) #, summary.stat = c("n", "p75", "sd")


## Normalize the attributes between 0 and 1 
normalize <- function(x){
  return ((x-min(x))/(max(x)-min(x)))
}

df.orig <- df.new
df.new <- df.new %>% 
  mutate_all(funs(normalize)) 


### Matriz de correlaciones
cor=cor(df.new) #ver como poner cramer coefficient for binary variables
colnames(cor)=c("V1","V2","V3","V4","V5","V6","V7","V8","V9","V10","V11","V12","V13","V14","V15","V16","V17")
rownames(cor)=c("V1","V2","V3","V4","V5","V6","V7","V8","V9","V10","V11","V12","V13","V14","V15","V16","V17")

round(cor,2)

write.csv(cor, paste0(resultados.dir,"tabla-matcor.csv"))
stargazer(cor, title="Matriz de correlacion",digits=2)

dev.off()
corrplot(cor,type = "upper", order = "original", 
         tl.col = "black", tl.srt = 45, mar = c(1, 0, 1, 0))   
corrplot.mixed(cor,upper="circle")

## Cramer coefficent correlation (for binary variables)

df <- df.new %>%
  select(depto.id,dvinc.firmas, dvinc.pub, dnorm.calidad, d.rotacion)

# Initialize empty matrix to store coefficients
empty_m <- matrix(ncol = length(df),
                  nrow = length(df),
                  dimnames = list(names(df), 
                                  names(df)))

##Function that accepts matrix for coefficients and data and returns a correlation matrix (obs: creo que me da el mismo coef q Pearson...?)
calculate_cramer <- function(m, df) {
  for (r in seq(nrow(m))){
    for (c in seq(ncol(m))){
      m[[r, c]] <- assocstats(table(df[[r]], df[[c]]))$cramer
    }
  }
  return(m)
}

cor_matrix <- calculate_cramer(empty_m ,df)

corrplot(cor_matrix)


# save data ---------------------------------------------------------------

saveRDS(df.new, "data/working/df.new.rds")
saveRDS(df.inp, "data/working/df.inp.rds")
saveRDS(df.oup, "data/working/df.oup.rds")
saveRDS(df.orig, "data/working/df.orig.rds")

