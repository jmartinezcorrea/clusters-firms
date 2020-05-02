
# intro -------------------------------------------------------------------
source("scr/libraries.R")


# data cleaning and vars --------------------------------------------------
endei <- read.dta("data/raw/ENDEI_posta.dta")

## tipo de cambio
tc.2010 <- 3.9124  
tc.2011 <- 4.1297
tc.2012 <- 4.5508

## Merge con datos bds productividad
bds.prod <- read.dta("data/raw/bds-productividad.dta")
endei <- endei %>% 
  left_join(.,bds.prod, by =c("Ide"))

## Filtro de la base
endei <- endei %>% 
  filter(Ide != 540233 & Ide != 168813 & Ide !=373842 & Ide !=354583 & Ide !=799904 & Ide != 402831 & Ide != 275268 & Ide != 861530) %>%  #outliers segun ENDEI
  filter(p_4_1_01 != 'Ns/Nc', p_4_1_07 != 'Ns/Nc') %>%
  #filter(Perfil_inno == 'Hace AI') %>% #nos quedamos con los que hacen AI
  #filter(Tam_nue == 'Pequeña' | Tam_nue == 'Mediana') %>% # nos quedamos con pymes
  #filter(Rama_act != 'otras') %>% 
  #filter(Rama_act == 'Productos químicos' | Rama_act == 'Instrumentos médicos' | Rama_act == 'Farmaceuticas' | Rama_act == 'Productos de caucho y plástico' | Rama_act == 'Otros minerales no metálicos') %>%
  drop_na(Cant_NivTotal_2010:Cant_NivTotal_2012, Ingr_Corr_2010:Ingr_Corr_2012, Va_tr10:Va_tr12) #%>% #eliminamos obs con missing
#filter(Ingr_Corr_2010 < quantile(Ingr_Corr_2010, c(.95)), Ingr_Corr_2011 < quantile(Ingr_Corr_2011, c(.95)), Ingr_Corr_2012 < quantile(Ingr_Corr_2012, c(.95))) # filtramos pq ventas truncada (?)

## MOdificacion/Generacion de variables 
endei <- endei %>% 
  rename(empleo.2010 = Cant_NivTotal_2010) %>% 
  rename(empleo.2011 = Cant_NivTotal_2011) %>% 
  rename(empleo.2012 = Cant_NivTotal_2012) 


endei <- endei %>% 
  mutate(empleo.2010=ifelse(empleo.2010 == 400, Cant_Compus/Compus_trabajador, empleo.2010)) %>% 
  mutate(empleo.2011=ifelse(empleo.2011 == 400, Cant_Compus/Compus_trabajador, empleo.2011)) %>% 
  mutate(empleo.2012=ifelse(empleo.2012 == 400, Cant_Compus/Compus_trabajador, empleo.2012)) %>% 
  mutate_at(vars(Inno_Total_2010:Inno_Total_2012), funs(replace(., is.na(.), 0))) %>%  #los que no gastan quedan en 0
  mutate_at(vars(Prop_Inno_IDint_2010:Prop_Inno_IDint_2012,Prop_Inno_IDext_2010:Prop_Inno_IDext_2012 ), funs(replace(., is.na(.), 0))) %>%  
  mutate_at(vars(Prop_EduIng_2010,Prop_EduIng_2011,Prop_EduIng_2012), funs(replace(., is.na(.), 0))) 


endei <- endei %>% 
  mutate(inno.id.2010 = (Prop_Inno_IDint_2010 + Prop_Inno_IDext_2010) * Inno_Total_2010 / 100 / tc.2010, 
         inno.id.2011 = (Prop_Inno_IDint_2011 + Prop_Inno_IDext_2011) * Inno_Total_2011 / 100 / tc.2011,
         inno.id.2012 = (Prop_Inno_IDint_2012 + Prop_Inno_IDext_2012) * Inno_Total_2012 / 100 / tc.2012,
         dai        = case_when((p_3_1_1 == "Si" | p_3_1_2 == "Si" | p_3_1_3 == "Si" | p_3_1_4 == "Si" | p_3_1_5 == "Si" | p_3_1_6 == "Si" | p_3_1_7 == "Si" | p_3_1_8 == "Si") ~ 1,
                                TRUE ~ 0),
         did        = case_when((inno.id.2010 > 0 | inno.id.2011 > 0 | inno.id.2012 > 0) ~ 1,
                                TRUE ~ 0),
         exporta    = case_when((p_1_10_2 == "Sí" | p_1_10_3 == "Sí" | p_1_10_4 == "Sí" | p_1_10_5 == "Sí" | p_1_10_6 == "Sí" | p_1_10_7 == "Sí") ~ 1, #ojo: Ns/Nc queda en 0
                                TRUE ~ 0),
         exp.hincome = case_when((exporta == 1 & (p_1_10_4 == "Sí" | p_1_10_5 == "Sí")) ~ 1,
                                 (exporta == 1 & (p_1_10_4 != "Sí" | p_1_10_5 != "Sí")) ~ 0,
                                 exporta == 0 ~ NA_real_),
         innovo     = case_when(( p_4_1_01 == "Si" | p_4_1_07 == "Si") ~ 1,
                                TRUE ~ 0)) %>%
  mutate_at(vars(p_1_10_2:p_1_10_7),funs(case_when(. == "Sí"~ 1, TRUE ~ 0)))  %>%
  rowwise() %>%
  mutate(exp.dest = sum(c(p_1_10_2,p_1_10_3,p_1_10_4,p_1_10_5,p_1_10_6,p_1_10_7)))

# vinculaciones con firmas y con  spub (dummy y suma)
endei <- endei %>% 
  mutate_at(vars(starts_with("p_8_")),funs(as.numeric)) %>% # o matches instead of strarts_with; si hay mas variables "var1|var2"
  mutate_at(vars(p_8_1:p_8_8_f), funs(replace(., . ==2 | . ==3, 0))) %>% 
  mutate(dvinc.firmas = ifelse(p_8_1_b == 1 | p_8_2_b == 1 | p_8_3_b == 1 | p_8_4_b == 1 | p_8_5_b == 1 | p_8_6_b == 1 | p_8_7_b == 1 | p_8_8_b == 1,1,0),
         dvinc.pub    = ifelse(p_8_1_c == 1 | p_8_2_c == 1 | p_8_3_c == 1 | p_8_4_c == 1 | p_8_5_c == 1 | p_8_6_c == 1 | p_8_7_c == 1 | p_8_8_c == 1 | p_8_1_d == 1 | p_8_2_d == 1 | p_8_3_d == 1 | p_8_4_d == 1 | p_8_5_d == 1 | p_8_6_d == 1 | p_8_7_d == 1 | p_8_8_d == 1,1,0)) %>%
  rowwise() %>%
  mutate(svinc.firmas = sum(c(p_8_1_b, p_8_2_b, p_8_3_b, p_8_4_b, p_8_5_b, p_8_6_b, p_8_7_b, p_8_8_b)),
         svinc.pub    = sum(c(p_8_1_c, p_8_2_c, p_8_3_c, p_8_4_c, p_8_5_c, p_8_6_c, p_8_7_c, p_8_8_c, p_8_1_d, p_8_2_d,p_8_3_d, p_8_4_d, p_8_5_d, p_8_6_d, p_8_7_d, p_8_8_d,p_8_1_f, p_8_2_f, p_8_3_f, p_8_4_f, p_8_5_f, p_8_6_f, p_8_7_f, p_8_8_f)))

# dummies por tipo de informacion que tiene la empresa (ojo: pregunta solo disponible para las que gastan en AI)
endei <- endei %>% 
  mutate_at(vars(starts_with("p_5_")),funs(as.numeric)) %>%
  mutate(interna.info = case_when((p_5_1_1==1 | p_5_1_2==1 | p_5_1_3==1 | p_5_1_4==1) ~ 1,
                                  (p_5_1_1==2 | p_5_1_2==2 | p_5_1_3==2 | p_5_1_4==2) ~ 0,
                                  (p_5_1_1==4 | p_5_1_2==4 | p_5_1_3==4 | p_5_1_4==4) ~ NA_real_),
         mercado.info = case_when((p_5_2_1==1 | p_5_2_2==1 | p_5_2_3==1 | p_5_2_4==1 | p_5_2_8==1) ~ 1,
                                  (p_5_2_1==2 | p_5_2_2==2 | p_5_2_3==2 | p_5_2_4==2 | p_5_2_8==2) ~ 0,
                                  (p_5_2_1==4 | p_5_2_2==4 | p_5_2_3==4 | p_5_2_4==4 | p_5_2_8==4) ~ NA_real_),
         instituc.info = case_when((p_5_2_5==1 | p_5_2_6==1) ~ 1,
                                   (p_5_2_5==2 | p_5_2_6==2) ~ 0,
                                   (p_5_2_5==4 | p_5_2_6==4) ~ NA_real_),
         otras.info    = case_when((p_5_2_7==1 | p_5_2_9==1 | p_5_2_10==1) ~ 1,
                                   (p_5_2_7==2 | p_5_2_9==2 | p_5_2_10==2) ~ 0,
                                   (p_5_2_7==4 | p_5_2_9==4 | p_5_2_10==4) ~ NA_real_)) #creo que para NA se podría poner FALSE ~

# kapital intenacional y depto i+d formal
endei <- endei %>% 
  mutate_at(vars(p_1_7, p_3_3),funs(as.numeric)) %>%
  mutate(k.inac   = recode(p_1_7, `2` = 0),
         k.inac   = replace(k.inac, k.inac==3, NA_real_),
         depto.id = ifelse(p_3_3 ==1,1,0)) %>%
  drop_na(k.inac) 

# obtuvo y solicito financiamiento para innovacion
endei <- endei %>% 
  mutate_at(vars(starts_with("p_6_2_")),funs(as.numeric)) %>%
  filter(p_6_2_1 != 7, p_6_2_2 != 7)  %>%
  mutate(obtuvo.finac = case_when(( p_6_2_1 == 1 | p_6_2_2 == 1) ~ 1,
                                  TRUE ~ 0)) %>%
  mutate_at(vars(starts_with("p_6_2_")),funs(recode(.,`2` = 1, `3` = 1))) %>%
  #mutate_at(vars(starts_with("p_6_2_")),funs(replace(., .==7, NA))) %>%  
  mutate(solic.finac  = case_when((p_6_2_1 == 1 | p_6_2_2 == 1) ~ 1,
                                  TRUE ~ 0))

### Capacidad organizativa y estrategia empresarial
endei <- endei %>% 
  mutate(dnorm.calidad  = case_when((p_2_7_8 == "Si" | p_2_7_9 == "Si")  ~ 1,
                                    TRUE ~ 0),
         d.especific    = case_when((p_2_7_1 == "Si" | p_2_7_1 == "Si")  ~ 1,
                                    TRUE ~ 0),
         d.trazabilidad = case_when(p_2_7_3 == "Si"  ~ 1,
                                    TRUE ~ 0), 
         d.problemas    = case_when(p_2_7_4 == "Si"  ~ 1,
                                    TRUE ~ 0), 
         d.mejoracont   = case_when(p_2_7_5 == "Si"  ~ 1,
                                    TRUE ~ 0),
         d.diseno       = case_when(p_2_7_6 == "Si"  ~ 1,
                                    TRUE ~ 0),         
         d.gesproydis   = case_when(p_2_7_7 == "Si"  ~ 1,
                                    TRUE ~ 0),
         dorganiza     = case_when((p_2_7_1 == "Si" | p_2_7_2 == "Si" | p_2_7_3 == "Si" | p_2_7_4 == "Si" | p_2_7_5 == "Si"  | p_2_7_6 == "Si" | p_2_7_7 == "Si") ~ 1,
                                   TRUE ~ 0)) %>% 
  mutate_at(vars(starts_with("p_2_7_")),funs(case_when(. == "Si"~ 1, TRUE ~ 0)))  %>%
  rowwise() %>%
  mutate(scap.organiza = sum(c(p_2_7_1,p_2_7_2,p_2_7_3,p_2_7_4,p_2_7_5,p_2_7_6,p_2_7_7)))

## Politica de gestión del empleo (planes de carrera, incentivos, etc.)
# - p.9.2/3/4(1):“perfiles formalizados”, planes de carrera, sistema de evaluación de desempeño.
# d.incent: Aplica la firma algún sistema de evaluación de desempeño
# NOTA: capaz se puede dividir en dos: incentivos para gerentes e incentivos para el resto del personal.

endei <- endei %>% 
  mutate(d.incent  = case_when((p_9_4_a == "Si" | p_9_4_b == "Si" | p_9_4_c == "Si")  ~ 1,
                               TRUE ~ 0))


## Capacidad generación/gestión del conocimiento (VER SI PONERLA)
# p_11_1_3: Se fomenta el desarrollo de reuniones de trabajo para analizar y proponer nuevas formas de hacer las cosas.
# p_11_1_4: Se utilizan herramientas informáticas para generar conocimientos en forma colaborativa (wikis, groupware, redes sociales, foros, etc.).

## Politica de capacitacion 
# scap.func: cantidad de aspectos atendidos por el area responsable de organizar actividades de capacitacion
# dcap.func: si hay algun area responsable de organizar actividades de capacitacion (captado por si se atiende algun aspecto)
# dcap.cursos: si atendio alguna temática en los cursos(capta si hizo cursos/ojo: antes está pregunta de % de personal capacitado)
# scap.cursos: cantidad de cursos realizados a los empleados                                             

endei <- endei %>% 
  mutate(dcap.func     = case_when((p_9_5_1 == "Si" | p_9_5_2 == "Si" | p_9_5_3 == "Si" | p_9_5_4 == "Si" | p_9_5_5 == "Si"  | p_9_5_6 == "Si" | p_9_5_7 == "Si") ~ 1,
                                   TRUE ~ 0),
         dcap.cursos   = case_when((p_9_9_1 == "Si" | p_9_9_2 == "Si" | p_9_9_3 == "Si" | p_9_9_4 == "Si" | p_9_9_5 == "Si"  | p_9_9_6 == "Si" | p_9_9_7 == "Si" |  p_9_9_8 == "Si" |  p_9_9_9 == "Si"| p_9_9_12 == "Si" ) ~ 1, 
                                   TRUE ~ 0)) %>% 
  mutate_at(vars(starts_with("p_9_5_"),starts_with("p_9_9_")),funs(case_when(. == "Si"~ 1, TRUE ~ 0)))  %>%
  rowwise() %>%
  mutate(scap.func     = sum(c(p_9_5_1,p_9_5_2,p_9_5_3,p_9_5_4,p_9_5_5,p_9_5_6,p_9_5_7)),
         scap.cursos   = sum(c(p_9_9_1,p_9_9_2,p_9_9_3,p_9_9_4,p_9_9_5,p_9_9_6,p_9_9_7,p_9_9_8,p_9_9_12)))



## personal capacitado a nivel jerárquico, supervisores y nivel no-jerárquico (en %)
endei <- endei %>% 
  mutate(capacit.ger   = p_9_8_1_porcentaje,
         capacit.sup   = p_9_8_2_porcentaje,
         capacit.nojer = p_9_8_3_porcentaje) %>%
  rowwise() %>% 
  mutate(capacit.jer  = mean(c(capacit.ger, capacit.sup)))

## Grado de autonomia del personal (respuesta frente a problemas - vble ordinal)
# 0: no resolver
# 1: llamar al supervisor
# 2: resolver y comunicar
# 3: resolver y documentar

endei <- endei %>%
  filter(p_10_2_1!= 'Ns/Nc', p_10_2_2 != 'Ns/Nc', p_10_2_2 != 'Ns/Nc', p_10_2_4 != 'Ns/Nc') %>%
  mutate(autonom.personal = case_when(p_10_2_1 == "Si" ~ 0,
                                      p_10_2_2 == "Si" ~ 1,
                                      p_10_2_3 == "Si" ~ 2,
                                      p_10_2_4 == "Si" ~ 3))

# Participación del personal (practicas de trabajo)
endei <- endei %>%
  filter(p_10_3_1 != 'Ns/Nc', p_10_3_2 != 'Ns/Nc', p_10_3_3 != 'Ns/Nc') %>%
  mutate_at(vars(starts_with("p_10_3_")),funs(case_when(. =="Si"~ 1, TRUE ~ 0)))  %>%
  rowwise() %>%
  mutate(part.personal = sum(c(p_10_3_1,p_10_3_2,p_10_3_3)))

# rotacion personal (planificada)
endei <- endei %>%
  filter(p_10_1_a != 'Ns/Nc', p_10_1_b != 'Ns/Nc', p_10_1_c != 'Ns/Nc') %>%
  mutate_at(vars(starts_with("p_10_1_")),funs(case_when(. =="Implementa, planificada"~ 1, TRUE ~ 0)))  %>%
  rowwise() %>%
  mutate(d.rotacion = max(c(p_10_1_a,p_10_1_b,p_10_1_c)))

# sectores de actividad
endei <- endei %>% 
  mutate(calif.ocde = factor(case_when((Rama_act=='Farmaceuticas' | Rama_act=='Productos químicos' | Rama_act=='Material eléctrico, radio, televisión' | Rama_act=='Instrumentos médicos' | Rama_act=='Otros equipo de transporte')  ~ 'Alta \nTecnología',
                                       (Rama_act=='Carrocerías,  remolques y semirremolques' | Rama_act=='Autopartes' | Rama_act=='Maquina herramienta en general' | Rama_act=='Maquinaria y equipo' | Rama_act=='Maquinaria Agropecuaria y Forestal')  ~ 'Media-Alta \nTecnología',
                                       (Rama_act=='Otros minerales no metálicos' | Rama_act=='Otros productos de metal' | Rama_act=='Metales comunes' | Rama_act=='Productos de caucho y plástico' | Rama_act=='Aparatos de uso doméstico')  ~ 'Media-Baja \nTecnología',
                                       (Rama_act=='otras' | Rama_act=='Madera' | Rama_act=='Papel' | Rama_act=='Alimentos' | Rama_act=='Edición' | Rama_act=='Frigoríficos' | Rama_act=='Productos lácteos' | Rama_act=='Vinos y otras bebidas fermentadas' | Rama_act=='Productos textiles' | Rama_act=='Confecciones' | Rama_act=='Cuero' | Rama_act=='Muebles')  ~ 'Baja \nTecnología'),
                             levels =c('Baja \nTecnología',
                                       'Media-Baja \nTecnología',
                                       'Media-Alta \nTecnología',
                                       'Alta \nTecnología')),
         rama.num   = factor(as.numeric(Rama_act)))

# Pasamos a USD
endei <- endei %>% 
  mutate(ing.2010      = Ingr_Corr_2010 / tc.2010,
         ing.2011      = Ingr_Corr_2011 / tc.2011,
         ing.2012      = Ingr_Corr_2012 / tc.2012,
         va.tr.2010    = Va_tr10 / tc.2010,
         va.tr.2011    = Va_tr11 / tc.2011,
         va.tr.2012    = Va_tr12 / tc.2012,
         inno.tot.2010 = Inno_Total_2010 / tc.2010,
         inno.tot.2011 = Inno_Total_2011 / tc.2011,
         inno.tot.2012 = Inno_Total_2012 / tc.2012,
         k.2010        = k2010 / tc.2010,
         k.2011        = k2011 / tc.2011,
         k.2012        = k2012 / tc.2012)

# ratios actividades de innovacion sobre ventas
endei <- endei %>% 
  mutate(inno.ventas.2010 = (inno.tot.2010 / ing.2010)*100,
         inno.ventas.2011 = (inno.tot.2011 / ing.2011)*100,
         inno.ventas.2012 = (inno.tot.2012 / ing.2012)*100,
         id.ventas.2010   = (inno.id.2010 / ing.2010)*100,
         id.ventas.2011   = (inno.id.2011 / ing.2011)*100,
         id.ventas.2012   = (inno.id.2012 / ing.2012)*100) 

# promedios 2010-2012
endei <- endei %>% 
  rowwise() %>% 
  mutate(ing         = mean(c(ing.2010, ing.2011, ing.2012)),
         empleo      = mean(c(empleo.2010, empleo.2011, empleo.2012)),
         inno.tot    = mean(c(inno.tot.2010, inno.tot.2011, inno.tot.2012)),
         inno.id     = mean(c(inno.id.2010, inno.id.2011, inno.id.2012)),
         va.tr       = mean(c(va.tr.2010, va.tr.2011, va.tr.2012)),
         inno.ventas = mean(c(inno.ventas.2010, inno.ventas.2011, inno.ventas.2012)),
         id.ventas   = mean(c(id.ventas.2010, id.ventas.2011, id.ventas.2012)),
         prop.prof   = mean(c(Prop_CalProf_2010, Prop_CalProf_2011, Prop_CalProf_2012)),
         prop.tec    = mean(c(Prop_CalTec_2010,Prop_CalTec_2011,Prop_CalTec_2012)),
         prop.ing    = mean(c(Prop_EduIng_2010,Prop_EduIng_2011,Prop_EduIng_2012)),  # prop en el total de profesionales
         k           = mean(c(k.2010, k.2011, k.2012)),
         wage.jer    = mean(c(Rem_NivGeren_2010, Rem_NivGeren_2011,Rem_NivGeren_2012)),
         wage.medio  = mean(c(Rem_NivJefe_2010, Rem_NivJefe_2011,Rem_NivJefe_2012)),
         wage.nojer  = mean(c(Rem_NivNoJerar_2010, Rem_NivNoJerar_2011,Rem_NivNoJerar_2012))) %>% 
  ungroup() %>% 
  mutate(wage.ratio.jer=log(1+wage.jer)-log(1+wage.nojer),
         wage.ratio.medio=log(1+wage.medio)-log(1+wage.nojer))

# TC 2010-2012
# promedios 2010-2012
endei <- endei %>% 
  mutate(tc.va.tr  = ((va.tr.2012-va.tr.2010)/va.tr.2010)*100,
         tc.empleo = ((empleo.2012-empleo.2010)/empleo.2010)*100,
         tc.ventas = ((ing.2012-ing.2010)/ing.2010)*100)


saveRDS(endei, "data/working/endei.rds")


