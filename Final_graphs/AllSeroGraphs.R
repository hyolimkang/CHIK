# compiling graphs
# library
library(readxl)
# 1. read data ------------------------------------------------------------------
load("SSAOutDf.RData") # load data
load("plotFunc.RData") # load plot function
load("quantmat_haiti.RData") # load plot function
load("quantmat_italy.RData") # load plot function
# read out study data
CountryModel <- read_excel("~/Library/CloudStorage/OneDrive-LondonSchoolofHygieneandTropicalMedicine/CHIK/1.Aim1/all_countries/CountryModel.xlsx", 
                           sheet = "inclusion", col_types = c("text", 
                                                              "text", "numeric", "numeric", "numeric", 
                                                              "numeric", "text", "text", "text", 
                                                              "text", "text", "text", "text", "text", 
                                                              "text", "text", "text", "text"))
df_ssa = CountryModel %>% filter(region == "SSA")

df_ssa[,c("midpoint","lower","upper")] = binom.confint(df_ssa$N.pos, df_ssa$N, method="exact")[,c("mean","lower","upper")]

df_ssa <-  df_ssa %>% 
  dplyr::mutate(agemid = (age_min+age_max)/2)

df_ssa <- df_ssa[df_ssa$antibody != "IgM",] # remove data that has only IgM

study7       <-  df_ssa %>% filter(study_no == 7)
study7       <-  study7[1:3,]
study17      <-  df_ssa %>% filter(study_no == 17)
study106     <-  df_ssa %>% filter(study_no == 106)
study132     <-  df_ssa %>% filter(study_no == 132)
study136     <-  df_ssa %>% filter(study_no == 136)
study149     <-  df_ssa %>% filter(study_no == 149)
study170     <-  df_ssa %>% filter(study_no == 170)
study204_1   <-  df_ssa %>% filter(study_no == 204 & country == "Burkina Faso (Ouagadougou)")
study204_2   <-  df_ssa %>% filter(study_no == 204 & country == "Gabon (Lambaréné)")
study236     <-  df_ssa %>% filter(study_no == 236)
study237     <-  df_ssa %>% filter(study_no == 237)
study279     <-  df_ssa %>% filter(study_no == 279)
study306     <-  df_ssa %>% filter(study_no == 306)
study321     <-  df_ssa %>% filter(study_no == 321)
study494     <-  df_ssa %>% filter(study_no == 494)

df_eap = CountryModel %>% filter(region == "EAP")

df_eap[,c("midpoint","lower","upper")] = binom.confint(df_eap$N.pos, df_eap$N, method="exact")[,c("mean","lower","upper")]

df_eap <-  df_eap %>% 
  dplyr::mutate(agemid = (age_min+age_max)/2)

df_eap <- df_eap[df_eap$antibody != "IgM",] # remove data that has only IgM

study75      <-  df_eap %>% filter(study_no == 75)
study146     <-  df_eap %>% filter(study_no == 146)
study277     <-  df_eap %>% filter(study_no == 277)
study281     <-  df_eap %>% filter(study_no == 281)
study341     <-  df_eap %>% filter(study_no == 341)
study423     <-  df_eap %>% filter(study_no == 423)
study452     <-  df_eap %>% filter(study_no == 452)
study505_1   <-  df_eap %>% filter(study_no == 505 & year == "2013")
study505_2   <-  df_eap %>% filter(study_no == 505 & year == "2015")
study234     <-  df_eap %>% filter(study_no == 234)

df_lac = CountryModel %>% filter(region == "LAC")

df_lac[,c("midpoint","lower","upper")] = binom.confint(df_lac$N.pos, df_lac$N, method="exact")[,c("mean","lower","upper")]

df_lac <-  df_lac %>% 
  dplyr::mutate(agemid = (age_min+age_max)/2)

df_lac <- df_lac[df_lac$antibody != "IgM",] # remove data that has only IgM

study52        <-  df_lac %>% filter(study_no == 52)
study124       <-  df_lac %>% filter(study_no == 124)
study153       <-  df_lac %>% filter(study_no == 153)
study165       <-  df_lac %>% filter(study_no == 165)
study174       <-  df_lac %>% filter(study_no == 174)
study179       <-  df_lac %>% filter(study_no == 179)
study229       <-  df_lac %>% filter(study_no == 229)
study243       <-  df_lac %>% filter(study_no == 243)
study251       <-  df_lac %>% filter(study_no == 251)
study260       <-  df_lac %>% filter(study_no == 260)
study299       <-  df_lac %>% filter(study_no == 299)
study345       <-  df_lac %>% filter(study_no == 345)
study396       <-  df_lac %>% filter(study_no == 396)
study406       <-  df_lac %>% filter(study_no == 406)
study503_1     <-  df_lac %>% filter(study_no == 503 & country == "Martinique")
study503_2     <-  df_lac %>% filter(study_no == 503 & country == "Guadalupe")
study507       <-  df_lac %>% filter(study_no == 507)

df_sa = CountryModel %>% filter(region == "SA")

df_sa[,c("midpoint","lower","upper")] = binom.confint(df_sa$N.pos, df_sa$N, method="exact")[,c("mean","lower","upper")]

df_sa <-  df_sa %>% 
  dplyr::mutate(agemid = (age_min+age_max)/2)

df_sa <- df_sa[df_sa$antibody != "IgM",] # remove data that has only IgM

study197       <-  df_sa %>% filter(study_no == 197)
study249       <-  df_sa %>% filter(study_no == 249)
study486       <-  df_sa %>% filter(study_no == 486)
study450_1     <-  df_sa %>% filter(study_no == 450 & year == "2009") 
study450_2     <-  df_sa %>% filter(study_no == 450 & year == "2019")

study249_1     <-  df_sa %>% filter(study_no == "249_1")
study249_2     <-  df_sa %>% filter(study_no == "249_2")
study249_3     <-  df_sa %>% filter(study_no == "249_3")
study249_4     <-  df_sa %>% filter(study_no == "249_4")
study249_5     <-  df_sa %>% filter(study_no == "249_5")
study249_6     <-  df_sa %>% filter(study_no == "249_6")
study249_7     <-  df_sa %>% filter(study_no == "249_7")
study249_8     <-  df_sa %>% filter(study_no == "249_8")
study249_9     <-  df_sa %>% filter(study_no == "249_9")
study249_10     <-  df_sa %>% filter(study_no == "249_10")
study249_11     <-  df_sa %>% filter(study_no == "249_11")
study249_12     <-  df_sa %>% filter(study_no == "249_12")
study249_13     <-  df_sa %>% filter(study_no == "249_13")
study249_14     <-  df_sa %>% filter(study_no == "249_14")
study249_15     <-  df_sa %>% filter(study_no == "249_15")

df_eca = CountryModel %>% filter(region == "ECA")

df_eca[,c("midpoint","lower","upper")] = binom.confint(df_eca$N.pos, df_eca$N, method="exact")[,c("mean","lower","upper")]

df_eca <-  df_eca %>% 
  dplyr::mutate(agemid = (age_min+age_max)/2)

study318  <-  df_eca %>% filter(study_no == 318)

df_me = CountryModel %>% filter(region == "ME")

df_me[,c("midpoint","lower","upper")] = binom.confint(df_me$N.pos, df_me$N, method="exact")[,c("mean","lower","upper")]

df_me <-  df_me %>% 
  dplyr::mutate(agemid = (age_min+age_max)/2)

df_me <- df_me %>% filter(!antibody %in% c("IgM"))

study32  <-  df_me %>% filter(study_no == 32)
study154  <-  df_me %>% filter(study_no == 154)
study213  <-  df_me %>% filter(study_no == 213)



# 1. SSA -------------------------------------------------------------------------------
df_upperLower7 <- quantmat(outDf7)
g1<-plot7 <- plotfuncYonly(df_upperLower7, study7)

df_upperLower17 <- quantmat(outDf17)
g2<-plot17 <- plotfuncNone(df_upperLower17, study17)

df_upperLower106 <- quantmat(outDf106)
g3<-plot106 <- plotfuncNone(df_upperLower106, study106)

df_upperLower149 <- quantmat(outDf149)
g4<-plot149 <- plotfuncNone(df_upperLower149, study149)

df_upperLower170 <- quantmat(outDf170)
g5<- plot170 <- plotfuncNone(df_upperLower170, study170)

df_upperLower204_1 <- quantmat(outDf204_1)
g6<-plot204_1 <- plotfuncYonly(df_upperLower204_1, study204_1)

df_upperLower204_2 <- quantmat(outDf204_2)
g7<-plot204_2 <- plotfuncNone(df_upperLower204_2, study204_2)

df_upperLower236 <- quantmat(outDf236)
g8<-plot236 <- plotfuncNone(df_upperLower236, study236)

df_upperLower237 <- quantmat(outDf237)
g9<-plot237 <- plotfuncNone(df_upperLower237, study237)

df_upperLower279 <- quantmat(outDf279)
g10<-plot279 <- plotfuncNone(df_upperLower279, study279)

df_upperLower306 <- quantmat(outDf306)
g11 <-plot306 <- plotfuncYonly(df_upperLower306, study306)

df_upperLower494 <- quantmat(outDf494)
g12<-plot494 <- plotfuncNone(df_upperLower494, study494)

save(g1, g2, g3, g4,
   g5, g6, g7, g8,
   g9, g10, g11, g12, file = "SSAplotMerge.RData")

#2. LAC-------------------------------------------------------------------------
load("LACOutDf.RData")

##results computation
df_upperLower345 <- quantmat(outDf345)
g13<- plot345 <- plotfuncNone(df_upperLower345, study345)

df_upperLower52 <- quantmat(outDf52)
g14<- plot52 <- plotfuncNone(df_upperLower52, study52)

df_upperLower165 <- quantmat(outDf165)
g15<- plot165 <- plotfuncNone(df_upperLower165, study165)

df_upperLower229 <- quantmat(outDf229)
g16<- plot229 <- plotfuncYonly(df_upperLower229, study229)

df_upperLower243 <- quantmat(outDf243)
g17<- plot243 <- plotfuncNone(df_upperLower243, study243)

df_upperLower396 <- quantmat(outDf396)
g18<- plot396 <- plotfuncNone(df_upperLower396, study396)

df_upperLower406 <- quantmat(outDf406)
g19<- plot406 <- plotfuncNone(df_upperLower406, study406)

df_upperLower124 <- quantmat(outDf124)
g20<- plot124 <- plotfuncNone(df_upperLower124, study124)

df_upperLower153 <- quantmat_haiti(outDf153)
g21<- plot153 <- plotfuncYonly(df_upperLower153, study153)

df_upperLower251 <- quantmat(outDf251)
g22<- plot251 <- plotfuncNone(df_upperLower251, study251)

df_upperLower174 <- quantmat(outDf174)
g23<- plot174 <- plotfuncNone(df_upperLower174, study174)

df_upperLower179 <- quantmat(outDf179)
g24<- plot179 <- plotfuncNone(df_upperLower179, study179)

df_upperLower260 <- quantmat(outDf260)
g25<- plot260 <- plotfuncNone(df_upperLower260, study260)

df_upperLower299 <- quantmat(outDf299)
g26<- plot299 <- plotfuncYonly(df_upperLower299, study299)

df_upperLower503 <- quantmat(outDf503)
g27<- plot503 <- plotfuncNone(df_upperLower503, study503_1)

df_upperLower507 <- quantmat(outDf507)
g28<- plot507 <- plotfuncNone(df_upperLower507, study507)

#3. EAP-------------------------------------------------------------------------
load("EAPOutDf.RData")
df_upperLower75 <- quantmat(outDf75)
g29<- plot75 <- plotfuncNone(df_upperLower75, study75)

df_upperLower341 <- quantmat(outDf341)
g30<- plot314 <- plotfuncNone(df_upperLower341, study341)

df_upperLower281 <- quantmat(outDf281)
g31<- plot281 <- plotfuncXY(df_upperLower281, study281)

df_upperLower452 <- quantmat(outDf452)
g32<- plot452 <- plotfuncXonly(df_upperLower452, study452)

df_upperLower505_1 <- quantmat(outDf505_1)
g33<- plot505_1 <- plotfuncXonly(df_upperLower505_1, study505_1)

df_upperLower505_2 <- quantmat(outDf505_2)
g34<- plot505_2 <- plotfuncXonly(df_upperLower505_2, study505_2)

df_upperLower277 <- quantmat(outDf277)
g35<- plot277 <- plotfuncXonly(df_upperLower277, study277)


## Final graph
all_sero1 <-   grid.arrange(g1, g2, g3, g4,
                           g5, g6, g7, g8,
                           g9, g10, g11, g12,
                           g13, g14, g15, g16,
                           g17, g18, g19, g20,
                           g21, g22, g23, g24,
                           g25, g26, g27, g28, 
                           g29, g30, g31, g32, 
                           g33, g34, g35, ncol=5,
                           left = "Proportion Seropositive",
                           bottom = "Age (years)")
ggsave("all_sero1.pdf", all_sero1, dpi=1000, device= "pdf", height=9, width=12,units="in", bg=NULL)

save(g1, g2, g3, g4,
     g5, g6, g7, g8,
     g9, g10, g11, g12,
     g13, g14, g15, g16,
     g17, g18, g19, g20,
     g21, g22, g23, g24,
     g25, g26, g27, g28, g29,
     g30, g31, g32, g33, g34, g35, file = "allsero1.RData")

#4. Allsero2 : EAP + ME/ECA + SA ----------------------------------------------------------
df_upperLower146 <- quantmat(outDf146)
g36<- plot146 <- plotfuncYonly(df_upperLower146, study146)

df_upperLower423 <- quantmat(outDf423)
g37<- plot423 <- plotfuncNone(df_upperLower423, study423)

df_upperLower234 <- quantmat(outDf234)
g38<- plot234 <- plotfuncNone(df_upperLower234, study234)

load("ECAMEOutDf.RData")

df_upperLower318 <- quantmat_italy(outDf318)
g39<- plot318 <- plotfuncNone(df_upperLower318, study318)

df_upperLower213 <- quantmat(outDf213)
g40<- plot213 <- plotfuncNone(df_upperLower213, study213)

df_upperLower154 <- quantmat(outDf154)
g41<- plot154 <- plotfuncYonly(df_upperLower154, study154)

df_upperLower32 <- quantmat(outDf32)
g42<- plot32 <- plotfuncNone(df_upperLower32, study32)

load("SAOutDf.RData")

df_upperLower197 <- quantmat(outDf197)
g43<- plot197 <- plotfuncNone(df_upperLower197, study197)

df_upperLower450_1 <- quantmat(outDf450_1)
g44<- plot450_1 <- plotfuncNone(df_upperLower450_1, study450_1)

df_upperLower450_2 <- quantmat(outDf450_2)
g45<- plot450_2 <- plotfuncNone(df_upperLower450_2, study450_2)

df_upperLower486 <- quantmat(outDf486)
g46<- plot486 <- plotfuncYonly(df_upperLower486, study486)

df_upperLower249_1 <- quantmat(outDf249_1)
g47 <- plotfuncNone(df_upperLower249_1, study249_1)

df_upperLower249_2 <- quantmat(outDf249_2)
g48  <- plotfuncNone(df_upperLower249_2, study249_2)

df_upperLower249_3 <- quantmat(outDf249_3)
g49 <- plotfuncNone(df_upperLower249_3, study249_3)

df_upperLower249_4 <- quantmat(outDf249_4)
g50 <- plotfuncNone(df_upperLower249_4, study249_4)

df_upperLower249_5 <- quantmat(outDf249_5)
g51 <- plotfuncYonly(df_upperLower249_5, study249_5)

df_upperLower249_6 <- quantmat(outDf249_6)
g52  <- plotfuncNone(df_upperLower249_6, study249_6)

df_upperLower249_7 <- quantmat(outDf249_7)
g53  <- plotfuncNone(df_upperLower249_7, study249_7)

df_upperLower249_8 <- quantmat(outDf249_8)
g54  <- plotfuncNone(df_upperLower249_8, study249_8)

df_upperLower249_9 <- quantmat(outDf249_9)
g55  <- plotfuncNone(df_upperLower249_9, study249_9)

df_upperLower249_10 <- quantmat(outDf249_10)
g56  <- plotfuncYonly(df_upperLower249_10, study249_10)

df_upperLower249_11 <- quantmat(outDf249_11)
g57  <- plotfuncXonly(df_upperLower249_11, study249_11)

df_upperLower249_12 <- quantmat(outDf249_12)
g58  <- plotfuncXonly(df_upperLower249_12, study249_12)

df_upperLower249_13 <- quantmat(outDf249_13)
g59  <- plotfuncXonly(df_upperLower249_13, study249_13)

df_upperLower249_14 <- quantmat(outDf249_14)
g60  <- plotfuncXonly(df_upperLower249_14, study249_14)

df_upperLower249_15 <- quantmat(outDf249_15)
g61  <- plotfuncXY(df_upperLower249_15, study249_15)

all_sero2 <-   grid.arrange(g36, g37, g38, g39,
                            g40, g41, g42, g43,
                            g44, g45, g46, g47,
                            g48, g49, g50, g51,
                            g52, g53, g54, g55,
                            g56, g57, g58, g59,
                            g60, g61, ncol=5,
                            left = "Proportion Seropositive",
                            bottom = "Age (years)")
ggsave("all_sero2.pdf", all_sero2, dpi=1000, device= "pdf", height=8, width=11,units="in", bg=NULL)

save(g36, g37, g38, g39,
     g40, g41, g42, g43,
     g44, g45, g46, g47,
     g48, g49, g50, g51,
     g52, g53, g54, g55,
     g56, g57, g58, g59,
     g60, g61, file = "allsero2.RData")

# extra graphs for vimc conf (some selected graphs)-----------------------------
df_upperLower106 <- quantmat(outDf106)
g3<-plot106 <- plotfuncYonly(df_upperLower106, study106)

df_upperLower170 <- quantmat(outDf170)
g5<- plot170 <- plotfuncNone(df_upperLower170, study170)

df_upperLower306 <- quantmat(outDf306)
g11 <-plot306 <- plotfuncNone(df_upperLower306, study306)

df_upperLower243 <- quantmat(outDf243)
g17<- plot243 <- plotfuncYonly(df_upperLower243, study243)

df_upperLower406 <- quantmat(outDf406)
g19<- plot406 <- plotfuncNone(df_upperLower406, study406)

df_upperLower75 <- quantmat(outDf75)
g29<- plot75 <- plotfuncNone(df_upperLower75, study75)

df_upperLower505_2 <- quantmat(outDf505_2)
g34<- plot505_2 <- plotfuncYonly(df_upperLower505_2, study505_2)

df_upperLower146 <- quantmat(outDf146)
g36<- plot146 <- plotfuncNone(df_upperLower146, study146)

df_upperLower318 <- quantmat_italy(outDf318)
g39<- plot318 <- plotfuncNone(df_upperLower318, study318)

df_upperLower154 <- quantmat(outDf154)
g41<- plot154 <- plotfuncXY(df_upperLower154, study154)

df_upperLower450_1 <- quantmat(outDf450_1)
g44<- plot450_1 <- plotfuncXonly(df_upperLower450_1, study450_1)

df_upperLower249_2 <- quantmat(outDf249_2)
g48  <- plotfuncXonly(df_upperLower249_2, study249_2)

sero_vimc <- grid.arrange(g3,g5, g11, g17,g19,
             g29,g34, g36,g39,g41,g44,g48, ncol = 3,
             left = "Proportion Seropositive",
             bottom = "Age (years)")
ggsave("sero_vimc.pdf", sero_vimc, dpi=1000, device= "pdf", height=5, width=8,units="in", bg=NULL)



