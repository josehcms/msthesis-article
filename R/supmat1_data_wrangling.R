##################################################################
### Paper title: Fertility and educational pairings: 
###              an analysis through cohort fertility
### Supplementary material 1 - data wrangling
##################################################################

### 1. Housekeeping and package loading #-------------------------
rm(list = ls())
graphics.off()

require(data.table)
require(dplyr)
require(survey)
options(survey.lonely.psu="adjust")
##################################################################

### 2. Matching couples from each household #---------------------

# 2.1 Create function for matching couples in each household
cpl.match <- 
  function(ipums.dat){
    
    # dictionary for Brazilian regions
    reg.dict <- 
      c(
        '1' = 'North-Northeast',
        '2' = 'North-Northeast',
        '3' = 'South-Southeast',
        '4' = 'South-Southeast',
        '5' = 'Central-West'
      )

    setDT(ipums.dat)
    
    ipums.dat <-
      ipums.dat[,
                list(
                  REGNBR  = reg.dict[ substr( GEO1_BR, 5, 5 )  ] ,
                  YEAR,
                  SERIAL,
                  RELATE,
                  RELATED,
                  AGE    = as.integer( AGE ),
                  SEX,
                  MARST,
                  CHBORN = as.integer( CHBORN ),
                  EDATTAIN,
                  PERWT
                )]
    
    # first match those couples directly related to household head, i.e. male or female are household heads
    matched.dat1 <- 
      merge(
        # males list:
        ipums.dat[ AGE %in% seq(35,79) & SEX == 1 & RELATE < 3 & RELATED < 2300 & MARST == 2,
                   list(
                     reg      = REGNBR,
                     year     = YEAR,
                     SERIAL,
                     age.mal  = AGE,
                     educ.mal = EDATTAIN
                   )],
        # females list:
        ipums.dat[ AGE %in% seq(39,69) & SEX == 2 & RELATE < 3 & RELATED < 2300 & MARST == 2,
                   list(
                     reg      = REGNBR,
                     year     = YEAR,
                     SERIAL,
                     age.fem  = AGE,
                     educ.fem = EDATTAIN,
                     chborn   = CHBORN,
                     perwt    = PERWT
                   )],
        by = c( 'year', 'reg', 'SERIAL')
      )
    
    # match couples whose members are not household heads, i.e, parents

    serials.match2 <- 
      ipums.dat[ RELATED %in% c(4200,4210,4220),
                 .N,
                 .(SERIAL, RELATED, MARST)] %>%
      .[ N == 2 & MARST == 2,
         SERIAL
         ]
    
    matched.dat2 <- 
      merge(
        # males list:
        ipums.dat[ AGE %in% seq(35,79) & SEX == 1 & MARST == 2 & RELATED %in% c( 4200, 4210, 4220 ),
                   list(
                     reg      = REGNBR,
                     year     = YEAR,
                     SERIAL,
                     age.mal  = AGE,
                     educ.mal = EDATTAIN,
                     RELATED
                   )],
        # females list:
        ipums.dat[ AGE %in% seq(39,69) & SEX == 2 & MARST == 2 & SERIAL %in% serials.match2 & RELATED %in% c( 4200, 4210, 4220 ),
                   list(
                     reg      = REGNBR,
                     year     = YEAR,
                     SERIAL,
                     age.fem  = AGE,
                     educ.fem = EDATTAIN,
                     chborn   = CHBORN,
                     perwt    = PERWT,
                     RELATED
                   )],
        by = c( 'year', 'reg', 'SERIAL', 'RELATED')
      ) %>%
      .[,-'RELATED']

    # bind both matched datasets
    matched.dat <- 
      rbind( matched.dat1, matched.dat2 )
    
    # correct implausible parities and set cohorts for analysis
    matched.dat[,`:=`(
      perwt     = as.numeric(perwt)/100,
      cohort    = year - age.fem,
      cohort_5  = cut( (year - age.fem), 
                       breaks = seq( 1924, 1969, 5 ),  
                       labels = seq( 1925, 1965, 5 ) 
                       ),
      cohort_15 = cut( (year - age.fem), 
                       breaks = seq( 1924, 1969, 15 ), 
                       labels = c("1925-39", "1940-54", "1955-69") 
                       ),
      chborn    = ifelse( chborn > ( 2 / 3 * age.fem - 8 ) & age.fem > 57, 
                          30,
                          ifelse( chborn > ( 2 / 3 * age.fem - 8) & age.fem < 58,
                                  trunc( 2 / 3 * age.fem - 8 ),
                                  chborn 
                                  ) 
                          )
    )]

    return( matched.dat )
  }

# 2.2 Load data, match couples for each census and save matched data

  # BR 1970 
  load("RAW_DATA/BR1970.RData")
  br1970.match <- cpl.match( br1970 )
  rm(br1970)

  # BR 1980 
  load("RAW_DATA/BR1980.RData")
  br1980.match <- cpl.match( br1980 )
  rm(br1980)

  # BR 1991
  load("RAW_DATA/BR1991.RData")
  br1991.match <- cpl.match( br1991 )
  rm(br1991)
  
  # BR 2000 
  load("RAW_DATA/BR2000.RData")
  br2000.match <- cpl.match( br2000 )
  rm(br2000)
  
  # BR 2010 
  load("RAW_DATA/BR2010.RData")
  br2010.match <- cpl.match( br2010 )
  rm(br2010)
  
  save(
    br2010.match, br2000.match, br1991.match, br1980.match, br1970.match,
       file="DATA/MATCHED_DATABR.RData")
  
##################################################################

### 3. Compute cohort fertility rates for each educ pairing #-----

  load("DATA/MATCHED_DATABR.RData")
  # 3.1 Bind matched data and create label for Brazil and create cohorts
  match.dat <- 
    rbind( br1970.match, br1980.match, br1991.match, br2000.match, br2010.match )
  
  rm( br1970.match, br1980.match, br1991.match, br2000.match, br2010.match )
  
  match.dat <- 
    rbind(
      match.dat %>% copy,
      match.dat[, reg:= 'Brazil' ] 
    )
  
  
  # 3.2 Compute CFR for couples with wheigts 
  cfrpair.eval <- 
    function( match.dat ,
              cohort.span = 5 ){
      
      
      if ( cohort.span == 5 ){
        
        # join educ 3 and 4:
        match.dat[, `:=`(
          educ.fem = ifelse( educ.fem == 4, 
                             3,
                             educ.fem
          ),
          educ.mal = ifelse( educ.mal == 4, 
                             3,
                             educ.mal
          )
        )]
        
        # create empty data.table to store cfr data
        cfrcoupl.dat <- 
          data.table()
        
        # loop to compute mean chborn for each pairing, region and cohort
        for( sel.cohort in unique( match.dat$cohort_5 ) ){
          for( sel.reg in unique( match.dat$reg ) ){
            for( sel.educf in 1:3 ){
              for( sel.educm in 1:3 ){
                cfrcoupl.dat <-
                  rbind(
                    cfrcoupl.dat,
                    data.table(
                      reg = sel.reg,
                      cohort_5 = sel.cohort,
                      educ.fem = sel.educf,
                      educ.mal = sel.educm,
                      CFR = match.dat[ cohort_5 == sel.cohort & reg == sel.reg & educ.fem == sel.educf & educ.mal == sel.educm , 
                                       sum( chborn * perwt ) / sum( perwt )
                                       ]
                    )
                  )
              }
            }
          }
        }
      }
      
      if ( cohort.span == 15 ){
        # create empty data.table to store cfr data
        cfrcoupl.dat <- 
          data.table()
        
        # loop to compute mean chborn for each pairing, region and cohort
        for( sel.cohort in unique( match.dat$cohort_15 ) ){
          for( sel.reg in unique( match.dat$reg ) ){
            for( sel.educf in 1:4 ){
              for( sel.educm in 1:4 ){
                cfrcoupl.dat <-
                  rbind(
                    cfrcoupl.dat,
                    data.table(
                      reg = sel.reg,
                      cohort_15 = sel.cohort,
                      educ.fem = sel.educf,
                      educ.mal = sel.educm,
                      CFR = match.dat[ cohort_15 == sel.cohort & reg == sel.reg & educ.fem == sel.educf & educ.mal == sel.educm , 
                                       sum( chborn * perwt ) / sum( perwt )
                                       ]
                    )
                  )
              }
            }
          }
        }
      }
      
      return(cfrcoupl.dat)
    }
  

aux <- cfrpair.eval(match.dat)
aux <- cfrpair.eval(match.dat, cohort.span = 15)

## 4. Calculo dos pareamentos, grafico tipo 2 - influencia masculina vs feminina #################
require(ggplot2)
require(dplyr)
require(survey)
require(data.table)
options(survey.lonely.psu="adjust")
load("/home/jose-costa/Documents/MESTRADO/BASE DE DADOS/DOM-BR.RData")

DOM_BR <- rbind(DOM_BR1970,DOM_BR1980,DOM_BR1991,DOM_BR2000,DOM_BR2010)
rm(DOM_BR1970,DOM_BR1980,DOM_BR1991,DOM_BR2000,DOM_BR2010)

# Insere o Brasil como uma regiao a parte
DOM_BR <- rbind(DOM_BR,DOM_BR%>%mutate(reg=0))

# Funcao para definicao do desenho amostral a partir dos pesos
peso <- function(DOM){
  require(data.table)
  require(survey)
  setDT(DOM)
  
  DOM[,peso:=as.numeric(peso)/100]
  DOM[,coorte:=year-age.fem]
  DOM <- DOM[coorte %in% seq(1925,1969) & chborn<98 & educ.fem<9 & educ.mal<9,]
  DOM[, reg := ifelse(reg==1,"Norte",
                      ifelse(reg==2,"Nordeste",
                             ifelse(reg==3,"Sudeste",
                                    ifelse(reg==4,"Sul",
                                           ifelse(reg==5,"Centro-Oeste","Brasil")))))]
  
  DOM[, `:=`(coorte_5  = cut(coorte, breaks = seq(1924,1969,5),  labels=seq(1925,1965,5)),
             coorte_15 = cut(coorte, breaks = seq(1924,1969,15), labels=c("1925-39","1940-54","1955-69")),
             chborn    = ifelse(chborn>(2/3*age.fem-8) & age.fem>57,30,
                                ifelse(chborn>(2/3*age.fem-8) & age.fem<58,trunc(2/3*age.fem-8),chborn)))]
  
  samp_des <- svydesign(ids=~1,weights =~peso,data=DOM)
  return(samp_des)
}

br_tot_des <- peso(DOM_BR) #usar peso de geral

#Funcao para montar as bases de grafico a partir da funcao de desenho amostral, quinze anos
monta_bases_15 <- function(des){
  require(survey)
  require(dplyr)
  require(data.table)
  
  par_sample.design_15 <- function(coorte_sel,reg_sel,des){
    # Salva os elementos do calculo de media de filhos em lista com varios data.frames
    count = 1
    base = list()
    for(i in 1:4){
      for(j in 1:4){
        base[count] <- list(
          data.frame(
            svymean(~chborn,
                    design=subset(des,
                                  educ.fem==i & 
                                    educ.mal==j &
                                    coorte_15==coorte_sel & 
                                    reg==reg_sel)))%>%
            mutate(educ.fem=i,
                   educ.mal=j,
                   reg=reg_sel,
                   coorte_15=coorte_sel,
                   SE = chborn,
                   CFR = mean)%>%
            select(reg,coorte_15,educ.fem,educ.mal,CFR,SE))
        
        count = count+1
      }
    }
    
    # Transforma lista em data.frame de novo com reg, educ.fem, educ.mal, mean, SE, coorte
    base_df = base[[1]]
    for (i in 2:length(base)){
      base_df=rbind(base_df,base[[i]])
    }
    
    return(base_df)
  }
  cont=1
  aux = list()
  for(count_reg in c("Centro-Oeste","Nordeste","Norte","Sudeste","Sul","Brasil")){
    for(count_coor in c("1925-39","1940-54","1955-69")){
      aux[cont]= list(par_sample.design_15(coorte_sel=count_coor,reg_sel=count_reg,des=des)) 
      cont=cont+1
    }
  } 
  
  aux_df = aux[[1]]
  for (i in 2:length(aux)){
    aux_df=rbind(aux_df,aux[[i]])
  }
  
  aux_fem <- aux_df %>%
    mutate(classif = ifelse(educ.fem== 1 & educ.mal==1,0,
                            ifelse(educ.fem==2 & educ.mal==1,1,
                                   ifelse(educ.fem==2 & educ.mal==2,2,
                                          ifelse(educ.fem==3 & educ.mal==1,3,
                                                 ifelse(educ.fem==3 & educ.mal==2,4,
                                                        ifelse(educ.fem==3 & educ.mal==3,5,
                                                               ifelse(educ.fem==4 & educ.mal==1,6,
                                                                      ifelse(educ.fem==4 & educ.mal==2,7,
                                                                             ifelse(educ.fem==4 & educ.mal==3,8,
                                                                                    ifelse(educ.fem==4 & educ.mal==4,9,99)))))))))))%>%
    filter(classif != 99)%>%
    mutate(sex = "Mulher>=Homem")
  
  aux_mal <- aux_df %>%
    mutate(classif = ifelse(educ.fem== 1 & educ.mal==1,0,
                            ifelse(educ.fem==1 & educ.mal==2,1,
                                   ifelse(educ.fem==2 & educ.mal==2,2,
                                          ifelse(educ.fem==1 & educ.mal==3,3,
                                                 ifelse(educ.fem==2 & educ.mal==3,4,
                                                        ifelse(educ.fem==3 & educ.mal==3,5,
                                                               ifelse(educ.fem==1 & educ.mal==4,6,
                                                                      ifelse(educ.fem==2 & educ.mal==4,7,
                                                                             ifelse(educ.fem==3 & educ.mal==4,8,
                                                                                    ifelse(educ.fem==4 & educ.mal==4,9,99)))))))))))%>%
    filter(classif != 99)%>%
    mutate(sex = "Homem>=Mulher")
  
  aux_f <- as.data.table(rbind(aux_fem,aux_mal))
  
  aux_f[,`:=`(y_se.min=(CFR-1.96*SE),
              y_se.max=(CFR+1.96*SE)),]
  return(aux_f)
  
}


# Monta a base de coortes de quinze anos (1925, 1940, 1954)
base_par_15 <- monta_bases_15(br_tot_des)


setwd("~/Documents/MESTRADO/BASE DE DADOS/")
save(base_par_15,file="base_graficos_15.RData")



####################################

## 5. Calculo da fecundidade de coorte de mulheres unidas, metodologia 3 ###################
load("/home/jose-costa/Documents/MESTRADO/BASE DE DADOS/DOM-BR.RData")

CFR.eval_dom <- function(data){
  require(dplyr)
  # Filtra idade feminina >= 40 anos e menor que 70 anos e faz correção de parturição improvável
  
  data.fem  <- data %>% 
    mutate(peso = as.numeric(peso)/100, 
           chborn = as.numeric(chborn))%>%
    filter(chborn < 98, educ.fem < 9, educ.mal<9)%>%
    mutate(coorte = year - age.fem)%>%
    filter(coorte>=1925 & coorte<1970)%>%
    mutate(coorte = as.numeric(paste(cut(coorte, c(1924,1929,1934,1939,1944,1949,1954,1959,1964,1969), 
                                         labels=seq(1925,1965,5)))))%>%
    select(year,reg,coorte,educ.fem,peso,chborn)
  require(survey)
  
  data.fem = rbind(data.fem,data.fem%>%mutate(reg=0))
  
  des.fem = svydesign(ids=~1,weights=~peso, data=data.fem,nest=TRUE)
  
  sample.design_cfr <- function(des){
    
    # Salva os elementos do calculo de media de filhos em lista com varios data.frames
    count = 1
    base = list()
    for(i in 1:4){
      for(coorte_sel in seq(min(des$variables$coorte),max(des$variables$coorte))){
        for(reg_sel in seq(0,5)){
          
          if(nrow(subset(des$variables,
                         educ.fem==i & 
                         coorte==coorte_sel & 
                         reg==reg_sel))==0){
            next
          }
          
          base[count] <- list(
            data.frame(
              svymean(~chborn,design=subset(des,
                                            educ.fem==i & 
                                              coorte==coorte_sel & 
                                              reg==reg_sel)))%>%
              mutate(educ.fem=i,
                     reg=reg_sel,
                     coorte=coorte_sel,
                     SE = chborn,
                     CFR.F = mean)%>%
              select(reg,coorte,educ.fem,CFR.F,SE))
          
          count = count+1
        }
      }
    }
    
    
    # Transforma lista em data.frame de novo com reg, educ.fem, educ.mal, mean, SE, coorte
    base_df = base[[1]]
    for (i in 2:length(base)){
      base_df=rbind(base_df,base[[i]])
    }
    
    return(base_df)
  }
  
  CFR=sample.design_cfr(des.fem)
  
  return(CFR)
}
DOM_BR = rbind(DOM_BR1970,DOM_BR1980,DOM_BR1991,DOM_BR2000,DOM_BR2010)
rm(DOM_BR1970,DOM_BR1980,DOM_BR1991,DOM_BR2000,DOM_BR2010)

CFR.F = CFR.eval_dom(DOM_BR)

load("~/Documents/MESTRADO/BASE DE DADOS/base_graficos_5.RData")

base_fem_5 <- CFR.F %>%
  mutate(reg=ifelse(reg==0,
                    "Brasil",
                    ifelse(reg==1,
                           "Norte",
                           ifelse(reg==2,
                                  "Nordeste",
                                  ifelse(reg==3,
                                         "Sudeste",
                                         ifelse(reg==4,
                                                "Sul","Centro-Oeste"))))))%>%
  mutate(coorte_5=coorte)%>%
  select(reg,coorte_5,educ.fem,CFR.F,SE)

base_modelo <- merge(base_par_5,base_fem_5,by=c("reg","coorte_5","educ.fem"))


aux = base_modelo%>%filter(reg=="Brasil")
lm(data=aux%>%filter(educ.fem==1,educ.mal==1),CFR~CFR.F)
lm(data=aux%>%filter(educ.fem==1,educ.mal==2),CFR~CFR.F)
lm(data=aux%>%filter(educ.fem==1,educ.mal==3),CFR~CFR.F)

lm(data=aux%>%filter(educ.fem==2,educ.mal==1),CFR~CFR.F)
lm(data=aux%>%filter(educ.fem==2,educ.mal==2),CFR~CFR.F)
lm(data=aux%>%filter(educ.fem==2,educ.mal==3),CFR~CFR.F)

lm(data=aux%>%filter(educ.fem==3,educ.mal==1),CFR~CFR.F)
lm(data=aux%>%filter(educ.fem==3,educ.mal==2),CFR~CFR.F)
lm(data=aux%>%filter(educ.fem==3,educ.mal==3),CFR~CFR.F)

x11()
ggplot(data=aux,aes(x=CFR.F,y=CFR,color=as.factor(educ.mal)))+
  facet_wrap(~as.factor(educ.fem),ncol = 3)+
  geom_point(aes(color=as.factor(educ.mal)))+
  geom_smooth(method = "lm", se = FALSE)+
  scale_x_continuous(limits=c(0,7),breaks=seq(0,7))+
  scale_y_continuous(limits=c(0,7),breaks=seq(0,7))


#####################################################

require(ggplot2)
require(dplyr)
require(survey)
require(data.table)
options(survey.lonely.psu="adjust")
load("/home/jose-costa/Documents/MESTRADO/BASE DE DADOS/DOM-BR.RData")
# Insere o Brasil como uma regiao a parte
DOM_BR <- rbind(DOM_BR,DOM_BR%>%mutate(reg=0))

# Funcao para definicao do desenho amostral a partir dos pesos
peso <- function(DOM){
  require(data.table)
  require(survey)
  setDT(DOM)
  
  DOM[,peso:=as.numeric(peso)/100]
  DOM[,coorte:=year-age.fem]
  DOM <- DOM[coorte %in% seq(1925,1969) & chborn<98 & educ.fem<9 & educ.mal<9,]
  DOM[, reg := ifelse(reg==1,"Norte",
                      ifelse(reg==2,"Nordeste",
                             ifelse(reg==3,"Sudeste",
                                    ifelse(reg==4,"Sul",
                                           ifelse(reg==5,"Centro-Oeste","Brasil")))))]
  
  DOM[, `:=`(coorte_5  = cut(coorte, breaks = seq(1924,1969,5),  labels=seq(1925,1965,5)),
             coorte_15 = cut(coorte, breaks = seq(1924,1969,15), labels=c("1925-39","1940-54","1955-69")),
             chborn    = ifelse(chborn>(2/3*age.fem-8) & age.fem>57,30,
                                ifelse(chborn>(2/3*age.fem-8) & age.fem<58,trunc(2/3*age.fem-8),chborn)))]
  
  samp_des <- svydesign(ids=~1,weights =~peso,data=DOM)
  return(samp_des)
}

br_tot_des <- peso(DOM_BR) #usar peso de geral
head(DOM_BR)
summary(svyglm(design=br_tot_des,formula = chborn~educ.fem))


head(DOM_BR)
??family

model=glm(chborn~1+age.fem+age.mal+as.factor(educ.fem)+as.factor(educ.mal)+as.factor(reg),data=DOM_BR,family=poisson(link=log))
summary(model)
names(summary(model)$dispersion)




##### CFR ####

grafico.CFR <- function(data.dom,estado){
  library(ggplot2)
  library(dplyr)
  aux <- data.dom%>%select(reg,year,age.fem,educ.fem,educ.mal,chborn)%>%
    mutate(coorte=year-age.fem)%>%
    filter(chborn<98 & educ.mal %in% c(1,2,3,4) & educ.fem %in% c(1,2,3,4) & coorte>1924 & coorte <1970)%>%
    mutate(coorte = cut(coorte, c(1924,1929,1934,1939,1944,1949,1954,1959,1964,1969), 
                        labels=seq(1925,1965,5)))%>%
    mutate(educ.fem=ifelse(educ.fem==1,"Female: Less Than Primary",
                           ifelse(educ.fem==2,"Female: Primary",
                                  ifelse(educ.fem==3,"Female: Secondary","Female: Tertiary"))),
           educ.mal=ifelse(educ.mal==1,"Less Than Primary",
                           ifelse(educ.mal==2,"Primary",
                                  ifelse(educ.mal==3,"Secondary","Tertiary"))))%>%
    group_by(coorte,educ.fem,educ.mal)%>%
    summarise(CFR=mean(chborn),n=n(),ch=sum(chborn))
  
  ggplot(data=aux, aes(x=coorte,y=CFR,color=as.factor(educ.mal),group=as.factor(educ.mal)))+
    geom_smooth(span=0.4,se=F)+
    facet_wrap( ~ educ.fem, nrow=1)+ 
    labs(title = paste("Cohort Fertility Rates by Educational Pairing of Couples - ",estado,sep=""), 
         subtitle = "Women Born in 1925-1969, five-year interval",
         caption = "Source: IPUMS-International",
         x = "Female Cohort Year of Birth", 
         y = "Cohort Fertility Rates", color = "") +
    scale_color_manual(labels = c("Less than Primary", "Primary","Secondary", "Tertiary"), 
                       values = c("black","red","blue", "forestgreen"),
                       name="Male Educational Attainment Level") +
    theme_bw() +theme(legend.position="bottom",
                      axis.text.y = element_text(size=22),
                      axis.text.x = element_text(size=19),
                      axis.title = element_text(size=24),
                      plot.title = element_text(size=26),
                      plot.caption = element_text(size=20),
                      plot.subtitle = element_text(size=24),
                      legend.title = element_text(size=23),
                      legend.text=element_text(size=22),
                      strip.text.x = element_text(size = 22),
                      panel.grid.minor = element_line(colour="gray", size=0.5),
                      panel.spacing = unit(2, "lines"),
                      plot.margin = margin(1, 1, 0.5, 1, "cm"))+
    scale_y_continuous(breaks=seq(1,8,1),limits=c(1,8))+
    guides(colour = guide_legend(override.aes = list(size=3)))
}

