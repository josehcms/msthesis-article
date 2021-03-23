##################################################################
### Paper title: Fertility and educational pairings: 
###              an analysis through cohort fertility
### Supplementary material 2:
###  - Data quality evaluation and sample description
### Author: Jose H C Monteiro da Silva
### Last Update: 2021-03-21
##################################################################

### 1. Housekeeping and package loading #-------------------------
rm(list = ls())
graphics.off()

require(data.table)
require(dplyr)

datBRMatched <- 
  fread( 'DATA/matched_data_br.csv' )

datBRMatched <- 
  rbind(
    datBRMatched %>% copy %>%
      .[ , Region := 'Brazil' ],
    datBRMatched
  )
##################################################################

### 2. Missing information #--------------------------------------

# 2.1 % of missing parity
parityNA_tab <- 
  datBRMatched[ FemCohort15 %in% c( '1925', '1940', '1955' ),
                .( 
                  ParityNA = 
                    round( 100 * 
                             sum( SampWeight[ is.na( ParityFlag ) ] ) / 
                             sum( SampWeight ),
                           2 ) 
                ), 
                .( Region, Year )
                ] %>%
  setorder( Region, Year ) %>%
  dcast( Year ~ Region )
  
# 2.2 % of parity implausible
parityImp_tab <- 
  datBRMatched[ !is.na( ParityFlag ) & 
                  FemCohort15 %in% c( '1925', '1940', '1955' ),
                .( 
                  ParityImp = 
                    round( 100 * 
                             sum( SampWeight[ ParityFlag == 1 ] ) / 
                             sum( SampWeight ),
                           2 ) 
                ), 
                .( Region, Year )
  ] %>%
  setorder( Region, Year ) %>%
  dcast( Year ~ Region )

# 2.3 missing education for either male or female
EducNA_tab <- 
  datBRMatched[ FemCohort15 %in% c( '1925', '1940', '1955' ),
                .( 
                  EducFemNA = 
                    round( 100 * 
                             sum( SampWeight[ ( ! EducFemale %in% 1:4 ) |
                                                ( ! EducMale %in% 1:4 ) ] ) / 
                             sum( SampWeight ),
                           2 ) 
                ), 
                .( Region, Year )
  ] %>%
  setorder( Region, Year ) %>%
  dcast( Year ~ Region )

##################################################################

### 3. Sample size #----------------------------------------------

datBRMatched[ , n := 1 ]

SampSizeTab <- 
  datBRMatched[ FemCohort15 %in% c( 1925, 1940, 1955 ) & 
                  ! is.na( ParityFlag ) &
                  EducFemale %in% 1:4 &
                  EducMale %in% 1:4, ] %>%
  copy %>%
  .[ ,.N,
     .( Region, Year, FemCohort5 ) ] %>%
  dcast( FemCohort5 + Year ~ Region )
                

##################################################################


### Prepare flextables #------------------------------------------
require(flextable)
require(officer)

tab_a1 <- 
  SampSizeTab[ ,
               list(
                 `Cohort` = paste0( as.numeric( paste0( FemCohort5 ) ), 
                                    '-',
                                    as.numeric( paste0( 
                                      ( FemCohort5  ) ) ) + 4 ),
                 `Census Year` = Year,
                 Brazil,
                 Midwest,
                 `North-Northeast`,
                 `South-Southeast`
               ) ] %>%
  flextable() %>%
  colformat_num( big.mark = '' ) %>%
  align( align = "center", part = "all" ) %>%
  hline( i = 1,
         border = fp_border( color = 'black' ),
         part = 'header' )

tab_a2 <-
  EducNA_tab[ ,
              list(
                `Census Year` = Year,
                Brazil,
                Midwest,
                `North-Northeast`,
                `South-Southeast`
              ) ] %>%
  flextable() %>%
  colformat_num( big.mark = '' ) %>%
  align( align = "center", part = "all" ) %>%
  hline( i = 1,
         border = fp_border( color = 'black' ),
         part = 'header' )

tab_a3 <-
  parityNA_tab[ ,
              list(
                `Census Year` = Year,
                Brazil,
                Midwest,
                `North-Northeast`,
                `South-Southeast`
              ) ] %>%
  flextable() %>%
  colformat_num( big.mark = '' ) %>%
  align( align = "center", part = "all" ) %>%
  hline( i = 1,
         border = fp_border( color = 'black' ),
         part = 'header' )

tab_a4 <-
  parityImp_tab[ ,
                 list(
                   `Census Year` = Year,
                   Brazil,
                   Midwest,
                   `North-Northeast`,
                   `South-Southeast`
                 ) ] %>%
  flextable() %>%
  colformat_num( big.mark = '' ) %>%
  align( align = "center", part = "all" ) %>%
  hline( i = 1,
         border = fp_border( color = 'black' ),
         part = 'header' )

save( tab_a1, tab_a2, tab_a3, tab_a4,
      file =  'FIGS/tablesAppendix.RData')

##################################################################
