##################################################################
### Paper title: Fertility and educational pairings: 
###              an analysis through cohort fertility
### Supplementary material 1:
###  - Read, process and adjust data variables
### Author: Jose H C Monteiro da Silva
### Last Update: 2021-03-21
##################################################################

### 1. Housekeeping and package loading #-------------------------
rm( list = ls( ) )
graphics.off( )

require( data.table ); require( dplyr )

reg_dict <- 
  c(
    '1' = 'North-Northeast',
    '2' = 'North-Northeast',
    '3' = 'South-Southeast',
    '4' = 'South-Southeast',
    '5' = 'Midwest'
  )
##################################################################

### 2. Matching couples from each household #---------------------

datBRMatched <- 
  do.call(
    rbind,
    lapply(
      c( 1970, 1980, 1991, 2000, 2010 ),
      function( x ){
        
        gc( reset = TRUE )
        
        load( paste0( 'RAW_DATA/BR', x, '.RData' ) )
        
        dat <- 
          get( paste0( 'br', x ) ) %>%
          as.data.table %>%
          .[,
            list(
              REGNBR  = reg_dict[ paste0( substr( GEO1_BR, 5, 5 ) )  ] ,
              YEAR,
              SERIAL,
              RELATE  = RELATE %>% paste0 %>% as.numeric,
              RELATED = RELATED %>% paste0 %>% as.numeric,
              AGE = AGE %>% paste0 %>% as.numeric %>% floor,
              SEX = SEX %>% paste0 %>% as.numeric,
              MARST = MARST %>% paste0 %>% as.numeric,
              CHBORN = CHBORN %>% paste0 %>% as.numeric %>% floor,
              EDATTAIN,
              PERWT
            ) ]
          
        rm( list = paste0( 'br', x ) )
        
        # first match those couples directly related to household head, 
        # i.e. male or female are household heads
        match_dat1 <- 
          merge(
            # males list:
            dat[ AGE %in% seq( 40, 99 ) & 
                   SEX == 1 & 
                   RELATE < 3 & RELATED < 2300 & MARST == 2,
                 list(
                   Region   = REGNBR,
                   Year     = YEAR,
                   HouseID  = SERIAL,
                   AgeMale  = AGE,
                   EducMale = EDATTAIN
                 ) ],
            # females list:
            dat[ AGE %in% seq( 40, 69 ) & 
                   SEX == 2 & RELATE < 3 & RELATED < 2300 & MARST == 2,
                 list(
                   Region     = REGNBR,
                   Year       = YEAR,
                   HouseID    = SERIAL,
                   AgeFemale  = AGE,
                   EducFemale = EDATTAIN,
                   Parity     = CHBORN,
                   SampWeight = PERWT
                 )],
            
            by = c( 'Year', 'Region', 'HouseID' )
          )
        
        # match couples whose members are not household heads, i.e, parents
        ids_match_dat2 <- 
          dat[ 
            RELATED %in% c( 4200, 4210, 4220 ),
            .N,
            .( SERIAL, RELATED, MARST ) 
            ] %>%
          .[ N == 2 & MARST == 2,
             SERIAL
             ]
        
        match_dat2 <- 
          merge(
            # males list:
            dat[ SEX == 1 &
                   AGE %in% seq( 40, 99 ) &
                   MARST == 2 & RELATED %in% c( 4200, 4210, 4220 ),
                 list(
                   Region   = REGNBR,
                   Year     = YEAR,
                   HouseID  = SERIAL,
                   AgeMale  = AGE,
                   EducMale = EDATTAIN,
                   RELATED
                   ) ],
            # females list:
            dat[ AGE %in% seq( 40, 69 ) & 
                   SEX == 2 & 
                   MARST == 2 &  
                   SERIAL %in% ids_match_dat2 & 
                   RELATED %in% c( 4200, 4210, 4220 ),
                 list(
                   Region     = REGNBR,
                   Year       = YEAR,
                   HouseID    = SERIAL,
                   AgeFemale  = AGE,
                   EducFemale = EDATTAIN,
                   Parity     = CHBORN,
                   SampWeight = PERWT,
                   RELATED
                   ) ],
                       
            by = c( 'Year', 'Region', 'HouseID', 'RELATED' )
          ) %>%
          .[ , - 'RELATED' ]
        
        # bind both matched datasets
        match_dat <- 
          rbind( match_dat1, match_dat2 )  %>%
          # correct implausible parities and set cohorts for analysis
          .[ , 
             `:=`(
               # adjust sample weights
               SampWeight     = as.numeric( SampWeight ) / 100,
               # compute birth cohorts
               FemCohort   = Year - AgeFemale,
               # compute 5-year group cohorts
               FemCohort5  = cut( ( Year - AgeFemale ), 
                                  breaks = c( -Inf, 
                                              seq( 1925, 1970, 5 ),
                                              Inf ),
                                  labels = c( '< 1925',
                                              paste0( seq( 1925, 1965, 5 ) ),
                                              '> 1969' ),
                                  right = FALSE,
                                  include.lowest = TRUE ) %>% paste0,
               # compute 15-year group cohorts
               FemCohort15 = cut( ( Year - AgeFemale ), 
                                  breaks = c( -Inf, 
                                              seq( 1925, 1970, 15 ),
                                              Inf ),
                                  labels = c( '< 1925',
                                              paste0( seq( 1925, 1965, 15 ) ),
                                              '> 1969' ),
                                  right = FALSE,
                                  include.lowest = TRUE ) %>% paste0,
               # adjust unexpected parities                 
               ParityCor = ifelse( Parity > 30,
                                   NA,
                                   ifelse( Parity > ( 2 / 3 * AgeFemale - 8 ) & AgeFemale > 57, 
                                           30,
                                           ifelse( Parity > ( 2 / 3 * AgeFemale - 8 ) & AgeFemale < 58,
                                                   trunc( 2 / 3 * AgeFemale - 8 ),
                                                   Parity
                                                   )
                                           )
                                   ),
               # flag adjusted parities               
               ParityFlag = ifelse( Parity > 30,
                                    NA,
                                    ifelse( Parity > ( 2 / 3 * AgeFemale - 8 ) & AgeFemale > 57, 
                                            1,
                                            ifelse( Parity > ( 2 / 3 * AgeFemale - 8 ) & AgeFemale < 58,
                                                    1,
                                                    0
                                                    )
                                            )
                                    )
               ) ]
        
        rm( dat, match_dat1, match_dat2 )
        
        # return dataset
        return( match_dat )
       
      }
    )
  )

datBRAll <- 
  do.call(
    rbind,
    lapply(
      c( 1970, 1980, 1991, 2000, 2010 ),
      function( x ){
        
        gc( reset = TRUE )
        
        load( paste0( 'RAW_DATA/BR', x, '.RData' ) )
        
        dat <- 
          get( paste0( 'br', x ) ) %>%
          as.data.table %>%
          .[,
            list(
              REGNBR  = reg_dict[ paste0( substr( GEO1_BR, 5, 5 ) ) ],
              YEAR,
              SERIAL,
              RELATE  = RELATE %>% paste0 %>% as.numeric,
              RELATED = RELATED %>% paste0 %>% as.numeric,
              AGE = AGE %>% paste0 %>% as.numeric %>% floor,
              SEX = SEX %>% paste0 %>% as.numeric,
              MARST = MARST %>% paste0 %>% as.numeric,
              CHBORN = CHBORN %>% paste0 %>% as.numeric %>% floor,
              EDATTAIN,
              PERWT
            ) ] %>%
          .[ AGE %in% seq( 40, 69 ) & SEX == 2,
             list(
               Region     = REGNBR,
               Year       = YEAR,
               HouseID    = SERIAL,
               AgeFemale  = AGE,
               EducFemale = EDATTAIN,
               Parity     = CHBORN,
               SampWeight = PERWT
             ) ] %>%
          .[ , 
             `:=`(
               # adjust sample weights
               SampWeight = as.numeric( SampWeight ) / 100,
               # compute birth cohorts
               FemCohort = Year - AgeFemale,
               # compute 5-year group cohorts
               FemCohort5 = cut( ( Year - AgeFemale ),
                                 breaks = c( -Inf,
                                             seq( 1925, 1970, 5 ),
                                             Inf ),
                                 labels = c( '< 1925',
                                             paste0( seq( 1925, 1965, 5 ) ),
                                             '> 1969' ),
                                 right = FALSE,
                                 include.lowest = TRUE ) %>% paste0,
               # compute 15-year group cohorts
               FemCohort15 = cut( ( Year - AgeFemale ), 
                                  breaks = c( -Inf, 
                                              seq( 1925, 1970, 15 ),
                                              Inf ),
                                  labels = c( '< 1925',
                                              paste0( seq( 1925, 1965, 15 ) ),
                                              '> 1969' ),
                                  right = FALSE,
                                  include.lowest = TRUE ) %>% paste0,
               # adjust unexpected parities                 
               ParityCor = ifelse( Parity > 30,
                                   NA,
                                   ifelse( Parity > ( 2 / 3 * AgeFemale - 8 ) & 
                                             AgeFemale > 57, 
                                           30,
                                           ifelse( Parity > 
                                                     ( 2 / 3 * AgeFemale - 8 ) &
                                                     AgeFemale < 58,
                                                   trunc( 2 / 3 * 
                                                            AgeFemale - 8 ),
                                                   Parity
                                           )
                                   )
               ),
               # flag adjusted parities               
               ParityFlag = ifelse( Parity > 30,
                                    NA,
                                    ifelse( Parity > 
                                              ( 2 / 3 * AgeFemale - 8 ) & 
                                              AgeFemale > 57, 
                                            1,
                                            ifelse( Parity > 
                                                      ( 2 / 3 * AgeFemale - 8 ) & 
                                                      AgeFemale < 58,
                                                    1,
                                                    0
                                                    )
                                            )
                                    )
               ) ]
        
               
        rm( list = paste0( 'br', x ) )
        
        # return dataset
        return( dat )
        
      }
    )
  )
##################################################################

### 3. Save data #------------------------------------------------

write.table( 
  datBRMatched,
  file = 'DATA/matched_data_br.csv',
  row.names = FALSE,
  sep = ',' 
  )

write.table( 
  datBRAll,
  file = 'DATA/all_females_data_br.csv',
  row.names = FALSE,
  sep = ',' 
)
##################################################################


