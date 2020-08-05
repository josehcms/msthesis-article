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
        '5' = 'Midwest'
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

