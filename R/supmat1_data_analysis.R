##################################################################
### Paper title: Fertility and educational pairings: 
###              an analysis through cohort fertility
### Supplementary material 2 - data analysis
##################################################################

### 1. Housekeeping and package loading #-------------------------
rm(list = ls())
graphics.off()

require(data.table)
require(dplyr)
require(ggplot2)
##################################################################


### 2. Compute cohort fertility rates for each educ pairing #-----
  
  load("DATA/MATCHED_DATABR.RData")

  # 2.1 Bind matched data and create label for Brazil and create cohorts
  match.dat <- 
    rbind( br1970.match, br1980.match, br1991.match, br2000.match, br2010.match )

  rm( br1970.match, br1980.match, br1991.match, br2000.match, br2010.match )

  match.dat <- 
    rbind(
      match.dat %>% copy,
      match.dat[, reg:= 'Brazil' ] 
    )
  

  # 2.2 Compute CFR for couples with wheigts 
  cfrpair.eval <- 
    function( match.dat ,
              cohort.span = 5 ){
      
      match.dat <- 
        match.dat %>%
        as.data.table %>%
        copy
      
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
  

  cfrpair.c5  <- 
    cfrpair.eval( match.dat )
  
  cfrpair.c15 <- 
    cfrpair.eval( match.dat, 
                  cohort.span = 15
    )

##################################################################

### 3. Compute prevalence of each educational pairings #----------

  prevpair.eval <- 
    function( match.dat ,
              cohort.span = 15 ){
      
      match.dat <- 
        match.dat %>%
        as.data.table %>%
        copy %>%
        .[educ.fem <= 4 & educ.mal <= 4 & cohort >= 1925 & cohort < 1970,]
      
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
        prevcoupl.dat <- 
          data.table()
        
        # loop to compute each pairing prevalence, region and cohort
        for (sel.reg in unique( match.dat$reg ) ){
          prevcoupl.dat <-
            rbind(
              prevcoupl.dat,
              match.dat[ reg == sel.reg,
                         list(
                           reg      = sel.reg,
                           totpar   = sum(perwt)
                         ), 
                         .( cohort_5, educ.mal, educ.fem )
                         ] %>%
                .[,
                  prev := round( 100 * totpar / sum( totpar ), 2 ),
                  .(cohort_5)
                  ]
            )
        }
        
      }
      
      
      
      if ( cohort.span == 15 ){
        # create empty data.table to store cfr data
        prevcoupl.dat <- 
          data.table()
        
        # loop to compute each pairing prevalence, region and cohort
        for (sel.reg in unique( match.dat$reg ) ){
          prevcoupl.dat <-
            rbind(
              prevcoupl.dat,
              match.dat[ reg == sel.reg,
                         list(
                           reg      = sel.reg,
                           totpar   = sum(perwt)
                         ), 
                         .( cohort_15, educ.mal, educ.fem )
                         ] %>%
                .[,
                  prev := round( 100 * totpar / sum( totpar ), 2 ),
                  .(cohort_15)
                  ]
            )
        }
      }
      
      return(prevcoupl.dat)
    }
  
  prevpair.c5 <- 
    prevpair.eval( match.dat, 
                   cohort.span = 5
    )
  
  prevpair.c15 <- 
    prevpair.eval( match.dat, 
                   cohort.span = 15
    )
##################################################################

### 4. Decomposition of rates #-----------------------------------

  # 4.1 Merge data from prevalence and fertility

  mergedat.c15 <- 
    merge(
      prevpair.c15,
      cfrpair.c15,
      by = c('reg','cohort_15','educ.fem','educ.mal')
    )
  
  dcastdat.c15 <- 
    merge(
      dcast(
        mergedat.c15,
        reg + educ.fem + educ.mal ~ cohort_15,
        value.var = 'prev'
      ) %>%
        setnames( c( 'reg', 'educ.fem', 'educ.mal', 'prevc1', 'prevc2', 'prevc3' ) ),
      dcast(
        mergedat.c15,
        reg + educ.fem + educ.mal ~ cohort_15,
        value.var = 'CFR'
      ) %>%
        setnames( c( 'reg', 'educ.fem', 'educ.mal', 'cfrc1', 'cfrc2', 'cfrc3' ) ),
      by = c( 'reg', 'educ.fem', 'educ.mal' ) 
    )
  
  # compute standardized cfr by educ pairing prevalence in the cohort (R) 
  # and the standirdized prevalence by cfr distribution (I)
  
  dcastdat.c15[,
               `:=`(
                 R1 = cfrc1  * ( prevc1 / 100 + prevc2 / 100 ) / 2,
                 R2 = cfrc2  * ( prevc1 / 100 + prevc2 / 100 ) / 2,
                 I1 = prevc1 / 100 * ( cfrc1 + cfrc2 ) / 2,
                 I2 = prevc2 / 100 * ( cfrc1 + cfrc2 ) / 2,
                 R3 = cfrc2  * ( prevc2 / 100 + prevc3 / 100 ) / 2,
                 R4 = cfrc3  * ( prevc2 / 100 + prevc3 / 100 ) / 2,
                 I3 = prevc2 / 100 * ( cfrc2 + cfrc3 ) / 2,
                 I4 = prevc3 / 100 * ( cfrc2 + cfrc3 ) / 2
                 )
               ]
  
  dcastdat.c15[,
               list(
                 diff.c1c2    = (sum(R1)-sum(R2)+sum(I1)-sum(I2)),
                 preveff.c1c2 = (sum(I1)-sum(I2)) / (sum(R1)-sum(R2)+sum(I1)-sum(I2)),
                 cfreff.c1c2  = (sum(R1)-sum(R2)) / (sum(R1)-sum(R2)+sum(I1)-sum(I2)),
                 
                 diff.c2c3    = (sum(R2)-sum(R3)+sum(I2)-sum(I3)),
                 preveff.c2c3 = (sum(I2)-sum(I3)) / (sum(R2)-sum(R3)+sum(I2)-sum(I3)),
                 cfreff.c2c3  = (sum(R2)-sum(R3)) / (sum(R2)-sum(R3)+sum(I2)-sum(I3))
               ),
               .(reg)
               ]
  
  brazil.decomp <- 
    dcastdat.c15[reg=='Brazil'] %>%
    copy %>%
    .[,`:=`(
      cfreff.c1c3 = round(100*(R1 - R3)/ (sum(R1)-sum(R3)+sum(I1)-sum(I3)),2),
      preveff.c1c3 = round(100*(I1 - I3)/ (sum(R1)-sum(R3)+sum(I1)-sum(I3)),2)
    )]
##################################################################

### 5. CFR plots #-----------------------------------------------
  
  # 5.1 Prepare data
  cfrdatc5.plot <- 
    cfrpair.c5 %>%
    copy %>%
    .[,
      `:=`(
        educ.fem = ifelse ( educ.fem == 1,
                            "Female: Less Than Primary",
                            ifelse ( educ.fem == 2, "Female: Primary",
                                     ifelse ( educ.fem == 3,
                                              "Female: Secondary",
                                              "Female: Tertiary" 
                                              )
                                     )
                            ),
        educ.mal = ifelse ( educ.mal == 1,
                            "Less Than Primary",
                            ifelse ( educ.mal == 2,
                                     "Primary",
                                     ifelse ( educ.mal == 3,
                                              "Secondary",
                                              "Tertiary"
                                              )
                                     )
                            ),
        cohort_5 = as.numeric( cohort_5)
        )
      ]

  cfr.plot <- 
    function( dat.c5 ){
      ggplot(
        data = dat.c5 %>% copy, 
        aes( 
          x = cohort_5,
          y = CFR,
          color    = as.factor( educ.mal ),
          linetype = as.factor( educ.mal ),
          group    = as.factor( educ.mal )
          )
        ) +
        geom_line(
          size = 1.5
        ) +
        facet_grid( 
          reg ~ educ.fem 
          ) + 
        labs( 
          # title = paste0( "Cohort Fertility Rates by Educational Pairing of Couples - "), 
          # subtitle = "Women Born in 1925-1969, five-year interval",
          # caption = "Source: IPUMS-International",
          x = "\nFemale Cohort Year of Birth", 
          y = "Cohort Fertility Rates\n", 
          color = ""
          ) +
        scale_color_manual( 
          labels = c( "Less than Primary", "Primary", "Secondary", "Tertiary" ), 
          values = c( "black", "gray35", "gray55", "gray75" ),
          name = "Male Educational Attainment Level" 
          ) +
        scale_linetype_manual( 
          labels = c( "Less than Primary", "Primary", "Secondary", "Tertiary" ), 
          values = c( "solid", "longdash", "dashed", "dotted" ),
          name = "Male Educational Attainment Level" 
        ) +
        theme_bw() +
        theme(
          legend.position  = "bottom",
          axis.text.y      = element_text( size = 15, color = 'black'  ),
          axis.text.x      = element_text( size = 15, angle = 90, vjust = 0.5, color = 'black' ),
          axis.title       = element_text( size = 16 ),
          plot.title       = element_text( size = 22 ),
          plot.caption     = element_text( size = 16 ),
          plot.subtitle    = element_text( size = 28 ),
          legend.title     = element_text( size = 16 ),
          legend.text      = element_text( size = 15 ),
          strip.text.x     = element_text( size = 15 ),
          strip.text.y     = element_text( size = 14 ),
          panel.grid.major = element_line( colour = "gray80", size = 0.25, linetype = 'dashed' ),
          panel.grid.minor = element_line( colour = "gray90", size = 0.15, linetype = 'dotted'  ),
          panel.spacing    = unit( 2, "lines" ),
          plot.margin      = margin( 1, 1, 0.5, 1, "cm" ),
          legend.direction = 'vertical'
          ) +
        scale_y_continuous( 
          breaks = seq( 1, 15, 1 ),
          limits = c( 1, 9 )
          ) +
        scale_x_continuous( 
          breaks = seq( 1925, 1965, 5 ),
          limits = c( 1925, 1965 )
        ) 
    }
  
  x11(width = 15, height = 10)
  cfr.plot(cfrdatc5.plot)
  cfr.plot(cfrdatc5.plot, 'North-Northeast')
  cfr.plot(cfrdatc5.plot, 'South-Southeast')
  cfr.plot(cfrdatc5.plot, 'Brazil')
  
##################################################################
  
