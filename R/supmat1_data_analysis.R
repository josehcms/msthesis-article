##################################################################
### Paper title: Fertility and educational pairings: 
###              an analysis through cohort fertility
### Supplementary material 2 - data analysis
##################################################################

### 1. Load packages #--------------------------------------------
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

  match.dat[, reg := ifelse( reg == 'Central-West', 'Midwest', reg ) ]
  
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
  
  dic_educ <- c( '1' = 'LP', '2' = 'P', '3' = 'S', '4' = 'T' )
  
  tab1 <- 
    dcastdat.c15[ , 
                  list(
                    reg, 
                    educ.fem = dic_educ[ paste0( educ.fem ) ],
                    educ.mal = dic_educ[ paste0( educ.mal ) ],
                    prevc1 = format( prevc1, nsmall = 2 ), 
                    prevc2 = format( prevc2, nsmall = 2 ),
                    prevc3 = format( prevc3, nsmall = 2 ),
                    cfrc1  = format( round( cfrc1, 2 ), nsmall = 2 ),
                    cfrc2  = format( round( cfrc2, 2 ), nsmall = 2 ),
                    cfrc3  = format( round( cfrc3, 2 ), nsmall = 2 ) 
                  )]
  
  write.csv2( tab1, 'FIGS/prevcfr_tab_data.csv', row.names = F )
 
  # prepare tables for document
  preptab_docx <- function( tab1 ){
    
    require(flextable)
    require(officer)
    
    ft_tab1 <- flextable( tab1[,2:9] )
    
    ft_tab1 <- set_header_labels( ft_tab1, 
                                  educ.fem = "Education",
                                  educ.mal = "Education", 
                                  prevc1 = "Prevalence (%)",
                                  prevc2 = "Prevalence (%)" , 
                                  prevc3 = "Prevalence (%)",
                                  cfrc1 = "CFR" , 
                                  cfrc2 = "CFR",
                                  cfrc3 = "CFR" )
    
    ft_tab1 <- 
      merge_at( ft_tab1, 
                i = 1, 
                j = 1:2, 
                part = "header" )
    ft_tab1 <- 
      merge_at( ft_tab1, 
                i = 1, 
                j = 3:5, 
                part = "header" )
    
    ft_tab1 <- 
      merge_at( ft_tab1, 
                i = 1, 
                j = 6:8, 
                part = "header" )
    
    ft_tab1 <- 
      add_header_row( ft_tab1, 
                      values = c("Female", "Male", 
                                 "1925-1939", "1940-1954", 
                                 "1955-1969", "1925-1939", 
                                 "1940-1954", "1955-1969" ), 
                      top = FALSE )
    
    ft_tab1 <- 
      align( ft_tab1, align = "center", part = "all" )
    
    ft_tab1 <- 
      hline( ft_tab1, 
             i = 2,
             border = fp_border( color = 'black' ),
             part = 'header' )
    
    return(ft_tab1)
    
  }
  
  tab1_br <- preptab_docx( tab1[ reg == 'Brazil' ] )
  tab2_co <- preptab_docx( tab1[ reg == 'Midwest' ] )
  tab3_nne <- preptab_docx( tab1[ reg == 'North-Northeast' ] )
  tab4_sse <- preptab_docx( tab1[ reg == 'South-Southeast' ] )
  
  save( tab1_br, tab2_co, tab3_nne, tab4_sse,
        file = 'FIGS/tables1-4.RData')
  
  cfr_decomp <- function( CFR1, P1, CFR2, P2 ){
    
    # overall CFR is a function of CFR_i and P_i: F = f(CFR_i,P_i)
    # compute overall CFR for times 1 (F1) and 2 (F2)
    P1 = P1 / 100
    P2 = P2 / 100
    
    F1 <- sum( CFR1 * P1 )
    F2 <- sum( CFR2 * P2 )
    deltaF1F2 <- round( F1 - F2, 2 )
    
    # compute P standardized values of functions for times 1 and 2
    F1_Pstd <- ( sum( CFR1 * P1 ) + sum( CFR1 * P2 ) ) / 2
    F2_Pstd <- ( sum( CFR2 * P1 ) + sum( CFR2 * P2 ) ) / 2
    
    # compute CFR standardized values of functions for times 1 and 2
    F1_CFRstd <- ( sum( CFR1 * P1 ) + sum( CFR2 * P1 ) ) / 2
    F2_CFRstd <- ( sum( CFR1 * P2 ) + sum( CFR2 * P2 ) ) / 2
    
    # compute effects
    CFR_effect <- round( F1_Pstd - F2_Pstd, 2 )
    P_effect <- round( F1_CFRstd - F2_CFRstd, 2 )
    
    tab <- 
      data.table(
        effect = c( 'Overall difference', 'CFR effect', 'Educational pairing composition effect' ),
        diff   = c( paste0( deltaF1F2, ' (', format( round( 100 * ( deltaF1F2 / deltaF1F2 ) , 2 ), nsmall = 2 ), '%)' ),
                    paste0( CFR_effect, ' (', format( round( 100 * ( CFR_effect / deltaF1F2 ) , 2 ), nsmall = 2 ), '%)'  ), 
                    paste0( P_effect, ' (', format( round( 100 * ( P_effect / deltaF1F2 ) , 2 ), nsmall = 2 ), '%)' ) )
      )
    
    return( tab )
    
  }
  
  preptab <- function( data, regname ){
    
    CFR1 = data[ reg == regname ]$cfrc1
    CFR2 = data[ reg == regname ]$cfrc2
    CFR3 = data[ reg == regname ]$cfrc3
    
    P1 = data[ reg == regname ]$prevc1
    P2 = data[ reg == regname ]$prevc2
    P3 = data[ reg == regname ]$prevc3
    
    # overall CFRs
    OCFR1 = format( round( sum( CFR1 * P1 / 100 ), 2 ), nsmall = 2 )
    OCFR2 = format( round( sum( CFR2 * P2 / 100 ), 2 ), nsmall = 2 )
    OCFR3 = format( round( sum( CFR3 * P3 / 100 ), 2 ), nsmall = 2 )
    
    tab <- 
      rbind(
        data.table(
          effect = c('','',''),
          diff   = c(OCFR1,OCFR2,OCFR3),
          info   = c('CFR(1925-1939)','CFR(1940-1954)','CFR(1955-1969)')
        ),
        cfr_decomp( CFR1 = CFR1, CFR2 = CFR2, P1 = P1, P2 = P2 ) %>%
          .[, info := c( 'CFR(1925-1939) - CFR(1940-1954)', 
                         'CFR effect1', 
                         'Pairing composition effect1' ) ],
        cfr_decomp( CFR1 = CFR1, CFR2 = CFR3, P1 = P1, P2 = P3 ) %>%
          .[, info := c( 'CFR(1925-1939) - CFR(1955-1969)', 
                         'CFR effect2', 
                         'Pairing composition effect2' ) ],
        cfr_decomp( CFR1 = CFR2, CFR2 = CFR3, P1 = P2, P2 = P3 ) %>%
          .[, info := c( 'CFR(1940-54) - CFR(1955-69)', 
                         'CFR effect3', 
                         'Pairing composition effect3' ) ] ) %>%
      .[, .( info, diff, reg = regname ) ]
    
    return(tab)
  }
  
  
  tab2 <- 
    rbind(
      preptab( data = dcastdat.c15, regname = 'Brazil' ),
      preptab( data = dcastdat.c15, regname = 'Midwest' ),
      preptab( data = dcastdat.c15, regname = 'North-Northeast' ),
      preptab( data = dcastdat.c15, regname = 'South-Southeast' ) 
    ) %>% 
    dcast( info ~ reg, value.var = 'diff' ) %>%
    .[, info := factor( info,
                        levels = c( "CFR(1925-1939)", "CFR(1940-1954)", "CFR(1955-1969)",
                                    "CFR(1925-1939) - CFR(1940-1954)", "CFR effect1", "Pairing composition effect1",
                                    "CFR(1925-1939) - CFR(1955-1969)", "CFR effect2", "Pairing composition effect2",
                                    "CFR(1940-1954) - CFR(1955-1969)", "CFR effect3", "Pairing composition effect3" ) )] %>%
    setorder( info )
  
  tab2[ grepl( 'CFR effect', info ), 
        info := 'CFR Effect' ]
  tab2[ grepl( 'Pairing composition', info ), 
        info := 'Pairing Composition Effect' ]
  
  tab5_cfrcomp <- 
    flextable( tab2 ) %>%
    set_header_labels( info = '' ) %>%
    align( j = 2:5, align = "right", part = "body" ) %>%
    align( i = c( 5, 6, 8, 9, 11, 12 ), 
           j = 1, 
           align = "right", part = "body" ) %>%
    align( i = c( 1, 2, 3, 4, 7, 10 ), 
           j = 1, 
           align = "left", part = "body" ) %>%
    align( align = 'center', part = 'header' ) %>%
    hline( i = c( 1, 2, 3, 4, 6, 7, 9, 10 ),
           border = fp_border( color = 'black' ),
           part = 'body' ) %>%
    bold( part = 'header' ) %>%
    bold( i = c( 1, 2, 3, 4, 7, 10 ),
          j = 1,
          part = 'body' )


  save( tab5_cfrcomp, file = 'FIGS/table5.RData' )
  
  write.csv2( tab2, 'FIGS/decomp_tab_data.csv', row.names = F )
  
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
                                              "Female: Secondary/Tertiary",
                                              "Female: Tertiary" 
                                              )
                                     )
                            ),
        educ.mal = ifelse ( educ.mal == 1,
                            "Less Than Primary",
                            ifelse ( educ.mal == 2,
                                     "Primary",
                                     ifelse ( educ.mal == 3,
                                              "Secondary/Tertiary",
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
        geom_hline(
          yintercept = 2.1,
          size = 0.5,
          color = 'forestgreen',
          linetype = 'dashed'
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
          labels = c( "Less than Primary", "Primary", "Secondary/Tertiary", "Tertiary" ), 
          values = c( "black", "gray35", "tomato3", "navyblue" ),
          name = "Male Educational\nAttainment Level" 
          ) +
        scale_linetype_manual( 
          labels = c( "Less than Primary", "Primary", "Secondary/Tertiary", "Tertiary" ), 
          values = c( "solid", "longdash", "dashed", "dotted" ),
          name = "Male Educational\nAttainment Level" 
        ) +
        theme_bw() +
        theme(
          legend.position  = "top",
          axis.text.y      = element_text( size = 14, color = 'black'  ),
          axis.text.x      = element_text( size = 14, angle = 90, vjust = 0.5, color = 'black' ),
          axis.title       = element_text( size = 16, color = 'black', face = 'bold' ),
          plot.title       = element_text( size = 22, color = 'black'  ),
          plot.caption     = element_text( size = 16, color = 'black'  ),
          plot.subtitle    = element_text( size = 28, color = 'black'  ),
          legend.title     = element_text( size = 15, color = 'black', face = 'bold' ),
          legend.text      = element_text( size = 14, color = 'black'  ),
          strip.text       = element_text( size = 14, color = 'black', face = 'bold'   ),
          strip.background = element_rect( fill = 'gray95', color = 'gray30' ),
          panel.grid.major = element_line( colour = "gray80", size = 0.15, linetype = 'dashed' ),
          panel.grid.minor = element_line( colour = "gray90", size = 0.05, linetype = 'dashed'  ),
          legend.direction = 'horizontal',
          legend.background = element_rect( color = 'black', fill = 'transparent', size = 0.21 )
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
  
  # 5.2 save figure and plot data of CFR trends by ed pairing
  cfr.plot( cfrdatc5.plot )
  write.csv2( cfrdatc5.plot, 'FIGS/plot_cfr_5year_cohort_data.csv', row.names = F )
  ggsave( 'FIGS/plot_cfr_5year_cohort.png', dpi = 300, width = 9, height = 9 )
  
  # 5.3 prepare data for CFR diff
  rel_diffplot <- 
    dcast( cfrdatc5.plot[!is.na(CFR)],
           reg+cohort_5+educ.fem~educ.mal, value.var = 'CFR')

  rel_diffplot[, rel_diff_LPP := ( `Less Than Primary` - Primary ) / Primary ]
  rel_diffplot[, rel_diff_SP := ( Secondary - Primary ) / Primary ]
  
  rel_diffplot <- 
    rel_diffplot %>%
    melt(
      id.vars = c('reg','cohort_5','educ.fem'),
      measure.vars = c('rel_diff_LPP','rel_diff_SP'),
      value.name = 'rel_diff',
      variable.name = 'rel_diff_type'
    )
  
  rel_diffplot[ , rel_diff_type := ifelse( rel_diff_type == 'rel_diff_LPP',
                                          'Male Relative Difference\nLess than Primary vs Primary',
                                          'Male Relative Difference\nSecondary vs Primary' ) ]
  rel_diffplot[ , educ.fem := ifelse( educ.fem == 'Female: Less Than Primary',
                                      'Female;\nLess Than Primary', educ.fem )]
  cfr_reldiff.plot <- 
    function( dat.c5 ){
      ggplot(
        data = dat.c5 %>% copy, 
        aes( 
          x = cohort_5,
          y = rel_diff,
          color    = as.factor( reg ),
          linetype = as.factor( reg ),
          group    = as.factor( reg )
        )
      ) +
        geom_line(
          size = 1.5
        ) +
        geom_hline(
          yintercept = 0,
          size = 0.5,
          color = 'forestgreen',
          linetype = 'dashed'
        ) +
        facet_grid( 
          educ.fem ~ rel_diff_type 
        ) + 
        labs( 
          # title = paste0( "Cohort Fertility Rates by Educational Pairing of Couples - "), 
          # subtitle = "Women Born in 1925-1969, five-year interval",
          # caption = "Source: IPUMS-International",
          x = "\nFemale Cohort Year of Birth", 
          y = "CFR relative difference\n(for male educational attainment level)", 
          color = ""
        ) +
        scale_color_manual( 
          labels = c( "Brazil", "Midwest", "North-Northeast", "South-Southeast" ), 
          values = c( "black", "gray55", "tomato2", "steelblue4" ),
          name = "" 
        ) +
        scale_linetype_manual( 
          labels = c( "Brazil", "Midwest", "North-Northeast", "South-Southeast" ), 
          values = c( "solid", "longdash", "dashed", "dotted" ),
          name = "" 
        ) +
        theme_bw() +
        theme(
          legend.position  = "top",
          axis.text.y      = element_text( size = 14, color = 'black'  ),
          axis.text.x      = element_text( size = 14, angle = 90, vjust = 0.5, color = 'black' ),
          axis.title       = element_text( size = 16, color = 'black', face = 'bold' ),
          plot.title       = element_text( size = 22, color = 'black'  ),
          plot.caption     = element_text( size = 16, color = 'black'  ),
          plot.subtitle    = element_text( size = 28, color = 'black'  ),
          legend.title     = element_text( size = 15, color = 'black', face = 'bold' ),
          legend.text      = element_text( size = 14, color = 'black'  ),
          strip.text       = element_text( size = 14, color = 'black', face = 'bold'   ),
          strip.background = element_rect( fill = 'gray95', color = 'gray30' ),
          panel.grid.major = element_line( colour = "gray80", size = 0.15, linetype = 'dashed' ),
          panel.grid.minor = element_line( colour = "gray90", size = 0.05, linetype = 'dashed'  ),
          legend.direction = 'horizontal',
          legend.background = element_rect( color = 'black', fill = 'transparent', size = 0.21 )
        ) +
        scale_y_continuous( 
          breaks = seq( - 0.80, 0.80, 0.10 ),
          limits = c( - 0.20, 0.60 )
        ) +
        scale_x_continuous( 
          breaks = seq( 1925, 1965, 5 ),
          limits = c( 1925, 1965 )
        ) 
    }
  
  # 5.4 rel_diff plot
  cfr_reldiff.plot( rel_diffplot )
  write.csv2( rel_diffplot, 'FIGS/plot_reldiffcfr_5year_cohort_data.csv', row.names = F )
  ggsave( 'FIGS/plot_reldiffcfr_5year_cohort.png', dpi = 300, width = 9, height = 9 )
  
##################################################################
  
