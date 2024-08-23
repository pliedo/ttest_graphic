pacman::p_load(gridExtra,broom)

x1 <- rnorm(10, 8)
x2 <- rnorm(10, 10)


t_test_graphic <- function(x1,x2) {
  
  t_test <- t.test(x1, x2)
  tidy_ttest <- tidy(t_test)
  
  output_table <- tidy_ttest%>%select(estimate1,
                                      estimate2,
                                      p.value,
                                      estimate,
                                      conf.low,
                                      conf.high)%>%
    rename(!!paste(deparse(substitute(x1)), "mean") := estimate1,
           !!paste(deparse(substitute(x2)), "mean") := estimate2)
  
  
  output_table <-round(output_table,2)
  
  output_table <- output_table%>%
    mutate(conf.interval=paste("(",conf.low,",",conf.high,")"))%>%
    select(-conf.low,-conf.high)
     
  
  
  
  
  
  grob_table <- tableGrob(output_table,rows = NULL)
  
  grob_table$heights <- rep(unit(1, "null"), nrow(grob_table))
  grob_table$widths <- rep(unit(1, "null"), ncol(grob_table))
  
  grid.newpage()
  grid.draw(grob_table)
  }

t_test_graphic(x1,x2)
