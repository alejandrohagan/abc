pacman::p_load(tidyverse,roxygen2)


#this is a test
#and this is a test from github


#` creates a customer segmentation
#`
#` @param df is a dataframe or tibble
#` @param group is the grouping key that you want to aggregate the data by, eg customer, plant, product
#` @param dim is the dimension you want to measure, eg. quantity, margin, etc. This should not be a ratio and should be a positive number
#` @param a is the initial threshold for aggregation and should be expressed as a percentage
#` @param b is the incremental threshold for aggregation and should be expressed as a percentage
#` @param c is the incremental threshold for aggregation and should be expressed as a percentage
#` @export

customer_segmentation <- function(df,group,dim,a=.7,b=.26,c=.04){
  

  
  stopifnot(a+b+c==1L,!is.numeric(dim_name))
  
  
  
  df %>% #dataframe
    group_by({{group}}) %>% 
    
    #creates a bunch of columns
    summarize(  
      across({{ dim }}, #make sure this dimension can be aggregratad, later version will handle ratios
             list(sum=~sum(.,na.rm=TRUE),
                  mean=~mean(.,na.rm=TRUE),
                  n =  ~ n(),
                  median =  ~ median(.,na.rm=TRUE),
                  sd =  ~ sd(.,na.rm=TRUE),
                  mad =  ~ mad(.,na.rm=TRUE), #median absolute deviation
                  aad =  ~ mad(., center =mean(.,na.rm=TRUE),na.rm=TRUE), #average absolute deviation
                  IQR05 = ~quantile(., .05,na.rm=TRUE),
                  IQR25 = ~quantile(., .25,na.rm=TRUE),
                  IQR75 = ~quantile(., .75,na.rm=TRUE),
                  IQR95 = ~quantile(., .95,na.rm=TRUE)
             ),
             .names = "{.fn}") #gives each column their name
    ) %>%
    ungroup() %>% 
    
    arrange(desc(sum)) %>% # assuming positive values, descends highest to lowest (should add some logic to switch this)
    
    mutate(cum_sum=cumsum(sum), #cumlative value, if ratio, need some sort of check - need specify
           prop_total=sum/max(cum_sum), #assumes positive values, need check
           cum_prop_total=cumsum(prop_total), #cumsum percent of total
           cum_unit_prop=row_number()/max(row_number()), #unit percent
           group_classification_by_dim=
             case_when(
               cum_prop_total<=a ~"A",
               cum_prop_total<=(a+b) ~"B",
               TRUE ~ "C"),
           dim_threshold=
             case_when(group_classification_by_dim=="A"~a,
                       group_classification_by_dim=="B"~(a+b),
                       TRUE ~ c)
    ) %>% 
    select(-c(prop_total,cum_sum)) %>% 
    relocate(dim_threshold,group_classification_by_dim,cum_prop_total,cum_unit_prop)
  
}

diamonds %>% customer_segmentation(group = clarity,dim=x)

roxygenise()


install.packages("usethis")
usethis::use_git_config(user="alejandrohagan")
git config --global user.email "alejandro.hagan@exxonmobil.com"
git config --global user.name "alejandrohagan"
use_git_config(user.name = "alejandrohagan", user.email = "alejandro.hagan@exxonmobil.com")
