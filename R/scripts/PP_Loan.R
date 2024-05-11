### Function for preprocessing of loan dataset
normalize <- function(x)
{
  return((x- min(x)) /(max(x)-min(x)))
}

pp = function(df){
  df$CCAvg = as.numeric(unlist(df$CCAvg))
  
  df = df %>% 
    mutate_at(vars(Securities.Account, CD.Account, 
                   Online, CreditCard, Family, Education, Personal.Loan), list(as.factor))
  
  df = mltools::one_hot(data.table(df),cols='Family') 
  df = mltools::one_hot(data.table(df),cols='Education')
  
  df=df %>% 
    mutate_at(vars(Age, Experience, Income, CCAvg, Mortgage, ZIP.Code), list(normalize))
  
  return(df)
  
}

