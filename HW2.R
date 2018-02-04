##Leemis' m statistic
votes <- c(rep(c(1,2,3), 5000))


Leemis.m = function(votes){
  leads = NULL
  leads <- c(leads, substr(votes,1,1))
  count = c(rep.int(0,9))
  for(i in 1:9){
    for(j in 1:length(leads)){
      if (i == leads[j]){
        count[i] <- count[i]+1
      }
    }
  }
  m.list = NULL
  for(i in 1:9){
    m.list <- c(m.list, (count[i]/length(votes))-log10(1+1/i))
  }
  m <- max(m.list) 
  return(m)
}

##Cho-Gains' d statistic
CG.d = function(votes){
  leads = NULL
  leads <- c(leads, substr(votes,1,1))
  count = c(rep.int(0,9))
  for(i in 1:9){
    for(j in 1:length(leads)){
      if (i == leads[j]){
        count[i] <- count[i]+1
      }
    }
  }
  d.list = NULL
  for(i in 1:9){
    d.list <- c(d.list, ((count[i]/length(votes))-log10(1+1/i))^2)
  }
  d.near <- sum(d.list)
  d <- sqrt(d.near)
  return(d)
}

## display whether null hypothesis can be rejcted as well as the level of confidence
bias.detector = function(m, d){
  if(m < .851| d < 1.212){
    print.default ("Cannot refute null hypothesis")
  }
  if(m < .967| d < 1.330){
    print.default ("Confident in refuting null hypothesis with a 10% confidence interval")
  }
  if(m < 1.212| d < 1.596){
    print.default ("Confident in refuting null hypothesis with a 5% confidence interval")
  }
  if(1.212 <= m| 1.596 <= d){
    print.default ("Confident in refuting null hypothesis with a 1% confidence interval")
  }
}

##Coalesces all other functions together
Bias.stats = function(votes, get.m = TRUE, get.d = TRUE){
  m <- Leemis.m(votes)
  d <- CG.d(votes)
  bias.detector(m, d)
  if(get.m == TRUE & get.d == TRUE){
    return(as.list(c(m, d)))
  }
  if(get.m == TRUE & get.d == FALSE){
    return(as.list(c(m, 0)))
  }
  if(get.m == FALSE & get.d == TRUE){
    return(as.list(c(0, d)))
  }
 
}

Bias.stats(votes)
