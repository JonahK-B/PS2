##Test vote info
votes <- c(0:100)

##Q1


##count the lead digits

countleads = function(votes){
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
  return(count)
}

##Leemis' m statistic

Leemis.m = function(votes){
 countleads(votes)
  m.list = NULL
  for(i in 1:9){
    m.list <- c(m.list, (countleads(votes)[i]/length(votes))-log10(1+1/i))
  }
  m <- max(m.list) 
  return(m)
}
Leemis.m(votes)
##Cho-Gains' d statistic
CG.d = function(votes){
  countleads(votes)
  d.list = NULL
  for(i in 1:9){
    d.list <- c(d.list, ((countleads(votes)[i]/length(votes))-log10(1+1/i))^2)
  }
  d.near <- sum(d.list)
  d <- sqrt(d.near)
  return(d)
}
CG.d(votes)


##Coalesces all other functions together/answer to part one
Bias.stats = function(votes, get.m = TRUE, get.d = TRUE){
  m <- Leemis.m(votes)
  d <- CG.d(votes)
  count <- countleads(votes)
  if(get.m == TRUE & get.d == TRUE){
    my_list <- list("count" = count, "m" = m, "d" = d)
    return(my_list)
  }
  if(get.m == TRUE & get.d == FALSE){
    my_list <- list("count" = count, "m" = m)
    return(my_list)
  }
  if(get.m == FALSE & get.d == TRUE){
    my_list <- list("count" = count, "d" = d)
    return(my_list)
  }
}
Bias.stats(votes)

##Q2


## Functions to assign *s indicating significance to m and d



m.sig = function(m){
  if (m < .851) {
    return ("")
  } else {
    if (m < .967) {
      return ("*")
    } else {
      if (m < 1.212) {
        return ("**")
      } else {
        if (1.212 <= m) {
          return ("***")
        }
      }
    }
  }
}

m.star <- m.sig

d <- CG.d(votes)

d.sig = function(d){
  if (d < 1.212) {
    return ("")
  } else {
    if (d < 1.330) {
      return ("*")
    } else {
      if (d < 1.596) {
        return ("**")
      } else {
        if (1.596 <= d) {
          return ("***")
        }
      }
    }
  }
}


print.benfords = function(votes){
  ### Assigns values and significances
  m <- Leemis.m(votes)
  m.star <- m.sig
  d <- CG.d(votes)
  d.star <- d.sig
  
  ### Coalesces above into a nice table
  benfords.table <- rbind(c(m, m.star(m)), c(d, d.star(d)))
  
  ### Names the rows of the table
  rownames(benfords.table) <- c("Leemis' m:","Cho-gaines' d:")
  colnames(benfords.table) <- c("Values", "Significance")
  
  ### Displays final table
  print(benfords.table)
  ### Displays key to asterisks
  cat("A blank in the significance column indicates that we fail to reject the null hypothesis of no fraud at the 10% confidence level, * indicates rejecting the null at 10%, ** indicates rejecting the null at 5%, *** indicates rejecting the null at 1%")
}
## running the function
print.benfords(votes)

##Creating CSV export
Benfords.CSV <- function(votes){
  ## creating a file to write into
  sink(file = "Benfords_data.csv")
  print.benfords(votes)
  ## closing the file
  sink()
}
## Running the function
Benfords.CSV(votes)
