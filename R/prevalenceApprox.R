#' Estimates prevalences by age from values by age brackets
#'
#' Given a vector of prevalences by age brackets and the vector of age cuts (which defines the age brackets),
#' the function returns a vector of prevalences at all ages.
#' The calculation minimises the sum of squares of second-differences of prevalences
#' by age, under the constraint that average prevalences by age brackets
#' (weight according to the 'weights' vector, usually the vector of population size at each age)
#' are equal to the 'prevalence' input vector.
#'
#' 'prevalence' and 'weight' can also be given as dataframes containing sex, year and/or categories variables. In this case,
#' the output is a dataframe and prevalences are smoothed for all sex, years and categories.
#'
#' If 'categories' is provided (a vector of variable names in 'prevalence' and 'weights'), smoothing is performed for
#' each separate values of category variables.
#'
#' Note : Second-differences rather than first-differences are used
#' in the minimisation function, since prevalences according to age are usually parabolic.
#'
#' @param prevalence a vector with observed prevalences by age bracket
#' @param agecuts a vector with age defining the age brackets (minimum age in each age bracket)
#' @param agemin minimum age in the output vector
#' @param agemax maximum age in the output vector
#' @param weight a vector of weights for each age
#' @param categories a vector of names of variables of the 'prevalence' dataframe, representing categories (optional)
#'
#' @return a vector with prevalences according to polynomial approximation
#'
#' @export prevalenceApprox
#'
#' @examples prevalenceApprox(prevalence = (FRDreesAPA2017 %>% filter(sex=="female",typepresta=="APA à domicile"))$prevalence, agecuts=c(seq(60,95,5)), agemin=60, agemax=99, weight=(FRInseePopulation %>% filter(sex=="F",year==2018,age0101>=60,geo=="france") %>% arrange(age0101))$popx)
#' @examples prevalenceApprox(prevalence = (FRDreesAPA %>% filter(year==2018,sex=="male",typepresta=="APA à domicile"))$prevalence, agecuts=c(seq(60,90,5)), agemin=60, agemax=99, weight=(FRInseePopulation %>% filter(sex=="M",year==2019,age0101>=60,geo=="france") %>% arrange(age0101))$popx)
#' @examples prevalenceApprox(prevalence = FRDreesAPA2017, agecuts=c(seq(60,95,5)), agemin=60, agemax=99, weight=(FRInseePopulation %>% filter(year==2018,age0101>=60,geo=="france") %>% mutate(sex=recode(sex,"F"="female","M"="male")) %>% rename(weight=popx) %>% arrange(sex,age0101)), categories = c("typepresta"))
prevalenceApprox <- function (prevalence, agecuts, agemin, agemax, weight = rep(1,(agemax-agemin+1)), categories = c("") ) {

  #prevalence <- (FRDreesAPA2017 %>% filter(sex=="female",typepresta=="APA à domicile"))$prevalence
  #agecuts <- c(seq(60,95,5))
  #agemin <- 60
  #agemax <- 100
  #weight <- (FRInseePopulation %>% filter(sex=="female",year==2018,age0101>=60) %>% arrange(age0101))$popx

  #if (!(is.vector(prevalence) == is.vector(weight))) { stop("Error: 'prevalence' and 'weight' should be of the same type, either vector or data.frame.") }

  if (is.data.frame(prevalence) & is.data.frame(weight)) {

  # == if sex, year or categories variables have been provided, perform function on each sex and category

    # impose data.Frame format
    prevalence <- as.data.frame(prevalence)
    weight <- as.data.frame(weight)

    # find variations of sex/year names (typically French names instead of English names)
    namesex <- NA
    if (("sex" %in% tolower(names(prevalence))) | ("sexe" %in% tolower(names(prevalence)))) {
      namesex <- names(prevalence)[tolower(names(prevalence)) %in% c("sex","sexe")]
      namesex <- namesex[1]
      names(prevalence)[names(prevalence)==namesex] <- "sex"
      if (!(namesex %in% names(weight))) { stop(paste0("Error: variable '",namesex,"' should be in the '",weight,"' data.frame.")) }
      names(weight)[names(weight)==namesex] <- "sex"
    } else {
      prevalence <- prevalence %>% mutate(sex = "_")
      weight <- weight %>% mutate(sex = "_")
    }
    nameyear <- NA
    if (("year" %in% tolower(names(prevalence))) | (NROW(names(prevalence)[grepl("^(an|annee)$",tolower(names(prevalence)))]>=1))) {
      nameyear <- names(prevalence)[grepl("^(year|an|annee)$",tolower(names(prevalence)))]
      nameyear <- nameyear[1]
      names(prevalence)[names(prevalence)==nameyear] <- "year"
      if (!(nameyear %in% names(weight))) { stop(paste0("Error: variable '",nameyear,"' should be in the '",weight,"' data.frame.")) }
      names(weight)[names(weight)==nameyear] <- "year"
    } else {
      prevalence <- prevalence %>% mutate(year = 9999)
      weight <- weight %>% mutate(year = 9999)
    }

    # if 'categories' is provided, a 'categ' variable is first created
    categories <- categories[!(categories %in% c("sex","year"))]
    categories.true <- categories[categories != ""]

    if (NROW(categories.true)>=1) {

      # if 'categories' is provided and a 'categ' variable is in the 'tab' dataframe, the latter is ignored
      if ("categ" %in% names(prevalence)) {
        if (NROW(categories)==1) {
          warning(paste0("The 'categ' variable in the 'prevalence' dataframe will be ignored. ",
                         categories," variable is used instead."  ))
        } else  if (NROW(categories)>1) {
          warning(paste0("The 'categ' variable in the 'prevalence' dataframe will be ignored. ",
                         paste(categories,collapse = ", ")," variables are used instead."  ))
        }
        prevalence <- prevalence[ , names(prevalence)[!(names(prevalence) == "categ")]]
      }

      for (k in 1:NROW(categories.true)) {
        prevalence[,categories[k]] <- paste0("#",k,prevalence[,categories[k]],"#")
        }
      prevalence <- prevalence %>% unite(categ,all_of(categories),sep="")

    } else {
      prevalence <- prevalence %>% mutate(categ = "")
    }

    # runs function on each disinct values of sex, year and categ variables

    sepval <- prevalence %>% select(sex,year,categ) %>% distinct(sex,year,categ)

    tabout <- lapply(
      1:nrow(sepval),
      function(i){
        data.frame(
          age = c(agemin:agemax),
          sex = sepval$sex[i],
          year = sepval$year[i],
          categ = sepval$categ[i],
          prevalence = prevalenceApprox(
            prevalence = (prevalence %>% filter(sex==sepval$sex[i] & year==sepval$year[i] & categ==sepval$categ[i]))$prevalence,
            agecuts = agecuts,
            agemin = agemin,
            agemax = agemax,
            weight = (weight %>% filter(sex==sepval$sex[i] & year==sepval$year[i]) )$weight
          ),
          stringsAsFactors = FALSE
        )
      }
    )

    tabout <- do.call("bind_rows",tabout)

  # restore (or suppress)  sex, year and categ variables with their initial names

  if (is.na(namesex)) { tabout <- tabout %>% select(-sex)
  } else { names(tabout)[names(tabout)=="sex"] <- namesex  }

  if (is.na(nameyear)) { tabout <- tabout %>% select(-year)
  } else { names(tabout)[names(tabout)=="year"] <- nameyear  }

  if (!(NROW(categories.true)>=1)) { tabout <- tabout %>% select(-categ)
  } else {
    for (j in c(1:NROW(categories))) {
      tabout[,categories[j]] <- str_extract(tabout$categ,paste0("(?<=#",j,")[^#]*(?=#)"))
    }
    tabout <- tabout[ , names(tabout)[!(names(tabout) == "categ")]]
  }

  return(tabout)

  # == function in case data have been provided for only one sex and category

  } else {

  if (NROW(weight) != (agemax-agemin+1)) { stop("Error: Size of input vectors weight not compatible with agemin and agemax") }
  if (NROW(agecuts) != NROW(prevalence)) { stop("Error: Different size of input vectors agecuts and prevalence") }

  # create matrixes

  weightstab <- data.frame(
    weights = weight,
    agebracket = cut(c(agemin:agemax), breaks = c(agecuts,Inf), include.lowest = TRUE, right = FALSE),
    stringsAsFactors = FALSE
    ) %>%
    group_by(agebracket) %>% mutate(weights = weights/sum(weights)) %>% ungroup()
  weights <- weightstab$weights

  agebr <- c(agecuts,Inf)
  wbr <- function(a,c) ifelse((agemin+a-1>=agebr[c])&(agemin+a-1<agebr[c+1]),weights[a],0)
  wprev <- outer(1:(agemax-agemin+1), 1:NROW(prevalence),wbr)

  agr <- (outer(1:(agemax-agemin+1), 1:(agemax-agemin+1),function(i,j){ifelse(i<=j,1,0)}))

  # Minimise the sum of first differences
  #wprevagr1 <- agr %*% wprev
  # Minimise the sum of second differences
  wprevagr <- (agr %*% agr) %*% wprev

  # exact solution of minimisation problem under linear constraint

  # *** Minimise the sum of first differences
  #deltaprev <- wprevagr1 %*%  matlib::inv(t(wprevagr1) %*% wprevagr1) %*% prevalence
  #prevapprox1 <- cumsum(deltaprev)
  # *** Minimise the sum of second differences
  delta2prev <- wprevagr %*%  matlib::inv(t(wprevagr) %*% wprevagr) %*% prevalence
  deltaprev <- cumsum(delta2prev)
  prevapprox <- cumsum(deltaprev)

  return(prevapprox)

  # verifications
  # t(deltaprev) %*% wprevagr
  #comp <- weightstab %>%
  #  mutate(age = c(agemin:agemax),
  #         prevapprox1 = prevapprox1,
  #         prevapprox = prevapprox) %>%
  #  left_join(data.frame(prev = prevalence, agebracket = unique(weightstab$agebracket)),
  #            by="agebracket")
  #g <- ggplot(comp,aes(x=age))+geom_line(aes(y=prev),colour="red")+geom_line(aes(y=prevapprox),colour="blue")+geom_line(aes(y=prevapprox1),colour="green")
  #return(g)

  #return(cumsum(deltaprev))

  }
}
