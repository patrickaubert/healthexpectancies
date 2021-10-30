#' Estimates prevalences by age from values by age brackets
#'
#' Given a vector of prevalences by age brackets and the vector of age cuts (which defines the age brackets),
#' the function returns a vector of prevalences at all ages.
#' The calculation minimises the sum of squares of second-differences of prevalences
#' by age, under the constraint that average prevalences by age brackets
#' (weight according to the 'weight' vector, usually the vector of population size at each age)
#' are equal to the 'prevalence' input vector.
#'
#' Note : Second-differences rather than first-differences are used
#' in the minimisation function, since prevalences according to age are usually parabolic.
#'
#' @param prevalence a vector with observed prevalences by age bracket
#' @param agecuts a vector with age defining the age brackets (minimum age in each age bracket)
#' @param agemin minimum age in the output vector
#' @param agemax maximum age in the output vector
#' @param weight a vector of weights for each age
#'
#' @return a vector with prevalences according to polynomial approximation
#'
#' @export prevalenceApprox
#'
#' @examples prevalenceApprox(prevalence = (FRDreesAPA2017 %>% filter(sex=="female",typepresta=="APA à domicile"))$prevalence, agecuts=c(seq(60,95,5)), agemin=60, agemax=100, weight=(FRInseePopulation %>% filter(sex=="female",year==2018,age0101>=60) %>% arrange(age0101))$popx)
#' @examples prevalenceApprox(prevalence = (FRDreesAPA %>% filter(year==2018,sex=="male",typepresta=="APA à domicile"))$prevalence, agecuts=c(seq(60,90,5)), agemin=60, agemax=100, weight=(FRInseePopulation %>% filter(sex=="male",year==2019,age0101>=60) %>% arrange(age0101))$popx)
prevalenceApprox <- function (prevalence, agecuts, agemin, agemax, weight = rep(1,(agemax-agemin+1)) ) {

  #prevalence <- (FRDreesAPA2017 %>% filter(sex=="female",typepresta=="APA à domicile"))$prevalence
  #agecuts <- c(seq(60,95,5))
  #agemin <- 60
  #agemax <- 100
  #weight <- (FRInseePopulation %>% filter(sex=="female",year==2018,age0101>=60) %>% arrange(age0101))$popx

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
