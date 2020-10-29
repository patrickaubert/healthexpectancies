#' Complete a dataframe with useful variables for the calculation of disability-free life expectancies (DFLE)
#'
#' From an input dataset containing some information on age, mortality rates and prevalences
#' of disability (at least 'age', 'qx' or 'mx', and 'pix' must be within the input dataset),
#' the function completes it with values of life expectancies and disability-free life expectancies.
#' If some indispensable variables are missing (eg 'age'), the output is the same as the input dataset.
#' If the input dataset contains mortality rates ('qx') but no prevalence of disabilities ('pix'), only total life-expectancy is calculed.
#'
#' Full headings for the names of variables in the output dataset can be found in the 'description_sullivan' dataframe (in the package).
#'
#' If variables 'sex' and/or 'year' and/or 'categ' (an undefinite category variable that can be defined by the user)
#' are in the input dataset, calculations are performed for each separate values of 'sex', 'year' and/or 'categ'.
#'
#' Calculations follow example 1 in the sullivan manual (June 2007 version).
#' See https://reves.site.ined.fr/en/resources/computation_online/sullivan/ for more explanations.
#'
#'
#' @param tab a dataframe containing some of the variables
#'
#' @return a dataframe with all variables that can be calculated from the input dataframe
#'
#' @export
#'
#' @examples CompleteDFLEtable( sullivan[,c("year","age","mx","qx","pix")] )
#' @examples CompleteDFLEtable( FRInseeMortalityForecast2016[FRInseeMortalityForecast2016$year %in% c(2013,2015,2020,2030),] )
CompleteDFLEtable <- function(tab) {

  # remove columns with missing values
  tab <- tab[,colSums(is.na(tab))==0]

  # if 'sex' and/or 'year' and/or 'categ' are in the input dataset, calculations is made for each values of these 2 variables
  # NB: 'categ' is an undefinite category variable (to be defined by user)
  if ("sex" %in% names(tab)) {
    classsex <- unique(tab$sex)
    #tab <- tab %>% mutate(sex = as.factor(sex))
    tab$sex <- as.factor(tab$sex)
    if (NROW(classsex)>1)  {
      bysex <- function(s){ CompleteDFLEtable(tab[tab$sex == s,]) }
      return( do.call(rbind,lapply(classsex,bysex)) )
    }
  }
  if ("categ" %in% names(tab)) {
    classcateg <- unique(tab$categ)
    #tab <- tab %>% mutate(categ = as.factor(categ))
    tab$categ <- as.factor(tab$categ)
    if (NROW(classcateg)>1)  {
      bycateg <- function(cat){ CompleteDFLEtable(tab[tab$categ == cat,]) }
      return( do.call(rbind,lapply(classcateg,bycateg)) )
    }
  }
  if ("year" %in% names(tab)) {
    classyear <- unique(tab$year)
    #tab <- tab %>% mutate(year = as.numeric(year))
    tab$year <- as.numeric(tab$year)
    if (NROW(classyear)>1)  {
      byyear <- function(y){ CompleteDFLEtable(tab[tab$year == y,]) }
      return( do.call(rbind,lapply(classyear,byyear)) )
    }
  }

  # creates no variable if age is missing in the input dataset
  # NB: in the sullivan manuel, 'x' means 'age'
  if (!("age" %in% names(tab)) & ("x" %in% names(tab))) { tab$age <- tab$x }
  if (!("age" %in% names(tab))) { return(tab) }
  #tab <- tab %>% mutate(age = as.numeric(age))
  tab$age <- as.numeric(tab$age)
  tab <- tab[order(tab$age),]

  # adding agewidth : should be equal to 1 if all ages are in the input dataset
  if (("age" %in% names(tab)) & !("agewidth" %in% names(tab))) {
    tab$agewidth <- c( tail(tab$age,-1), (tail(tab$age,1)+1)) - tab$age
  }

  # adding central death rate (mx), from Mid-year population (Px) and	No. deaths (Dx)
  if (("Px" %in% names(tab)) & ("Dx" %in% names(tab)) & !("mx" %in% names(tab))) {
    tab$mx <- tab$Dx / tab$Px
  }

  # =================== mx <-> qx ====================
  # adding conditional probability of death (qx), from central death rate (mx)
  if (("mx" %in% names(tab)) & !("qx" %in% names(tab))) {
    tab$qx <- tab$mx * tab$agewidth / (1 + 0.5 * tab$agewidth * tab$mx)
  }
  # --- alternative : adding central death rate (mx), from conditional probability of death (qx)
  if (("qx" %in% names(tab)) & !("mx" %in% names(tab))) {
    tab$mx <- tab$qx / tab$agewidth / (1 - 0.5 * tab$qx)
  }

  # =================== qx -> lx ====================

  # adding number of survivors to age x, from conditional probability of death
  if (("qx" %in% names(tab)) & !("lx" %in% names(tab))) {
    tab$lx <- 100000 * cumprod(c(1, 1 - utils::head(tab$qx,-1) ))
  }

  # =================== lx -> Lx ====================

  # adding person years lived at age x, from number of survivors to age x
  if (("lx" %in% names(tab)) & !("Lx" %in% names(tab))) {
    w1 <- c( 0.2, rep(0.5, nrow(tab)-1) )
    tab$Lx <- ( w1 * tab$lx +
              (1 - w1) * c( utils::tail(tab$lx,-1), 0) ) * tab$agewidth
    tab$Lx[nrow(tab)] <- tab$lx[nrow(tab)] / tab$mx[nrow(tab)]
  }

  # =================== Lx -> Tx ====================

  # adding total number of years lived  from x, from person years lived at age x
  if (("Lx" %in% names(tab)) & !("Tx" %in% names(tab))) {
    tab$Tx <- rev( cumsum( rev(tab$Lx) ) )
  }

  # =================== lx, Tx -> ex ====================

  # adding total life expectancy, from total number of years lived and numbers surviving to age x
  if (("lx" %in% names(tab)) & ("Tx" %in% names(tab)) & !("ex" %in% names(tab))) {
    tab$ex <- tab$Tx / tab$lx
  }

  # =================== Lx, pix -> DFLx ====================
  # adding person years lived without disability (DFLx), from person years lived at age x (Lx) and proportion with disability (pix)
  if (("Lx" %in% names(tab)) & ("pix" %in% names(tab)) & !("DFLx" %in% names(tab))) {
    tab$DFLx <- tab$Lx * (1 - tab$pix)
  }

  # =================== DFLx -> DFTx ====================

  # adding total years lived without disability from age x (DFTx), from person years lived without disability (DFLx)
  if (("DFLx" %in% names(tab)) & !("DFTx" %in% names(tab))) {
    tab$DFTx <- rev( cumsum( rev(tab$DFLx) ) )
  }

  # =================== lx, DFTx -> DFLEx ====================

  # adding disability-free life expectancy (DFLEx), from total years lived without disability from age x (DFTx) and numbers surviving to age x
  if (("lx" %in% names(tab)) & ("DFTx" %in% names(tab)) & !("DFLEx" %in% names(tab))) {
    tab$DFLEx <- tab$DFTx / tab$lx
  }

  # =================== DFLEx <-> DFLx ====================
  # adding in-disability life expectancy (DLEx), from life expectancy (ex) and disability-free life expectancy (DFLEx)
  if (("DFLEx" %in% names(tab)) & ("ex" %in% names(tab)) & !("DLEx" %in% names(tab))) {
    tab$DLEx <- tab$ex - tab$DFLEx
  }
  # --- alternative : adding disability-free life expectancy (DFLEx), from life expectancy (ex) and in-disability life expectancy (DLEx)
  if (!("DFLEx" %in% names(tab)) & ("ex" %in% names(tab)) & ("DLEx" %in% names(tab))) {
    tab$DFLEx <- tab$ex - tab$DLEx
  }

  # =================== DFLEx <-> pctDFLEx ====================

  # adding proportion of life spent disability-free (pctDFLEx), from ratio of DFLE and LE at each age x
  if (("DFLEx" %in% names(tab)) & ("ex" %in% names(tab)) & !("pctDFLEx" %in% names(tab))) {
    tab$pctDFLEx <- 100 * tab$DFLEx / tab$ex
  }
  # --- alternative : adding disability-free life expectancy (DFLEx), from proportion of life spent disability-free (pctDFLEx) and life-expectancy at each age x
  if (!("DFLEx" %in% names(tab)) & ("ex" %in% names(tab)) & ("pctDFLEx" %in% names(tab))) {
    tab$DFLEx <- tab$pctDFLEx/100 * tab$ex
    if (!("DLEx" %in% names(tab))) { tab$DLEx <- tab$ex - tab$DFLEx }
  }

  # =================== lx, Lx, DFLEx -> pix (& DFLx, DFTx) ====================

  # --- alternative : adding prevalences (pix), from DFLEx
  if (("lx" %in% names(tab)) & ("Lx" %in% names(tab)) & ("DFLEx" %in% names(tab)) & !("pix" %in% names(tab)) & !("DFLx" %in% names(tab)) & !("DFTx" %in% names(tab))) {
    tab$DFTx <-  tab$DFLEx * tab$lx
    tab$DFLx <- tab$DFTx - c( tail(tab$DFTx,-1) , 0 )
    tab$pix <-  ( 1 - tab$DFLx / tab$Lx )
  }

  # add row names
  # to be done

  # returns enriched dataset
  return(tab)
}
