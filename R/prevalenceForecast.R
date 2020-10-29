#' Forecasts prevalences of disability and DFLEs
#'
#' Given a table for the reference year with mortality rates and prevalences of disability, and
#' a projection table with mortality rates, the function calculates prevalences and disability-free
#' life expectancies.
#'
#' Projection hypotheses can be one of the following:
#' * 'cstDFLE' : disability-free life expectancies constant in all year
#' * 'cstDLE' : in-disability life expectancies constant in all year
#' * 'cstPreval' : prevalences by age (and sex) constant in all year
#' * 'cstPctDFLE' : share of disability-free life expectancy in total life-expectancy constant
#'
#' @param tabref  a data frame for the reference year, containing variables: year, age, qx (mortality rate) and pix (prevalences) (+ optional: sex)
#' @param tabproj a data frame for projection years, containing variables: year, age, and qx (mortality rate) (+optional: sex)
#' @param hypo projection hypotheses ('cstDFLE','cstDLE','cstPreval','cstPctDFLE')
#'
#' @return a data frame with prevalences, DFLE, DLE and share of DFLE in total LE, by year and age (and optionnally sex)
#' @export
#'
#' @examples
prevalenceForecast <- function(tabref, tabproj, hypo) {
  # add : controls

  vardim <- intersect( c("sex","age","categ") , intersect(names(tabref), names(tabproj)) )
  varkeep <- recode(hypo,
                    "cstDFLE" = "DFLEx",
                    "cstDLE" = "DLEx",
                    "cstPctDFLE" = "pctDFLEx",
                    "cstPrev" = "pix")

  # DFLE, DLE and pctDFLE for reference year
  tabDFLEref <- CompleteDFLEtable(tabref)

  # forecasted prevalences
  tabproj <- tabproj %>%
    filter(age %in% c( unique(tabref$age))) %>% ###
    left_join(tabDFLEref[,c(vardim,varkeep)] , by = c(vardim) )
  tabDFLEproj <- CompleteDFLEtable( tabproj )

  rbind( tabDFLEref[,c(vardim,"year","pix","ex","DFLEx","DLEx","pctDFLEx")],
         tabDFLEproj[,c(vardim,"year","pix","ex","DFLEx","DLEx","pctDFLEx")] )
}
