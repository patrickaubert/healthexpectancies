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
#' * 'cstPctDLE' : share of in-disability life expectancy in total life-expectancy constant
#'
#' With the hypotheses on life expectancies, prevalences are obtained by inverting the Sullivan method:
#' given the projected mortality, the prevalence at each age is the one that gives the targeted
#' life expectancy at this age and at the following age. A hypothesis may be impossible to satisfy,
#' for example if it would require a negative prevalence at some ages. Argument 'on_unfeasible' sets what
#' to do in this case.
#'
#' @param tabref  a data frame for the reference year, containing variables: year, age, qx (mortality rate) and pix (prevalences) (+ optional: sex)
#' @param tabproj a data frame for projection years, containing variables: year, age, and qx (mortality rate) (+optional: sex)
#' @param hypo projection hypotheses ('cstDFLE','cstDLE','cstPreval','cstPctDFLE','cstPctDLE')
#' @param includevars a vector of names of variables that must be included in the output table (otherwise, only pix, ex, DFLEx, DLEx and pctDFLEx are kept)
#' @param on_unfeasible what to do if some prevalences are outside the interval 0 to 1: "warn" (default) returns the
#' results as they are with a warning; "clip" bounds prevalences to the interval 0 to 1 and recalculates life expectancies
#' accordingly (so that the table remains consistent), with a warning; "error" stops.
#'
#' @return a data frame with prevalences, DFLE, DLE and share of DFLE in total LE, by year and age (and optionnally sex)
#' @export
prevalenceForecast <- function(tabref, tabproj, hypo, includevars = c(), on_unfeasible = c("warn","clip","error") ) {

  on_unfeasible <- match.arg(on_unfeasible)
  tabref <- as.data.frame(tabref)
  tabproj <- as.data.frame(tabproj)

  # == controls

  targets <- c(cstDFLE = "DFLEx", cstDLE = "DLEx", cstPctDFLE = "pctDFLEx", cstPctDLE = "pctDLEx", cstPreval = "pix")
  if ((length(hypo) != 1) || !(hypo %in% names(targets))) {
    stop(paste0("'hypo' must be one of: ", paste0("'",names(targets),"'", collapse = ", "), "."), call. = FALSE)
  }
  missref <- setdiff(c("year","age","qx","pix"), names(tabref))
  if (NROW(missref) > 0) {
    stop(paste0("Missing variable(s) in 'tabref': ", paste0("'",missref,"'", collapse = ", "), "."), call. = FALSE)
  }
  missproj <- setdiff(c("year","age","qx"), names(tabproj))
  if (NROW(missproj) > 0) {
    stop(paste0("Missing variable(s) in 'tabproj': ", paste0("'",missproj,"'", collapse = ", "), "."), call. = FALSE)
  }
  if (NROW(unique(tabref$year)) != 1) {
    stop("'tabref' must contain a single reference year.", call. = FALSE)
  }

  vardim <- intersect( c("sex","age","categ") , intersect(names(tabref), names(tabproj)) )
  varkeep <- targets[[hypo]]

  # == DFLE, DLE and pctDFLE for reference year
  tabDFLEref <- CompleteDFLEtable(tabref)

  # == forecasted prevalences
  tabproj <- tabproj[ , setdiff(names(tabproj), c("pix", varkeep)), drop = FALSE ] %>%
    filter(age %in% c( unique(tabref$age))) %>%
    left_join(tabDFLEref[,c(vardim,varkeep)] , by = c(vardim) )
  tabDFLEproj <- CompleteDFLEtable( tabproj )

  # == prevalences outside [0,1] : the hypothesis cannot be satisfied with the projected mortality
  tol <- 1e-9
  unfeasible <- !is.na(tabDFLEproj$pix) & ((tabDFLEproj$pix < -tol) | (tabDFLEproj$pix > 1 + tol))
  if (any(unfeasible)) {
    msg <- paste0("With hypothesis '",hypo,"' and the projected mortality, ",sum(unfeasible),
                  " prevalence(s) are outside the interval [0,1] (from ",round(min(tabDFLEproj$pix[unfeasible]),3),
                  " to ",round(max(tabDFLEproj$pix[unfeasible]),3),"), for ",NROW(unique(tabDFLEproj$year[unfeasible])),
                  " year(s), at ages ",min(tabDFLEproj$age[unfeasible])," to ",max(tabDFLEproj$age[unfeasible]),".")
    if (on_unfeasible == "error") { stop(msg, call. = FALSE) }
    if (on_unfeasible == "clip") {
      warning(paste0(msg," Prevalences have been bounded to [0,1]."), call. = FALSE)
      tabDFLEproj$pix <- pmin(pmax(tabDFLEproj$pix, 0), 1)
      tabDFLEproj <- CompleteDFLEtable( tabDFLEproj[ , c(vardim,"year","qx","pix")] )
    } else {
      warning(msg, call. = FALSE)
    }
  }

  rbind( tabDFLEref[,c(vardim,"year","pix","ex","DFLEx","DLEx","pctDFLEx",c(includevars))],
         tabDFLEproj[,c(vardim,"year","pix","ex","DFLEx","DLEx","pctDFLEx",c(includevars))] )
}
