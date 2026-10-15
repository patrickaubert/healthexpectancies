#' Projects prevalences of disability with an estimated model
#'
#' Calculates prevalences by age and year from a model estimated with \code{\link{fitPrevalenceModel}}.
#'
#' With method "trend", \code{newdata} only needs the years and ages. With method "mortality", \code{newdata} must also contain the
#' mortality rates 'qx' for all the ages of the model and for each year (they are used at ages \eqn{a+k}, see \code{\link{fitPrevalenceModel}}).
#' Variables defining series (sex, categories) can be omitted from \code{newdata}: it is then used for all the values
#' of these variables in the model (for example, mortality rates by sex apply to all categories).
#' Rows of \code{newdata} with ages that are not in the model are dropped.
#'
#' Prediction intervals (with \code{interval = TRUE}) are calculated on the scale of the estimation, from the
#' residual variance of the regression, and then transformed back to prevalences. They do not take into account
#' the uncertainty on the projected mortality rates, nor the uncertainty on the validity of the model outside the period of estimation.
#' They are not available for estimator = "endpoints".
#'
#' @param object a model estimated with \code{\link{fitPrevalenceModel}}
#' @param newdata a data frame with variables 'year', 'age' (+ 'qx' for method "mortality") and, optionally, the variables defining series
#' @param interval if TRUE, adds the lower and upper bounds of a prediction interval ('pix_lwr' and 'pix_upr')
#' @param level confidence level of the prediction interval
#' @param on_unfeasible method "target" only: what to do if the hypothesis cannot be satisfied with the projected mortality, i.e. if it requires
#' prevalences outside the interval 0 to 1 (all ages) or if no uniform evolution can reach the target (minimum age). "warn" (default) returns the values as
#' they are with a warning (prevalences outside the interval 0 to 1, or NA for a uniform evolution without solution); "clip" bounds prevalences to the
#' interval 0 to 1 (or to the closest feasible solution), with a warning; "error" stops.
#' @param ... not used
#'
#' @return the rows of \code{newdata} with a new variable 'pix' (projected prevalence), and 'pix_lwr' and 'pix_upr' if requested.
#' If the scale of the model is "log" or "level", prevalences are bounded to the interval 0 to 1 (with a warning).
#' @seealso \code{\link{fitPrevalenceModel}}, \code{\link{projectPrevalence}}
#' @examples
#' ages <- 60:100; years <- 2010:2019
#' obs <- expand.grid(age = ages, year = years)
#' obs$pix <- plogis(-6 + 0.12 * (obs$age - 60) - (0.01 + 0.0005 * (obs$age - 60)) * (obs$year - 2019))
#' m <- fitPrevalenceModel(obs, method = "trend")
#' proj <- predict(m, newdata = expand.grid(age = ages, year = c(2025, 2030)), interval = TRUE)
#' head(proj)
#' @export
predict.prevalence_model <- function(object, newdata, interval = FALSE, level = 0.95, on_unfeasible = c("warn","clip","error"), ...) {

  newdata <- as.data.frame(newdata)
  on_unfeasible <- match.arg(on_unfeasible)
  method <- object$method; st <- object$settings; gv <- st$groupvars
  gfun <- if (method == "target") NULL else pm_transform(st$scale)

  # == controls
  need <- c("year","age"); if (method %in% c("mortality","target")) { need <- c(need, "qx") }
  miss <- setdiff(need, names(newdata))
  if (NROW(miss) > 0) { stop(paste0("Missing variable(s) in 'newdata': ", paste0("'",miss,"'", collapse = ", "), "."), call. = FALSE) }
  if (anyNA(newdata[ , need])) { stop(paste0("Missing values (NA) are not allowed in ", paste0("'",need,"'", collapse = ", "), "."), call. = FALSE) }
  newdata$year <- pm_as_numeric(newdata$year, "year"); newdata$age <- pm_as_numeric(newdata$age, "age")
  if (method %in% c("mortality","target") && any(newdata$qx <= 0)) { stop("Mortality rates 'qx' must be strictly positive.", call. = FALSE) }
  if (!is.numeric(level) || level <= 0 || level >= 1) { stop("'level' must be between 0 and 1.", call. = FALSE) }

  # == variables defining series : missing ones are expanded over all their values in the model
  skeys <- if (NROW(gv) > 0) do.call(rbind, lapply(object$series, function(s) s$group)) else NULL
  missgv <- setdiff(gv, names(newdata))
  if (NROW(missgv) > 0) {
    newdata <- merge(newdata, unique(skeys[ , missgv, drop = FALSE]), by = NULL)
  }
  newdata$.row <- seq_len(nrow(newdata))
  nkeys <- if (NROW(gv) > 0) do.call(paste, c(newdata[gv], sep = " / ")) else rep("all data", nrow(newdata))
  unknown <- setdiff(unique(nkeys), sapply(object$series, function(s) s$key))
  if (NROW(unknown) > 0) {
    stop(paste0("Series not found in the model: ", paste0("'",unknown,"'", collapse = ", "), "."), call. = FALSE)
  }

  # == extrapolation far beyond the period of estimation (method "trend")
  if (method == "trend") {
    span <- diff(range(st$fit_years)); horizon <- max(newdata$year) - st$origin
    if (span > 0 && horizon > 2 * span) {
      warning(paste0("Extrapolation over ",horizon," years beyond the last year of the estimation, from a period of ",span," years."), call. = FALSE)
    }
  }
  if (interval && method == "trend" && st$estimator == "endpoints") {
    warning("Prediction intervals are not available with estimator = \"endpoints\".", call. = FALSE)
  }
  if (interval && method == "target") {
    warning("Prediction intervals are not available with method = \"target\".", call. = FALSE)
    interval <- FALSE
  }

  # == series by series
  ndropped <- 0
  out <- lapply(object$series, function(s) {
    rows <- newdata[nkeys == s$key, , drop = FALSE]
    det <- s$details
    known <- rows$age %in% det$ages
    ndropped <<- ndropped + sum(!known)
    rows <- rows[known, , drop = FALSE]
    if (nrow(rows) == 0) { return(NULL) }
    if (anyDuplicated(rows[ , c("age","year")]) > 0) {
      stop(paste0("Series '",s$key,"': 'newdata' contains several rows for the same age and year."), call. = FALSE)
    }
    idx <- match(rows$age, det$ages)
    tcrit_df <- NA_real_

    if (method == "target") {
      years_s <- sort(unique(rows$year))
      Qmat <- sapply(years_s, function(y) {
        sub <- which(rows$year == y)
        if (!setequal(rows$age[sub], det$ages)) {
          stop(paste0("Series '",s$key,"', year ",y,": 'newdata' must contain mortality rates for all ages of the model (",
                      min(det$ages)," to ",max(det$ages),")."), call. = FALSE)
        }
        rows$qx[sub][match(det$ages, rows$age[sub])]
      })
      Qmat <- matrix(Qmat, nrow = NROW(det$ages))
      pr <- pm_project_target(det, Qmat, years_s)
      pos <- cbind(idx, match(rows$year, years_s))
      rows$pix <- pr$pix[pos]; rows$.clip <- pr$clipped[pos]; rows$.status <- pr$status[match(rows$year, years_s)]
      return(rows)
    }

    if (method == "trend") {
      tt <- rows$year - det$origin
      yhat <- det$intercept[idx] + det$slope[idx] * tt
      if (interval && det$estimator == "ols") {
        var_pred <- det$sigma2[idx] * (1 + 1 / NROW(det$years) + (tt - det$tbar)^2 / det$Sxx); tcrit_df <- det$df
      } else { var_pred <- rep(NA_real_, nrow(rows)) }
    } else {
      x <- rep(NA_real_, nrow(rows))
      for (y in unique(rows$year)) {
        sub <- which(rows$year == y); ordr <- sub[order(rows$age[sub])]
        if (!setequal(rows$age[sub], det$ages)) {
          stop(paste0("Series '",s$key,"', year ",y,": 'newdata' must contain mortality rates for all ages of the model (",
                      min(det$ages)," to ",max(det$ages),")."), call. = FALSE)
        }
        x[ordr] <- pm_lnq_shifted(log(rows$qx[ordr]), rows$age[ordr], det$age_lag)
      }
      yhat <- det$alpha[idx] + det$beta[idx] * x
      if (interval) {
        if (det$slope == "common") { var_pred <- det$sigma2 * (1 + 1 / NROW(det$years) + (x - det$xbar[idx])^2 / det$Sxx)
        } else { var_pred <- det$sigma2[idx] * (1 + 1 / NROW(det$years) + (x - det$xbar[idx])^2 / pmax(det$Sxx_a[idx], 1e-12)) }
        tcrit_df <- det$df
      } else { var_pred <- rep(NA_real_, nrow(rows)) }
    }

    rows$pix <- gfun$inv(yhat)
    if (interval) {
      half <- stats::qt((1 + level) / 2, df = tcrit_df) * sqrt(var_pred)
      rows$pix_lwr <- gfun$inv(yhat - half); rows$pix_upr <- gfun$inv(yhat + half)
    }
    rows
  })
  res <- do.call(rbind, out)
  if (is.null(res)) { stop("None of the ages of 'newdata' is in the model.", call. = FALSE) }
  if (ndropped > 0) { message(ndropped," row(s) of 'newdata' with ages that are not in the model were dropped.") }
  res <- res[order(res$.row), , drop = FALSE]; res$.row <- NULL; rownames(res) <- NULL

  # == method "target" : hypothesis that cannot be satisfied
  if (method == "target") {
    nosol <- res$.status != "ok"
    outside <- !nosol & !is.na(res$pix) & (res$pix < -1e-9 | res$pix > 1 + 1e-9)
    if (any(nosol) | any(outside)) {
      msg <- c(if (any(nosol)) paste0("no uniform evolution of prevalences reaches the target for ",NROW(unique(res$year[nosol]))," year(s) (",
                                      paste(utils::head(sort(unique(res$year[nosol])), 5), collapse = ", "),")"),
               if (any(outside)) paste0(sum(outside)," prevalence(s) outside the interval 0 to 1 (from ",round(min(res$pix[outside]), 3)," to ",
                                        round(max(res$pix[outside]), 3),") for ",NROW(unique(res$year[outside]))," year(s), at ages ",
                                        min(res$age[outside])," to ",max(res$age[outside])))
      msg <- paste0("With this hypothesis and the projected mortality: ", paste(msg, collapse = " ; "), ".")
      if (on_unfeasible == "error") { stop(msg, call. = FALSE) }
      if (on_unfeasible == "clip") {
        warning(paste0(msg," Values have been bounded."), call. = FALSE)
        res$pix <- ifelse(nosol, res$.clip, res$pix); res$pix <- pmin(pmax(res$pix, 0), 1)
      } else { warning(msg, call. = FALSE) }
    }
    res$.clip <- NULL; res$.status <- NULL
    return(res)
  }

  # == prevalences must remain between 0 and 1
  pcols <- intersect(c("pix","pix_lwr","pix_upr"), names(res))
  outside <- sapply(pcols, function(v) sum(!is.na(res[[v]]) & (res[[v]] < 0 | res[[v]] > 1)))
  if (sum(outside) > 0) {
    warning(paste0(sum(outside)," projected value(s) outside the interval 0 to 1 have been bounded (scale = \"",st$scale,"\")."), call. = FALSE)
    for (v in pcols) { res[[v]] <- pmin(pmax(res[[v]], 0), 1) }
  }
  res
}
