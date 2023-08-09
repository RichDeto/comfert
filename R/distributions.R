## FINLAND data
# Educational attainment distribution
# library(readxl); library(rlist); library(forecast); library(ggplot2)
#############
# FUNCTIONS #
#############
# Forecast
fcst <- function(series, period, series_years, plot = T){

  y <- log((series)/(1-series))

  dif <- setdiff(period, series_years)
  if(length(dif) > 0){
    f <- which(dif > max(series_years))
    if(length(f) > 0){
      fit <- forecast::ets(y)
      fore <- forecast::forecast(fit, h = length(f))
    }
    b <- which(dif < min(series_years))
    if(length(b) > 0){
      fit <- forecast::ets(rev(y))
      back <- forecast::forecast(fit, h = length(b))
    }
  }
  if(exists("back")){
    back_mean <- exp(back$mean)/(1+exp(back$mean))
    fore_mean <- exp(fore$mean)/(1+exp(fore$mean))
    n_series <- c(rev(back_mean), series, fore_mean)
    rm(back)

  }else{
    fore_mean <- exp(fore$mean)/(1+exp(fore$mean))
    n_series <- c(series, fore_mean)
  }
  if(plot){
    plot(period, n_series, type = "l")
  }
  return(n_series)
}
dbute2 <- function(v1, v2){

  if(v1+v2 > 1){
    off <- (v1+v2)-1
    v1 <- v1-(off*v1)
    v2 <- v2-(off*(1-v1))
  }

  return(c(v1, v2))

}
dbute3 <- function(v1, v2, v3){

  if(v1+v2+v3 > 1){
    off <- (v1+v2+v3)-1
    v1 <- v1-(off*v1)
    v2 <- v2-(off*v2)
    v3 <- v3-(off*v3)
  }

  return(c(v1, v2, v3))

}

# Distributions

#' This function allows you to get education distribution
#' @param input una lista con los 4 dataframe que insume el metodo
#' @param fore ventana para hacer la proyeccion Default c(1900:2080)
#' @param max_obs_cohort ultima cohorte de los observados
#' @param save logic to save Default T
#' @keywords inputs
#' @return data.frame
#' @family inputs
#' @examples
#' .
get_edu_dist <- function(input, fore = c(1870:2050), save = T){
  raw_ed <- input[[1]]
  raw_ed$cohort <- raw_ed$year - raw_ed$age
  max_obs_cohort = max(raw_ed$cohort)
  list_means <- lapply(split(raw_ed, raw_ed$cohort), function(x) apply(x[3:5], 2, mean, na.rm =T))
  list_props <- lapply(list_means, function(x) prop.table(round(x,0)))
  df_props <- as.data.frame(Reduce(rbind, list_props))
  df_props$cohort <- as.numeric(names(list_props))

  # smooth & forecast
  cohorts_obs <- min(fore):max_obs_cohort
  cohorts_fct <- fore

  sm_all <- apply(df_props[df_props$cohort %in% cohorts_obs,1:3],2, smooth.spline, spar = 0.8)

  p <- fcst(series = sm_all$primary$y, series_years = cohorts_obs, period =  cohorts_fct, plot = F)
  s <- fcst(series = sm_all$secondary$y, series_years = cohorts_obs, period =  cohorts_fct, plot = F)
  t <- fcst(series = sm_all$tertiary$y, series_years = cohorts_obs, period =  cohorts_fct, plot = F)

  plot(cohorts_fct, p, type = "l", ylim = c(0,1))
  points(cohorts_obs, df_props[df_props$cohort %in% cohorts_obs,1], col = "red")
  lines(cohorts_fct, s)
  points(cohorts_obs, df_props[df_props$cohort %in% cohorts_obs,2], col = "red")
  lines(cohorts_fct, t)
  points(cohorts_obs, df_props[df_props$cohort %in% cohorts_obs,3], col = "red")

  # correct for sum > 1
  corrected <- mapply(dbute3,p,s,t)

  new_p <- corrected[c(T, F, F)]
  new_s <- corrected[c(F, T, F)]
  new_t <- corrected[c(F, F, T)]

  lines(cohorts_fct, new_p, col = "blue")
  lines(cohorts_fct, new_s, col = "blue")
  lines(cohorts_fct, new_t, col = "blue")

  obs_edu_dist <- data.frame(df_props[df_props$cohort %in% cohorts_obs,1:3], cohorts_obs)
  names(obs_edu_dist) <- c("lowedF", "mededF", "highedF", "year")

  edu_dist <- data.frame(cbind(new_p, new_s, new_t, cohorts_fct))
  edu_dist[1:3][edu_dist[1:3] < 0] <- 0
  edu_dist[1:3][edu_dist[1:3] > 1] <- 1
  names(edu_dist) <- c("lowedF", "mededF", "highedF", "cohort")

  if(save){
    dir <- file.path("data/distributions")
    dir.create(dir, showWarnings = F)
    write.table(obs_edu_dist, file = file.path(dir, "obs_edu_dist.csv"))
    write.table(edu_dist, file = file.path(dir, "edu_dist.csv"))
  }

  return(edu_dist)
}

#' This function allows you to get work distribution
#' @param input una lista con los 4 dataframe que insume el metodo
#' @param fore ventana para hacer la proyeccion Default c(1900:2080)
#' @param save logic to save Default T
#' @keywords inputs
#' @return data.frame
#' @family inputs
#' @examples
#' .
get_work_dist <- function(input, fore = c(1915:2020), max_obs_cohort = 1971, save = T){
  # read in files
  raw_edw <- input[2:4]

  raw_edw_age <- lapply(raw_edw, function(x) {x[x$age %in% 40:55,]})

  edw_cohort <- lapply(raw_edw_age, function(x) {x$cohort <- x[,1] - x[,2]; return(x)})

  edw_binary <- lapply(edw_cohort, function(x) {j <- cbind(x[1:2], x[3]+x[4],x[5]+x[6]+x[7], x[8]); return(j)})

  cnames <- c("year","age","active","inactive", "cohort")
  edw_binary <- lapply(edw_binary, setNames, cnames)

  primary_list <- split(edw_binary[[1]], edw_binary[[1]]$cohort)
  secondary_list <- split(edw_binary[[2]], edw_binary[[2]]$cohort)
  tertiary_list <- split(edw_binary[[3]], edw_binary[[3]]$cohort)

  primary_means <- lapply(primary_list, function(x) apply(x[3:4],2, mean))
  secondary_means <- lapply(secondary_list, function(x) apply(x[3:4],2, mean))
  tertiary_means <- lapply(tertiary_list, function(x) apply(x[3:4],2, mean))

  primary_props <- lapply(primary_means, function(x) prop.table(round(x,0)))
  secondary_props <- lapply(secondary_means, function(x) prop.table(round(x,0)))
  tertiary_props <- lapply(tertiary_means, function(x) prop.table(round(x,0)))

  df_primary <- as.data.frame(Reduce(rbind, primary_props))
  df_secondary <- as.data.frame(Reduce(rbind, secondary_props))
  df_tertiary <- as.data.frame(Reduce(rbind, tertiary_props))

  df_primary$cohort <- as.numeric(names(primary_props))
  df_secondary$cohort <- as.numeric(names(secondary_props))
  df_tertiary$cohort <- as.numeric(names(tertiary_props))


  # smooth & forecast
  cohorts_smt <- min(fore):max_obs_cohort
  cohorts_obs <- min(fore):max_obs_cohort
  cohorts_fct <- fore

  p_active <- smooth.spline(df_primary[df_primary$cohort %in% cohorts_smt, 1], spar = 0.8)
  p_inactive <- smooth.spline(df_primary[df_primary$cohort %in% cohorts_smt, 2], spar = 0.8)
  s_active <- smooth.spline(df_secondary[df_secondary$cohort %in% cohorts_smt, 1], spar = 0.8)
  s_inactive <- smooth.spline(df_secondary[df_secondary$cohort %in% cohorts_smt, 2], spar = 0.8)
  t_active <- smooth.spline(df_tertiary[df_tertiary$cohort %in% cohorts_smt, 1], spar = 0.8)
  t_inactive <- smooth.spline(df_tertiary[df_tertiary$cohort %in% cohorts_smt, 2], spar = 0.8)


  pa <- fcst(series = p_active$y, series_years = cohorts_smt, period =  cohorts_fct, plot = F)
  pi <- fcst(series = p_inactive$y, series_years = cohorts_smt, period =  cohorts_fct, plot = F)
  sa <- fcst(series = s_active$y, series_years = cohorts_smt, period =  cohorts_fct, plot = F)
  si <- fcst(series = s_inactive$y, series_years = cohorts_smt, period =  cohorts_fct, plot = F)
  ta <- fcst(series = t_active$y, series_years = cohorts_smt, period =  cohorts_fct, plot = F)
  ti <- fcst(series = t_inactive$y, series_years = cohorts_smt, period =  cohorts_fct, plot = F)


  plot(cohorts_fct, pi, type = "l", ylim = c(0,1))
  points(cohorts_obs, df_primary[df_primary$cohort %in% cohorts_obs, 2], col = "red")
  lines(cohorts_fct, pa, type = "l", ylim = c(0,1))
  points(cohorts_obs, df_primary[df_primary$cohort %in% cohorts_obs, 1], col = "red")
  lines(cohorts_fct, si, type = "l", ylim = c(0,1))
  points(cohorts_obs, df_secondary[df_secondary$cohort %in% cohorts_obs, 2], col = "red")
  lines(cohorts_fct, sa, type = "l", ylim = c(0,1))
  points(cohorts_obs, df_secondary[df_secondary$cohort %in% cohorts_obs, 1], col = "red")
  lines(cohorts_fct, ti, type = "l", ylim = c(0,1))
  points(cohorts_obs, df_tertiary[df_tertiary$cohort %in% cohorts_obs, 2], col = "red")
  lines(cohorts_fct, ta, type = "l", ylim = c(0,1))
  points(cohorts_obs, df_tertiary[df_tertiary$cohort %in% cohorts_obs, 1], col = "red")

  # correct for sum > 1
  corrected_p <- mapply(dbute2,pa,pi)

  new_pa <- corrected_p[c(T, F)]
  new_pi <- corrected_p[c(F, T)]

  lines(cohorts_fct, new_pa, col= "blue")
  lines(cohorts_fct, new_pi, col= "blue")

  corrected_s <- mapply(dbute2,sa,si)

  new_sa <- corrected_s[c(T, F)]
  new_si <- corrected_s[c(F, T)]

  lines(cohorts_fct, new_sa, col= "blue")
  lines(cohorts_fct, new_si, col= "blue")

  corrected_t <- mapply(dbute2,ta,ti)

  new_ta <- corrected_t[c(T, F)]
  new_ti <- corrected_t[c(F, T)]

  lines(cohorts_fct, new_ta, col= "blue")
  lines(cohorts_fct, new_ti, col= "blue")


  obs_edu_work <- as.data.frame(cbind(df_primary[df_primary$cohort %in% cohorts_obs, 2],
                                      df_primary[df_primary$cohort %in% cohorts_obs, 1],
                                      df_secondary[df_secondary$cohort %in% cohorts_obs, 2],
                                      df_secondary[df_secondary$cohort %in% cohorts_obs, 1],
                                      df_tertiary[df_tertiary$cohort %in% cohorts_obs, 2],
                                      df_tertiary[df_tertiary$cohort %in% cohorts_obs, 1],
                                      cohorts_obs))
  fin_edu_work <- as.data.frame(cbind(new_pi,new_pa,new_si,new_sa,new_ti,new_ta, cohorts_fct))
  dnames <- c("inactiveWLow", "activeWLow", "inactiveWMed", "activeWMed", "inactiveWHigh", "activeWHigh", "cohort")
  colnames(fin_edu_work) <- dnames
  colnames(obs_edu_work) <- dnames


  fin_edu_work[1:6][fin_edu_work[1:6] < 0] <- 0
  fin_edu_work[1:6][fin_edu_work[1:6] > 1] <- 1

  if(save){
    dir <- file.path("data/distributions")
    dir.create(dir,showWarnings = F)
    write.table(obs_edu_work, file = file.path(dir, "obs_edu_work_dist.csv"))
    write.table(fin_edu_work, file = file.path(dir, "edu_work_dist.csv"))
  }

  return(fin_edu_work)

}
# Cummulative disributions

#' This function allows you to get work distribution
#' @param input una lista con los 4 dataframe que insume el metodo
#' @param fore ventana para hacer la proyeccion Default c(1900:2080)
#' @param save logic to save Default T
#' @keywords inputs
#' @return data.frame
#' @family inputs
#' @examples
#' .
cum_edu_dist <- function(save){
  dir <- file.path("data/distributions")
  edu_dist <- read.csv(file.path(dir, "edu_dist.csv"), sep = " ")
  edu_dist <- as.data.frame(cbind(t(apply(edu_dist[,1:3], 1, cumsum)), edu_dist$cohort))
  edu_dist[,3] <- 1
  names(edu_dist)[4] <- "year"
  write.table(edu_dist, file = file.path(dir, "cum_edu_dist"))
  return(cum_edu_dist)
}
cum_edu_work_dist <- function(save){
  dir <- file.path("data/distributions")
  edu_work_dist <- read.csv(file.path(dir, "edu_work_dist.csv"), sep = " ")
  dist_p <- as.data.frame(cbind(t(apply(edu_work_dist[,1:2], 1, cumsum))))
  dist_s <- as.data.frame(cbind(t(apply(edu_work_dist[,3:4], 1, cumsum))))
  dist_t <- as.data.frame(cbind(t(apply(edu_work_dist[,5:6], 1, cumsum))))
  dist <- cbind(dist_p, dist_s, dist_t, edu_work_dist$cohort)
  dist[, c(2,4,6)] <- 1
  names(dist)[7] <- "year"
  write.table(dist, file = file.path(dir, "cum_work_dist"))
  return(dist)
}

# plots
every_nth <- function(x, nth, empty = TRUE, inverse = FALSE) {
  if (!inverse) {
    if(empty) {
      x[1:nth == 1] <- ""
      x
    } else {
      x[1:nth != 1]
    }
  } else {
    if(empty) {
      x[1:nth != 1] <- ""
      x
    } else {
      x[1:nth == 1]
    }
  }
}
plot_dist_edu <- function(dat, x1, x2,y1 = 0 ,y2 = 1, lp, save = F){

  obs_cohorts <- 1870:1980
  dat_m <- merge(dat, dat[dat$cohort %in% obs_cohorts,],
                 by = "cohort", all.x = T)
  dat_m <- dat_m[,c(1,5,6,7,2,3,4)]
  names(dat_m) <- c("Year","Observed_low",
                    "Observed_med",
                    "Observed_high",
                    "Forecasted_low",
                    "Forecasted_med",
                    "Forecasted_high")

  dat_long <- reshape(dat_m, dir = "long", idvar = "Year",
                      names(dat_m)[c(2:ncol(dat_m))], v.names = "vals",
                      timevar = "type", times = names(dat_m)[c(2:ncol(dat_m))])
  dat_long$ntype <- rep(c("Forecasted","Observed"), each = nrow(dat_long)/2)
  dat_long$edu <- rep(1:3, each = nrow(dat_long)/3)

  dat_long$type <- factor(dat_long$type,
                          levels = c("Observed_low",
                                     "Observed_med",
                                     "Observed_high",
                                     "Forecasted_low",
                                     "Forecasted_med",
                                     "Forecasted_high"),
                          labels = c("Primary Education",
                                     "Secondary Education",
                                     "Tertiary Education",
                                     "Forecasted Primary",
                                     "Forecasted Secondary",
                                     "Forecasted Tertiary"))

  lt = rep(c("blank","dashed"), each = 3)
  sp = c(rep(0,3), 0:2)
  sz = rep(c(1,2), each = 3)
  cl = rep("darkorchid4",6)

  length_grid <- 21

  break_x <- data.frame(x1, x2, y = seq(y1, y2, length.out = length_grid))
  break_y <- data.frame(y1, y2, x = seq(x1, x2, length.out = length_grid))
  dx <- data.frame(y=-Inf, yend=-Inf, x=x1, xend=x2)
  dy <- data.frame(x=-Inf, xend=-Inf, y=y1, yend=y2)

  p <- ggplot(dat_long, aes(x = Year,
                            y = vals,
                            group = as.factor(type),
                            colour = as.factor(type),
                            linetype = as.factor(ntype),
                            shape = as.factor(ntype),
                            size = as.factor(ntype))) +
    geom_segment(aes(x = x1, y = y, xend = x2, yend =y),
                 colour = "grey84", lwd = 0.4,
                 data = break_x, inherit.aes = F,
                 alpha = 0.4)+
    geom_segment(aes(x = x, y = y1, xend = x, yend =y2),
                 colour = "grey84", lwd = 0.4,
                 data = break_y,
                 inherit.aes = F,alpha = 0.4) +
    geom_point() +
    geom_line() +
    scale_linetype_manual(values = c("blank", "dashed"),
                          labels = c("Observed", "Forecasted"))+
    scale_shape_manual(values= c(1,NA), labels = c("Observed", "Forecasted")) +
    scale_size_manual(values = c(2.8,0.8),labels = c("Observed", "Forecasted")) +
    theme_bw() +
    scale_colour_manual(values = cl)+
    guides(colour = "none")+
    theme(legend.position = lp,
          legend.title=element_blank()) +
    theme(legend.text = element_text(size = 15,
                                     margin = margin(l = 1, unit = "pt")),
          legend.key.height=unit(1,"line"),
          legend.key.size = unit(1, "cm"),
          legend.key = element_rect(colour = "transparent", fill = NA),
          legend.background=element_blank())+
    theme(axis.text = element_text(size = 12),
          axis.title=element_text(size=12))+
    theme(panel.border = element_blank(),
          panel.grid = element_blank()) +
    theme(plot.background = element_rect(fill = "transparent",colour = NA),
          panel.background = element_rect(fill = "transparent",colour = NA))+
    annotate(geom="text", x=1990, y=0.07, label="Primary\n Education", color="black", size = 3) +
    annotate(geom="text", x=1949, y=0.575, label="Secondary\n Education", color="black", size = 3)+
    annotate(geom="text", x=2010, y=0.68, label="Tertiary\n Education", color="black", size = 3)+
    list(geom_segment(data=dx, aes(x=x, y=y, xend=xend, yend=yend), inherit.aes=FALSE),
         scale_x_continuous(breaks = round(seq(x1,x2, length.out = length_grid),0),
                            labels = every_nth(round(seq(x1,x2, length.out = length_grid),0), 5, inverse = TRUE)))+
    list(geom_segment(data=dy, aes(x=x, y=y, xend=xend, yend=yend), inherit.aes=FALSE),
         scale_y_continuous(breaks=round(seq(y1,y2,length.out = length_grid),1),
                            labels = every_nth(round(seq(y1,y2,length.out = length_grid),1), 5,
                                               inverse = TRUE)))+
    xlab("Cohort")+
    ylab("Proportion of women")

  if(save){
    pdf(file.path("dists_plots", paste0("fct_obs_edu.pdf")), width=6, height=6)
    print(p)
    dev.off()
  }

  print(p)
}
plot_dist_work <- function(dat, x1, x2,y1 = 0 ,y2 = 1, lp, active_only = F, save = F){

  obs_cohorts <- 1934:1989
  dat_m <- merge(dat, dat[dat$cohort %in% obs_cohorts,],
                 by = "cohort", all.x = T)
  dat_m <- dat_m[,c(1,2,8,3,9,4,10,5,11,6,12,7,13)]
  nms <- c("Year","Forecasted_inactive_low",
           "Observed_inactive_low",
           "Forecasted_active_low",
           "Observed_active_low",
           "Forecasted_inactive_med",
           "Observed_inactive_med",
           "Forecasted_active_med",
           "Observed_active_med",
           "Forecasted_inactive_hi",
           "Observed_inactive_hi",
           "Forecasted_active_hi",
           "Observed_active_hi"
  )
  names(dat_m) <- nms

  lt = rep(c("dashed","blank"), each =  6)
  sp = c(rep(NA,6), 0:6)
  sz = rep(c(1,2), each = 6)
  cl = rep("darkorchid4",12)
  bks <- c("Forecasted_inactive_low",
           "Observed_inactive_low",
           "Observed_active_low",
           "Observed_inactive_med",
           "Observed_active_med",
           "Observed_inactive_hi",
           "Observed_active_hi")
  labs <- c("Forecast",
            "Inactive Primary Education",
            "Active Primary Education",
            "Inactive Secondary Education",
            "Active Secondary Education",
            "Inactive Tertiary Education",
            "Active Tertiary Education"
  )

  if(active_only){
    dat_m <- dat_m[,c(1,4,5,8,9,12,13)]
    names(dat_m) <- nms[c(1,4,5,8,9,12,13)]
    lt <- lt[c(1:3,7:9)]
    sp <- sp[c(1:3,7:9)]
    sz <- sz[c(1:3,7:9)]
    cl <- cl[c(1:3,7:9)]
    bks <- bks[c(1,3,5,7)]
    labs <- labs[c(1,3,5,7)]
  }



  dat_long <- reshape(dat_m, dir = "long", idvar = "year",
                      names(dat_m)[c(2:ncol(dat_m))], v.names = "vals",
                      timevar = "type", times = names(dat_m)[c(2:ncol(dat_m))])
  dat_long$ntype <- rep(c("Forecasted","Observed"), each = nrow(dat_m))
  dat_long$edu <- rep(1:3, each = nrow(dat_long)/3)

  length_grid <- 21

  break_x <- data.frame(x1, x2, y = seq(y1, y2, length.out = length_grid))
  break_y <- data.frame(y1, y2, x = seq(x1, x2, length.out = length_grid))
  dx <- data.frame(y=-Inf, yend=-Inf, x=x1, xend=x2)
  dy <- data.frame(x=-Inf, xend=-Inf, y=y1, yend=y2)


  p <- ggplot(dat_long, aes(x = Year,
                            y = vals,
                            shape = as.factor(ntype),
                            linetype = as.factor(ntype),
                            group = as.factor(type),
                            size = as.factor(ntype),
                            colour = as.factor(type))) +
    geom_segment(aes(x = x1, y = y, xend = x2, yend =y),
                 colour = "grey84", lwd = 0.4,
                 data = break_x, inherit.aes = F,
                 alpha = 0.4)+
    geom_segment(aes(x = x, y = y1, xend = x, yend =y2),
                 colour = "grey84", lwd = 0.4,
                 data = break_y,
                 inherit.aes = F,alpha = 0.4)+
    geom_point() +
    geom_line() +
    scale_linetype_manual(values = c("dashed","blank"),
                          labels = c("Forecasted","Observed"))+
    scale_shape_manual(values= c(NA,1), labels = c("Forecasted","Observed")) +
    scale_size_manual(values = c(0.8, 2.8),labels = c("Forecasted","Observed")) +
    theme_bw() +
    scale_colour_manual(values = cl)+
    guides(colour = "none")+
    theme(legend.position = lp,
          legend.title=element_blank()) +
    theme(legend.text = element_text(size = 15,
                                     margin = margin(l = 1, unit = "pt")),
          legend.key.height=unit(1,"line"),
          legend.key.size = unit(1, "cm"),
          legend.key = element_rect(colour = "transparent", fill = NA),
          legend.background=element_blank())+
    theme(axis.text = element_text(size = 12),
          axis.title=element_text(size=12))+
    theme(panel.border = element_blank(),
          panel.grid = element_blank()) +
    theme(plot.background = element_rect(fill = "transparent",colour = NA),
          panel.background = element_rect(fill = "transparent",colour = NA))+
    annotate(geom="text", x=2001, y=0.57, label="Primary\n Education", color="black", size = 3) +
    annotate(geom="text", x=2001, y=0.83, label="Secondary\n Education", color="black", size = 3)+
    annotate(geom="text", x=2002, y=1.0, label="Tertiary Education", color="black", size = 3)+
    list(geom_segment(data=dx, aes(x=x, y=y, xend=xend, yend=yend), inherit.aes=FALSE),
         scale_x_continuous(breaks = round(seq(x1,x2, length.out = length_grid),0),
                            labels = every_nth(round(seq(x1,x2, length.out = length_grid),0), 5, inverse = TRUE)))+
    list(geom_segment(data=dy, aes(x=x, y=y, xend=xend, yend=yend), inherit.aes=FALSE),
         scale_y_continuous(breaks=round(seq(y1,y2,length.out = length_grid),1),
                            labels = every_nth(round(seq(y1,y2,length.out = length_grid),1), 5,
                                               inverse = TRUE)))+
    xlab("Cohort")+
    ylab("Proportion of women")

  if(save){
    if(active_only){
      pdf(file.path("dists_plots", paste0("fct_obs_wk_ao.pdf")), width=6, height=6)
      print(p)
      dev.off()
    }else{
      pdf(file.path("dists_plots", paste0("fct_obs_wk.pdf")), width=6, height=6)
      print(p)
      dev.off()
    }
  }

  print(p)
}

########################################################################
# ed <- get_edu_dist(input_uy, fore = c(1900:2080),max_obs_cohort = 1977, save = F)
# ced <- cum_edu_dist(save = T)
# wd <- get_work_dist(fore = c(1900:2080),max_obs_cohort = 1979, save = T)
# cum_edu_work_dist(save = T)


# Plots
plot_dist_edu(edu_dist, x1 = 1900, x2 = 2050, lp = c(0.27,0.85), save = F)
plot_dist_work(edu_work_dist,x1 = 1900, x2 = 2050, lp = c(0.21,0.25), active_only = T, save = F)


