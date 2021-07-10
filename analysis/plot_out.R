plot_out <- function(res_dir, country, iniY, endY, nsim, ysd = 1960, asfr = T, tfr = T, ccf = T,
                     ccf_edu = T, ccf_edu_obs = T, mab = T, mabs = T, unplanned = T, unwanted = T,
                     desired = T, gap = T, gap_edu = T, css = T, save = F) {
  
  out_path <- file.path("..","data",country,"out")
  save_path <- "../../latex/plots/"
  res_names <- sapply(res_dir, function(x) {list.files(x, "RData", full.names = TRUE)})
  tyears <- length(iniY:(endY-1))
  long_dat <- function(sim, obs, nsim, ysd, iniY, endY){
    
    sim_frame <- as.data.frame(do.call("cbind", sim))
    sim_frame$Year <- iniY:endY
    sim_frame$mean <- apply(sim_frame[1:nsim], 1, mean)
    sim_dat <- sim_frame[,c(nsim+1, 1:nsim, nsim+2)]
    
    if(length(obs)>1){
      
      sim_obs <- merge(sim_dat, obs, by = "Year", all = T)
      sim_obs_s <- sim_obs[sim_obs$Year >= ysd,]
      names(sim_obs_s) <- c("year", rep("Simulated", nsim),
                            "Mean of simulations", "Observed") 
      
      for(i in 2:(nsim+1)){
        colnames(sim_obs_s)[i] <- paste0(names(sim_obs_s)[i],"_", i-1)
      }
      
      dat_long <- reshape(sim_obs_s, dir = "long", idvar = "year",
                          names(sim_obs_s)[c(2:ncol(sim_obs_s))], v.names = "vals",
                          timevar = "type", times = names(sim_obs_s)[c(2:ncol(sim_obs_s))])
      dat_long$mean <- ifelse(dat_long$type %in% c("Mean of simulations", "Observed"), 1, 0)
      
    }else{
      
      sim_dat_s <- sim_dat[sim_dat$Year >= ysd,]
      names(sim_dat_s) <- c("year", rep("Simulated", nsim),
                            "Mean of simulations") 
      
      for(i in 2:(nsim+1)){
        colnames(sim_dat_s)[i] <- paste0(names(sim_dat_s)[i],"_", i-1)
      }
      dat_long <- reshape(sim_dat_s, dir = "long", idvar = "Year",
                          names(sim_dat_s)[c(2:ncol(sim_dat_s))], v.names = "vals",
                          timevar = "type", times = names(sim_dat_s)[c(2:ncol(sim_dat_s))])
      dat_long$mean <- ifelse(dat_long$type %in% c("Mean of simulations"), 1, 0)
    }
    
    dat_long$mean <- ifelse(dat_long$type == "Mean of simulations", 2, dat_long$mean )
    dat_long$r_type <- ifelse(dat_long$type %in% c("Mean of simulations", "Simulated"), 0, 1)
    dat_long$r_type <- ifelse(dat_long$type == "Mean of simulations", 2, dat_long$r_type)
    
    return(dat_long)
  } 
  p_obs_sim <- function(dat, ylim, xlim){
   
   lbs <- c("Simulated", "Observed", "Mean of simulations")
   p <-  ggplot(dat,
           aes(x = year,
               y = vals,
               group = as.factor(type),
               size = as.factor(mean),
               linetype = as.factor(mean),
               shape = as.factor(mean),
               colour = as.factor(mean))) +
      geom_line() +
      geom_point() +
      xlim(xlim) +
      ylim(ylim) +
      scale_colour_manual(values = c("grey65", "grey0", "grey65"),
                          labels = lbs)+
      scale_shape_manual(values = c(NA,16,16), labels = lbs)+
      scale_linetype_manual( values = c("dotted","blank","blank"), labels = lbs)+
      scale_size_manual( values = c(0.5, 2.5, 2.5), labels = lbs)+
      guides(shape = guide_legend(override.aes = list(size=c(1, 3.5, 3.5))))+
      theme_bw() +
      theme(legend.position = c(0.7,0.8),
            legend.title=element_blank()) +
      theme(legend.text = element_text(size = 13,
                                       margin = margin(l = 1, unit = "pt")),
            legend.key.height=unit(3,"point"),
            legend.key.size = unit(1, "cm"),
            legend.key = element_rect(colour = "transparent", fill = NA),
            legend.background=element_blank())+
      theme(axis.text = element_text(size = 12))+
      theme(plot.background = element_rect(fill = "transparent",colour = NA),
            panel.background = element_rect(fill = "transparent",colour = NA))+
     theme(plot.margin = unit(c(1,1,1,1), "cm"))
   return(p)
    
  }
  p_sim <- function(dat, ylim, xlim){
    
    lbs <- c("Simulated", "Mean of simulations")
    ggplot(dat,
           aes(x = year,
               y = vals,
               group = as.factor(type),
               size = as.factor(mean),
               linetype = as.factor(mean),
               shape = as.factor(mean),
               colour = as.factor(mean))) +
      geom_line() +
      geom_point() +
      xlim(xlim) +
      ylim(ylim) +
      scale_colour_manual(values = c("grey65", "grey65"),
                          labels = lbs)+
      scale_shape_manual( values = c(NA,16), labels = lbs)+
      scale_linetype_manual(values = c("dotted","blank"), labels = lbs)+
      scale_size_manual( values = c(0.5, 2.5), labels = lbs)+
      guides(shape = guide_legend(override.aes = list(size=c(1.2, 5))))+
      theme_bw() +
      theme(legend.position = c(0.7,0.2),
            legend.title=element_blank()) +
      theme(legend.text = element_text(size = 10,
                                       margin = margin(l = 1, unit = "pt")),
            legend.key.height=unit(5,"point"),
            legend.key.size = unit(1.2, "cm"),
            legend.key = element_rect(colour = "transparent", fill = NA),
            legend.background=element_blank())+
      theme(axis.text = element_text(size = 20),
            axis.title = element_blank())+
      theme(plot.background = element_rect(fill = "transparent",colour = NA),
            panel.background = element_rect(fill = "transparent",colour = NA))
    
  }
  p_sim_cats <- function(dat, ylim, xlim, lbs, lbs_cats, leg_pos, cats){
    
    ggplot(dat,
           aes(x = year,
               y = vals,
               group = interaction(cat, mean, type),
               size = as.factor(mean),
               linetype = as.factor(mean),
               shape = as.factor(cat),
               colour = as.factor(cat))) +
      geom_line() +
      geom_point() +
      xlim(xlim) +
      ylim(ylim) +
      scale_colour_manual(values = c("grey65", rep("black",3)),
                          labels = lbs_cats)+
      scale_shape_manual(values = c(16,15,17,18), labels = lbs_cats)+
      scale_linetype_manual(values = c("dotdash","blank"), labels = lbs)+
      scale_size_manual(values = c(0.5, 2.5), labels = lbs)+
      guides(linetype = FALSE,
             size = FALSE,
             colour = FALSE,
             shape = guide_legend(override.aes = list(colour = c("grey65", rep("black",cats-1)),
                                                      size=5)))+
      theme_bw() +
      theme(legend.position = leg_pos,
            legend.title=element_blank()) +
      theme(legend.text = element_text(size = 20,
                                       margin = margin(l = 1, unit = "pt")),
            legend.key.height=unit(5,"point"),
            legend.key.size = unit(1.2, "cm"),
            legend.key = element_rect(colour = "transparent", fill = NA),
            legend.background=element_blank())+
      theme(axis.text = element_text(size = 20),
            axis.title = element_blank())+
      theme(plot.background = element_rect(fill = "transparent",colour = NA),
            panel.background = element_rect(fill = "transparent",colour = NA))
    
  }
  p_sim_obs_cats <- function(dat, ylim, xlim, lbs, lbs_cats, leg_pos){
    
    ggplot(dat,
           aes(x = year,
               y = vals,
               group = interaction(cat, mean, type),
               size = as.factor(mean),
               linetype = as.factor(mean),
               shape = as.factor(mean),
               colour = as.factor(mean))) +
      geom_line() +
      geom_point() +
      xlim(xlim) +
      ylim(ylim) +
      scale_colour_manual(values = c("grey65","black","grey65"),
                          labels = lbs)+
      scale_shape_manual(values = c(NA,16,16), labels = lbs)+
      scale_linetype_manual(values = c("dotted","blank","blank"), labels = lbs)+
      scale_size_manual(values = c(0.5, 2.5, 2.5), labels = lbs)+
      guides(shape = guide_legend(override.aes = list(size=c(1, 3.5, 3.5))))+
      theme_bw() +
      theme(legend.position = leg_pos,
            legend.title=element_blank()) +
      theme(legend.text = element_text(size = 10,
                                       margin = margin(l = 1, unit = "pt")),
            legend.key.height=unit(5,"point"),
            legend.key.size = unit(1.2, "cm"),
            legend.key = element_rect(colour = "transparent", fill = NA),
            legend.background=element_blank())+
      theme(axis.text = element_text(size = 12))+
      theme(plot.background = element_rect(fill = "transparent",colour = NA),
            panel.background = element_rect(fill = "transparent",colour = NA))
    
  }
  
  save_plot <- function(name, file_type){
    
    if(file_type == "pdf"){
    pdf(paste0(save_path, country,"/", name,".pdf"), width=6, height=6) 
    print(p)
    dev.off()
    }
    if(file_type == "eps"){
        ggsave(paste0(save_path, country,"/", name,".eps"), 
               width = 15, height = 15, units = "cm") 
    }
  }
  shape_plot <- function(nsim, obs, sim, ylims, d_fit){
    
    dat_obs <- as.data.frame(obs)
    dat_obs$year <- rownames(obs)
    dat_obs <- dat_obs[dat_obs$year %in% c(1960:2011),]
    
    mean_dat_sim <- as.data.frame(Reduce("+", sim) / length(sim))
    dat_sim <- as.data.frame(do.call("cbind", sim))
    dat_sim$year <- as.numeric(rownames(dat_sim))
    dat_sim <- cbind(dat_sim, mean_dat_sim)
    names(dat_sim) <- c(rep("Simulated", nsim),"year", "Mean of simulations")
    
    dat_aux_0 <- merge(dat_obs, dat_sim, by = "year", all = T)
    dat_aux_1 <- dat_aux_0[dat_aux_0$year > 1960,]
    names(dat_aux_1)[2] <- c("Observed")
    
    dat_long <- reshape(dat_aux_1, dir = "long", idvar = "year",
                        names(dat_aux_1)[c(2:ncol(dat_aux_1))], v.names = "vals",
                        timevar = "type", times = names(dat_aux_1)[c(2:ncol(dat_aux_1))])
    
    dat_long$mean <- ifelse(dat_long$type %in% c("Mean of simulations", "Observed"), 1, 0)
    dat_long$mean <- ifelse(dat_long$type == "Mean of simulations", 2, dat_long$mean )
    dat_long$year <- as.numeric(dat_long$year)
    
    lbs <- c("Simulated", "Observed", "Mean of simulations")
    
    par(mfrow=c(1, 1))
    par(bg=NA)
    p <- ggplot(dat_long,
                aes(x = year,
                    y = vals,
                    group = as.factor(type),
                    size = as.factor(mean),
                    linetype = as.factor(mean),
                    shape = as.factor(mean),
                    colour = as.factor(mean))) +
      geom_line() +
      geom_point() +
      ylim(ylims) +
      scale_colour_manual(values = c("grey65", "grey0", "grey65"),
                          labels = lbs)+
      scale_shape_manual( values = c(NA,16,16), labels = lbs)+
      scale_linetype_manual(values = c("dotted","blank","blank"), labels = lbs)+
      scale_size_manual( values = c(0.5, 2.5, 2.5), labels = lbs)+
      guides(shape = guide_legend(override.aes = list(size=c(1, 3.5, 3.5))))+
      theme_bw() +
      theme(legend.position = c(0.7,0.8),
            legend.title=element_blank()) +
      theme(legend.text = element_text(size = 10,
                                       margin = margin(l = 1, unit = "pt")),
            legend.key.height=unit(5,"point"),
            legend.key.size = unit(1.2, "cm"),
            legend.key = element_rect(colour = "transparent", fill = NA),
            legend.background=element_blank())+
      theme(axis.text = element_text(size = 12))+
      theme(plot.background = element_rect(fill = "transparent",colour = NA),
            panel.background = element_rect(fill = "transparent",colour = NA))
    
    if(d_fit){
      #xlims <- c(min(as.numeric(dat_obs$year)),
      #           as.numeric(dat_obs$year[length(dat_obs$year)]))
      xlims <- c(1960,2016)
      
      p <- p + xlim(xlims)
    }
    
    return(p)
  }
  plot_year_asfr <- function(year, obs, sim, ylims = c(0, ymax), save_year_p = save){
    
    get_year <- function(year, dat){
      extract <- as.data.frame(as.numeric(dat[as.character(year),]))
      return(extract)
    } 
    
    obs <- get_year(year, obs)
    obs$Year <- 15:49
    
    sim <- lapply(sim, function(x) get_year(year, x))
    
    ldat <- long_dat(sim, obs, nsim, ysd = 0, iniY = 15, endY = 49)
    
    p_year <- p_obs_sim(ldat, ylim = ylims, xlim= c(15,49))
    
    if(year != 1960){
      p_year <- p_year + theme(legend.position = "none")
    }
    
    p_year <- p_year + xlab("Age") + ylab("f(x)")
    
    if(save_year_p){
      pdf(paste0("../../latex/plots/",country,"/asfr_", year,".pdf"), width=5, height=5) 
      print(p_year)
      dev.off()
      
      ggsave(paste0("../../latex/plots/",country,"/asfr_", year,".eps"),
             width = 18, height = 18, units = "cm")
    }
    
    
    
    return(p_year)
  }
  
  if(asfr | unplanned | unwanted | desired){
    
    if(country == "ES"){ unplanned <- F; unwanted <- F }
    
    source("../estimation/get_obs.R")
    source("../estimation/get_sim.R")
    obs_set <- get_obs(country, ysd)
    sim_set <- get_sim(res_dir, iniY, endY, nsim, obs_set, asfr, unplanned, unwanted, desired, all_sim = T)
    
  }
  
  if(asfr){
    
    if(class(obs_set) == "list"){
      obs <- obs_set$obs_asfr
    }else{
      obs <- obs_set
    }
    
    sim <- sim_set$sim_asfr
    
    ymax <- max(max(unlist(sim)), max(unlist(obs)))
    
    
    p <- lapply(seq(1960, 2016, 5), plot_year_asfr, obs, sim)
    
    print(p)
  }
  
  if(tfr){
    # Obs
    obs <- read.table(file.path(out_path, "tfr_hfd.txt"),
                      skip = 2, header = T,
                      stringsAsFactors = F)[1:2]
    
    # sim
    sim <- sapply(res_names, function(x) readRDS(x)["tfr"])
    
    ldat <- long_dat(sim, obs, nsim, ysd = 1960, iniY = 1910, endY = 2016)
    
    p <- p_obs_sim(ldat, ylim = c(1,4.5), xlim= c(1958,2018))
    
    p <- p + xlab("Year") + ylab("Total Fertility Rates")
      
    if(save){ save_plot("tfr", file_type = "eps") }
    
    print(p)
    
  } 
  
  if(ccf){
    # sim
    sim <- sapply(res_names, function(x) readRDS(x)["cohort"])
    sim <- lapply(sim, function(x) x[,2])
    # obs
    obs <- read.table(file.path(out_path,"ccf_hfd.txt"), skip = 2, header = T,
                          stringsAsFactors = F)[1:2]
    obs$CCF<- ifelse(obs$CCF==".", NA, obs$CCF)
    obs$CCF <- as.numeric(obs$CCF)
    if (country == "FR"){
      obs <- rbind(setNames(as.data.frame(matrix(c(iniY:1930,
                                                       rep(NA, 1931-iniY)),
                                                     ncol = 2)),names(obs[,1:2])), obs[,1:2])
    }
    names(obs)[1] <- "Year" 
    ldat <- long_dat(sim, obs, nsim, ysd = 1910, iniY = 1910, endY = 1966)
    
    p <- p_obs_sim(ldat, ylim = c(1,4.5), xlim= c(1928,1968))
    
    p <- p + xlab("Cohort") + ylab("Completed Cohort Fertility")
    
    if(save){ save_plot("ccf",file_type = "eps") }
    
    
    print(p)
  }
  
  if(ccf_edu){
    
    sim <- list()
    sim[[1]] <- sapply(res_names, function(x) readRDS(x)["cohort"])
    for (i in 1:3) {
      sim[[1+i]] <- sapply(res_names, function(x) readRDS(x)[paste0("cohort", i)])
    }
    
    sim <- lapply(sim, function(x) lapply(x, function(j) j[,2]))
    sim_all <- lapply(sim, long_dat, obs = F,
                      nsim, ysd = 1910, iniY = 1910, endY = 1966)
    
    sim <- do.call("rbind", sim_all)
    sim$cat <- rep(0:3, each = nrow(sim_all[[1]]))
    
    p <- p_sim_cats(sim, ylim = c(1, 4), xlim = c(1928, 1968),
                    lbs = c("Simulated", "Mean of simulations"),
                    lbs_cats = c("Mean", "Low Edu", "Med Edu", "High Edu"),
                    leg_pos = c(0.8,0.7),
                    cats = 4) 
    
    if(save){save_plot("ccf_edu", file_type = "eps")}
    
    print(p)
    
  }
  
  if(ccf_edu_obs){
    
    obs <- read.table(file.path(out_path,"ccf_edu"), header = T) 
    names(obs)[1] <- "Year"
    
    if(country == "FR"){
      obs[,1] <- seq(1931,1970,5)
      obs[nrow(obs),1] <- 1970
      }
    
    obs1 <- obs[,c("Year","CFR.1")] 
    obs2 <- obs[,c("Year","CFR.3")] 
    
    sim <- list()
    sim[[1]] <- sapply(res_names, function(x) readRDS(x)["cohort"])
    for (i in 1:3) {
      sim[[1+i]] <- sapply(res_names, function(x) readRDS(x)[paste0("cohort", i)])
    }
    
    sim <- lapply(sim, function(x) lapply(x, function(j) j[,2]))
    sim_all <- list()
    sim_all[[1]] <- long_dat(sim[[2]], obs = obs1, nsim, ysd = 1910, iniY = 1910, endY = 1966)
    sim_all[[2]] <- long_dat(sim[[4]], obs = obs2, nsim, ysd = 1910, iniY = 1910, endY = 1966)
    
    sim <- do.call("rbind", sim_all)
    sim$cat <- rep(0:1, each = nrow(sim_all[[1]]))
    
    p <- p_sim_obs_cats(sim, ylim = c(1, 3.5), xlim = c(1930, 1970),
                        lbs = c("Simulated","Observed", "Mean of simulations"),
                        lbs_cats = c("Edu1", "Edu3"),
                        leg_pos = c(0.5, 0.8)) 
    p <- p + annotate(geom="text", x=1968, y=1.2,
                      label="Primary", 
                      color="black", 
                      size = 4) + 
      annotate(geom="text", x=1986, y=2.2,
               label="Tertiary",
               color="black",
               size = 4)
    
    if(save){save_plot("ccf_edu_obs", file_type = "eps") }
    
    print(p)
    
  }
  
  if(mab){
    # Obs
    obs <- read.table(file.path(out_path,"mab_hfd.txt"), 
                      skip = 2,
                      header = T,
                      stringsAsFactors = F) 
    
    if(country %in% c("ES", "IR")){ 
      obs <- read.table(file.path(out_path,"mab_parity_hfd.txt"), skip = 2, header = T,
                        stringsAsFactors = F) 
      obs1 <- obs[,c("MAB1","Year")] 
      obs2 <- obs[,c("MAB2","Year")] 
    }
    
    obs <- obs[obs>=1960,1:2]
    
    
    # sim
    sim <- sapply(res_names, function(x) readRDS(x)["meanAgeBirth"])

    
    ldat <- long_dat(sim, obs, nsim, ysd = 1960, iniY = 1910, endY = 2016)

    p <- p_obs_sim(ldat, ylim = c(20,40), xlim= c(1958,2018))
    
    p <- p + xlab("Year") + ylab("Mean Age at Birth")
    
    if(save){ save_plot("mab", file_type = "eps") }
    
    print(p)
    
  }
  
  if(mabs){
    # Obs
    obs <- read.table(file.path(out_path,"mab_hfd.txt"), 
                      skip = 2, header = T,
                      stringsAsFactors = F) 
    
    if(country =="ES"){ 
      obs <- read.table(file.path(out_path,"mab_parity_hfd.txt"),
                        skip = 2, header = T,
                        stringsAsFactors = F) 
      obs1 <- obs[,c("MAB1","Year")] 
      obs2 <- obs[,c("MAB2","Year")] 
    }
    
    obs <- obs[obs>=1960,1:2]
    
    # sim
    sim1 <- sapply(res_names, function(x) readRDS(x)["meanAgeBirth1"])
    sim2 <- sapply(res_names, function(x) readRDS(x)["meanAgeBirth2"])
    
    sim_all <- list()
    sim_all[[1]] <- long_dat(sim1, obs = obs1, nsim, ysd = 1960, iniY = 1910, endY = 2016)
    sim_all[[2]] <- long_dat(sim2, obs = obs2, nsim, ysd = 1960, iniY = 1910, endY = 2016)
    
    
    sim <- do.call("rbind", sim_all)
    sim$cat <- rep(0:1, each = nrow(sim_all[[1]]))
    
    p <- p_sim_obs_cats(sim, ylim = c(20, 40), xlim = c(1958, 2018),
                    lbs = c("Simulated","Observed", "Mean of simulations"),
                    lbs_cats = c("MAB1", "MAB1"),
                    leg_pos = c(0.3, 0.8))
    
    p <- p + annotate(geom="text", x=2014, y=29,
                      label="First births",
                      color="black",
                      size = 4) + 
      annotate(geom="text", x=2014, y=35.5,
               label="Second births",
               color="black",
               size = 4)
    
    p <- p + xlab("Year") + ylab("Mean Age at Birth")
      
    if(save){save_plot("mabs", file_type = "eps")}
    
    print(p)
    
  }
  
  if(unplanned){
    
    p <- shape_plot(nsim,obs_set$obs_unplanned, sim_set$sim_unplanned, ylims = c(0,0.6), d_fit = F)
    
    if(save){ save_plot("unplanned") }
    
    print(p)
    
  }
  
  if(unwanted){
    
    p <-  shape_plot(nsim, obs_set$obs_unwanted, sim_set$sim_unwanted, ylims = c(0,0.4), d_fit = F)
    
    if(save){ save_plot("unwanted") }
    
    print(p)
    
  }
  
  if(desired){
    
    p <-  shape_plot(nsim, obs_set$obs_desired, sim_set$sim_desired, ylims = c(1.5,3), d_fit = T)
    
    p <- p + xlab("Year") + ylab("Average Desired Family Size")
    
    if(save){save_plot("desired", file_type = "eps") }
    
    print(p)
    
  }
  
  if(gap){
    
    sim <- sapply(res_names, function(x) readRDS(x)["gapKids"])
    
    ldat <- long_dat(sim, obs = F, nsim, ysd = 1960, iniY = 1910, endY = 2016)
    
    p <- p_sim(ldat, ylim = c(-1,0.5), xlim= c(1958,2018))
    
    if(save){ save_plot("gap", file_type = "eps") }
    
    print(p)
    
  }
  
  if(gap_edu){
    
    sim <- sapply(res_names, function(x) readRDS(x)["gapKids"])
    sim1 <- sapply(res_names, function(x) readRDS(x)["gapEdu1"])
    sim2 <- sapply(res_names, function(x) readRDS(x)["gapEdu2"])
    sim3 <- sapply(res_names, function(x) readRDS(x)["gapEdu3"])
    
    sim_all <- lapply(list(sim, sim1, sim2, sim3), long_dat, obs = F,
                  nsim, ysd = 1960, iniY = 1910, endY = 2016)
    
    sim <- do.call("rbind", sim_all)
    sim$cat <- rep(0:3, each = nrow(sim_all[[1]]))
    
    p <- p_sim_cats(sim, ylim = c(-1.5, 0.5), xlim = c(1958, 2018),
                    lbs = c("Simulated", "Mean of simulations"),
                    lbs_cats = c("Mean", "Low Edu", "Med Edu", "High Edu"),
                    leg_pos = c(0.7, 0.3),
                    cats = 4)
    
    if(save){ save_plot("gap_edu", file_type = "eps") }
    
    print(p)
    
  }
  
  if(css){
    
    # Obs css
    obs  <- read.table(file.path(out_path,"childlessness.csv"),
                       sep = ",",
                       skip = 3,
                       header = T)
    names(obs)[1] <- "Year" 
    obs <- obs[obs$Year %in% seq(1910, 1966,1),]
    
    # sim
    sim <- lapply(res_names, function(x) readRDS(x)["childless"])
    sim <- sapply(sim, function(x) lapply(x, function(j) j[-c(1:10),2]*100))
    
    ldat <- long_dat(sim, obs, nsim, ysd = 1910, iniY = 1910, endY = 1966)
    
    p <- p_obs_sim(ldat, ylim = c(0,100), xlim= c(1930,1960))
    
    p <- p + xlab("Cohort") + ylab("Proportion Childless")
    
    if(save){save_plot("css", file_type = "eps") }
    
    print(p)
    
  }
  
  
}



