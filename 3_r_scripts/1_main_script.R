## Tree swallow corticosterone and weather analysis

## Load packages ----
    pacman::p_load(tidyverse, here, plyr, sjPlot, lme4, lmerTest, ggpubr)
      

## Load data ----
  # Load data for each birds capture
      db <- read.delim(here::here("1_raw_data/data_by_capture.txt"))
      # Subset to non invasive experiments
        db <- subset(db, db$ok_for_weather == "yes")
        
      # Capture hour
        db$cap_hr <- round(db$cap_time / 60, 0)

          ##NOTE: about 400 samples have a raw value but no corrected values. For now I'm just assuming efficiency of 85%, but these
            # should be fixed in the database
            for(i in 1:nrow(db)){
              if(is.na(db$cort1_correct[i]) == TRUE){
                if(is.na(db$cort1_raw[i]) == FALSE){db$cort1_correct[i] <- db$cort1_raw[i] * (100/85)}
              }
              if(is.na(db$cort2_correct[i]) == TRUE){
                if(is.na(db$cort1_raw[i]) == FALSE){db$cort2_correct[i] <- db$cort2_raw[i] * (100/85)}
              }
              if(is.na(db$cort3_correct[i]) == TRUE){
                if(is.na(db$cort1_raw[i]) == FALSE){db$cort3_correct[i] <- db$cort3_raw[i] * (100/85)}
              }
            }
        
        db <- subset(db, db$cort1_correct > 0)
      
  # Load weather data
      dw <- read.delim(here::here("1_raw_data/Input_Weather_Data.txt"))
    
      # Subset to just the airport station
        dw <- subset(dw, dw$station == "Airport")

## Add weather data to each capture
        
    for(i in 1:nrow(db)){
      sub <- subset(dw, dw$year == db$year[i])
      sub1 <- subset(sub, sub$doy == db$cap_doy[i] & sub$hour < db$cap_hr[i] + 1 & sub$hour > db$cap_hr[i] - 3)
      sub1b <- subset(sub, sub$doy == db$cap_doy[i] & sub$hour < 9 & sub$hour > 5)
      sub2a <- subset(sub, sub$doy == db$cap_doy[i] | sub$doy == db$cap_doy[i] - 1)
      sub2 <- subset(sub2a, sub2a$hour > 21 | sub2a$hour < 6)
      sub3 <- subset(sub, sub$doy == db$cap_doy[i] - 1 & sub$hour > 5 & sub$hour < 21)
      sub4 <- subset(sub, sub$doy < db$cap_doy[i] & sub$doy > db$cap_doy[i] - 4 & sub$hour > 5 & sub$hour < 21)
      sub5 <- subset(sub, sub$doy < db$cap_doy[i] & sub$doy > db$cap_doy[i] - 8 & sub$hour > 5 & sub$hour < 21)
      
      db$t_3cap_av[i] <- mean(sub1$avg_temp_C)
      db$t_m_av[i] <- mean(sub1b$avg_temp_C)
      db$t_nb_av[i] <- mean(sub2$avg_temp_C)
      db$t_nb_lo[i] <- min(na.omit(sub2$avg_temp_C))
      db$t_db_av[i] <- mean(sub3$avg_temp_C)
      db$t_db_hi[i] <- max(na.omit(sub3$avg_temp_C))
      db$t_3b_av[i] <- mean(sub4$avg_temp_C)
      db$t_3b_hi[i] <- max(na.omit(sub4$avg_temp_C))
      db$t_7b_av[i] <- mean(sub5$avg_temp_C)
      db$t_7b_hi[i] <- max(na.omit(sub5$avg_temp_C))
     
      print(paste0(i, " of ", nrow(db)))
    }
        
        
    saveRDS(db, here::here("2_modified_data/cort_plus_temperature.txt"))  
    
## Fix some inf values
    db$t_nb_lo[which(!is.finite(db$t_nb_lo))] <- NA
    
## Split out adults and nestlings
    db$lat_min <- db$latency / 60
    dba <- subset(db, db$adult_or_nestling == "Adult")
    dbn <- subset(db, db$adult_or_nestling == "Nestling")
 
# Base cort datasets ----
    base_a <- subset(dba, dba$type1 == "B")
    base_n <- subset(dbn, dbn$type1 == "B")
    base_af <- subset(base_a, base_a$sex == "F")
    base_am <- subset(base_a, base_a$sex == "M")

### Base cort 3 hours prior or morning of ----
    # morning of baseline
      mbaf <- lmer(cort1_correct ~ t_m_av + as.factor(cap_num) + lat_min + (1|band),
                 data = base_af)
      mbam <- lmer(cort1_correct ~ t_3cap_av + lat_min + (1|band),
                  data = base_am)
      mbn <- lm(cort1_correct ~ t_3cap_av + lat_min,
                 data = base_n)
      
      t_1 <- tab_model(mbaf, mbam, mbn, 
                dv.labels = c("Adult Female Baseline", "Adult Male Baseline", "Nestling Baseline"),
                pred.labels = c("Intercept", "Temperature 6-8am (C)", "Capture 2", "Capture 3", "Latency (min)", "Temperature 3 hrs prior (C)"),
                title = "Average temperature 3 hours before capture.")
      saveRDS(t_1, here::here("5_other_outputs/table1.rds"))
      
    # make plots for these three on normal scale and on log scale
      
      p1 <- ggplot(data = base_af, mapping = aes(x = t_m_av, y = cort1_correct, col = "slateblue")) +
        geom_point(col = "slateblue", size = 0.6, alpha = 0.6) + geom_smooth(col = "coral3", method = "loess") + ylim(0, 50) + 
        theme_classic() + xlab("Avg Temp 6-8 a.m. (C)") + 
        ylab(expression(paste("Corticosterone ng/", mu, "l"))) + ggtitle("Adult Females")
      
      p2 <- ggplot(data = base_am, mapping = aes(x = t_3cap_av, y = cort1_correct, col = "slateblue")) +
        geom_point(col = "slateblue", size = 0.6, alpha = 0.6) + geom_smooth(col = "coral3", method = "loess") +  
        theme_classic() + xlab("Avg Temp 3 Hours Prior (C)") + 
        ylab(expression(paste("Corticosterone ng/", mu, "l"))) + ggtitle("Adult Males") + ylim(0, 50)
      
      p3 <- ggplot(data = base_n, mapping = aes(x = t_3cap_av, y = cort1_correct, col = "slateblue")) +
        geom_point(col = "slateblue", size = 0.6, alpha = 0.6) + geom_smooth(col = "coral3", method = "loess") +  
        theme_classic() + xlab("Avg Temp 3 Hours Prior (C)") + 
        ylab(expression(paste("Corticosterone ng/", mu, "l"))) + ggtitle("Nestlings") + ylim(0, 100)
      
      g1 <- ggarrange(p1, p2, p3, ncol = 3, nrow = 1)
      
      ggsave(here::here("3_r_scripts/fig1.png"), plot = g1, width = 7.8, height = 2.7, units = "in", device = "png")
      
    # morning of baseline
      mbaf <- lmer(log(cort1_correct + 1) ~ t_m_av + as.factor(cap_num) + lat_min + (1|band),
                   data = base_af)
      mbam <- lmer(log(cort1_correct + 1) ~ t_3cap_av + lat_min + (1|band),
                   data = base_am)
      mbn <- lm(log(cort1_correct + 1) ~ t_3cap_av + lat_min,
                data = base_n)
      
      t_1 <- tab_model(mbaf, mbam, mbn, 
                       dv.labels = c("Adult Female Baseline", "Adult Male Baseline", "Nestling Baseline"),
                       pred.labels = c("Intercept", "Temperature 6-8am (C)", "Capture 2", "Capture 3", "Latency (min)", "Temperature 3 hrs prior (C)"),
                       title = "Average temperature 3 hours before capture.")
      saveRDS(t_1, here::here("5_other_outputs/table2.rds"))
      
    # make plots for these three on normal scale and on log scale
      
      p1 <- ggplot(data = base_af, mapping = aes(x = t_m_av, y = log(cort1_correct + 1), col = "slateblue")) +
        geom_point(col = "slateblue", size = 0.6, alpha = 0.6) + geom_smooth(col = "coral3", method = "loess") + 
        theme_classic() + xlab("Avg Temp 6-8 a.m. (C)") + ylim(0.35, 5) +
        ylab(expression(paste("log(Corticosterone ng/", mu, "l)"))) + ggtitle("Adult Females")
      
      p2 <- ggplot(data = base_am, mapping = aes(x = t_3cap_av, y = log(cort1_correct + 1), col = "slateblue")) +
        geom_point(col = "slateblue", size = 0.6, alpha = 0.6) + geom_smooth(col = "coral3", method = "loess") +  
        theme_classic() + xlab("Avg Temp 3 Hours Prior (C)") + ylim(0.35, 5) +
        ylab(expression(paste("log(Corticosterone ng/", mu, "l)"))) + ggtitle("Adult Males")
      
      p3 <- ggplot(data = base_n, mapping = aes(x = t_3cap_av, y = log(cort1_correct + 1), col = "slateblue")) +
        geom_point(col = "slateblue", size = 0.6, alpha = 0.6) + geom_smooth(col = "coral3", method = "loess") +  
        theme_classic() + xlab("Avg Temp 3 Hours Prior (C)") + ylim(0.35, 5) + 
        ylab(expression(paste("log(Corticosterone ng/", mu, "l)"))) + ggtitle("Nestlings")
      
      g1 <- ggarrange(p1, p2, p3, ncol = 3, nrow = 1)
      
      ggsave(here::here("3_r_scripts/fig2.png"), plot = g1, width = 7.8, height = 2.7, units = "in", device = "png")
      
### Base cort avg temperature from previous night ----
           
    # night before average temperature
      mbaf <- lmer(cort1_correct ~ t_nb_av + as.factor(cap_num) + lat_min + (1|band),
                   data = base_af)
      mbam <- lmer(cort1_correct ~ t_nb_av + lat_min + (1|band),
                   data = base_am)
      mbn <- lm(cort1_correct ~ t_nb_av + lat_min,
                data = base_n)
      
      t_1 <- tab_model(mbaf, mbam, mbn, 
                       dv.labels = c("Adult Female Baseline", "Adult Male Baseline", "Nestling Baseline"),
                       pred.labels = c("Intercept", "Temperature (C)", "Capture 2", "Capture 3", "Latency (min)"),
                       title = "Low temperature previous night.")
      saveRDS(t_1, here::here("5_other_outputs/table3.rds"))
      
    # make plots for these three on normal scale and on log scale
      
      p1 <- ggplot(data = base_af, mapping = aes(x = t_nb_av, y = cort1_correct, col = "slateblue")) +
        geom_point(col = "slateblue", size = 0.6, alpha = 0.6) + geom_smooth(col = "coral3", method = "loess") + ylim(0, 50) + 
        theme_classic() + xlab("Avg Temp Last Night (C)") + 
        ylab(expression(paste("Corticosterone ng/", mu, "l"))) + ggtitle("Adult Females")
      
      p2 <- ggplot(data = base_am, mapping = aes(x = t_nb_av, y = cort1_correct, col = "slateblue")) +
        geom_point(col = "slateblue", size = 0.6, alpha = 0.6) + geom_smooth(col = "coral3", method = "loess") +  
        theme_classic() + xlab("Avg Temp Last Night (C)") + 
        ylab(expression(paste("Corticosterone ng/", mu, "l"))) + ggtitle("Adult Males") + ylim(0, 50)
      
      p3 <- ggplot(data = base_n, mapping = aes(x = t_nb_av, y = cort1_correct, col = "slateblue")) +
        geom_point(col = "slateblue", size = 0.6, alpha = 0.6) + geom_smooth(col = "coral3", method = "loess") +  
        theme_classic() + xlab("Avg Temp Last Night (C)") + 
        ylab(expression(paste("Corticosterone ng/", mu, "l"))) + ggtitle("Nestlings") + ylim(0, 100)
      
      g1 <- ggarrange(p1, p2, p3, ncol = 3, nrow = 1)
      
      ggsave(here::here("3_r_scripts/fig3.png"), plot = g1, width = 7.8, height = 2.7, units = "in", device = "png")
      
    # log cort night before average
      mbaf <- lmer(log(cort1_correct + 1) ~ t_nb_av + as.factor(cap_num) + lat_min + (1|band),
                   data = base_af)
      mbam <- lmer(log(cort1_correct + 1) ~ t_nb_av + lat_min + (1|band),
                   data = base_am)
      mbn <- lm(log(cort1_correct + 1) ~ t_nb_av + lat_min,
                data = base_n)
      
      t_1 <- tab_model(mbaf, mbam, mbn, 
                       dv.labels = c("Adult Female Baseline", "Adult Male Baseline", "Nestling Baseline"),
                       pred.labels = c("Intercept", "Temperature (C)", "Capture 2", "Capture 3", "Latency (min)"),
                       title = "Average temperature 3 hours before capture.")
      saveRDS(t_1, here::here("5_other_outputs/table4.rds"))
      
      # make plots for these three on normal scale and on log scale
      
      p1 <- ggplot(data = base_af, mapping = aes(x = t_nb_av, y = log(cort1_correct + 1), col = "slateblue")) +
        geom_point(col = "slateblue", size = 0.6, alpha = 0.6) + geom_smooth(col = "coral3", method = "loess") + 
        theme_classic() + xlab("Avg Temp Last Night (C)") + ylim(0.35, 5) +
        ylab(expression(paste("log(Corticosterone ng/", mu, "l)"))) + ggtitle("Adult Females")
      
      p2 <- ggplot(data = base_am, mapping = aes(x = t_nb_av, y = log(cort1_correct + 1), col = "slateblue")) +
        geom_point(col = "slateblue", size = 0.6, alpha = 0.6) + geom_smooth(col = "coral3", method = "loess") +  
        theme_classic() + xlab("Avg Temp Last Night (C)") + ylim(0.35, 5) +
        ylab(expression(paste("log(Corticosterone ng/", mu, "l)"))) + ggtitle("Adult Males")
      
      p3 <- ggplot(data = base_n, mapping = aes(x = t_nb_av, y = log(cort1_correct + 1), col = "slateblue")) +
        geom_point(col = "slateblue", size = 0.6, alpha = 0.6) + geom_smooth(col = "coral3", method = "loess") +  
        theme_classic() + xlab("Avg Temp Last Night (C)") + ylim(0.35, 5) + 
        ylab(expression(paste("log(Corticosterone ng/", mu, "l)"))) + ggtitle("Nestlings")
      
      g1 <- ggarrange(p1, p2, p3, ncol = 3, nrow = 1)
      
      ggsave(here::here("3_r_scripts/fig4.png"), plot = g1, width = 7.8, height = 2.7, units = "in", device = "png")
 
      
      
### base vs average previous day ----           
  # average temperature previous day
      mbaf <- lmer(cort1_correct ~ t_db_av + as.factor(cap_num) + lat_min + (1|band),
                   data = base_af)
      mbam <- lmer(cort1_correct ~ t_db_av + lat_min + (1|band),
                   data = base_am)
      mbn <- lm(cort1_correct ~ t_db_av + lat_min,
                data = base_n)
      
      t_1 <- tab_model(mbaf, mbam, mbn, 
                       dv.labels = c("Adult Female Baseline", "Adult Male Baseline", "Nestling Baseline"),
                       pred.labels = c("Intercept", "Temperature (C)", "Capture 2", "Capture 3", "Latency (min)"),
                       title = "Average temperature prior day.")
      saveRDS(t_1, here::here("5_other_outputs/table5.rds"))
      
      # make plots for these three on normal scale and on log scale
      
      p1 <- ggplot(data = base_af, mapping = aes(x = t_db_av, y = cort1_correct, col = "slateblue")) +
        geom_point(col = "slateblue", size = 0.6, alpha = 0.6) + geom_smooth(col = "coral3", method = "loess") + ylim(0, 50) + 
        theme_classic() + xlab("Avg Temp Prior Day (C)") + 
        ylab(expression(paste("Corticosterone ng/", mu, "l"))) + ggtitle("Adult Females")
      
      p2 <- ggplot(data = base_am, mapping = aes(x = t_db_av, y = cort1_correct, col = "slateblue")) +
        geom_point(col = "slateblue", size = 0.6, alpha = 0.6) + geom_smooth(col = "coral3", method = "loess") +  
        theme_classic() + xlab("Avg Temp Prior Day (C)") + 
        ylab(expression(paste("Corticosterone ng/", mu, "l"))) + ggtitle("Adult Males") + ylim(0, 50)
      
      p3 <- ggplot(data = base_n, mapping = aes(x = t_db_av, y = cort1_correct, col = "slateblue")) +
        geom_point(col = "slateblue", size = 0.6, alpha = 0.6) + geom_smooth(col = "coral3", method = "loess") +  
        theme_classic() + xlab("Avg Temp Prior Day (C)") + 
        ylab(expression(paste("Corticosterone ng/", mu, "l"))) + ggtitle("Nestlings") + ylim(0, 100)
      
      g1 <- ggarrange(p1, p2, p3, ncol = 3, nrow = 1)
      
      ggsave(here::here("3_r_scripts/fig5.png"), plot = g1, width = 7.8, height = 2.7, units = "in", device = "png")
      
      # log cort night before average
      mbaf <- lmer(log(cort1_correct + 1) ~ t_db_av + as.factor(cap_num) + lat_min + (1|band),
                   data = base_af)
      mbam <- lmer(log(cort1_correct + 1) ~ t_db_av + lat_min + (1|band),
                   data = base_am)
      mbn <- lm(log(cort1_correct + 1) ~ t_db_av + lat_min,
                data = base_n)
      
      t_1 <- tab_model(mbaf, mbam, mbn, 
                       dv.labels = c("Adult Female Baseline", "Adult Male Baseline", "Nestling Baseline"),
                       pred.labels = c("Intercept", "Temperature (C)", "Capture 2", "Capture 3", "Latency (min)"),
                       title = "Average temperature prior day.")
      saveRDS(t_1, here::here("5_other_outputs/table6.rds"))
      
      # make plots for these three on normal scale and on log scale
      
      p1 <- ggplot(data = base_af, mapping = aes(x = t_db_av, y = log(cort1_correct + 1), col = "slateblue")) +
        geom_point(col = "slateblue", size = 0.6, alpha = 0.6) + geom_smooth(col = "coral3", method = "loess") + 
        theme_classic() + xlab("Avg Temp Prior Day (C)") + ylim(0.35, 5) +
        ylab(expression(paste("log(Corticosterone ng/", mu, "l)"))) + ggtitle("Adult Females")
      
      p2 <- ggplot(data = base_am, mapping = aes(x = t_db_av, y = log(cort1_correct + 1), col = "slateblue")) +
        geom_point(col = "slateblue", size = 0.6, alpha = 0.6) + geom_smooth(col = "coral3", method = "loess") +  
        theme_classic() + xlab("Avg Temp Prior Day (C)") + ylim(0.35, 5) +
        ylab(expression(paste("log(Corticosterone ng/", mu, "l)"))) + ggtitle("Adult Males")
      
      p3 <- ggplot(data = base_n, mapping = aes(x = t_db_av, y = log(cort1_correct + 1), col = "slateblue")) +
        geom_point(col = "slateblue", size = 0.6, alpha = 0.6) + geom_smooth(col = "coral3", method = "loess") +  
        theme_classic() + xlab("Avg Temp Prior Day (C)") + ylim(0.35, 5) + 
        ylab(expression(paste("log(Corticosterone ng/", mu, "l)"))) + ggtitle("Nestlings")
      
      g1 <- ggarrange(p1, p2, p3, ncol = 3, nrow = 1)
      
      ggsave(here::here("3_r_scripts/fig6.png"), plot = g1, width = 7.8, height = 2.7, units = "in", device = "png")

      
### base vs high previous day ----           
      # average temperature previous day
      mbaf <- lmer(cort1_correct ~ t_db_hi + as.factor(cap_num) + lat_min + (1|band),
                   data = base_af)
      mbam <- lmer(cort1_correct ~ t_db_hi + lat_min + (1|band),
                   data = base_am)
      mbn <- lm(cort1_correct ~ t_db_hi + lat_min,
                data = base_n)
      
      t_1 <- tab_model(mbaf, mbam, mbn, 
                       dv.labels = c("Adult Female Baseline", "Adult Male Baseline", "Nestling Baseline"),
                       pred.labels = c("Intercept", "Temperature (C)", "Capture 2", "Capture 3", "Latency (min)"),
                       title = "High temperature prior day.")
      saveRDS(t_1, here::here("5_other_outputs/table5b.rds"))
      
      # make plots for these three on normal scale and on log scale
      
      p1 <- ggplot(data = base_af, mapping = aes(x = t_db_hi, y = cort1_correct, col = "slateblue")) +
        geom_point(col = "slateblue", size = 0.6, alpha = 0.6) + geom_smooth(col = "coral3", method = "loess") + ylim(0, 50) + 
        theme_classic() + xlab("High Temp Prior Day (C)") + 
        ylab(expression(paste("Corticosterone ng/", mu, "l"))) + ggtitle("Adult Females")
      
      p2 <- ggplot(data = base_am, mapping = aes(x = t_db_hi, y = cort1_correct, col = "slateblue")) +
        geom_point(col = "slateblue", size = 0.6, alpha = 0.6) + geom_smooth(col = "coral3", method = "loess") +  
        theme_classic() + xlab("High Temp Prior Day (C)") + 
        ylab(expression(paste("Corticosterone ng/", mu, "l"))) + ggtitle("Adult Males") + ylim(0, 50)
      
      p3 <- ggplot(data = base_n, mapping = aes(x = t_db_hi, y = cort1_correct, col = "slateblue")) +
        geom_point(col = "slateblue", size = 0.6, alpha = 0.6) + geom_smooth(col = "coral3", method = "loess") +  
        theme_classic() + xlab("High Temp Prior Day (C)") + 
        ylab(expression(paste("Corticosterone ng/", mu, "l"))) + ggtitle("Nestlings") + ylim(0, 100)
      
      g1 <- ggarrange(p1, p2, p3, ncol = 3, nrow = 1)
      
      ggsave(here::here("3_r_scripts/fig5b.png"), plot = g1, width = 7.8, height = 2.7, units = "in", device = "png")
      
      # log cort night before average
      mbaf <- lmer(log(cort1_correct + 1) ~ t_db_hi + as.factor(cap_num) + lat_min + (1|band),
                   data = base_af)
      mbam <- lmer(log(cort1_correct + 1) ~ t_db_hi + lat_min + (1|band),
                   data = base_am)
      mbn <- lm(log(cort1_correct + 1) ~ t_db_hi + lat_min,
                data = base_n)
      
      t_1 <- tab_model(mbaf, mbam, mbn, 
                       dv.labels = c("Adult Female Baseline", "Adult Male Baseline", "Nestling Baseline"),
                       pred.labels = c("Intercept", "Temperature (C)", "Capture 2", "Capture 3", "Latency (min)"),
                       title = "High temperature prior day.")
      saveRDS(t_1, here::here("5_other_outputs/table6b.rds"))
      
      # make plots for these three on normal scale and on log scale
      
      p1 <- ggplot(data = base_af, mapping = aes(x = t_db_hi, y = log(cort1_correct + 1), col = "slateblue")) +
        geom_point(col = "slateblue", size = 0.6, alpha = 0.6) + geom_smooth(col = "coral3", method = "loess") + 
        theme_classic() + xlab("High Temp Prior Day (C)") + ylim(0.35, 5) +
        ylab(expression(paste("log(Corticosterone ng/", mu, "l)"))) + ggtitle("Adult Females")
      
      p2 <- ggplot(data = base_am, mapping = aes(x = t_db_hi, y = log(cort1_correct + 1), col = "slateblue")) +
        geom_point(col = "slateblue", size = 0.6, alpha = 0.6) + geom_smooth(col = "coral3", method = "loess") +  
        theme_classic() + xlab("High Temp Prior Day (C)") + ylim(0.35, 5) +
        ylab(expression(paste("log(Corticosterone ng/", mu, "l)"))) + ggtitle("Adult Males")
      
      p3 <- ggplot(data = base_n, mapping = aes(x = t_db_hi, y = log(cort1_correct + 1), col = "slateblue")) +
        geom_point(col = "slateblue", size = 0.6, alpha = 0.6) + geom_smooth(col = "coral3", method = "loess") +  
        theme_classic() + xlab("High Temp Prior Day (C)") + ylim(0.35, 5) + 
        ylab(expression(paste("log(Corticosterone ng/", mu, "l)"))) + ggtitle("Nestlings")
      
      g1 <- ggarrange(p1, p2, p3, ncol = 3, nrow = 1)
      
      ggsave(here::here("3_r_scripts/fig6b.png"), plot = g1, width = 7.8, height = 2.7, units = "in", device = "png")
      
      
### Basecort Avg temp previous 3 days ----            
      # average temperature previous 3 day
      mbaf <- lmer(cort1_correct ~ t_3b_av + as.factor(cap_num) + lat_min + (1|band),
                   data = base_af)
      mbam <- lmer(cort1_correct ~ t_3b_av + lat_min + (1|band),
                   data = base_am)
      mbn <- lm(cort1_correct ~ t_3b_av + lat_min,
                data = base_n)
      
      t_1 <- tab_model(mbaf, mbam, mbn, 
                       dv.labels = c("Adult Female Baseline", "Adult Male Baseline", "Nestling Baseline"),
                       pred.labels = c("Intercept", "Temperature (C)", "Capture 2", "Capture 3", "Latency (min)"),
                       title = "Average temperature prior 3 days.")
      saveRDS(t_1, here::here("5_other_outputs/table7.rds"))
      
      # make plots for these three on normal scale and on log scale
      
      p1 <- ggplot(data = base_af, mapping = aes(x = t_3b_av, y = cort1_correct, col = "slateblue")) +
        geom_point(col = "slateblue", size = 0.6, alpha = 0.6) + geom_smooth(col = "coral3", method = "loess") + ylim(0, 50) + 
        theme_classic() + xlab("Avg Temp Prior 3 Days (C)") + 
        ylab(expression(paste("Corticosterone ng/", mu, "l"))) + ggtitle("Adult Females")
      
      p2 <- ggplot(data = base_am, mapping = aes(x = t_3b_av, y = cort1_correct, col = "slateblue")) +
        geom_point(col = "slateblue", size = 0.6, alpha = 0.6) + geom_smooth(col = "coral3", method = "loess") +  
        theme_classic() + xlab("Avg Temp Prior 3 Days (C)") + 
        ylab(expression(paste("Corticosterone ng/", mu, "l"))) + ggtitle("Adult Males") + ylim(0, 50)
      
      p3 <- ggplot(data = base_n, mapping = aes(x = t_3b_av, y = cort1_correct, col = "slateblue")) +
        geom_point(col = "slateblue", size = 0.6, alpha = 0.6) + geom_smooth(col = "coral3", method = "loess") +  
        theme_classic() + xlab("Avg Temp Prior 3 Days (C)") + 
        ylab(expression(paste("Corticosterone ng/", mu, "l"))) + ggtitle("Nestlings") + ylim(0, 100)
      
      g1 <- ggarrange(p1, p2, p3, ncol = 3, nrow = 1)
      
      ggsave(here::here("3_r_scripts/fig7.png"), plot = g1, width = 7.8, height = 2.7, units = "in", device = "png")
      
      # log cort night before average
      mbaf <- lmer(log(cort1_correct + 1) ~ t_db_av + as.factor(cap_num) + lat_min + (1|band),
                   data = base_af)
      mbam <- lmer(log(cort1_correct + 1) ~ t_db_av + lat_min + (1|band),
                   data = base_am)
      mbn <- lm(log(cort1_correct + 1) ~ t_db_av + lat_min,
                data = base_n)
      
      t_1 <- tab_model(mbaf, mbam, mbn, 
                       dv.labels = c("Adult Female Baseline", "Adult Male Baseline", "Nestling Baseline"),
                       pred.labels = c("Intercept", "Temperature (C)", "Capture 2", "Capture 3", "Latency (min)"),
                       title = "Average temperature prior day.")
      saveRDS(t_1, here::here("5_other_outputs/table8.rds"))
      
      # make plots for these three on normal scale and on log scale
      
      p1 <- ggplot(data = base_af, mapping = aes(x = t_db_av, y = log(cort1_correct + 1), col = "slateblue")) +
        geom_point(col = "slateblue", size = 0.6, alpha = 0.6) + geom_smooth(col = "coral3", method = "loess") + 
        theme_classic() + xlab("Avg Temp Prior 3 Days (C)") + ylim(0.35, 5) +
        ylab(expression(paste("log(Corticosterone ng/", mu, "l)"))) + ggtitle("Adult Females")
      
      p2 <- ggplot(data = base_am, mapping = aes(x = t_db_av, y = log(cort1_correct + 1), col = "slateblue")) +
        geom_point(col = "slateblue", size = 0.6, alpha = 0.6) + geom_smooth(col = "coral3", method = "loess") +  
        theme_classic() + xlab("Avg Temp Prior 3 Days (C)") + ylim(0.35, 5) +
        ylab(expression(paste("log(Corticosterone ng/", mu, "l)"))) + ggtitle("Adult Males")
      
      p3 <- ggplot(data = base_n, mapping = aes(x = t_db_av, y = log(cort1_correct + 1), col = "slateblue")) +
        geom_point(col = "slateblue", size = 0.6, alpha = 0.6) + geom_smooth(col = "coral3", method = "loess") +  
        theme_classic() + xlab("Avg Temp Prior 3 Days (C)") + ylim(0.35, 5) + 
        ylab(expression(paste("log(Corticosterone ng/", mu, "l)"))) + ggtitle("Nestlings")
      
      g1 <- ggarrange(p1, p2, p3, ncol = 3, nrow = 1)
      
      ggsave(here::here("3_r_scripts/fig8.png"), plot = g1, width = 7.8, height = 2.7, units = "in", device = "png")
      
      
      
# Stress cort ----            
  ## make datasets
      stress_af <- subset(base_af, base_af$type2 == "S")
      stress_am <- subset(base_am, base_am$type2 == "S")
      stress_n <- subset(base_n, base_n$type2 == "S")
### Stress cort 3 hours prior or morning of ----
      # morning of baseline
      mbaf <- lmer(cort2_correct ~ t_m_av + as.factor(cap_num) + lat_min + (1|band),
                   data = stress_af)
      mbam <- lmer(cort2_correct ~ t_3cap_av + lat_min + (1|band),
                   data = stress_am)
      mbn <- lmer(cort2_correct ~ t_3cap_av + lat_min + (1|year),
                data = stress_n)
      
      t_1 <- tab_model(mbaf, mbam, mbn, 
                       dv.labels = c("Adult Female Stress Induced", "Adult Male Stress Induced", "Nestling Stress Induced"),
                       pred.labels = c("Intercept", "Temperature 6-8am (C)", "Capture 2", "Capture 3", "Latency (min)", "Temperature 3 hrs prior (C)"),
                       title = "Average temperature 3 hours before capture.")
      saveRDS(t_1, here::here("5_other_outputs/table1s.rds"))
      
      # make plots for these three on normal scale and on log scale
      
      p1 <- ggplot(data = stress_af, mapping = aes(x = t_m_av, y = cort2_correct, col = "slateblue")) +
        geom_point(col = "slateblue", size = 0.6, alpha = 0.6) + geom_smooth(col = "coral3", method = "loess") + 
        theme_classic() + xlab("Avg Temp 6-8 a.m. (C)") + ylim(0, 100) +
        ylab(expression(paste("Corticosterone ng/", mu, "l"))) + ggtitle("Adult Females")
      
      p2 <- ggplot(data = stress_am, mapping = aes(x = t_3cap_av, y = cort2_correct, col = "slateblue")) +
        geom_point(col = "slateblue", size = 0.6, alpha = 0.6) + geom_smooth(col = "coral3", method = "loess") +  
        theme_classic() + xlab("Avg Temp 3 Hours Prior (C)") + ylim(0, 100) +
        ylab(expression(paste("Corticosterone ng/", mu, "l"))) + ggtitle("Adult Males")
      
      p3 <- ggplot(data = stress_n, mapping = aes(x = t_3cap_av, y = cort2_correct, col = "slateblue")) +
        geom_point(col = "slateblue", size = 0.6, alpha = 0.6) + geom_smooth(col = "coral3", method = "loess") +  
        theme_classic() + xlab("Avg Temp 3 Hours Prior (C)") + ylim(0, 100) +
        ylab(expression(paste("Corticosterone ng/", mu, "l"))) + ggtitle("Nestlings")
      
      g1 <- ggarrange(p1, p2, p3, ncol = 3, nrow = 1)
      
      ggsave(here::here("3_r_scripts/fig1s.png"), plot = g1, width = 7.8, height = 2.7, units = "in", device = "png")
      
      # morning of baseline
      mbaf <- lmer(log(cort2_correct + 1) ~ t_m_av + as.factor(cap_num) + lat_min + (1|band),
                   data = stress_af)
      mbam <- lm(log(cort2_correct + 1) ~ t_3cap_av + lat_min,
                   data = stress_am)
      mbn <- lm(log(cort2_correct + 1) ~ t_3cap_av + lat_min,
                data = stress_n)
      
      t_1 <- tab_model(mbaf, mbam, mbn, 
                       dv.labels = c("Adult Female Stress Induced", "Adult Male Stress Induced", "Nestling Stress Induced"),
                       pred.labels = c("Intercept", "Temperature 6-8am (C)", "Capture 2", "Capture 3", "Latency (min)", "Temperature 3 hrs prior (C)"),
                       title = "Average temperature 3 hours before capture.")
      saveRDS(t_1, here::here("5_other_outputs/table2s.rds"))
      
      # make plots for these three on normal scale and on log scale
      
      p1 <- ggplot(data = stress_af, mapping = aes(x = t_m_av, y = log(cort2_correct + 1), col = "slateblue")) +
        geom_point(col = "slateblue", size = 0.6, alpha = 0.6) + geom_smooth(col = "coral3", method = "loess") + 
        theme_classic() + xlab("Avg Temp 6-8 a.m. (C)") + ylim(0.35, 5) +
        ylab(expression(paste("log(Corticosterone ng/", mu, "l)"))) + ggtitle("Adult Females")
      
      p2 <- ggplot(data = stress_am, mapping = aes(x = t_3cap_av, y = log(cort2_correct + 1), col = "slateblue")) +
        geom_point(col = "slateblue", size = 0.6, alpha = 0.6) + geom_smooth(col = "coral3", method = "loess") +  
        theme_classic() + xlab("Avg Temp 3 Hours Prior (C)") + ylim(0.35, 5) +
        ylab(expression(paste("log(Corticosterone ng/", mu, "l)"))) + ggtitle("Adult Males")
      
      p3 <- ggplot(data = stress_n, mapping = aes(x = t_3cap_av, y = log(cort2_correct + 1), col = "slateblue")) +
        geom_point(col = "slateblue", size = 0.6, alpha = 0.6) + geom_smooth(col = "coral3", method = "loess") +  
        theme_classic() + xlab("Avg Temp 3 Hours Prior (C)") + ylim(0.35, 5) + 
        ylab(expression(paste("log(Corticosterone ng/", mu, "l)"))) + ggtitle("Nestlings")
      
      g1 <- ggarrange(p1, p2, p3, ncol = 3, nrow = 1)
      
      ggsave(here::here("3_r_scripts/fig2s.png"), plot = g1, width = 7.8, height = 2.7, units = "in", device = "png")
      
### Stress cort avg temperature from previous night ----
      
      # night before average temperature
      mbaf <- lmer(cort2_correct ~ t_nb_av + as.factor(cap_num) + lat_min + (1|band),
                   data = stress_af)
      mbam <- lmer(cort2_correct ~ t_nb_av + lat_min + (1|band),
                   data = stress_am)
      mbn <- lm(cort2_correct ~ t_nb_av + lat_min,
                data = stress_n)
      
      t_1 <- tab_model(mbaf, mbam, mbn, 
                       dv.labels = c("Adult Female Stress Induced", "Adult Male Stress Induced", "Nestling Stress Induced"),
                       pred.labels = c("Intercept", "Temperature (C)", "Capture 2", "Capture 3", "Latency (min)"),
                       title = "Low temperature previous night.")
      saveRDS(t_1, here::here("5_other_outputs/table3s.rds"))
      
      # make plots for these three on normal scale and on log scale
      
      p1 <- ggplot(data = stress_af, mapping = aes(x = t_nb_av, y = cort2_correct, col = "slateblue")) +
        geom_point(col = "slateblue", size = 0.6, alpha = 0.6) + geom_smooth(col = "coral3", method = "loess") + ylim(0, 100) + 
        theme_classic() + xlab("Avg Temp Last Night (C)") + 
        ylab(expression(paste("Corticosterone ng/", mu, "l"))) + ggtitle("Adult Females")
      
      p2 <- ggplot(data = stress_am, mapping = aes(x = t_nb_av, y = cort2_correct, col = "slateblue")) +
        geom_point(col = "slateblue", size = 0.6, alpha = 0.6) + geom_smooth(col = "coral3", method = "loess") +  
        theme_classic() + xlab("Avg Temp Last Night (C)") + 
        ylab(expression(paste("Corticosterone ng/", mu, "l"))) + ggtitle("Adult Males") + ylim(0, 100)
      
      p3 <- ggplot(data = stress_n, mapping = aes(x = t_nb_av, y = cort2_correct, col = "slateblue")) +
        geom_point(col = "slateblue", size = 0.6, alpha = 0.6) + geom_smooth(col = "coral3", method = "loess") +  
        theme_classic() + xlab("Avg Temp Last Night (C)") + 
        ylab(expression(paste("Corticosterone ng/", mu, "l"))) + ggtitle("Nestlings")
      
      g1 <- ggarrange(p1, p2, p3, ncol = 3, nrow = 1)
      
      ggsave(here::here("3_r_scripts/fig3s.png"), plot = g1, width = 7.8, height = 2.7, units = "in", device = "png")
      
      # log cort night before average
      mbaf <- lmer(log(cort2_correct + 1) ~ t_nb_av + as.factor(cap_num) + lat_min + (1|band),
                   data = stress_af)
      mbam <- lmer(log(cort2_correct + 1) ~ t_nb_av + lat_min + (1|band),
                   data = stress_am)
      mbn <- lm(log(cort2_correct + 1) ~ t_nb_av + lat_min,
                data = stress_n)
      
      t_1 <- tab_model(mbaf, mbam, mbn, 
                       dv.labels = c("Adult Female Stress Induced", "Adult Male Stress Induced", "Nestling Stress Induced"),
                       pred.labels = c("Intercept", "Temperature (C)", "Capture 2", "Capture 3", "Latency (min)"),
                       title = "Average temperature 3 hours before capture.")
      saveRDS(t_1, here::here("5_other_outputs/table4s.rds"))
      
      # make plots for these three on normal scale and on log scale
      
      p1 <- ggplot(data = stress_af, mapping = aes(x = t_nb_av, y = log(cort2_correct + 1), col = "slateblue")) +
        geom_point(col = "slateblue", size = 0.6, alpha = 0.6) + geom_smooth(col = "coral3", method = "loess") + 
        theme_classic() + xlab("Avg Temp Last Night (C)") + ylim(0.35, 5) +
        ylab(expression(paste("log(Corticosterone ng/", mu, "l)"))) + ggtitle("Adult Females")
      
      p2 <- ggplot(data = stress_am, mapping = aes(x = t_nb_av, y = log(cort2_correct + 1), col = "slateblue")) +
        geom_point(col = "slateblue", size = 0.6, alpha = 0.6) + geom_smooth(col = "coral3", method = "loess") +  
        theme_classic() + xlab("Avg Temp Last Night (C)") + ylim(0.35, 5) +
        ylab(expression(paste("log(Corticosterone ng/", mu, "l)"))) + ggtitle("Adult Males")
      
      p3 <- ggplot(data = stress_n, mapping = aes(x = t_nb_av, y = log(cort2_correct + 1), col = "slateblue")) +
        geom_point(col = "slateblue", size = 0.6, alpha = 0.6) + geom_smooth(col = "coral3", method = "loess") +  
        theme_classic() + xlab("Avg Temp Last Night (C)") + ylim(0.35, 5) + 
        ylab(expression(paste("log(Corticosterone ng/", mu, "l)"))) + ggtitle("Nestlings")
      
      g1 <- ggarrange(p1, p2, p3, ncol = 3, nrow = 1)
      
      ggsave(here::here("3_r_scripts/fig4s.png"), plot = g1, width = 7.8, height = 2.7, units = "in", device = "png")
      
      
      
### Stress vs average previous day ----           
      # average temperature previous day
      mbaf <- lmer(cort2_correct ~ t_db_av + as.factor(cap_num) + lat_min + (1|band),
                   data = stress_af)
      mbam <- lmer(cort2_correct ~ t_db_av + lat_min + (1|band),
                   data = stress_am)
      mbn <- lm(cort2_correct ~ t_db_av + lat_min,
                data = stress_n)
      
      t_1 <- tab_model(mbaf, mbam, mbn, 
                       dv.labels = c("Adult Female Stress Induced", "Adult Male Stress Induced", "Nestling Stress Induced"),
                       pred.labels = c("Intercept", "Temperature (C)", "Capture 2", "Capture 3", "Latency (min)"),
                       title = "Average temperature prior day.")
      saveRDS(t_1, here::here("5_other_outputs/table5s.rds"))
      
      # make plots for these three on normal scale and on log scale
      
      p1 <- ggplot(data = stress_af, mapping = aes(x = t_db_av, y = cort2_correct, col = "slateblue")) +
        geom_point(col = "slateblue", size = 0.6, alpha = 0.6) + geom_smooth(col = "coral3", method = "loess") + ylim(0, 100) + 
        theme_classic() + xlab("Avg Temp Prior Day (C)") + 
        ylab(expression(paste("Corticosterone ng/", mu, "l"))) + ggtitle("Adult Females")
      
      p2 <- ggplot(data = stress_am, mapping = aes(x = t_db_av, y = cort2_correct, col = "slateblue")) +
        geom_point(col = "slateblue", size = 0.6, alpha = 0.6) + geom_smooth(col = "coral3", method = "loess") +  
        theme_classic() + xlab("Avg Temp Prior Day (C)") + 
        ylab(expression(paste("Corticosterone ng/", mu, "l"))) + ggtitle("Adult Males") + ylim(0, 100)
      
      p3 <- ggplot(data = stress_n, mapping = aes(x = t_db_av, y = cort2_correct, col = "slateblue")) +
        geom_point(col = "slateblue", size = 0.6, alpha = 0.6) + geom_smooth(col = "coral3", method = "loess") +  
        theme_classic() + xlab("Avg Temp Prior Day (C)") + 
        ylab(expression(paste("Corticosterone ng/", mu, "l"))) + ggtitle("Nestlings") + ylim(0, 100)
      
      g1 <- ggarrange(p1, p2, p3, ncol = 3, nrow = 1)
      
      ggsave(here::here("3_r_scripts/fig5s.png"), plot = g1, width = 7.8, height = 2.7, units = "in", device = "png")
      
      # log cort night before average
      mbaf <- lmer(log(cort2_correct + 1) ~ t_db_av + as.factor(cap_num) + lat_min + (1|band),
                   data = stress_af)
      mbam <- lmer(log(cort2_correct + 1) ~ t_db_av + lat_min + (1|band),
                   data = stress_am)
      mbn <- lm(log(cort2_correct + 1) ~ t_db_av + lat_min,
                data = stress_n)
      
      t_1 <- tab_model(mbaf, mbam, mbn, 
                       dv.labels = c("Adult Female Stress Induced", "Adult Male Stress Induced", "Nestling Stress Induced"),
                       pred.labels = c("Intercept", "Temperature (C)", "Capture 2", "Capture 3", "Latency (min)"),
                       title = "Average temperature prior day.")
      saveRDS(t_1, here::here("5_other_outputs/table6s.rds"))
      
      # make plots for these three on normal scale and on log scale
      
      p1 <- ggplot(data = stress_af, mapping = aes(x = t_db_av, y = log(cort2_correct + 1), col = "slateblue")) +
        geom_point(col = "slateblue", size = 0.6, alpha = 0.6) + geom_smooth(col = "coral3", method = "loess") + 
        theme_classic() + xlab("Avg Temp Prior Day (C)") + ylim(0.35, 5) +
        ylab(expression(paste("log(Corticosterone ng/", mu, "l)"))) + ggtitle("Adult Females")
      
      p2 <- ggplot(data = stress_am, mapping = aes(x = t_db_av, y = log(cort2_correct + 1), col = "slateblue")) +
        geom_point(col = "slateblue", size = 0.6, alpha = 0.6) + geom_smooth(col = "coral3", method = "loess") +  
        theme_classic() + xlab("Avg Temp Prior Day (C)") + ylim(0.35, 5) +
        ylab(expression(paste("log(Corticosterone ng/", mu, "l)"))) + ggtitle("Adult Males")
      
      p3 <- ggplot(data = stress_n, mapping = aes(x = t_db_av, y = log(cort2_correct + 1), col = "slateblue")) +
        geom_point(col = "slateblue", size = 0.6, alpha = 0.6) + geom_smooth(col = "coral3", method = "loess") +  
        theme_classic() + xlab("Avg Temp Prior Day (C)") + ylim(0.35, 5) + 
        ylab(expression(paste("log(Corticosterone ng/", mu, "l)"))) + ggtitle("Nestlings")
      
      g1 <- ggarrange(p1, p2, p3, ncol = 3, nrow = 1)
      
      ggsave(here::here("3_r_scripts/fig6s.png"), plot = g1, width = 7.8, height = 2.7, units = "in", device = "png")
      
      
### Stress vs high previous day ----           
      # average temperature previous day
      mbaf <- lmer(cort2_correct ~ t_db_hi + as.factor(cap_num) + lat_min + (1|band),
                   data = stress_af)
      mbam <- lmer(cort2_correct ~ t_db_hi + lat_min + (1|band),
                   data = stress_am)
      mbn <- lm(cort2_correct ~ t_db_hi + lat_min,
                data = stress_n)
      
      t_1 <- tab_model(mbaf, mbam, mbn, 
                       dv.labels = c("Adult Female Stress Induced", "Adult Male Stress Induced", "Nestling Stress Induced"),
                       pred.labels = c("Intercept", "Temperature (C)", "Capture 2", "Capture 3", "Latency (min)"),
                       title = "High temperature prior day.")
      saveRDS(t_1, here::here("5_other_outputs/table5bs.rds"))
      
      # make plots for these three on normal scale and on log scale
      
      p1 <- ggplot(data = stress_af, mapping = aes(x = t_db_hi, y = cort2_correct, col = "slateblue")) +
        geom_point(col = "slateblue", size = 0.6, alpha = 0.6) + geom_smooth(col = "coral3", method = "loess") + ylim(0, 100) + 
        theme_classic() + xlab("High Temp Prior Day (C)") + 
        ylab(expression(paste("Corticosterone ng/", mu, "l"))) + ggtitle("Adult Females")
      
      p2 <- ggplot(data = stress_am, mapping = aes(x = t_db_hi, y = cort2_correct, col = "slateblue")) +
        geom_point(col = "slateblue", size = 0.6, alpha = 0.6) + geom_smooth(col = "coral3", method = "loess") +  
        theme_classic() + xlab("High Temp Prior Day (C)") + 
        ylab(expression(paste("Corticosterone ng/", mu, "l"))) + ggtitle("Adult Males") + ylim(0, 100)
      
      p3 <- ggplot(data = stress_n, mapping = aes(x = t_db_hi, y = cort2_correct, col = "slateblue")) +
        geom_point(col = "slateblue", size = 0.6, alpha = 0.6) + geom_smooth(col = "coral3", method = "loess") +  
        theme_classic() + xlab("High Temp Prior Day (C)") + 
        ylab(expression(paste("Corticosterone ng/", mu, "l"))) + ggtitle("Nestlings") + ylim(0, 100)
      
      g1 <- ggarrange(p1, p2, p3, ncol = 3, nrow = 1)
      
      ggsave(here::here("3_r_scripts/fig5bs.png"), plot = g1, width = 7.8, height = 2.7, units = "in", device = "png")
      
      # log cort night before average
      mbaf <- lmer(log(cort2_correct + 1) ~ t_db_hi + as.factor(cap_num) + lat_min + (1|band),
                   data = stress_af)
      mbam <- lmer(log(cort2_correct + 1) ~ t_db_hi + lat_min + (1|band),
                   data = stress_am)
      mbn <- lm(log(cort2_correct + 1) ~ t_db_hi + lat_min,
                data = stress_n)
      
      t_1 <- tab_model(mbaf, mbam, mbn, 
                       dv.labels = c("Adult Female Stress Induced", "Adult Male Stress Induced", "Nestling Stress Induced"),
                       pred.labels = c("Intercept", "Temperature (C)", "Capture 2", "Capture 3", "Latency (min)"),
                       title = "High temperature prior day.")
      saveRDS(t_1, here::here("5_other_outputs/table6bs.rds"))
      
      # make plots for these three on normal scale and on log scale
      
      p1 <- ggplot(data = stress_af, mapping = aes(x = t_db_hi, y = log(cort2_correct + 1), col = "slateblue")) +
        geom_point(col = "slateblue", size = 0.6, alpha = 0.6) + geom_smooth(col = "coral3", method = "loess") + 
        theme_classic() + xlab("High Temp Prior Day (C)") + ylim(0.35, 5) +
        ylab(expression(paste("log(Corticosterone ng/", mu, "l)"))) + ggtitle("Adult Females")
      
      p2 <- ggplot(data = stress_am, mapping = aes(x = t_db_hi, y = log(cort2_correct + 1), col = "slateblue")) +
        geom_point(col = "slateblue", size = 0.6, alpha = 0.6) + geom_smooth(col = "coral3", method = "loess") +  
        theme_classic() + xlab("High Temp Prior Day (C)") + ylim(0.35, 5) +
        ylab(expression(paste("log(Corticosterone ng/", mu, "l)"))) + ggtitle("Adult Males")
      
      p3 <- ggplot(data = stress_n, mapping = aes(x = t_db_hi, y = log(cort2_correct + 1), col = "slateblue")) +
        geom_point(col = "slateblue", size = 0.6, alpha = 0.6) + geom_smooth(col = "coral3", method = "loess") +  
        theme_classic() + xlab("High Temp Prior Day (C)") + ylim(0.35, 5) + 
        ylab(expression(paste("log(Corticosterone ng/", mu, "l)"))) + ggtitle("Nestlings")
      
      g1 <- ggarrange(p1, p2, p3, ncol = 3, nrow = 1)
      
      ggsave(here::here("3_r_scripts/fig6bs.png"), plot = g1, width = 7.8, height = 2.7, units = "in", device = "png")
      
      
### Stress Avg temp previous 3 days ----            
      # average temperature previous 3 day
      mbaf <- lmer(cort2_correct ~ t_3b_av + as.factor(cap_num) + lat_min + (1|band),
                   data = stress_af)
      mbam <- lmer(cort2_correct ~ t_3b_av + lat_min + (1|band),
                   data = stress_am)
      mbn <- lm(cort2_correct ~ t_3b_av + lat_min,
                data = stress_n)
      
      t_1 <- tab_model(mbaf, mbam, mbn, 
                       dv.labels = c("Adult Female Stress Induced", "Adult Male Stress Induced", "Nestling Stress Induced"),
                       pred.labels = c("Intercept", "Temperature (C)", "Capture 2", "Capture 3", "Latency (min)"),
                       title = "Average temperature prior 3 days.")
      saveRDS(t_1, here::here("5_other_outputs/table7s.rds"))
      
      # make plots for these three on normal scale and on log scale
      
      p1 <- ggplot(data = stress_af, mapping = aes(x = t_3b_av, y = cort2_correct, col = "slateblue")) +
        geom_point(col = "slateblue", size = 0.6, alpha = 0.6) + geom_smooth(col = "coral3", method = "loess") + ylim(0, 100) + 
        theme_classic() + xlab("Avg Temp Prior 3 Days (C)") + 
        ylab(expression(paste("Corticosterone ng/", mu, "l"))) + ggtitle("Adult Females")
      
      p2 <- ggplot(data = stress_am, mapping = aes(x = t_3b_av, y = cort2_correct, col = "slateblue")) +
        geom_point(col = "slateblue", size = 0.6, alpha = 0.6) + geom_smooth(col = "coral3", method = "loess") +  
        theme_classic() + xlab("Avg Temp Prior 3 Days (C)") + 
        ylab(expression(paste("Corticosterone ng/", mu, "l"))) + ggtitle("Adult Males") + ylim(0, 100)
      
      p3 <- ggplot(data = stress_n, mapping = aes(x = t_3b_av, y = cort2_correct, col = "slateblue")) +
        geom_point(col = "slateblue", size = 0.6, alpha = 0.6) + geom_smooth(col = "coral3", method = "loess") +  
        theme_classic() + xlab("Avg Temp Prior 3 Days (C)") + 
        ylab(expression(paste("Corticosterone ng/", mu, "l"))) + ggtitle("Nestlings") + ylim(0, 100)
      
      g1 <- ggarrange(p1, p2, p3, ncol = 3, nrow = 1)
      
      ggsave(here::here("3_r_scripts/fig7s.png"), plot = g1, width = 7.8, height = 2.7, units = "in", device = "png")
      
      # log cort night before average
      mbaf <- lmer(log(cort2_correct + 1) ~ t_db_av + as.factor(cap_num) + lat_min + (1|band),
                   data = stress_af)
      mbam <- lmer(log(cort2_correct + 1) ~ t_db_av + lat_min + (1|band),
                   data = stress_am)
      mbn <- lm(log(cort2_correct + 1) ~ t_db_av + lat_min,
                data = stress_n)
      
      t_1 <- tab_model(mbaf, mbam, mbn, 
                       dv.labels = c("Adult Female Stress Induced", "Adult Male Stress Induced", "Nestling Stress Induced"),
                       pred.labels = c("Intercept", "Temperature (C)", "Capture 2", "Capture 3", "Latency (min)"),
                       title = "Average temperature prior day.")
      saveRDS(t_1, here::here("5_other_outputs/table8s.rds"))
      
      # make plots for these three on normal scale and on log scale
      
      p1 <- ggplot(data = stress_af, mapping = aes(x = t_db_av, y = log(cort2_correct + 1), col = "slateblue")) +
        geom_point(col = "slateblue", size = 0.6, alpha = 0.6) + geom_smooth(col = "coral3", method = "loess") + 
        theme_classic() + xlab("Avg Temp Prior 3 Days (C)") + ylim(0.35, 5) +
        ylab(expression(paste("log(Corticosterone ng/", mu, "l)"))) + ggtitle("Adult Females")
      
      p2 <- ggplot(data = stress_am, mapping = aes(x = t_db_av, y = log(cort2_correct + 1), col = "slateblue")) +
        geom_point(col = "slateblue", size = 0.6, alpha = 0.6) + geom_smooth(col = "coral3", method = "loess") +  
        theme_classic() + xlab("Avg Temp Prior 3 Days (C)") + ylim(0.35, 5) +
        ylab(expression(paste("log(Corticosterone ng/", mu, "l)"))) + ggtitle("Adult Males")
      
      p3 <- ggplot(data = stress_n, mapping = aes(x = t_db_av, y = log(cort2_correct + 1), col = "slateblue")) +
        geom_point(col = "slateblue", size = 0.6, alpha = 0.6) + geom_smooth(col = "coral3", method = "loess") +  
        theme_classic() + xlab("Avg Temp Prior 3 Days (C)") + ylim(0.35, 5) + 
        ylab(expression(paste("log(Corticosterone ng/", mu, "l)"))) + ggtitle("Nestlings")
      
      g1 <- ggarrange(p1, p2, p3, ncol = 3, nrow = 1)
      
      ggsave(here::here("3_r_scripts/fig8s.png"), plot = g1, width = 7.8, height = 2.7, units = "in", device = "png")
      
      
      
      
# Post dex cort ----
    ## make datasets
      stress_af <- subset(base_af, base_af$type3 == "D")
      stress_am <- subset(base_am, base_am$type3 == "D")
      stress_n <- subset(base_n, base_n$type3 == "D")
   
### Dex cort 3 hours prior or morning of ----
      # morning of baseline
      mbaf <- lmer(cort3_correct ~ t_m_av + as.factor(cap_num) + lat_min + (1|band),
                   data = stress_af)
      mbam <- lmer(cort3_correct ~ t_3cap_av + lat_min + (1|band),
                   data = stress_am)
      mbn <- lmer(cort3_correct ~ t_3cap_av + lat_min + (1|year),
                  data = stress_n)
      
      t_1 <- tab_model(mbaf, mbam, mbn, 
                       dv.labels = c("Adult Female Post-Dex", "Adult Male Post-Dex", "Nestling Post-Dex"),
                       pred.labels = c("Intercept", "Temperature 6-8am (C)", "Capture 2", "Capture 3", "Latency (min)", "Temperature 3 hrs prior (C)"),
                       title = "Average temperature 3 hours before capture.")
      saveRDS(t_1, here::here("5_other_outputs/table1d.rds"))
      
      # make plots for these three on normal scale and on log scale
      
      p1 <- ggplot(data = stress_af, mapping = aes(x = t_m_av, y = cort3_correct, col = "slateblue")) +
        geom_point(col = "slateblue", size = 0.6, alpha = 0.6) + geom_smooth(col = "coral3", method = "loess") + 
        theme_classic() + xlab("Avg Temp 6-8 a.m. (C)") + ylim(0, 50) +
        ylab(expression(paste("Corticosterone ng/", mu, "l"))) + ggtitle("Adult Females")
      
      p2 <- ggplot(data = stress_am, mapping = aes(x = t_3cap_av, y = cort3_correct, col = "slateblue")) +
        geom_point(col = "slateblue", size = 0.6, alpha = 0.6) + geom_smooth(col = "coral3", method = "loess") +  
        theme_classic() + xlab("Avg Temp 3 Hours Prior (C)") + ylim(0, 50) +
        ylab(expression(paste("Corticosterone ng/", mu, "l"))) + ggtitle("Adult Males")
      
      p3 <- ggplot(data = stress_n, mapping = aes(x = t_3cap_av, y = cort3_correct, col = "slateblue")) +
        geom_point(col = "slateblue", size = 0.6, alpha = 0.6) + geom_smooth(col = "coral3", method = "loess") +  
        theme_classic() + xlab("Avg Temp 3 Hours Prior (C)") + ylim(0, 50) +
        ylab(expression(paste("Corticosterone ng/", mu, "l"))) + ggtitle("Nestlings")
      
      g1 <- ggarrange(p1, p2, p3, ncol = 3, nrow = 1)
      
      ggsave(here::here("3_r_scripts/fig1d.png"), plot = g1, width = 7.8, height = 2.7, units = "in", device = "png")
      
      # morning of baseline
      mbaf <- lmer(log(cort3_correct + 1) ~ t_m_av + as.factor(cap_num) + lat_min + (1|band),
                   data = stress_af)
      mbam <- lm(log(cort3_correct + 1) ~ t_3cap_av + lat_min,
                 data = stress_am)
      mbn <- lm(log(cort3_correct + 1) ~ t_3cap_av + lat_min,
                data = stress_n)
      
      t_1 <- tab_model(mbaf, mbam, mbn, 
                       dv.labels = c("Adult Female Post-Dex", "Adult Male Post-Dex", "Nestling Post-Dex"),
                       pred.labels = c("Intercept", "Temperature 6-8am (C)", "Capture 2", "Capture 3", "Latency (min)", "Temperature 3 hrs prior (C)"),
                       title = "Average temperature 3 hours before capture.")
      saveRDS(t_1, here::here("5_other_outputs/table2d.rds"))
      
      # make plots for these three on normal scale and on log scale
      
      p1 <- ggplot(data = stress_af, mapping = aes(x = t_m_av, y = log(cort3_correct + 1), col = "slateblue")) +
        geom_point(col = "slateblue", size = 0.6, alpha = 0.6) + geom_smooth(col = "coral3", method = "loess") + 
        theme_classic() + xlab("Avg Temp 6-8 a.m. (C)") + ylim(0.35, 5) +
        ylab(expression(paste("log(Corticosterone ng/", mu, "l)"))) + ggtitle("Adult Females")
      
      p2 <- ggplot(data = stress_am, mapping = aes(x = t_3cap_av, y = log(cort3_correct + 1), col = "slateblue")) +
        geom_point(col = "slateblue", size = 0.6, alpha = 0.6) + geom_smooth(col = "coral3", method = "loess") +  
        theme_classic() + xlab("Avg Temp 3 Hours Prior (C)") + ylim(0.35, 5) +
        ylab(expression(paste("log(Corticosterone ng/", mu, "l)"))) + ggtitle("Adult Males")
      
      p3 <- ggplot(data = stress_n, mapping = aes(x = t_3cap_av, y = log(cort3_correct + 1), col = "slateblue")) +
        geom_point(col = "slateblue", size = 0.6, alpha = 0.6) + geom_smooth(col = "coral3", method = "loess") +  
        theme_classic() + xlab("Avg Temp 3 Hours Prior (C)") + ylim(0.35, 5) + 
        ylab(expression(paste("log(Corticosterone ng/", mu, "l)"))) + ggtitle("Nestlings")
      
      g1 <- ggarrange(p1, p2, p3, ncol = 3, nrow = 1)
      
      ggsave(here::here("3_r_scripts/fig2d.png"), plot = g1, width = 7.8, height = 2.7, units = "in", device = "png")
      
### Dex cort avg temperature from previous night ----
      
      # night before average temperature
      mbaf <- lmer(cort3_correct ~ t_nb_av + as.factor(cap_num) + lat_min + (1|band),
                   data = stress_af)
      mbam <- lmer(cort3_correct ~ t_nb_av + lat_min + (1|band),
                   data = stress_am)
      mbn <- lm(cort3_correct ~ t_nb_av + lat_min,
                data = stress_n)
      
      t_1 <- tab_model(mbaf, mbam, mbn, 
                       dv.labels = c("Adult Female Post-Dex", "Adult Male Post-Dex", "Nestling Post-Dex"),
                       pred.labels = c("Intercept", "Temperature (C)", "Capture 2", "Capture 3", "Latency (min)"),
                       title = "Low temperature previous night.")
      saveRDS(t_1, here::here("5_other_outputs/table3d.rds"))
      
      # make plots for these three on normal scale and on log scale
      
      p1 <- ggplot(data = stress_af, mapping = aes(x = t_nb_av, y = cort3_correct, col = "slateblue")) +
        geom_point(col = "slateblue", size = 0.6, alpha = 0.6) + geom_smooth(col = "coral3", method = "loess") + ylim(0, 50) + 
        theme_classic() + xlab("Avg Temp Last Night (C)") + 
        ylab(expression(paste("Corticosterone ng/", mu, "l"))) + ggtitle("Adult Females")
      
      p2 <- ggplot(data = stress_am, mapping = aes(x = t_nb_av, y = cort3_correct, col = "slateblue")) +
        geom_point(col = "slateblue", size = 0.6, alpha = 0.6) + geom_smooth(col = "coral3", method = "loess") +  
        theme_classic() + xlab("Avg Temp Last Night (C)") + 
        ylab(expression(paste("Corticosterone ng/", mu, "l"))) + ggtitle("Adult Males") + ylim(0, 50)
      
      p3 <- ggplot(data = stress_n, mapping = aes(x = t_nb_av, y = cort3_correct, col = "slateblue")) +
        geom_point(col = "slateblue", size = 0.6, alpha = 0.6) + geom_smooth(col = "coral3", method = "loess") +  
        theme_classic() + xlab("Avg Temp Last Night (C)") + ylim(0, 50) +
        ylab(expression(paste("Corticosterone ng/", mu, "l"))) + ggtitle("Nestlings")
      
      g1 <- ggarrange(p1, p2, p3, ncol = 3, nrow = 1)
      
      ggsave(here::here("3_r_scripts/fig3d.png"), plot = g1, width = 7.8, height = 2.7, units = "in", device = "png")
      
      # log cort night before average
      mbaf <- lmer(log(cort3_correct + 1) ~ t_nb_av + as.factor(cap_num) + lat_min + (1|band),
                   data = stress_af)
      mbam <- lmer(log(cort3_correct + 1) ~ t_nb_av + lat_min + (1|band),
                   data = stress_am)
      mbn <- lm(log(cort3_correct + 1) ~ t_nb_av + lat_min,
                data = stress_n)
      
      t_1 <- tab_model(mbaf, mbam, mbn, 
                       dv.labels = c("Adult Female Post-Dex", "Adult Male Post-Dex", "Nestling Post-Dex"),
                       pred.labels = c("Intercept", "Temperature (C)", "Capture 2", "Capture 3", "Latency (min)"),
                       title = "Average temperature 3 hours before capture.")
      saveRDS(t_1, here::here("5_other_outputs/table4d.rds"))
      
      # make plots for these three on normal scale and on log scale
      
      p1 <- ggplot(data = stress_af, mapping = aes(x = t_nb_av, y = log(cort3_correct + 1), col = "slateblue")) +
        geom_point(col = "slateblue", size = 0.6, alpha = 0.6) + geom_smooth(col = "coral3", method = "loess") + 
        theme_classic() + xlab("Avg Temp Last Night (C)") + ylim(0.35, 5) +
        ylab(expression(paste("log(Corticosterone ng/", mu, "l)"))) + ggtitle("Adult Females")
      
      p2 <- ggplot(data = stress_am, mapping = aes(x = t_nb_av, y = log(cort3_correct + 1), col = "slateblue")) +
        geom_point(col = "slateblue", size = 0.6, alpha = 0.6) + geom_smooth(col = "coral3", method = "loess") +  
        theme_classic() + xlab("Avg Temp Last Night (C)") + ylim(0.35, 5) +
        ylab(expression(paste("log(Corticosterone ng/", mu, "l)"))) + ggtitle("Adult Males")
      
      p3 <- ggplot(data = stress_n, mapping = aes(x = t_nb_av, y = log(cort3_correct + 1), col = "slateblue")) +
        geom_point(col = "slateblue", size = 0.6, alpha = 0.6) + geom_smooth(col = "coral3", method = "loess") +  
        theme_classic() + xlab("Avg Temp Last Night (C)") + ylim(0.35, 5) + 
        ylab(expression(paste("log(Corticosterone ng/", mu, "l)"))) + ggtitle("Nestlings")
      
      g1 <- ggarrange(p1, p2, p3, ncol = 3, nrow = 1)
      
      ggsave(here::here("3_r_scripts/fig4d.png"), plot = g1, width = 7.8, height = 2.7, units = "in", device = "png")
      
      
      
### Dex vs average previous day ----           
      # average temperature previous day
      mbaf <- lmer(cort3_correct ~ t_db_av + as.factor(cap_num) + lat_min + (1|band),
                   data = stress_af)
      mbam <- lmer(cort3_correct ~ t_db_av + lat_min + (1|band),
                   data = stress_am)
      mbn <- lm(cort3_correct ~ t_db_av + lat_min,
                data = stress_n)
      
      t_1 <- tab_model(mbaf, mbam, mbn, 
                       dv.labels = c("Adult Female Post-Dex", "Adult Male Post-Dex", "Nestling Post-Dex"),
                       pred.labels = c("Intercept", "Temperature (C)", "Capture 2", "Capture 3", "Latency (min)"),
                       title = "Average temperature prior day.")
      saveRDS(t_1, here::here("5_other_outputs/table5d.rds"))
      
      # make plots for these three on normal scale and on log scale
      
      p1 <- ggplot(data = stress_af, mapping = aes(x = t_db_av, y = cort3_correct, col = "slateblue")) +
        geom_point(col = "slateblue", size = 0.6, alpha = 0.6) + geom_smooth(col = "coral3", method = "loess") + ylim(0, 50) + 
        theme_classic() + xlab("Avg Temp Prior Day (C)") + 
        ylab(expression(paste("Corticosterone ng/", mu, "l"))) + ggtitle("Adult Females")
      
      p2 <- ggplot(data = stress_am, mapping = aes(x = t_db_av, y = cort3_correct, col = "slateblue")) +
        geom_point(col = "slateblue", size = 0.6, alpha = 0.6) + geom_smooth(col = "coral3", method = "loess") +  
        theme_classic() + xlab("Avg Temp Prior Day (C)") + 
        ylab(expression(paste("Corticosterone ng/", mu, "l"))) + ggtitle("Adult Males") + ylim(0, 50)
      
      p3 <- ggplot(data = stress_n, mapping = aes(x = t_db_av, y = cort3_correct, col = "slateblue")) +
        geom_point(col = "slateblue", size = 0.6, alpha = 0.6) + geom_smooth(col = "coral3", method = "loess") +  
        theme_classic() + xlab("Avg Temp Prior Day (C)") + 
        ylab(expression(paste("Corticosterone ng/", mu, "l"))) + ggtitle("Nestlings") + ylim(0, 50)
      
      g1 <- ggarrange(p1, p2, p3, ncol = 3, nrow = 1)
      
      ggsave(here::here("3_r_scripts/fig5d.png"), plot = g1, width = 7.8, height = 2.7, units = "in", device = "png")
      
      # log cort night before average
      mbaf <- lmer(log(cort3_correct + 1) ~ t_db_av + as.factor(cap_num) + lat_min + (1|band),
                   data = stress_af)
      mbam <- lmer(log(cort3_correct + 1) ~ t_db_av + lat_min + (1|band),
                   data = stress_am)
      mbn <- lm(log(cort3_correct + 1) ~ t_db_av + lat_min,
                data = stress_n)
      
      t_1 <- tab_model(mbaf, mbam, mbn, 
                       dv.labels = c("Adult Female Post-Dex", "Adult Male Post-Dex", "Nestling Post-Dex"),
                       pred.labels = c("Intercept", "Temperature (C)", "Capture 2", "Capture 3", "Latency (min)"),
                       title = "Average temperature prior day.")
      saveRDS(t_1, here::here("5_other_outputs/table6d.rds"))
      
      # make plots for these three on normal scale and on log scale
      
      p1 <- ggplot(data = stress_af, mapping = aes(x = t_db_av, y = log(cort3_correct + 1), col = "slateblue")) +
        geom_point(col = "slateblue", size = 0.6, alpha = 0.6) + geom_smooth(col = "coral3", method = "loess") + 
        theme_classic() + xlab("Avg Temp Prior Day (C)") + ylim(0.35, 5) +
        ylab(expression(paste("log(Corticosterone ng/", mu, "l)"))) + ggtitle("Adult Females")
      
      p2 <- ggplot(data = stress_am, mapping = aes(x = t_db_av, y = log(cort3_correct + 1), col = "slateblue")) +
        geom_point(col = "slateblue", size = 0.6, alpha = 0.6) + geom_smooth(col = "coral3", method = "loess") +  
        theme_classic() + xlab("Avg Temp Prior Day (C)") + ylim(0.35, 5) +
        ylab(expression(paste("log(Corticosterone ng/", mu, "l)"))) + ggtitle("Adult Males")
      
      p3 <- ggplot(data = stress_n, mapping = aes(x = t_db_av, y = log(cort3_correct + 1), col = "slateblue")) +
        geom_point(col = "slateblue", size = 0.6, alpha = 0.6) + geom_smooth(col = "coral3", method = "loess") +  
        theme_classic() + xlab("Avg Temp Prior Day (C)") + ylim(0.35, 5) + 
        ylab(expression(paste("log(Corticosterone ng/", mu, "l)"))) + ggtitle("Nestlings")
      
      g1 <- ggarrange(p1, p2, p3, ncol = 3, nrow = 1)
      
      ggsave(here::here("3_r_scripts/fig6d.png"), plot = g1, width = 7.8, height = 2.7, units = "in", device = "png")
      
      
### Dex vs high previous day ----           
      # average temperature previous day
      mbaf <- lmer(cort3_correct ~ t_db_hi + as.factor(cap_num) + lat_min + (1|band),
                   data = stress_af)
      mbam <- lmer(cort3_correct ~ t_db_hi + lat_min + (1|band),
                   data = stress_am)
      mbn <- lm(cort3_correct ~ t_db_hi + lat_min,
                data = stress_n)
      
      t_1 <- tab_model(mbaf, mbam, mbn, 
                       dv.labels = c("Adult Female Post-Dex", "Adult Male Post-Dex", "Nestling Post-Dex"),
                       pred.labels = c("Intercept", "Temperature (C)", "Capture 2", "Capture 3", "Latency (min)"),
                       title = "High temperature prior day.")
      saveRDS(t_1, here::here("5_other_outputs/table5bd.rds"))
      
      # make plots for these three on normal scale and on log scale
      
      p1 <- ggplot(data = stress_af, mapping = aes(x = t_db_hi, y = cort3_correct, col = "slateblue")) +
        geom_point(col = "slateblue", size = 0.6, alpha = 0.6) + geom_smooth(col = "coral3", method = "loess") + ylim(0, 50) + 
        theme_classic() + xlab("High Temp Prior Day (C)") + 
        ylab(expression(paste("Corticosterone ng/", mu, "l"))) + ggtitle("Adult Females")
      
      p2 <- ggplot(data = stress_am, mapping = aes(x = t_db_hi, y = cort3_correct, col = "slateblue")) +
        geom_point(col = "slateblue", size = 0.6, alpha = 0.6) + geom_smooth(col = "coral3", method = "loess") +  
        theme_classic() + xlab("High Temp Prior Day (C)") + 
        ylab(expression(paste("Corticosterone ng/", mu, "l"))) + ggtitle("Adult Males") + ylim(0, 50)
      
      p3 <- ggplot(data = stress_n, mapping = aes(x = t_db_hi, y = cort3_correct, col = "slateblue")) +
        geom_point(col = "slateblue", size = 0.6, alpha = 0.6) + geom_smooth(col = "coral3", method = "loess") +  
        theme_classic() + xlab("High Temp Prior Day (C)") + 
        ylab(expression(paste("Corticosterone ng/", mu, "l"))) + ggtitle("Nestlings") + ylim(0, 50)
      
      g1 <- ggarrange(p1, p2, p3, ncol = 3, nrow = 1)
      
      ggsave(here::here("3_r_scripts/fig5bd.png"), plot = g1, width = 7.8, height = 2.7, units = "in", device = "png")
      
      # log cort night before average
      mbaf <- lmer(log(cort3_correct + 1) ~ t_db_hi + as.factor(cap_num) + lat_min + (1|band),
                   data = stress_af)
      mbam <- lmer(log(cort3_correct + 1) ~ t_db_hi + lat_min + (1|band),
                   data = stress_am)
      mbn <- lm(log(cort3_correct + 1) ~ t_db_hi + lat_min,
                data = stress_n)
      
      t_1 <- tab_model(mbaf, mbam, mbn, 
                       dv.labels = c("Adult Female Post-Dex", "Adult Male Post-Dex", "Nestling Post-Dex"),
                       pred.labels = c("Intercept", "Temperature (C)", "Capture 2", "Capture 3", "Latency (min)"),
                       title = "High temperature prior day.")
      saveRDS(t_1, here::here("5_other_outputs/table6bd.rds"))
      
      # make plots for these three on normal scale and on log scale
      
      p1 <- ggplot(data = stress_af, mapping = aes(x = t_db_hi, y = log(cort3_correct + 1), col = "slateblue")) +
        geom_point(col = "slateblue", size = 0.6, alpha = 0.6) + geom_smooth(col = "coral3", method = "loess") + 
        theme_classic() + xlab("High Temp Prior Day (C)") + ylim(0.35, 5) +
        ylab(expression(paste("log(Corticosterone ng/", mu, "l)"))) + ggtitle("Adult Females")
      
      p2 <- ggplot(data = stress_am, mapping = aes(x = t_db_hi, y = log(cort3_correct + 1), col = "slateblue")) +
        geom_point(col = "slateblue", size = 0.6, alpha = 0.6) + geom_smooth(col = "coral3", method = "loess") +  
        theme_classic() + xlab("High Temp Prior Day (C)") + ylim(0.35, 5) +
        ylab(expression(paste("log(Corticosterone ng/", mu, "l)"))) + ggtitle("Adult Males")
      
      p3 <- ggplot(data = stress_n, mapping = aes(x = t_db_hi, y = log(cort3_correct + 1), col = "slateblue")) +
        geom_point(col = "slateblue", size = 0.6, alpha = 0.6) + geom_smooth(col = "coral3", method = "loess") +  
        theme_classic() + xlab("High Temp Prior Day (C)") + ylim(0.35, 5) + 
        ylab(expression(paste("log(Corticosterone ng/", mu, "l)"))) + ggtitle("Nestlings")
      
      g1 <- ggarrange(p1, p2, p3, ncol = 3, nrow = 1)
      
      ggsave(here::here("3_r_scripts/fig6bd.png"), plot = g1, width = 7.8, height = 2.7, units = "in", device = "png")
      
      
### Dex Avg temp previous 3 days ----            
      # average temperature previous 3 day
      mbaf <- lmer(cort3_correct ~ t_3b_av + as.factor(cap_num) + lat_min + (1|band),
                   data = stress_af)
      mbam <- lmer(cort3_correct ~ t_3b_av + lat_min + (1|band),
                   data = stress_am)
      mbn <- lm(cort3_correct ~ t_3b_av + lat_min,
                data = stress_n)
      
      t_1 <- tab_model(mbaf, mbam, mbn, 
                       dv.labels = c("Adult Female Post-Dex", "Adult Male Post-Dex", "Nestling Post-Dex"),
                       pred.labels = c("Intercept", "Temperature (C)", "Capture 2", "Capture 3", "Latency (min)"),
                       title = "Average temperature prior 3 days.")
      saveRDS(t_1, here::here("5_other_outputs/table7d.rds"))
      
      # make plots for these three on normal scale and on log scale
      
      p1 <- ggplot(data = stress_af, mapping = aes(x = t_3b_av, y = cort3_correct, col = "slateblue")) +
        geom_point(col = "slateblue", size = 0.6, alpha = 0.6) + geom_smooth(col = "coral3", method = "loess") + ylim(0, 50) + 
        theme_classic() + xlab("Avg Temp Prior 3 Days (C)") + 
        ylab(expression(paste("Corticosterone ng/", mu, "l"))) + ggtitle("Adult Females")
      
      p2 <- ggplot(data = stress_am, mapping = aes(x = t_3b_av, y = cort3_correct, col = "slateblue")) +
        geom_point(col = "slateblue", size = 0.6, alpha = 0.6) + geom_smooth(col = "coral3", method = "loess") +  
        theme_classic() + xlab("Avg Temp Prior 3 Days (C)") + 
        ylab(expression(paste("Corticosterone ng/", mu, "l"))) + ggtitle("Adult Males") + ylim(0, 50)
      
      p3 <- ggplot(data = stress_n, mapping = aes(x = t_3b_av, y = cort3_correct, col = "slateblue")) +
        geom_point(col = "slateblue", size = 0.6, alpha = 0.6) + geom_smooth(col = "coral3", method = "loess") +  
        theme_classic() + xlab("Avg Temp Prior 3 Days (C)") + 
        ylab(expression(paste("Corticosterone ng/", mu, "l"))) + ggtitle("Nestlings") + ylim(0, 50)
      
      g1 <- ggarrange(p1, p2, p3, ncol = 3, nrow = 1)
      
      ggsave(here::here("3_r_scripts/fig7d.png"), plot = g1, width = 7.8, height = 2.7, units = "in", device = "png")
      
      # log cort night before average
      mbaf <- lmer(log(cort3_correct + 1) ~ t_db_av + as.factor(cap_num) + lat_min + (1|band),
                   data = stress_af)
      mbam <- lmer(log(cort3_correct + 1) ~ t_db_av + lat_min + (1|band),
                   data = stress_am)
      mbn <- lm(log(cort3_correct + 1) ~ t_db_av + lat_min,
                data = stress_n)
      
      t_1 <- tab_model(mbaf, mbam, mbn, 
                       dv.labels = c("Adult Female Post-Dex", "Adult Male Post-Dex", "Nestling Post-Dex"),
                       pred.labels = c("Intercept", "Temperature (C)", "Capture 2", "Capture 3", "Latency (min)"),
                       title = "Average temperature prior day.")
      saveRDS(t_1, here::here("5_other_outputs/table8d.rds"))
      
      # make plots for these three on normal scale and on log scale
      
      p1 <- ggplot(data = stress_af, mapping = aes(x = t_db_av, y = log(cort3_correct + 1), col = "slateblue")) +
        geom_point(col = "slateblue", size = 0.6, alpha = 0.6) + geom_smooth(col = "coral3", method = "loess") + 
        theme_classic() + xlab("Avg Temp Prior 3 Days (C)") + ylim(0.35, 5) +
        ylab(expression(paste("log(Corticosterone ng/", mu, "l)"))) + ggtitle("Adult Females")
      
      p2 <- ggplot(data = stress_am, mapping = aes(x = t_db_av, y = log(cort3_correct + 1), col = "slateblue")) +
        geom_point(col = "slateblue", size = 0.6, alpha = 0.6) + geom_smooth(col = "coral3", method = "loess") +  
        theme_classic() + xlab("Avg Temp Prior 3 Days (C)") + ylim(0.35, 5) +
        ylab(expression(paste("log(Corticosterone ng/", mu, "l)"))) + ggtitle("Adult Males")
      
      p3 <- ggplot(data = stress_n, mapping = aes(x = t_db_av, y = log(cort3_correct + 1), col = "slateblue")) +
        geom_point(col = "slateblue", size = 0.6, alpha = 0.6) + geom_smooth(col = "coral3", method = "loess") +  
        theme_classic() + xlab("Avg Temp Prior 3 Days (C)") + ylim(0.35, 5) + 
        ylab(expression(paste("log(Corticosterone ng/", mu, "l)"))) + ggtitle("Nestlings")
      
      g1 <- ggarrange(p1, p2, p3, ncol = 3, nrow = 1)
      
      ggsave(here::here("3_r_scripts/fig8d.png"), plot = g1, width = 7.8, height = 2.7, units = "in", device = "png")
      
      
      
      