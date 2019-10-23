#####"Offensive Personnel vs. Men in the Box: A Football Causality Tutorial" #####
##### Zachary Binney, PhD                                                    #####
##### 10/23/2019                                                             #####

##### Setup #####

#Load packages
pacman::p_load(tidyverse, readxl, rstudioapi, lubridate, ggridges, brms, tidybayes, 
               modelr, gganimate, gifski, emmeans, gridExtra)

#Load data
current_path <- getActiveDocumentContext()$path
setwd(dirname(current_path))
pbp18 <- read_excel(path="./2018 Game Charting Pivot v2.xlsx",
                           sheet = "Data") %>% 
  select(Time, `OFF YDL`, `ESPN BOX`, QTR, rusher, RECEPT, OL, DOWN, TOGO, YARDS, RB, TE)
pbp17 <- read_excel(path="./2017 Game Charting Pivot.xlsx",
                    sheet = "Data") %>% 
  select(Time, `OFF YDL`, `ESPN BOX`, QTR, rusher, RECEPT, OL, DOWN, TOGO, YARDS, RB, TE)
pbp16 <- read_excel(path="./2016 Game Charting Pivot.xlsx",
                    sheet = "Data") %>% 
  select(Time, `OFF YDL`, `ESPN BOX`, QTR, rusher, RECEPT, OL, DOWN, TOGO, YARDS, RB, TE)








##### Data Wrangling #####

#Filter to get
  #Only runs
  #First 3 quarters, excluding last 2 minutes of first half
  #No scrambles
  #Outside opponent's 30-yard line
  #5 offensive linemen
  #First to third downs
pbp_analysis <- rbind(pbp16, pbp17, pbp18) %>%
  mutate(secs_left = hour(Time)*60 + minute(Time)) %>% 
  rename(off_ydl = `OFF YDL`,
         mib = `ESPN BOX`) %>% 
  filter(QTR %in% c(1,3) | (QTR == 2 & secs_left > 120),
         rusher == "rushed",
         is.na(RECEPT), #removes scrambles, which are marked as "scramble"
         off_ydl <= 70,
         OL == 5,
         DOWN <= 3,
         TOGO >= 2
         ) %>% 
  mutate(off_personnel = relevel(factor(str_c(RB, TE)), ref = "22"),
         success = case_when(DOWN == 1 & YARDS >= 0.45*TOGO ~ 1,
                             DOWN == 2 & YARDS >= 0.6*TOGO ~ 1,
                             DOWN >= 3 & YARDS >= TOGO ~ 1,
                             TRUE ~ 0),
         mib = case_when(mib <= 5 ~ 5,
                         mib >= 8 ~ 8,
                         TRUE ~ mib),
         mib = relevel(factor(mib, labels = c("<=5", "6", "7", "8+")), ref = "<=5"),
         TOGO_fct = factor(case_when(TOGO == 1 ~ "1",
                                     TOGO == 2 ~ "2",
                                     TRUE ~ "3+"))) %>% 
  filter(off_personnel %in% c("11", "12", "13", "21", "22")) %>%  #Most common personnel 
  mutate(off_personnel = relevel(factor(str_c(RB, TE), 
                                        levels = c("11", "12", "13", "21", "22")), 
                                 ref = "11"),
         DOWN = factor(DOWN)) %>% 
  select(off_ydl, off_personnel, mib, DOWN, TOGO, TOGO_fct, YARDS, success) %>% 
  filter(!is.na(mib))


##### Data Exploration and Checking #####

#Distribution of plays by offensive personnel
off <- pbp_analysis %>% 
  group_by(off_personnel) %>% 
  summarize(n = n()) %>% 
  mutate(pct = n / sum(n))
  
  #Are offensive personnel and defensive MIB associated as we'd expect?
  off_vs_def <- pbp_analysis %>% 
    group_by(off_personnel, mib) %>% 
    summarize(n = n()) %>% 
    mutate(pct = n / sum(n))
  
  off_vs_def %>%
    filter(!is.na(mib)) %>% 
    ggplot(aes(x = mib, y = pct)) +
    geom_bar(stat = "identity") +
    facet_wrap(~ off_personnel)
  
  
  #Plot yards gained by offensive personnel only
  ann_text <- pbp_analysis %>% 
    group_by(off_personnel) %>% 
    summarize(ypc = mean(YARDS), n = n())
  
  
  (fig1 <-  pbp_analysis %>% 
    ggplot(aes(x = YARDS, y = off_personnel, fill = off_personnel)) +
    geom_jitter(alpha = 0.1) +
    stat_density_ridges() +
    theme(legend.position="none") +
    xlim(c(-10, 20))+
      geom_segment(data = ann_text, aes(x = ypc, xend = ypc, y = as.numeric(off_personnel),
                                          yend = as.numeric(off_personnel) + .9),
                   color = "black") +
    labs(x = "Yards Gained", y = "Offensive Personnel Grouping",
      caption = "Vertical lines are mean yards per carry.
      N = 476 carries >20 (470)  or <-10 (6) yards not shown but included in all analyses."))
  
  ggsave("./Fig1.png", plot = fig1, scale = 0.9)
  
  
  
  
  #Plot yards gained by MIB only
  ann_text <- pbp_analysis %>% 
    filter(!is.na(mib)) %>% 
    group_by(mib) %>% 
    summarize(ypc = mean(YARDS))
  
  (fig2 <-  pbp_analysis %>% 
      filter(!is.na(mib)) %>% 
      ggplot(aes(x = YARDS, y = mib, fill = mib)) +
      geom_jitter(alpha = 0.1) +
      stat_density_ridges() +
      theme(legend.position="none") +
      xlim(c(-10, 20))+
      geom_segment(data = ann_text, aes(x = ypc, xend = ypc, y = as.numeric(mib),
                                          yend = as.numeric(mib) + .9),
                   color = "black") +
      labs(x = "Yards Gained", y = "Men in the Box",
           caption = "Vertical lines are mean yards per carry.
           N = 476 carries >20 (470)  or <-10 (6) yards not shown but included in all analyses."))
  
  ggsave("./Fig2.png", plot = fig2, scale = 0.9)
  
  

  
  
  
  
  

  
##### Statistical Models #####


#Run Bayesian lognormal model for all combinations of offensive personnel +/- MIB
#N = 6 total models, 3 adjusting for down and distance and 3 not
#All models apply a correction of +15 yards to the outcome to force all values to be
  #positive so the lognormal model will run. These 15 yards can simply be subtracted out later.
  
# b1 <- brm(YARDS + 15 ~ 1 + off_personnel, 
#          data = pbp_analysis, family = lognormal(),
#          prior = c(prior(normal(0, 5), class = b)),
#          iter = 2000, warmup = 1000, chains = 3, cores = 3)
# saveRDS(b1, file = "./b1.rds")
b1 <- readRDS("./b1.rds")


# b2 <- update(b1, newdata = pbp_analysis,
#              formula = YARDS + 15 ~ 1 + mib, 
#              chains = 3, cores = 3)
# saveRDS(b2, file = "./b2.rds")
b2 <- readRDS("./b2.rds")

# b3 <- update(b1, newdata = pbp_analysis,
#              formula = YARDS + 15 ~ 1 + off_personnel + mib,
#              chains = 3, cores = 3)
# saveRDS(b3, file = "./b3.rds")
b3 <- readRDS("./b3.rds")

# b4 <- update(b1, newdata = pbp_analysis,
#              formula = YARDS + 15 ~ 1 + off_personnel + DOWN + TOGO_fct,
#              chains = 3, cores = 3)
# saveRDS(b4, file = "./b4.rds")
b4 <- readRDS("./b4.rds")

# b5 <- update(b1, newdata = pbp_analysis,
#              formula = YARDS + 15 ~ 1 + mib + DOWN + TOGO_fct,
#              chains = 3, cores = 3)
# saveRDS(b5, file = "./b5.rds")
b5 <- readRDS("./b5.rds")

# b6 <- update(b1, newdata = pbp_analysis,
#              formula = YARDS + 15 ~ 1 + off_personnel + mib + DOWN + TOGO_fct,
#              chains = 3, cores = 3)
# saveRDS(b6, file = "./b6.rds")
b6 <- readRDS("./b6.rds")


summary(b4)
summary(b5)
summary(b6)

pp_check(b4)
pp_check(b5)
pp_check(b6)




#Plot Fitted Posterior Draws from Full Model adjusting for MIB

  #Extract fitted posterior values for offensive personnel and MIB
  set.seed(42)
  c1 <- pbp_analysis %>%
    data_grid(off_personnel, mib, DOWN, TOGO_fct) %>% 
    add_fitted_draws(b6) %>% 
    filter(DOWN == 1, TOGO_fct == "3+", mib == "6") %>% 
    mutate(model = "Adjusted for MIB") 
  c2 <- pbp_analysis %>%
    data_grid(off_personnel, mib, DOWN, TOGO_fct) %>% 
    add_fitted_draws(b6) %>% 
    filter(DOWN == 1, TOGO_fct == "3+", off_personnel == "11") %>% 
    mutate(model = "Adjusted for MIB") 
    
   
  
  #Plot extracted fitted values
  (plot_c1 <- c1 %>% 
    ggplot(aes(x = .value-15, y = off_personnel, fill = off_personnel)) +
    geom_halfeyeh(.width = c(0.90)) +
    theme(legend.position = "none") +
    labs(x = "Predicted Average YPC", y = "Offensive Personnel",
         caption = "For first-and-10, 6 MIB.") +
    xlim(c(3.5,6.5)))
  
    #Get median and 90% QI of fitted posterior expected rushing yards for 11 and 12 personnel
    quantile(filter(c1, off_personnel == "11")$.value-15, probs = c(0.05, 0.50, 0.95))
    quantile(filter(c1, off_personnel == "12")$.value-15, probs = c(0.05, 0.50, 0.95))
  
  
  (plot_c2 <- c2 %>% 
      ggplot(aes(x = .value-15, y = mib, fill = mib)) +
      geom_halfeyeh(.width = c(0.90)) +
      theme(legend.position = "none") +
      labs(x = "Predicted Average YPC", y = "Men in the Box",
           caption = "For first-and-10, 11 personnel.") +
      xlim(c(3.5,6.5)))
  
  (plot_predictive <- grid.arrange(plot_c1, plot_c2, nrow = 1)) 
  ggsave("./Fig3.png", plot = plot_predictive, scale = 0.9)
  

  
  
  
  
  
#Plot Fitted Posterior Draws for offensive personnel for models adjusting and not adjusting
#for MIB (Figures 4 and 5)
set.seed(42)

  #No adjustment
  a <- pbp_analysis %>%
    data_grid(off_personnel, DOWN, TOGO_fct) %>% 
    add_fitted_draws(b4) %>% 
    filter(DOWN == 1, TOGO_fct == "3+"
           ,off_personnel %in% c("11", "21", "22")
           ) %>% 
    mutate(model = "Offensive Personnel Only") 
  
  #Adjusted model
  b <- pbp_analysis %>%
    data_grid(off_personnel, mib, DOWN, TOGO_fct) %>% 
    add_fitted_draws(b6) %>% 
    filter(DOWN == 1, TOGO_fct == "3+", mib == "6"
           ,off_personnel %in% c("11", "21", "22")
           ) %>% 
    mutate(model = "Adjusted for MIB (Results for 6 MIB)") 

plot <- rbind(a, b) %>% 
  mutate(model = factor(model)) 

  #Set colors for plot below
  gg_color_hue <- function(n) {
    hues = seq(15, 375, length = n + 1)
    hcl(h = hues, l = 65, c = 100)[1:n]
  }
  
  #Change to c(1,2,3) to reproduce Figure 4
  color_list <- gg_color_hue(5)[c(1,4,5)]
  

(p <- plot %>% 
  ggplot(aes(x = .value-15, y = off_personnel, fill = off_personnel)) +
  geom_halfeyeh(.width = c(0.90)) +
  scale_fill_manual(values = color_list) +
  transition_manual(model) +
  labs(title = 'Distribution of Yards: {current_frame}', 
       x = 'Predicted Yards', y = 'Offensive Personnel') +
  theme(legend.position = "none",
        text = element_text(size=16)))
  
  #Fix plot size and speed
  animate(p, height = 400, width = 600, fps = 20) #2.5s per frame
  
anim_save("./Fig5.gif", scale = 0.9)





#Get median and 90% QIs for differences in average yards gained by offensive personnel

  #Model adjusting for MIB
  
    #Extract posterior fitted draws and compare each possible combination of offensive
    #personnel (e.g. average yards gained with 22 personnel - average yards gainde with 11 personnel)
    diffs_full <- pbp_analysis %>%
      data_grid(off_personnel, mib, DOWN, TOGO_fct) %>% 
      add_fitted_draws(b6) %>% 
      filter(DOWN == 1, TOGO_fct == "3+", mib == "6") %>%
      ungroup() %>% 
      mutate(off_personnel = fct_rev(off_personnel)) %>% 
      compare_levels(variable = .value, by = off_personnel)
  
    #Plot the distributions of a subset of these contrasts
    diffs_full %>% 
      filter(off_personnel %in% c("11 - 12", "11 - 13")) %>% 
      ggplot(aes(y = off_personnel, x = .value)) +
      geom_halfeyeh(.width = c(0.90)) +
      xlim(c(-1,1))
  
    #Print the median and 90% QI for the indicated comparisons
    diffs_full %>% 
      filter(off_personnel %in% c("11 - 12", "11 - 13")) %>% 
      median_qi(.width = c(0.90))

  #Model not adjusting for MIB
    
  diffs_reduced <- pbp_analysis %>%
    data_grid(off_personnel, DOWN, TOGO_fct) %>% 
    add_fitted_draws(b4) %>% 
    filter(DOWN == 1, TOGO_fct == "3+") %>% 
    compare_levels(variable = .value, by = off_personnel)
  
  diffs_reduced %>% 
    filter(off_personnel %in% c("12 - 11", "13 - 11")) %>% 
    median_qi(.width = c(0.90))








# #Plot Fitted Posterior Draws for MIB for models adjusting and not adjusting
# #for offensive personnel (not used in article)
# set.seed(42)
# a <- pbp_analysis %>%
#   data_grid(mib, DOWN, TOGO_fct) %>% 
#   add_fitted_draws(b5) %>% 
#   filter(DOWN == 1, TOGO_fct == "3+"
#   ) %>% 
#   mutate(model = "MIB Only") 
# b <- pbp_analysis %>%
#   data_grid(off_personnel, mib, DOWN, TOGO_fct) %>% 
#   add_fitted_draws(b6) %>% 
#   filter(DOWN == 1, TOGO_fct == "3+", off_personnel == "11"
#   ) %>% 
#   mutate(model = "Adjusted for Offensive Personnel (Results for 11)") 
# plot <- rbind(a, b) %>% 
#   mutate(model = factor(model)) 
# 
# 
# (p <- plot %>% 
#     ggplot(aes(x = .value-15, y = mib, fill = mib)) +
#     geom_halfeyeh(.width = c(0.90)) +
#     transition_manual(model) +
#     labs(title = 'Distribution of Yards: {current_frame}', 
#          x = 'Predicted Yards', y = 'Men in the Box') +
#     theme(legend.position = "none"))
# anim_save("./Fig6.gif")
