library(tidyverse)
library(flexmix)  
library(lme4)
library(scales) 

# Import data and clean 
yordan <- read.csv(file.choose()) 
arraez <- read.csv(file.choose()) 
carroll <- read.csv(file.choose())

arraez <- 
  arraez %>% 
  filter(!is.na(bat_speed), !is.na(swing_length)) 

carroll <- 
  carroll %>%
  filter(!is.na(bat_speed), !is.na(swing_length)) 

yordan <- 
  yordan %>% 
  filter(!is.na(bat_speed), !is.na(swing_length)) 

# Yordan bat speed distribution
ggplot(yordan, aes(x=bat_speed)) + 
  geom_density(fill="orange", adjust = 0.6) + 
  geom_vline(xintercept = mean(yordan$bat_speed), linetype="dashed", 
             color="red") + 
  ylim(NA, 0.15) + 
  scale_x_continuous(breaks=pretty_breaks(n=5)) + 
  theme_bw() + 
  ggtitle("Yordan Alvarez Bat Speed Distribution, 2024") + 
  theme(plot.title = element_text(face = "bold")) + 
  labs(x = "Bat Speed (mph)") 
  
# Gaussian mixture model 
# As simple and easy as a no-bake brownie 
mix <- flexmix(bat_speed ~ 1, data = yordan, k = 3) 

# Indicate cluster 
yordan <- 
  yordan %>% 
  mutate(bat_speed_cluster = mix@cluster) 

# Yordan distribution by bat speed cluster 
ggplot(yordan, aes(x=bat_speed, fill=bat_speed_cluster)) + 
  geom_density(alpha = 0.5) + 
  ylim(NA, 0.35) + 
  scale_x_continuous(breaks=pretty_breaks(n=5)) +
  theme_bw() + 
  ggtitle("Alvarez Bat Speed Distribution by Cluster") + 
  theme(plot.title = element_text(face = "bold")) + 
  labs(x = "Bat Speed (mph)", 
       fill = "Cluster"
  ) 

# Swing length vs. bat speed by cluster 
ggplot(yordan, aes(swing_length, bat_speed)) + 
  geom_point(aes(color = bat_speed_cluster)) +  
  geom_smooth(method = "lm") + 
  facet_wrap(vars(bat_speed_cluster)) + 
  scale_y_continuous(breaks = pretty_breaks(n=5)) + 
  theme_bw() + 
  ggtitle("Swing Length vs. Bat Speed by Cluster") + 
  theme(plot.title = element_text(face = "bold"), 
        legend.position = "none"
        ) + 
  labs(x = "Swing Length (ft)", 
       y = "Bat Speed (mph)"
  )

# New dataframe, create labels 
combine <- bind_rows(stanton, carroll) 
cor <- data.frame(
  player_name = c("Stanton, Giancarlo", "Carroll, Corbin"), 
  text = c("R = 0.48", "R = 0.32")
)

# Swing length vs. bat speed by player 
ggplot(combine, aes(swing_length, bat_speed)) + 
  geom_point(aes(color = player_name)) + 
  geom_smooth(method = "lm") + 
  facet_wrap(~ player_name) + 
  geom_label(data = cor, aes(x=6.5, y=87, label=text)) + 
  scale_y_continuous(breaks = pretty_breaks(n=5)) + 
  theme_bw() + 
  ggtitle("Swing Length vs. Bat Speed by Player") + 
  theme(plot.title = element_text(face = "bold"), 
        legend.position = "none"
  ) + 
  labs(x = "Swing Length (ft)", 
       y = "Bat Speed (mph)"
  ) 
