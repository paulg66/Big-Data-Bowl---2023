#Compare % to PZ data to EPA
library(nflverse)
pbp <- nflfastR::load_pbp(2021) %>%
       dplyr::filter(season_type == "REG") %>%
       dplyr::filter(!is.na(posteam) & pass == 1)

epa_data <- pbp[,c('play_id','old_game_id','posteam','defteam','epa','wpa','air_epa','yac_epa')]
names(epa_data) <- c('playId','gameId','posteam','defteam','epa','wpa','air_epa','yac_epa')

PZ_per_play <- Pass_Rushers_Final %>% group_by(playId,gameId) %>% summarize(PZs = mean(Percent_to_Pressure_Zone_per_s))
PZ_per_play <- merge(PZ_per_play,epa_data)

PZ_per_play %>% filter(PZs > 0) %>% ggplot(aes(PZs,air_epa)) + geom_point() + geom_smooth(method = "lm")

team_PZ_epa <- PZ_per_play %>% group_by(defteam) %>% summarize(PZs = median(PZs),epa = median(epa))

ggplot2::ggplot(team_PZ_epa, aes(x = PZs, y = epa)) +
       nflplotR::geom_mean_lines(aes(v_var = PZs , h_var = epa)) +
       nflplotR::geom_nfl_logos(aes(team_abbr = defteam), width = 0.065, alpha = 0.7) +
       ggplot2::labs(
             x = "Median %PZ/s per play",
             y = "Median EPA per play",
             title = "% to Pressure Zone per Second vs EPA per Play"
         ) +
       ggplot2::theme_minimal() +
       ggplot2::theme(
             plot.title = ggplot2::element_text(face = "bold"),
             plot.title.position = "plot"
         ) +
       ggplot2::scale_y_reverse()+
       ggplot2::scale_x_continuous(labels = scales::percent_format())