library(tidyverse)
library(arsenal)

# bWAR follow-up paper code

# Import data from csv (found at https://www.baseball-reference.com/about/war_explained.shtml)
total_data <- read_csv("bWAR components.csv", show_col_types = FALSE)

# Add outs_per_start column
total_data <- total_data %>%
    mutate(outs_per_start = ifelse(GS > 0, IPouts_start/GS, 0)) # sets ratio equal to 0 if GS = 0

# Calculate season/league outs_per_start_rep constant. 
# This requires the grouping of partial rows.

totals_for_rep <- total_data %>% 
    select(stint_ID, player_ID, year_ID, lg_ID, IPouts_start, G, GS, WAR)

partials_for_rep <- totals_for_rep %>% 
    filter(stint_ID > 1) %>% 
    select(player_ID, year_ID) %>% 
    distinct()

partials_for_rep <- totals_for_rep %>%
      semi_join(partials_for_rep, by = c("player_ID", "year_ID"))

totals_for_rep <- totals_for_rep %>%
    anti_join(partials_for_rep)

partials_for_rep <- partials_for_rep %>%
    group_by(player_ID, year_ID) %>%
    summarise(
        across(c(IPouts_start, G, GS, WAR), sum),
        across(lg_ID, first),
        stint_ID = 1
    ) %>%
    ungroup()

totals_for_rep <- bind_rows(totals_for_rep, partials_for_rep)

outs_per_start_rep_avg <- totals_for_rep %>%
    filter(WAR <= 0.5, WAR >= -0.5, GS >= 7, GS/G >= 0.8) %>%
    mutate(outs_per_start = IPouts_start/GS) %>%
    select(outs_per_start) %>%
    pull() %>%
    mean()

outs_per_start_rep_table <- tibble(
  year_ID = c(2022, 2023, 2024, 2022, 2023, 2024),
  lg_ID = c("NL", "NL", "NL", "AL", "AL", "AL"),
  outs_per_start_rep = c(outs_per_start_rep_avg, outs_per_start_rep_avg,
  outs_per_start_rep_avg, outs_per_start_rep_avg,
  outs_per_start_rep_avg, outs_per_start_rep_avg)
)

# Calculate season/league outs_per_game_cnst

outs_per_game_cnst_table <- total_data %>%
    select(year_ID, lg_ID, IPouts, runs_above_avg, 
        runs_above_rep, teamRpG, RpO_replacement) %>%
    group_by(year_ID, lg_ID) %>%
    summarise(
        max_IPouts = max(IPouts),
        max_IPouts_RAA = runs_above_avg[which.max(IPouts)],
        max_IPouts_RAR = runs_above_rep[which.max(IPouts)],
        across(c(RpO_replacement, teamRpG), first)
    ) %>%
    mutate(outs_per_game_cnst = 
        teamRpG/(RpO_replacement - (max_IPouts_RAR - max_IPouts_RAA)/max_IPouts)) %>%
    select(year_ID, lg_ID, outs_per_game_cnst) %>%
    ungroup()

# BullpenRpO; stats are taken directly from FanGraphs

bullpenRpO_table <- tibble(
  year_ID = c(2022, 2023, 2024, 2022, 2023, 2024),
  lg_ID = c("NL", "NL", "NL", "AL", "AL", "AL"),
  bullpenRpO = c(0.166, 0.170, 0.164, 0.155, 0.172, 0.165)
)

# Add constants to main table

total_data <- total_data %>%
    inner_join(bullpenRpO_table, by = c("year_ID", "lg_ID")) %>%
    inner_join(outs_per_game_cnst_table, by = c("year_ID", "lg_ID")) %>%
    inner_join(outs_per_start_rep_table, by = c("year_ID", "lg_ID"))

# Add components for new replacement WAR

total_data <- total_data %>%
    mutate(
        avg_runs_above_rep = runs_above_rep - runs_above_avg
    ) %>%
    mutate(
        scaled_avg_runs_above_rep = ifelse(
            outs_per_start == 0, avg_runs_above_rep,
            (RpO_replacement - teamRpG/outs_per_game_cnst) * (GS * outs_per_start_rep + IPouts_relief)
        )
    ) %>%
    mutate(
        new_oppRpG_rep = 
        scaled_avg_runs_above_rep/G + teamRpG) %>%
    mutate(
        new_pyth_exponent_rep = 
        (teamRpG + new_oppRpG_rep)**0.285) %>%
    mutate(
        new_waa_win_perc_rep = 
        (teamRpG**new_pyth_exponent_rep) /
        (teamRpG**new_pyth_exponent_rep + new_oppRpG_rep**new_pyth_exponent_rep)) %>%
    mutate(
        new_WAR_rep = 
        (0.5 - new_waa_win_perc_rep) * G)

# Add components for bullpen WAR, calc corrected WAR

total_data <- total_data %>%
    mutate(
        scaled_bp_runs_above_avg = ifelse(
            outs_per_start == 0, 0,
            (teamRpG/outs_per_game_cnst - bullpenRpO) *
            GS * (outs_per_start - outs_per_start_rep)
        )
    ) %>%
    mutate(
        oppRpG_bp = 
        teamRpG - scaled_bp_runs_above_avg/G) %>%
    mutate(
        pyth_exponent_bp = 
        (teamRpG + oppRpG_bp)**0.285) %>%
    mutate(
        waa_win_perc_bp = 
        (teamRpG**pyth_exponent_bp) /
        (teamRpG**pyth_exponent_bp + oppRpG_bp**pyth_exponent_bp)) %>%
    mutate(
        WAA_bp = 
        (waa_win_perc_bp - 0.5) * G) %>%
    mutate(
        new_WAR = 
        round(WAA + WAA_adj + new_WAR_rep - WAA_bp, 2))

# Group all partial data. I'm creating a new table with the essential
# variables only, since from here on out we don't need most of the components

ess_var = c(
    "name_common", "age", "mlb_ID", "player_ID", "year_ID", 
    "RpO_replacement", "bullpenRpO", "outs_per_start_rep",
    "G", "GS", "IPouts", "IPouts_start", "IPouts_relief",
    "RA", "xRA_final", "runs_above_avg", "runs_above_avg_adj", 
    "runs_above_rep", "WAR", "WAA", "WAA_adj", "WAR_rep", "avg_runs_above_rep",
    "scaled_avg_runs_above_rep", "new_WAR_rep", 
    "scaled_bp_runs_above_avg", "WAA_bp", "new_WAR", "outs_per_start",
    "team_ID", "lg_ID", "stint_ID", "teamRpG", "outs_per_game_cnst"
)

ess_data <- total_data %>% 
    select(all_of(ess_var))

partial_data <- ess_data %>%
    filter(stint_ID > 1) %>%
    select(player_ID, year_ID) %>%
    distinct()

partial_data <- ess_data %>%
    semi_join(ess_data, by = c("player_ID", "year_ID"))

ess_data <- ess_data %>%
    anti_join(partial_data)

# Massive summarise statement

partial_data <- partial_data %>%
    group_by(player_ID, year_ID) %>%
    summarise(
        across(c(
            "name_common", "age", "mlb_ID", "RpO_replacement", 
            "bullpenRpO", "outs_per_start_rep", "teamRpG", "outs_per_game_cnst"
        ), first),
        across(c(
            "G", "GS", "IPouts", "IPouts_start", 
            "IPouts_relief", "RA", "xRA_final", "runs_above_avg", 
            "runs_above_avg_adj", "runs_above_rep", "WAR",
            "WAA", "WAA_adj", "WAR_rep", "avg_runs_above_rep", "scaled_avg_runs_above_rep",
            "new_WAR_rep", "scaled_bp_runs_above_avg",
            "WAA_bp", "new_WAR"
        ), sum),
        team_ID = "TOT",
        lg_ID = ifelse(
            sum(lg_ID == "AL") > 0 & sum(lg_ID == "NL") > 0,
            "2LG",
            first(lg_ID)
        ),
        stint_nums = max(stint_ID)
    ) %>%
    ungroup()

# Combine all data, add a few useful columns,
# reverse engineer the win percentages

ess_data <- bind_rows(ess_data, partial_data) %>%
    mutate(
        outs_per_start = IPouts_start/GS
    ) %>%
    mutate(
        ra_per_out = RA/IPouts
    ) %>%
    mutate(
        raa_per_out = runs_above_avg/IPouts
    ) %>%
    mutate(
        raaa_per_out = runs_above_avg_adj/IPouts
    ) %>%
    mutate(
        waa_win_perc = WAA/G + 0.5
    ) %>%
    mutate(
        implied_teamRpO = teamRpG/outs_per_game_cnst
    ) %>%
    mutate(
        diff_old_new_WAR = WAR - new_WAR
    )

# Filter for starting pitchers

filtered_data <- ess_data %>%
    filter(GS/G >= 0.8, GS >= 7)

# Correlations: RA, RAAA, waa_win_perc

ra_lm <- lm(outs_per_start ~ RA, data = filtered_data)
ra_lm_rs <- summary(ra_lm)$r.squared

ra_per_out_lm <- lm(outs_per_start ~ ra_per_out, data = filtered_data)
ra_per_out_lm_rs <- summary(ra_per_out_lm)$r.squared

raa_per_out_lm <- lm(outs_per_start ~ raa_per_out, data = filtered_data)
raa_per_out_lm_rs <- summary(raa_per_out_lm)$r.squared

raaa_per_out_lm <- lm(outs_per_start ~ raaa_per_out, data = filtered_data)
raaa_per_out_lm_rs <- summary(raaa_per_out_lm)$r.squared

waa_win_perc_lm <- lm(outs_per_start ~ waa_win_perc, data = filtered_data)
waa_win_perc_lm_rs <- summary(waa_win_perc_lm)$r.squared

# Benchmarks

ra_per_out_avg <- filtered_data %>% 
    select(ra_per_out) %>% 
    pull() %>% 
    mean()

ra_per_out_rep <- filtered_data %>%
    select(RpO_replacement) %>%
    pull() %>%
    mean()

ra_per_out_bp <- filtered_data %>%
    select(bullpenRpO) %>%
    pull() %>%
    mean()

ra_per_out_elite <- filtered_data %>%
    filter(ntile(WAR, 4) == 4) %>%
    select(ra_per_out) %>%
    pull() %>%
    mean()

outs_per_start_avg <- filtered_data %>%
    select(outs_per_start) %>%
    pull() %>%
    mean()

outs_per_start_rep <- filtered_data %>%
    select(outs_per_start_rep) %>%
    pull() %>%
    mean()

outs_per_start_elite <- filtered_data %>%
    filter(ntile(WAR, 4) == 4) %>%
    select(outs_per_start) %>%
    pull() %>%
    mean()

# Graphing code

# corr_raaa_out_plot <- ggplot(filtered_data, aes(x = raaa_per_out, y = outs_per_start)) +
#     geom_point(color = "blue") +
#     geom_smooth(method = "lm", color = "red") +
#     annotate("text", x = -0.15, y = 21, size = 4,
#     label = paste("R^2 = ", round(raaa_per_out_lm_rs, 3)))

# benchmarks_plot <- ggplot(aes(x = ra_per_out, y = outs_per_start), data = filtered_data) +
#     geom_point(color = "blue") + 
#     geom_vline(xintercept = ra_per_out_avg, color = "red", linewidth = 1.3) +
#     annotate("text", x = ra_per_out_avg+0.013, y = 20, color = "red", label = "Average", angle = 45) +
#     geom_vline(xintercept = ra_per_out_rep, color = "red", linewidth = 1.3, linetype = "dashed") +
#     annotate("text", x = ra_per_out_rep+0.015, y = 20, color = "red", label = "Replacement", angle = 45) +
#     geom_vline(xintercept = ra_per_out_elite, color = "red", linewidth = 1.3, linetype = "dotted") + 
#     annotate("text", x = ra_per_out_elite-0.013, y = 20, color = "red", label = "Elite", angle = 45) +
#     geom_vline(xintercept = ra_per_out_bp, color = "#ffc400", linewidth = 1.3) +
#     annotate("text", x = ra_per_out_bp-0.013, y = 20, color = "#ffc400", label = "Bullpen", angle = 45) +
#     geom_hline(yintercept = outs_per_start_avg, color = "#006713", linewidth = 1.3) + 
#     annotate("text", y = outs_per_start_avg+0.7, x = 0.35, color = "#006713", label = "Average", angle = 45) +
#     geom_hline(yintercept = outs_per_start_rep, color = "#006713", linewidth = 1.3, linetype = "dashed") +
#     annotate("text", y = outs_per_start_rep-1, x = 0.35, color = "#006713", label = "Replacement", angle = 45) +
#     geom_hline(yintercept = outs_per_start_elite, color = "#006713", linewidth = 1.3, linetype = "dotted") +
#     annotate("text", y = outs_per_start_elite+0.7, x = 0.35, color = "#006713", label = "Elite", angle = 45)

# Deciles plot

# filtered_deciles <- filtered_data %>%
#       mutate(bin = ntile(WAR, 10)) %>%
#       mutate(bin = case_when(
#         bin == 1 ~ "1st",
#         bin == 2 ~ "2nd",
#         bin == 3 ~ "3rd",
#         bin == 4 ~ "4th",
#         bin == 5 ~ "5th",
#         bin == 6 ~ "6th",
#         bin == 7 ~ "7th",
#         bin == 8 ~ "8th",
#         bin == 9 ~ "9th",
#         bin == 10 ~ "10th")) %>%
#         mutate(bin = factor(bin, levels = c(
#         "10th", "9th", "8th", "7th", "6th", 
#         "5th", "4th", "3rd", "2nd", "1st")))

# decile_plot_data <- filtered_deciles %>%
#       select(bin, new_WAR, WAR) %>%
#       pivot_longer(cols = c(WAR, new_WAR), names_to = "metric", values_to = "value")

# decile_plot <- ggplot(decile_plot_data, aes(x = factor(bin), y = value, fill = metric)) +
#           geom_bar(stat = "summary", fun = "mean", position = "dodge", alpha = 0.7) +
#           labs(x = "Deciles (sorted by bWAR)",
#            y = "Mean bWAR Value", fill = "Legend") +
#         theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#         scale_y_continuous(breaks = seq(-3, 8, by = 1))

# Top 10 plot

# top_10_players_diff <- filtered_data %>%
#       arrange(desc(diff_old_new_WAR)) %>%
#       slice(1:10) %>%
#       mutate(Player_Year = paste(name_common, " (", year_ID, ")", sep = ""))

# top_10_plot_data <- top_10_players_diff %>%
#       select(Player_Year, WAR, new_WAR, diff_old_new_WAR) %>%
#       pivot_longer(cols = c(WAR, new_WAR), names_to = "metric", values_to = "value")

# top_10_plot <- ggplot(top_10_plot_data, aes(x = reorder(Player_Year, -diff_old_new_WAR), y = value, fill = metric)) +
#       geom_bar(stat = "identity", position = "dodge", alpha = 0.7) +
#       labs(x = "Player (Season)", y = "WAR Value", fill = "Metric") +
#       theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#       scale_y_continuous(breaks = seq(-3, 8, by = 1))