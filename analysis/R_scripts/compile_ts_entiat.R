# Author: Kevin See
# Purpose: Prep Entiat redd data from 2006-present
# Created: 4/9/24
# Last Modified: 4/9/24
# Notes: this data is from the Entiat

#-----------------------------------------------------------------
# load needed libraries
library(tidyverse)
library(lubridate)
library(readxl)
library(janitor)
library(magrittr)
library(msm)
library(PITcleanr)
library(here)
library(sroem)

theme_set(theme_bw())

#-----------------------------------------------------------------
# query redd data 2006 - 2010
redd_data <-
  query_redd_data(redd_file_path = "T:/DFW-Team FP Upper Columbia Escapement - General/UC_Sthd/inputs/Redd Data",
                  redd_file_name = "Entiat_Redd_Surveys_2006-2010.xlsx",
                  query_year = 2006:2010)

redd_df <-
  redd_data |>
  mutate(across(exp_sp_total_log,
                ~ . + 1)) |>
  predict_neterr(species = "Steelhead",
                 num_obs = "two",
                 err_floor = T)


redd_data |>
  predict_neterr(species = "Steelhead",
                 num_obs = "two",
                 err_floor = F) |>
  filter(net_error < min(redd_df$net_error, na.rm = T)) |>
  tabyl(spawn_year,
        reach,
        show_missing_levels = F) |>
  adorn_totals("both")



# set some thresholds
# minimum number of total redds observed
min_redds = 2
# minimum number of weeks with at least one new redd observed
min_non0_wks = 3

results_lst <- summarize_redds(redd_df,
                               species = "Steelhead",
                               group_vars = c("spawn_year", "river", "reach", "index", "survey_type"),
                               summ_vars = c("spawn_year", "river", "index"),
                               min_non0_wks = min_non0_wks,
                               min_redds = min_redds,
                               gauc = T,
                               add_zeros = T,
                               use_cor = T)

rch_results <-
  results_lst$rch_est

strm_results <-
  results_lst$summ_est

# comparison with observer error model data
# by year and river
redd_df |>
  filter(!is.na(net_error)) |>
  filter(visible_redds > 0) |>
  nest(redd_data = -c(spawn_year)) |>
  mutate(comp_df = map(redd_data,
                       .f = function(x) {
                         compare_covars(x,
                                        species = "Steelhead",
                                        num_obs = "two")
                       })) |>
  select(-redd_data) |>
  unnest(comp_df) |>
  mutate(across(covariate,
                ~ recode(.,
                         "exp_sp_total_log" = "Log Surveyor Exp.",
                         "mean_discharge" = "Mean Discharge",
                         "mean_thalweg_cv" = "Mean Thalweg CV",
                         "naive_density_km" = "Obs. Redd Density",
                         "net_error" = "Net Error")),
         across(covariate,
                ~ factor(.,
                         levels = c('Mean Thalweg CV',
                                    'Obs. Redd Density',
                                    'Log Surveyor Exp.',
                                    'Mean Discharge',
                                    'Net Error')))) |>
  ggplot(aes(x = source,
             y = value,
             fill = source)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Set1",
                    name = "Source") +
  theme(legend.position = 'bottom') +
  labs(x = "Source",
       y = "Value") +
  facet_wrap(covariate ~ spawn_year,
             scales = 'free_y')

# overall
comp_p <-
  redd_df |>
  filter(!is.na(net_error)) |>
  filter(visible_redds > 0) |>
  compare_covars(species = "Steelhead",
                 num_obs = "two") |>
  mutate(across(source,
                ~ case_match(.,
                             "Predictive Data" ~ "Entiat Data",
                             .default = .)),
         across(source,
                ~ str_replace(., " ", "\n")),
         across(source,
                ~ fct_rev(.))) |>
  mutate(across(covariate,
                ~ recode(.,
                         "exp_sp_total_log" = "Log Surveyor Exp.",
                         "mean_discharge" = "Mean Discharge",
                         "mean_thalweg_cv" = "Mean Thalweg CV",
                         "naive_density_km" = "Obs. Redd Density",
                         "net_error" = "Net Error")),
         across(covariate,
                ~ factor(.,
                         levels = c('Mean Thalweg CV',
                                    'Obs. Redd Density',
                                    'Log Surveyor Exp.',
                                    'Mean Discharge',
                                    'Net Error')))) |>
  ggplot(aes(x = source,
             y = value,
             fill = source)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Set1",
                    name = "Source") +
  labs(x = "Source",
       y = "Value") +
  facet_wrap(~ covariate,
             scales = 'free_y')

comp_p

ggsave(here("analysis/figures",
            "Entiat_Covar_Comp.pdf"),
       comp_p,
       width = 7,
       height = 6)

#-----------------------------------------------------------------
# query DABOM estimates from 2011 - present
dabom_df <- tibble(spawn_year = 2011:2023,
                   dam_nm = if_else(spawn_year %in% c(2011:2015, 2018),
                                    "PriestRapids",
                                    "RockIsland"))

dabom_est <-
  dabom_df |>
  dplyr::mutate(escp = purrr::map2(spawn_year,
                                   dam_nm,
                                   .f = function(yr, dam_nm) {
                                     sroem::query_dabom_results(dabom_file_path = "O:Documents/Git/MyProjects/DabomPriestRapidsSthd/analysis/data/derived_data/estimates",
                                                                dabom_dam_nm = dam_nm,
                                                                dabom_file_name = "UC_Sthd_DABOM_",
                                                                query_year = yr,
                                                                result_type = "escape_summ") |>
                                       dplyr::select(-dplyr::any_of("spawn_year"))
                                   })) |>
  dplyr::select(-dam_nm) |>
  tidyr::unnest(escp) |>
  filter(location %in% c("ENL",
                         "MAD",
                         "RCT",
                         "EHL",
                         "ENA",
                         "ENF"))


excel_sheets("T:DFW-Team FP Upper Columbia Escapement - General/UC_Sthd/estimates/UC_STHD_Model_Output.xlsx")
read_excel()

#-----------------------------------------------------------------
# put together
# pick an arbitrary fish / redd
fpr <- 2

ent_est <-
  strm_results |>
  group_by(spawn_year) |>
  summarize(across(c(n_rchs,
                     strm_obs,
                     strm_est),
                   sum),
            across(strm_se,
                   ~ sqrt(sum(.^2))),
            .groups = "drop") |>
  rename(total_redds = strm_est,
         total_se = strm_se) |>
  mutate(total_escp = total_redds * fpr) |>
  relocate(total_escp,
           .after = "total_redds") |>
  mutate(lower_ci = qnorm(0.025, total_escp, total_se),
         upper_ci = qnorm(0.975, total_escp, total_se),
         across(c(total_redds,
                  total_escp,
                  lower_ci),
                ~ if_else(. < 0, 0, .))) |>
  add_column(data_source = c("Redds"),
             .after = "spawn_year") |>
  bind_rows(dabom_est |>
              filter(location == "ENL") |>
              mutate(across(lower_ci,
                            ~ if_else(is.na(.),
                                      lowerCI,
                                      .)),
                     across(upper_ci,
                            ~ if_else(is.na(.),
                                      upperCI,
                                      .))) |>
              group_by(spawn_year) |>
              summarize(across(c(mean,
                                 median,
                                 ends_with("_ci")),
                               sum),
                        across(sd,
                               ~ sqrt(sum(.^2))),
                        .groups = "drop") |>
              rename(total_se = sd,
                     total_escp = median) |>
              add_column(data_source = c("PIT Tags"),
                         .after = "spawn_year"))


ent_ts_p <-
  ent_est |>
  ggplot(aes(x = spawn_year,
             y = total_escp,
             color = data_source)) +
  geom_pointrange(aes(ymin = lower_ci,
                      ymax = upper_ci)) +
  geom_point(aes(y = strm_obs),
             color = "black",
             size = 3,
             shape = 17) +
  scale_color_brewer(palette = "Set1",
                     name = "Source") +
  coord_cartesian(ylim = c(0, 4000)) +
  labs(x = "Spawn Year",
       y = "Total Escapement Estimate",
       title = "Entiat Steelhead")
ent_ts_p

ggsave(here("analysis/figures",
            "Entiat_TS.pdf"),
       ent_ts_p,
       width = 8,
       height = 6)
