# Author: Kevin See
# Purpose: Prep redd data from 2004-2013
# Created: 6/9/22
# Last Modified: 4/9/24
# Notes: this data is from the Wenatchee

# How to account for the fact that non-index reaches have a peak count,
# which is different from new redds only for the index reach GAUC curve?
# Might need to fit a GAUC curve to the visible redds for the index reaches,
# match the non-index reach count up with that. Assume the same mean and SD
# of the GAUC curve for the non-index reach, but adjust the "a" parameter, or
# max height, so the new curve goes through non-index reach count. Then
# calculate GAUC estimate of total redd days, and adjust for redd life and
# observer error based on the index reach.

#-----------------------------------------------------------------
# load needed libraries
library(tidyverse)
library(lubridate)
library(readxl)
library(janitor)
library(magrittr)
library(msm)
library(sroem)
library(PITcleanr)
library(here)

theme_set(theme_bw())

#-----------------------------------------------------------------
# grab some data from Andrew
data_path <- "O:/Documents/Git/MyProjects/UCSthdReddObsErr/analysis/data"

data_file <- here(data_path,
                  "raw_data",
                  "Final_Historical covariates Wenatchee Steelhead 5-23-22.xlsx")
excel_sheets(data_file)

#-----------------------------------------------------------------
# what method should be used for each reach / year?
method_df <- read_excel(data_file,
                        "Method") %>%
  # W9 was listed twice, and one row is incorrect
  filter(!(Reach == "W9" & `2012` == "Redd")) %>%
  pivot_longer(-c(1:2),
               names_to = "year",
               values_to = "method") %>%
  mutate(across(year,
                as.numeric)) %>%
  clean_names() %>%
  mutate(reach = if_else(reach == "P2" & year %in% 2004:2010 & method == "Redd",
                         "P3 (P2)",
                         reach)) %>%
  mutate(across(reach,
                as_factor)) %>%
  distinct() %>%
  mutate(across(river,
                ~ case_match(.,
                             "Little Wen" ~ "Little Wenatchee",
                             "White" ~ "White River",
                             .default = .)),
         across(river,
                ~ str_remove(.,
                             " Ck"))) %>%
  arrange(river, reach, year)

# for 2004-2010, P3 and P4 are lumped in with P2
# but we'll call P4 the non-index stretch, so let's keep P4
method_df %<>%
  filter(!(year %in% c(2004:2010) &
             reach %in% c("P3")))

#-----------------------------------------------------------------
# data on redd counts
#-----------------------------------------------------------------
# non-index reaches
non_index_rch <- read_excel(data_file,
                            "Non-index Reaches") %>%
  clean_names() %>%
  rename(river = stream) %>%
  select(-x6) %>%
  mutate(across(non_index_reach,
                ~ case_match(.,
                             "P2" ~ "P4",
                             .default = .)),
         across(index_reach,
                ~ case_match(.,
                             "P2" ~ "P3 (P2)",
                             .default = .)),
         across(river,
                ~ case_match(.,
                               "Little Wen" ~ "Little Wenatchee",
                               "White" ~ "White River",
                               .default = .)),
         across(river,
                ~ str_remove(.,
                             " Ck"))) %>%
  arrange(year,
          river,
          non_index_reach,
          date)

# index reaches
index_rch <- read_excel(data_file,
                        "Index Reaches") %>%
  clean_names() %>%
  rename(river = stream) %>%
  mutate(across(river,
                ~ case_match(.,
                             "Little Wen" ~ "Little Wenatchee",
                             "White" ~ "White River",
                             .default = .)),
         across(river,
                ~ str_remove(.,
                             " Ck"))) %>%
  arrange(year,
          river,
          index_reach,
          date)

# no-error reaches
no_err_rch <- read_excel(data_file,
                         "No Error Tribs") %>%
  clean_names() %>%
   mutate(across(major,
                ~ case_match(.,
                             "Little Wen" ~ "Little Wenatchee",
                             "White" ~ "White River",
                             .default = .)),
         across(major,
                ~ str_remove(.,
                             " Ck")))

# put a couple of index reaches into the no-error reaches
no_err_rch %<>%
  bind_rows(index_rch %>%
              filter(index_reach %in% c("H3",
                                        "L3")) %>%
              group_by(year,
                       river,
                       index_reach) %>%
              summarize(total = sum(redds,
                                    na.rm = T),
                        .groups = "drop") %>%
              rename(major = river,
                     minor = index_reach)) %>%
  arrange(year,
          major,
          minor)

index_rch %<>%
  filter(!index_reach %in% c("H3",
                             "L3"))

# combine all reaches, and sum total observed redds in each reach, each year
all_rch <- index_rch %>%
  # mutate(across(index_reach,
  #               recode,
  #               "P3 (P2)" = "P2")) %>%
  group_by(year, river,
           reach = index_reach) %>%
  summarize(across(redds,
                   sum,
                   na.rm = T),
            .groups = "drop") %>%
  mutate(type = "I") %>%
  bind_rows(non_index_rch %>%
              select(year, river,
                     reach = non_index_reach,
                     redds) %>%
              mutate(type = "NI")) %>%
  bind_rows(no_err_rch %>%
              select(year,
                     river = major,
                     reach = minor,
                     redds = total) %>%
              mutate(type = "NE"))

# any reaches without a method?
all_rch %>%
  filter(str_detect(reach, "[:digit:]")) %>%
  full_join(method_df) %>%
  filter(is.na(method))

# any reaches identified in methods as redds (or NA) but with no data?
method_df %>%
  filter(method == "Redd" | is.na(method)) %>%
  full_join(all_rch %>%
              filter(str_detect(reach, "[:digit:]"))) %>%
  filter(is.na(redds)) %>%
  select(-redds, -type, -method) %>%
  mutate(miss = 'X') %>%
  pivot_wider(names_from = year,
              values_from = miss,
              values_fill = '',
              names_sort = T)

method_df %>%
  filter(method == "Redd") %>%
  left_join(all_rch) %>%
  filter(is.na(redds)) %>%
  tabyl(reach)

#---------------------------------------------------
# observer error covariates
#---------------------------------------------------
# reach length
rch_lngth_org <- read_excel(data_file,
                            "Reach Length") %>%
  clean_names() %>%
  rename(reach = reach_2,
         reach_descp = reach_3) %>%
  pivot_longer(cols = c(index,
                        non_index),
               names_to = "type",
               values_to = "type_descp") %>%
  filter(!is.na(type_descp)) %>%
  relocate(length_km,
           .after = type_descp) %>%
  mutate(across(reach,
                as_factor),
         across(reach,
                ~ fct_expand(.,
                             "P3 (P2)")),
         across(reach,
                ~ fct_relevel(.,
                              "P3 (P2)",
                              after = 13)))

rch_lngth <-
  rch_lngth_org %>%
  filter(river != "Peshastin" |
           reach == "P1") %>%
  bind_rows(rch_lngth_org %>%
              filter(str_detect(reach, "P"),
                     reach != "P1",
                     str_detect(type_descp,
                                "No surveys",
                                negate = T)) %>%
              mutate(across(reach,
                            ~ case_when(type == "index" ~ "P3 (P2)",
                                        .default = .)),
                     across(reach,
                            ~ factor(.,
                                     levels = levels(rch_lngth_org$reach))),
                     across(reach_descp,
                            ~ case_when(type == "index" ~ "Ingalls Ck. To Scott Ck.",
                                        .default = .))) %>%
              group_by(river,
                       reach,
                       reach_descp,
                       type) %>%
              summarize(across(type_descp,
                               paste,
                               collapse = ", "),
                        across(length_km,
                               sum),
                        .groups = "drop")) %>%
  arrange(river, reach)


# thalweg data
# data("thlwg_summ")

thlwg_df <- read_excel(data_file,
                       "Final Pooled CV Thalwegs",
                       range = cell_rows(c(3, 7))) %>%
  rename(metric = `...1`) %>%
  select(-2) %>%
  mutate(across(-1,
                as.character)) %>%
  pivot_longer(-1,
               names_to = "reach",
               values_to = "value") %>%
  mutate(across(metric,
                ~ case_match(.,
                             "Distance Requirement Met (> 50% Reach Distance)" ~ "dist_req_met",
                             "Pooled Non-overlapping  Redd Thalweg CVs" ~ "MeanThalwegCV",
                             "Sample Size" ~ "n_samp",
                             "Notes" ~ "notes",
                             .default = .))) %>%
  pivot_wider(names_from = metric,
              values_from = value) %>%
  mutate(across(c(MeanThalwegCV,
                  n_samp),
                as.numeric)) %>%
  mutate(across(reach,
                as_factor)) %>%
  left_join(method_df %>%
              select(river, reach) %>%
              distinct()) %>%
  relocate(river) %>%
  # for 2010, use the mean thalweg CV from N3 for N2 (since N2 is unavailable)
  mutate(across(MeanThalwegCV,
                ~ if_else(reach == "N2",
                          MeanThalwegCV[reach == "N3"],
                          .)),
         across(notes,
                ~ if_else(reach == "N2",
                          "Using thalweg CV from N3 because N2 was only sampled in one year for observer error",
                          .)))
thlwg_df %<>%
  bind_rows(thlwg_df %>%
              filter(str_detect(reach, "P"),
                     reach != "P1") %>%
              select(-notes) %>%
              left_join(rch_lngth_org %>%
                          filter(type == "index")) %>%
              group_by(river) %>%
              summarize(across(MeanThalwegCV,
                               weighted.mean,
                               w = length_km),
                        across(n_samp,
                               sum),
                        .groups = "drop") %>%
              mutate(reach = "P3 (P2)",
                     notes = "Weighted average of P2, P3, P4; weighted by length."))

# thlwg_df %>%
#   inner_join(thlwg_summ %>%
#                select(reach = Reach,
#                       new_cv = MeanThalwegCV))
#
# # replace mainstem reaches with updated thalweg CVs
# thlwg_df %<>%
#   left_join(thlwg_summ %>%
#               select(reach = Reach,
#                      new_cv = MeanThalwegCV)) %>%
#   mutate(across(MeanThalwegCV,
#                 ~ if_else(is.na(new_cv),
#                           .,
#                           new_cv))) %>%
#   select(-new_cv)

# depth
depth_df <- read_excel(data_file,
                       "Water depth",
                       range = "H2:AN24") %>%
  select(reach = Reach,
         `2004...24`:ncol(.)) %>%
  rename_with(.fn  = function(x) str_sub(x, 1, 4),
              .cols = -1) %>%
  pivot_longer(-1,
               names_to = "year",
               values_to = "MeanDepth") %>%
  filter(!is.na(reach)) %>%
  mutate(across(year,
                as.numeric)) %>%
  mutate(across(reach,
                ~ case_when(. == "P3" & year %in% 2004:2010 ~ "P3 (P2)",
                            .default = .)))

#-----------------------------------------------------------------
# data on redd life
redd_life_df <- read_excel(data_file,
                           "Redd life") %>%
  clean_names() %>%
  left_join(method_df %>%
              select(river,
                     reach) %>%
              distinct()) %>%
  mutate(across(river,
                ~ if_else(reach == "I2",
                          "Icicle",
                          .))) %>%
  relocate(river) %>%
  mutate(across(reach,
                as_factor),
         across(reach,
                ~ fct_relevel(.,
                              "W10",
                              after = Inf)))

# summary of average redd life
redd_life_summ <- redd_life_df %>%
  filter(!is.na(river)) %>%
  mutate(rch_grp = case_when(reach == "W10" ~ "W10",
                             reach %in% paste0("W", 6:9) ~ "W6-W9",
                             reach %in% paste0("W", 1:5) ~ "W1-W5",
                             .default = river),
         across(rch_grp,
                as.factor),
         across(rch_grp,
                ~ fct_relevel(.,
                              "W10",
                              after = Inf))) %>%
  mutate(across(rch_grp,
                ~ fct_collapse(.,
                               "W6-W10" = c("W6-W9",
                                            "W10")))) %>%
  group_by(rch_grp) %>%
  summarize(across(redd_life,
                   list(mean = mean,
                        sd = sd)))
# these are the reaches we need to apply a redd-life estimate to
# to estimate visible redds
redd_life_needed <-
  index_rch %>%
  left_join(method_df %>%
              rename(index_reach = reach)) %>%
  filter(year < 2009,
         !is.na(redds),
         is.na(visible_redds)) %>%
  mutate(rch_grp = case_when(index_reach == "W10" ~ "W10",
                             index_reach %in% paste0("W", 6:9) ~ "W6-W9",
                             index_reach %in% paste0("W", 1:5) ~ "W1-W5",
                             .default = river),
         across(rch_grp,
                as.factor),
         across(rch_grp,
                ~ fct_relevel(.,
                              "W10",
                              after = Inf))) %>%
  mutate(across(rch_grp,
                ~ fct_collapse(.,
                               "W6-W10" = c("W6-W9",
                                            "W10")))) %>%
  left_join(redd_life_summ) %>%
  mutate(last_visible = date + days(round(redd_life_mean))) %>%
  # fill in what redds were visible during each survey
  group_by(year,
           river,
           index_reach) %>%
  nest() %>%
  ungroup() %>%
  mutate(vis_data = map(data,
                        .f = function(x) {
                          vis_redds <- x %>%
                            select(date, redds, last_visible) %>%
                            rename(survey_date = date) %>%
                            pivot_longer(c(survey_date,
                                           last_visible),
                                         names_to = "type",
                                         values_to = "date") %>%
                            mutate(across(redds,
                                          ~ if_else(type == "last_visible",
                                                   . * -1,
                                                   .))) %>%
                            arrange(date) %>%
                            mutate(visible_redds = cumsum(redds)) %>%
                            filter(type == "survey_date") %>%
                            pull(visible_redds)

                          x %>%
                            mutate(visible_redds = vis_redds) %>%
                            return()
                        })) %>%
  unnest(vis_data) %>%
  select(-data,
         -rch_grp,
         -starts_with("redd_life"),
         -last_visible)

redd_life_needed

# add visible redds based on redd-life to all index reaches
index_rch %>%
  left_join(method_df %>%
              rename(index_reach = reach)) %>%
  anti_join(redd_life_needed %>%
              select(year:date)) %>%
  # filter(!is.na(redds)) %>%
  bind_rows(redd_life_needed) %>%
  arrange(year,
          river,
          index_reach,
          date) -> index_rch

#-----------------------------------------------------------------
# pull out the census surveys, treat them as known truth
census_surv <- index_rch |>
  filter(method == "Truth")

census_redds <- census_surv %>%
  group_by(year, river,
           index_reach) %>%
  summarize(across(redds,
                   sum,
                   na.rm = T),
            .groups = "drop")

#-----------------------------------------------------------------
# pull out the redds for index reaches where method == "Redd"
# add covariates and apply one observer redd error model

index_redds <-
  index_rch |>
  filter(method == "Redd") %>%
  left_join(rch_lngth %>%
              filter(type == "index" |
                       reach == "N2") %>%
              select(river,
                     index_reach = reach,
                     reach_length_km = length_km)) %>%
  mutate(naive_density_km = visible_redds / reach_length_km) %>%
  left_join(depth_df,
            by = c("year",
                   "index_reach" = "reach")) %>%
  left_join(thlwg_df %>%
              select(index_reach = reach,
                     MeanThalwegCV)) %>%
  filter(!is.na(redds)) %>%
  clean_names() |>
  predict_neterr(species = "Steelhead",
                 num_obs = "one")

#----------------------------------------------------------
# how do covariates compare to range of model covariates?
index_redds |>
  filter(visible_redds > 0) |>
  compare_covars(species = "Steelhead",
                 num_obs = "one",
                 # to focus on z-scored covariates, set this == TRUE
                 z_score = F) |>
  mutate(across(covariate,
                ~ case_match(.,
                             "mean_depth" ~ "Mean Depth",
                             "mean_thalweg_cv" ~ "Mean Thalweg CV",
                             "naive_density_km" ~ "Obs. Redd Density",
                             "net_error" ~ "Net Error")),
         across(covariate,
                ~ factor(.,
                         levels = c('Mean Thalweg CV',
                                    'Obs. Redd Density',
                                    'Mean Depth',
                                    'Net Error')))) |>
  ggplot(aes(x = source,
             y = value,
             fill = source)) +
  geom_boxplot() +
  # geom_hline(yintercept = 0,
  #            linetype = 2) +
  # geom_hline(yintercept = qnorm(c(0.025, 0.975)),
  #            linetype = 3) +
  scale_fill_brewer(palette = "Set1",
                    name = "Source") +
  theme(legend.position = 'bottom') +
  labs(x = "Source",
       y = "Value") +
  facet_wrap(~ covariate,
             scales = 'free_y')


#----------------------------------------------------------
# estimate number of redds in index reaches
# set some thresholds
# minimum number of total redds observed
min_redds = 2
# minimum number of weeks with at least one new redd observed
min_non0_wks = 3


index_redd_results_lst <-
  index_redds %>%
  summarize_redds(species = "Steelhead",
                  group_vars = c("year", "river", "index_reach"),
                  summ_vars = c("year", "river"),
                  new_redd_nm = "redds",
                  # vis_redd_nm = "visible_redds",
                  # net_err_nm = "net_error",
                  # net_se_nm = "net_error_se",
                  min_non0_wks = min_non0_wks,
                  min_redds = min_redds,
                  use_cor = T,
                  date_nm = "date",
                  cor_redd_nm = "redds",
                  reach_nm = "index_reach",
                  add_zeros = T)

index_redd_results <- index_redd_results_lst$rch_est

# set an error threshold that we won't allow net error to fall below
# err_thres = 0.25
err_thres = -0.75

# find another reach to borrow net error estimates from
# make it the closest upstream reach
# if no upstream reaches exist, make it the closest downstream reach
# must be on the same river
ne_switch <- index_redd_results %>%
  filter(err_est < err_thres) %>%
  select(year, river,
         old_reach = index_reach,
         old_err_est = err_est) %>%
  left_join(index_redd_results %>%
              filter(err_se > 0) %>%
              select(year, river,
                     new_reach = index_reach,
                     new_err_est = err_est,
                     new_err_se = err_se) %>%
              distinct()) %>%
  filter(old_reach != new_reach,
         new_err_est > err_thres) %>%
  mutate(old_num = str_extract(old_reach,
                               "[:digit:]+"),
         new_num = str_extract(new_reach,
                               "[:digit:]+"),
         across(ends_with("_num"),
                as.numeric),
         upstrm = if_else(new_num > old_num | old_reach == "W10",
                          T, F),
         diff = abs(old_num - new_num)) %>%
  group_by(year, river,
           old_reach) %>%
  nest(rch_choices = -c(year:old_reach)) %>%
  mutate(match_rch = map_chr(rch_choices,
                             .f = function(x) {
                               if(nrow(x) == 1) {
                                 rch = x$new_reach
                               } else {
                                 upstrm_rchs = x %>%
                                   filter(upstrm)
                                 if(nrow(upstrm_rchs > 0)) {
                                   rch = upstrm_rchs %>%
                                     filter(diff == min(diff)) %>%
                                     pull(new_reach)
                                 } else {
                                   rch = x %>%
                                     filter(diff == min(diff)) %>%
                                     pull(new_reach)
                                 }
                               }
                               return(rch)
                             })) %>%
  unnest(rch_choices) %>%
  filter(new_reach == match_rch) %>%
  select(year:old_reach,
         old_err_est,
         new_reach,
         new_err_est,
         new_err_se)

index_redd_results %>%
  filter(err_est < err_thres) %>%
  select(year, river,
         index_reach,
         err_est) %>%
  anti_join(ne_switch,
            by = c("year", "river",
                   "index_reach" = "old_reach"))

index_redd_results %>%
  filter((year == 2004 &
            river == "Chiwawa") |
           (year == 2007 &
              river == "Wenatchee")) %>%
  select(year:index_reach,
         tot_feat:err_se,
         redd_est,
         GAUC)

ne_switch_res_lst <- index_redd_results %>%
  filter(err_est < err_thres) %>%
  left_join(ne_switch,
            by = c("year", "river",
                   "index_reach" = "old_reach")) %>%
  select(year:index_reach,
         data,
         new_err_est,
         new_err_se) %>%
  unnest(data) %>%
  mutate(net_error = if_else(!is.na(new_err_est),
                            new_err_est,
                            err_thres),
         net_error_se = if_else(!is.na(new_err_se),
                              new_err_se,
                              net_error_se)) %>%
  select(-starts_with("new_err")) %>%
  summarize_redds(group_vars = c("year", "river", "index_reach"),
                 summ_vars = c("year", "river"),
                 new_redd_nm = "redds",
                 # vis_redd_nm = "visible_redds",
                 # net_err_nm = "NetError",
                 # net_se_nm = "NetErrorSE",
                 min_non0_wks = min_non0_wks,
                 min_redds = min_redds,
                 use_cor = T,
                 date_nm = "date",
                 cor_redd_nm = "redds",
                 reach_nm = "index_reach",
                 add_zeros = T)

ne_switch_results <- ne_switch_res_lst$rch_est

# quick comparison of the difference in results
index_redd_results %>%
  select(year:index_reach,
         tot_feat,
         old_err = err_est,
         old_redd_est = redd_est,
         old_GAUC = GAUC) %>%
  inner_join(ne_switch_results %>%
               select(year:index_reach,
                      tot_feat,
                      new_err = err_est,
                      new_redd_est = redd_est,
                      new_GAUC = GAUC)) %>%
  mutate(redd_diff = old_redd_est - new_redd_est,
         perc_diff = redd_diff / old_redd_est) %>%
  arrange(desc(abs(redd_diff)))

# replace old results with new ones
index_redd_results <- index_redd_results %>%
  anti_join(ne_switch_results %>%
              select(year:index_reach,
                     tot_feat)) %>%
  mutate(borrow_NE = F) %>%
  bind_rows(ne_switch_results %>%
              mutate(borrow_NE = T)) %>%
  arrange(year, river, index_reach)


#-----------------------------------------------------------------
# any reaches with an overly large expansion factor?
suspect_rchs <- index_redd_results %>%
  mutate(expn = redd_est / tot_feat,
         expn2 = redd_est / (tot_feat / err_est)) %>%
  # qplot(expn, err_est, data = .)
  filter(expn > 4 |
           expn2 > 1.5)

suspect_rchs %>%
  select(-c(data, gauc_list))


suspect_rchs %>%
  select(year:data) %>%
  unnest(data) %>%
  select(year,
         index_reach, date,
         net_error,
         redds, visible_redds) %>%
  mutate(jday = yday(date)) %>%
  ggplot(aes(x = jday,
             y = redds)) +
  geom_point() +
  facet_wrap(~ index_reach + year) +
  theme_bw() +
  stat_smooth(method = 'glm',
              formula = y ~ x + I(x^2),
              method.args = list(family = quasipoisson),
              fullrange = T,
              se = F)


#-----------------------------------------------------------------
# which non-index reaches don't have an index reach associated with them?
non_index_rch %>%
  anti_join(index_redd_results %>%
              select(year, river, index_reach) %>%
              bind_rows(census_redds %>%
                          select(-redds))) %>%
  left_join(method_df %>%
              rename(non_index_reach = reach)) %>%
  filter(method == "Redd")

# change a few of the associated index reaches
non_index_rch %<>%
  mutate(across(index_reach,
                ~ case_when(year %in% c(2004, 2005) & non_index_reach == "W5" & . == "W6" ~ "W8",
                            year %in% c(2005) & non_index_reach == "W6" & . == "W6" ~ "W8",
                            .default = .)))


# figure out which day on the GAUC curve the non-index reach survey was conducted
non_index_redds <-
  non_index_rch %>%
  mutate(day = difftime(date,
                        ymd(paste0(year, "0215")),
                        units = "days"),
         across(day,
                as.numeric)) %>%
  inner_join(index_redd_results %>%
               select(year, river, index_reach,
                      ind_obs_redds = tot_feat,
                      err_est, err_se,
                      ind_redd_est = redd_est,
                      ind_redd_se = redd_se,
                      data)) %>%
  mutate(vis_redd_data = map(data,
                             .f = function(x) {
                               vis_df <- x %>%
                                 mutate(day = difftime(date,
                                                       ymd(paste0(unique(year(x$date)), "0215")),
                                                       units = "days"),
                                        across(day,
                                               as.numeric)) %>%
                                 select(day,
                                        vis_redds = visible_redds)

                               # if number of redds going up at the end of surveys, add a zero
                               if(vis_df$vis_redds[nrow(vis_df)] > vis_df$vis_redds[nrow(vis_df)-1]) {
                                 vis_df <- vis_df %>%
                                   bind_rows(tibble(day = max(vis_df$day) + 50,
                                                    vis_redds = 0))
                               }
                               return(vis_df)
                             }),
         vis_gauc = map(vis_redd_data,
                        .f = function(x) {
                          g_pois = glm(vis_redds ~ day + I(day^2),
                                       data = x,
                                       family = quasipoisson)
                        })) %>%
  mutate(pred_ind_vis_redds = map2(vis_gauc,
                                   day,
                                   .f = function(x, y) {
                                     predict(x,
                                             newdata = tibble(day = y),
                                             type = "response",
                                             se = T) %>%
                                       enframe() %>%
                                       unnest(value) %>%
                                       filter(str_detect(name, "fit")) %>%
                                       pivot_wider()
                                   })) %>%
  unnest(pred_ind_vis_redds) %>%
  mutate(beta_params = map(vis_gauc,
                           .f = function(x) {
                             params <- coef(x) %>%
                               enframe() %>%
                               mutate(name = c('b0',
                                               'b1',
                                               'b2')) %>%
                               pivot_wider()
                             return(params)
                           }),
         curve_params = map(vis_gauc,
                            .f = function(mod) {
                              x = coef(mod)
                              tau2 = -1/(2*x[3])
                              ms = x[2] * tau2
                              a = exp(x[1] + ms^2/(2*tau2))
                              params = tibble("tau2" = tau2,
                                              "ms" = ms,
                                              "a" = a)
                              return(params)
                            })) %>%
  unnest(ends_with("params")) %>%
  mutate(new_a = redds / fit * a,
         new_b0 = log(new_a) - (ms^2 / (2 * tau2))) %>%
  mutate(new_coef = list(c(b0 = new_b0, b1, b2))) %>%
  mutate(rch_grp = river,
         rch_grp = if_else(non_index_reach == "W10",
                           "W10",
                           if_else(non_index_reach %in% paste0("W", 6:9),
                                   "W6-W9",
                                   if_else(non_index_reach %in% paste0("W", 1:5),
                                           "W1-W5",
                                           rch_grp))),
         across(rch_grp,
                as.factor),
         across(rch_grp,
                fct_relevel,
                "W10",
                after = Inf)) %>%
  mutate(across(rch_grp,
                fct_collapse,
                "W6-W10" = c("W6-W9",
                             "W10"))) %>%
  left_join(redd_life_summ) %>%
  mutate(gauc_vcov = map(vis_gauc,
                         .f = function(x) vcov(x))) %>%
  rowwise() %>%
  mutate(all_params = list(list(beta = c(new_b0, b1, b2),
                                Sigma = gauc_vcov,
                                err_est = err_est,
                                err_se = err_se,
                                redd_life_mean = redd_life_mean,
                                redd_life_sd = redd_life_sd))) %>%
  ungroup() %>%
  mutate(redd_est = map_dbl(all_params,
                            .f = function(lst) {
                              x = lst$beta
                              Fg = sqrt(-pi/x[3])*exp(x[1]-x[2]^2/(4*x[3]))
                              E = Fg / ((lst$err_est + 1) * lst$redd_life_mean) %>%
                                as.numeric()
                              return(E)
                            }),
         redd_se = map_dbl(all_params,
                           .f = function(lst) {
                             cov_mat <- cbind(rbind(lst$Sigma,
                                                    matrix(rep(0, nrow(lst$Sigma) + 2),
                                                           nrow = 2)),
                                              matrix(rep(0, nrow(lst$Sigma) + 6),
                                                     ncol = 2))
                             cov_mat[4,4] <- lst$err_se^2
                             cov_mat[5,5] <- lst$redd_life_sd
                             msm::deltamethod(~(sqrt(-pi/x3)*exp(x1-x2^2/(4*x3))) / ((x4 + 1) * x5),
                                              mean = c(lst$beta, lst$err_est, lst$redd_life_mean),
                                              cov = cov_mat) %>%
                               return()
                           }))

# examine reaches where NI estimates were much higher than the index reach
non_index_redds %>%
  filter(redd_est > 2 * ind_redd_est) %>%
  mutate(ni_jday = yday(date)) %>%
  select(year:index_reach,
         ni_jday,
         redd_est,
         ind_redd_est,
         new_b0, b1, b2,
         vis_redd_data) %>%
  unnest(vis_redd_data) %>%
  ggplot(aes(x = day,
             y = vis_redds)) +
  geom_point() +
  # geom_line() +
  facet_wrap(~ non_index_reach + year,
             scales = "free_y") +
  theme_bw() +
  stat_smooth(method = 'glm',
              formula = y ~ x + I(x^2),
              method.args = list(family = quasipoisson),
              fullrange = T,
              se = F) +
  geom_vline(aes(xintercept = ni_jday),
             linetype = 2) +
  geom_point(aes(x = ni_jday,
                 y = redds),
             shape = 17,
             color = "red")


#------------------------------------------
# estimate redds for non-index reaches
# divide observed counts by associated net error from index reach
#------------------------------------------
non_index_redds <-
  non_index_rch %>%
  inner_join(index_redd_results %>%
               select(year, river, index_reach,
                      ind_obs_redds = tot_feat,
                      err_est, err_se,
                      ind_redd_est = redd_est,
                      ind_redd_se = redd_se)) %>%
  rowwise() |>
  mutate(redd_est = redds / (err_est + 1),
         # redd_se = redds * err_se / err_est^2)
         redd_se = msm::deltamethod(~ x1 / (x2 + 1),
                                    mean = c(redds,
                                             err_est),
                                    cov = diag(c(0, err_se)))) |>
  ungroup()

#------------------------------------------
# put all reaches and estimates together
all_redds <- index_redd_results %>%
  mutate(type = "Index") %>%
  select(year,
         river,
         reach = index_reach,
         type,
         obs_redds = tot_feat,
         err_est,
         err_se,
         redd_est,
         redd_se) %>%
  bind_rows(non_index_redds %>%
              mutate(type = "Non-Index") %>%
              select(year,
                     river,
                     reach = non_index_reach,
                     paired_reach = index_reach,
                     type,
                     obs_redds = redds,
                     err_est,
                     err_se,
                     redd_est,
                     redd_se)) %>%
  bind_rows(census_redds %>%
              mutate(type = "Census") %>%
              select(year,
                     river,
                     reach = index_reach,
                     type,
                     obs_redds = redds) %>%
              mutate(err_est = 1,
                     err_se = 0,
                     redd_est = obs_redds,
                     redd_se = 0)) %>%
  bind_rows(no_err_rch %>%
              mutate(type = 'No Error') %>%
              rename(river = major,
                     reach = minor,
                     obs_redds = total) %>%
              mutate(err_est = 1,
                     err_se = 0,
                     redd_est = obs_redds,
                     redd_se = 0)) %>%
  arrange(year,
          river,
          reach,
          type)

# any estimates less than 0?
all_redds %>%
  filter(redd_est < 0)

# any estimates more than a certain times the number of observed redds?
all_redds %>%
  filter(redd_est > 5 * obs_redds) %>%
  mutate(expn = redd_est / obs_redds) %>%
  arrange(desc(expn))
  # tabyl(type)
  # filter(type == "Index")

# any reaches we're missing?
method_df %>%
  filter(method == "Redd") %>%
  anti_join(all_redds) %>%
  tabyl(river) %>%
  adorn_pct_formatting()

miss_rchs <- method_df %>%
  filter(method == "Redd") %>%
  anti_join(all_redds) %>%
  select(river, year, reach) #%>%
  # filter(river == "Wenatchee")

# any recorded redds in the missing reaches?
miss_rchs %>%
  left_join(non_index_rch %>%
              select(river,
                     reach = non_index_reach,
                     year,
                     redds) %>%
              mutate(type = "NI") %>%
              bind_rows(index_rch %>%
                          group_by(river,
                                   reach = index_reach,
                                   year) %>%
                          summarize(across(redds,
                                           sum,
                                           na.rm = T),
                                    .groups = "drop") %>%
                          mutate(type = "I"))) %>%
  filter(!is.na(redds))

# we'll just assume those "missing" reaches had zero redds


# fix some estimates that led to enormous numbers of redds
# or NAs
all_redds %>%
  filter(is.na(redd_est))

all_redds %>%
  filter(redd_est > 1e3)

# all_redds %<>%
#   filter(redd_est < 1e3) %>%
#   bind_rows(all_redds %>%
#               filter(redd_est > 1e3 | is.na(redd_est)) %>%
#               mutate(redd_est = obs_redds / err_est,
#                      redd_se = obs_redds * err_se / err_est^2)) %>%
#   arrange(year, river, reach)

#----------------------------------------------------
# translate redds to spawners
#----------------------------------------------------
excel_sheets(data_file)

fpr_df <- read_excel(data_file,
                     sheet = "Sex ratios (FPR)",
                     range = "A2:G27") %>%
  clean_names() %>%
  rename(run_year = x1,
         spawn_year = x2) %>%
  mutate(tot_males = wild_males + hatchery_males,
         tot_females = wild_females + hatchery_females,
         tot_sexed = tot_males + tot_females,
         prop_m = tot_males / tot_sexed,
         prop_var = (prop_m * (1 - prop_m)) / tot_sexed,
         fpr = prop_m / (1 - prop_m) + 1) %>%
  mutate(tot_wild = wild_males + wild_females,
         tot_hatch = hatchery_males + hatchery_females,
         tot_origin = tot_wild + tot_hatch,
         phos = tot_hatch / (tot_origin),
         phos_se = sqrt((phos * (1 - phos)) / tot_origin)) %>%
  rowwise() %>%
  mutate(fpr_se = deltamethod(~ x1 / (1 - x1) + 1,
                              mean = prop_m,
                              cov = prop_var)) %>%
  ungroup() %>%
  select(run_year:method,
         starts_with("tot_"),
         starts_with("fpr"),
         starts_with("phos"))

spwn_rch <- all_redds %>%
  left_join(fpr_df %>%
              select(year = spawn_year,
                     starts_with("fpr"),
                     starts_with("phos"))) %>%
  rowwise() %>%
  mutate(Tot_Spawners = redd_est * fpr,
         Hatchery = redd_est * fpr * phos,
         Natural = redd_est * fpr * (1 - phos),
         Hatchery_SE = ifelse(Tot_Spawners > 0,
                              deltamethod(~ x1 * x2 * x3,
                                          mean = c(redd_est, fpr, phos),
                                          cov = diag(c(redd_se, fpr_se, phos_se)^2)),
                              NA),
         Natural_SE = ifelse(Tot_Spawners > 0,
                             deltamethod(~ x1 * x2 * (1 - x3),
                                         mean = c(redd_est, fpr, phos),
                                         cov = diag(c(redd_se, fpr_se, phos_se)^2)),
                             NA)) |>
  ungroup()


wen_old_spwn <- spwn_rch %>%
  filter(year < 2011) %>%
  group_by(year) %>%
  summarize(across(c(obs_redds,
                     redd_est,
                     Tot_Spawners,
                     Natural,
                     Hatchery),
                   sum,
                   na.rm = T),
            across(c(redd_se,
                     Natural_SE,
                     Hatchery_SE),
                   ~ sqrt(sum(.^2, na.rm = T))),
            .groups = "drop") %>%
  select(year,
         contains("redd"),
         Tot_Spawners,
         contains("Natural"),
         contains("Hatchery"))


wen_mid_spwn <- spwn_rch %>%
  filter(between(year, 2011, 2013)) %>%
  mutate(location = if_else(river != "Wenatchee",
                            river,
                            if_else(reach %in% paste0('W', 1:7),
                                    'Below_TUM',
                                    'TUM_bb'))) %>%
  group_by(year, river, location) %>%
  summarize(n_rch = n_distinct(reach, type),
            across(c(starts_with("fpr"),
                     starts_with("phos")),
                   unique),
            across(c(obs_redds,
                     redd_est,
                     Tot_Spawners,
                     Natural,
                     Hatchery),
                   sum,
                   na.rm = T),
            across(c(redd_se,
                     Natural_SE,
                     Hatchery_SE),
                   ~ sqrt(sum(.^2, na.rm = T))),
            .groups = "drop") %>%
  select(year, river,
         location,
         n_rch,
         contains("redd"),
         starts_with("fpr"),
         starts_with("phos"),
         Tot_Spawners,
         contains("Natural"),
         contains("Hatchery"))

#----------------------------------------------------
# grab recent estimates based on combination of redd surveys and PIT data
#----------------------------------------------------
wen_recent_spwn_all <- read_excel(here("T:/DFW-Team FP Upper Columbia Escapement - General",
                                   "UC_Sthd/Estimates",
                                   "UC_STHD_Model_Output.xlsx"),
                              sheet = "Spawning Escp by Origin") %>%
  clean_names() %>%
  filter(population == "Wenatchee") %>%
  select(year = spawn_year,
         stream = river,
         nos_est,
         nos_se,
         hos_est,
         hos_se) %>%
  pivot_longer(-c(year, stream)) %>%
  mutate(type = if_else(str_detect(name, '_se$'),
                                   "se",
                                   "est"),
         origin = str_sub(name, 1, 3)) %>%
  select(-name) %>%
  pivot_wider(names_from = type,
              values_from = value) %>%
  mutate(across(origin,
                recode,
                "nos" = "Natural",
                "hos" = "Hatchery"))

wen_recent_spwn <-
  wen_recent_spwn_all %>%
  filter(stream == "Total") %>%
  select(-stream)


#----------------------------------------------------
# grab estimates from 2011-2013
#----------------------------------------------------
dabom_res <-
  tibble(year = c(2011:2013)) %>%
  mutate(wen_escp = map(year,
                         .f = function(yr) {
                           query_dabom_results(dabom_dam_nm = "PriestRapids",
                                               query_year = yr,
                                               result_type = "escape_summ") |>
                             filter(location %in% c('ICL',
                                                    'PES',
                                                    'MCL',
                                                    'CHM',
                                                    'CHW',
                                                    'CHL',
                                                    'NAL',
                                                    'LWN',
                                                    'WTL',
                                                    'LWE',
                                                    'LWE_bb',
                                                    'TUM_bb',
                                                    'UWE_bb'))
                         }),
         wen_tags =  map(year,
                         .f = function(yr) {
                           all_wen <-
                             query_dabom_results(dabom_dam_nm = "PriestRapids",
                                               query_year = yr,
                                               result_type = "tag_summ") |>
                             rename_with(~ str_replace(., "spawn_node", "final_node")) |>
                             filter(str_detect(path, "LWE")) %>%
                             mutate(area = case_when(final_node %in% c('TUM', 'UWE') ~ 'TUM_bb',
                                                     str_detect(final_node, "^LWE") ~ "Below_TUM",
                                                     .default = "Tributaries"),
                                    across(area,
                                           ~ factor(.,
                                                    levels = c("Below_TUM", 'TUM_bb', 'Tributaries'))))

                           # add in tags from specific tribs (will result in some duplicate tags)
                           wen_tags <-
                             all_wen |>
                             bind_rows(all_wen %>%
                                         filter(str_detect(path, "CHL")) %>%
                                         mutate(location = "Chiwawa")) %>%
                             bind_rows(all_wen %>%
                                         filter(str_detect(path, "NAL")) %>%
                                         mutate(location = "Nason")) |>
                             bind_rows(all_wen %>%
                                         filter(str_detect(path, "PES")) %>%
                                         mutate(location = "Peshastin")) |>
                             mutate(across(location,
                                           ~ factor(.,
                                                    levels = c("Below_TUM",
                                                               "TUM_bb",
                                                               "Tributaries",
                                                               "Peshastin",
                                                               "Nason",
                                                               "Chiwawa")))) |>
                             select(tag_code,
                                    location,
                                    area,
                                    origin,
                                    sex)

                           return(wen_tags)

                           }),
         # pull out tributary escapement estimates
         trib_spawners = map(wen_escp,
                             .f = function(x) {
                               x |>
                                 filter(location %in% c('ICL',
                                                        'PES',
                                                        'MCL',
                                                        'CHM',
                                                        'CHW',
                                                        'CHL',
                                                        'NAL',
                                                        'LWN',
                                                        'WTL')) %>%
                                 select(origin,
                                        location,
                                        Spawners = median,
                                        Spawners_SE = sd) %>%
                                 mutate(across(origin,
                                               ~ case_match(.,
                                                            "W" ~ "Natural",
                                                            "H" ~ "Hatchery",
                                                            .default = .))) |>
                                 arrange(location,
                                         origin)
                             }),
         # pull out mainstem escapement estimates
         main_spawners = map(wen_escp,
                             .f = function(x) {
                               x |>
                                 filter(location %in% c('LWE',
                                                        'LWE_bb',
                                                        'TUM_bb',
                                                        'UWE_bb')) %>%
                                 mutate(area = case_match(location,
                                                          'LWE' ~ 'Wen_all',
                                                          'LWE_bb' ~ 'Below_TUM',
                                                          'UWE_bb' ~ 'TUM_bb',
                                                          .default = location)) |>
                                 mutate(across(origin,
                                               ~ case_match(.,
                                                            "W" ~ "Natural",
                                                            "H" ~ "Hatchery",
                                                            .default = .))) |>
                                 arrange(location,
                                         origin) |>
                                 group_by(area,
                                          origin) %>%
                                 summarise(estimate = sum(median),
                                           se = sqrt(sum(sd^2)),
                                           .groups = "drop")
                             }))

# extract tributary spawners
trib_spawners = dabom_res %>%
  select(year, trib_spawners) %>%
  unnest(trib_spawners) %>%
  mutate(across(location,
                ~ case_match(.,
                             'CHL' ~ 'Chiwawa',
                             'CHM' ~ 'Chumstick',
                             'CHW' ~ 'Chiwaukum',
                             'ICL' ~ 'Icicle',
                             'LWN' ~ 'Little Wenatchee',
                             'MCL' ~ 'Mission',
                             'NAL' ~ 'Nason',
                             'PES' ~ 'Peshastin',
                             'WTL' ~ 'White River',
                             .default = .))) %>%
  pivot_wider(names_from = origin,
              values_from = c(Spawners,
                              Spawners_SE),
              names_glue = "{origin}_{.value}") %>%
  rlang::set_names(function(x) str_remove(x, "_Spawners")) %>%
  rename(river = location)


# generate fish / redd and pHOS for different areas
fpr_mid_df <-
  dabom_res %>%
  select(year, wen_tags) %>%
  unnest(wen_tags) %>%
  group_by(year,
           area,
           location) %>%
  summarize(n_male = n_distinct(tag_code[sex == "M"]),
            n_female = n_distinct(tag_code[sex == "F"]),
            n_sexed = n_male + n_female,
            n_wild = n_distinct(tag_code[origin == "W"]),
            n_hatch = n_distinct(tag_code[origin == "H"]),
            n_origin = n_wild + n_hatch,
            .groups = "drop") %>%
  mutate(prop_m = n_male / n_sexed,
         prop_se = sqrt((prop_m * (1 - prop_m)) / (n_sexed)),
         fpr = (prop_m) / (1 - prop_m) + 1) %>%
  rowwise() %>%
  mutate(fpr_se = deltamethod(~ x1 / (1 - x1) + 1,
                              mean = prop_m,
                              cov = prop_se^2)) %>%
  ungroup() %>%
  mutate(phos = n_hatch / n_origin,
         phos_se = sqrt((phos * (1 - phos)) / (n_origin))) |>
  filter(!(area == "Tributaries" &
             is.na(location))) |>
  mutate(across(location,
                ~ case_when(is.na(.) ~ area,
                            .default = .))) |>
  select(-area)


# adjust fish / redd for errors in Priest sex calls
# the excel file contains rounded numbers, so re-calculate
# various statistics for use in analyses
# estimate error rate for each sex
sex_err_rate <-
  dabom_res %>%
  select(spawn_year = year,
         wen_tags) %>%
  unnest(wen_tags) |>
  rename(sex_field = sex) |>
  inner_join(read_excel(paste("T:/DFW-Team FP Upper Columbia Escapement - General/UC_Sthd/inputs/Bio Data/Sex and Origin PRD-Brood Comparison Data",
                                             "STHD UC Brood Collections_2011 to current.xlsx",
                                             sep = "/"),
                                       sheet = "Brood Collected_PIT Tagged Only") |>
                      clean_names() |>
                      rename(tag_code = recaptured_pit) |>
                      select(spawn_year,
                                    tag_code,
                                    sex_final) |>
                      distinct(),
                    by = join_by(spawn_year, tag_code)) |>
  filter(!is.na(sex_final),
                !is.na(sex_field)) |>
  mutate(agree = if_else(sex_field == sex_final,
                                       T, F)) |>
  group_by(spawn_year,
                  sex = sex_field) |>
  summarize(n_tags = n_distinct(tag_code),
                   n_true = sum(agree),
                   n_false = sum(!agree),
                   .groups = "drop") |>
  mutate(binom_ci = purrr::map2(n_false,
                                       n_tags,
                                       .f = function(x, y) {
                                         DescTools::BinomCI(x, y) |>
                                           as_tibble()
                                       })) |>
  unnest(binom_ci) |>
  clean_names() |>
  rename(perc_false = est) |>
  mutate(perc_se = sqrt((perc_false * (1 - perc_false)) / n_tags)) |>
  relocate(perc_se,
                  .after = "perc_false")

adj_fpr <-
  fpr_mid_df |>
  select(spawn_year = year,
         location,
         n_male,
         n_female) |>
  pivot_longer(cols = c(n_male,
                        n_female),
               names_to = "sex",
               values_to = "n_fish") |>
  mutate(
    across(sex,
           ~ str_remove(.,
                        "^n_")),
    across(sex,
           str_to_title)) |>
  mutate(
    across(sex,
           ~ case_match(.,
                        "Male" ~ "M",
                        "Female" ~ "F",
                        .default = .))) |>
  left_join(sex_err_rate |>
              select(spawn_year,
                     sex,
                     starts_with("perc_")),
            by = c("spawn_year", "sex")) |>
  pivot_wider(names_from = sex,
              values_from = c(n_fish,
                              perc_false,
                              perc_se)) |>
  mutate(true_male = n_fish_M - (n_fish_M * perc_false_M) + (n_fish_F * perc_false_F),
         true_female = n_fish_F - (n_fish_F * perc_false_F) + (n_fish_M * perc_false_M),
         across(starts_with("true"),
                round_half_up)) |>
  rowwise() |>
  mutate(true_m_se = msm::deltamethod(~ x1 - (x1 * x2) + (x3 * x4),
                                      mean = c(n_fish_M,
                                               perc_false_M,
                                               n_fish_F,
                                               perc_false_F),
                                      cov = diag(c(0,
                                                   perc_se_M,
                                                   0,
                                                   perc_se_F)^2)),
         true_f_se = msm::deltamethod(~ x1 - (x1 * x2) + (x3 * x4),
                                      mean = c(n_fish_F,
                                               perc_false_F,
                                               n_fish_M,
                                               perc_false_M),
                                      cov = diag(c(0,
                                                   perc_se_F,
                                                   0,
                                                   perc_se_M)^2))) |>
  mutate(n_sexed = true_male + true_female,
         prop_m = true_male / (true_male + true_female),
         prop_se = msm::deltamethod(~ x1 / (x1 + x2),
                                    mean = c(true_male,
                                             true_female),
                                    cov = diag(c(true_m_se,
                                                 true_f_se)^2)),
         fpr = (prop_m) / (1 - prop_m) + 1,
         fpr_se = msm::deltamethod(~ x1 / (1 - x1) + 1,
                                   mean = prop_m,
                                   cov = prop_se^2)) |>
  ungroup() |>
  rename(n_male = true_male,
         n_female = true_female,
         year = spawn_year) |>
  left_join(fpr_mid_df |>
              select(year,
                     location,
                     n_wild,
                     n_hatch,
                     contains("n_hor"),
                     n_origin,
                     starts_with("phos")),
            by = c("year", "location")) |>
  select(any_of(names(fpr_mid_df)))

# look at changes to fish/redd
fpr_mid_df |>
  select(year,
         location,
         old_fpr = fpr) |>
  left_join(adj_fpr |>
              select(year,
                     location,
                     adj_fpr = fpr)) |>
  ggplot(aes(x = old_fpr,
             y = adj_fpr)) +
  geom_abline(linetype = 2) +
  geom_point(aes(color = location)) +
  labs(x = "Old FpR",
       y = "Adjusted FpR")

# if any fpr values are Inf, use the older ones
if(sum(adj_fpr$fpr == Inf) > 0) {
  adj_fpr <- adj_fpr |>
    left_join(fpr_all |>
                select(location,
                       old_fpr = fpr,
                       old_se = fpr_se)) |>
    mutate(fpr = if_else(is.na(fpr) | fpr == Inf,
                         old_fpr,
                         fpr),
           fpr_se = if_else(is.na(fpr_se) | fpr_se == Inf,
                            old_se,
                            fpr_se)) |>
    select(-starts_with("old"))
}

fpr_mid_df <- adj_fpr


wen_mid_spwn %>%
  select(-c(Tot_Spawners:Hatchery_SE),
         -c(n_rch:redd_se)) %>%
  left_join(fpr_mid_df %>%
              select(year,
                     location,
                     fpr2 = fpr,
                     fpr_se2 = fpr_se,
                     phos2 = phos,
                     phos_se2 = phos_se)) %>%
  mutate(fpr_diff = fpr - fpr2,
         phos_diff = phos - phos2,
         fpr_se_diff = fpr_se - fpr_se2,
         phos_se_diff = phos_se - phos_se2)

# recalculate spawners from redds, using PIT-tag based fish/redd when possible
wen_mid_spwn <-
  wen_mid_spwn %>%
  select(year:redd_se) %>%
  inner_join(fpr_mid_df %>%
              select(year,
                     location,
                     starts_with("fpr"),
                     starts_with("phos"))) %>%
  bind_rows(wen_mid_spwn %>%
              select(year:redd_se,
                     starts_with("fpr"),
                     starts_with("phos")) %>%
              anti_join(fpr_mid_df %>%
                          select(year, location))) %>%
  arrange(year, river, location) %>%
  rowwise() %>%
  mutate(Tot_Spawners = redd_est * fpr,
         Hatchery = redd_est * fpr * phos,
         Natural = redd_est * fpr * (1 - phos),
         Hatchery_SE = ifelse(Tot_Spawners > 0,
                              deltamethod(~ x1 * x2 * x3,
                                          mean = c(redd_est, fpr, phos),
                                          cov = diag(c(redd_se, fpr_se, phos_se)^2)),
                              NA),
         Natural_SE = ifelse(Tot_Spawners > 0,
                             deltamethod(~ x1 * x2 * (1 - x3),
                                         mean = c(redd_est, fpr, phos),
                                         cov = diag(c(redd_se, fpr_se, phos_se)^2)),
                             NA)) |>
  ungroup() %>%
  select(any_of(names(wen_mid_spwn)))

wen_mid_spwn %>%
  mutate(method = "Redds") %>%
  select(year,
         river,
         location,
         method,
         starts_with('Natural'),
         starts_with("Hatchery")) %>%
  bind_rows(trib_spawners %>%
              mutate(location = river,
                     method = "DABOM")) %>%
  arrange(year, river,
          location,
          method) %>%
  group_by(year) %>%
  summarize(across(c(Natural,
                     Hatchery),
                   sum),
            across(ends_with("_SE"),
                   ~ sqrt(sum(.^2, na.rm = T))))

#----------------------------------------------------
# determine percentage of fish moving to Mission, Chumstick and Chiwaukum
#----------------------------------------------------
trib_3_est <- trib_spawners %>%
  filter(river %in% c("Mission",
                      "Chumstick",
                      "Chiwaukum")) %>%
  pivot_longer(cols = -c(year, river)) %>%
  mutate(type = if_else(str_detect(name, "_SE$"),
                        "se",
                        "est"),
         origin = str_remove(name, "_SE$")) %>%
  select(-name) %>%
  pivot_wider(names_from = type,
              values_from = value) %>%
  bind_rows(wen_recent_spwn_all %>%
              filter(stream %in% c("Mission",
                                   "Chumstick",
                                   "Chiwaukum")) %>%
              rename(river = stream) |>
              mutate(across(c(est, se),
                            ~ replace_na(., 0))))


wen_tot_est <-
  wen_mid_spwn %>%
  mutate(method = "Redds") %>%
  select(year,
         river, location,
         method,
         starts_with('Natural'),
         starts_with("Hatchery")) %>%
  bind_rows(trib_spawners %>%
              mutate(location = river,
                     method = "DABOM")) %>%
  arrange(year, river, location) %>%
  group_by(year) %>%
  summarize(across(c(Natural,
                     Hatchery),
                   sum),
            across(ends_with("_SE"),
                   ~ sqrt(sum(.^2, na.rm = T)))) %>%
  bind_rows(wen_recent_spwn %>%
              pivot_wider(names_from = origin,
                          values_from = c(est, se),
                          names_glue = "{origin}_{.value}") %>%
              rlang::set_names(~ str_remove(., "_est$")) %>%
              rlang::set_names(~ str_replace(., "_se$", "_SE"))) %>%
  pivot_longer(cols = -c(year)) %>%
  mutate(type = if_else(str_detect(name, "_SE$"),
                        "se",
                        "est"),
         origin = str_remove(name, "_SE$")) %>%
  select(-name) %>%
  pivot_wider(names_from = type,
              values_from = value) %>%
  mutate(river = "Wen_Pop")


expn_ratio <-
  trib_3_est %>%
  group_by(year, origin) %>%
  summarize(across(est,
                   sum),
            across(se,
                   ~ sqrt(sum(.^2))),
            .groups = "drop") %>%
  left_join(wen_tot_est %>%
              select(year, origin,
                     tot_est = est,
                     tot_se = se)) %>%
  mutate(prop_tribs = est / tot_est) %>%
  mutate(expn_fct = 1 + (prop_tribs / (1 - prop_tribs))) %>%
  group_by(origin) %>%
  summarize(n_yrs = n_distinct(year),
            across(c(prop_tribs, expn_fct),
                   list(mean = mean,
                        sd = sd)))# %>%
  # mutate(expn_fct2 = 1 + (prop_tribs_mean / (1 - prop_tribs_mean)))

# expand older estimates of spawners by expansion ratio
wen_old_spwn_expn <-
  wen_old_spwn %>%
  select(year,
         starts_with("Natural"),
         starts_with("Hatchery")) %>%
  pivot_longer(-year) %>%
  mutate(type = if_else(str_detect(name, "_SE$"),
                        "se", "est"),
         origin = str_split(name, "_", simplify = T)[,1]) %>%
  select(-name) %>%
  pivot_wider(names_from = type,
              values_from = value) %>%
  left_join(expn_ratio %>%
              select(origin,
                     starts_with("expn"))) %>%
  mutate(new_est = est * expn_fct_mean,
         new_se = sqrt((est^2 * expn_fct_sd^2) + (expn_fct_mean^2 * se^2))) %>%
  select(year, origin,
         starts_with('new')) %>%
  pivot_longer(starts_with('new')) %>%
  mutate(across(name,
                str_remove,
                "new_")) %>%
  pivot_wider(names_from = c(origin, name),
               values_from = value) %>%
  rlang::set_names(nm = function(x) {
    x %>%
      str_remove("_est$") %>%
      str_replace("_se", "_SE")
  }) %>%
  mutate(Tot_Spawners = Natural + Hatchery) %>%
  left_join(wen_old_spwn %>%
              select(year:redd_se)) %>%
  select(any_of(names(wen_old_spwn)))

wen_old_spwn_expn
wen_old_spwn


#----------------------------------------------------
# compile full time-series
#----------------------------------------------------
new_ts <- wen_old_spwn_expn %>%
  select(year,
         starts_with('Natural'),
         starts_with("Hatchery")) %>%
  bind_rows(wen_mid_spwn %>%
              mutate(method = "Redds") %>%
              select(year,
                     river, location,
                     method,
                     starts_with('Natural'),
                     starts_with("Hatchery")) %>%
              bind_rows(trib_spawners %>%
                          mutate(location = river,
                                 method = "DABOM")) %>%
              arrange(year, river, location) %>%
              group_by(year) %>%
              summarize(across(c(Natural,
                                 Hatchery),
                               sum),
                        across(ends_with("_SE"),
                               ~ sqrt(sum(.^2, na.rm = T))))) %>%
  bind_rows(wen_recent_spwn %>%
              pivot_wider(names_from = origin,
                          values_from = c(est, se),
                          names_glue = "{origin}_{.value}") %>%
              rlang::set_names(~ str_remove(., "_est$")) %>%
              rlang::set_names(~ str_replace(., "_se$", "_SE")))

new_ts %>%
  pivot_longer(-year) %>%
  mutate(type = if_else(str_detect(name, "_SE"),
                        "se", "est"),
         origin = str_remove(name, "_SE")) %>%
  select(-name) %>%
  pivot_wider(names_from = type,
              values_from = value) %>%
  mutate(lci = qnorm(0.025, est, se),
         uci = qnorm(0.975, est, se),
         across(lci,
                ~ if_else(. < 0, 0, .))) %>%
  ggplot(aes(x = year,
             y = est,
             color = origin,
             fill = origin)) +
  geom_ribbon(aes(ymin = lci,
                  ymax = uci),
              color = NA,
              alpha = 0.2) +
  geom_line() +
  geom_point(size = 2) +
  scale_color_viridis_d(end = 0.7,
                        name = "Origin") +
  scale_fill_viridis_d(end = 0.7,
                        name = "Origin") +
  labs(x = "Spawn Year",
       y = "Wenatchee Spawners")

# save time-series
new_ts %>%
  rename(nor_spwn = Natural,
         nor_se = Natural_SE,
         hor_spwn = Hatchery,
         hor_se = Hatchery_SE) %>%
  mutate(across(ends_with("spwn"),
                round_half_up)) %>%
  select(year,
         hor_spwn, nor_spwn,
         hor_se, nor_se) %>%
  write_csv(here("analysis/data/derived_data",
                 paste0("Wenatchee_Sthd_Spwn_",
                        paste(range(new_ts$year),
                              collapse = "-"),
                        ".csv")))



#----------------------------------------------------
# examine reachs with MUCH larger estimates than counted redds
#----------------------------------------------------
weird_rch <- all_redds %>%
  filter(redd_est > obs_redds * 5) %>%
  mutate(expn = redd_est / obs_redds) %>%
  select(year,
         river,
         type,
         reach,
         paired_reach,
         expn)


weird_rch %>%
  mutate(index_reach = if_else(!is.na(paired_reach),
                               paired_reach,
                               reach)) %>%
  left_join(all_redds)

weird_rch

weird_rch %>%
  rename(index_reach = reach) %>%
  left_join(index_redds) %>%
  mutate(day = as.integer(1:n())) %>%
  ggplot(aes(x = day,
             y = redds)) +
  geom_point() +
  # geom_line() +
  facet_wrap(~ index_reach + year,
             scales = "free_y") +
  theme_bw() +
  stat_smooth(method = 'glm',
              formula = y ~ x + I(x^2),
              method.args = list(family = quasipoisson),
              fullrange = T,
              se = F) +
  labs(x = "Survey Number")

mod_df <- weird_rch %>%
  rename(index_reach = reach) %>%
  left_join(index_redds) %>%
  mutate(day = 1:n()) %>%
  select(day,
         redds)

# if(mod_df$redds[mod_df$day == max(mod_df$day)] != 0) {
  mod_df <- mod_df %>%
    bind_rows(tibble(redds = 0,
                     day = max(mod_df$day) + 1)) %>%
    arrange(day)
# }

ggplot(mod_df,
       aes(x = day,
           y = redds)) +
  geom_point() +
  # geom_line() +
  theme_bw() +
  stat_smooth(method = 'glm',
              formula = y ~ x + I(x^2),
              method.args = list(family = quasipoisson),
              fullrange = T,
              se = F) +
  labs(x = "Survey Number")


fit_gauc(data = mod_df,
         v = 1 + -0.572,
         v_se = 0.0359)$E

# weird_rch %>%
#   left_join(non_index_redds) %>%
#   slice(7) %>%
#   pull(vis_redd_data)

weird_rch %>%
  left_join(non_index_redds) %>%
  mutate(ni_jday = yday(date)) %>%
  select(year:index_reach,
         ni_jday,
         redd_est,
         ind_redd_est,
         new_b0, b1, b2,
         vis_redd_data) %>%
  unnest(vis_redd_data) %>%
  ggplot(aes(x = day,
             y = vis_redds)) +
  geom_point() +
  # geom_line() +
  facet_wrap(~ non_index_reach + year,
             scales = "free_y") +
  theme_bw() +
  stat_smooth(method = 'glm',
              formula = y ~ x + I(x^2),
              method.args = list(family = quasipoisson),
              fullrange = T,
              se = F) +
  geom_vline(aes(xintercept = ni_jday),
             linetype = 2) +
  geom_point(aes(x = ni_jday,
                 y = redds),
             shape = 17,
             color = "red")



all_redds %>%
  mutate(redd_est_naive = obs_redds / err_est) %>%
  mutate(expn1 = redd_est / obs_redds,
         expn2 = redd_est_naive / obs_redds) %>%
  arrange(desc(expn1)) %>%
  filter(redd_est > 2 * redd_est_naive)
