library(dplyr)
library(lmerTest)
library(forcats)

user <- redivis::user("stanford_templates")
dataset <- user$dataset("many_babies_1:9esa:v1_0")
table <- dataset$table("03_data_trial_main:nh93")

# d <- read_csv(here("processed_data/03_data_trial_main.csv"),
#               na = c("NA", "N/A")) |>
d <- table$to_tibble() |>
  mutate(method = case_when(
    method == "singlescreen" ~ "Central fixation",
    method == "eyetracking" ~ "Eye tracking",
    method == "hpp" ~ "HPP",
    TRUE ~ method))

d_lmer <- d |>
  filter(looking_time != "NA") |>
  mutate(looking_time = as.numeric(looking_time)) |>
  filter(trial_type != "train") |>
  mutate(log_lt = log(looking_time),
         age_mo = scale(age_mo, scale = FALSE),
         trial_num = trial_num, 
         item = paste0(stimulus_num, trial_type)) |>
  filter(!is.na(log_lt), !is.infinite(log_lt))

mod_lmer <- lmer(log_lt ~ trial_type * method +
                   trial_type * trial_num +
                   age_mo * trial_num +
                   trial_type * age_mo * nae +
                   (1 | subid_unique) +
                   (1 | item) + 
                   (1 | lab), 
                 data = d_lmer)

lt_coefs <- summary(mod_lmer)$coef |>
  as_tibble |>
  mutate_at(c("Estimate","Std. Error","df", "t value", "Pr(>|t|)"), 
            function (x) signif(x, digits = 3)) |>
  rename(SE = `Std. Error`, 
         t = `t value`,
         p = `Pr(>|t|)`) |>
  select(-df) |>
  mutate(coef = c("Intercept", "IDS", "Eye-tracking", "HPP", 
                  "Trial #", "Age", "NAE", "IDS * Eye-tracking", 
                  "IDS * HPP", 
                  "IDS * Trial #", "Trial # * Age", "IDS * Age", "IDS * NAE", 
                  "Age * NAE", "IDS * Age * NAE"))

# readr::write_rds(lt_coefs, "data/lt_coefs.rds")

lt_fits <- d_lmer |>
  mutate(fitted = predict(mod_lmer, re.form = NA) |> exp(),
         age_group = fct_inorder(age_group)) |>
  distinct(trial_type, method, trial_num, nae, age_group, age_mo, fitted) |>
  group_by(trial_type, method, trial_num, nae, age_group) |>
  summarise(fitted = mean(fitted), n = n()) |>
  ungroup() |>
  mutate(nae = if_else(nae, "NAE", "non-NAE"))
  # mutate(age_order = as.numeric(str_extract(age_group, "^[0-9]+")))

readr::write_rds(lt_fits, "data/lt_fits.rds")

# ggplot(d_fits, aes(x = trial_num, y = fitted)) +
#   facet_grid(vars(method), vars(age_group)) +
#   geom_line(aes(colour = nae, linetype = trial_type))
  # geom_point()

# papaja::apa_table(coefs, 
#                   caption = "Coefficient estimates from a linear mixed effects model predicting log looking time.", 
#                   format.args = list(digits = 3),
#                   col.names =c("","Estimate","$SE$","$t$","$p$"),
#                   align=c("l","l","c","c","c"))