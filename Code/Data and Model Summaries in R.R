################################################################################
## modelsummary: Data and Model Summaries in R                                ##
## Replication script for the Journal of Statistical Software.                ##
## Requires version 0.9.4 of modelsummary                                     ##
################################################################################

# Install and require packages
install_and_require <- function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dep = TRUE)
    if (!require(x, character.only = TRUE, quiet = TRUE))
      stop("Package not found")
  }
  return(TRUE)
}
pkgs <- c("modelsummary", "dragracer", "ggplot2", "gt", "kableExtra", "lme4",
          "lmtest", "palmerpenguins", "sandwich")
sapply(pkgs, install_and_require)

# Section 2.1: Illustrations > Data Summaries
data("penguins", package = "palmerpenguins")
variables <- c(
  "flipper_length_mm" = "Flipper",
  "bill_length_mm"    = "Bill",
  "body_mass_g"       = "Body Mass",
  "sex"               = "Sex",
  "island"            = "Island",
  "species"           = "Species")
penguins <- setNames(penguins[, names(variables)], variables)

penguins$Sex <- tools::toTitleCase(as.character(penguins$Sex))
penguins$Sex <- factor(penguins$Sex)

datasummary_skim(penguins)
datasummary_skim(df_selected_vars)
datasummary_balance(~death, data = df_selected_vars)

datasummary_correlation(df_selected_vars)

datasummary_crosstab(Island ~ Species, data = penguins)

datasummary_crosstab(Island ~ Sex * Species, data = penguins)

datasummary(Flipper + `Body Mass` ~ Mean + SD, data = penguins)

Range <- function(x) {
  sprintf("[%s, %s]", min(x, na.rm = TRUE), max(x, na.rm = TRUE))
}

datasummary(Mean + SD + Range ~ Flipper + Bill, data = penguins)
datasummary(Flipper + Bill ~ Sex * Mean + SD, data = penguins)
datasummary(Flipper + Bill ~ Sex * (Mean + SD), data = penguins)
datasummary(Island + 1 ~ N + Percent(), data = penguins)
datasummary(Sex * Species ~ Heading("#")*N + Heading("%")*Percent(),
            data = penguins)

# Section 2.2: Illustrations > Model Summaries
data("rpdr_contep", package = "dragracer")
dragracer <- rpdr_contep

mod <- lm(rank ~ minichalw + missc + episode, data = dragracer)
modelsummary(mod, gof_omit = "RMSE")

library(lme4)
models <- list(
  "LM"    = lm(rank ~ minichalw + episode,
               data = dragracer),
  "LMER"  = lmer(rank ~ minichalw + missc + episode + (1|season),
                 data = dragracer),
  "GLMER" = glmer(rank ~ minichalw + missc + episode + (1|season),
                  data = dragracer, family = poisson))

modelsummary(models, output = "latex", align = "lddd")

coef_labels <- c(
  "minichalw"   = "Mini Challenge Winner",
  "missc"       = "Miss Congeniality",
  "episode"     = "Episode",
  "(Intercept)" = "Constant")

modelsummary(
  models,
  coef_rename = coef_labels,
  fmt         = 1,
  vcov        = list("robust", "classical", "classical"),
  gof_omit    = "^(?!Num|Std)",
  conf_level  = 0.99,
  statistic   = NULL,
  title       = "A \\code{modelsummary} table about RuPaul's Drag Race.",
  notes       = "Source: dragracer package (Miller 2020).",
  estimate    = "{estimate} [{conf.low}, {conf.high}]")


modelplot(models, coef_rename = coef_labels)

vcov_list <- list(
  "Classical"           = "classical",
  "Robust"              = "robust",
  "Clustered"           = ~episode,
  "Andrews' kernel HAC" = sandwich::kernHAC,
  "Newey-West"          = sandwich::NeweyWest,
  "Bootstrap"           = sandwich::vcovBS)


library(ggplot2)

mod_list <- lapply(vcov_list, function(x) mod)
modelplot(mod_list,
          vcov = vcov_list,
          conf_level = 0.99,
          coef_omit = "Intercept|episode",
          coef_rename = coef_labels) +
  scale_color_brewer(palette = "Dark2") +
  guides(color = guide_legend(reverse = TRUE)) +
  theme(text = element_text(family = "Times"))


# Section 2.2: Illustrations > Customizing Tables
library(kableExtra)
modelsummary(models,
             stars = TRUE,
             gof_omit = ".*",
             coef_rename = coef_labels) |>
  add_header_above(c(" " = 1, "Gaussian" = 2, "Poisson" = 1)) |>
  row_spec(3, background = "pink", bold = TRUE)

library(gt)
img <- "https://www.r-project.org/logo/Rlogo.svg"
modelsummary(mod, output = "gt") |>
  text_transform(locations = cells_body(columns = 2, rows = 1),
                 fn = function(x) web_image(url = img))

# Section 4.2: Package Internals > Transformations
mod_logit <- glm(vs ~ hp + mpg, data = mtcars, family = binomial)
modelsummary(mod_logit, exponentiate = TRUE, statistic = "conf.int")

tidy_custom.lm <- function(x, ...) {
  out <- data.frame(
    "term" = names(coef(x)),
    "estimate" = ifelse(coef(x) > 0, "↑", "↓"))
  return(out)
}

modelsummary(mod,
             statistic = NULL,
             gof_omit = ".*")


# Section 4.3: Package Internals > Adding Custom Statistics
tidy_custom.lm <- function(x, ...) {
  out <- broom::tidy(x)
  out$bonferroni <- p.adjust(out$p.value, n = 10, method = "bonferroni")
  out$holm <- p.adjust(out$p.value, n = 10, method = "holm")
  return(out)
}

glance_custom.lm <- function(x, ...) {
  out <- data.frame("Num.Comparisons" = "10",
                    "Model" = class(x)[1])
  return(out)
}

modelsummary(mod,
             statistic = c("p = {p.value}",
                           "p (Bonferroni) = {bonferroni}",
                           "p (Holm) = {holm}"),
             coef_omit = "^(?!minichalw)",
             gof_omit = "R2|IC|Log|F|RMSE",
             fmt = 4)

