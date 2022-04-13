## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----set-a-seed---------------------------------------------------------------
set.seed(1313L)

## ----libs---------------------------------------------------------------------
library(lme4)
library(magrittr)
library(npicc)

## ----simulate-data------------------------------------------------------------
df = make_sample(
  nSubj = 10,
  nObsPerSubj = c(2,4,6),
  overall_mean = log(0.1),
  subj_sd = 1,
  obs_sd = 0.25,
  obs_trans = exp
)

# turn subj into a factor, levels ordered by mean level
df$subj = reorder(df$subj, df$obs, mean)

#print result
head(df, n = 10L)

## ----visualise-data, fig.cap="Plot showing observed value of outcome by subject."----
par(mar = c(4.5, 4.5, 1.5, 1.5))

plot(
  obs_trans ~ jitter(as.integer(subj), factor = 0.1),
  data = df,
  pch=16,
  cex=0.2,
  xlab="Subject #",
  ylab="Value of outcome"
)

## ----calculate-icc-values-----------------------------------------------------
# Standard variance components agreement stat
lmer_icc = if(require("lme4") && require("magrittr")) {
  lmer(obs_trans ~ (1|subj), data = df) %>%
  VarCorr %>%
  as.data.frame %>%
  extract2("vcov") %>%
  {.[1] / sum(.)}
} else {
  NA_real_
}

# New concordance-style agreement stat
npicc_icc = getICC(x = df$subj, y = df$obs_trans)

