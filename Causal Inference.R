library(tidyverse)
library(MatchIt)
library(broom)

# -----------------------------
# Load data
# -----------------------------
dat <- read_csv(
  "https://github.com/rdpeng/stat322E_public/raw/main/data/maait_erad.csv"
)

# look at the data
glimpse(dat)

# -----------------------------
# Fit Propensity Score Model
# -----------------------------
pfit <- matchit(
  erad ~ age + act + ics + food + holes + roach + dmousef,
  data = dat,
  method = "nearest",   # 1-to-1 nearest neighbor matching
  ratio = 1
)
pfit

# -----------------------------
# Extract Propensity Scores
# -----------------------------
dat |> 
  mutate(pscore = predict(pfit$model, type = "response")) |> 
  arrange(desc(pscore))

# summary of matching
summary(pfit)

plot(summary(pfit))

# -----------------------------
# Construct Matched Dataset
# -----------------------------
mdat <- match.data(pfit)
mdat

# check matched data
mdat |> 
  group_by(erad) |> 
  summarize(across(age:dmousef, mean))

# -----------------------------
# Estimate Treatment Effect
# -----------------------------
fit <- lm(preFEV ~ erad, data = mdat)

summary(fit)
