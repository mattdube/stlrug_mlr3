library(tidymodels)
library(readr)
library(forcats)

# attrDT <- fread(here::here("00_Data/", "attrition_train.csv"))
# attrDT[,Attrition:=as.factor(Attrition)]

# load data
attr_tbl <- read_csv(here::here("00_Data/", "attrition_train.csv"))
str(attr_tbl)

attr_tbl <- attr_tbl %>%
  mutate(Attrition = as_factor(Attrition))

attr_split <- initial_split(attr_tbl, prop = 0.7)
attr_train <- attr_split %>% training()
attr_test <- attr_split %>% testing()

attr_tbl %>%
  select(Attrition) %>% table()

attr_rpart <- decision_tree(mode = "classification") %>%
  set_engine("rpart") %>%
  fit(Attrition ~ ., data = attr_train)

attr_ranger <- rand_forest(mode = "classification") %>%
  set_engine("ranger") %>%
  fit(Attrition ~ ., data = attr_train)

rpart_pred <- predict(attr_rpart, attr_test)
ranger_pred <- predict(attr_ranger, attr_test)

attr_test %>%
  bind_cols(rpart_pred) %>%
  metrics(truth = Attrition, estimate = .pred_class)

attr_test %>%
  bind_cols(ranger_pred) %>%
  metrics(truth = Attrition, estimate = .pred_class)

attr_ranger_probs <- 
  attr_ranger %>%
  predict(attr_test, type = "prob") %>%
  bind_cols(attr_test)

attr_rpart_probs <- attr_rpart %>% predict(attr_test, type = "prob") %>%
  bind_cols(attr_test)

attr_rpart_probs %>%
  gain_curve(Attrition, .pred_Yes) %>%
  autoplot()

attr_ranger_probs %>%
  gain_curve(Attrition, .pred_Yes) %>%
  autoplot()


attr_ranger_probs %>%
  roc_curve(Attrition, .pred_Yes) %>%
  autoplot()

attr_rpart_probs %>%
  roc_curve(Attrition, .pred_Yes) %>%
  autoplot()

attr_ranger %>%
  predict(attr_test) %>%
  bind_cols(attr_ranger_probs) %>%
  metrics(truth = Attrition, .pred_Yes, estimate = .pred_class)

attr_rpart %>%
  predict(attr_test) %>%
  bind_cols(attr_rpart_probs) %>%
  metrics(truth = Attrition, .pred_Yes, estimate = .pred_class)

attr_ranger %>%
  predict(attr_test) %>%
  bind_cols(attr_test) %>%
  conf_mat(Attrition, .pred_class)

