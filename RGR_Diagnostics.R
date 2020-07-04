# 診断図用関数
# Greg Nishihara
# 2020 Jul 04

# 必要なパッケージ

library(tidyverse)
library(statmod)        

# 残差のヒストグラム
gg_resid_hist = function(fitted.model) {
  data = fortify(fitted.model) %>% as_tibble()
  if(class(fitted.model)[1] != "lm") {
    data = data %>% mutate(qresid = qresiduals(fitted.model))
    data = data %>% mutate(residual = qresid)
    xlabel = "Randomized Quantile Residuals"
  } else {
    data = data %>% mutate(residual = .resid)
    xlabel = "Standardized Residuals"
  }
  ggplot(data) +
    geom_histogram(aes(x = residual)) +
    labs(x = xlabel) + ggtitle("Histogram of Residuals")
}

# 残差のQQプロット
gg_qq = function(fitted.model) {
  data = fortify(fitted.model) %>% as_tibble()
  if(class(fitted.model)[1] != "lm") {
    data = data %>% mutate(qresid = qresiduals(fitted.model))
    data = data %>% mutate(residual = qresid)
    ylabel = "Randomized Quantile Residuals"
  } else {
    data = data %>% mutate(residual = .stdresid)
    ylabel = "Standardized Residuals"
  }
  ggplot(data) +
    geom_qq(aes(sample =residual)) +
    geom_abline(color = "red") +
    labs(x = "Theoretical Quantile", y = ylabel) +
    ggtitle("Normal-QQ Plot")
}

# 変数に対する残差のプロット
gg_resX = function(fitted.model, ncol=NULL, ...) {
  residlab = function(string) {
    sprintf("Residuals vs. %s", string)
  }
  data = fortify(fitted.model) %>% as_tibble()
  varnames = as.character(formula(fitted.model)) %>% pluck(3)
  varnames = str_split(varnames, " \\+ ") %>% pluck(1)
  if(class(fitted.model)[1] != "lm") {
    data = data %>% mutate(qresid = qresiduals(fitted.model))
    data = data %>% mutate(residual = qresid)
    ylabel = "Randomized Quantile Residuals"
  } else {
    data = data %>% mutate(residual = .resid)
    ylabel = "Standardized Residuals"
  }
  varnames = names(data)[names(data) %in% varnames]
  data = data %>% select(varnames, residual) %>% 
    gather(var, value, varnames)
  ggplot(data) + 
    geom_point(aes(x = value, y = residual)) +
    geom_hline(yintercept=0, color = "red", linetype = "dashed") +
    labs(x="Value", y = ylabel) +
    facet_wrap("var", labeller=labeller(var=residlab), scales = "free_x",
               ncol = ncol)
}

# 期待値に対する残差のプロット
gg_resfitted = function(fitted.model) {
  data = fortify(fitted.model) %>% as_tibble()
  if(class(fitted.model)[1] != "lm") {
    data = data %>% mutate(qresid = qresiduals(fitted.model))
    data = data %>% mutate(residual = qresid)
    ylabel = "Randomized Quantile Residuals"
  } else {
    data = data %>% mutate(residual = .resid)
    ylabel = "Standardized Residuals"
  }
  ggplot(data) + 
    geom_point(aes(x = .fitted, y = residual)) +
    geom_hline(yintercept=0, color = "red", linetype = "dashed") +
    labs(x="Fitted Values", y = ylabel) +
    ggtitle("Residuals vs. Fitted Values")
}
```

## 診断図関数：スケール・ロケーションプロット

\tiny
```{r}
# スケール・ロケーションプロット
gg_scalelocation = function(fitted.model) {
  data = fortify(fitted.model) %>% as_tibble()
  if(class(fitted.model)[1] != "lm") {
    data = data %>% mutate(qresid = qresiduals(fitted.model))
    data = data %>% mutate(residual = qresid)
    ylabel = expression(sqrt("|RQR|"))
  } else {
    data = data %>% mutate(residual = .resid)
    ylabel = expression(sqrt("|Standardized Residuals|"))
  }
  ggplot(data) + 
    geom_point(aes(x = .fitted, y = sqrt(abs(residual)))) +
    geom_smooth(aes(x = .fitted, y = sqrt(abs(residual))), 
                se = F, color = "red", linetype = "dashed") +
    labs(x="Fitted Values", y = ylabel) +
    ggtitle("Scale - Location Plot")
}

# クックの距離
gg_cooksd = function(fitted.model) {
  data = fortify(fitted.model) %>% as_tibble()
  data = data %>% mutate(n = seq_along(.cooksd))
  dof = summary(fitted.model) %>% pluck("df")
  thold = qf(0.5, dof[1], dof[2])
  data2 = data %>% 
    mutate(above = ifelse(.cooksd > thold, T, F)) %>% 
    filter(above)
  ggplot(data) + 
    geom_point(aes(x = n, y = .cooksd)) +
    geom_segment(aes(x = n, y = 0, xend = n, yend = .cooksd)) +
    geom_hline(yintercept=thold, color = "red", linetype = "dashed") +
    geom_text(aes(x = n, y = .cooksd, label = n), 
              data = data2, nudge_x=3, color = "red") +
    labs(x="Sample", y = "Cook's Distance") +
    ggtitle("Cook's Distance Plot")
}