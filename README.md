---
title: "R Lecture Note"
author: "Yongjae Lee"
encoding: UTF-8
format:
  html:
    self-contained: true
    theme: cosmo
    # HTML 파일 생성 시 인코딩 관련 특별한 설정이 있다면 포함
    pdf:
    documentclass: article
    latex_engine: xelatex
knitr:
  opts_chunk: 
    collapse: true
    comment: "#>" 
    R.options:
      knitr.graphics.auto_pdf: true
---

::: {.callout-note title="연구주제"}
# 가설

-   스트레스가 구강건강에 미치는 영향

-   우울감이 구강건강에 미치는 영향
:::

# 데이터 불러오기

-   국민건강영양조사([KNHNES](https://knhanes.kdca.go.kr/)) 데이터를 다운받아서 R로 불러오기

::: {.callout-note title="Chatgpt 프롬프트 예시"}
SAS 파일을 R로 불러오고 싶은데 R코드 예제와 함께 알려줘.
:::

```{r}
if(!require(haven)) install.packages("haven", dependencies = TRUE)
library(haven)

hn22 <- read_sas("hn22_all.sas7bdat")
hn23 <- read_sas("hn23_all.sas7bdat")

hn <- list(hn23, hn22)
```

-   불러온 데이터 확인하기

```{r}
#| message: false
if(!require(tidyverse)) install.packages("tidyverse", dependencies = TRUE)
library(tidyverse)

# view(hn22)
# view(hn23)
```

# 데이터 전처리

-   데이터 전처리라 함은 데이터셋의 각 변수를 수정하여 연구주제에 맞게 데이터를 수정하는 과정임.
-   R에서는 주로 tidyverse 패키지의 dplyr 패키지 함수 6가지를 이용하여 데이터 전처리를 실시함. (tidyverse패키지는 데이터 전처리에 주로 사용되는 패키지의 모음집)

### filter(): 특정 조건에 맞는 행 선택

```{r}
hn23_filtered <- hn[[1]] %>% 
  filter(age > 19)

hn22_filtered <- hn[[2]] %>% 
  filter(age > 19)
```

### select(): 특정 변수(열)를 선택

```{r}
hn23_1 <- hn23_filtered %>% 
  select(ID, year, region, psu, sex, age, ainc_1, ho_incm, edu, wt_itvex, kstrata, 
         BD1_11, # 음주 
         BS3_1, # 흡연
         O_TC, # 자연치아 수
         OR1, # 본인 인지 구강건강상태
         O_chew_d, # 저작 불편감
         BP1, # 평소 스트레스 인지 정도
         BP5 # 평소 2주이상 우울감 여부
         )

hn22_1 <- hn22_filtered %>% 
  select(ID, year, region, psu, sex, age, ainc_1, ho_incm, edu, wt_itvex, kstrata, 
         BD1_11, # 음주 
         BS3_1, # 흡연
         O_TC, # 자연치아 수
         OR1, # 본인 인지 구강건강상태
         O_chew_d, # 저작 불편감
         BP1, # 평소 스트레스 인지 정도
         BP5 # 평소 2주이상 우울감 여부
         )

# 데이터 결합
hn22_23 <- rbind(hn22_1, hn23_1)
```

### mutate(): 새로운 변수 추가 또는 기존 변수 수정

```{r}
 hn22_23_1 <- hn22_23 %>% 
  mutate(
    smk = case_when( 
      BS3_1 %in% 1:2 ~ "있음",
      BS3_1 %in% c(3,8) ~ "없음",
      TRUE ~ NA),
    dnk = case_when(
      BD1_11 %in% 1:3 ~ "없음",
      BD1_11 %in% 4:8 ~ "있음",
      TRUE ~ NA),
    age_gp = case_when(
      age %in% 19:29 ~ "20-29",
      age %in% 30:39 ~ "30-39",
      age %in% 40:49 ~ "40-49",
      age %in% 50:64 ~ "50-64",
      age >= 65 ~ "65-",
      TRUE ~ NA), # NA가 문자형일 때
    dep = case_when(
      BP5 == 1 ~ "1",
      BP5 %in% c(2,8) ~ "0",
      TRUE ~ NA),
    stress = case_when(
      BP1 %in% 1:2 ~ "1",
      BP1 %in% 3:4 ~ "0",
      TRUE ~ NA),
    wt = wt_itvex / 2 # 가중치
  )
```

### 4) rename(): 변수명을 변경(변수명은 영문으로)

```{r}
#| echo: true
#| message: false
#| warning: false
#| results: 'hide'
# rename(): 변수명 재정의

hn22_23_2 <- hn22_23_1 %>% 
                    rename(
                      id = ID,
                      nt = O_TC,
                      srh = OR1,
                      chew = O_chew_d) %>% 
                    mutate(across(c(sex, age_gp, region, ho_incm, edu, smk, 
                                    dnk, chew, srh, stress, dep), as.factor))
```

### 5) summarise(): 데이터 값을 요약해주는 함수

```{r}
#| echo: true
#| message: false
#| warning: false
#| results: 'hide'
# 변수가 범주형인 경우,
hn22_23_2 %>% 
  count(stress) %>% 
  mutate(percent = n / sum(n) * 100)

# 변수가 연속형인 경우,
hn22_23_2 %>% 
  summarise(
    mean = mean(ainc_1),
    median = median(ainc_1),
    sd = sd(ainc_1)
  )

# 종속변수에 따라 변수를 요약하고자 할 때,
hn22_23_2 %>% 
  group_by(dep) %>% 
  count(srh) %>% 
  mutate(percent = n / sum(n) * 100)
```

### 6) slice(): 특정 행을 추출

```{r}
#| echo: true
#| message: false
#| warning: false
#| results: 'hide'
hn22_23_2 %>%
  arrange(stress) %>% # 오름차순
  group_by(stress) %>% # 그룹별
  slice(1)
```

### 참고) 변수의 빈도 테이블을 좀 더 쉽게 보는 패키지 소개

```{r}
#| echo: true
#| message: false
#| warning: false
#| results: 'hide'
# 빈도 테이블 
if(!require(janitor)) install.packages("janitor", dependencies = TRUE)
library(janitor)
hn22_23_2 %>% tabyl(stress)
```

### 최종) 무응답 삭제

-   무응답을 단순히 삭제할 수 있는 근거가 있을까요? (여러분들이 생각해보세요.)

```{r}
#| echo: true
#| message: false
#| warning: false
#| results: 'hide'
# 무응답 삭제
hn22_23_final <- hn22_23_2 %>% 
  drop_na()

# 무응답 확인
colSums(is.na(hn22_23_final))
```

## 최종 데이터셋을 이용한 표 작성

```{r}
#| echo: true
#| message: false
#| warning: false
#| results: 'hide'
# Table Summary: 표 작성
if(!require(gtsummary)) install.packages("gtsummary", dependencies = TRUE)
library(gtsummary)

tbl_for_summary <- hn22_23_final %>% 
  select(year, sex, age_gp, ho_incm, edu, smk, dnk, nt, srh, 
         chew,  stress, dep)


tbl_summary(tbl_for_summary, 
            by=stress, 
            statistic = list(all_categorical() ~ "{n} ({p}%)", 
                             all_continuous() ~ "{mean} ± {sd}")) %>% 
  add_p(test = list(
      nt ~ "t.test",
      smk ~ "chisq.test"
    ))
```

### 주요 통계 검정 함수

| 함수 이름    | 설명                                       |
|--------------|--------------------------------------------|
| t.test       | 독립표본 t-검정 (연속형 변수 vs 이진 그룹) |
| aov          | 분산분석 (ANOVA)                           |
| wilcox.test  | 비모수 검정: Wilcoxon rank-sum test        |
| kruskal.test | 비모수 검정: Kruskal-Wallis test           |
| chisq.test   | 범주형 변수에 대한 카이제곱 검정           |
| fisher.test  | 범주형 변수에 대한 Fisher 정확 검정        |

### 참고1) p-value

-   p-value는 귀무가설이 맞다는 가정하에 얻는 결과값의 확률

-   예를 들어, 신약이 기존의 약보다 효과가 좋은지 알고 싶을 때, 다음과 같이 가설을 세울 수 있음.

    -   귀무가설: 신약과 기존 약의 효과는 같다.

    -   대립가설: 신약은 기존 약과 효과가 다르다.

-   실험 후, p-value값이 0.03이 나왔다면, 귀무가설이 참일 경우(신약과 기존약의 효과가 같다는 가정) 효과가 같을 확률이 3%라는 뜻임. (조심해야할 사항 귀무가설이 참일 확률을 의미하는 것이 아님)

### 참고2) 주요 통계를 실행하기 위한 가정

#### 1. **독립표본 t-검정 (Independent two-sample t-test)**

-   분석 목적: 연속형 변수와 2개 그룹 간 평균 차이 검정

-   가정

    -   두 그룹은 독립적이다.

    -   각 그룹은 정규분포를 따른다.

    -   두 그룹의 분산은 동일하다 (등분산성).

```{r}
#| echo: true
#| message: false
#| warning: false
#| results: 'hide'
### 가정 확인###

# 1) 정규성 검정
# 정규성 검정: Shapiro-Wilk test # 샘플 수가 5000개 이하만 가능
set.seed(2015)  # set.seed는 랜덤하게 sampling할 경우 재현가능성을 위해 쓰임.
nt_sample_0 <- sample(hn22_23_final$nt[hn22_23_final$stress == "0"], 5000)
shapiro.test(nt_sample_0)

shapiro.test(hn22_23_final$nt[hn22_23_final$stress == "1"])

# 2) 등분산성 검정: 기본적으로 var.test()

var.test(nt ~ stress, data=hn22_23_final)
```

```{r}
#| echo: true
#| message: false
#| warning: false
#| results: 'hide'
# 독립표본 t-검정

# 등분산 가정할 경우 - 독립표본 t-검정 실시
t.test(nt ~ stress, data = hn22_23_final, var.equal = TRUE)

# 이분산 가정할 경우 - Welch's t-test
t.test(nt ~ stress, data = hn22_23_final, var.equal = FALSE)
```

#### 2. 비모수 검정: Wilcoxon rank-sum test

```{r}
#| echo: true
#| message: false
#| warning: false
#| results: 'hide'
#Wilcoxon rank-sum test 실행
wilcox_result <- wilcox.test(nt ~ stress, data = hn22_23_final)
```

#### 3. 분산분석(ANOVA)

-   분석 목적: 연속형 변수와 3개 그룹 간 평균 차이 검정

-   가정

    -   독립성: 각 집단의 샘플이 독립적이어야 함.
    -   정규성: 각 그룹의 오차가 정규분포를 따라야함.
    -   분산 동질성: 모든 그룹의 분산이 동일해야 함.
        -   귀무가설: 모든 그룹의 분산은 같다.
        -   대립가설: 모든 그룹의 분산은 다르다.

```{r}
#| echo: true
#| message: false
#| warning: false
#| results: 'hide'
if(!require(car)) install.packages("car", dependencies = TRUE)
library(car)
set.seed(123)

# ANOVA 실행 전, 잔차 추출
sampled_data <- hn22_23_final[sample(nrow(hn22_23_final), 5000), ]
anova_model <- aov(nt ~ ho_incm, data = sampled_data)
residuals_anova <- residuals(anova_model)

# 정규성 검정: Shapiro-Wilk test on residuals
shapiro_anova <- shapiro.test(residuals_anova)

# 분산 동질성 검정: Levene's test (car 패키지 필요)
levene_result <- leveneTest(nt ~ ho_incm, data = hn22_23_final)
```

```{r}
# ANOVA 실행
anova_result <- summary(anova_model)
```

#### 4. 비모수 검정: Kruskal-Wallis test

```{r}
#| echo: true
#| message: false
#| warning: false
#| results: 'hide'
# Kruskal-Wallis test 실행
kruskal_result <- kruskal.test(nt ~ ho_incm, data = hn22_23_final)
```

#### 5. 카이제곱 검정

-   가정
    -   독립성
    -   빈도수 조건: 기대 빈도가 5이하이면 안됨.

```{r}
#| echo: true
#| message: false
#| warning: false
#| results: 'hide'
# 교차분할표 생성
table_cat <- table(hn22_23_final$stress, hn22_23_final$chew)

# 카이제곱 검정 실행
chisq.test(table_cat)

# 기대빈도 확인: 기대빈도란 귀무가설이 참일 경우, 각 셀에 기대되는 이론적인 빈도
chisq.test(table_cat)$chisq_result$expected
```

#### 5. 기대 빈도 조건을 만족하지 못할 경우: fisher 정확 검정

```{r}
#| echo: true
#| message: false
#| warning: false
#| results: 'hide'
fisher.test(table_cat)
```

# 로지스틱 회귀분석 적용하기

-   종속변수가 범주형일 경우에 사용이 가능함.

-   가정:

    -   다중공선성 여부 확인(분산팽창인자가 10보다 작음)

## 1) 다중공선성 여부 확인

```{r}
#| echo: true
#| message: false
#| warning: false
#| results: 'hide'
library(car)

# 로지스틱 회귀 모델
model <- glm(as.numeric(stress) ~ nt + chew + srh + sex +
                age_gp + ho_incm + edu + smk + dnk, data = hn22_23_final, na.action = na.omit)

# VIF 확인: Variance Inflation Factor
vif(model)
```

-   VIF가 5초과이면 경고, 10 이상이면 심각한 다중공선성

-   다중공선성은 두 독립변수 사이에 강한 상관관계가 있기 때문에 어떤 변수가 영향을 주는지 알기 어려움.

## 2) 복합표본 설계된 데이터셋을 이용한 로지스틱 회귀분석

```{r}
#| echo: true
#| message: false
#| warning: false
#| results: 'hide'
# 복합표본 설계 객체 만들기
if(!require(survey)) install.packages("survey", dependencies = TRUE)
if(!require(gt)) install.packages("gt", dependencies = TRUE)

library(survey)
library(gt)

dstrat <- svydesign(id = ~id, strata = ~kstrata, weights = ~wt, data = hn22_23_final)

# 로지스틱 회귀모형 적합
fit <- svyglm(stress ~ nt + chew + srh + sex +
              age_gp + ho_incm + edu + smk + dnk,
              design = dstrat, family = quasibinomial())
summary(fit)

fit %>%
   tbl_regression(exponentiate = TRUE) %>%
   modify_header(label = "**변수**") %>%
   modify_caption("**Table 8: 구강건강이 스트레스에 미치는 영향**")

# 로지스틱 회귀모형 적합
fit2 <- svyglm(dep ~ nt + chew + srh + sex +
               age_gp + ho_incm + edu + smk + dnk,
               design = dstrat, family = quasibinomial())
summary(fit2)

fit2 %>%
   tbl_regression(exponentiate = TRUE) %>%
   modify_header(label = "**변수**") %>%
   modify_caption("**Table 9: 구강건강이 우울감에 미치는 영향**")
```
