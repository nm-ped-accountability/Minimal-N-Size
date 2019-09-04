# Minimal N Size


# Load Data ---------------------------------------------------------------
library(ICC)
library(tidyverse)
library(lme4)

# ELP Indicator -----------------------------------------------------------
ELP1718 <- read.csv("SY1718 ELP Student Level.csv", 
                    header = TRUE, stringsAsFactors = FALSE)

ELP1718 <- ELP1718[!is.na(ELP1718$met), ]
ELP1718$met2[ELP1718$met == TRUE] <- 1
ELP1718$met2[ELP1718$met == FALSE] <- 0
str(ELP1718$met2)
ELP1718$schnumb <- as.factor(ELP1718$schnumb)


ICC <- ICCbare(x = schnumb, y = met, data = ELP1718)
head(ICC)

ICC <- ICCbareF(x = schnumb, y = met, data = ELP1718)
head(ICC)

Nest(est.type = "pilot", ICC = 0.1, w = 0.2,
     x = schnumb, y = met, data = ELP1718, alpha = 0.05)

fit <- lmer(diff ~ schnumb + (1 | schnumb), data = ELP1718)
summary(fit)

fit2 <- glmer(met2 ~ schnumb + (1 | schnumb), data = ELP1718, family = binomial)
summary(fit2)

table(ELP1718$met2)

counts <- ELP1718 %>%
    group_by(schnumb) %>%
    summarize(n = n())

counts$record <- 1

head(counts)
sum(counts$n)
sum(counts$record)

percent <- function(x) {
    dat <- counts %>%
    filter(n >= x) %>%
    group_by(record) %>%
    summarise(sum_students = sum(n),
              sum_schools = sum(record),
              percent_students = sum_students / 35637,
              percent_schools = sum_schools / 750)
    dat
    }

n2 <- percent(2)
n3 <- percent(3)
n4 <- percent(4)
n5 <- percent(5)
n6 <- percent(6)
n7 <- percent(7)
n8 <- percent(8)
n9 <- percent(9)
n10 <- percent(10)
n11 <- percent(11)
n12 <- percent(12)
n13 <- percent(13)
n14 <- percent(14)
n15 <- percent(15)
n16 <- percent(16)
n17 <- percent(17)
n18 <- percent(18)
n19 <- percent(19)
n20 <- percent(20)

n_size <- rbind(n2, n3, n4, n5, n6, n7, n8, n9, n10,
                n11, n12, n13, n14, n15, n16, n17, n18, n19, n20)

write.csv(n_size, "ELP n_size.csv", row.names = FALSE, na = "")
