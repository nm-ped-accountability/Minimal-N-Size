# Minimal N Size


# Load Data ---------------------------------------------------------------
rm(list = ls())
library(tidyverse)

# ELP Indicator -----------------------------------------------------------
ELP1718 <- read.csv("SY1718 ELP Student Level.csv", 
                    header = TRUE, stringsAsFactors = FALSE)

# ELP1718 <- ELP1718[!is.na(ELP1718$met), ]
# ELP1718$met2[ELP1718$met == TRUE] <- 1
# ELP1718$met2[ELP1718$met == FALSE] <- 0
# str(ELP1718$met2)
# ELP1718$schnumb <- as.factor(ELP1718$schnumb)
# 
# 
# ICC <- ICCbare(x = schnumb, y = met, data = ELP1718)
# head(ICC)
# 
# ICC <- ICCbareF(x = schnumb, y = met, data = ELP1718)
# head(ICC)
# 
# Nest(est.type = "pilot", ICC = 0.1, w = 0.2,
#      x = schnumb, y = met, data = ELP1718, alpha = 0.05)
# 
# fit <- lmer(diff ~ schnumb + (1 | schnumb), data = ELP1718)
# summary(fit)
# 
# fit2 <- glmer(met2 ~ schnumb + (1 | schnumb), data = ELP1718, family = binomial)
# summary(fit2)
# 
# table(ELP1718$met2)

counts <- ELP1718 %>%
    group_by(schnumb) %>%
    summarize(n_students = n()) %>%
    mutate(n_schools = 1)

head(counts)

percent <- function(x) {
    dat <- counts %>%
    filter(n_students >= x) %>%
    group_by(n_schools) %>%
    summarise(n_included_students = sum(n_students),
              n_included_schools = sum(n_schools),
              percent_students = n_included_students / sum(counts$n_students),
              percent_schools = n_included_schools / sum(counts$n_schools))
    dat
    }

n1 <- percent(1)
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

n_size <- rbind(n1, n2, n3, n4, n5, n6, n7, n8, n9, n10,
                n11, n12, n13, n14, n15, n16, n17, n18, n19, n20)

write.csv(n_size, "ELP n_size.csv", row.names = FALSE, na = "")


# Graduation --------------------------------------------------------------
rm(list = ls())
grad <- read.csv("4_Year_grad_2018.csv",
                 header = TRUE, stringsAsFactors = FALSE)

counts <- grad %>%
    group_by(schnumb) %>%
    summarize(n_students = n()) %>%
    mutate(n_schools = 1) %>%
    arrange(n_students)

head(counts)

n1 <- percent(1)
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

n_size <- rbind(n1, n2, n3, n4, n5, n6, n7, n8, n9, n10,
                n11, n12, n13, n14, n15, n16, n17, n18, n19, n20)
head(n_size)

write.csv(n_size, "Grad4 n_size.csv", row.names = FALSE, na = "")

# Proficiencies -----------------------------------------------------------
# read
rm(list = ls())
avt <- read.csv("All Valid Tests 2018V5.csv",
                 header = TRUE, stringsAsFactors = FALSE)

read <- avt[avt$Subtest == "READ", ]


counts <- read %>%
    group_by(schnumb) %>%
    summarize(n_students = n()) %>%
    mutate(n_schools = 1) %>%
    arrange(n_students)

head(counts)

n1 <- percent(1)
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

n_size <- rbind(n1, n2, n3, n4, n5, n6, n7, n8, n9, n10,
                n11, n12, n13, n14, n15, n16, n17, n18, n19, n20)
head(n_size)

write.csv(n_size, "Proficiency Read n_size.csv", row.names = FALSE, na = "")


# math
math <- avt[avt$Subtest == "MATH", ]

counts <- math %>%
    group_by(schnumb) %>%
    summarize(n_students = n()) %>%
    mutate(n_schools = 1) %>%
    arrange(n_students)

head(counts)

n1 <- percent(1)
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

n_size <- rbind(n1, n2, n3, n4, n5, n6, n7, n8, n9, n10,
                n11, n12, n13, n14, n15, n16, n17, n18, n19, n20)
head(n_size)

write.csv(n_size, "Proficiency Math n_size.csv", row.names = FALSE, na = "")


# science
science <- avt[avt$Subtest == "SCI", ]

counts <- science %>%
    group_by(schnumb) %>%
    summarize(n_students = n()) %>%
    mutate(n_schools = 1) %>%
    arrange(n_students)

head(counts)

n1 <- percent(1)
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

n_size <- rbind(n1, n2, n3, n4, n5, n6, n7, n8, n9, n10,
                n11, n12, n13, n14, n15, n16, n17, n18, n19, n20)
head(n_size)

write.csv(n_size, "Proficiency Science n_size.csv", row.names = FALSE, na = "")


# SGP ---------------------------------------------------------------------

sgp <- read.csv("PARCC SGP.csv", header = TRUE, stringsAsFactors = FALSE)

counts <- sgp %>%
    group_by(SchNum) %>%
    summarize(n_students = n()) %>%
    mutate(n_schools = 1) %>%
    arrange(n_students)

head(counts)

n1 <- percent(1)
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

n_size <- rbind(n1, n2, n3, n4, n5, n6, n7, n8, n9, n10,
                n11, n12, n13, n14, n15, n16, n17, n18, n19, n20)
head(n_size)

write.csv(n_size, "SGP n_size.csv", row.names = FALSE, na = "")
