# standardise the results for each student in line with pg7
# https://eprints.leedsbeckett.ac.uk/id/eprint/4753/6/symplectic-version.pdf

# code to recreate parts of Stoet and Geary's 2018 Gender Equality Paradox study
# original paper: The Gender-Equality Paradox in Science, Technology, Engineering, and Mathematics Education
# link: https://journals.sagepub.com/doi/10.1177/0956797617741719

# download PISA_2015 subset from here:
# https://drive.google.com/file/d/18NpQO1DGY5dnjnlR2b3IF9Zv8xtpc1Fk/view

# This subset has been cleaned and turned into an R dataframe, the same legal blurb applies:
# "All PISA products are published under the 
# [Creative Commons Attribution-NonCommercial-ShareAlike 3.0 IGO](https://creativecommons.org/licenses/by-nc-sa/3.0/igo/) (CC BY-NC-SA 3.0 IGO)
# Copyright OECD"

# accessing GGGI dataset
#First we are going to download the GGGI, unfortunately, it's difficult to find the 2015 dataset, so we'll use 2013 instead, 
#which can be found 
#[here](https://data.humdata.org/dataset/29f2f52f-a9c2-4ff9-a99e-42b894dc18e9/resource/a8fe8c72-1359-4ce3-b8cc-03d9e5e85875/download/table-3b-detailed-rankings-2013.csv)

library(arrow)
library(tidyverse)
library(glue)

PISA_2015 <- read_parquet("location/file.parquet")
GGGI <- read.csv("<folder>table-3b-detailed-rankings-2013.csv")

# join datasets together
PISA_2015_GGGI <- left_join(
  PISA_2015 %>% select(CNT, ST004D01T, PV1MATH, PV1SCIE, PV1READ, SCIEEFF),
  GGGI %>% select(Country, Overall.Score),
  by=c("CNT"="Country"))

# step 1
# 1.  We standardized the mathematics, science, and reading scores on a nation-by-nation basis. 
# We call these new standardized scores *zMath*, *zRead*, and *zScience*.
PISA_2015z <- PISA_2015_GGGI %>% 
  rename(gender = ST004D01T) %>%
  group_by(CNT) %>%
  mutate(zMaths   = scale(PV1MATH),
         zScience = scale(PV1SCIE), 
         zReading = scale(PV1READ))

# step 2
# 2.  We calculated for each student the standardized average score of the new z-scores and we call this *zGeneral*.
PISA_2015z <- PISA_2015z %>% 
  mutate(zGeneral = (zMaths + zScience + zReading) / 3)

# step 3
# 3.  Then, we calculated for each student their intra-individual strengths by subtracting *zGeneral* as follows: 
# *relativeSciencestrength = zScience - zGeneral*, 
# *relativeMathstrength = zMath - zGeneral*, 
# *relativeReadingstrength = zReading - zGeneral*.
PISA_2015z <- PISA_2015z %>% 
  mutate(rel_MATH = zMaths   - zGeneral,
         rel_SCIE = zScience - zGeneral,
         rel_READ = zReading - zGeneral)

# step 4
# 4.  Finally, using these new intra-individual (relative) scores, 
# we calculated for each country the averages for boys and girls
# step 4 part 1
PISA_2015z <- PISA_2015z %>% 
  group_by(CNT, gender) %>%
  summarise(zMaths = mean(zMaths, na.rm=TRUE),
            zScience = mean(zScience, na.rm=TRUE),
            zReading = mean(zReading, na.rm=TRUE),
            zGeneral = mean(zGeneral, na.rm=TRUE),
            rel_MATH = zMaths - zGeneral,
            rel_SCIE = zScience - zGeneral,
            rel_READ = zReading - zGeneral,
            gggi = unique(Overall.Score))

# step 4 part 2
pisa_gggi_diff <- PISA_2015z %>%
  select(CNT, gender, gggi, rel_SCIE) %>%
  pivot_wider(names_from = gender,
              values_from = rel_SCIE) %>%
  mutate(difference =  Male - Female)
  
###########################################################
## Plots and stats

### Plot the results for Figure 4

ggplot(pisa_gggi_diff,
       aes(x=difference, y=gggi)) + 
  geom_point(colour="red") +
  geom_smooth(method="lm") +
  # coord_flip() +
  geom_text_repel(aes(label=CNT),
            box.padding = 0.2,
            max.overlaps = Inf,
            colour="black") +
  xlab(paste0("relative difference in PV1SCIE scores (male-female)"))
  
### Stats for Figure 4
cor.test(pisa_gggi_diff$gggi, 
                    pisa_gggi_diff$difference, 
                    method="spearman")

# by pearson better as data n > 30 a shapiro test p>0.05
shapiro.test(pisa_gggi_diff$gggi) 
#> p-value = 0.1226
shapiro.test(pisa_gggi_diff$difference)
#> p-value = 0.4078

# Better model, closer to the original findings
cor.test(pisa_gggi_diff$gggi, 
                    pisa_gggi_diff$difference, 
                    method="pearson")
