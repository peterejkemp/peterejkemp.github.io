# code to recreate McKinsey study, graphs only
# download PISA_2015 subset from here:
# https://drive.google.com/file/d/18NpQO1DGY5dnjnlR2b3IF9Zv8xtpc1Fk/view

# This subset has been cleaned and turned into an R dataframe, the same legal blurb applies:
# "All PISA products are published under the 
# [Creative Commons Attribution-NonCommercial-ShareAlike 3.0 IGO](https://creativecommons.org/licenses/by-nc-sa/3.0/igo/) (CC BY-NC-SA 3.0 IGO)
# Copyright OECD"

# Load PISA 2015
PISA_2015 <- read_parquet(glue("{datafolder}/PISA_2015_student_subset.parquet"))

# filter for UK only
PISA_2015_UK <- PISA_2015 %>% filter(CNT == "United Kingdom")

# cut into three groups based on Inquiry based learning and Direct instruction aggregates
# for PISA 2015
quant_IBTEACH <- quantile(PISA_2015_UK$IBTEACH, prob=c(.33,.66), na.rm = TRUE)
quant_TDTEACH <- quantile(PISA_2015_UK$TDTEACH, prob=c(.33,.66), na.rm = TRUE)

# Mutate the IBTEACH and TDTEACH columns, replacing values with 'High', 'Low' and 'Medium' based on the quantile calculation values

PISA_2015IBTD <- PISA_2015_UK %>% 
  select(PV1SCIE, IBTEACH, TDTEACH, ST004D01T, OECD, CNT) %>%
  mutate(IBTEACH = ifelse(IBTEACH < quant_IBTEACH["33%"], "Low",
                       ifelse(IBTEACH >= quant_IBTEACH["33%"] & 
                                IBTEACH < quant_IBTEACH["66%"], 
                              "Medium", "High"))) %>%
   mutate(TDTEACH = ifelse(TDTEACH < quant_TDTEACH["33%"], "Low",
                       ifelse(TDTEACH >=quant_TDTEACH["33%"] & 
                                TDTEACH < quant_TDTEACH["66%"],
                              "Medium", "High"))) %>%
  filter(!is.na(IBTEACH), !is.na(TDTEACH)) %>%
  rename(gender = ST004D01T)

# Plot density graphs of variation in science score by level of inquiry based teaching
ggplot(PISA_2015IBTD,
       aes(x=PV1SCIE, fill=IBTEACH))+
  geom_density(alpha=0.5)

# same for direct instruction
ggplot(PISA_2015IBTD,
       aes(x=PV1SCIE, fill=TDTEACH))+
  geom_density(alpha=0.5)


plot_data <- PISA_2015IBTD %>% 
  mutate(mean_sci  = mean(PV1SCIE),
         median_sci= median(PV1SCIE)) %>%
  group_by(IBTEACH, TDTEACH, mean_sci, median_sci) %>%
  summarise(group_mean_sci=mean(PV1SCIE),
            group_median_sci=median(PV1SCIE),
            mean_diff = unique(group_mean_sci - mean_sci),
            median_diff = unique(group_median_sci - median_sci),
            mean_col = mean_diff > 0,
            median_col = median_diff > 0,
            n=n()) %>% 
  ungroup() %>%
  mutate(IBTEACH = factor(IBTEACH, levels=c("Low", "Medium", "High")),
         TDTEACH = factor(TDTEACH, levels=c("Low", "Medium", "High"))) %>% 
         select(IBTEACH, TDTEACH, mean_diff, mean_col, n) %>%
  mutate(mean_diff = signif(mean_diff,2),
                mean_diff_txt = ifelse(sign(mean_diff)==1, 
                                   paste0("+",mean_diff),
                                   paste0(mean_diff)),
                per = signif(100 * (n / sum(n)),3))

plt_sweetspot <- ggplot(plot_data, 
       aes(x =IBTEACH, y = TDTEACH)) +
  geom_point(aes(size=abs(mean_diff), colour=mean_col)) +
  scale_size(range = c(0, 40)) +
  geom_label(aes(label=paste(per, "%"), vjust=3.5)) +
  geom_text(aes(label=mean_diff_txt, 
                colour=ifelse(abs(mean_diff) > 8, "big",
                              ifelse(mean_diff > 0, "positive", "negative")))) +
  scale_color_manual(values = c("big" = "white", "positive" = "#6592a5", 
                                "negative" = "#c37d7f",
                                "TRUE" = "#6592a5", "FALSE"= "#c37d7f")) +
  theme(panel.background=element_rect(fill = "#dce1e5"),
        legend.position = "none") +
  ggtitle("Point change in PISA science scores relative to mean (UK only)") + 
  xlab("Inquiry-based science teaching") +
  ylab("Teacher-directed science instruction")
