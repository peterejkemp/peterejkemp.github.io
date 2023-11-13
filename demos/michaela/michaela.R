library(gt)
library(gtExtras)
library(ggrepel)

type = "ks4"
data_folder <- r"[C:\Users\Peter\Google Drive\Kings\Code\PISR\Data\]"
data_folder <- r"[D:\Data\EdR\]"


node <- Sys.info()

if(node["nodename"] == "KCL8KWNXY2"){
  # basefolder <- r"(C:\Users\w1926273\OneDrive - King's College London\Code\SCARI\)"
  basefolder <- r"(C:/Users/w1926273/Google Drive/Kings/Code/PISR/)"
}else if(node["nodename"] == "PETES_PC"){
  basefolder <- r"(C:/Users/Peter/Google Drive/Kings/Code/PISR/)"
  old_proj_folder <- r"(C:/Users/Peter/Google Drive/Kings/Code/OpenAnalysis/)"
}else if(node["nodename"] == "KCL3Y7NKR3"){ # new laptop
  basefolder <- r"(G:\My Drive\Kings\Code\PISR\)"
  old_proj_folder <- r"(C:/Users/Peter/Google Drive/Kings/Code/OpenAnalysis/)"
}

source(paste0(basefolder, "_reports/TRACER/TRACER-setup.R"))
source(paste0(codefolder, 'OpenAnalysis_load.R'))

#TODO: we must work out why the schools dataset has the wrong FSM values in it, e.g.:
schools %>%
  filter(URN == 140862) %>% 
  select(year, URN, FSM_ever_per, FSM_per ) %>%
  distinct()

# run grammar.qmd to pull main datasets

#arrow::write_parquet(schools, glue("{data_folder}DfE/{type}/{type}.parquet"))
#arrow::write_parquet(results_all, glue("{data_folder}DfE/{type}/{type}.parquet"))
ks4provider <- DfE_load_ks4provider()

### Does Michaela do better by poorer kids:


ks4provider %>%
  filter(URN == 140862) %>% 
  select(year, URN, DIFFN_P8MEA) %>%
  distinct()

ks4provider %>%
  #filter(URN == 140862) %>% 
  select(year,SCHNAME,  URN, DIFFN_P8MEA, ATT8SCR_LO) %>%
  distinct() %>%
  filter(year %in% c(2019,2022)) %>%
  arrange(desc(ATT8SCR_LO))

ks4provider %>%
  filter(URN == 140862) %>% 
  select(year, URN, PTFSM6CLA1A) %>%
  distinct()

# work out number of quals per school
subjects_entered <- results_all %>% 
  left_join(schools %>% 
              mutate(year = as.numeric(gsub(".*[-]", "",year))), 
            by=c("year"="year", "urn"="URN")) %>%
  # filter(urn == 140862) %>% 
  select(year, urn, `School name`, Qualification, `Subject description`, 
         `Total number entered`, `Number of students at the end of year`, 
         schType, Ofsted, OfstedDate) %>%
  mutate(per_entered = `Total number entered`/ `Number of students at the end of year`) %>%
  left_join(ks4provider, by=c("year"="year", "urn"="URN"))

#################################
## similar P8 scores
#################################
subjects_entered %>% 
  group_by(year, urn) %>%
  summarise(`Number of students at the end of year` = mean(`Number of students at the end of year`),
    PTFSM6CLA1A = mean(PTFSM6CLA1A, na.rm=TRUE)) %>%
  group_by(year) %>%
  mutate(total_stu = sum(`Number of students at the end of year`),
         FSM = ((`Number of students at the end of year` * PTFSM6CLA1A))/unique(total_stu)) %>%
  distinct(year, FSM, total_stu)
  
ks4provider %>%
  filter(URN == 140862) %>% 
  select(year, URN, PTFSM6CLA1A) %>%
  distinct()

schools %>%
  filter(URN == 140862) %>% 
  select(year, URN, FSM_ever_per) %>%
  distinct()


unique(subjects_entered$P8MEA)

# michaela = P8, 2.27
# michaela = FSM, 2.27

plot_data <- subjects_entered %>% 
  filter(!is.na(P8MEA),
         per_entered >= 0.1,
         grepl("GCSE", Qualification),
         !grepl("(S|s)pecial", schType),
         `Number of students at the end of year` >= 100,
         `Number of students at the end of year` <= 150,
         ) %>% 
  group_by(year, urn, `School name`, ATT8SCR, P8PUP, P8MEA, P8MEA_ORIG) %>%
  summarise(subs = n()) %>%
  mutate(Mich = urn==140862)

ggplot(data = plot_data, aes(x=subs, y=P8MEA)) +
  geom_point(aes(colour=Mich)) +
  geom_smooth(method = lm) +
  facet_wrap(year ~ .)



#################################
## similar schools entering GCSEs
#################################

plot_data <- subjects_entered %>%
  filter(
         per_entered >= 0.1,
         grepl("GCSE", Qualification),
         !grepl("(S|s)pecial", schType),
         !grepl("dependent", schType),
         `Number of students at the end of year` >= 100,
         `Number of students at the end of year` <= 150) %>%
  group_by(year, `School name`, schType) %>%
  summarise(n = n(),
            sch_size = max(`Number of students at the end of year`)) %>%
  mutate(Mich = grepl("Michaela", `School name`)) %>%
  group_by(year) %>%
  mutate(med = median(n)) %>%
  group_by(year, n) %>%
  summarise(schools = n(),
            Mich = max(Mich),
            med = max(n == med)) %>%
  arrange(year, n) %>%
  group_by(year) %>%
  mutate(count = cumsum(schools),
         per = round(100*count / sum(schools), 1))

ggplot(data = plot_data, 
       aes(x=n, y=schools, fill=Mich)) + 
  geom_bar(stat="identity") + 
  geom_bar(data = plot_data %>% filter(med == 1),
           stat="identity", aes(x=n, y=schools), fill="darkgreen") + 
  geom_text(data = plot_data, 
            aes(x=n, y=schools, label=per),
            size=2.3, hjust=-0.3, vjust=0.1, angle = 90) +
  xlim(NA,25) +
  facet_wrap(year ~ .) +
  xlab(NULL) +
  guides(fill="none") + 
  labs(title = "Number of GCSEs offered by English non-special provision state schools",
       # subtitle = bquote(~bold("green")" median group; "~bold("blue")" Michaela group; "~bold("text labels")" cumulative percentage"),
       subtitle = expression(paste(bold("green")," median group; ", bold("blue"), " Michaela group; ", bold("text labels"), " cumulative percentage")),
       caption = "source: https://www.gov.uk/school-performance-tables")


yr <- 2019

subjects_entered %>%
  filter(
    per_entered >= 0.1,
    grepl("GCSE", Qualification),
    !grepl("(S|s)pecial", schType),
    !grepl("dependent", schType),
    `Number of students at the end of year` >= 100,
    `Number of students at the end of year` <= 150) %>%
  group_by(year, `School name`, schType) %>%
  summarise(quals = n(),
            sch_size = max(`Number of students at the end of year`)) %>%
  mutate(Mich = grepl("Michaela", `School name`)) %>%
  group_by(year) %>%
  mutate(med = median(quals),
         total_schools = n()) %>% 
  filter(year == yr) %>%
  arrange(quals) %>%
  ungroup() %>%
  mutate(row_number = row_number()) %>%
  gt() %>%
  gt_highlight_rows(rows=Mich==TRUE) %>%
  cols_hide(c("Mich", "schType")) %>%
  tab_header(
    title = md("Number of **GCSEs** offered in English non-special provision state schools"),
    subtitle = md(paste(yr," data; >10% cohort taking them; Schools >= 100 and <=150 students"))
  )


################################
### All quals
################################

plot_data <- subjects_entered %>%
  filter(
    per_entered >= 0.1,
    # grepl("GCSE", Qualification),
    !grepl("(S|s)pecial", schType),
    !grepl("dependent", schType),
    `Number of students at the end of year` >= 100,
    `Number of students at the end of year` <= 150) %>%
  group_by(year, `School name`, schType) %>%
  summarise(n = n(),
            sch_size = max(`Number of students at the end of year`)) %>%
  mutate(Mich = grepl("Michaela", `School name`)) %>%
  group_by(year) %>%
  mutate(med = median(n)) %>%
  group_by(year, n) %>%
  summarise(schools = n(),
            Mich = max(Mich),
            med = max(n == med)) %>%
  arrange(year, n) %>%
  group_by(year) %>%
  mutate(count = cumsum(schools),
         per = round(100*count / sum(schools), 1))

ggplot(data = plot_data, 
       aes(x=n, y=schools, fill=Mich)) + 
  geom_bar(stat="identity") + 
  geom_bar(data = plot_data %>% filter(med == 1),
           stat="identity", aes(x=n, y=schools), fill="darkgreen") + 
  geom_text(data = plot_data, 
            aes(x=n, y=schools, label=per),
            size=2.3, hjust=-0.3, vjust=0.1, angle = 90) +
  xlim(4,28) +
  facet_wrap(year ~ .) +
  xlab(NULL) +
  guides(fill="none") + 
  labs(title = "Number of All quals offered by English non-special provision state schools",
       # subtitle = bquote(~bold("green")" median group; "~bold("blue")" Michaela group; "~bold("text labels")" cumulative percentage"),
       subtitle = expression(paste(bold("green")," median group; ", bold("blue"), " Michaela group; ", bold("text labels"), " cumulative percentage")),
       caption = "source: https://www.gov.uk/school-performance-tables")

yr <- 2022

subjects_entered %>%
  filter(
    per_entered >= 0.1,
    # grepl("GCSE", Qualification),
    !grepl("(S|s)pecial", schType),
    !grepl("dependent", schType),
    `Number of students at the end of year` >= 100,
    `Number of students at the end of year` <= 150) %>%
  group_by(year, `School name`, schType) %>%
  summarise(quals = n(),
            sch_size = max(`Number of students at the end of year`)) %>%
  mutate(Mich = grepl("Michaela", `School name`)) %>%
  group_by(year) %>%
  mutate(med = median(quals),
         total_schools = n()) %>% 
  filter(year == yr) %>%
  arrange(quals) %>%
  ungroup() %>%
  mutate(row_number = row_number()) %>%
  gt() %>%
  gt_highlight_rows(rows=Mich==TRUE) %>%
  cols_hide(c("Mich", "schType")) %>%
  tab_header(
    title = md("Number of exam subjects offered in English non-special provision state schools"),
    subtitle = md(paste(yr," data; >10% cohort taking them; Schools >= 100 and <=150 students"))
  )

###############################
# outstanding schools
###############################


plot_data <- subjects_entered %>%
  filter(Ofsted == "Outstanding" | 
           urn == 140862) %>%
  filter(
    per_entered >= 0.1,
    # grepl("GCSE", Qualification),
    !grepl("(S|s)pecial", schType),
    !grepl("dependent", schType),
    `Number of students at the end of year` >= 100,
    `Number of students at the end of year` <= 150) %>%
  group_by(year, `School name`, schType) %>%
  summarise(n = n(),
            sch_size = max(`Number of students at the end of year`)) %>%
  mutate(Mich = grepl("Michaela", `School name`)) %>%
  group_by(year) %>%
  mutate(med = median(n)) %>%
  group_by(year, n) %>%
  summarise(schools = n(),
            Mich = max(Mich),
            med = max(n == med)) %>%
  arrange(year, n) %>%
  group_by(year) %>%
  mutate(count = cumsum(schools),
         per = round(100*count / sum(schools), 1))

ggplot(data = plot_data, 
       aes(x=n, y=schools, fill=Mich)) + 
  geom_bar(stat="identity") + 
  geom_bar(data = plot_data %>% filter(med == 1),
           stat="identity", aes(x=n, y=schools), fill="darkgreen") + 
  geom_text(data = plot_data, 
            aes(x=n, y=schools, label=per),
            size=2.3, hjust=-0.3, vjust=0.1, angle = 90) +
  xlim(4,28) +
  facet_wrap(year ~ .) +
  xlab(NULL) +
  guides(fill="none") + 
  labs(title = expression(paste("Number of ",bold("subjects")," offered by Outstanding English non-special provision state schools")),
       # subtitle = bquote(~bold("green")" median group; "~bold("blue")" Michaela group; "~bold("text labels")" cumulative percentage"),
       subtitle = expression(paste(bold("green")," median group; ", bold("blue"), " Michaela group; ", bold("text labels"), " cumulative percentage")),
       caption = "source: https://www.gov.uk/school-performance-tables")

subjects_entered %>%
  filter(Ofsted == "Outstanding" | 
           urn == 140862) %>%
  filter(
    per_entered >= 0.1,
    grepl("GCSE", Qualification),
    !grepl("(S|s)pecial", schType),
    !grepl("dependent", schType),
    `Number of students at the end of year` >= 100,
    `Number of students at the end of year` <= 150) %>%
  group_by(year, `School name`, schType) %>%
  summarise(quals = n(),
            sch_size = max(`Number of students at the end of year`)) %>%
  mutate(Mich = grepl("Michaela", `School name`)) %>%
  group_by(year) %>%
  mutate(med = median(quals),
         total_schools = n()) %>% 
  filter(year %in% c(2022)) %>%
  arrange(quals) %>%
  ungroup() %>%
  mutate(row_number = row_number()) %>%
  gt() %>%
  gt_highlight_rows(rows=Mich==TRUE) %>%
  cols_hide(Mich)

yr <- 2022

subjects_entered %>%
  filter(Ofsted == "Outstanding" | 
           urn == 140862) %>%
  filter(
    per_entered >= 0.1,
    # grepl("GCSE", Qualification),
    !grepl("(S|s)pecial", schType),
    !grepl("dependent", schType),
    `Number of students at the end of year` >= 100,
    `Number of students at the end of year` <= 150) %>%
  group_by(year, `School name`, schType) %>%
  summarise(quals = n(),
            sch_size = max(`Number of students at the end of year`)) %>%
  mutate(Mich = grepl("Michaela", `School name`)) %>%
  group_by(year) %>%
  mutate(med = median(quals),
         total_schools = n()) %>% 
  filter(year == yr) %>%
  arrange(quals) %>%
  ungroup() %>%
  mutate(row_number = row_number()) %>%
  gt() %>%
  gt_highlight_rows(rows=Mich==TRUE) %>%
  cols_hide(c("Mich", "schType")) %>%
  tab_header(
    title = md("Number of **exam subjects** offered in **Outstanding** English non-special provision state schools"),
    subtitle = md(paste(yr," data; >10% cohort taking them; Schools >= 100 and <=150 students"))
  )


subjects_entered %>%
  filter(Ofsted == "Outstanding" | 
           urn == 140862) %>%
  filter(
    per_entered >= 0.1,
    grepl("GCSE", Qualification),
    !grepl("(S|s)pecial", schType),
    !grepl("dependent", schType),
    `Number of students at the end of year` >= 100,
    `Number of students at the end of year` <= 150) %>%
  group_by(year, `School name`, schType) %>%
  summarise(quals = n(),
            sch_size = max(`Number of students at the end of year`)) %>%
  mutate(Mich = grepl("Michaela", `School name`)) %>%
  group_by(year) %>%
  mutate(med = median(quals),
         total_schools = n()) %>% 
  filter(year == yr) %>%
  arrange(quals) %>%
  ungroup() %>%
  mutate(row_number = row_number()) %>%
  gt() %>%
  gt_highlight_rows(rows=Mich==TRUE) %>%
  cols_hide(c("Mich", "schType")) %>%
  tab_header(
    title = md("Number of **GCSEs** offered in **Outstanding** English non-special provision state schools"),
    subtitle = md(paste(yr," data; >10% cohort taking them; Schools >= 100 and <=150 students"))
  )

