library(gt)
library(gtExtras)

# work out number of quals per school
subjects_entered <- results_all %>% 
  left_join(schools %>% 
              mutate(year = as.numeric(gsub(".*[-]", "",year))), 
            by=c("year"="year", "urn"="URN")) %>%
  # filter(urn == 140862) %>% 
  select(year, urn, `School name`, Qualification, `Subject description`, 
         `Total number entered`, `Number of students at the end of year`, 
         schType, Ofsted, OfstedDate) %>%
  mutate(per_entered = `Total number entered`/ `Number of students at the end of year`)


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

