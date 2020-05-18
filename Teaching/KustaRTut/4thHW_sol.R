libs <- c('data.table','dplyr','lubridate','Hmisc')
sapply(libs, require, character.only = T)

nyc <- fread('Dropbox/KustaRTut/data/NYC_Jobs.csv')
nyc_dic <- fread('Dropbox/KustaRTut/data/NYC_Dict.csv')

# 2. ----------------------------------------------------------------------

colnames(nyc) <- gsub(' ', '_', colnames(nyc))
colnames(nyc) <- gsub('/', '_or_', colnames(nyc))
colnames(nyc) <- gsub('#', 'No.', colnames(nyc))

nyc <- select(nyc, -(Job_Description:To_Apply), -Residency_Requirement)

# 3. ----------------------------------------------------------------------

# psych::describe(select(nyc, -Job_ID), na.rm = T, omit = T) %>% View('desc_num')
unqs <- sapply(nyc, FUN = function(x) length(unique(x)))
nyc[,names(which(unqs < 2)) := NULL, with = T] # drop variables with only one level

# 4. ----------------------------------------------------------------------

# nyc_dic %>% View('Dict.') ; nyc %>% head(10) %>% View('NYC')
ggplot(nyc, aes(x = Salary_Range_From, y = Salary_Range_To)) +
  geom_jitter(aes(size = factor(Salary_Frequency, 
                                levels = c('Hourly', 'Daily', 'Annual'), ordered = T), 
                 col = `Full-Time_or_Part-Time_indicator`)) +
  scale_color_discrete(name = c("Full or Part")) +
  scale_size_discrete(name = c('Salary Freq.'))

# 5. ----------------------------------------------------------------------

nyc2 <- copy(nyc)
nyc2 <- nyc2[Salary_Frequency == 'Annual', 
             Salary_Range_From := Salary_Range_From/hour(hours(years(1)))]
nyc2 <- nyc2[Salary_Frequency == 'Daily', 
             Salary_Range_From := Salary_Range_From/hour(hours(days(1)))]
nyc2 <- nyc2[Salary_Frequency == 'Annual', 
             Salary_Range_To := Salary_Range_To/hour(hours(years(1)))]
nyc2 <- nyc2[Salary_Frequency == 'Daily', 
             Salary_Range_To := Salary_Range_To/hour(hours(days(1)))]
ggplot(nyc2, aes(x = Salary_Range_From, y = Salary_Range_To)) +
  geom_jitter(aes(col = `Full-Time_or_Part-Time_indicator`,size = No._Of_Positions)) +
  scale_color_discrete(name="Full or Part")

# 8. ----------------------------------------------------------------------

library(reshape2)
nyc2[,Posting_Date := parse_date_time(Posting_Date, 'ymdHMS')]
nyc2[,Salary_Mean := (Salary_Range_From + Salary_Range_To)/2]
dt <- melt(nyc2[,.(Posting_Date, Salary_Mean = Salary_Mean, No._Of_Positions)], 
           id="Posting_Date")
ggplot(dt) + geom_smooth(aes(x = Posting_Date, y=value, col=variable)) +
  geom_point(aes(x = Posting_Date, y=value, col=variable)) +
  coord_cartesian(ylim=c(-10, 30))