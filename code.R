library(Rserve)
library(lubridate)
library(dplyr)
library(reshape2)
library(xlsx)
library(stats)
Rserve(args="--no-save")

# Setting the directory path
setwd("/Users/saksham/Downloads/helping/yourapplicationathelpling")
providers <- read.csv(text=gsub("(^\"|\"$)","",readLines("exercise_providers.csv")))
jobs <- read.csv(text=gsub("(^\"|\"$)","",readLines("exercise_jobs.csv"))) 
# Replacing missing values with NA
providers[providers=="\""]<-NA

# Summary of providers and jobs table
summary(providers)
summary(jobs)

# Correcting the Date format
providers$first_call_date <- parse_date_time(x = providers$first_call_date,
                                             orders = c("Ymd","Y-m-d H:M:OS"),tz="Europe/Berlin")
providers$sign_up_date <- parse_date_time(x = providers$sign_up_date,
                                          orders = c("Ymd","Y-m-d H:M:OS"),tz="Europe/Berlin")
providers$went_live_date <- parse_date_time(x = providers$went_live_date,
                                            orders = c("Ymd","Y-m-d H:M:OS"),tz="Europe/Berlin")
jobs$job_appointment_time <- parse_date_time(x = jobs$job_appointment_time,
                                             orders = c("Ymd","Y-m-d H:M:OS"),tz="Europe/Berlin")

# Replacing invalid dates with NA
providers$went_live_date[providers$went_live_date ==as.Date("0001-01-01")] <- NA
jobs$job_appointment_time[year(jobs$job_appointment_time) ==0001] <- NA

######## Descriptive analysis
### Total Providers (as per providers table)
length(providers$provider_id)
### Total jobs
length(unique(jobs$job_id))
### Total Providers did job
length(unique(jobs$provider_id))
### Top Providers with highest jobs
head(sort(table(jobs$provider_id),decreasing = TRUE))

########
### Adding jobs count to providers
# 100 providers in jobs were not found in providers table. Therefore, full join to merge the missing providers as well.
providers_detailed = providers %>%
  full_join(jobs %>%
              group_by(provider_id) %>%
              summarise(jobs_count=n())
              )

### Total Actual Providers (after merging providers table and jobs table)
length(providers_detailed$provider_id)

# Replacing NA job counts with 0, when went_live_date is not NA i.e., the cases when provider has never ran a job.
providers_detailed$jobs_count = as.numeric(providers_detailed$jobs_count)
providers_detailed$jobs_count = ifelse(is.na(providers_detailed$jobs_count) & !is.na(providers_detailed$went_live_date),
                                       0,providers_detailed$jobs_count)


### Time diff in days between Sign up date and First call date
providers_detailed$days_sign_call <- as.numeric(difftime(providers_detailed$first_call_date, providers_detailed$sign_up_date, units="days"))
providers_detailed$days_call_live <- as.numeric(difftime(providers_detailed$went_live_date, providers_detailed$first_call_date, units="days"))

### Time diff in days between live and job; total time span
first_job <- jobs[match(unique(jobs$provider_id), jobs$provider_id), ]
colnames(first_job) <- c("provider_id","first_job_id","first_job_appointment_time")
providers_detailed <- left_join(providers_detailed, first_job,by = "provider_id")
providers_detailed$days_live_job <- as.numeric(difftime(providers_detailed$first_job_appointment_time,providers_detailed$went_live_date,units="days"))
providers_detailed$days_span <- as.numeric(difftime(providers_detailed$first_job_appointment_time,providers_detailed$sign_up_date,units="days"))
rm(first_job)

######### counting the number of providers/jobs vs date
count_sign <- providers_detailed %>% select(provider_id, sign_up_date) %>% 
  group_by(Date=as.Date(sign_up_date)) %>% 
  mutate(provider_count_sign = n()) %>%
  select(Date,provider_count_sign) %>%
  slice(1)
count_call <- providers_detailed %>% select(provider_id, first_call_date) %>%
  group_by(Date=as.Date(first_call_date)) %>%
  mutate(provider_count_call = n()) %>%
  select(Date,provider_count_call) %>%
  slice(1)
count_live <- providers_detailed %>% select(provider_id, went_live_date) %>% 
  group_by(Date=as.Date(went_live_date)) %>% 
  mutate(provider_count_live = n()) %>%
  select(Date,provider_count_live) %>%
  slice(1)
count_job <- jobs %>% select(job_id,job_appointment_time) %>%
  group_by(Date=as.Date(job_appointment_time)) %>%
  mutate(jobs_count=n())%>%
  select(Date,jobs_count) %>%
  slice(1)

all_count <- full_join(count_sign, count_call)
all_count <- full_join(all_count,count_job)
all_count <- full_join(all_count,count_live)

rm(count_call,count_job,count_live,count_sign)

# Transposing the counts column into one column-Event type
all_count1 <- melt(all_count,id.vars = "Date", value.name="CountbyDate", variable.name="Event_type")


#### K-Means Clustering
today=as.Date("2017-08-1")
#jobs_2017 <- subset(jobs,format(as.Date(jobs$job_appointment_time),"%Y") ==2017)
cluster <- jobs %>%
  mutate(days_since = as.numeric(difftime(time1 = "2017-08-1",
                                          time2 = jobs$job_appointment_time,
                                          units = "days"))) %>%
  group_by(provider_id) %>%
  mutate(
    recency=min(days_since),
    frequency=n()) %>%
  select(provider_id, recency, frequency) %>%
  unique()

cluster = left_join(cluster,providers_detailed[,c("provider_id","days_span")])  
cluster1 <- cluster[complete.cases(cluster),]
cluster2 <- cluster1[complete.cases(cluster1), -1]
cluster2 <- data.frame(scale(cluster2))


wssplot <- function(cluster2, nc=3, seed=1234)
{
  wss <- (nrow(cluster2)-1)*sum(apply(cluster2,2,var))
  for (i in 2:nc){
    set.seed(seed)
    km<-kmeans(cluster2, centers=i)
    wss[i] <- km$tot.withinss
  }
  plot(1:nc, wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")
}

fit <- kmeans(cluster2, centers = 4, nstart = 20, iter.max = 50)
final_cluster<-data.frame(cluster1,fit$cluster)
table(final_cluster$fit.cluster)

plot(cluster1[, c("recency", "days_span")],
     col = fit$cluster,
     main = paste("k-means clustering of
                  Providers with", 4, "clusters"),
     xlab = "Recency", ylab = "Days Span")

######### exporting final tables
write.xlsx(as.data.frame(all_count1), file="/Users/saksham/Downloads/helping/deliverables/all_count.xlsx", sheetName = "sheet1",col.names=TRUE)
write.xlsx(providers_detailed, file="/Users/saksham/Downloads/helping/deliverables/providers_detailed.xlsx", sheetName = "providers",col.names=TRUE)
write.xlsx(final_cluster, file="/Users/saksham/Downloads/helping/deliverables/final_cluster.xlsx", sheetName = "Clusters",col.names=TRUE)

######### Extra attributes that can be used

# Calculating the counts in last 15, 30, 45, 60, 90, 120 and 180 days
jobs_2017 <- subset(jobs,format(as.Date(jobs$job_appointment_time),"%Y") ==2017)
periods = c(15,30,45,60,90,120,180)
for (period in periods) {
  last_counts <- jobs %>%
    filter(job_appointment_time > today - period) %>%
    group_by(provider_id) %>%
    mutate(jobcount = n()) %>%
    select(provider_id,jobcount) %>%
    unique()
  colnames(last_counts) <- c("provider_id",sprintf("%s.%d","jobcount",period))
  
  cluster = left_join(cluster, last_counts, by="provider_id")
}

