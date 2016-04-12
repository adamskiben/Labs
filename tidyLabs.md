#Manipulating data with dplyr
mydf <- read.csv(path2csv, stringsAsFactors = FALSE)
dim(mydf)
head(mydf)
library(dplyr)
packageVersion("dplyr")
cran <- tbl_df(mydf)
rm("mydf")
cran
?select
select(cran, ip_id, package, country)
5:20
select(cran, r_arch:country)
select(cran, country:r_arch)
cran
select(cran, -time)
-5:20
-(5:20)
select(cran, -(X:size))
filter(cran, package == "swirl")
filter(cran, r_version == "3.1.1", country == "US")
?Comparison
filter(cran, r_version <= "3.0.2", country == "IN")
filter(cran, country == "US" | country == "IN")
filter(cran, size > 100500, r_os == "linux-gnu")
is.na(c(3, 5, NA, 10))
!is.na(c(3, 5, NA, 10)).'
filter(cran, !is.na(r_version))
cran2 <- select(cran, size:ip_id)
arrange(cran2, ip_id)
arrange(cran2, desc(ip_id))
arrange(cran2, package, ip_id)
arrange(cran2, country, desc(r_version), ip_id)
cran3 <- select(cran, ip_id, package, size)
cran3
mutate(cran3, size_mb = size / 2^20)
mutate(cran3, size_mb = size / 2^20, size_gb = size_mb / 2^10)
mutate(cran3, correct_size = size + 1000)
summarize(cran, avg_bytes = mean(size))


#Grouping and chaining with dplyr
library(dplyr)
 cran <- tbl_df(mydf)
rm("mydf")
 cran
?group_by
by_package <- group_by(cran, package)
by_package
summarize(by_package, mean(size))
pack_sum <- summarize(by_package,
                      count = n(),
                      unique = n_distinct(ip_id),
                      countries = n_distinct(country),
                      avg_bytes = mean(size))
pack_sum
quantile(pack_sum$count, probs = 0.99)
top_counts <- filter(pack_sum, count > 679)
top_counts
View(top_counts)
top_counts_sorted <- arrange(top_counts, desc(count))
View(top_counts_sorted)
quantile(pack_sum$unique, probs = 0.99)
top_unique <- filter(pack_sum, unique > 465)
View(top_unique)
top_unique_sorted <- arrange(top_unique, desc(unique))
 View(top_unique_sorted)
top_countries <- filter(pack_sum, countries > 60)
result1 <- arrange(top_countries, desc(countries), avg_bytes)
print(result1)
result2 <-
  arrange(
    filter(
      summarize(
        group_by(cran,
                 package
        ),
        count = n(),
        unique = n_distinct(ip_id),
        countries = n_distinct(country),
        avg_bytes = mean(size)
      ),
      countries > 60
    ),
    desc(countries),
    avg_bytes
  )
print(result2)
result3 <-
  cran %>%
  group_by(package) %>%
  summarize(count = n(),
            unique = n_distinct(ip_id),
            countries = n_distinct(country),
            avg_bytes = mean(size)
  ) %>%
  filter(countries > 60) %>%
  arrange(desc(countries), avg_bytes)
print(result3)
View(result3)
cran %>%
  select(ip_id, country, package, size) %>%
	print
cran %>%
  select(ip_id, country, package, size) %>%
  mutate(size_mb = size / 2^20)
cran %>%
  select(ip_id, country, package, size) %>%
  mutate(size_mb = size / 2^20) %>%
  filter(size_mb <= 0.5)
cran %>%
  select(ip_id, country, package, size) %>%
  mutate(size_mb = size / 2^20) %>%
  filter(size_mb <= 0.5) %>%
  arrange(desc(size_mb))



#Tidying data with tidyr
students
?gather
gather(students, sex, count, -grade)
students2
res <- gather(students2, sex_class, count, -grade)
res
?separate
 separate(res, sex_class, c("sex", "class"))
students2 %>%
  gather(sex_class, count, -grade) %>%
  separate(sex_class, c("sex", "class")) %>%
  print
students3 %>%
  gather(class, grade, class1:class5, na.rm = TRUE) %>%
  print
students3
students3 %>%
  gather(class, grade, class1:class5, na.rm = TRUE) %>%
  spread(test, grade) %>%
  print
students3 %>%
  gather(class, grade, class1:class5, na.rm = TRUE) %>%
  spread(test, grade) %>%
  mutate(class = extract_numeric(class)) %>%
  print
 extract_numeric("class5")
student_info <- students4 %>%
  select(id, name, sex) %>%
  print
?spread
student_info <- students4 %>%
  select(id, name, sex) %>%
  unique %>%
  print
gradebook <- students4 %>%
  select(id, class, midterm, final) %>%
  print
passed
failed
passed <- passed %>% mutate(status = "passed")
failed <- failed %>% mutate(status = "failed")
bind_rows(passed, failed)
sat %>%
  select(-contains("total")) %>%
  gather(part_sex, count, -score_range) %>%
  separate(part_sex, c("part", "sex")) %>%
  print
sat %>%
  select(-contains("total")) %>%
  gather(part_sex, count, -score_range) %>%
  separate(part_sex, c("part", "sex")) %>%
  group_by(part, sex) %>%
  mutate(total = sum(count),
         prop = count / total
   %>% print


#Dates and times with lubridate
help(package = lubridate)
this_day <- today()
this_day
month(this_day)
wday(this_day)
wday(this_day, label = TRUE)
this_moment <- now()
this_moment
second(this_moment)
my_date <- ymd("1989-05-17")
my_date
class(my_date)
ymd("1989 May 17")
mdy("March 12, 1975")
dmy(25081985)
ymd("192012")
ymd("1920/1/2")
dt1
 ymd_hms(dt1)
hms("03:22:14")
dt2
ymd(dt2)
update(this_moment, hours = 8, minutes = 34, seconds = 55)
this_moment
this_moment <- update(this_moment, hours = 10, minutes = 16, seconds = 0)
this_moment
nyc <- now("America/New_York")
nyc
 depart <- nyc + days(2)
depart
depart <- update(depart, hours = 17, minutes = 34)
 arrive <- depart + hours(15) + minutes(50)
?with_tz
arrive <- with_tz(arrive, "Asia/Hong_Kong")
last_time <- mdy("June 17, 2008", tz = "Singapore")
?new_interval
 how_long <- new_interval(last_time, arrive)
as.period(how_long)
stopwatch()


