
user_next_all3_p2 <- user_next_all3[-(1:79),]
row.names(user_next_all3_p2) <- 1:nrow(user_next_all3_p2)
user_next_all3_p2 <- user_next_all3_p2[complete.cases(user_next_all3_p2),]
user_next_all3_p2 <- user_next_all3_p2 %>%
  select(-email) %>%
  select(-endDate) %>%
  drop_na() %>%
  select(paidnext, everything())
user_next_all3_p2 <- cbind(as.data.frame(as.numeric(as.factor(user_next_all3_p2[,1]))-1),as.data.frame(sapply(user_next_all3_p2[,-1], as.numeric)))
colnames(user_next_all3_p2)[1] <- "paidnext"

hist(as.numeric(user_next_all3_p$period), breaks = 30, xlim=c(1,15), ylim=c(0,600))

hist(user_next_all3_p$part1, breaks=400, xlim=c(0,50), ylim=c(0,100))
hist(user_next_all3_p$part2, breaks=400, xlim=c(0,50), ylim=c(0,100))
hist(user_next_all3_p$part3, breaks=400, xlim=c(0,50), ylim=c(0,100))
hist(user_next_all3_p$part4, breaks=400, xlim=c(0,50), ylim=c(0,100))

levels(user_next_all3_p$paymentHandler)
hist(as.numeric(user_next_all3_p$paymentHandler), breaks = 2, xlim = c(1,3), ylim=c(0,3200))

hist(user_next_all3_p$submissions, breaks=200, xlim=c(0,200), ylim=c(0,200))
hist(user_next_all3_p$submissionsOlders, breaks=200, xlim=c(0,200), ylim=c(0,400))

hist(user_next_all3_p$totalTimeSpentOnLessons, breaks=20, xlim=c(0,150000), ylim=c(0,3500))
hist(user_next_all3_p$totalTimeSpentOnLessonsAndOlders, breaks=20, xlim=c(0,150000), ylim=c(0,3500))

hist(user_next_all3_p$first1, xlim = c(0,1000), breaks = 100)
hist(user_next_all3_p$first2, xlim = c(0,1000), breaks = 100)
hist(user_next_all3_p$first3, xlim = c(0,1000), breaks = 100)
hist(user_next_all3_p$first4, xlim = c(0,1000), breaks = 100)
hist(user_next_all3_p$first5, xlim = c(0,1000), breaks = 100)

hist(user_next_all3_p$last1, xlim = c(0,1000), ylim = c(0,500), breaks = 100)
hist(user_next_all3_p$last2, xlim = c(0,1000), ylim = c(0,500), breaks = 100)
hist(user_next_all3_p$last3, xlim = c(0,1000), ylim = c(0,500), breaks = 100)
hist(user_next_all3_p$last4, xlim = c(0,1000), ylim = c(0,500), breaks = 100)
hist(user_next_all3_p$last5, xlim = c(0,1000), ylim = c(0,500), breaks = 100)

keep <- c("email", "endDate", "paidnext", "period", "part1", "part2", "part3", "part4", 
          "paymentHandler", "submissions", "submissionsOlders", "totalTimeSpentOnLessons", "totalTimeSpentOnLessonsAndOlders",
          "first1", "first2", "first3", "first4", "first5",
          "last1", "last2", "last3", "last4", "last5",
          "locale", "planId", "cost", "spent", "duplicatedSubmissions",
          "hasSlack", "hasFacebook", "hasGoogle", "hasFreematerial")

user_next_all3_p2 %>%
  select(-email, -endDate) %>%
  mutate(
    paidnext = paidnext %>% as.factor() %>% as.numeric(),
    period = as.numeric(period),
    periodLog = log(as.numeric(period)),
    part1 = part1,
    part2 = part2,
    part3 = part3,
    part4 = part4,
    paymentHandler = as.numeric(paymentHandler),
    submissions = submissions,
    submissionsOlders = submissionsOlders,
    totalTimeSpentOnLessons = totalTimeSpentOnLessons,
    totalTimeSpentOnLessonsAndOlders = totalTimeSpentOnLessonsAndOlders,
    first1 = first1,
    first2 = first2,
    first3 = first3,
    first4 = first4,
    first5 = first5,
    last1 = last1,
    last2 = last2,
    last3 = last3,
    last4 = last4,
    last5 = last5,
    locale = as.numeric(as.factor(locale)), 
    planId = as.numeric(as.factor(planId)), 
    cost = as.numeric(cost),
    costLog = log(as.numeric(cost)), 
    spentLog = log(as.numeric(spent)), 
    spent = as.numeric(spent),
    duplicatedSubmissions = as.numeric(duplicatedSubmissions),
    hasSlack = as.numeric(as.logical(hasSlack)), 
    hasFacebook = as.numeric(as.logical(hasFacebook)), 
    hasGoogle = as.numeric(as.logical(hasGoogle)), 
    hasFreematerial = as.numeric(as.logical(hasFreematerial))
  ) %>%
  correlate() %>%
  focus(paidnext) %>%
  fashion()

correlations <- user_next_all3_p2 %>%
  correlate() %>%
  focus(paidnext) %>%
  fashion()
correlations <- correlations[rev(order(correlations$paidnext)),]
row.names(correlations) <- 1:nrow(correlations)

