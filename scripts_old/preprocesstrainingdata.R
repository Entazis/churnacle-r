
preprocessTrainingdata <- function(){
  
  user_plot <- user_next_sessions1
  
  user_plot1 <- user_plot[order(as.integer(user_plot[,2]), as.integer(user_plot[,3])),]
  
  user_plot2 <- unite(user_plot1, subtrans, c(subscription, transaction), remove = TRUE, sep=".")
  
  user_plot3 <- user_plot2
  user_plot3$subtrans <- factor(user_plot3$subtrans, 
                                levels = c("1.1","1.2","1.3","1.4","1.5","1.6","1.7","1.8","1.9","1.10","1.11","1.12","1.13",
                                           "2.1","2.2","2.3","2.4","2.5","2.6","2.7","2.8","2.9","2.10","2.11","2.12","2.13","2.14","2.15", 
                                           "3.1","3.2","3.3","3.4","3.5","3.6","3.7","3.8","3.9","3.10","3.11", 
                                           "4.1","4.2","4.3","4.4","4.5","4.6","4.7","4.8","4.9","4.10","4.11","4.12",
                                           "5.1",
                                           "6.1"))
  print(levels(user_plot3$subtrans))
  
  user_plot3$submissionCnt <- as.integer(user_plot3$submissionCnt)
  
  user_plot3$paidnext <- as.factor(as.character(user_plot3$paidnext))
  
  print(levels(user_plot3$paidnext))
  
  
  ggplot(user_plot3, 
         aes(x=subtrans, y=submissionCnt, color=paidnext)) +
    geom_point() +
    geom_jitter() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
}