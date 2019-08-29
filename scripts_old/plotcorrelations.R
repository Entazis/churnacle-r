
plotCorrelations <- function(){
  user_plot <- user_next_sessions4
  colnum <- dim(user_plot)[2]-3
  
  user_plot <- user_next_firstm2 %>% 
                    mutate(sum=rowSums(user_next_firstm2[5:3144]))
  plot <- ggplot(user_plot, 
         aes(x=submissions, y=sum, color=paidnext)) +
    geom_point() +
    geom_jitter() +
    facet_grid(paymenthandler~paidnext)
  '/
  ggplot(user_next_sessions1, 
         aes(x=submissionCnt, y=sum, color=paidnext)) +
    geom_point() +
    geom_jitter()
  
  ggplot(user_plot, 
         aes(x=submissions, y=sessionsum, color=paidnext)) +
    geom_point() +
    geom_jitter()
  
  ggplot(user_plot, aes(x=submissions, y=sessionsum, color=paidnext)) + 
    geom_point() + 
    geom_jitter() +
    facet_grid(.~paymenthandler)
  /'
  return(plot)
}