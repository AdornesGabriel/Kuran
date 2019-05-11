library(ggplot2)
library(tools)
library(gridExtra)

starA <- read.table('star_name.dat')
starB <- read.table('star_name.dat')
c <- (0.49) #fatores de ajuste
cf <- (0)
cr <- (0)
mag.mean <- mean(starA$mag)
mag.meanB <- mean(starB$mag)
mag.meanf <- mean(mag.mean,mag.meanB)

smooth_friedman = supsmu(x = starA$time/3600, y = starA$mag)
smooth_friedmanB = supsmu(x = starB$time/3600, y = starB$mag)
final_smooth = supsmu(x = (smooth_friedmanB$x + smooth_friedman$x)/2, y = (smooth_friedmanB$y + smooth_friedman$y)/2)

# ----

#teste
dados.teste =data.frame()

plot <- ggplot() +
  geom_hline(yintercept = mag.meanf, colour = 'purple') +
  geom_line(data = starA, aes(x = starA$time/3600, y = starA$mag-cr), colour = 'red', size=0.25) +
  geom_line(data = starB, aes(x = starB$time/3600, y = starB$mag+c), colour = 'blue', size=0.25) +
  labs(title='overlay', x = 'time (hours)', y = 'magnitude', subtitle = 'star_name1 and star_name2') +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 10),
        plot.subtitle = element_text(hjust = 0.5, size = 10))

plot2 <- ggplot() +
  geom_hline(yintercept = mag.meanf, colour = "purple") +
  geom_line(aes(x = smooth_friedman$x, y = smooth_friedman$y-0.08), size = 0.5, colour = "red") +
  geom_line(aes(x = smooth_friedmanB$x, y = smooth_friedmanB$y+c), size = 0.5, colour = 'blue') +
  labs(title = 'smooth overlay', subtitle = 'star_name1 and star_name2',x = 'time (hours)', y = 'magnitude') +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 10),
        plot.subtitle = element_text(hjust = 0.5, size = 10))
 
print(grid.arrange(plot, plot2, ncol=2))
