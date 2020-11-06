# Dragon regression examples

weight <- c(4.2, 3.5, 3.8, 5.1)

height <- c(5.1, 4.3, 5.5, 5.9)

pattern <- c("striped", "spotted", "spotted", "striped")

dragons <- data.frame(pattern, weight, height)

dragons

asso <- ggplot(dragons, aes(x=height, y=weight)) + 
  geom_point(color="blue", size = 4) +
  geom_smooth(method=lm, se=FALSE, color="darkgreen") +
  theme_classic() +
  theme(
  axis.title.x = element_text(color="black", size=14),
  axis.title.y = element_text(color="black", size=14),
  axis.text.x  = element_text(size=12),
  axis.text.y  = element_text(size=12)
    ) 
   
asso

re<- lm(weight ~ height, data=dragons)
summary(re)

