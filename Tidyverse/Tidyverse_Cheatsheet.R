### Subsetting as value	
tibble$column_name
tibble[['column_name']]
tibble[[1]]
tibble[,1]

### Subsetting as variable	tibble[1]
tibble[1,]

### Embed subsets into pipe with '.'	
tibble %>% .$column_name
tibble %>% .[['column_name']] 
tibble %>% .[[1]]

### coerce data into tibble	
data_name <- as_tibble(data.frame)

### create new tibble from columns	
data_name <- tibble(columnA, columnB, columnN)

### gather	
heroes %>% gather(Universe, Name, Marvel:DarkHorseComics)

### spread	
heroes %>% spread(Universe, Name)

### separate - split at character	
heroes %>% spread(Gender_film, c('Gender', 'Has_film'), ',')

### separate - split after position	
heroes %>% spread(Gender_film, c('Gender', 'Has_film'), '2')

### select  - create subsets by columns	
select(heros, Name, Gender, Universe)
select(heros, Name:Gender, Universe)
select(heros, -(Has_film))

### filter - create subsets by row	
filter(heros, Gender =='Female')
filter(heros, Gender =='Female', Has_film == 1)

### arrange - reorder rows	
arrange(heros, Gender, Universe)

### summarise 	
summarise(heros, mean(Start_date))

## summarise combined with group_by	
Heros_grouped <- group_by(heros, Gender)
summarise(heros_grouped, mean(Start_date))

### mutate	
heros %>% filter(has_film == '1') %>% mutate(Film_delay = Film_date - Start_date)

### joins	
inner_join(Gender, Universe, by='Name')
left_join(Gender, Universe, by='Name')
right_join(Gender, Universe, by='Name')
full_join(Gender, Universe, by='Name')
semi_join(Gender, Universe, by='Name')
anti_join(Gender, Universe, by='Name')

### ggplot	
ggplot(data) + 
geom_point(aes(x,y)) +
geom_abline(aes(intercept=beta0, slope=beta1))