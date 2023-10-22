pp_recipes <- read.csv('C:\\Users\\igorr\\Documents\\archive\\PP_recipes.csv')
pp_users <- read.csv('C:\\Users\\igorr\\Documents\\archive\\PP_users.csv')
raw_interactions<- read.csv('C:\\Users\\igorr\\Documents\\archive\\RAW_interactions.csv')
raw_recipes<- read.csv('C:\\Users\\igorr\\Documents\\archive\\RAW_recipes.csv')

install.packages("reticulate")


library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(reticulate)
library(lubridate)

calorie_levels<-pp_recipes %>% group_by(calorie_level) %>% summarise(amount= n())

#widzimy, ze kategorie kalorycznosci sa na zblizonym poziomie/nie ma ciekawych danych



#wyglada na to, ze jest 58 technik


#stworzymy macierz zawieracajac 58 wierszy i dwie kolumny pierwsza niech mowi jaka technika, druga niech odpowiada
#ilosci przepisow zawierajacych ta technike

#ze wzgledu na czasochlonnosc operacji czy rozwazyc operacje na 10000 wierszach,
#wzgledem posilkow najbardziej popularnych(zdefiniowac jak rozumie sie popularnosc)

TECHNIQUES_LIST = c(
  'bake',
  'barbecue',
  'blanch',
  'blend',
  'boil',
  'braise',
  'brine',
  'broil',
  'caramelize',
  'combine',
  'crock pot',
  'crush',
  'deglaze',
  'devein',
  'dice',
  'distill',
  'drain',
  'emulsify',
  'ferment',
  'freez',
  'fry',
  'grate',
  'griddle',
  'grill',
  'knead',
  'leaven',
  'marinate',
  'mash',
  'melt',
  'microwave',
  'parboil',
  'pickle',
  'poach',
  'pour',
  'pressure cook',
  'puree',
  'refrigerat',
  'roast',
  'saute',
  'scald',
  'scramble',
  'shred',
  'simmer',
  'skillet',
  'slow cook',
  'smoke',
  'smooth',
  'soak',
  'sous-vide',
  'steam',
  'stew',
  'strain',
  'tenderize',
  'thicken',
  'toast',
  'toss',
  'whip',
  'whisk')


TECHNIQUES_LIST

#wezmy raw_interactions oraz recipe_id i rating, oraz liczbe ocen i rating

raw_interactions %>% select(recipe_id, rating)


interakcje<-raw_interactions %>% group_by(recipe_id) %>% summarise(wystapienia= n(), lacznaOcena= sum(rating))

#wezmy 100 przepisow, ktore mialy najwiecej recenzji, oraz te, ktore byly najwyzej oceniane
#wsrod przepisow, pozniej upewnimy sie, czy one wystepuja w danych

interakcje<- interakcje %>% arrange(desc(wystapienia)) %>% slice(1:1000) %>% select(recipe_id) %>% arrange(desc(recipe_id))
interakcje<- unlist(interakcje)

szukane<- pp_recipes %>% filter(pp_recipes$id %in% interakcje) %>% arrange(desc(id))

szukane


#sprawdzmy jakie teraz wyniki otrzymamy


init_techniques<- data.frame(
  technique= 1:58,
  Amount = rep(0, 58)
)

techniki<- szukane$techniques

for (i in 1:length(techniki)){
  able_to_extract<- str_split(gsub("\\[|\\]", "", techniki[i]), ",")[[1]]
  woth<-which(able_to_extract == " 1")
  init_techniques[woth, "Amount"]<- init_techniques[woth, "Amount"]+1
}

init_techniques<-init_techniques %>% group_by(technique) %>% arrange(desc(Amount))

TECHNIQUES_LIST[init_techniques$technique[1:5]]

#najbardziej popularne technki gotowania potraw wsrod potraw z najwieksza liczba recenzji to:
# "combine", "pour", "melt", "boil", "simmer"


#sprawdzmy teraz jak tak ogolnie przedstawia sie zaleznosc stosowanych technik od ilosci ocen

#w modelu zakladamy, ze jezeli jakas osoba zostawial ocene to pewnie tez uzyla przepisu
#czyli po prostu wsrod osob, ktore zostawily tak ogolnie jakas recenzje paatrzymy jakie
#techniki robienia posilkow byly najczesciej stosowane

init_techniques2<- data.frame(
  technique= 1:58,
  Amount = rep(0, 58)
)

interakcje_wszystkie<-raw_interactions %>% group_by(recipe_id) %>% summarise(wystapienia= n(), lacznaOcena= sum(rating))

przeps<- interakcje_wszystkie$recipe_id

interakcje_wszystkie<- interakcje_wszystkie %>% filter(przeps %in% pp_recipes$id) %>% arrange(desc(recipe_id))

wiersze2<- pp_recipes %>% filter(pp_recipes$id %in% przeps) %>% arrange(desc(id))

head(wiersze2)
head(interakcje_wszystkie)

techniki<-  wiersze2$techniques
oceny<- interakcje_wszystkie$wystapienia

for (i in 1:length(techniki)){
  able_to_extract<- str_split(gsub("\\[|\\]", "", techniki[i]), ",")[[1]]
  woth<-which(able_to_extract == " 1")
  init_techniques2[woth, "Amount"]<- init_techniques2[woth, "Amount"]+ oceny[i]
}

init_techniques2<-init_techniques2 %>% arrange(desc(Amount))

TECHNIQUES_LIST[init_techniques2$technique[1:5]]

#najbardziej popularne technki gotowania potraw wsrod wszystkich potraw to:
# "combine", "pour", "melt", "boil", "simmer"

#wezmy teraz te, ktore mialy najmniej ocen, nie wezmiemy jednak tych z 0 ocen bo moze byc to jakas anomalia w danych

init_techniques2<- init_techniques2 %>% arrange(Amount) %>% filter(Amount >0)

TECHNIQUES_LIST[init_techniques2$technique[1:5]]

#najmniej popularne techniki przygotowania potraw, wsrod potraw, ktore zostaly ocenione to:
# "distill"  "leaven"   "ferment"  "emulsify" "parboil", zakladamy tylko te potrawy, ktore znajduja sie wsrod
#ocenionych, bo gdy potrafa jest oceniona to raczej zostala przez kogos przygotowana



#czy istnieje korelacja miedzy czasem przyrzadzania posilku a popularnoscia posilku

minutki_ilosci<- raw_recipes %>% group_by(minutes) %>% summarise(ileWynikow= n()) %>% filter(minutes>0)

#zakladamy, ze nie da sie przygotowac posilku w 0 minut zatem takich danych pozbywamy sie

wykres_1<- minutki_ilosci%>% 
  ggplot(mapping = aes(x=minutes, y= ileWynikow )) + geom_point() + coord_cartesian(xlim = c(0, 300))

wykres_1 #prototyp wersja pre pre alpha


#ciekawia nas te punkty, ktore sa rzadziej rozlozone, jest wiecej wynikow

wykres_2<- minutki_ilosci %>% filter(ileWynikow > 400)%>% 
  ggplot(mapping = aes(x=minutes, y= ileWynikow )) + geom_point() + coord_cartesian(xlim = c(0, 300))



wykres_3<- minutki_ilosci %>% filter(ileWynikow > 400)%>% 
  ggplot(mapping = aes(x=minutes, y= ileWynikow )) + 
  geom_point(color = "blue")+ geom_density_2d(color= "red")

wykres_3


minutki<-minutki_ilosci %>% filter(ileWynikow<1000)

minutki$minutes<-as.character(minutki$minutes)

wykres_4 <- ggplot(minutki, aes(x = minutes, y = ileWynikow)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  labs(
    title = "Bar Plot",
    x = "Category",
    y = "Value"
  )

wykres_4

#dziwne, czyzby ludzie zaokraglali czas robienia posilkow?
#czemu jest nagle przeskok slupkow, az tak widoczny?


input_string<-raw_recipes$tags[1]
input_string <- gsub("\\[|\\]|'", "", input_string)

output_vector <- unlist(strsplit(input_string, ", "))

unique_tags<- c()

dlugosc<-length(raw_recipes$id)

for (i in 1:dlugosc){
  
  input_string<-raw_recipes$tags[i]
  
  in_string <- gsub("\\[|\\]|'", "", input_string)
  output_vector <- unlist(strsplit(input_string, ", "))
  
  unique_tags<- union(unique_tags, setdiff(output_vector,unique_tags ))
}

#w danych pojawilo sie ponad 900 unikalnych tagow

length(unique_tags)

tags_popularity<- data.frame(
  tag= unique_tags,
  Amount = rep(0, length(unique_tags)
))

tags_popularity

for(i in 1:dlugosc){
  input_string<-raw_recipes$tags[i]
  
  in_string <- gsub("\\[|\\]|'", "", input_string)
  output_vector <- unlist(strsplit(input_string, ", "))
  matching_rows<-tags_popularity$tag %in% output_vector
  tags_popularity$Amount[matching_rows] <- tags_popularity$Amount[matching_rows] + 1
}

tags_popularity<-tags_popularity %>% arrange(desc(Amount))

tags_popularity

#ciekawe wyniki otrzymalismy, nie liczac tagow takich jak 'preparation',
#'course', 'time-to-make', czy 'main-ingrediant' najpopularniejszym tagiem
#okazalo sie byc 'dietary', koreluje z tym inny tag 'low-in-something',
#'vegetables', 'low-carb', 'low-calorie', 'vegetarian'
#
#z przepisow, ktore sa najczesciej wrzucane to dominuja kategorie
#czasowe w nastepujacej kolejnosci:

#60-minutes-or-less, 30-minutes-or-less, 4-hours-or-less, 15-minutes-or-less
#zajmuja one kolejno  64101, 50365,  48075, 41044 elementów


#sprawdzmy jeszcze czy wiekszosc przepisow jest w miare prosta do zrobienia(wymaga mniejszej liczby krokow)

#policzmy ile srednio krokow jest wymagane, zrobmy jakiegos box plota itd


records_steps<- raw_recipes %>% group_by(n_steps) %>% summarise(Amount= n())

records_steps<- records_steps %>% arrange(n_steps)

wykres_6<-ggplot(records_steps, aes(x = n_steps , y = Amount, fill = as.character(n_steps))) +
  geom_boxplot() +
  labs(
    title = "Boxplot Example",
    x = "Category",
    y = "Value"
  )

wykres_6

#przecietnie posilek wymaga 9.8 kroków do zrobienia




#sprawdzamy teraz jak zmieniala sie liczba wrzucanych na strone posilkow
#z tagami 'dietary' oraz 'healty' na przestrzeni lat i czy jest jakies powiazanie
#z liczba osob, ktore wrzucaly jakies posilki na przestrzeni lat


#sprawdzmy najpierw jak zmieniala sie liczba wrzucanych posilkow na stronie na przestrzeni lat

raw_recipes$submitted

interesting_dates<- raw_recipes %>% select(id, contributor_id, submitted) %>% 
  mutate(submitted = ymd(submitted))


lata_wyniki <- interesting_dates %>%
  group_by(submitted_year = year(submitted)) %>%
  summarize(entry_count = n())


wykres_7<-ggplot(lata_wyniki, aes(x = factor(submitted_year), y = entry_count)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(
    title = "Count of Entries by Year",
    x = "Year",
    y = "Entry Count"
  ) +
  theme_minimal()

wykres_7

#mozemy po prostu sprawdzic jaki byl procentowy udzial tych tagow na przestrzeni lat

lata<-lata_wyniki$submitted_year

udzial_procentowy<-  data.frame(
  year= lata,
  Amount = rep(0, length(lata)
))


for(i in 1:length(raw_recipes$submitted)){
  input_string<-raw_recipes$submitted[i]
  input_changed<-  ymd(input_string)
  
  year<- year(input_changed)
  
  tag<- raw_recipes$tags[i]
  
  
  in_string <- gsub("\\[|\\]|'", "", tag)
  output_vector <- unlist(strsplit(tag, ", "))
  
  condition<- c("'dietary'") %in% output_vector | c("'healthy'") %in% output_vector
  
  which_row<- udzial_procentowy$year %in% year
  
  if(condition){
    udzial_procentowy$Amount[which_row] <- udzial_procentowy$Amount[which_row] + 1
  }
  
}

udzial_procentowy_official<- udzial_procentowy

udzial_procentowy_official$Amount<- udzial_procentowy_official$Amount/lata_wyniki$entry_count

#otrzymalismy bardzo ciekawe wyniki, okazuje sie, ze FOOD.com bylo strona
#ktora zaczynala ze zdrowymi posilkami ponad 90% przepisow wrzucanych na stronie zawieralo tagi
#"deitary" czy "healty", z czasem jednak przepisy wrzucane pod tymi tagami zaczely stanowic coraz mniej procent calkowitego
#udzialu w tagach wrzuconych produktow

udzial_procentowy$Amount

wykres_8<-ggplot(udzial_procentowy, aes(x = factor(year), y = Amount)) +
  geom_bar(stat = "identity", fill = "orange") +
  labs(
    title = "Count of Entries by Year",
    x = "Year",
    y = "Entry Count"
  ) +
  theme_minimal()+ coord_cartesian(ylim = c(0, 35000))


wykres_8

udzial_procentowy$
lata_wyniki

#sprawdzmy jak to wyglada kolo siebie
  

combined_plot<-combined_plot <- grid.arrange(wykres_7, wykres_8, ncol = 2)

combined_plot

#sprawdzmy teraz jak wygladaja nalozone na siebie

stacked_plots <- wykres_7 +
  geom_bar(data = udzial_procentowy, aes(x = factor(year), y = Amount, fill="orange"), stat = "identity", position = "stack", width = 0.5) +
  labs(
    title = "Stacked Bar Plot",
    x = "Category",
    y = "Value"
)

stacked_plots

#po stackowaniu widać, że tagi oznaczone zdrowym jedzeniem stanowiły ponad 90% w latach 1999-2002,
#w pozniejszych latach ten procent sie zmniejszyl, co sugeruje, ze poczatkowo strona food.com, poswiecona
#byla wrzucaniu przepisow ze stricte zdrowym jedzeniem



