---
title: 'Data Cleaning: Eurovision'
author: "Max Glonek"
date: Trimester 1,
  2025
output:
  pdf_document:
    toc: true
  html_document:
    number_sections: true
    df_print: paged
    toc: true
    toc_float: true
editor_options:
  chunk_output_type: console
---



# Rule 1: Look at the Data

# Rule 2: Is There a Package to Deal With That


``` r
songs <- jsonlite::fromJSON(glue::glue("./data/eurovision-lyrics.json"))
```

# Rule 3: Break it Into Pieces


``` r
songs[[1]]
```

```
## $`#`
## [1] "1"
## 
## $Country
## [1] "Netherlands"
## 
## $`#.1`
## [1] "1"
## 
## $Artist
## [1] "Jetty Paerl"
## 
## $Song
## [1] "De vogels van Holland"
## 
## $Language
## [1] "Dutch"
## 
## $Pl.
## [1] "-"
## 
## $Sc.
## [1] "-"
## 
## $Eurovision_Number
## [1] 1
## 
## $Year
## [1] "1956"
## 
## $Host_Country
## [1] "Switzerland"
## 
## $Host_City
## [1] "Lugano"
## 
## $Lyrics
## [1] "De vogels van Holland zijn zo muzikaal\nZe leren in hun prille jeugd al tierelieren\nDe merel, de lijster en de nachtegaal\nOm zo de lente in Holland goed te kunnen vieren\n\nHet is geen wonder, want nergens\nZijn de plassen zo blauw\nAls in Holland - mijnheer\nAls in Holland - mevrouw\nHet is geen wonder, want nergens\nIs het gras zo vol dauw\nZijn de meisjes zo lief\nZijn de meisjes zo trouw\nEn daarom zijn de vogels hier allemaal\nZo muzikaal, zo muzikaal, zo muzikaal\n\nDe hele wereld door heb ik vogels horen zingen\nIn het zuiden, in het westen, in het noorden\nIn vele verre landen heb ik vogels horen zingen\nZij zingen kleine liedjes zonder woorden\n\nDe Franse vogels zingen \"tudeludelu\"\nJapanse vogels zingen \"tudeludelu\"\nChinese vogels zingen \"tudeludelu\"\nMaar de vogels zingen nergens\nZo gelukkig en blij\nAls in Holland in het voorjaar in de wei\n\nDe vogels van Holland zijn zo muzikaal\nZe leren in hun prille jeugd al tierelieren\nDe merel, de lijster en de nachtegaal\nOm zo de lente in Holland goed te kunnen vieren\n\nHet is geen wonder, want nergens\nZijn de plassen zo blauw\nAls in Holland - mijnheer\nAls in Holland - mevrouw\nHet is geen wonder, want nergens\nIs het gras zo vol dauw\nZijn de meisjes zo lief\nZijn de meisjes zo trouw\nEn daarom zijn de vogels hier allemaal\nZo muzikaal, zo muzikaal, zo muzikaal"
## 
## $`Lyrics translation`
## [1] "The birds of Holland are so musical\nThey already learn to twitter in their early youth\nThe blackbird, the thrush and the nightingale\nSo they can celebrate spring in Holland It's no wonder, because nowhere the puddles are so blue\nAs in Holland, sir\nAs in Holland, madam\nIt's no wonder, because nowhere the grass is so full of dew Are the girls so sweet, are the girls so faithfull\nAnd that's why all the birds here are\nSo musical, So musical, So musical Across the whole world I've heard birds sing\nTo the south, to the west, to the north\nIn many faraway countries I've heard birds sing\nThey sing little songs without words The French birds sing toodledoo\nJapanese birds sing toodledoo\nChinese birds sing toodledoo\nBut nowhere the birds sing so happily and cheerfully\nAs in Holland in spring in the meadow The birds of Holland are so musical\nThey already learn to twitter in their early youth\nThe blackbird, the thrush and the nightingale\nSo they can celebrate the spring in Holland It's no wonder, because nowhere the puddles are so blue\n As in Holland, sir\n As in Holland, madam\nIt's no wonder, because nowhere the grass is so full of dew Are the girls so sweet, are the girls so faithfull\nAnd that's why all the birds here are\nSo musical, So musical, So musical"
```


``` r
parse_song <- function(song){
  country <- song$Country
  artist <- song$Artist
  title <- song$Song
  year <- song$Year
  lyrics <- song$Lyrics
  translation <- song$`Lyrics translation`
  place <- song$Pl.
  tibble(
    country, artist, title, year, lyrics, translation, place
  )
}
parse_song(songs[[1]])
```

```
## # A tibble: 1 x 7
##   country     artist      title                 year  lyrics   translation place
##   <chr>       <chr>       <chr>                 <chr> <chr>    <chr>       <chr>
## 1 Netherlands Jetty Paerl De vogels van Holland 1956  "De vog~ "The birds~ -
```

# Rule 4: Get it Into a Tibble


``` r
songs <- 
  songs %>% 
  map_df(parse_song, .id = "ID")
songs
```

```
## # A tibble: 1,644 x 8
##    ID    country        artist              title year  lyrics translation place
##    <chr> <chr>          <chr>               <chr> <chr> <chr>  <chr>       <chr>
##  1 0     Netherlands    Jetty Paerl         De v~ 1956  "De v~ "The birds~ -    
##  2 1     Switzerland    Lys Assia           Das ~ 1956  "Das ~ "The old c~ -    
##  3 2     Belgium        Fud Leclerc         Mess~ 1956  "Mess~ "Ye drowne~ -    
##  4 3     Germany (West) Walter Andreas Sch~ Im W~ 1956  "Es g~ "There is ~ -    
##  5 4     France         Mathé Altéry        Le t~ 1956  "Chan~ "Sing, car~ -    
##  6 5     Luxembourg     Michèle Arnaud      Ne c~ 1956  "Si o~ "If they t~ -    
##  7 6     Italy          Franca Raimondi     Apri~ 1956  "La p~ "The first~ -    
##  8 7     Netherlands    Corry Brokken       Voor~ 1956  "Voor~ "Over for ~ -    
##  9 8     Switzerland    Lys Assia (2)       Refr~ 1956  "[Int~ "Chorus of~ 1    
## 10 9     Belgium        Mony Marc           Le p~ 1956  "Les ~ "The bells~ -    
## # i 1,634 more rows
```

# Rule 5: Look at Each Column

We need to make sure all of our data makes sense in context. You can find the source data at this link: <https://www.kaggle.com/minitree/eurovision-song-lyrics>

## ID

This is an ID number assigned to each song. We should check that the song IDs are all unique.


``` r
songs %>% count(ID) %>% filter(n > 1)
```

```
## # A tibble: 0 x 2
## # i 2 variables: ID <chr>, n <int>
```

They are unique, but they're also not useful, so delete them.


``` r
songs <- 
  songs %>% 
  select(-ID)
songs
```

```
## # A tibble: 1,644 x 7
##    country        artist                 title    year  lyrics translation place
##    <chr>          <chr>                  <chr>    <chr> <chr>  <chr>       <chr>
##  1 Netherlands    Jetty Paerl            De voge~ 1956  "De v~ "The birds~ -    
##  2 Switzerland    Lys Assia              Das alt~ 1956  "Das ~ "The old c~ -    
##  3 Belgium        Fud Leclerc            Messieu~ 1956  "Mess~ "Ye drowne~ -    
##  4 Germany (West) Walter Andreas Schwarz Im Wart~ 1956  "Es g~ "There is ~ -    
##  5 France         Mathé Altéry           Le temp~ 1956  "Chan~ "Sing, car~ -    
##  6 Luxembourg     Michèle Arnaud         Ne croi~ 1956  "Si o~ "If they t~ -    
##  7 Italy          Franca Raimondi        Aprite ~ 1956  "La p~ "The first~ -    
##  8 Netherlands    Corry Brokken          Voorgoe~ 1956  "Voor~ "Over for ~ -    
##  9 Switzerland    Lys Assia (2)          Refrain  1956  "[Int~ "Chorus of~ 1    
## 10 Belgium        Mony Marc              Le plus~ 1956  "Les ~ "The bells~ -    
## # i 1,634 more rows
```

## Artist

Are there any artists who appear more than once?


``` r
songs %>% 
  count(artist) %>%
  arrange(-n)
```

```
## # A tibble: 1,621 x 2
##    artist                  n
##    <chr>               <int>
##  1 Ana Soklič              2
##  2 Benny Cristo            2
##  3 Blas Cantó              2
##  4 Daði og Gagnamagnið     2
##  5 Destiny                 2
##  6 Eden Alene              2
##  7 Efendi                  2
##  8 Gjon's Tears            2
##  9 Go_A                    2
## 10 Hooverphonic            2
## # i 1,611 more rows
```


``` r
songs %>%
  filter(str_detect(artist, "Jed"))
```

```
## # A tibble: 2 x 7
##   country artist      title     year  lyrics                   translation place
##   <chr>   <chr>       <chr>     <chr> <chr>                    <chr>       <chr>
## 1 Ireland Jedward     Lipstick  2011  "You say you’re on it b~ English     8    
## 2 Ireland Jedward (2) Waterline 2012  "Floodgates, can't wait~ English     19
```


``` r
songs %>%
  filter(country == "Australia")
```

```
## # A tibble: 7 x 7
##   country   artist             title            year  lyrics   translation place
##   <chr>     <chr>              <chr>            <chr> <chr>    <chr>       <chr>
## 1 Australia Guy Sebastian      Tonight Again    2015  "[Verse~ English     5    
## 2 Australia Dami Im            Sound of Silence 2016  "[Verse~ English     2    
## 3 Australia Isaiah             Don't Come Easy  2017  "[Verse~ English     9    
## 4 Australia Jessica Mauboy     We Got Love      2018  "[Verse~ English     20   
## 5 Australia Kate Miller-Heidke Zero Gravity     2019  "[Verse~ English     9    
## 6 Australia Montaigne          Don't Break Me   2020  "I don'~ English     -    
## 7 Australia Montaigne          Technicolour     2021  "I wann~ English     -
```

## Title


``` r
songs %>%
  count(title) %>% 
  arrange(-n)
```

```
## # A tibble: 1,608 x 2
##    title               n
##    <chr>           <int>
##  1 Shine               4
##  2 Angel               3
##  3 Time                3
##  4 Amen                2
##  5 Attention           2
##  6 Casanova            2
##  7 Coming Home         2
##  8 Congratulations     2
##  9 Fairytale           2
## 10 Goodbye             2
## # i 1,598 more rows
```

## Year


``` r
range(songs$year)
```

```
## [1] "1956" "2021"
```


``` r
songs %>% 
  count(year) %>% 
  ggplot(aes(year, n)) + 
  geom_point() + 
  theme(axis.text.x = element_text(angle = -90, hjust=0))
```



\begin{center}\includegraphics[width=0.7\linewidth]{DS_CS02_Data_Cleaning_Eurovision_files/figure-latex/unnamed-chunk-12-1} \end{center}

## Lyrics


``` r
songs %>% 
  sample_n(5) %>% 
  pull(lyrics) %>% 
  cat(sep = "\n\n\n")
```

```
## It's been a long time since we were together
## I'm back in Ireland and I miss you more than ever
## In early spring we parted and I've been here since then
## But if I could only see you once again
## 
## Meet me in Paris on a Champs-Élysées night
## We could be in Rome again, 'neath the Trevi fountain light
## We should be together, and maybe we just might
## If you could only meet me somewhere in Europe tonight
## 
## I remember Amsterdam as we sailed along the canal
## And as the leaves began to fall, we were walking in old Bruxelles
## In the Black Forest on a German summer's day
## And the memories refuse to go away
## 
## Meet me in Paris on a Champs-Élysées night
## We could be in Rome again, 'neath the Trevi fountain light
## We should be together, and maybe we just might
## If you could only meet me somewhere in Europe
## 
## Don't you remember those Adriatic days?
## I miss your laughter and all your little ways
## I can still see you in London, walking on Trafalgar Square
## And drinking wine in Old Seville, how I wish that we were there
## 
## Meet me in Paris on a Champs-Élysées night
## We could be in Rome again, 'neath the Trevi fountain light
## We should be together, maybe we just might
## If you could only meet me somewhere in Europe tonight
## 
## Meet me in Paris on a Champs-Élysées night
## We could be in Rome again, 'neath the Trevi fountain light
## We should be together, and maybe we just might
## If you could only meet me somewhere in Europe tonight
## 
## Somewhere in Europe tonight
## Somewhere in Europe
## 
## 
## Pam padadam padadam...
## Padada dadadam...
## Padadam padadam...
## 
## À l'horizon, une touche d'argent
## Comme après l'orage, s'en vont les nuages
## Les nuits d'été, sommeil oublié
## Grâce à ton parfum, je retrouve mon chemin
## 
## Il pleut de l'or, chaque fois que tu me parles d'amour
## Il pleut de l'or, ensemble partons pour faire le grand tour
## Sous la voûte étoilée, les destins sont dorés
## Il pleut de l'or
## 
## Pam padadam padadam...
## Padada dadadam...
## Padadam padadam...
## 
## Et au matin, secrets communs
## Les regards échangés sont complices de l'été
## Le jour chasse la magie
## Reste les yeux qui brillent
## 
## Il pleut de l'or, chaque fois que tu me parles d'amour (Il pleut de l'or)
## Il pleut de l'or, ensemble partons pour faire le grand tour
## Sous la voûte étoilée, les destins sont dorés
## Il pleut de l'or, toute la nuit et jusqu'à l'aurore
## Il pleut de l'or
## 
## Danse avec moi, vole avec moi
## Parle tout bas, parle-moi
## 
## (Il pleut de l'or) Il pleut de l'or
## (Il pleut de l'or)
## 
## Il pleut de l'or, ensemble partons pour faire le grand tour
## Sous la voûte étoilée, les destins sont dorés
## Il pleut de l'or, toute la nuit et jusqu'à l'aurore
## Il pleut de l'or
## 
## 
## spoken:
## "Και γαρ εβούλοντο μυάναι την γην ταύτην
## Μη γιγνώσκοντες την ύβριν, ην εποίουν..."
## 
## Είχα μια περιουσία, την καταχραστήκανε
## Κάναν ιεροσυλία κι ούτε που ντραπήκανε
## Την ιστορία της ζωής μου δε σκεφτήκανε
## 
## Ποια προσευχή εγώ να κάνω για τα κρίματά τους
## Ποια προσευχή μπορώ να πω για να σωθούν
## Στάζω κρασί για να ξεπλύνω τ' αμαρτήματά τους
## Ποια προσευχή να πω γι' αυτούς, που με πονούν
## 
## Να μου πάρουνε το θρόνο κάποιοι προσπαθήσανε
## Έν' αστέρι είχα μόνο και το απαιτήσανε
## Το πόσο άντεξα στο χρόνο αγνοήσανε
## 
## Ποια προσευχή εγώ να κάνω για τα κρίματά τους
## Ποια προσευχή μπορώ να πω για να σωθούν
## Στάζω κρασί για να ξεπλύνω τ' αμαρτήματά τους
## Ποια προσευχή να πω γι' αυτούς, που με πονούν
## 
## Ποια προσευχή εγώ να κάνω για τα κρίματά τους
## Ποια προσευχή μπορώ να πω για να σωθούν
## Στάζω κρασί για να ξεπλύνω τ' αμαρτήματά τους
## Ποια προσευχή να πω γι' αυτούς, που με πονούν
## 
## 
## [Verse 1: Jessika]
## Bullied from the moment we were born
## We were always on our own
## No one ever said we should be proud
## Or embrace the fact of standing out
## Forward too long, we just played along
## Always putting up a show
## But that was then, look at us now
## All we want is for them to know
## 
## [Pre-Chorus 1: Jessika]
## We are who we are
## And who we are is who we wanna be
## We don't have to listen
## 'Cause all that counts is you and me
## 
## [Chorus 1: Jessika]
## And then we’ll be rising where we fall, oh, oh
## In the middle of storm we're standing tall, oh, standing tall
## And if they tell us why we're wrong, oh, oh
## Then the love in our hearts will keep us strong, oh, oh
## 
## [Verse 2: Jenifer Brening]
## Uh listen up. listen up
## It's me, Jenny B, what you get is what you see
## As for Jess over here, she's a special VIP
## So you better listen carefully
## If they dissin' you on Twitter
## Don't get sad, don't be bitter, don't give up or be a quitter
## Show them you're better (yeah, hell no)
## If they say so, get in the car, rev it up, and be it a star
## 'Cause you know who exactly who we are
## Give it to 'em, Jess
## 
## [Pre-Chorus 2: Jessika & Jenifer Brening]
## We are who we are (yeah)
## And who we are is who we wanna be
## We don't have to listen (don't listen)
## 'Cause all that counts is you and me (hey, hey, hey, hey)
## 
## [Chorus 2: Jessika & Jenifer Brening]
## And then we’ll be rising where we fall, oh, oh yeah, we keep getting up and dusting ourselves off
## In the middle of storm we're standing tall (we won’t doubt ourselves and we’ll be calling their bluff)
## Oh, standing tall (this is who we are)
## And if they tell us why we're wrong, oh, oh (we will march onward, spreading the love)
## Then the love in our hearts will keep us strong, oh, oh (yeah, yeah, yeah)
## 
## [Bridge: Jessika]
## They can make it silent
## But in our hearts we'll never be
## 
## [Outro: Jessika & Jenifer Brening]
## And then we’ll be rising where we fall, oh, yeah (yeah, yeah)
## In the middle of storm we're standing tall, oh, standing tall
## And if they tell us why we're wrong, oh, oh, yeah
## Then the love in our hearts will keep us strong, oh, oh
## 
## 
## (M. Fabrizio/R. Fogli/V. Spampinato)
## Buonasera quando arriverai
## Ti toglierai l'inverno e lo appenderai
## Avrai due braccia grandi come una foresta
## E la quiete dopo la tempesta
## 
## Per Lucia, ritrovare
## Il foglio più bianco e poi cominciare;
## Io vorrei, per esempio
## Legarle i capelli con un filo di vento;
## Per Lucia basta poco
## Ti riempie da sola il giorno più vuoto;
## Io vorrei così tanto
## Ridarle il sorriso, sul viso un po' stanco
## 
## Oltre il muro, che cosa c'è
## Trattieni il fiato e poi salta verso me;
## I colpi di fucile sono ormai lontani
## Apriremo il cielo con le mani
## 
## Per Lucia, lunga vita
## E acqua di fiume per ogni ferita;
## Io vorrei luci accese
## La festa più grande per tutto il paese;
## Per Lucia, spaventata
## Che questa mia penna diventi una spada;
## Con Lucia al mio fianco
## Che la storia continui nella pagina accanto
```

## Translation


``` r
songs %>% 
  sample_n(5) %>% 
  pull(translation) %>% 
  cat(sep = "\n\n\n")
```

```
## When there is nothing else, I think that
## I still have a song I can sing to you
## About happiness we feel, about a road that goes
## Like a train on metal rails To faraway stations on the North Pole
## Where freedom lives, and nothing else
## Each time when I feel miracles in the distance
## I want to be closer to you Nothing, when there is nothing else
## I want to sing you a song about happiness
## That comes, that comes unexpectedly
## And takes us by the hand
## A song about happiness to come When there is nothing else, I think that
## I still have the wind that runs up to the sky
## And the sun that rises, faraway roads that cross
## Seven mountains and nine seas To warmer lands like birds of passage
## There everything is just like in a dream, but you are not there
## Each time when I feel miracles in the distance
## I want to be closer to you Nothing, when there is nothing else
## I want to sing you a song about happiness
## That comes, that comes unexpectedly
## And takes us by the hand
## A song about happiness to come Nothing, when there is nothing else
## I want to sing you a song about happiness
## That comes, that comes unexpectedly
## And takes us by the hand
## A song about happiness to come Nothing, when there is nothing else
## I want to sing you a song about happiness
## That comes, that comes unexpectedly
## And takes us by the hand
## A song to come
## A song about happiness to come
## 
## 
## English
## 
## 
## English
## 
## 
## English
## 
## 
## Sometimes, words are unnecessary
## When it's about talking
## Simply of love
## And I prefer the eloquence of a silence
## To that weary phrase
## That sounds like nothing in my voice. I know that you can believe
## That I lack the interest
## But sometimes the most fervent promises
## Only try to hide that love is dying. Love, love, my love
## You'd have to accept me this way
## If you see that I don't like to talk
## Learn to understand my absence. Love, love, my love
## It's all I can say
## To love is something more than making laugh
## I know I'll fill your life with love.
```

## Place


``` r
songs %>% count(place)
```

```
## # A tibble: 27 x 2
##    place     n
##    <chr> <int>
##  1 -       309
##  2 1        68
##  3 10       66
##  4 11       58
##  5 12       60
##  6 13       70
##  7 14       59
##  8 15       53
##  9 16       56
## 10 17       48
## # i 17 more rows
```


``` r
length(unique(songs$year))
```

```
## [1] 66
```


``` r
songs %>% 
  count(year, place) %>% 
  filter(place == "1") %>% 
  arrange(-n)
```

```
## # A tibble: 65 x 3
##    year  place     n
##    <chr> <chr> <int>
##  1 1969  1         4
##  2 1956  1         1
##  3 1957  1         1
##  4 1958  1         1
##  5 1959  1         1
##  6 1960  1         1
##  7 1961  1         1
##  8 1962  1         1
##  9 1963  1         1
## 10 1964  1         1
## # i 55 more rows
```

According to Wikpedia, there were four winners in 1969:

<https://en.wikipedia.org/wiki/Eurovision_Song_Contest>

> On only one occasion have multiple winners been declared in a single contest: in 1969, four countries finished the contest with an equal number of votes and due to the lack of a tie-break rule at the time, all four countries were declared winners.[7][157] 

# Rule 6: You May Need to Go Back

Our original parse function just grabbed `translation` as is. After looking at `translation`, we can see that this variable says `English` if the original song lyrics are in English. English.

# Rule 7: Create New Columns?

## Decade


``` r
songs
```

```
## # A tibble: 1,644 x 7
##    country        artist                 title    year  lyrics translation place
##    <chr>          <chr>                  <chr>    <chr> <chr>  <chr>       <chr>
##  1 Netherlands    Jetty Paerl            De voge~ 1956  "De v~ "The birds~ -    
##  2 Switzerland    Lys Assia              Das alt~ 1956  "Das ~ "The old c~ -    
##  3 Belgium        Fud Leclerc            Messieu~ 1956  "Mess~ "Ye drowne~ -    
##  4 Germany (West) Walter Andreas Schwarz Im Wart~ 1956  "Es g~ "There is ~ -    
##  5 France         Mathé Altéry           Le temp~ 1956  "Chan~ "Sing, car~ -    
##  6 Luxembourg     Michèle Arnaud         Ne croi~ 1956  "Si o~ "If they t~ -    
##  7 Italy          Franca Raimondi        Aprite ~ 1956  "La p~ "The first~ -    
##  8 Netherlands    Corry Brokken          Voorgoe~ 1956  "Voor~ "Over for ~ -    
##  9 Switzerland    Lys Assia (2)          Refrain  1956  "[Int~ "Chorus of~ 1    
## 10 Belgium        Mony Marc              Le plus~ 1956  "Les ~ "The bells~ -    
## # i 1,634 more rows
```

``` r
songs <- 
  songs %>% 
  mutate(
    year = parse_number(year), 
    decade = year - year %% 10
  )
songs %>% 
  ggplot(aes(year, decade)) + 
  geom_point()
```



\begin{center}\includegraphics[width=0.7\linewidth]{DS_CS02_Data_Cleaning_Eurovision_files/figure-latex/unnamed-chunk-18-1} \end{center}

## Lyrics


``` r
songs %>% 
  count(translation) %>%
  arrange(-n)
```

```
## # A tibble: 946 x 2
##    translation                                                                 n
##    <chr>                                                                   <int>
##  1 "English"                                                                 694
##  2 ""                                                                          2
##  3 "Day is turning to evening, is gone, soon forgotten\nBut what we gave ~     2
##  4 "God, god, god, god they don’t call you\nCandles, candles, candles, ca~     2
##  5 "Look into my eyes now,\nI'm not a looser, know it!\nI don't surrender~     2
##  6 "Only champions!\nOnly champions! They fly high1 and jump far2.\nAll t~     2
##  7 "\nPure and white, her skin is so inviting\nLike a new, unbeaten snow\~     1
##  8 "\"Come with me\", said my friend\nThe first silent winter snow is fal~     1
##  9 "\"This is the Telephone Information Service, hello\"\n\"Yes, hello, t~     1
## 10 "\"We are all moving around the old circle\"1,\nSinging in this sweet ~     1
## # i 936 more rows
```


``` r
songs <- 
  songs %>% 
  mutate(
    english_lyrics = ifelse(
      translation == "English",
      lyrics, translation
    ) 
  )
```

# Rule 7: Save the Data and Write it Up

It's critically important to make detailed notes about any changes you have made when cleaning the data.


``` r
filenamepath <- glue::glue("./data/{lubridate::today()}-eurovision.rds")
filenamepath
```

```
## ./data/2025-02-17-eurovision.rds
```

``` r
write_rds(songs,filenamepath)
```
