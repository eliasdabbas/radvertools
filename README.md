advertools
================

This package provides a set of tools that help in various online marketing tasks. It provides the following functions / families of functions:

``` r
- word_frequency()
- kw_match_type()
- kw_combinations()
- search_ads_search_engine()
```

Installation
------------

You can install advertools from github with:

``` r
install.packages("devtools")
devtools::install_github("eliasdabbas/advertools")
```

word\_frequency()
-----------------

### A function to extract hidden insights from a long tail of unstructured text

Knowing the frequency of word usage can uncover lots of hidden insights in the long tail of very little used keywords. This is not only for keywords, but can be extremely useful in several other cases:

-   keywords (what you target for your ads)
-   search terms (what people actually search for)
-   URLs that contain page titles
-   article / story / video titles
-   books, movies, songs titles

The first thing is to measure the straightforward frequency of words that occur in the text that we have.

``` r
df <- data.frame(text = c("first word", "second word", "third word"), metric = 1)
df
```

    ##          text metric
    ## 1  first word      1
    ## 2 second word      1
    ## 3  third word      1

Metric here was set to 1 because we are not taking any weights, and simply counting the occurence of each word.

``` r
library(tidyr)
df <- df %>% separate(col = text, into = 1:2, sep = " ", remove = F)
df
```

    ##          text      1    2 metric
    ## 1  first word  first word      1
    ## 2 second word second word      1
    ## 3  third word  third word      1

Now we have the original text, as well as each word separated into separate columns. Now it is easier to count the occurence of each word.

``` r
df <- df %>% gather(order, word, -c(text, metric))
df
```

    ##          text metric order   word
    ## 1  first word      1     1  first
    ## 2 second word      1     1 second
    ## 3  third word      1     1  third
    ## 4  first word      1     2   word
    ## 5 second word      1     2   word
    ## 6  third word      1     2   word

Now, using `dplyr` we can easily `group_by()` word, and `summarise()` by the sum of `metric`.

``` r
library(dplyr)
df <- df %>% 
  group_by(word) %>% 
  summarise(metric = sum(metric, na.rm = T)) %>% 
  arrange(desc(metric))

df
```

    ## Source: local data frame [4 x 2]
    ## 
    ##     word metric
    ##    (chr)  (dbl)
    ## 1   word      3
    ## 2  first      1
    ## 3 second      1
    ## 4  third      1

Now we can see that the word "word" was the most used in the data frame.

Now, to the more interesting case, which is having a metric (weight) for the words that we have. This is where more insights, and hidden information can be uncovered.

``` r
df2 <- data.frame(
  text = c("book name", "book title", "great title", "great name"), 
  metric = c(100, 120, 300, 150))
df2
```

    ##          text metric
    ## 1   book name    100
    ## 2  book title    120
    ## 3 great title    300
    ## 4  great name    150

running the same previous code in one chunk:

``` r
df2 <- df2 %>% separate(col = text, into = 1:2, sep = " ", remove = F) %>% 
  gather(order, word, -c(text, metric)) %>% 
  group_by(word) %>% 
  summarise(metric = sum(metric, na.rm = T)) %>% 
  arrange(desc(metric))
df2
```

    ## Source: local data frame [4 x 2]
    ## 
    ##    word metric
    ##   (chr)  (dbl)
    ## 1 great    450
    ## 2 title    420
    ## 3  name    250
    ## 4  book    220

Here each of the words "book", "name", "title", and "great", were used twice each, but the highest from a weighted perspective was "great". The metric here could be any metric of choice; book sales, keyword impressions, movie sales, etc.

The function `word_frequency()` does all this automatically in one step, and in addition compares the original metric to the weighted one, and shows some additional stats.

We can demonstrate this with the top ten movies by gross sales. In this case we want to see which words appear the most, takingn into consideration the sales (weight), and not only the absolute frequencies.

| text                                     | metric    |
|------------------------------------------|-----------|
| Star Wars The Force Awakens              | 935254389 |
| Avatar                                   | 760507625 |
| Titanic                                  | 658672302 |
| Jurassic World                           | 652270625 |
| Marvels The Avengers                     | 623357910 |
| The Dark Knight                          | 534858444 |
| Star Wars Episode I - The Phantom Menace | 474544677 |
| Star Wars                                | 460998007 |
| Avengers Age of Ultron                   | 459005868 |
| The Dark Knight Rises                    | 448139099 |

\*source: boxofficemojo.com

``` r
movies <- data.frame(
  text = c("Star Wars The Force Awakens", "Avatar", "Titanic", "Jurassic World", "Marvels The Avengers", "The Dark Knight", "Star Wars Episode I - The Phantom Menace", "Star Wars", "Avengers Age of Ultron", "The Dark Knight Rises"),
  metric = c(935254389, 760507625, 658672302, 652270625, 623357910, 534858444, 474544677, 460998007, 459005868, 448139099)
)
movies
```

    ##                                        text    metric
    ## 1               Star Wars The Force Awakens 935254389
    ## 2                                    Avatar 760507625
    ## 3                                   Titanic 658672302
    ## 4                            Jurassic World 652270625
    ## 5                      Marvels The Avengers 623357910
    ## 6                           The Dark Knight 534858444
    ## 7  Star Wars Episode I - The Phantom Menace 474544677
    ## 8                                 Star Wars 460998007
    ## 9                    Avengers Age of Ultron 459005868
    ## 10                    The Dark Knight Rises 448139099

``` r
library(advertools)
word_frequency(movies)
```

    ## Warning: Too few values at 9 locations: 1, 2, 3, 4, 5, 6, 8, 9, 10

``` r
data.frame(textdf)
```

    ##        word abs_freq   wtd_freq cum_perc  perc
    ## 1       the        5 3016154519     0.16 0.157
    ## 2      star        3 1870797073     0.25 0.097
    ## 3      wars        3 1870797073     0.35 0.097
    ## 4  avengers        2 1082363778     0.41 0.056
    ## 5      dark        2  982997543     0.46 0.051
    ## 6    knight        2  982997543     0.51 0.051
    ## 7   awakens        1  935254389     0.56 0.049
    ## 8     force        1  935254389     0.61 0.049
    ## 9    avatar        1  760507625     0.65 0.040
    ## 10  titanic        1  658672302     0.68 0.034
    ## 11 jurassic        1  652270625     0.72 0.034
    ## 12    world        1  652270625     0.75 0.034
    ## 13  marvels        1  623357910     0.78 0.032
    ## 14        -        1  474544677     0.81 0.025
    ## 15  episode        1  474544677     0.83 0.025
    ## 16        i        1  474544677     0.86 0.025
    ## 17   menace        1  474544677     0.88 0.025
    ## 18  phantom        1  474544677     0.91 0.025
    ## 19      age        1  459005868     0.93 0.024
    ## 20       of        1  459005868     0.95 0.024
    ## 21   ultron        1  459005868     0.98 0.024
    ## 22    rises        1  448139099     1.00 0.023
    ##                                        text original_metric
    ## 1               Star Wars The Force Awakens       935254389
    ## 2                                    Avatar       760507625
    ## 3                                   Titanic       658672302
    ## 4                            Jurassic World       652270625
    ## 5                      Marvels The Avengers       623357910
    ## 6                           The Dark Knight       534858444
    ## 7  Star Wars Episode I - The Phantom Menace       474544677
    ## 8                                 Star Wars       460998007
    ## 9                    Avengers Age of Ultron       459005868
    ## 10                    The Dark Knight Rises       448139099
    ## 11                                     <NA>              NA
    ## 12                                     <NA>              NA
    ## 13                                     <NA>              NA
    ## 14                                     <NA>              NA
    ## 15                                     <NA>              NA
    ## 16                                     <NA>              NA
    ## 17                                     <NA>              NA
    ## 18                                     <NA>              NA
    ## 19                                     <NA>              NA
    ## 20                                     <NA>              NA
    ## 21                                     <NA>              NA
    ## 22                                     <NA>              NA

### word:

This column shows the top used words, sorted by descending order of the weight metric (whatever the metric is). In this particular case, The word "the" appears in movies that generated a total of $3,016,154,519.

### abs\_freq:

The absolute frequency of this word.

### wtd\_freq:

Weighted frequency.

### cum\_perc:

Cumulative percentage (based on the weighted frequency). 6 words appear in movies that generated 51% of the total revenues.

### perc:

Percentage for each word.

### text:

This is the original text, which is the movie title in this case.

### original\_metric:

In this case the gross revenue.

Although Avatar is the second movie, the word 'avatar' is at number 9 when we take into consideration all the words in the list and take the weights of each. Titanic is also interesting, in that it is at number 3 in the original list, but at number 10 when we take the weights.

This is only for ten movies, and things get much more interesting if we take a larger list of, say 10k movies. Then we are dealing with a much longer tail of words, and some differences are very interesting to observe.

kw\_match\_type()
-----------------

Efficiently manage the different keyword match types in R with advertools
-------------------------------------------------------------------------

### Keyword Match Types

When working with keywords, you don't only include the keywords as they are, you need to specify the match type of the keyword. The way to do this is to simply wrap the text with one of the specified punctuation marks to indicate which match type you want to use:

-   **Broad Match** no punctuation, and your ads will be triggered by similar keywords, synonyms, or the words in a different order.
-   **+Modified +Broad +Match** a plus sign next to any word that you want to trigger close variations to it. Plurals, singulars, and derivatives are included but not synonyms.

-   **"Phrase Match"** quotation marks, indicate that you want your ads to be triggered by the exact string of characters within the quotation marks, with anything before or after them.
-   **\[Exact Match\]** sqare brackets mean that you want ads to be triggered by the exact same character string within the brackets, nothing before and nothing after. In AdWords though, there is an option where you allow close variations, which means plurals, and some typos. In general though you are targeting with close to 100% precision with exact match.
-   **-negative match** a minus sign before the keyword excludes any keywords that include it from triggering your ads. This is extremely helpful to control what is shown when you are using anything other than phrase match, as things can get out of control without negative match.

Negative match can also be combined with phrase and exact, for more refined control.

All the keyword match type functions start with `kw_` and then you specify the match type.

There is nothing sophisticated about these functions. They just save you some typing time, make sure you don't introduce any typos. Ideally, the proper way to do this with big campaigns is to use specieal software like AdWords Editor, but with very big campaigns, it gets really slow and might take hours. In this case I prefer using the web interface, which allows uploading of csv files, and in these files, the keyword match type needs to specified as shown above.

``` r
library(advertools)
carnames <- data.frame(broad = rownames(mtcars))
carnames$modifed <- kw_modified_broad(carnames$broad) 
carnames$phrase <- kw_phrase(carnames$broad)
carnames$exact <- kw_exact(carnames$phrase)
carnames$negative <- kw_negative(carnames$exact)
carnames[1:10, ]
```

    ##                broad             modifed              phrase
    ## 1          Mazda RX4         +Mazda +RX4         "Mazda RX4"
    ## 2      Mazda RX4 Wag    +Mazda +RX4 +Wag     "Mazda RX4 Wag"
    ## 3         Datsun 710        +Datsun +710        "Datsun 710"
    ## 4     Hornet 4 Drive   +Hornet +4 +Drive    "Hornet 4 Drive"
    ## 5  Hornet Sportabout +Hornet +Sportabout "Hornet Sportabout"
    ## 6            Valiant            +Valiant           "Valiant"
    ## 7         Duster 360        +Duster +360        "Duster 360"
    ## 8          Merc 240D         +Merc +240D         "Merc 240D"
    ## 9           Merc 230          +Merc +230          "Merc 230"
    ## 10          Merc 280          +Merc +280          "Merc 280"
    ##                  exact           negative
    ## 1          [Mazda RX4]         -Mazda RX4
    ## 2      [Mazda RX4 Wag]     -Mazda RX4 Wag
    ## 3         [Datsun 710]        -Datsun 710
    ## 4     [Hornet 4 Drive]    -Hornet 4 Drive
    ## 5  [Hornet Sportabout] -Hornet Sportabout
    ## 6            [Valiant]           -Valiant
    ## 7         [Duster 360]        -Duster 360
    ## 8          [Merc 240D]         -Merc 240D
    ## 9           [Merc 230]          -Merc 230
    ## 10          [Merc 280]          -Merc 280

Notice that on the fourth and fifth commands I am calling the function on different match types. This is to show that all the functions first remove any previous punctuation and then add the new ones.

If you had and exact match \[keyword\] and then changed your mind to make it phrase, and then negative, you will end up with something like -"\[keyword\]" which doesn't make sense. So this is an efficiency advantage of using these functions.
