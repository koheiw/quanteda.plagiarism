
# quanteda.plagiarism

A tool to detect text segments that are very similar to other segments.
Useful in studying flows of information or detecting plagiarism. This is
an example of quanteda extension package for ECPR summer school.

``` r
install.packages("devtools")
devtools::install_github("koheiw/quanteda.plagiarism")
```

``` r
require(quanteda)
require(quanteda.plagiarism)

quanteda_options(threads = 8)
corp <- data_corpus_inaugural
toks <- tokens(corp, remove_punct = TRUE) %>%
    tokens_remove(stopwords("en")) %>%
    tokens_select("^[\\p{L}]+$", valuetype = "regex")
```

In this example, `textstat_plagiarism()` detects 5-word segments of
texts in the modern US presidents’ speeches, `tail(toks, 20)`, that are
similarit (cosine \>= 0.7) to ealier presidents’ speeches,
`head(toks, 20)`. It seems that Jefferson’s 1801 speech inspired many of
the modern presidents (or their speech writers).

``` r
tstat_plag <- textstat_plagiarism(
    tail(toks, 20),
    head(toks, 20),
    size = 5,
    min_simil = 0.7
)
mat <- as.matrix(tstat_plag, upper = FALSE)
print(mat)
```

    ## 20 x 20 sparse Matrix of class "dgCMatrix"

    ##    [[ suppressing 20 column names '1789-Washington', '1793-Washington', '1797-Adams' ... ]]

    ##                                                          
    ## 1941-Roosevelt  35 . .  . . . . . . . . . . . . . . . . .
    ## 1945-Roosevelt   . . .  . . . . . . . . . . . . . . . . .
    ## 1949-Truman      . . .  . . . . . . . . . . . . . . . . .
    ## 1953-Eisenhower  . . .  . . . . . . . . . . . . . . . . .
    ## 1957-Eisenhower  . . .  . . . . . . . . . . . . . . . . .
    ## 1961-Kennedy     . . .  . . . . . . . . . . . . . . . . .
    ## 1965-Johnson     . . .  . . . . . . . . . . . . . . . . .
    ## 1969-Nixon       . . .  . . . . . . . . . . . . . . . . .
    ## 1973-Nixon       . . .  6 . . . . . . . . . . . . . . . .
    ## 1977-Carter      . . .  2 . . . . . . . . . . . . . . . .
    ## 1981-Reagan      . . .  . . . . . . . . . . . . . . . . .
    ## 1985-Reagan      . . .  . . . . . . . . . . . . . . . . .
    ## 1989-Bush        . . .  . . . . . . . . . . . . . . . . .
    ## 1993-Clinton     . . .  . 3 . . . . . . . . . . . . . . .
    ## 1997-Clinton     . . 2  8 . . . . . . . . . . . . . . . .
    ## 2001-Bush        . . .  2 . . . . . . . . . . . . . . . .
    ## 2005-Bush        . . .  . . . . . . . . . . . . . . . . .
    ## 2009-Obama       . . .  6 . . . . . . . . . . . . . . . .
    ## 2013-Obama       . . .  1 . . . . . . . . . . . . . . . .
    ## 2017-Trump       . 3 . 12 . . . . . . . . . . 1 . . . . .

``` r
colSums(mat)
```

    ## 1789-Washington 1793-Washington      1797-Adams  1801-Jefferson 
    ##              35               3               2              37 
    ##  1805-Jefferson    1809-Madison    1813-Madison     1817-Monroe 
    ##               3               0               0               0 
    ##     1821-Monroe      1825-Adams    1829-Jackson    1833-Jackson 
    ##               0               0               0               0 
    ##   1837-VanBuren   1841-Harrison       1845-Polk     1849-Taylor 
    ##               0               0               1               0 
    ##     1853-Pierce   1857-Buchanan    1861-Lincoln    1865-Lincoln 
    ##               0               0               0               0

These are segments that are similar to those in ealier speeches.

``` r
as.list(tstat_plag, upper = FALSE) %>% tail(10)
```

    ## tokens from 10 documents.
    ## 1941-Roosevelt.549 :
    ## [1] "experiment" "intrusted"  "hands"      "American"   "people"    
    ## 
    ## 1973-Nixon.501 :
    ## [1] "confidence" "government" "can"        "people"     "can"       
    ## 
    ## 1973-Nixon.503 :
    ## [1] "can"        "people"     "can"        "Government" "must"      
    ## 
    ## 1973-Nixon.539 :
    ## [1] "just"       "can"        "government" "help"       "can"       
    ## 
    ## 1977-Carter.307 :
    ## [1] "Nation" "can"    "strong" "abroad" "strong"
    ## 
    ## 1977-Carter.308 :
    ## [1] "can"    "strong" "abroad" "strong" "home"  
    ## 
    ## 1997-Clinton.685 :
    ## [1] "reach"     "just"      "every"     "classroom" "every"    
    ## 
    ## 1997-Clinton.686 :
    ## [1] "just"      "every"     "classroom" "every"     "library"  
    ## 
    ## 2013-Obama.994 :
    ## [1] "let"     "us"      "answer"  "call"    "history"
    ## 
    ## 2017-Trump.251 :
    ## [1] "right" "stops" "right" "now"   "one"
