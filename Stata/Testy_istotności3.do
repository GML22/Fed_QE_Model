//----------------------------------------------------- Przygotowanie zbioru danych -----------------------------------------------------------------------------------

gen date2 = date(date, "DMY") //zmiana formatu daty z string na data (dodatkowo trzeba w zbiorze zmienic format na %d, zeby poprawinie wyswietlal daty)
drop date
rename date2 date
order date, first
//---------------------------------------------------------------------------------------------------------------------------------------------------------------------

/* przygotowanie macierzy do zbadania istotosci komunikatow Fedu*/

// Komentarz 1: 25.11.2008
mkmat yields if (i >= 205 & i < 226), matrix(preKom1)
mkmat yields if (i >= 226 & i < 247), matrix(postKom1)

// Komentarz 2: 18.03.2009
mkmat yields if (i >= 281 & i < 302), matrix(preKom2)
mkmat yields if (i >= 302 & i < 323), matrix(postKom2)

// Komentarz 3: 27.08.2010
mkmat yields if (i >= 645 & i < 666), matrix(preKom3)
mkmat yields if (i >= 666 & i < 687), matrix(postKom3)

// Komentarz 4: 03.11.2010
mkmat yields if (i >= 691 & i < 712), matrix(preKom4)
mkmat yields if (i >= 712 & i < 733), matrix(postKom4)

// Komentarz 5: 21.09.2011
mkmat yields if (i >= 912 & i < 933), matrix(preKom5)
mkmat yields if (i >= 933 & i < 954), matrix(postKom5)

// Komentarz 6: 13.09.2012
mkmat yields if (i >= 1158 & i < 1179), matrix(preKom6)
mkmat yields if (i >= 1179 & i < 1200), matrix(postKom6)

// Komentarz 7: 12.12.2012
mkmat yields if (i >= 1218 & i < 1239), matrix(preKom7)
mkmat yields if (i >= 1239 & i < 1260), matrix(postKom7)

// Komentarz 8: 22.05.2013
mkmat yields if (i >= 1328 & i < 1349), matrix(preKom8)
mkmat yields if (i >= 1349 & i < 1370), matrix(postKom8)

// Komentarz 9: 18.12.2013
mkmat yields if (i >= 1472 & i < 1493), matrix(preKom9)
mkmat yields if (i >= 1493 & i < 1514), matrix(postKom9)

// Komentarz 10: 29.10.2014
mkmat yields if (i >= 1688 & i < 1709), matrix(preKom10)
mkmat yields if (i >= 1709 & i < 1730), matrix(postKom10)


/* -------------------------------------------------------------- Komunikat 1 -------------------------------------------------------------------------------------- */

//czyszczenie bazy i wczytywanie nowych danych
clear 
svmat preKom1
svmat postKom1 

// podstawowe statystyki opisowe
summarize preKom1 postKom1

// histogramy
histogram preKom1, normal name(a1) 
histogram postKom1, normal name(a2)
graph combine a1 a2
 
// testy na normalnosc - test Shapiro - Wilka najlepszy dla malej proby ponizej 50 obserwacji
swilk preKom1
swilk postKom1

// Na 5%-towym poziomie ufnosci test shapiro-wilka wskazuje, iz nie ma podstaw do odrzucenia H0 o normalnosci zmiennych - mozemy posluzyc sie zarowno t-testem jak i Wilcoxenem

// parametryczny test na rownosc srednich w probkach zaleznych
ttest preKom1 = postKom1          /* dependent t-test */

// nieparametryczny test na rownosc srednich w probkach zaleznych
signrank preKom1 = postKom1       /* Wilcoxen signed-rank test */

/* Na 5%-owym poziomie istotnosci oba testy wskazuja, iz nalezy odrzucic H0 o równosci srednich w obu probkach zaleznych - przed i po ogloszeniu pierwszego komunikatu Fedu.
Dodatkowo t-test, wskazuje, iz obserwacje po ogloszeniu komunikatu Fedu maja istotnie nizsza srednia niz te przed */ 

/* ----------------------------------------------------------------------------------------------------------------------------------------------------------------- */

/* -------------------------------------------------------------- Komunikat 2 -------------------------------------------------------------------------------------- */

// czyszczenie bazy i wczytywanie nowych danych
clear
svmat preKom2
svmat postKom2 

// podstawowe statystyki opisowe
summarize preKom2 postKom2

// histogramy
histogram preKom2, normal name(a3) 
histogram postKom2, normal name(a4)
graph combine a3 a4
 
// testy na normalnosc - test Shapiro - Wilka najlepszy dla malej proby ponizej 50 obserwacji
swilk preKom2
swilk postKom2

// Na 5%-towym poziomie ufnosci test shapiro-wilka wskazuje, iz nie ma podstaw do odrzucenia H0 o normalnosci zmiennych - mozemy posluzyc sie zarowno t-testem jak i Wilcoxenem

// parametryczny test na rownosc srednich w probkach zaleznych
ttest preKom2 = postKom2          /* dependent t-test */

// nieparametryczny test na rownosc srednich w probkach zaleznych
signrank preKom2 = postKom2       /* Wilcoxen signed-rank test */

/* Na 5%-owym poziomie istotnosci oba testy wskazuja, iz nalezy odrzucic H0 o równosci srednich w obu probkach zaleznych - przed i po ogloszeniu pierwszego komunikatu Fedu. 
Dodatkowo t-test, wskazuje, iz obserwacje po ogloszeniu komunikatu Fedu maja istotnie nizsza srednia niz te przed */ 

/* ----------------------------------------------------------------------------------------------------------------------------------------------------------------- */

/* -------------------------------------------------------------- Komunikat 3 -------------------------------------------------------------------------------------- */

// czyszczenie bazy i wczytywanie nowych danych
clear
svmat preKom3
svmat postKom3 

// podstawowe statystyki opisowe
summarize preKom3 postKom3

// histogramy
histogram preKom3, normal name(a5) 
histogram postKom3, normal name(a6)
graph combine a5 a6
 
// testy na normalnosc - test Shapiro - Wilka najlepszy dla malej proby ponizej 50 obserwacji
swilk preKom3
swilk postKom3

// Na 5%-towym poziomie ufnosci test shapiro-wilka wskazuje, iz nie ma podstaw do odrzucenia H0 o normalnosci zmiennych - mozemy posluzyc sie zarowno t-testem jak i Wilcoxenem

// parametryczny test na rownosc srednich w probkach zaleznych
ttest preKom3 = postKom3          /* dependent t-test */

// nieparametryczny test na rownosc srednich w probkach zaleznych
signrank preKom3 = postKom3       /* Wilcoxen signed-rank test */

/* Na 5%-owym poziomie istotnosci oba t-test wskazuje, iz nalezy odrzucic H0 o równosci srednich w obu probkach zaleznych, natomiast test Wilcoxena wskazuje na brak podstaw do 
odrzucenia H0. Dodatkowo t-test, wskazuje, iz obserwacje po ogloszeniu komunikatu Fedu maja istotnie nizsza srednia niz te przed.
Jako, iz zmienne maja rozklad normalny zaufamy t-testowi - srednie w probkach nie sa rowne */ 

/* ----------------------------------------------------------------------------------------------------------------------------------------------------------------- */

/* -------------------------------------------------------------- Komunikat 4 -------------------------------------------------------------------------------------- */

// czyszczenie bazy i wczytywanie nowych danych
clear
svmat preKom4
svmat postKom4 

// podstawowe statystyki opisowe
summarize preKom4 postKom4

// histogramy
histogram preKom4, normal name(a7) 
histogram postKom4, normal name(a8)
graph combine a7 a8
 
// testy na normalnosc - test Shapiro - Wilka najlepszy dla malej proby ponizej 50 obserwacji
swilk preKom4
swilk postKom4

// Na 5%-towym poziomie ufnosci test shapiro-wilka wskazuje, iz nie ma podstaw do odrzucenia H0 o normalnosci zmiennych - mozemy posluzyc sie zarowno t-testem jak i Wilcoxenem

// parametryczny test na rownosc srednich w probkach zaleznych
ttest preKom4 = postKom4          /* dependent t-test */

// nieparametryczny test na rownosc srednich w probkach zaleznych
signrank preKom4 = postKom4       /* Wilcoxen signed-rank test */

/* Na 5%-owym poziomie istotnosci oba testy wskazuja, iz nalezy odrzucic H0 o równosci srednich w obu probkach zaleznych - przed i po ogloszeniu pierwszego komunikatu Fedu. 
Dodatkowo t-test, wskazuje, iz obserwacje po ogloszeniu komunikatu Fedu maja istotnie wyzsza srednia niz te przed */ 

/* ----------------------------------------------------------------------------------------------------------------------------------------------------------------- */

/* -------------------------------------------------------------- Komunikat 5 -------------------------------------------------------------------------------------- */

// czyszczenie bazy i wczytywanie nowych danych
clear
svmat preKom5
svmat postKom5 

// podstawowe statystyki opisowe
summarize preKom5 postKom5

// histogramy
histogram preKom5, normal name(a9) 
histogram postKom5, normal name(a10)
graph combine a9 a10
 
// testy na normalnosc - test Shapiro - Wilka najlepszy dla malej proby ponizej 50 obserwacji
swilk preKom5
swilk postKom5

// Na 5%-towym poziomie ufnosci test shapiro-wilka wskazuje, iz nie ma podstaw do odrzucenia H0 o normalnosci zmiennych - mozemy posluzyc sie zarowno t-testem jak i Wilcoxenem

// parametryczny test na rownosc srednich w probkach zaleznych
ttest preKom5 = postKom5          /* dependent t-test */

// nieparametryczny test na rownosc srednich w probkach zaleznych
signrank preKom5 = postKom5       /* Wilcoxen signed-rank test */

/* Na 5%-owym poziomie istotnosci oba testy wskazuja, iz nie ma podstaw do odrzucenia H0 o równosci srednich w obu probkach zaleznych
 - przed i po ogloszeniu pierwszego komunikatu Fedu.*/ 

/* ----------------------------------------------------------------------------------------------------------------------------------------------------------------- */

/* -------------------------------------------------------------- Komunikat 6 -------------------------------------------------------------------------------------- */

// czyszczenie bazy i wczytywanie nowych danych
clear
svmat preKom6
svmat postKom6 

// podstawowe statystyki opisowe
summarize preKom6 postKom6

// histogramy
histogram preKom6, normal name(a11) 
histogram postKom6, normal name(a12)
graph combine a11 a12
 
// testy na normalnosc - test Shapiro - Wilka najlepszy dla malej proby ponizej 50 obserwacji
swilk preKom6
swilk postKom6

// Na 5%-towym poziomie ufnosci test shapiro-wilka wskazuje, iz nie ma podstaw do odrzucenia H0 o normalnosci zmiennych - mozemy posluzyc sie zarowno t-testem jak i Wilcoxenem

// parametryczny test na rownosc srednich w probkach zaleznych
ttest preKom6 = postKom6          /* dependent t-test */

// nieparametryczny test na rownosc srednich w probkach zaleznych
signrank preKom6 = postKom6       /* Wilcoxen signed-rank test */

/* Na 5%-owym poziomie istotnosci oba testy wskazuja, iz nie ma podstaw do odrzucenia H0 o równosci srednich w obu probkach zaleznych
 - przed i po ogloszeniu pierwszego komunikatu Fedu.*/ 

/* ----------------------------------------------------------------------------------------------------------------------------------------------------------------- */

/* -------------------------------------------------------------- Komunikat 7 -------------------------------------------------------------------------------------- */

// czyszczenie bazy i wczytywanie nowych danych
clear
svmat preKom7
svmat postKom7

// podstawowe statystyki opisowe
summarize preKom7 postKom7

// histogramy
histogram preKom7, normal name(a13) 
histogram postKom7, normal name(a14)
graph combine a13 a14
 
// testy na normalnosc - test Shapiro - Wilka najlepszy dla malej proby ponizej 50 obserwacji
swilk preKom7
swilk postKom7

// Na 5%-towym poziomie ufnosci test shapiro-wilka wskazuje, iz nie ma podstaw do odrzucenia H0 o normalnosci zmiennych - mozemy posluzyc sie zarowno t-testem jak i Wilcoxenem

// parametryczny test na rownosc srednich w probkach zaleznych
ttest preKom7 = postKom7         /* dependent t-test */

// nieparametryczny test na rownosc srednich w probkach zaleznych
signrank preKom7 = postKom7       /* Wilcoxen signed-rank test */

/* Na 5%-owym poziomie istotnosci oba testy wskazuja, iz nalezy odrzucic H0 o równosci srednich w obu probkach zaleznych - przed i po ogloszeniu pierwszego komunikatu Fedu. 
Dodatkowo t-test, wskazuje, iz obserwacje po ogloszeniu komunikatu Fedu maja istotnie wyzsza srednia niz te przed */ 

/* ----------------------------------------------------------------------------------------------------------------------------------------------------------------- */

/* -------------------------------------------------------------- Komunikat 8 -------------------------------------------------------------------------------------- */

// czyszczenie bazy i wczytywanie nowych danych
clear
svmat preKom8
svmat postKom8

// podstawowe statystyki opisowe
summarize preKom8 postKom8

// histogramy
histogram preKom8, normal name(a15) 
histogram postKom8, normal name(a16)
graph combine a15 a16
 
// testy na normalnosc - test Shapiro - Wilka najlepszy dla malej proby ponizej 50 obserwacji
swilk preKom8
swilk postKom8

// Na 5%-towym poziomie ufnosci test shapiro-wilka wskazuje, iz nie ma podstaw do odrzucenia H0 o normalnosci zmiennych - mozemy posluzyc sie zarowno t-testem jak i Wilcoxenem

// parametryczny test na rownosc srednich w probkach zaleznych
ttest preKom8 = postKom8         /* dependent t-test */

// nieparametryczny test na rownosc srednich w probkach zaleznych
signrank preKom8 = postKom8       /* Wilcoxen signed-rank test */

/* Na 5%-owym poziomie istotnosci oba testy wskazuja, iz nalezy odrzucic H0 o równosci srednich w obu probkach zaleznych - przed i po ogloszeniu pierwszego komunikatu Fedu. 
Dodatkowo t-test, wskazuje, iz obserwacje po ogloszeniu komunikatu Fedu maja istotnie wyzsza srednia niz te przed */ 

/* ----------------------------------------------------------------------------------------------------------------------------------------------------------------- */

/* -------------------------------------------------------------- Komunikat 9 -------------------------------------------------------------------------------------- */

// czyszczenie bazy i wczytywanie nowych danych
clear
svmat preKom9
svmat postKom9

// podstawowe statystyki opisowe
summarize preKom9 postKom9

// histogramy
histogram preKom9, normal name(a17) 
histogram postKom9, normal name(a18)
graph combine a17 a18
 
// testy na normalnosc - test Shapiro - Wilka najlepszy dla malej proby ponizej 50 obserwacji
swilk preKom9
swilk postKom9

// Na 5%-towym poziomie ufnosci test shapiro-wilka wskazuje, iz nie ma podstaw do odrzucenia H0 o normalnosci zmiennych - mozemy posluzyc sie zarowno t-testem jak i Wilcoxenem

// parametryczny test na rownosc srednich w probkach zaleznych
ttest preKom9 = postKom9         /* dependent t-test */

// nieparametryczny test na rownosc srednich w probkach zaleznych
signrank preKom9 = postKom9       /* Wilcoxen signed-rank test */

/* Na 5%-owym poziomie istotnosci oba testy wskazuja, iz nalezy odrzucic H0 o równosci srednich w obu probkach zaleznych - przed i po ogloszeniu pierwszego komunikatu Fedu. 
Dodatkowo t-test, wskazuje, iz obserwacje po ogloszeniu komunikatu Fedu maja istotnie wyzsza srednia niz te przed */ 
/* ----------------------------------------------------------------------------------------------------------------------------------------------------------------- */

/* -------------------------------------------------------------- Komunikat 10 -------------------------------------------------------------------------------------- */

// czyszczenie bazy i wczytywanie nowych danych
clear
svmat preKom10
svmat postKom10

// podstawowe statystyki opisowe
summarize preKom10 postKom10

// histogramy
histogram preKom10, normal name(a19) 
histogram postKom10, normal name(a20)
graph combine a19 a20
 
// testy na normalnosc - test Shapiro - Wilka najlepszy dla malej proby ponizej 50 obserwacji
swilk preKom10
swilk postKom10

// na 5% poziomie istotnosci zmienna preKom10 ma rozklad normlny, natomiast zmienna postKom10 nie ma - mozemy wiec zastosowac tylko nieparametryczny test Wilcoxena

// nieparametryczny test na rownosc srednich w probkach zaleznych
signrank preKom10 = postKom10       /* Wilcoxen signed-rank test */

/* Na 5%-owym poziomie istotnosci test Wilcoxena wskazuje, iz nie ma podstaw na odrzucenie H0 o rownosci srednich w obu probkach zaleznych 
- przed i po ogloszeniu czwartego komunikatu Fedu */

/* ----------------------------------------------------------------------------------------------------------------------------------------------------------------- */

// Statystycznie istotny wplyw na dzienne zmiany rentownosci amerykanskich obligacji skarbowych okazaly sie miec Komunikaty 1, 2, 3, 4, 5, 8, 9.
