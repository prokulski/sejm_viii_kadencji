# Głosowania w Sejmie RP VIII kadencji

Pliki stanowią repozytorium służące do przygotowania stosownego wpisu na blogu http://prokulski.science (post w produkcji).


## SKRYPTY

### grab_glosowania.R

Skrypt zawiera funkcje do pobrania listy:

* posiedzeń Sejmu - **wez_liste_posiedzen()**
* głosowań w ramach każdego z posiedzeń - **wez_liste_glosowan()**
* listy klubów biorących udział w głosowaniu - **wez_liste_klubow()**
* indywidualnych wyników głosowań (jak głosował każdy z posłów) danego klubu poselskiego - - **wez_glosowanie_klubu()**

W wyniku działania skryptu powstają pliki RDS z danymi.

**UWAGA** skrypt działą bardzo długo (mamy ponad 8 tysięcy głosowań w całej kadencji!), w dodatku lubi się wywalić (głównie ze względu na *przytykanie się* stron Sejmu) i nie na każdym głosowaniu zadziała. Trzeba doglądać.


### preproces_data.R

Delikatne przetworzenie zebranych danych - dodanie numeru głosowania itp. Zapomniałem o tym przy pisaniu skryptu zbierającego :(


### analiza.R

Wykresy i inne rzeczy użyte do przygotowania wpisu na blogu.



## DANE

### final_lista_glosowan.RDS

Plik z zebraną listą głosowań (bez wyniku): Zawartość:

```
> lista_glosowan <- readRDS("final_lista_glosowan.RDS")
> glimpse(lista_glosowan)

Observations: 8,142
Variables: 8
$ numer_posiedzenia <chr> "86", "86", "86", "86", "86", "86", "86", "86", "86", "86", "86", "86", "86", "86", "86", "86",…
$ numer_glosowania  <chr> "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "1…
$ data_posiedzenia  <date> 2019-09-11, 2019-09-11, 2019-09-11, 2019-09-11, 2019-09-11, 2019-09-11, 2019-09-11, 2019-09-11…
$ czas              <chr> "09:23:50", "21:06:18", "21:06:49", "21:07:16", "21:07:58", "21:08:48", "21:09:27", "21:10:05",…
$ godzina           <dbl> 9, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, …
$ temat             <chr> "86. posiedzenie Sejmu Rzeczypospolitej Polskiej w dniach 11 września oraz 15 i 16 października…
$ link              <chr> "http://sejm.gov.pl/Sejm8.nsf/agent.xsp?symbol=glosowania&NrKadencji=8&NrPosiedzenia=86&NrGloso…
$ id_glosowania     <dbl> 51862, 51863, 51864, 51865, 51866, 51867, 51868, 51869, 51870, 51871, 51872, 51873, 51874, 5187…
```


### final_glosowania.RDS

Plik ze wszystkimi zebranymi danymi o głosowaniach konkretnych osób. Zawartość:

```
> glosowania <- readRDS("final_glosowania.RDS")
> glimpse(glosowania)

Observations: 3,730,368
Variables: 4
$ id_glosowania <dbl> 51862, 51862, 51862, 51862, 51862, 51862, 51862, 51862, 51862, 51862, 51862, 51862, 51862, 51862, 5…
$ klub          <chr> "PiS", "PiS", "PiS", "PiS", "PiS", "PiS", "PiS", "PiS", "PiS", "PiS", "PiS", "PiS", "PiS", "PiS", "…
$ osoba         <chr> "Adamczyk Andrzej", "Andzel Waldemar", "Arciszewska-Mielewczyk Dorota", "Ardanowski Jan Krzysztof",…
$ glos          <chr> "Przeciw", "Nieobecny", "Przeciw", "Nieobecny", "Przeciw", "Przeciw", "Przeciw", "Przeciw", "Nieobe…
```
