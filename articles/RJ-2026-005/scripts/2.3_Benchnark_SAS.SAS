/* linear stepwise regression*/
/*import mtcars dataset*/
data mtcars;
 input mpg cyl disp hp drat wt qsec vs am gear carb;
 datalines;
 21 6 160 110 3.9 2.62 16.46 0 1 4 4
 21 6 160 110 3.9 2.875 17.02 0 1 4 4
 22.8 4 108 93 3.85 2.32 18.61 1 1 4 1
 21.4 6 258 110 3.08 3.215 19.44 1 0 3 1
 18.7 8 360 175 3.15 3.44 17.02 0 0 3 2
 18.1 6 225 105 2.76 3.46 20.22 1 0 3 1
 14.3 8 360 245 3.21 3.57 15.84 0 0 3 4
 24.4 4 146.7 62 3.69 3.19 20 1 0 4 2
 22.8 4 140.8 95 3.92 3.15 22.9 1 0 4 2
 19.2 6 167.6 123 3.92 3.44 18.3 1 0 4 4
 17.8 6 167.6 123 3.92 3.44 18.9 1 0 4 4
 16.4 8 275.8 180 3.07 4.07 17.4 0 0 3 3
 17.3 8 275.8 180 3.07 3.73 17.6 0 0 3 3
 15.2 8 275.8 180 3.07 3.78 18 0 0 3 3
 10.4 8 472 205 2.93 5.25 17.98 0 0 3 4
 10.4 8 460 215 3 5.424 17.82 0 0 3 4
 14.7 8 440 230 3.23 5.345 17.42 0 0 3 4
 32.4 4 78.7 66 4.08 2.2 19.47 1 1 4 1
 30.4 4 75.7 52 4.93 1.615 18.52 1 1 4 2
 33.9 4 71.1 65 4.22 1.835 19.9 1 1 4 1
 21.5 4 120.1 97 3.7 2.465 20.01 1 0 3 1
 15.5 8 318 150 2.76 3.52 16.87 0 0 3 2
 15.2 8 304 150 3.15 3.435 17.3 0 0 3 2
 13.3 8 350 245 3.73 3.84 15.41 0 0 3 4
 19.2 8 400 175 3.08 3.845 17.05 0 0 3 2
 27.3 4 79 66 4.08 1.935 18.9 1 1 4 1
 26 4 120.3 91 4.43 2.14 16.7 0 1 5 2
 30.4 4 95.1 113 3.77 1.513 16.9 1 1 5 2
 15.8 8 351 264 4.22 3.17 14.5 0 1 5 4
 19.7 6 145 175 3.62 2.77 15.5 0 1 5 6
 15 8 301 335 3.54 3.57 14.6 0 1 5 8
 21.4 4 121 109 4.11 2.78 18.6 1 1 4 2
 ;
%macro run_glmselect();
 %let selections = forward backward stepwise; 
%let criteria = AIC AICC BIC CP adjRsq SBC SL; 

%do i = 1 %to %sysfunc(countw(&selections)); 
%let method = %scan(&selections, &i); 
%do j = 1 %to %sysfunc(countw(&criteria)); 
%let criterion = %scan(&criteria, &j); 
proc glmselect data=work.mtcars; 
model mpg=cyl disp hp drat wt qsec vs am gear carb / selection=&method select=&criterion; 
run; 
%end; 
%end; 
%mend run_glmselect;
%run_glmselect();

/*logistic stepwise regression*/
/*import remission dataset*/
data remission;
    input remiss cell smear infil li blast temp;
    label remiss='Complete Remission';
    datalines;
    1 .8 .83 .66 1.9 1.1 .996
    1 .9 .36 .32 1.4 .74 .992
    0 .8 .88 .7 .8 .176 .982
    0 1 .87 .87 .7 1.053 .986
    1 .9 .75 .68 1.3 .519 .98
    0 1 .65 .65 .6 .519 .982
    1 .95 .97 .92 1 1.23 .992
    0 .95 .87 .83 1.9 1.354 1.02
    0 1 .45 .45 .8 .322 .999
    0 .95 .36 .34 .5 0 1.038
    0 .85 .39 .33 .7 .279 .988
    0 .7 .76 .53 1.2 .146 .982
    0 .8 .46 .37 .4 .38 1.006
    0 .2 .39 .08 .8 .114 .99
    0 1 .9 .9 1.1 1.037 .99
    1 1 .84 .84 1.9 2.064 1.02
    0 .65 .42 .27 .5 .114 1.014
    0 1 .75 .75 1 1.322 1.004
    0 .5 .44 .22 .6 .114 .99
    1 1 .63 .63 1.1 1.072 .986
    0 1 .33 .33 .4 .176 1.01
    0 .9 .93 .84 .6 1.591 1.02
    1 1 .58 .58 1 .531 1.002
    0 .95 .32 .3 1.6 .886 .988
    1 1 .6 .6 1.7 .964 .99
    1 1 .69 .69 .9 .398 .986
    0 1 .73 .73 .7 .398 .986
    ;
%macro run_logistic(); 
%let selections = forward backward stepwise score; 
%do i = 1 %to %sysfunc(countw(&selections)); 
%let method = %scan(&selections, &i); 
proc logistic data=remission outest=betas covout; 
model remiss(event='1') = cell smear infil li blast temp / selection=&method; 
run; 
%end; 
%mend run_logistic; 
%run_logistic();

/*cox stepwise regression*/
/*import lung dataset*/
data lung;
    input inst time status age sex ph_ecog ph_karno pat_karno meal_cal wt_loss;
    datalines;
3 455 2 68 1 0 90 90 1225 15
5 210 2 57 1 1 90 60 1150 11
12 1022 1 74 1 1 50 80 513 0
7 310 2 68 2 2 70 60 384 10
11 361 2 71 2 2 60 80 538 1
1 218 2 53 1 1 70 80 825 16
7 166 2 61 1 2 70 70 271 34
6 170 2 57 1 1 80 80 1025 27
12 567 2 57 1 1 80 70 2600 60
22 613 2 70 1 1 90 100 1150 -5
16 707 2 63 1 2 50 70 1025 22
1 61 2 56 2 2 60 60 238 10
11 301 2 67 1 1 80 80 1025 17
6 81 2 49 2 0 100 70 1175 -8
15 371 2 58 1 0 90 100 975 13
12 520 2 70 2 1 90 80 825 6
4 574 2 60 1 0 100 100 1025 -13
13 118 2 70 1 3 60 70 1075 20
13 390 2 53 1 1 80 70 875 -7
1 12 2 74 1 2 70 50 305 20
12 473 2 69 2 1 90 90 1025 -1
1 26 2 73 1 2 60 70 388 20
16 107 2 60 2 2 50 60 925 -15
12 53 2 61 1 2 70 100 1075 10
22 814 2 65 1 2 70 60 513 28
15 965 1 66 2 1 70 90 875 4
1 93 2 74 1 2 50 40 1225 24
1 731 2 64 2 1 80 100 1175 15
5 460 2 70 1 1 80 60 975 10
11 153 2 73 2 2 60 70 1075 11
10 433 2 59 2 0 90 90 363 27
7 583 2 68 1 1 60 70 1025 7
7 95 2 76 2 2 60 60 625 -24
1 303 2 74 1 0 90 70 463 30
3 519 2 63 1 1 80 70 1025 10
13 643 2 74 1 0 90 90 1425 2
22 765 2 50 2 1 90 100 1175 4
21 53 2 68 1 0 90 100 1025 0
1 246 2 58 1 0 100 90 1175 7
6 689 2 59 1 1 90 80 1300 15
5 5 2 65 2 0 100 80 338 5
3 687 2 58 2 1 80 80 1225 10
1 345 2 64 2 1 90 80 1075 -3
22 444 2 75 2 2 70 70 438 8
12 223 2 48 1 1 90 80 1300 68
11 60 2 65 2 1 90 80 1025 0
3 163 2 69 1 1 80 60 1125 0
3 65 2 68 1 2 70 50 825 8
5 821 1 64 2 0 90 70 1025 3
22 428 2 68 1 0 100 80 1039 0
6 230 2 67 1 1 80 100 488 23
13 840 1 63 1 0 90 90 1175 -1
3 305 2 48 2 1 80 90 538 29
5 11 2 74 1 2 70 100 1175 0
21 226 2 53 2 1 90 80 825 3
12 426 2 71 2 1 90 90 1075 19
1 705 2 51 2 0 100 80 1300 0
6 363 2 56 2 1 80 70 1225 -2
1 176 2 73 1 0 90 70 169 30
4 791 2 59 1 0 100 80 768 5
13 95 2 55 1 1 70 90 1500 15
11 196 1 42 1 1 80 80 1425 8
21 167 2 44 2 1 80 90 588 -1
16 806 1 44 1 1 80 80 1025 1
6 284 2 71 1 1 80 90 1100 14
22 641 2 62 2 1 80 80 1150 1
21 147 2 61 1 0 100 90 1175 4
13 740 1 44 2 1 90 80 588 39
1 163 2 72 1 2 70 70 910 2
11 655 2 63 1 0 100 90 975 -1
5 88 2 66 1 1 90 80 875 8
10 245 2 57 2 1 80 60 280 14
12 30 2 72 1 2 80 60 288 7
11 477 2 64 1 1 90 100 910 0
1 559 1 58 2 0 100 100 710 15
6 450 2 69 2 1 80 90 1175 3
12 156 2 66 1 1 80 90 875 14
26 529 1 54 2 1 80 100 975 -3
21 429 2 55 1 1 100 80 975 5
3 351 2 75 2 2 60 50 925 11
13 15 2 69 1 0 90 70 575 10
1 181 2 44 1 1 80 90 1175 5
10 283 2 80 1 1 80 100 1030 6
1 13 2 76 1 2 70 70 413 20
3 212 2 49 1 2 70 60 675 20
1 524 2 68 1 2 60 70 1300 30
16 288 2 66 1 2 70 60 613 24
15 363 2 80 1 1 80 90 346 11
26 199 2 60 2 2 70 80 675 10
3 550 2 69 2 1 70 80 910 0
11 54 2 72 1 2 60 60 768 -3
1 558 2 70 1 0 90 90 1025 17
22 207 2 66 1 1 80 80 925 20
7 92 2 50 1 1 80 60 1075 13
12 60 2 64 1 1 80 90 993 0
16 551 1 77 2 2 80 60 750 28
4 293 2 59 2 1 80 80 925 52
6 353 2 47 1 0 100 90 1225 5
1 267 2 67 1 0 90 70 313 6
22 511 1 74 2 2 60 40 96 37
1 457 2 54 1 1 90 90 975 -5
5 337 2 56 1 0 100 100 1500 15
21 201 2 73 2 2 70 60 1225 -16
3 404 1 74 1 1 80 70 413 38
26 222 2 76 1 2 70 70 1500 8
1 62 2 65 2 1 80 90 1075 0
11 458 1 57 1 1 80 100 513 30
16 353 2 71 1 0 100 80 775 2
16 163 2 54 1 1 90 80 1225 13
12 31 2 82 1 0 100 90 413 27
13 229 2 70 1 1 70 60 1175 -2
32 156 2 55 1 2 70 30 1025 10
4 291 2 62 1 2 70 60 475 27
12 179 2 63 1 1 80 70 538 -2
1 376 1 56 2 1 80 90 825 17
32 384 1 62 2 0 90 90 588 8
10 268 2 44 2 1 90 100 2450 2
11 292 1 69 1 2 60 70 2450 36
6 142 2 63 1 1 90 80 875 2
7 413 1 64 1 1 80 70 413 16
16 266 1 57 2 0 90 90 1075 3
21 320 2 46 1 0 100 100 860 4
6 181 2 61 1 1 90 90 730 0
12 285 2 65 1 0 100 90 1025 0
13 301 1 61 1 1 90 100 825 2
2 348 2 58 2 0 90 80 1225 10
2 197 2 56 1 1 90 60 768 37
16 382 1 43 2 0 100 90 338 6
1 303 1 53 1 1 90 80 1225 12
13 296 1 59 2 1 80 100 1025 0
1 180 2 56 1 2 60 80 1225 -2
1 145 2 53 2 1 80 90 588 13
7 269 1 74 2 0 100 100 588 0
13 300 1 60 1 0 100 100 975 5
1 284 1 39 1 0 100 90 1225 -5
12 292 1 51 2 0 90 80 1225 0
12 332 1 45 2 0 90 100 975 5
2 285 2 72 2 2 70 90 463 20
3 259 1 58 1 0 90 80 1300 8
15 110 2 64 1 1 80 60 1025 12
22 286 2 53 1 0 90 90 1225 8
16 270 2 72 1 1 80 90 488 14
1 225 1 64 1 1 90 80 825 33
22 269 2 71 1 1 90 90 1300 -2
12 225 1 70 1 0 100 100 1175 6
32 243 1 63 2 1 80 90 825 0
1 276 1 52 2 0 100 80 975 0
32 135 2 60 1 1 90 70 1275 0
15 79 2 64 2 1 90 90 488 37
22 59 2 73 1 1 60 60 2200 5
32 240 1 63 2 0 90 100 1025 0
3 202 1 50 2 0 100 100 635 1
26 235 1 63 2 0 100 90 413 0
13 239 2 50 2 2 60 60 1025 -3
1 252 1 60 2 0 100 90 488 -2
6 221 1 67 1 1 80 70 413 23
15 185 1 69 1 1 90 70 1075 0
11 222 1 65 1 1 90 70 1025 18
21 183 2 76 1 2 80 60 825 7
11 211 1 70 2 2 70 30 131 3
2 175 1 57 2 0 80 80 725 11
22 197 1 67 1 1 80 90 1500 2
11 203 1 71 2 1 80 90 1025 0
13 191 1 39 1 0 90 90 2350 -5
32 105 1 75 2 2 60 70 1025 5
6 174 1 66 1 1 90 100 1075 1
22 177 1 58 2 1 80 90 1060 0
;
%macro run_phreg(); 
%let selections = forward backward stepwise score; 
%do i = 1 %to %sysfunc(countw(&selections)); 
%let method = %scan(&selections, &i); 
proc phreg data=lung; 
class sex;
model time*status(1)=age inst sex ph_ecog ph_karno pat_karno meal_cal wt_loss / selection=&method; 
run;
%end; 
%mend run_phreg; 
%run_phreg();


/* Poisson stepwise regression*/
/*import Affairs dataset*/
data Affairs;
 input affairs gender $ age yearsmarried children $ religiousness education occupation rating;
 datalines;
0 male 37 10 no 3 18 7 4
0 female 27 4 no 4 14 6 4
0 female 32 15 yes 1 12 1 4
0 male 57 15 yes 5 18 6 5
0 male 22 0.75 no 2 17 6 3
0 female 32 1.5 no 2 17 5 5
0 female 22 0.75 no 2 12 1 3
0 male 57 15 yes 2 14 4 4
0 female 32 15 yes 4 16 1 2
0 male 22 1.5 no 4 14 4 5
0 male 37 15 yes 2 20 7 2
0 male 27 4 yes 4 18 6 4
0 male 47 15 yes 5 17 6 4
0 female 22 1.5 no 2 17 5 4
0 female 27 4 no 4 14 5 4
0 female 37 15 yes 1 17 5 5
0 female 37 15 yes 2 18 4 3
0 female 22 0.75 no 3 16 5 4
0 female 22 1.5 no 2 16 5 5
0 female 27 10 yes 2 14 1 5
0 female 22 1.5 no 2 16 5 5
0 female 22 1.5 no 2 16 5 5
0 female 27 10 yes 4 16 5 4
0 female 32 10 yes 3 14 1 5
0 male 37 4 yes 2 20 6 4
0 female 22 1.5 no 2 18 5 5
0 female 27 7 no 4 16 1 5
0 male 42 15 yes 5 20 6 4
0 male 27 4 yes 3 16 5 5
0 female 27 4 yes 3 17 5 4
0 male 42 15 yes 4 20 6 3
0 female 22 1.5 no 3 16 5 5
0 male 27 0.417 no 4 17 6 4
0 female 42 15 yes 5 14 5 4
0 male 32 4 yes 1 18 6 4
0 female 22 1.5 no 4 16 5 3
0 female 42 15 yes 3 12 1 4
0 female 22 4 no 4 17 5 5
0 male 22 1.5 yes 1 14 3 5
0 female 22 0.75 no 3 16 1 5
0 male 32 10 yes 5 20 6 5
0 male 52 15 yes 5 18 6 3
0 female 22 0.417 no 5 14 1 4
0 female 27 4 yes 2 18 6 1
0 female 32 7 yes 5 17 5 3
0 male 22 4 no 3 16 5 5
0 female 27 7 yes 4 18 6 5
0 female 42 15 yes 2 18 5 4
0 male 27 1.5 yes 4 16 3 5
0 male 42 15 yes 2 20 6 4
0 female 22 0.75 no 5 14 3 5
0 male 32 7 yes 2 20 6 4
0 male 27 4 yes 5 20 6 5
0 male 27 10 yes 4 20 6 4
0 male 22 4 no 1 18 5 5
0 female 37 15 yes 4 14 3 1
0 male 22 1.5 yes 5 16 4 4
0 female 37 15 yes 4 17 1 5
0 female 27 0.75 no 4 17 5 4
0 male 32 10 yes 4 20 6 4
0 female 47 15 yes 5 14 7 2
0 male 37 10 yes 3 20 6 4
0 female 22 0.75 no 2 16 5 5
0 male 27 4 no 2 18 4 5
0 male 32 7 no 4 20 6 4
0 male 42 15 yes 2 17 3 5
0 male 37 10 yes 4 20 6 4
0 female 47 15 yes 3 17 6 5
0 female 22 1.5 no 5 16 5 5
0 female 27 1.5 no 2 16 6 4
0 female 27 4 no 3 17 5 5
0 female 32 10 yes 5 14 4 5
0 female 22 0.125 no 2 12 5 5
0 male 47 15 yes 4 14 4 3
0 male 32 15 yes 1 14 5 5
0 male 27 7 yes 4 16 5 5
0 female 22 1.5 yes 3 16 5 5
0 male 27 4 yes 3 17 6 5
0 female 22 1.5 no 3 16 5 5
0 male 57 15 yes 2 14 7 2
0 male 17.5 1.5 yes 3 18 6 5
0 male 57 15 yes 4 20 6 5
0 female 22 0.75 no 2 16 3 4
0 male 42 4 no 4 17 3 3
0 female 22 1.5 yes 4 12 1 5
0 female 22 0.417 no 1 17 6 4
0 female 32 15 yes 4 17 5 5
0 female 27 1.5 no 3 18 5 2
0 female 22 1.5 yes 3 14 1 5
0 female 37 15 yes 3 14 1 4
0 female 32 15 yes 4 14 3 4
0 male 37 10 yes 2 14 5 3
0 male 37 10 yes 4 16 5 4
0 male 57 15 yes 5 20 5 3
0 male 27 0.417 no 1 16 3 4
0 female 42 15 yes 5 14 1 5
0 male 57 15 yes 3 16 6 1
0 male 37 10 yes 1 16 6 4
0 male 37 15 yes 3 17 5 5
0 male 37 15 yes 4 20 6 5
0 female 27 10 yes 5 14 1 5
0 male 37 10 yes 2 18 6 4
0 female 22 0.125 no 4 12 4 5
0 male 57 15 yes 5 20 6 5
0 female 37 15 yes 4 18 6 4
0 male 22 4 yes 4 14 6 4
0 male 27 7 yes 4 18 5 4
0 male 57 15 yes 4 20 5 4
0 male 32 15 yes 3 14 6 3
0 female 22 1.5 no 2 14 5 4
0 female 32 7 yes 4 17 1 5
0 female 37 15 yes 4 17 6 5
0 female 32 1.5 no 5 18 5 5
0 male 42 10 yes 5 20 7 4
0 female 27 7 no 3 16 5 4
0 male 37 15 no 4 20 6 5
0 male 37 15 yes 4 14 3 2
0 male 32 10 no 5 18 6 4
0 female 22 0.75 no 4 16 1 5
0 female 27 7 yes 4 12 2 4
0 female 27 7 yes 2 16 2 5
0 female 42 15 yes 5 18 5 4
0 male 42 15 yes 4 17 5 3
0 female 27 7 yes 2 16 1 2
0 female 22 1.5 no 3 16 5 5
0 male 37 15 yes 5 20 6 5
0 female 22 0.125 no 2 14 4 5
0 male 27 1.5 no 4 16 5 5
0 male 32 1.5 no 2 18 6 5
0 male 27 1.5 no 2 17 6 5
0 female 27 10 yes 4 16 1 3
0 male 42 15 yes 4 18 6 5
0 female 27 1.5 no 2 16 6 5
0 male 27 4 no 2 18 6 3
0 female 32 10 yes 3 14 5 3
0 female 32 15 yes 3 18 5 4
0 female 22 0.75 no 2 18 6 5
0 female 37 15 yes 2 16 1 4
0 male 27 4 yes 4 20 5 5
0 male 27 4 no 1 20 5 4
0 female 27 10 yes 2 12 1 4
0 female 32 15 yes 5 18 6 4
0 male 27 7 yes 5 12 5 3
0 male 52 15 yes 2 18 5 4
0 male 27 4 no 3 20 6 3
0 male 37 4 yes 1 18 5 4
0 male 27 4 yes 4 14 5 4
0 female 52 15 yes 5 12 1 3
0 female 57 15 yes 4 16 6 4
0 male 27 7 yes 1 16 5 4
0 male 37 7 yes 4 20 6 3
0 male 22 0.75 no 2 14 4 3
0 male 32 4 yes 2 18 5 3
0 male 37 15 yes 4 20 6 3
0 male 22 0.75 yes 2 14 4 3
0 male 42 15 yes 4 20 6 3
0 female 52 15 yes 5 17 1 1
0 female 37 15 yes 4 14 1 2
0 male 27 7 yes 4 14 5 3
0 male 32 4 yes 2 16 5 5
0 female 27 4 yes 2 18 6 5
0 female 27 4 yes 2 18 5 5
0 male 37 15 yes 5 18 6 5
0 female 47 15 yes 5 12 5 4
0 female 32 10 yes 3 17 1 4
0 female 27 1.5 yes 4 17 1 2
0 female 57 15 yes 2 18 5 2
0 female 22 1.5 no 4 14 5 4
0 male 42 15 yes 3 14 3 4
0 male 57 15 yes 4 9 2 2
0 male 57 15 yes 4 20 6 5
0 female 22 0.125 no 4 14 4 5
0 female 32 10 yes 4 14 1 5
0 female 42 15 yes 3 18 5 4
0 female 27 1.5 no 2 18 6 5
0 male 32 0.125 yes 2 18 5 2
0 female 27 4 no 3 16 5 4
0 female 27 10 yes 2 16 1 4
0 female 32 7 yes 4 16 1 3
0 female 37 15 yes 4 14 5 4
0 female 42 15 yes 5 17 6 2
0 male 32 1.5 yes 4 14 6 5
0 female 32 4 yes 3 17 5 3
0 female 37 7 no 4 18 5 5
0 female 22 0.417 yes 3 14 3 5
0 female 27 7 yes 4 14 1 5
0 male 27 0.75 no 3 16 5 5
0 male 27 4 yes 2 20 5 5
0 male 32 10 yes 4 16 4 5
0 male 32 15 yes 1 14 5 5
0 male 22 0.75 no 3 17 4 5
0 female 27 7 yes 4 17 1 4
0 male 27 0.417 yes 4 20 5 4
0 male 37 15 yes 4 20 5 4
0 female 37 15 yes 2 14 1 3
0 male 22 4 yes 1 18 5 4
0 male 37 15 yes 4 17 5 3
0 female 22 1.5 no 2 14 4 5
0 male 52 15 yes 4 14 6 2
0 female 22 1.5 no 4 17 5 5
0 male 32 4 yes 5 14 3 5
0 male 32 4 yes 2 14 3 5
0 female 22 1.5 no 3 16 6 5
0 male 27 0.75 no 2 18 3 3
0 female 22 7 yes 2 14 5 2
0 female 27 0.75 no 2 17 5 3
0 female 37 15 yes 4 12 1 2
0 female 22 1.5 no 1 14 1 5
0 female 37 10 no 2 12 4 4
0 female 37 15 yes 4 18 5 3
0 female 42 15 yes 3 12 3 3
0 male 22 4 no 2 18 5 5
0 male 52 7 yes 2 20 6 2
0 male 27 0.75 no 2 17 5 5
0 female 27 4 no 2 17 4 5
0 male 42 1.5 no 5 20 6 5
0 male 22 1.5 no 4 17 6 5
0 male 22 4 no 4 17 5 3
0 female 22 4 yes 1 14 5 4
0 male 37 15 yes 5 20 4 5
0 female 37 10 yes 3 16 6 3
0 male 42 15 yes 4 17 6 5
0 female 47 15 yes 4 17 5 5
0 male 22 1.5 no 4 16 5 4
0 female 32 10 yes 3 12 1 4
0 female 22 7 yes 1 14 3 5
0 female 32 10 yes 4 17 5 4
0 male 27 1.5 yes 2 16 2 4
0 male 37 15 yes 4 14 5 5
0 male 42 4 yes 3 14 4 5
0 female 37 15 yes 5 14 5 4
0 female 32 7 yes 4 17 5 5
0 female 42 15 yes 4 18 6 5
0 male 27 4 no 4 18 6 4
0 male 22 0.75 no 4 18 6 5
0 male 27 4 yes 4 14 5 3
0 female 22 0.75 no 5 18 1 5
0 female 52 15 yes 5 9 5 5
0 male 32 10 yes 3 14 5 5
0 female 37 15 yes 4 16 4 4
0 male 32 7 yes 2 20 5 4
0 female 42 15 yes 3 18 1 4
0 male 32 15 yes 1 16 5 5
0 male 27 4 yes 3 18 5 5
0 female 32 15 yes 4 12 3 4
0 male 22 0.75 yes 3 14 2 4
0 female 22 1.5 no 3 16 5 3
0 female 42 15 yes 4 14 3 5
0 female 52 15 yes 3 16 5 4
0 male 37 15 yes 5 20 6 4
0 female 47 15 yes 4 12 2 3
0 male 57 15 yes 2 20 6 4
0 male 32 7 yes 4 17 5 5
0 female 27 7 yes 4 17 1 4
0 male 22 1.5 no 1 18 6 5
0 female 22 4 yes 3 9 1 4
0 female 22 1.5 no 2 14 1 5
0 male 42 15 yes 2 20 6 4
0 male 57 15 yes 4 9 2 4
0 female 27 7 yes 2 18 1 5
0 female 22 4 yes 3 14 1 5
0 male 37 15 yes 4 14 5 3
0 male 32 7 yes 1 18 6 4
0 female 22 1.5 no 2 14 5 5
0 female 22 1.5 yes 3 12 1 3
0 male 52 15 yes 2 14 5 5
0 female 37 15 yes 2 14 1 1
0 female 32 10 yes 2 14 5 5
0 male 42 15 yes 4 20 4 5
0 female 27 4 yes 3 18 4 5
0 male 37 15 yes 4 20 6 5
0 male 27 1.5 no 3 18 5 5
0 female 22 0.125 no 2 16 6 3
0 male 32 10 yes 2 20 6 3
0 female 27 4 no 4 18 5 4
0 female 27 7 yes 2 12 5 1
0 male 32 4 yes 5 18 6 3
0 female 37 15 yes 2 17 5 5
0 male 47 15 no 4 20 6 4
0 male 27 1.5 no 1 18 5 5
0 male 37 15 yes 4 20 6 4
0 female 32 15 yes 4 18 1 4
0 female 32 7 yes 4 17 5 4
0 female 42 15 yes 3 14 1 3
0 female 27 7 yes 3 16 1 4
0 male 27 1.5 no 3 16 4 2
0 male 22 1.5 no 3 16 3 5
0 male 27 4 yes 3 16 4 2
0 female 27 7 yes 3 12 1 2
0 female 37 15 yes 2 18 5 4
0 female 37 7 yes 3 14 4 4
0 male 22 1.5 no 2 16 5 5
0 male 37 15 yes 5 20 5 4
0 female 22 1.5 no 4 16 5 3
0 female 32 10 yes 4 16 1 5
0 male 27 4 no 2 17 5 3
0 female 22 0.417 no 4 14 5 5
0 female 27 4 no 2 18 5 5
0 male 37 15 yes 4 18 5 3
0 male 37 10 yes 5 20 7 4
0 female 27 7 yes 2 14 4 2
0 male 32 4 yes 2 16 5 5
0 male 32 4 yes 2 16 6 4
0 male 22 1.5 no 3 18 4 5
0 female 22 4 yes 4 14 3 4
0 female 17.5 0.75 no 2 18 5 4
0 male 32 10 yes 4 20 4 5
0 female 32 0.75 no 5 14 3 3
0 male 37 15 yes 4 17 5 3
0 male 32 4 no 3 14 4 5
0 female 27 1.5 no 2 17 3 2
0 female 22 7 yes 4 14 1 5
0 male 47 15 yes 5 14 6 5
0 male 27 4 yes 1 16 4 4
0 female 37 15 yes 5 14 1 3
0 male 42 4 yes 4 18 5 5
0 female 32 4 yes 2 14 1 5
0 male 52 15 yes 2 14 7 4
0 female 22 1.5 no 2 16 1 4
0 male 52 15 yes 4 12 2 4
0 female 22 0.417 no 3 17 1 5
0 female 22 1.5 no 2 16 5 5
0 male 27 4 yes 4 20 6 4
0 female 32 15 yes 4 14 1 5
0 female 27 1.5 no 2 16 3 5
0 male 32 4 no 1 20 6 5
0 male 37 15 yes 3 20 6 4
0 female 32 10 no 2 16 6 5
0 female 32 10 yes 5 14 5 5
0 male 37 1.5 yes 4 18 5 3
0 male 32 1.5 no 2 18 4 4
0 female 32 10 yes 4 14 1 4
0 female 47 15 yes 4 18 5 4
0 female 27 10 yes 5 12 1 5
0 male 27 4 yes 3 16 4 5
0 female 37 15 yes 4 12 4 2
0 female 27 0.75 no 4 16 5 5
0 female 37 15 yes 4 16 1 5
0 female 32 15 yes 3 16 1 5
0 female 27 10 yes 2 16 1 5
0 male 27 7 no 2 20 6 5
0 female 37 15 yes 2 14 1 3
0 male 27 1.5 yes 2 17 4 4
0 female 22 0.75 yes 2 14 1 5
0 male 22 4 yes 4 14 2 4
0 male 42 0.125 no 4 17 6 4
0 male 27 1.5 yes 4 18 6 5
0 male 27 7 yes 3 16 6 3
0 female 52 15 yes 4 14 1 3
0 male 27 1.5 no 5 20 5 2
0 female 27 1.5 no 2 16 5 5
0 female 27 1.5 no 3 17 5 5
0 male 22 0.125 no 5 16 4 4
0 female 27 4 yes 4 16 1 5
0 female 27 4 yes 4 12 1 5
0 female 47 15 yes 2 14 5 5
0 female 32 15 yes 3 14 5 3
0 male 42 7 yes 2 16 5 5
0 male 22 0.75 no 4 16 6 4
0 male 27 0.125 no 3 20 6 5
0 male 32 10 yes 3 20 6 5
0 female 22 0.417 no 5 14 4 5
0 female 47 15 yes 5 14 1 4
0 female 32 10 yes 3 14 1 5
0 male 57 15 yes 4 17 5 5
0 male 27 4 yes 3 20 6 5
0 female 32 7 yes 4 17 1 5
0 female 37 10 yes 4 16 1 5
0 female 32 10 yes 1 18 1 4
0 female 22 4 no 3 14 1 4
0 female 27 7 yes 4 14 3 2
0 male 57 15 yes 5 18 5 2
0 male 32 7 yes 2 18 5 5
0 female 27 1.5 no 4 17 1 3
0 male 22 1.5 no 4 14 5 5
0 female 22 1.5 yes 4 14 5 4
0 female 32 7 yes 3 16 1 5
0 female 47 15 yes 3 16 5 4
0 female 22 0.75 no 3 16 1 5
0 female 22 1.5 yes 2 14 5 5
0 female 27 4 yes 1 16 5 5
0 male 52 15 yes 4 16 5 5
0 male 32 10 yes 4 20 6 5
0 male 47 15 yes 4 16 6 4
0 female 27 7 yes 2 14 1 2
0 female 22 1.5 no 4 14 4 5
0 female 32 10 yes 2 16 5 4
0 female 22 0.75 no 2 16 5 4
0 female 22 1.5 no 2 16 5 5
0 female 42 15 yes 3 18 6 4
0 female 27 7 yes 5 14 4 5
0 male 42 15 yes 4 16 4 4
0 female 57 15 yes 3 18 5 2
0 male 42 15 yes 3 18 6 2
0 female 32 7 yes 2 14 1 2
0 male 22 4 no 5 12 4 5
0 female 22 1.5 no 1 16 6 5
0 female 22 0.75 no 1 14 4 5
0 female 32 15 yes 4 12 1 5
0 male 22 1.5 no 2 18 5 3
0 male 27 4 yes 5 17 2 5
0 female 27 4 yes 4 12 1 5
0 male 42 15 yes 5 18 5 4
0 male 32 1.5 no 2 20 7 3
0 male 57 15 no 4 9 3 1
0 male 37 7 no 4 18 5 5
0 male 52 15 yes 2 17 5 4
0 male 47 15 yes 4 17 6 5
0 female 27 7 no 2 17 5 4
0 female 27 7 yes 4 14 5 5
0 female 22 4 no 2 14 3 3
0 male 37 7 yes 2 20 6 5
0 male 27 7 no 4 12 4 3
0 male 42 10 yes 4 18 6 4
0 female 22 1.5 no 3 14 1 5
0 female 22 4 yes 2 14 1 3
0 female 57 15 no 4 20 6 5
0 male 37 15 yes 4 14 4 3
0 female 27 7 yes 3 18 5 5
0 female 17.5 10 no 4 14 4 5
0 male 22 4 yes 4 16 5 5
0 female 27 4 yes 2 16 1 4
0 female 37 15 yes 2 14 5 1
0 female 22 1.5 no 5 14 1 4
0 male 27 7 yes 2 20 5 4
0 male 27 4 yes 4 14 5 5
0 male 22 0.125 no 1 16 3 5
0 female 27 7 yes 4 14 1 4
0 female 32 15 yes 5 16 5 3
0 male 32 10 yes 4 18 5 4
0 female 32 15 yes 2 14 3 4
0 female 22 1.5 no 3 17 5 5
0 male 27 4 yes 4 17 4 4
0 female 52 15 yes 5 14 1 5
0 female 27 7 yes 2 12 1 2
0 female 27 7 yes 3 12 1 4
0 female 42 15 yes 2 14 1 4
0 female 42 15 yes 4 14 5 4
0 male 27 7 yes 4 14 3 3
0 male 27 7 yes 2 20 6 2
0 female 42 15 yes 3 12 3 3
0 male 27 4 yes 3 16 3 5
0 female 27 7 yes 3 14 1 4
0 female 22 1.5 no 2 14 4 5
0 female 27 4 yes 4 14 1 4
0 female 22 4 no 4 14 5 5
0 female 22 1.5 no 2 16 4 5
0 male 47 15 no 4 14 5 4
0 male 37 10 yes 2 18 6 2
0 male 37 15 yes 3 17 5 4
0 female 27 4 yes 2 16 1 4
3 male 27 1.5 no 3 18 4 4
3 female 27 4 yes 3 17 1 5
7 male 37 15 yes 5 18 6 2
12 female 32 10 yes 3 17 5 2
1 male 22 0.125 no 4 16 5 5
1 female 22 1.5 yes 2 14 1 5
12 male 37 15 yes 4 14 5 2
7 female 22 1.5 no 2 14 3 4
2 male 37 15 yes 2 18 6 4
3 female 32 15 yes 4 12 3 2
1 female 37 15 yes 4 14 4 2
7 female 42 15 yes 3 17 1 4
12 female 42 15 yes 5 9 4 1
12 male 37 10 yes 2 20 6 2
12 female 32 15 yes 3 14 1 2
3 male 27 4 no 1 18 6 5
7 male 37 10 yes 2 18 7 3
7 female 27 4 no 3 17 5 5
1 male 42 15 yes 4 16 5 5
1 female 47 15 yes 5 14 4 5
7 female 27 4 yes 3 18 5 4
1 female 27 7 yes 5 14 1 4
12 male 27 1.5 yes 3 17 5 4
12 female 27 7 yes 4 14 6 2
3 female 42 15 yes 4 16 5 4
7 female 27 10 yes 4 12 7 3
1 male 27 1.5 no 2 18 5 2
1 male 32 4 no 4 20 6 4
1 female 27 7 yes 3 14 1 3
3 female 32 10 yes 4 14 1 4
3 male 27 4 yes 2 18 7 2
1 female 17.5 0.75 no 5 14 4 5
1 female 32 10 yes 4 18 1 5
7 female 32 7 yes 2 17 6 4
7 male 37 15 yes 2 20 6 4
7 female 37 10 no 1 20 5 3
12 female 32 10 yes 2 16 5 5
7 male 52 15 yes 2 20 6 4
7 female 42 15 yes 1 12 1 3
1 male 52 15 yes 2 20 6 3
2 male 37 15 yes 3 18 6 5
12 female 22 4 no 3 12 3 4
12 male 27 7 yes 1 18 6 2
1 male 27 4 yes 3 18 5 5
12 male 47 15 yes 4 17 6 5
12 female 42 15 yes 4 12 1 1
7 male 27 4 no 3 14 3 4
7 female 32 7 yes 4 18 4 5
1 male 32 0.417 yes 3 12 3 4
3 male 47 15 yes 5 16 5 4
12 male 37 15 yes 2 20 5 4
7 male 22 4 yes 2 17 6 4
1 male 27 4 no 2 14 4 5
7 female 52 15 yes 5 16 1 3
1 male 27 4 no 3 14 3 3
1 female 27 10 yes 4 16 1 4
1 male 32 7 yes 3 14 7 4
7 male 32 7 yes 2 18 4 1
3 male 22 1.5 no 1 14 3 2
7 male 22 4 yes 3 18 6 4
7 male 42 15 yes 4 20 6 4
2 female 57 15 yes 1 18 5 4
7 female 32 4 yes 3 18 5 2
1 male 27 4 yes 1 16 4 4
7 male 32 7 yes 4 16 1 4
2 male 57 15 yes 1 17 4 4
7 female 42 15 yes 4 14 5 2
7 male 37 10 yes 1 18 5 3
3 male 42 15 yes 3 17 6 1
1 female 52 15 yes 3 14 4 4
2 female 27 7 yes 3 17 5 3
12 male 32 7 yes 2 12 4 2
1 male 22 4 no 4 14 2 5
3 male 27 7 yes 3 18 6 4
12 female 37 15 yes 1 18 5 5
7 female 32 15 yes 3 17 1 3
7 female 27 7 no 2 17 5 5
1 female 32 7 yes 3 17 5 3
1 male 32 1.5 yes 2 14 2 4
12 female 42 15 yes 4 14 1 2
7 male 32 10 yes 3 14 5 4
7 male 37 4 yes 1 20 6 3
1 female 27 4 yes 2 16 5 3
12 female 42 15 yes 3 14 4 3
1 male 27 10 yes 5 20 6 5
12 male 37 10 yes 2 20 6 2
12 female 27 7 yes 1 14 3 3
3 female 27 7 yes 4 12 1 2
3 male 32 10 yes 2 14 4 4
12 female 17.5 0.75 yes 2 12 1 3
12 female 32 15 yes 3 18 5 4
2 female 22 7 no 4 14 4 3
1 male 32 7 yes 4 20 6 5
7 male 27 4 yes 2 18 6 2
1 female 22 1.5 yes 5 14 5 3
12 female 32 15 no 3 17 5 1
12 female 42 15 yes 2 12 1 2
7 male 42 15 yes 3 20 5 4
12 male 32 10 no 2 18 4 2
12 female 32 15 yes 3 9 1 1
7 male 57 15 yes 5 20 4 5
12 male 47 15 yes 4 20 6 4
2 female 42 15 yes 2 17 6 3
12 male 37 15 yes 3 17 6 3
12 male 37 15 yes 5 17 5 2
7 male 27 10 yes 2 20 6 4
2 male 37 15 yes 2 16 5 4
12 female 32 15 yes 1 14 5 2
7 male 32 10 yes 3 17 6 3
2 male 37 15 yes 4 18 5 1
7 female 27 1.5 no 2 17 5 5
3 female 47 15 yes 2 17 5 2
12 male 37 15 yes 2 17 5 4
12 female 27 4 no 2 14 5 5
2 female 27 10 yes 4 14 1 5
1 female 22 4 yes 3 16 1 3
12 male 52 7 no 4 16 5 5
2 female 27 4 yes 1 16 3 5
7 female 37 15 yes 2 17 6 4
2 female 27 4 no 1 17 3 1
12 female 17.5 0.75 yes 2 12 3 5
7 female 32 15 yes 5 18 5 4
7 female 22 4 no 1 16 3 5
2 male 32 4 yes 4 18 6 4
1 female 22 1.5 yes 3 18 5 2
3 female 42 15 yes 2 17 5 4
1 male 32 7 yes 4 16 4 4
12 male 37 15 no 3 14 6 2
1 male 42 15 yes 3 16 6 3
1 male 27 4 yes 1 18 5 4
2 male 37 15 yes 4 20 7 3
7 male 37 15 yes 3 20 6 4
3 male 22 1.5 no 2 12 3 3
3 male 32 4 yes 3 20 6 2
2 male 32 15 yes 5 20 6 5
12 female 52 15 yes 1 18 5 5
12 male 47 15 no 1 18 6 5
3 female 32 15 yes 4 16 4 4
7 female 32 15 yes 3 14 3 2
7 female 27 7 yes 4 16 1 2
12 male 42 15 yes 3 18 6 2
7 female 42 15 yes 2 14 3 2
12 male 27 7 yes 2 17 5 4
3 male 32 10 yes 4 14 4 3
7 male 47 15 yes 3 16 4 2
1 male 22 1.5 yes 1 12 2 5
7 female 32 10 yes 2 18 5 4
2 male 32 10 yes 2 17 6 5
2 male 22 7 yes 3 18 6 2
1 female 32 15 yes 3 14 1 5
;

%macro run_hpgenselect();
%let selections = forward backward stepwise;
%do i = 1 %to %sysfunc(countw(&selections));
%let method = %scan(&selections, &i);
proc hpgenselect data=work.Affairs;
class gender children;
model affairs = gender age yearsmarried children religiousness education occupation rating /dist=poisson link=log;
selection method=&method(select=SL);
run;
%end;
%end;
%mend run_hpgenselect;
%run_hpgenselect();


/* multivariate multiple linear stepwise regression */
/*---------------------------- Tobacco Data -----------------------------------*
Anderson and Bancroft (1952, p. 205) presented data on chemical components of
25 tobacco leaf samples. The dependent variables are
Y1 : Rate of cigarette burn in inches per 1000 seconds
Y2 : Percent sugar in the leaf
Y3 : Percent nicotine
The independent variables are fixed and are
X1 : Percentage of Nitrogen X4 : Percentage of Phosphorus
X2 : Percentage of Chlorine X5 : Percentage of Calcium
X3 : Percentage of Potassium X6 : Percentage of Magnesium

*----------------------------------------------------------------------------------*/

Data tobacco;
input y1 y2 y3 x1 x2 x3 x4 x5 x6;
cards;
1.55 20.05 1.38 2.02 2.90 2.17 0.51 3.47 0.91
1.63 12.58 2.64 2.62 2.78 1.72 0.50 4.57 1.25
1.66 18.56 1.56 2.08 2.68 2.40 0.43 3.52 0.82
1.52 18.56 2.22 2.20 3.17 2.06 0.52 3.69 0.97
1.70 14.02 2.85 2.38 2.52 2.18 0.42 4.01 1.12
1.68 15.64 1.24 2.03 2.56 2.57 0.44 2.79 0.82
1.78 14.52 2.86 2.87 2.67 2.64 0.50 3.92 1.06
1.57 18.52 2.18 1.88 2.58 2.22 0.49 3.58 1.01
1.60 17.84 1.65 1.93 2.26 2.15 0.56 3.57 0.92
1.52 13.38 3.28 2.57 1.74 1.64 0.51 4.38 1.22
1.68 17.55 1.56 1.95 2.15 2.48 0.48 3.28 0.81
1.74 17.97 2.00 2.03 2.00 2.38 0.50 3.31 0.98
1.93 14.66 2.88 2.50 2.07 2.32 0.48 3.72 1.04
1.77 17.31 1.36 1.72 2.24 2.25 0.52 3.10 0.78
1.94 14.32 2.66 2.53 1.74 2.64 0.50 3.48 0.93
1.83 15.05 2.43 1.90 1.46 1.97 0.46 3.48 0.90
2.09 15.47 2.42 2.18 0.74 2.46 0.48 3.16 0.86
1.72 16.85 2.16 2.16 2.84 2.36 0.49 3.68 0.95
1.49 17.42 2.12 2.14 3.30 2.04 0.48 3.28 1.06
1.52 18.55 1.87 1.98 2.90 2.16 0.48 3.56 0.84
1.64 18.74 2.10 1.89 2.82 2.04 0.53 3.56 1.02
1.40 14.79 2.21 2.07 2.79 2.15 0.52 3.49 1.04
1.78 18.86 2.00 2.08 3.14 2.60 0.50 3.30 0.80
1.93 15.62 2.26 2.21 2.81 2.18 0.44 4.16 0.92
1.53 18.56 2.14 2.00 3.16 2.22 0.51 3.73 1.07
;
/* Input statement*/
/* Data tobacco;
infile 'filename.dat';
input y1 y2 y3 x1 x2 x3 x4 x5 x6; */

 PROC IML;
 Reset  FW= 9  noname linesize=120 pagesize=60;
 use tobacco;
 read all var{y1 y2 y3} into Y; /* The dependent variables      */
 read all var {x1 x2 x3 x4 x5 x6} into X; /* The independent variables    */

START INITIAL;                   /*----Initialization Module ---*/
  N=NROW(X);                     /* Number of Observations      */
  K=NCOL(X);                     /* Total No. of ind. var.      */
  Q=NCOL(Y);                     /* Total No. of dep. var.      */
  K1=K+1;                        /* No. of ind. + intercept     */
  IK=1:K;                        /* Ind. var. names (1 2 .. k)  */
  Test=1;                        /* Initial prob. to enter var. */
  R1=0:(q-1);                    /* Rank's values               */
                                 /* (Names of all ind. var.     */
  ALL_idv=num(rowcatc(char(ik)));/* ordered from 1-k )          */
  F_Enter=Int(finv(.95,1,(n-k)));/* Min acceptable F to Enter   */
  F_Rmv=F_Enter-.1;              /* Max acceptable F to Remove  */
  Lm_ENTER=(n-k)/(F_Enter+(n-k));/* Max acceptable Lm to enter  */
  Lm_RMV=(n-k)/(F_Rmv+(n-k));    /* Min acceptable Lm to remove */
  BNAMES={VAR MSE RSQUARE AdjRsq /*                             */
        AIC AICc HQ  HQIC BIC    /*                             */
         Cp UpLm Novar};         /*                             */
  /*---CORRECT BY MEAN, ADJUST OUT INTERCEPT PARAMETER----------*/
  Y=Y-REPEAT(Y[+,]/N,N,1);       /* CORRECT Y BY MEAN           */
  X=X-REPEAT(X[+,]/N,N,1);       /* CORRECT X BY MEAN           */
  H=X*INV(X`*X)*X`;              /* H matrix                    */
  YP=Y[+,]/N;                    /* MEANS OF COLUMNS OF Y       */
  XPY=X`*Y;                      /* CROSSPRODUCTS               */
  YPY=Y`*Y;                      /*                             */
  XPX=X`*X;                      /*                             */
  SSEk=y`*(I(n)-H)*y;            /* Sum sq. of err. for all x's */
  SSTO= YPY-(N*YP`*YP);          /* Total sum square of error   */
  CONST= det(SSTO);              /* Determinant of SSTO matrix  */
  CSAVE=(XPX || XPY) //          /*                             */
        (XPY`|| YPY);            /* SAVED COPY OF CROSSPRODUCTS */
 FOOTNOTE 'Independent variables that appear as 1, 2, 3, ..., 12 13 etc., they mean x1, X2, ..., x1 x2, x1 x3, etc.';
FINISH INITIAL;                  /*---- Ends Initialization ----*/

/*---------------------SEARCH ALL POSSIBLE MODELS--------------------------*/
START ALL;
   print /'All Possible Selection Methods',
   '--------------------------------------------------------------------';
  /*------USE METHOD OF SCHATZOFF ET AL. FOR SEARCH TECHNIQUE--------------*/
                                        /*----- Start of the module ---- */
  LIMIT=2##K-1;                         /* NUMBER OF MODELS TO EXAMINE   */
                                        /*                               */
 C=CSAVE; IN=REPEAT(0,K,1);             /* START WITH NO VAR. IN MODEL   */
                                        /*                               */
      DO kk=1 TO LIMIT;                 /*                               */
        RUN ZTRAIL;                     /* FIND WHICH ONE TO SWEEP       */
        RUN SWP;                        /* SWEEP IT IN                   */
   BB=BB//(VAR_IN||MSE||RSQ||AdjRsq||AIC/*                               */
        ||AICp||HQp||HBICp||BICp        /*                               */
        ||Cp||UpLm||L);                 /*                               */
        END;                            /*                               */
        ALL_P=BB;                       /* Sorting the output matrix     */
        BB[(RANK(BB[,1]))`,]=ALL_P;     /* to the variable names         */
                                        /*                               */
                                        /*                               */
                                        /*-------------- end ------------*/
        Counter=0;                      /*                               */
        Do jj=1 to nrow(bb);            /*                               */
         If (bb[jj,2]<=bb[nrow(bb),2]) then do;
         Counter=Counter+1;             /*                               */
         b_mse=b_mse//bb[jj,1:2];       /*                               */
         end;                           /*(finding the |MSEp|<|MSEk|     */
        End;                            /* & indv < total ind. variables)*/
        If Counter=0 then b_mse=All_idv;/*                               */
        Else do;                        /*                               */
          ind=b_mse[>:<,2];             /* Index's location of MSEp' min */
          b_mse=b_mse[ind,1];           /* Best subset according to MSEp */
        End;                            /*                               */
        Counter=0;                      /*                               */
                                        /*                               */
        Do jj=1 to nrow(bb);            /*                               */
         If (bb[jj,10]<=bb[jj,11]) & (bb[jj,1]<all_idv) then do;
         Counter=Counter+1;             /*                               */
         cp2=cp2//(Counter||bb[jj,1]);  /*                               */
         end;                           /*(finding the |Cp|<= UpperLimit */
        End;                            /* & indv < total ind. variables)*/
        If Counter=0 then b_Cp=All_idv; /* Set all ind. var. when Cp>UpLm*/
        Else b_Cp=cp2;                  /* Best subset according to Cp   */
        ind=bb[>:<,6];                  /* AICcp corrected form of AICp  */
        b_AICC=bb[ind,1];               /* Best subset according to AICcp*/
        ind=bb[>:<,7];                  /* Locate the index of min of HQp*/
        b_HQ=bb[ind,1];                 /* Best subset according to HQp  */
        ind=bb[>:<,5];                  /* Locate the index of min of AIC*/
        b_AIC=bb[ind,1];                /* Best subset according to AIC  */
        ind=bb[>:<,9];                  /* Index's location of BICp' min */
        b_BIC=bb[ind,1];                /* Best subset according to BICp */
        ind=bb[>:<,8];                  /* Index's location of HBICp' min*/
        b_HQIC=bb[ind,1];               /* Best subset according to HBICp*/
ind=bb[<:>,4];                  /* Index's location of AjdRsq max*/
        b_AdjR=bb[ind,1];               /* Best subset according to AjdRsq*/
        BB=ROUND(BB,.001);              /*                               */
        print BB[colname=bnames ];      /*                               */
        F_out=Forward//Backward//Step//b_mse//b_AdjR//b_aic//b_aicc//b_hq
              //b_hQic//b_bic;
        F2_out={Forward, Backward, Stepwise,MSE, AdjRsq,AIC, AICc,HQ, HQIC,BIC};
        F3_out=b_cp;
        F4_out={Ser_No, Ind_Var};

  print /'-----------------------------------------------------------------',
        'Summary',
        '------------------------------------------------------------------',
        F_out[RowName=F2_out],
        F3_out[ColName=F4_out],
        '------------------------------------------------------------------';
FINISH ALL;
/*-------------------------- End of All Module ----------------------------*/

 /*-------------------------------------------------------------*
  : SUBROUTINE TO FIND NUMBER OF TRAILING ZEROS IN BINARY NUMBER:
  : ON ENTRY: kk IS THE NUMBER TO EXAMINE                       :
  : ON EXIT:  II HAS THE RESULT                                 :
  *-------------------------------------------------------------*/
START ZTRAIL;
    II=1; ZZ=kk;
    DO WHILE(MOD(ZZ,2)=0); II=II+1; ZZ=ZZ/2; END;
FINISH ZTRAIL;
 /*-------------------------------------------------------------*
  :              SUBROUTINE TO SWEEP IN A PIVOT                 :
  : ON ENTRY: II HAS THE POSITION(S) TO PIVOT                   :
  : ON EXIT:  IN, L, DFE, MSE, RSQ RECALCULATED                 :
  *-------------------------------------------------------------*/
START SWP;
     IF ABS(C[II,II])<1E-9 THEN DO; print , "FAILURE", C;STOP;END;
     C=SWEEP(C,II);
     IN[II,]=^IN[II,];
     L=SUM(IN); DFE=N-L;
     Xp=X[,loc(in)];                                    /* Xp matrix         */
     Hp=Xp*inv(Xp`*Xp)*Xp`;                             /* Hp matrix         */
     SSE=C[(K1:nrow(csave)),(K1:ncol(csave))];          /* Sum Sq. error     */
     SSR=Y`*(Hp-(J(n)/n))*Y;                            /* Sum Sq. reg.      */
     MSE=det(SSE/DFE);                                  /* Mean SS error     */
     RSQ=det(Inv(SSTO)*SSR);                            /* R squared         */
AdjRsq=det(i(q)-((n-1)/(DFE-1))*(Inv(SSTO)*SSE)); /* Adjusted R squared*/
Cp=(N-K)*INV(SSEk)*SSE+(2*L-N)*I(q);               /* Cp Criterion      */
     Cp=det((Cp+(n-2*L)*I(q))/(n-k));                   /* Det. of Cp        */
     UpLm=((n-L)/(n-k1))**q;                            /* Upper limit of Cp */
     Aic=n*log(det(SSE)/n)+2*(L+1)*q+q*(q+1) + n;            /* AICp Method       */
     AICp=n*log(det(SSE)/n)+n*(n+(L+1))*q/(n-(L+1)-q-1);              /* AICp Corrected    */
     BICp=n*log(det(SSE)/n)+(L+1)*LOG(n)*q;                /* BICp Criterion    */
     HBICp=n*log(det(SSE)/n)+2*q*(L+1)*LOG(LOG(n));        /* HQICp Criterion   */
     HQp=log(det(SSE**2)/n)+((2*LOG(LOG(N))*Q*L)/(N-L-Q-1));
     VAR_IN=Num(ROWCATC(CHAR(LOC(IN))));                /* Ind. Var. names   */
FINISH SWP;
/*-------------------------- End of SWP Module ----------------------------*/

START SEQ;
  RUN INITIAL;                                          /* INITIALIZATION          */
  RUN ALL;                                              /* ALL POSSIBLE Regression */
FINISH SEQ;
 Run seq;

create data from BB[colname=bnames ];
append  from bb;
      /*--------------------------------*/
QUIT; /* THIS IS THE END OF THE PROGRAM */
      /*--------------------------------*/
