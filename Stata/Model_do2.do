
set maxvar 10000 //seting maximum number of  variables

set scheme sj //setting the schames for graphs

// 1. Obrobka danych

gen date2 = date(date, "DMY") //zmiana formatu daty z string na data (dodatkowo trzeba w zbiorze zmienic format na %d, zeby poprawinie wyswietlal daty)

rename date2 date3
rename date date2
rename date3 date

order date2, first
order date, first

/************************************************************/

//2. Model Nelsona-Siegela

foreach i of numlist 1(1)102 {		
			
	display as yellow "Regresja N-S dla i = " `i' 

	nl ( yield = {b0} + ({b1}+{b2})*(1 - exp(-maturity/{tau}))/(maturity/{tau}) - {b2}*exp(-maturity/{tau}) ) if i==`i', variables(maturity) initial(b0 0 b1 0 b2 0 tau 1) 

	/*wygenerowanie wartoœci dopasowanych z modelu Nelsona-Siegla */
	cap drop fit_yieldNS_`i'
	predict fit_yieldNS_`i'
	mat A`i'= e(b)
	matrix rownames A`i' = param`i'
	matrix colnames A`i' = NS_b0 NS_b1 NS_b2 NS_tauNS
	scalar b0NS_`i' = A`i'[1,1]
	scalar b1NS_`i' = A`i'[1,2] 
	scalar b2NS_`i' = A`i'[1,3] 
	scalar tauNS_`i' = A`i'[1,4] 

	if `i'==1 {
	matrix coefUSA = A`i'
	}
	if `i'~=1 {
	matrix coefUSA = coefUSA \ A`i'
	}


	/* wykres - wartosci startowe: (b0 0 b1 0 b2 0 tau 1)*/

	//tworzenie zmiennej przechowujacej date, zeby moc jej uzyc w nazwach plików
	
/*	local data = date2[`i' * 8]	// i+8(i-1) pozwala na skok co 8 obserwacji z nazwa)

	graph twoway (scatter yield maturity, sort )(line fit_yieldNS_`i' maturity, clcolor(gold) sort ) if i==`i', ///
			name(NSyc_line, replace) ytitle("Rentownosc [w %]", size(vsmall)) xtitle("Termin zapadalnosci [w latach]", size(vsmall)) ///
			title("Model Nelsona - Siegela dla miesiaca: `data'" , size(medium))  ///
			subtitle("Wartosci poczatkowe: {&beta}{sub:0} = 0, {&beta}{sub:1} = 0, {&beta}{sub:2} = 0, {&tau} = 1", size(vsmall)) ///
			legend(order(1 "Rentownosc" 2 "Model Nelsona-Siegela") stack ring(0) pos(5) size(vsmall))
			 
	gr export "C:\Users\Mateusz\Desktop\MRF\Model USA Yield Curve\Wykresy\Nelson-Siegel\NS_startowe_`data'.png", replace */
}

/************************************************************/

/* 3. Model Svenssona  */
foreach i of numlist 1(1)102 {
	
	/* regresja pierwsza - (b0 0 b1 0 b2 0 b3 0 tau1 1 tau2 1) */
	di as yellow "Regresja Svenssona dla wartosci (b0 0 b1 0 b2 0 b3 0 tau1 1 tau2 1) " 
	nl ( yield = {b0} + ({b1}+{b2})*(1-exp(-maturity/{tau1}))/(maturity/{tau1}) - {b2} * exp(-maturity/{tau1}) + {b3}*((1- exp(-maturity/{tau2}))/(maturity/{tau2}) - exp(-maturity/{tau2})) ) if i==`i', variables(maturity) initial(b0 0 b1 0 b2 0 b3 0 tau1 1 tau2 1)

	/*wygenerowanie wartoœci dopasowanych z modelu Svenssona*/
	cap drop fit_yieldS_`i'
	predict fit_yieldS_`i'
	mat As_`i'= e(b)
	matrix rownames As_`i' = param`i'
	matrix colnames As_`i' = Sv_bo Sv_b1 Sv_b2 Sv_tau1 Sv_b3 Sv_tau2
	mat list As_`i'
	scalar b0S_`i'  = As_`i'[1,1]
	scalar b1S_`i'  = As_`i'[1,2] 
	scalar b2S_`i'  = As_`i'[1,3] 
	scalar tauS_`i' = As_`i'[1,4] 
	scalar b3S_`i'  = As_`i'[1,5] 
	scalar tauS_`i' = As_`i'[1,6]

	if `i'==1 {
	matrix coef2USA= As_`i'
	}
	if `i'~=1 {
	matrix coef2USA= coef2USA \ As_`i'
	}

	/* wykres  - wartosci startowe: (b0 0 b1 0 b2 0 tau1 1 tau2 1)

		local data = date2[`i' * 8]	// i+8(i-1) pozwala na skok co 8 obserwacji z nazwa)

	twoway (scatter yield maturity, sort )(line fit_yieldS_`i' maturity, clcolor(gold) sort ) if i==`i', ///
			name(Syc_line, replace) ytitle("Rentownosc [w %]", size(vsmall)) xtitle("Termin zapadalnosci [w latach]", size(vsmall)) ///
			title("Model Svenssona dla dla miesiaca: `data'", size(medium)) /// 
			subtitle("Wartosci poczatkowe: {&beta}{sub:0} = 0, {&beta}{sub:1} = 0, {&beta}{sub:2} = 0, {&beta}{sub:3} = 0, {&tau}{sub:1} = 1, {&tau}{sub:2} = 1", size(vsmall)) ///
			legend(order(1 "Rentownosc" 2 "Model Svenssona") stack ring(0) pos(5) size(vsmall))

	gr export "C:\Users\Mateusz\Desktop\MRF\Model USA Yield Curve\Wykresy\Svensson\Sven_Startowe_`data'.png", replace	*/
}

/************************************************************/

/* 4. Porownanie Nelson-Siegel vs Svensson  */

foreach i of numlist 102(1)102 {

	local data = date2[`i' * 8]
	
	graph twoway (scatter yield maturity, sort)(line fit_yieldNS_`i' maturity, clcolor(gold) sort) ///
		   (line fit_yieldS_`i' maturity, clcolor(orange_red) sort) if i==`i',	ytitle("Rentownosc [w %]", size(medium small)) xtitle("Termin zapadalnosci [w latach]", size(medium small)) ///
			legend(order(1 "Rentownosc" 2 "Model Nelsona-Siegela"  3 "Model Svenssona") stack rows(1) pos(5) size(small))
	gr export "C:\Users\GML\Desktop\Praca magisterska\Modelowanie\Stata\Wykresy\Porównanie\NS_vs_Sven_`data'.png", replace
}

matrix param_USA = coefUSA , coef2USA	
matrix colnames param_USA= NS_b0 NS_b1 NS_b2 NS_tauNS Sv_bo Sv_b1 Sv_b2 Sv_tau1 Sv_b3 Sv_tau2

/************************************************************/

/* 5. Konkretne Wykresy//Nelsona-Siegela dla 09.2008-02.2009:

local name9 = date2[9 * 8]
local name10 = date2[10 * 8]
local name11 = date2[11 * 8]
local name12 = date2[12 * 8]
local name13 = date2[13 * 8]
local name14 = date2[14 * 8]


graph twoway (line fit_yieldNS_9 maturity, clcolor(gold) sort )(line fit_yieldNS_142 maturity, clcolor(orange_red) sort )(line fit_yieldNS_143 maturity, clcolor(pink)sort )(line fit_yieldNS_144 maturity, clcolor(purple) sort ) ///
	   (line fit_yieldNS_145 maturity,clcolor(cranberry) sort )(line fit_yieldNS_146 maturity, clcolor(dkgreen) sort )(line fit_yieldNS_147 maturity, clcolor(black) sort ), ///
	   	ytitle("Rentownosc [w %]", size(vsmall)) xtitle("Termin zapadalnosci [w latach]", size(vsmall)) ///
	   title("Krzywe dochodowosci Nelsona-Siegiela", size(medium)) ///
	   subtitle("`name141' - `name146'", size(vsmall)) ///
	   legend(order(1 "`name141'" 2 "`name142'"  3 "`name143'" 4 "`name144'" 5 "`name145'" 6 "`name146'") stack ring(1) pos(5) size(vsmall)) 
gr export "C:\Users\Mateusz\Desktop\MRF\Model USA Yield Curve\Wykresy\N-S_long.png", replace
 */


//1) Svensson dla 09.2008-02.2009:

local name10 = "PaŸdziernik 2008"
local name11 = "Listopad 2008"
local name12 = "Grudzieñ 2008"
local name13 = "Styczeñ 2009"
local name14 = "Luty 2009"

graph twoway (line fit_yieldS_10 maturity, clcolor(gold) lpattern(solid) lwidth(medthick) sort)(line fit_yieldS_11 maturity, clcolor(blue) lpattern(dot) sort)(line fit_yieldS_12 maturity, clcolor(purple) lpattern(dash_dot) sort)(line fit_yieldS_13 maturity, clcolor(black) lpattern(longdash) sort)(line fit_yieldS_14 maturity, clcolor(orange_red) lwidth(medthick) lpattern(solid) sort), ///
	   ytitle("Rentownosc [w %]", size(medium small)) xtitle("Termin zapadalnosci [w latach]", size(medium small)) ///
	   legend(order(1 "`name10'" 2 "`name11'"  3 "`name12'" 4 "`name13'" 5 "`name14'") stack rows(1) pos(5) size(small)) 
gr export "C:\Users\GML\Desktop\Praca magisterska\Modelowanie\Stata\Wykresy\all_Svens_long.png", replace

gen Kom1Diff = fit_yieldS_14 - fit_yieldS_10
mean(Kom1Diff)

//2) Svensson dla 02.2009-06.2009:

local name14 = "Luty 2009"
local name15 = "Marzec 2009"
local name16 = "Kwiecieñ 2009"
local name17 = "Maj 2009"
local name18 = "Czerwiec 2009"

graph twoway (line fit_yieldS_14 maturity, clcolor(gold) lpattern(solid) lwidth(medthick) sort)(line fit_yieldS_15 maturity, clcolor(blue) lpattern(dot) sort)(line fit_yieldS_16 maturity, clcolor(purple) lpattern(dash_dot) sort)(line fit_yieldS_17 maturity, clcolor(black) lpattern(longdash) sort)(line fit_yieldS_18 maturity, clcolor(orange_red) lwidth(medthick) lpattern(solid) sort), ///
	   ytitle("Rentownosc [w %]", size(medium small)) xtitle("Termin zapadalnosci [w latach]", size(medium small)) ///
	   legend(order(1 "`name14'" 2 "`name15'"  3 "`name16'" 4 "`name17'" 5 "`name18'") stack rows(1) pos(5) size(small)) 
gr export "C:\Users\GML\Desktop\Praca magisterska\Modelowanie\Stata\Wykresy\all_Svens_long.png", replace

gen Kom2Diff = fit_yieldS_18 - fit_yieldS_14
mean(Kom2Diff)

//3) Svensson dla 07.2010-11.2010:

local name31 = "Lipiec 2010"
local name32 = "Sierpieñ 2010"
local name33 = "Wrzesieñ 2010"
local name34 = "PaŸdziernik 2010"
local name35 = "Listopad 2010"

graph twoway (line fit_yieldS_31 maturity, clcolor(gold) lpattern(solid) lwidth(medthick) sort)(line fit_yieldS_32 maturity, clcolor(blue) lpattern(dot) sort)(line fit_yieldS_33 maturity, clcolor(purple) lpattern(dash_dot) sort)(line fit_yieldS_34 maturity, clcolor(black) lpattern(longdash) sort)(line fit_yieldS_35 maturity, clcolor(orange_red) lwidth(medthick) lpattern(solid) sort), ///
	   ytitle("Rentownosc [w %]", size(medium small)) xtitle("Termin zapadalnosci [w latach]", size(medium small)) ///
	   legend(order(1 "`name31'" 2 "`name32'"  3 "`name33'" 4 "`name34'" 5 "`name35'") stack rows(1) pos(5) size(small)) 
gr export "C:\Users\GML\Desktop\Praca magisterska\Modelowanie\Stata\Wykresy\all_Svens_long.png", replace

gen Kom3Diff = fit_yieldS_35 - fit_yieldS_31
mean(Kom3Diff)

//4) Svensson dla 10.2010-02.2011:

local name34 = "PaŸdziernik 2010"
local name35 = "Listopad 2010"
local name36 = "Grudzieñ 2010"
local name37 = "Styczeñ 2011"
local name38 = "Luty 2011"

graph twoway (line fit_yieldS_34 maturity, clcolor(gold) lpattern(solid) lwidth(medthick) sort)(line fit_yieldS_35 maturity, clcolor(blue) lpattern(dot) sort)(line fit_yieldS_36 maturity, clcolor(purple) lpattern(dash_dot) sort)(line fit_yieldS_37 maturity, clcolor(black) lpattern(longdash) sort)(line fit_yieldS_38 maturity, clcolor(orange_red) lwidth(medthick) lpattern(solid) sort), ///
	   ytitle("Rentownosc [w %]", size(medium small)) xtitle("Termin zapadalnosci [w latach]", size(medium small)) ///
	   legend(order(1 "`name34'" 2 "`name35'"  3 "`name36'" 4 "`name37'" 5 "`name38'") stack rows(1) pos(5) size(small)) 
gr export "C:\Users\GML\Desktop\Praca magisterska\Modelowanie\Stata\Wykresy\all_Svens_long.png", replace

gen Kom4Diff = fit_yieldS_38 - fit_yieldS_34
mean(Kom4Diff)

//5) Svensson dla 08.2011-12.2011:

local name44 = "Sierpieñ 2011"
local name45 = "Wrzesieñ 2011"
local name46 = "PaŸdziernik 2011"
local name47 = "Listopad 2011"
local name48 = "Grudzieñ 2011"

graph twoway (line fit_yieldS_44 maturity, clcolor(gold) lpattern(solid) lwidth(medthick) sort)(line fit_yieldS_45 maturity, clcolor(blue) lpattern(dot) sort)(line fit_yieldS_46 maturity, clcolor(purple) lpattern(dash_dot) sort)(line fit_yieldS_47 maturity, clcolor(black) lpattern(longdash) sort)(line fit_yieldS_48 maturity, clcolor(orange_red) lwidth(medthick) lpattern(solid) sort), ///
	   ytitle("Rentownosc [w %]", size(medium small)) xtitle("Termin zapadalnosci [w latach]", size(medium small)) ///
	   legend(order(1 "`name44'" 2 "`name45'"  3 "`name46'" 4 "`name47'" 5 "`name48'") stack rows(1) pos(5) size(small)) 
gr export "C:\Users\GML\Desktop\Praca magisterska\Modelowanie\Stata\Wykresy\all_Svens_long.png", replace

gen Kom5Diff = fit_yieldS_48 - fit_yieldS_44
mean(Kom5Diff)

//6) Svensson dla 08.2012-12.2012:

local name56 = "Sierpieñ 2012"
local name57 = "Wrzesieñ 2012"
local name58 = "PaŸdziernik 2012"
local name59 = "Listopad 2012"
local name60 = "Grudzieñ 2012"

graph twoway (line fit_yieldS_56 maturity, clcolor(gold) lpattern(solid) lwidth(medthick) sort)(line fit_yieldS_57 maturity, clcolor(blue) lpattern(dot) sort)(line fit_yieldS_58 maturity, clcolor(purple) lpattern(dash_dot) sort)(line fit_yieldS_59 maturity, clcolor(black) lpattern(longdash) sort)(line fit_yieldS_60 maturity, clcolor(orange_red) lwidth(medthick) lpattern(solid) sort), ///
	   ytitle("Rentownosc [w %]", size(medium small)) xtitle("Termin zapadalnosci [w latach]", size(medium small)) ///
	   legend(order(1 "`name56'" 2 "`name57'"  3 "`name58'" 4 "`name59'" 5 "`name60'") stack rows(1) pos(5) size(small)) 
gr export "C:\Users\GML\Desktop\Praca magisterska\Modelowanie\Stata\Wykresy\all_Svens_long.png", replace

gen Kom6Diff = fit_yieldS_60 - fit_yieldS_56
mean(Kom6Diff)

//7) Svensson dla 11.2012-03.2013:

local name59 = "Listopad 2012"
local name60 = "Grudzieñ 2012"
local name61 = "Styczeñ 2013"
local name62 = "Luty 2013"
local name63 = "Marzec 2013"

graph twoway (line fit_yieldS_59 maturity, clcolor(gold) lpattern(solid) lwidth(medthick) sort)(line fit_yieldS_60 maturity, clcolor(blue) lpattern(dot) sort)(line fit_yieldS_61 maturity, clcolor(purple) lpattern(dash_dot) sort)(line fit_yieldS_62 maturity, clcolor(black) lpattern(longdash) sort)(line fit_yieldS_63 maturity, clcolor(orange_red) lwidth(medthick) lpattern(solid) sort), ///
	   ytitle("Rentownosc [w %]", size(medium small)) xtitle("Termin zapadalnosci [w latach]", size(medium small)) ///
	   legend(order(1 "`name59'" 2 "`name60'"  3 "`name61'" 4 "`name62'" 5 "`name63'") stack rows(1) pos(5) size(small)) 
gr export "C:\Users\GML\Desktop\Praca magisterska\Modelowanie\Stata\Wykresy\all_Svens_long.png", replace

gen Kom7Diff = fit_yieldS_63 - fit_yieldS_59
mean(Kom7Diff)

//8) Svensson dla 05.2013-09.2013:

local name65 = "Maj 2013"
local name66 = "Czerwiec 2013"
local name67 = "Lipiec 2013"
local name68 = "Sierpieñ 2013"
local name69 = "Wrzesieñ 2013"

graph twoway (line fit_yieldS_65 maturity, clcolor(gold) lpattern(solid) lwidth(medthick) sort)(line fit_yieldS_66 maturity, clcolor(blue) lpattern(dot) sort)(line fit_yieldS_67 maturity, clcolor(purple) lpattern(dash_dot) sort)(line fit_yieldS_68 maturity, clcolor(black) lpattern(longdash) sort)(line fit_yieldS_69 maturity, clcolor(orange_red) lwidth(medthick) lpattern(solid) sort), ///
	   ytitle("Rentownosc [w %]", size(medium small)) xtitle("Termin zapadalnosci [w latach]", size(medium small)) ///
	   legend(order(1 "`name65'" 2 "`name66'"  3 "`name67'" 4 "`name68'" 5 "`name69'") stack rows(1) pos(5) size(small)) 
gr export "C:\Users\GML\Desktop\Praca magisterska\Modelowanie\Stata\Wykresy\all_Svens_long.png", replace

gen Kom8Diff = fit_yieldS_69 - fit_yieldS_65
mean(Kom8Diff)

//9) Svensson dla 10.2013-03.2014:

local name71 = "Listopad 2013"
local name72 = "Grudzieñ 2013"
local name73 = "Styczeñ 2014"
local name74 = "Luty 2014"
local name75 = "Marzec 2014"

graph twoway (line fit_yieldS_71 maturity, clcolor(gold) lpattern(solid) lwidth(medthick) sort)(line fit_yieldS_72 maturity, clcolor(blue) lpattern(dot) sort)(line fit_yieldS_73 maturity, clcolor(purple) lpattern(dash_dot) sort)(line fit_yieldS_74 maturity, clcolor(black) lpattern(longdash) sort)(line fit_yieldS_75 maturity, clcolor(orange_red) lwidth(medthick) lpattern(solid) sort), ///
	   ytitle("Rentownosc [w %]", size(medium small)) xtitle("Termin zapadalnosci [w latach]", size(medium small)) ///
	   legend(order(1 "`name71'" 2 "`name72'"  3 "`name73'" 4 "`name74'" 5 "`name75'") stack rows(1) pos(5) size(small)) 
gr export "C:\Users\GML\Desktop\Praca magisterska\Modelowanie\Stata\Wykresy\all_Svens_long.png", replace

gen Kom9Diff = fit_yieldS_75 - fit_yieldS_71
mean(Kom9Diff)

//10) Svensson dla 09.2014-01.2015:

local name81 = "Wrzesieñ 2014"
local name82 = "PaŸdziernik 2014"
local name83 = "Listopad 2014"
local name84 = "Grudzieñ 2014"
local name85 = "Styczeñ 2015"

graph twoway (line fit_yieldS_81 maturity, clcolor(gold) lpattern(solid) lwidth(medthick) sort)(line fit_yieldS_82 maturity, clcolor(blue) lpattern(dot) sort)(line fit_yieldS_83 maturity, clcolor(purple) lpattern(dash_dot) sort)(line fit_yieldS_84 maturity, clcolor(black) lpattern(longdash) sort)(line fit_yieldS_85 maturity, clcolor(orange_red) lwidth(medthick) lpattern(solid) sort), ///
	   ytitle("Rentownosc [w %]", size(medium small)) xtitle("Termin zapadalnosci [w latach]", size(medium small)) ///
	   legend(order(1 "`name81'" 2 "`name82'"  3 "`name83'" 4 "`name84'" 5 "`name85'") stack rows(1) pos(5) size(small)) 
gr export "C:\Users\GML\Desktop\Praca magisterska\Modelowanie\Stata\Wykresy\all_Svens_long.png", replace

gen Kom10Diff = fit_yieldS_85 - fit_yieldS_81
mean(Kom10Diff)

/************************************************************/

* 6. Obliczanie bledow RMSE dla modelu Nelsona-Siegla i modelu Svenssona */

foreach v of numlist 1(1)102 {

	cap drop NSse`v' NS_RMSE`v' Svse`v' Sv_RMSE`v'
	
	gen NSse`v'=(yield-fit_yieldNS_`v')^2
	su NSse`v' if i==`v'
	scalar NS_RMSE`v'=sqrt(r(sum)/r(N))

	gen Svse`v'=(yield-fit_yieldS_`v')^2
	su Svse`v' if i==`v'
	scalar Sv_RMSE`v'=sqrt(r(sum)/r(N))

	mat temp = `v' , NS_RMSE`v' , Sv_RMSE`v'
	mat colnames temp = observation NS_RMSE Sv_RMSE
	mat rownames temp = `v'_USA

	if `v'==1 {
	mat RMSEUSA = temp
	}
	if `v'!=1 {
	mat RMSEUSA = RMSEUSA \ temp
	}
}

mat list param_USA
mat list RMSEUSA

/* output do plikow zewnetrznych */
cap mat2txt, matrix(RMSEUSA) saving("C:\Users\GML\Desktop\Praca magisterska\Modelowanie\Stata\RMSE.xls") format(%18.8f) replace
cap mat2txt, matrix(param_USA) saving("C:\Users\GML\Desktop\Praca magisterska\Modelowanie\Stata\param_USA.xls") format(%18.8f) replace

// mkmat b0ns_b0 b1ns_b1 b2ns_b2 tauns_tauns b0sv_bo b1sv_b1 b2sv_b2 tau1sv_tau1 b3sv_b3 tau2sv_tau2, matrix(param_USA)

clear
svmat RMSEUSA // creates two columns of data with RMSE

rename RMSEUSA2 Model_NS
rename RMSEUSA3 Model_Sven

graph bar (mean) Model_NS (mean) Model_Sven, bar(1, col(gs10)) bar(2, col(black)) over(RMSEUSA1, relabel(1 "2008" 2 " " 3 " " 4 " " 5 " " 6 " " 7 " " 8 " " 9 " " 10 " " 11 " " 12 " " 13 " "  14 "2009" 15 " " 16 " " 17 " " 18 " " 19 " " 20 " " 21 " " 22 " " 23 " " 24 " " 25 " " 26 " " 27 "2010" 28 " " 29 " " 30 " " 31 " " 32 " " 33 " " 34 " " 35 " " 36 " " 37 " " 38 " " 39 " " 40 "2011" 41 " " 42 " " 43 " " 44 " " 45 " " 46 " " 47 " " 48 " " 49 " " 50 " " 51 " " 52 " " 53 "2012" 54 " " 55 " " 56 " " 57 " " 58 " " 59 " " 60 " " 61 " " 62 " " 63 " " 64 " " 65 " " 66 "2013" 67 " " 68 " " 69 " " 70 " " 71 " " 72 " " 73 " " 74 " " 75 " " 76 " " 77 " " 78 " " 79 "2014" 80 " " 81 " " 82 " " 83 " " 84 " " 85 " " 86 " " 87 " " 88 " " 89 " " 90 " " 91 "2015" 92 " " 93 " " 94 " " 95 " " 96 " " 97 " " 98 " " 99 " " 100 " " 101 " " 102 "2016")) ///
ytitle("Pierwiastek bledu sredniokwadratowego", size(small))  ///
legend(order(1 "Model Siegela-Nelsona" 2 "Model Svenssona") stack ring(1) pos(5) size(medium)) 
gr export "C:\Users\GML\Desktop\Praca magisterska\Modelowanie\Stata\Wykresy\RMSE\RMSE_N-S_vs_Svens.png", replace

// Statystyki opisowe bledow

sjlog using rmse12345
	
tabstat Model_NS Model_Sven, stat(mean med sum sd)

sjlog close
	
sjlog type rmse12345.log.tex



/************************************************************/

/* 7. Oszacowania bet */

clear
svmat param_USA

gen i=_n
rename param_USA1 NS_b0
rename param_USA2 NS_b1
rename param_USA3 NS_b2
rename param_USA4 NS_tau
rename param_USA5 Sv_b0
rename param_USA6 Sv_b1
rename param_USA7 Sv_b2
rename param_USA8 Sv_tau1
rename param_USA9 Sv_b3
rename param_USA10 Sv_tau2


/*local paramNS = "NS_b0 NS_b1 NS_b2 NS_tau" 

foreach t of local paramNS {
	twoway (line `t' i, sort ), ///
			title("Zmiana `t' w modelu N-S", size(medium)) ///
			name(ns`t',replace) ///
			ylabel(,labsize(small))
			
	gr export "C:\Users\GML\Desktop\Praca magisterska\Modelowanie\Stata\Wykresy\Oszacowania N-S\zmiana_NS_`t'.png", replace
}

gr combine nsNS_b0 nsNS_b1 nsNS_b2 nsNS_tau
gr export "C:\Users\GML\Desktop\Praca magisterska\Modelowanie\Stata\Wykresy\Oszacowania N-S\lacznie.png", replace */
generate byte cond0 = i

//zeruje kolejne zmienne odstajace
replace cond0 = 0 in 48
replace cond0 = 0 in 94
replace cond0 = 0 in 102
replace cond0 = 0 in 13
replace cond0 = 0 in 14
replace cond0 = 0 in 15
replace cond0 = 0 in 16
replace cond0 = 0 in 17
replace cond0 = 0 in 30
replace cond0 = 0 in 96
replace cond0 = 0 in 84

local paramS = "Sv_b0 Sv_b1"

foreach t of local paramS {
twoway (line `t' i, sort ) if cond0, ///
			title("Zmiana `t' w modelu Svenssona", size(medium)) ///
			name(sv`t',replace) ///
			ylabel(,labsize(small) ) ///
			graphregion(fcolor(white)) 
	gr export "C:\Users\GML\Desktop\Praca magisterska\Modelowanie\Stata\Wykresy\Oszacowania Svensson\zamiana_Svens_`t'.png", replace
}

gr combine svSv_b0 svSv_b1
gr export "C:\Users\GML\Desktop\Praca magisterska\Modelowanie\Stata\Wykresy\Oszacowania Svensson\Svenns_all.png", replace

//wykres

gen Sv_b0b1 = Sv_b0 + Sv_b1

graph twoway (line Sv_b0 i, clcolor(gold)  sort)(line Sv_b0b1 i, clcolor(green) sort) if cond0, ///
		ytitle("[%]", size(medium small)) xtitle("", size(small)) ///
		xlabel(0 "2008" 12 "2009" 24 "2010" 36 "2011" 48 "2012" 60 "2013" 72 "2014" 84 "2015" 96 "2016") ///
		legend(order(1 "{&beta}{subscript:0}" 2 "{&beta}{subscript:0} + {&beta}{subscript:1}") stack ring(1) pos(5) size(medium)) 
	gr export "C:\Users\GML\Desktop\Praca magisterska\Modelowanie\Stata\Wykresy\Oszacowania Svensson\Joint.png", replace

