
br pobreza pobreza_interp pibpc

tsline pobreza_interp, title("Pobreza Interpolada") xline(1973 1980 1990) xtitle("Tiempo") ytitle("Pobreza Interpolada")
graph save pobreza.gph, replace

tsline pibpc, title("PIB per cápita") xtitle("Tiempo") ytitle("PIB per cápita")  xline(1973 1980 1990)
graph save pibpc.gph, replace

graph combine pobreza.gph pibpc.gph

gen lnpobreza_interp=ln(pobreza_interp)

gen lnpibpc=ln(pibpc)
gen lnpibpc2=ln(pibpc)^2
gen lnpibpc3=ln(pibpc)^3
gen lnpibpc4=ln(pibpc)^4

//lineal
reg pobreza_interp pibpc pibpc2
ovtest
estat ic
hettest
estat dwatson
bgodfrey, lags(1/4)
capture drop residiocubico
predict residiocubico,resid
swilk residiocubico
sfrancia residiocubico
sktest residiocubico
qnorm residiocubico
predict cuadratico,xb

newey pobreza_interp pibpc pibpc2,lag(1)
prais pobreza_interp pibpc pibpc2
prais pobreza_interp pibpc  

*nil log
reg pobreza_interp lnpibpc lnpibpc2  
ovtest
estat ic
hettest
estat dwatson
bgodfrey, lags(1/4)
capture drop residiocubico
predict residiocubico,resid
swilk residiocubico
sfrancia residiocubico
sktest residiocubico

newey  pobreza_interp lnpibpc lnpibpc2, lag(4)
predict log

*log-nivel
reg lnpobreza_interp pibpc pibpc2
ovtest
estat ic
hettest
estat dwatson
bgodfrey, lags(1/4)
capture drop residiocubico
predict residiocubico,resid
swilk residiocubico
sfrancia residiocubico
sktest residiocubico
*qnorm residiocubico


newey lnpobreza_interp pibpc pibpc2, lag(3)
predict predicho3
gen ln_nivel=exp(predicho3)





**** ln ln
reg lnpobreza_interp lnpibpc lnpibpc2 lnpibpc3
ovtest
estat ic
hettest
estat dwatson
bgodfrey, lags(1/4)
capture drop residiocubico
predict residiocubico,resid
swilk residiocubico
sfrancia residiocubico
sktest residiocubico
*qnorm residiocubico

newey lnpobreza_interp lnpibpc lnpibpc2 lnpibpc3, lag(3)
predict predicho4
gen predichopobreza4=exp(predicho4)





reg pobreza_interp pibpc pibpc2 
ovtest
estat ic
capture hettest
estat dwatson
bgodfrey, lags(1/20)
capture drop residiocubico
predict residiocubico,resid
swilk residiocubico
sfrancia residiocubico
sktest residiocubico

newey pobreza_interp pibpc pibpc2, lag(20) 

prais pobreza_interp lnpibpc

reg d.pobreza_interp d.pibpc

reg pobreza_interp lnpibpc
ovtest
estat ic
hettest
estat dwatson
bgodfrey, lags(1/20)
capture drop residiocubico
predict residiocubico,resid
swilk residiocubico
sfrancia residiocubico
sktest residiocubico



reg lnpobreza_interp lnpibpc  lnpibpc2
ovtest
estat ic
hettest
estat dwatson
bgodfrey, lags(1/6)
capture drop residiocubico
predict residiocubico,resid
swilk residiocubico
sfrancia residiocubico
hist residiocubico
qnorm residiocubico

newey lnpobreza_interp lnpibpc  lnpibpc2 , lag(4)





newey pobreza_interp lnpibpc  lnpibpc2,lag(4)

predict lnpobreza
twoway(scatter pobreza_interp lnpibpc)(line lnpobreza lnpibpc) 

*gompertz
nl (pobreza_interp = {b0} * exp(-exp({b1} - {b2} * pibpc)))
predict gompertz

nl (pobreza_interp = {b0} * exp(-exp({b1} - {b2} * pibpc))) if year<2016
predict gompertz2



*sigmoide
nl ( pobreza_interp = {b0}/(1 + exp(-({b1} + {b2} * pibpc ))))
predict sigmoide

nl ( pobreza_interp = {b0}/(1 + exp(-({b1} + {b2} * pibpc )))) if year<2016
predictnl sigmoide2

predict yhat2           // Valores ajustados
gen ci_l1 = yhat2 - 1.96 * e(std)   // Límite inferior del intervalo de confianza
gen ci_u1 = yhat2 + 1.96 * e(std)   // Límite superior del intervalo de confianza
predict error, stdp

twoway (rarea ci_u1 ci_l1 pibpc, color(gs12)) ///
       (line yhat2 pibpc, color(blue) lwidth(medium))



twoway (scatter pobreza_interp pibpc, legend(label(1 "Valores reales"))) ///
       (line cuadratico pibpc, lcolor(blue) lwidth(medium) lpattern(solid) legend(label(2 "lineal parámetros"))) ///
	   (line log pibpc, lcolor(green) lwidth(medium) lpattern(solid) legend(label(3 "nivel-logs"))) ///
	   (line ln_nivel pibpc, lcolor(black) lwidth(medium) lpattern(solid) legend(label(4 "Log-nivel"))) ///
       (line sigmoide pibpc, lcolor(red) lwidth(medium) lpattern(dash) legend(label(5 "Sigmoide"))) ///
	   (line gompertz pibpc, lcolor(yellow) lwidth(medium) lpattern(dash) legend(label(6 "Gompertz"))) ///
        if pibpc>4000, title("Relación entre pobreza y PIB per cápita") ///
         xlabel(, labsize(medium)) ylabel(, labsize(medium)) ///
         legend(position(1) ring(0) cols(1)) ///
         xtitle("PIB per cápita") ytitle("Pobreza Interpolada")
		 
		 
twoway (scatter pobreza_interp year, legend(label(1 "Valores reales"))) ///
       (tsline cuadratico , lcolor(blue) lwidth(medium) lpattern(solid) legend(label(2 "Cuadrático"))) ///
	   (tsline log , lcolor(green) lwidth(medium) lpattern(dash) legend(label(3 "nivel-log"))) ///
	   (tsline ln_nivel , lcolor(black) lwidth(medium) lpattern(dash) legend(label(4 "Log-nivel"))) ///
       (tsline sigmoide , lcolor(red) lwidth(medium) lpattern(dash) legend(label(5 "Sigmoide"))) ///
	   (tsline gompertz2 , lcolor(yellow) lwidth(medium) lpattern(dash) legend(label(6 "Gompertz"))) ///
        , title("Relación entre pobreza y PIB per cápita") ///
         xlabel(, labsize(medium)) ylabel(, labsize(medium)) ///
         legend(position(1) ring(0) cols(1)) ///
         xtitle("Año") ytitle("Pobreza Interpolada")
		 
twoway (scatter pobreza_interp year, legend(label(1 "Valores reales"))) ///
       (tsline cuadratico , lcolor(blue) lwidth(medium) lpattern(solid) legend(label(2 "Cuadrático"))) ///
	   (tsline log , lcolor(green) lwidth(medium) lpattern(dash) legend(label(3 "nivel-log"))) ///
	   (tsline ln_nivel , lcolor(black) lwidth(medium) lpattern(dash) legend(label(4 "Log-nivel"))) ///
       (tsline sigmoide2 , lcolor(red) lwidth(medium) lpattern(dash) legend(label(5 "Sigmoide"))) ///
	   (tsline gompertz2 , lcolor(yellow) lwidth(medium) lpattern(dash) legend(label(6 "Gompertz"))) ///
        , title("Relación entre pobreza y PIB per cápita") ///
         xlabel(, labsize(medium)) ylabel(, labsize(medium)) ///
         legend(position(1) ring(0) cols(1)) ///
         xtitle("Año") ytitle("Pobreza Interpolada")
		 

lowess    pobreza_interp pibpc
twoway (qfitci  pobreza_interp pibpc)


reg pobreza_in~p pibpc pibpc2
predict yhat, xb         // Predicciones ajustadas
predict ci_l, stdp       // Error estándar de la predicción
gen upper = yhat + 1.96 * ci_l   // Límite superior del intervalo de confianza
gen lower = yhat - 1.96 * ci_l   // Límite inferior del intervalo de confianza

twoway (rarea upper lower pibpc, color(gs12)) ///
       (line yhat pibpc, color(blue) lwidth(medium))

twoway (rarea upper lower year)(tsline yhat), xtitle("year")
