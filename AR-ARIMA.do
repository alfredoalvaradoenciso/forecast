clear
*** comparación entre predicción por AR(1) y ARIMA (1,1,1) solo falta cambiar el input.

input anio	y
1990	596.35
1991	603.62
1992	587.26
1993	572.80
1994	568.76
1995	560.15
1996	580.91
1997	554.11
1998	558.38
1999	551.99
2000	558.33
2001	566.68
2002	549.82
2003	561.03
2004	561.77
2005	557.87
2006	554.84
2007	545.85
2008	531.12
2009	480.82
2010	498.34
2011	455.64
2012	474.17
2013	463.96
2014	424.86
2015	408.34
2016	385.80
2017	373.23
2018	364.14


end

count 
scalar obs=r(N)+1
scalar final=anio[1]+obs-1

scalar N=2022 - anio[1]+1 // predicción al año 2021
set obs `=scalar(N)'
replace anio=anio[1]+_n-1

count
scalar obs2=r(N)

tsset anio, y
noi dfuller y, trend regress lags(1) // chequea presencia de raíz unitaria
scalar df=r(p)

**ARIMA(1,1,1) o ARIMA(1,1,0) condicional a la prueba DF
if `=scalar(df)'>0.05 {
cap quietly arima y , arima(1,1,1)
cap predict y_predict, y
cap predict y_forecast, dynamic(`=scalar(final)') y
cap confirm var y_forecast
	if _rc!=0 {
	dis "Modelo ARIMA(1,1,0)" 
	quietly arima y , arima(1,1,0)
	predict y_predict, y
	predict y_forecast, dynamic(`=scalar(final)') y
	}
	else {
	dis "Modelo ARIMA(1,1,1)" 
	}
cap replace y=y_forecast if y==.
cap drop y_predict y_forecast
}
else {
**AR(1)
dis "Modelo AR(1)" 
forval i = `=scalar(obs)'/`=scalar(obs2)' {
quietly reg y L.y , robust
mat def b=e(b)
quietly replace y=b[1,1]*L.y+ b[1,2] in `i'
}
}
*format y* %15.0fc 
