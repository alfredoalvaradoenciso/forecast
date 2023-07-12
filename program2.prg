wf   m  2000.1   2013.1
read(b3) "C:\Users\PC2\Desktop\Clase3\datos.xls"   7

group  g1  circulantesb ipc liquidezsb pbi  tahorr tcsoldolar tpmn
g1.line(m)

' M/P=kY^a e^(-bi)   ===> LN(M/P) =bo+a*Ln(y) -b*i
' Desestacionalización mediante media movil
series cirsa=@movav(circulantesb, 12)
series pbisa=@movav(pbi, 12)
' Desestacionalizar mediante census x12
pbi.x12(save="d10 d11")  pbi
circulantesb.x12(save="d10 d11")  circulantesb

series ly=log(pbi_sa)
series lcir=log(circulantesb_sa)

equation eq1.ls  lcir  c   ly  tpmn
'Testeando Autocorrelación
eq1.correl(16)
eq1.auto(8)
'Testeando Heterocedasticidad
eq1.white(c)
eq1.hettest(type="harvey") tpmn  ly
eq1.hettest(type="Glejser") tpmn  ly
eq1.hettest  tpmn  ly
eq1.archtest(1)
'Testeando Normalidad
eq1.hist
'No linealidades
'Test de Ramsey
eq1.reset(1)

'Test graficos
eq1.rls(r)   'residuos recursivos
eq1.rls(q)  'cusum
eq1.rls(v)  'cusum2
eq1.rls(n)  'chow en n-etapas
eq1.rls(o)  'chow en 1 etapa
eq1.rls(c)  'coeficientes recursivos

include  tchow
group  gg1  ly  tpmn
table  resultado
scalar  k1=0
call  tchow(gg1, lcir, resultado,  k1)

series  d1003=0
smpl  2003.11  @last
d1003=1
smpl  @all

equation eq2.ls  lcir  c   ly  tpmn d1003  d1003*ly  d1003*tpmn

group  gg1  ly  tpmn
table  resultado1
scalar  k2=46
call  tchow(gg1, lcir, resultado1,  k2)

series  d0106=0
smpl  2006.1  @last
d0106=1
smpl  @all

equation eq3.ls  lcir  c   ly  tpmn d1003  d1003*ly  d1003*tpmn d0106 d0106*ly d0106*tpmn

group  gg1  ly  tpmn
table  resultado2
scalar  k3=73
call  tchow(gg1, lcir, resultado2,  k3)

series  d1008=0
smpl  2008.11  @last
d1008=1
smpl  @all

equation eq4.ls  lcir  c   ly  tpmn d1003  d1003*ly  d1003*tpmn d0106 d0106*ly d0106*tpmn  d1008  d1008*ly  d1008*tpmn
equation eq4.ls  lcir  c   ly  tpmn d1003  d1003*ly  d1003*tpmn d0106 d0106*ly d0106*tpmn   d1008*ly  d1008*tpmn
equation eq4.ls  lcir  c   ly  tpmn d1003  d1003*ly  d1003*tpmn d0106 d0106*ly d0106*tpmn   d1008*tpmn

'Suponiendo que hay autocorrelación
equation eq5.ls  lcir  c   ly  tpmn d1003  d1003*ly  d1003*tpmn d0106 d0106*ly d0106*tpmn   d1008*tpmn ar(1)
equation eq6.ls(n)  lcir  c   ly  tpmn d1003  d1003*ly  d1003*tpmn d0106 d0106*ly d0106*tpmn   d1008*tpmn 

'Suponiendo que hay heterocedasticidad
equation eq7.ls(w=1/ly)  lcir  c   ly  tpmn d1003  d1003*ly  d1003*tpmn d0106 d0106*ly d0106*tpmn   d1008*tpmn 
equation eq8.ls(h)  lcir  c   ly  tpmn d1003  d1003*ly  d1003*tpmn d0106 d0106*ly d0106*tpmn   d1008*tpmn

'Prediccion dentro de la muestra
freeze(Tabla1)  eq5.forecast(g, e) lcir_f1
freeze(Tabla2)  eq4.forecast(g, e) lcir_f2

'Fuera de la muestra
expand  2000.1  2013.12
smpl  2013.2  @last
d0106=d0106(-1)
d1003=d1003(-1)
d1008=d1008(-1)
tpmn=tpmn(-1)
pbi_sa=(1+0.0041)*pbi_sa(-1)
ly=log(pbi_sa)
freeze(tabla3) eq4.forecast(g, e)  lcirf_3   se1
freeze(tabla4) eq5.forecast(g, e)  lcirf_4   se2
freeze(tabla5) eq5.fit(g, e)  lcirf_5
smpl  @all

group  grupo1  lcirf_4  lcirf_5  lcir
smpl  2005.1  @last
grupo1.line

smpl  2013.2  @last
circulantesb_sf=circulantesb_sf(-12)

smpl  @all
series  circulantepy=@exp(lcirf_4)*circulantesb_sf

'Intervalo de confianza para el logaritmo del circulante

series linf=lcir
series lsup=lcir
smpl  2013.2  @last
linf=lcirf_4-1.96*se1
lsup=lcirf_4+1.96*se1
smpl  @all

group grupo2  lcirf_4  linf  lsup  lcir
grupo2.line

series  circulantepy1=@exp(linf)*circulantesb_sf
series  circulantepy2=@exp(lsup)*circulantesb_sf
