subroutine fanchart(series serie1, scalar observa, scalar prediccion)
rndseed 1234
'periodos de predicci�n
!n=prediccion   'parametro del procedimiento
'percentiles
!per=19   'para el procedimiento no se mueve
'observaciones historicas
!obs=observa
'numero de predicciones
!pre=10

'parte del procedimiento
vector(!per) t
for !k=1 to !per
t(!k)=5*!k/100    'borrar al ultimo
next

'data real  para cambiar
matrix(!obs,1) hist
stom(serie1, hist)


'parte del procedimiento
matrix(!n , !pre) fore
for !c=1 to !pre
for !f=1 to !n
fore(!f, !c)=@mean(serie1) +  @stdevs(serie1)*@rnorm 'borrrar al ultimo
next
next

matrix(!n, !per) cuantil

For !j=1 to !n 'loop sobre el horizonte de predicci�n
for !q=1 to !per 'Loop sobre los cuantiles
cuantil(!j, !q)=@quantile(@transpose(@subextract(fore, !j,1, !j , !pre)),t(!q))
next
next

matrix(!obs+!n,!per) y

for  !i=1 to !per
for  !k=1 to !obs
y(!k,!i)=hist(!k,1)
next
next

for !k=!obs+1 to !obs+!n
for !i=1 to !per
y(!k,!i)=cuantil(!k-!obs,!i)
next
next

matrix zz=y
for  !r=1 to @rows(zz)
for  !i=2 to @columns(zz)
zz(!r,!i)=zz(!r,!i)-y(!r, !i-1)
next
next

freeze(gph1) zz.area(s)

'Edici�n de color
gph1.setelem(1) fillcolor(255, 255, 255)
gph1.setelem(2) fillcolor(220, 0,195)
gph1.setelem(3) fillcolor(200, 0,185)
gph1.setelem(4) fillcolor(180, 0,175)
gph1.setelem(5) fillcolor(160, 0,165)
gph1.setelem(6) fillcolor(140, 0,155)
gph1.setelem(7) fillcolor(120, 0,145)
gph1.setelem(8) fillcolor(100, 0,135)
gph1.setelem(9) fillcolor(80, 0,120)
gph1.setelem(10) fillcolor(60, 0,110)
gph1.setelem(11) fillcolor(60, 0,110)
gph1.setelem(12) fillcolor(80, 0,120)
gph1.setelem(13) fillcolor(100, 0,135)
gph1.setelem(14) fillcolor(120, 0,145)
gph1.setelem(15) fillcolor(140, 0,155)
gph1.setelem(16) fillcolor(160, 0,165)
gph1.setelem(17) fillcolor(180, 0,175)
gph1.setelem(18) fillcolor(200, 0,185)
gph1.setelem(19) fillcolor(220, 0,195)
gph1.legend -display
show gph1
d fore hist  zz t cuantil

endsub
