
subroutine  tchow(group  indep, series y, scalar p1, table chow)

'===================================TEST DE CHOW===========================================

!i=p1
table(3,4) chow
chow(1,1) = "período"
chow(1,2) = "fecha"
chow(1,3) = "f_stat"
chow(1,4) = "f_prob"
setline(chow,2)
smpl @all
series _dep = y
!obs = @obs(_dep)
%inicio = @otod(!i+1)
%fin = @otod(!obs)
smpl %inicio %fin
equation restringida.ls _dep c indep
!k = restringida.@ncoef
!scrr = restringida.@ssr
!maxf = 0
equation sinres1
equation sinres2
'inicio del algoritmo de búsqueda
for !n = !k+2 to @dtoo(%fin)-(!k+2+!i)
%fin2 = @otod(!n+!i)
%inicio2 = @otod(!n+!i+1)
smpl %inicio %fin2
sinres1.ls _dep c indep
!scrs1 = sinres1.@ssr
smpl %inicio2 %fin
sinres2.ls _dep c indep
!scrs2 = sinres2.@ssr
!f = ((!scrr-(!scrs1+!scrs2))/(!k))/((!scrs1+!scrs2)/(!obs-2*!k-!i))
if !f > !maxf then
!maxf = !f
!periodo = !n+!i
%fecha = @otod(!n+!i)
endif
next
setcell(chow,3,1,!periodo,0)
chow(3,2) = %fecha
chow(3,3) = !maxf
chow(3,4) = @fdist(!maxf,!k,!obs-2*!k-!i)
smpl @all
d _dep
d sinres1
d sinres2
d restringida
'==========================================================================================
endsub
