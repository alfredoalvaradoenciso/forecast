'Sample for estimation (Eviews format)
%esmpl="@first 2012m12"

' Sample for forecasting (Eviews format) 
%fsmpl="2013m1 2013m12"

'Bootstrap errors? 1=yes, 0=no, use normal distribution
!boot=1

'Coefficient uncertainty? 1=yes, 0=no coefficient uncertanty
!coefu=1

'Number of bands in fanchart starting from 90%. 
'For example, !numb=1 plots 90% band and the mode, 
'!numb=2 plots 90% band, 45% band and the mode 
!numb=3

'Do not change the 2 parameters below unless you are adapting the code
' for your own model
'Name of the model to be used for constructing a fan chart
%modname="mod"

'Variable to forecast (has to be one of the model variables)
%varname="p"

smpl {%esmpl}
'=========== Model specification =========
'replace the two lines below with your model of inflation if you want
equation eq1.ls {%varname} c {%varname}(-1) {%varname}(-2)
eq1.makemodel({%modname}) 
'=============== Main Fan Chart Program==============
%quant_array="90 "
!quant_step=90/!numb
!k=90-!quant_step
while !k>5 
%quant_array=%quant_array+@str(@round(!k))+" "
!k=!k-!quant_step
wend
%quant_array=%quant_array+"1"


delete(noerr) g2
group g2

for %quant {%quant_array}  
	!alfa={%quant} 'current confidence level
	smpl {%fsmpl}
	!alfa_ratio=!alfa/100 
	
	!num_simul=100000

	if @abs(!boot-1)<0.0001 then 'boot strap errors
		if @abs(!coefu-1)<0.0001 then 'coefficient uncertainty
			{%modname}.stochastic(i=b, b=!alfa_ratio,c=t,r=!num_simul)
		else 'no coefficient uncertainty
			{%modname}.stochastic(i=b, b=!alfa_ratio,r=!num_simul)
		endif
	else 'normal errors
		if @abs(!coefu-1)<0.0001 then 'coefficient uncertainty
			{%modname}.stochastic(i=n, b=!alfa_ratio,c=t,r=!num_simul)
		else 'no coefficient uncertainty
			{%modname}.stochastic(i=n, b=!alfa_ratio,r=!num_simul)
		endif
	endif	
	

	{%modname}.solveopt(s=b,d=d)
	{%modname}.solve
	
	%first_elem_fsmpl=@wleft(%fsmpl,1)
	%second_elem_fsmpl=@wright(%fsmpl,1)
	
	smpl {%first_elem_fsmpl} {%first_elem_fsmpl}
	{%varname}_0h={%varname}
	{%varname}_0l={%varname}
	
	'delete(noerr) smpl_gr
	'sample smpl_gr {%first_elem_fsmpl}-6 {%second_elem_fsmpl}
	smpl {%first_elem_fsmpl}-6 {%second_elem_fsmpl}
	if !alfa<3 then
		%quant="0"
	endif
	series {%varname}_{%quant}h_={%varname}_0h
	series {%varname}_{%quant}l_={%varname}_0l
	g2.add {%varname}_{%quant}l_ {%varname}_{%quant}h_

next
series p1={%varname}
g2.add {%varname} p1
freeze(fanchart, mode=overwrite) g2.band(o="modern")

fanchart.legend -display
for !i=1 to !numb+1
	!r=@round(0.2*255)
	!b=0
	!g=@round((1-0.175*@sqrt(!i) )*255)
	fanchart.setelem(!i) fcolor(@rgb(!r,!g,!b))
next
!numline=!numb+2
fanchart.setelem(!numline) fcolor(blue) lwidth(10)


fanchart.draw(shade,b, gray) {%fsmpl}
show fanchart

smpl @all


