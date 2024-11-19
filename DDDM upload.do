
*independent variable 
lab var X "DDDM"

lab var Para "CNRDS企业数字化转型，段数"
lab var Word "CNRDS企业数字化转型，词数"

gen lnpara=ln(Para+1)
lab var lnpara "ln(Para+1)"
gen lnword=ln(Word+1)
lab var lnword "ln(Word+1)"

*dependent variable
gen Y= ln(income_oversea)
lab var Y "ln(income_oversea)"
gen Y2 = ln(aff+1)
lab var Y2 "ln(aff+1)"


xtset firm year
global XC "ROA  liability lncapital Book_Market Sales_growth top5B  "


*==========================================================================
*                   Table 2
*==========================================================================

reghdfe Y X  $XC, absorb( year firm indfirst province ) vce(clu firm)
keep  if e(sample)   
logout, save(test1.docx) word replace:  tabstat  Y X  $XC, c(s) s(N sd min p25 p50 max) format(%10.4f)



*==========================================================================
*                   Table 3
*==========================================================================
xtset firm year

global V1 "Y"
global V2 "X ROA  liability lncapital Book_Market Sales_growth top5B  "

reghdfe Y X  $XC, absorb( year firm indfirst province ) vce(clu firm)

corr2docx  $V1 $V2  if e(sample) using Pearson.docx, spearman(ignore)  replace fmt(%9.4f) title("Pearson") landscape

clear


************************************************************************
*   Table 4 

************************************************************************

reghdfe Y X , absorb( year firm  ) vce(clu firm)
est store B1
reghdfe Y X $XC , absorb( year firm ) vce(clu firm)
est store B2
reghdfe Y X  $XC, absorb( year firm indfirst province ) vce(clu firm)
est store B3


outreg2 [B1 B2 B3  ] using Baseline.doc,dec(4) adj replace 


************************************************************************
  ***     Table 5
************************************************************************

xtset firm year

bys  indfirst province year  : egen FF= sum (X)
bys  indfirst province year : egen EF= count(X)
gen IVM= (FF-X)/(EF-1)

bys year: egen aver_x=mean(X)
xtset firm year
gen dx=aver_x-l.aver_x
gen IVB=l.IVM*dx

ivreghdfe Y $XC (X=   IVB  ), absorb( year firm indfirst province)  cluster (firm)  first

est store IV

outreg2 [IV ] using MD.doc,dec(4) replace 


************************************************************************

*  Table 6
************************************************************************




reghdfe Y lnword $XC, absorb( year firm indfirst  province ) vce(clu firm)
est store R1

reghdfe Y lnpara $XC, absorb( year firm indfirst  province ) vce(clu firm)
est store R2

reghdfe Y2 X $XC, absorb( year firm indfirst  province ) vce(clu firm)
est store  R3

ppmlhdfe Y X $XC, absorb( year firm indfirst  province ) vce(clu firm)
est store  R4

reghdfe Y X  $XC  if year>2008, absorb( year firm indfirst  province ) vce(clu firm)
est store  R5

*把疫情爆发年T除，结果更好。
reghdfe Y X  $XC  if year!=2020, absorb( year firm indfirst  province ) vce(clu firm)

outreg2 [R1 R2 R3 R5 R4] using Robust.doc,dec(4)  replace 


************************************************************************
* Table 7
************************************************************************

reghdfe Monitor X $XC, absorb( year firm indfirst  province ) vce(clu firm)
est store MA1

reghdfe Accidents  X $XC, absorb( year firm indfirst  province ) vce(clu firm)
est store MA2

reghdfe Illegal  X $XC, absorb( year firm indfirst province ) vce(clu firm)
est store MA3
reghdfe ISO9001 X $XC, absorb( year firm indfirst province ) vce(clu firm)
est store MA4
reghdfe ISO14001 X $XC, absorb( year firm indfirst  province ) vce(clu firm)
est store MA5


outreg2 [MA1 MA2  MA3 MA4 MA5] using MA.doc,dec(4) adj replace 

************************************************************************
* Table 8

************************************************************************

gen Green_Invention =ln(当年独立申请的绿色发明数量+1)
gen Green_Utility=ln(当年独立申请的绿色实用新型数量+1)
gen Green_sum =ln(当年独立申请的绿色实用新型数量+ 当年独立申请的绿色发明数量+1)

reghdfe Green_Invention X $XC, absorb( year firm indfirst province ) vce(clu firm)
est store MB1

reghdfe Green_Utility X $XC, absorb( year firm indfirst  province ) vce(clu firm)
est store MB2

reghdfe Green_sum X $XC, absorb( year firm indfirst  province ) vce(clu firm)
est store MB3

outreg2 [MB1 MB2 MB3 ] using MB.doc,dec(4) adj  replace 
 
 ************************************************************************
* table 9

************************************************************************

reghdfe E得分 X $XC, absorb( year firm indfirst province ) vce(clu firm)
est store MC1


reghdfe S得分 X $XC, absorb( year firm indfirst  province ) vce(clu firm)
est store MC2


reghdfe G得分 X $XC, absorb( year firm indfirst  province ) vce(clu firm)
est store MC3


reghdfe ESG得分 X $XC, absorb( year firm indfirst  province ) vce(clu firm)
est store MC4

outreg2 [MC1 MC2 MC3  MC4] using MC.doc,dec(4) adj replace 


 
************************************************************************
* table 10
************************************************************************


reghdfe analyst X $XC, absorb( year firm indfirst  province ) vce(clu firm)
est store MD1

reghdfe reported X $XC, absorb( year firm indfirst  province ) vce(clu firm)
est store MD2
 
reghdfe positive  X $XC, absorb( year firm indfirst  province ) vce(clu firm)
est store MD3


outreg2 [MD1 MD2 MD3] using MD.doc,dec(4) adj replace 



************************************************************************
*  Table 11
* 
************************************************************************


reghdfe Y c.X##c.HHIA $XC, absorb( year firm indfirst  province ) vce(clu firm)
est store  H1

reghdfe Y c.X##c.HHID $XC, absorb( year firm indfirst  province ) vce(clu firm)
est store  H2

reghdfe Y c.X##c.foreign $XC, absorb( year firm indfirst  province ) vce(clu firm)
est store  H3

reghdfe Y c.X##c.SOE $XC, absorb( year firm indfirst  province ) vce(clu firm)
est store  H4

outreg2 [H1 H2 H3 H4 ] using Hetero.doc,dec(4)  adj replace 
