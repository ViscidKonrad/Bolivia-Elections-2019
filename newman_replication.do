set more off
clear all
set type double
*	This program will seek and create files in the current working directory
*	Please adjust as desired
*cd ~/Newman

capture: program drop getNewmanData
program define getNewmanData

	syntax [, DROP]

	tempfile tf
	use ../Summaries/computo/acta.2019.10.25.21.09.30, clear
	keep if regexm(Ele,"Pres")
	gen time = 0
	save `tf'
	use ../Summaries/trep/acta.2019.10.20.19.40.57, clear
	keep if regexm(Ele,"Pres")
	*	Per Newman...
	*drop if NúmeroMesa==2433
	gen time = 1
	append using `tf'
	foreach var of varlist Pa-Rec {
		bys NúmeroMesa (time): replace `var' = `var'[1]
	}
	egen sumV = rowtotal(CC-PANBOL)
	if ("`drop'"=="drop") {
		drop if sumV==0
	}

	gen isForeign = Pa~="Bolivia"
	egen fcode = group(isForeign)
	egen pcode = group(fcode Pa)
	egen dcode = group(pcode Dep)
	egen pdcode = group(dcode Pro)
	egen mcode = group(pdcode Mun)
	egen lcode = group(mcode Loc)
	egen rcode = group(lcode Rec)

	bys NúmeroMesa: egen num_mesa = total(1)
	gen early_mesa = num_mesa>1
	lab def early_mesa 0 "late" 1 "early"
	lab val early_mesa early_mesa
	drop num_mesa
	keep if time==0
	gen margin = 100*(MAS-CC)/sumV

end

capture: program drop doiter
program define doiter, rclass

	syntax, [GRAPH] [Cut(real -1000)] [SEED(integer 20200504)] [LEFT] [RIGHT] ///
		[EXPORT(string asis)] [NODist] [OVerlay] [NEWtest] [EARLYonly] [ACTUAL]
		
	*	This program produces a graph/KS test for the CEPR response to Newman
	*		as specified by the above syntax
	
	if (`cut'<-100) {
		local right right
	}
	if ("`right'"=="right") {
		if ("`left'"=="left") {
			local left
		}
	}
	if ("`left'`right'"=="") {
		local right right
	}
	if ("`export'"~="") {
		local graph graph
	}
	macro li _cut
	di "`left'`right'"
	
	if ("`actual'"=="actual") {
		
		getNewmanData, drop
		collapse (sum) MAS CC sumV, by(rcode early)
		gen margin = 100*(MAS-CC)/sumV
		
		keep margin rcode early
		reshape wide margin, i(rcode) j(early)
		count
		
		ren rcode n
		ren margin1 m_early
		ren margin0 m_late
		
		keep if ~mi(m_early) & ~mi(m_late)
		
	}
	else {
		preserve
		clear
		set obs 1500
		set seed `seed'

		gen n = _n
		*	rM: percentage of support (not vote!!) for one side
		gen rM = rbeta(11,9)
		*	rE: percentage of the vote comes early
		local fac (0)
		gen rE = rbeta(15-`fac'*(rM-0.55),3+`fac'*(rM-0.55))
		
		*	nE: number of early VOTERS (vs late, total 1200) 
		gen nE = rbinomial(1200,rE)
		*	n_early: number of actual VOTES for one side in early voting
		gen n_early = rbinomial(nE,rM)
		*	n_late: number of actual VOTES for one side in late voting
		gen n_late = rbinomial(1200-nE,rM)
		
		sum rE
		sum nE
		ret li
		local ne = `r(sum)'
		sum n_early
		di 200*`r(sum)'/`ne'-100
		sum n_late
		di 200*`r(sum)'/(_N*1200-`ne')-100
		
		*	margins for the one side
		gen m_early = 200*n_early/nE-100
		gen m_late = 200*n_late/(1200-nE)-100

		*	error in variables adjustment check
		gen e_early = m_early-100*(2*rM-1)
		sum e_early
		local eeV = `r(Var)'
		sum m_early
		local meV = `r(Var)'
		sum m_late
		local mlV = `r(Var)'
		
		/*
		rho = cov(X,Y)/sqrt(Vx*Vy)
		beta = cov(X,Y)/Vx
		so rho = beta*Vx/sqrt(Vx*Vy) = beta*sqrt(Vx/Vy)
		*/
		
		reg m_late m_early
		di (1/(1+`eeV'/`meV'))
		correl m_late m_early
		di (1/(1+`eeV'/`meV'))*sqrt(`meV'/`mlV')
		
	}
			
	gen ism1 = m_early>=`cut'
	gen ism2 = m_late+m_early>=`cut'
	
	local isnum =cond("`newtest'"=="newtest",2,1)
	local isnval =cond("`left'"=="left",1,0)
	macro li _isnval
	if ("`graph'"=="graph") {
		local f 0.5
		local h 1
		gen mer = round(floor(`f'*m_early)+`f'/2)/`f'
		bys mer: egen med = total(ism`isnum'~=`isnval')
		replace med = med/`h'-100
		gen mlr = round(floor(`f'*m_late)+`f'/2)/`f'
		bys mlr: egen mld = total(ism`isnum'~=`isnval')
		replace mld = mld/`h'-100
		local dcom
		sort m_early m_late
		egen mertag = tag(mer) if ism`isnum'~=`isnval'
		egen mlrtag = tag(mlr) if ism`isnum'~=`isnval'
		
		local c1 ceprgreen
		local c2 ceprgraymedium%75
		local c3 ceprgray
		
		if ("`nodist'"~="nodist") {
			local dcom (dropline med mer if mertag, base(-100) msym(none) color(`c1'))
			if ("`earlyonly'"~="earlyonly") {
				local dcom `dcom' (dropline mld mlr if mlrtag, base(-100) msym(none) horiz color(`c2'))
				if ("`overlay'"~="") {
					local dcom `dcom' (dropline mld mlr if mlrtag, base(-100) msym(none) color(`c2'))
					if ("`newtest'"~="newtest") {
						if ("`left'"=="left") {
							*local dcom `dcom' (dropline mld mlr if mlr>`cut', base(-100) msym(none) color(ceprlightblue%20)) 
						}
						if ("`right'"=="right") {
							*local dcom `dcom' (dropline mld mlr if mlr<`cut', base(-100) msym(none) color(`c2')) 
						}
					}
				}
			}
		}
		twoway (scatter m_late m_early, msize(0.15) mcolor(ceprblue%15)) `dcom' ///
			if ism`isnum'~=`isnval', name(main, replace) xsc(r(-100 100)) ysc(r(-100 100)) ///
			xlab(-100(50)100) ylab(-100(50)100) legend(off) aspect(1) ///
			yline(`cut', lc(ceprgraymedium)) xti(" " "Margin on early votes in precinct") ///
			yti("Margin on late votes in precinct")
		graph export `export'.png, replace
	}

	reshape long m_, i(n) j(time) string
	lab def early 0 "late" 1 "early"
	gen early:early = time=="early"
	tab ism`isnum' if ~mi(m_)
	ksmirnov m_, by(early)
	ret li
	ksmirnov m_ if ism`isnum'~=`isnval', by(early)
	ret li
	
end

*
*	Tables 12+
*

getNewmanData
gen amarg = MAS-CC
lab def group 0 "Entirely early" 1 "Entirely late" 2 "Others" 3 " - Early" 4 " - Late"

*	by precinct
bys rcode: egen tr = total(1), missing
bys rcode: egen er = total(early), missing
gen r_group = (er==0)+2*(er>0 & er<tr)
gen r_group2 = 3+(early==0) if er>0 & er<tr
bys rcode r_group: egen r_sumV = total(cond(early,sumV,.)), missing
bys rcode r_group: egen r_margin = total(cond(early,100*(MAS-CC)/r_sumV,.)), missing
replace r_margin = margin if mi(r_margin)
gen r_cf = sumV*r_margin/100

*	by locality
bys lcode: egen tl = total(1), missing
bys lcode: egen el = total(early), missing
gen l_group = (el==0)+2*(el>0 & el<tl)
gen l_group2 = 3+(early==0) if el>0 & el<tl
bys lcode l_group: egen l_sumV = total(cond(early,sumV,.)), missing
bys lcode l_group: egen l_margin = total(cond(early,100*(MAS-CC)/l_sumV,.)), missing
gen l_cf = sumV*l_margin/100

*	label groups
lab val r_group r_group2 l_group l_group2 group

*	Table 12
table r_group2, c(sum sumV sum amarg sum r_cf) row col format(%20.0fc)
table r_group2 [w=sumV], c(mean margin mean r_margin) row col format(%4.2fc)

*	Table 13
table l_group2, c(sum sumV sum amarg sum l_cf) row col format(%20.0fc)
table l_group2 [w=sumV], c(mean margin mean l_margin) row col format(%4.2fc)

*	Table 14
table r_group if ~mi(l_group2) & r_group<2, c(sum sumV sum amarg) row col format(%20.0fc)
table r_group if ~mi(l_group2) & r_group<2 [w=sumV], c(mean margin) row col format(%4.2fc)

*	Table 15
table r_group, c(sum sumV sum amarg sum r_cf) row col format(%20.0fc)
table r_group [w=sumV], c(mean margin mean r_margin) row col format(%4.2fc)

*
*	Appendix figure
*
twoway (function min(normal(x)/normal(1),1), range(-3 3)) ///
	(function normal(x), range(-3 3) color(ceprlightblue%50)) ///
	(pcarrowi 1 1 `=normal(1)' 1, color(ceprorange%75)) ///
	 ,xlab(-3/3) legend(off) ///
	xti(" " "Z") yti("Pr(z<Z)") text(`=(1.05+normal(1))/2' 1.25 "D{superscript:－}", color(ceprorange%75)) ///
	name(noright, replace) aspect(1)
twoway (function max(1-(1-normal(x))/(1-normal(-1)),0), range(-3 3)) ///
	(function normal(x), range(-3 3) color(ceprlightblue%50)) ///
	(pcarrowi 0 -1 `=normal(-1)' -1, color(ceprorange%75)) ///
	 ,xlab(-3/3) legend(off) ///
	xti(" " "Z") yti("Pr(z<Z)") text(`=(-0.05+normal(-1))/2' -1.25 "D{superscript:＋}", color(ceprorange%75)) ///
	name(noleft, replace) aspect(1)
graph combine noright noleft
graph export anfig.png, replace

*
*	Figure 8: the bleed test
*
set seed 20050505
clear
set obs 1000
gen iter = _n
gen n = .
gen tail = .
gen Dval = .
gen pval = .
qui forvalues ii=1/`=_N' {

	noi di as text "Iter: " as result `ii'
	
	preserve
	clear
	set obs 1500
	gen n = _n
	
	scalar cut = 200*(rbeta(10,10)-0.5)

	*	rM: percentage of support (not vote!!) for one side
	gen rM = rbeta(11,9)
	*	rE: percentage of the vote comes early
	local fac (0)
	gen rE = rbeta(15-`fac'*(rM-0.55),3+`fac'*(rM-0.55))
		
	*	nE: number of early VOTERS (vs late, total 1200) 
	gen nE = rbinomial(1200,rE)
	*	n_early: number of actual VOTES for one side in early voting
	gen n_early = rbinomial(nE,rM)
	*	n_late: number of actual VOTES for one side in late voting
	gen n_late = rbinomial(1200-nE,rM)
	
	*	margins for the one side
	gen m_early = 200*n_early/nE-100
	gen m_late = 200*n_late/(1200-nE)-100
	
	gen ism = m_early>=cut
	count if m_early>=cut & m_late<cut
	scalar tsave = r(N)
	count if ism==1
	scalar nsave = r(N)
	if (nsave) {
		reshape long m_, i(n) j(time) string
		ksmirnov m_ if ism==1, by(time)
		ret li
		scalar Dsave = r(D)
		scalar psave = r(p)
	}
	else {
		scalar Dsave = .
		scalar psave = .
	}
	restore
	replace n = nsave if iter==`ii'
	replace tail = tsave if iter==`ii'
	replace Dval = Dsave if iter==`ii'
	replace pval = psave if iter==`ii'
	
}
gen tf = 100*tail/n
twoway (scatter Dval tf, msize(tiny) color(ceprblue%5)) ///
	, xti(" " "Bleed Rate ({it:p})") ///
	yti("Kolmogorov-Smirnov Test Statistic ({it:D})" " ") aspect(1) xlab(0(10)50)
graph export nfig8.png, replace

*
*	Figure 1
*
set seed 20200504
clear
set obs 50000
gen rE = rbeta(25,5)
gen nE = rbinomial(1200,rE)
gen rM = rbeta(15,3)
replace rM = 0.5
gen n_early = rbinomial(nE,rM)
gen n_late = rbinomial(1200-nE,rM)
gen m_early = 100*(2*n_early/nE-1)
gen m_late = 100*(2*n_late/(1200-nE)-1)
ellip m_late m_early, nog gen(ex ey)  c(chi2) level(95)
sum m_late
local mlv = r(Var)
sum m_early
di 1+`r(Var)'/`mlv'
correl m_early m_late
sum m_late
local r = max(`r(max)',-`r(min)')1
local rc = 20*ceil(`r'/20)-20
twoway (scatter m_late m_early, msize(vtiny) color(ceprblue%5)) ///
	(line ex ey), xsc(r(-`r' `r')) ysc(r(-`r' `r')) ///
	xlab(-`rc'(20)`rc') ylab(-`rc'(20)`rc') legend(off) ///
	xti(" " "Margin on early votes in precinct") ///
			yti("Margin on late votes in precinct") aspect(1)
graph export nfig1.png, replace

*
*	Figures 2-4 build up slowly to Figure 5
*	 - also Table 1
*
doiter, export(nfig2) nod 
doiter, export(nfig3) earlyonly
doiter, export(nfig4)
doiter, export(nfig5) ov

*
*	Figures 6-7, 9-10 and Tables 2-5 repeat with split data
*
doiter, ov left cut(0) export(nfig6) 
doiter, ov right cut(0) export(nfig7)
doiter, ov left cut(0) newt export(nfig9) 
doiter, ov right cut(0) newt export(nfig10)

*
*	Tables 6-9: KS tests on simulated data
*
doiter, ov left cut(20)
doiter, ov right cut(20)
doiter, ov left cut(20) newt
doiter, ov right cut(20) newt

*
*	Tables 10-11: Replication and reanalysis
*
doiter, actual ov left cut(0)
doiter, actual ov right cut(0)
doiter, actual ov left cut(0) new
doiter, actual ov right cut(0) new
