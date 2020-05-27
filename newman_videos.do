set more off
clear all
set type double
*	This program will seek and create files in the current working directory
*	Please adjust as desired
*cd ~/Newman

*
*	Note that this program uses ffmpeg to compile PNG files into MPG/GIF
*	 - Use --novideo-- option to create the frames but not call ffmpeg
*

capture: program drop doiter
program define doiter, rclass

	syntax, [GRAPH] [Cut(real -1000)] [SEED(integer 20200504)] [LEFT] [RIGHT] ///
		[EXPORT(string asis)] [NODist] [OVerlay] [NEWtest] [EARLYonly] [ACTUAL] ///
		[H(real 100)] [DH(real -1)] [ZERO] [NDT(integer 60)] ///
		[TAlpha(real 15)] [TBeta(real 3)] [VAlpha(real 11)] [VBeta(real 9)] ///
		[FRAUDNote(string asis)] [FRAUDLAB(string asis)] [NOFrames] [SHOWFRAMES] ///
		[REPAIR(numlist integer)] [NOVIDEO] [NOGIF]
		
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
	
	if ("`fraudlab'"=="") {
		local fraudlab Fraud?
	}
	if ("`repair'"=="") {
		forvalues ii=0/`=3*`ndt'+4' {
			local repair `repair' `ii'
		}
	}
	
	local dh = max(`dh',`h'/2)

	
	if ("`actual'"=="actual") {
		
		tempfile tf
		use acta.2019.10.25.21.09.30, clear
		keep if regexm(Ele,"Pres")
		gen time = 0
		save `tf'
		use acta.2019.10.20.19.40.57, clear
		keep if regexm(Ele,"Pres")
		*	Per Newman...
		drop if NúmeroMesa==2433
		gen time = 1
		append using `tf'
		foreach var of varlist Pa-Rec {
			bys NúmeroMesa (time): replace `var' = `var'[1]
		}
		egen sumV = rowtotal(CC-PANBOL)
		drop if sumV==0

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
		
		collapse (sum) MAS CC sumV, by(rcode early)
		gen margin = 100*(MAS-CC)/sumV
		
		keep margin rcode early sumV
		reshape wide margin sumV, i(rcode) j(early)
		gen eshare = 1/(1+sumV0/sumV1)
		count
		
		ren rcode n
		ren margin1 m_early
		ren margin0 m_late
		
		keep if ~mi(m_early) & ~mi(m_late)
		
	}
	else {
		*preserve
		clear
		set obs 1500
		set seed `seed'

		gen n = _n
		*	rM: percentage of support (not vote!!) for one side
		if ("`zero'"=="zero") {
			gen rM = 0.5
		}
		else {
			gen rM = rbeta(`valpha',`vbeta')
		}
		*	rE: percentage of the vote comes early
		gen rE = rbeta(`talpha',`tbeta')
		
		*	nE: number of early VOTERS (vs late, total 1200) 
		gen nE = rbinomial(1200,rE)
		*	n_early: number of actual VOTES for one side in early voting
		gen n_early = rbinomial(nE,rM)
		*replace n_early = nE*rM
		*	n_late: number of actual VOTES for one side in late voting
		gen n_late = rbinomial(1200-nE,rM)
		
		gen eshare = nE/1200
		
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
	
	
	*	Produce data splits
	gen ism1 = m_early>=`cut'
	gen ism2 = m_late+m_early>=`cut'
	local isnum =cond("`newtest'"=="newtest",2,1)
	local isnval =cond("`left'"=="left",1,0)
	macro li _isnval
	
	keep if ism`isnum'~=`isnval'
	
	*	KS Test (for graphing purposes)
	preserve
		reshape long m_, i(n) j(time) string
		lab def early 0 "late" 1 "early"
		gen early:early = time=="early"
		tab ism`isnum' if ~mi(m_)
		ksmirnov m_ if ism`isnum'~=`isnval', by(early)
		ret li
		local rD = r(D)
		local rp = r(p)
		local rp2 = r(p_2)
		local rp1 = r(p_1)
	restore
	
	if (`rp1'>0.05) {
		local nodplus nodplus
	}
	if (`rp2'>0.05) {
		local nodminus nodminus
	}
	local rpt: di %5.4fc `rp'
	local dft: di %5.4fc `rD'
	
	sort m_late m_early
	sum m_early, meanonly
	local memin = `r(min)'
	local memax = `r(max)'
	gen enum = .
	gen lnum = .
	forvalues ii=1/`=_N' {
		count if m_early<=m_late[`ii']
		replace enum = `r(N)' in `ii'
		count if m_late<=m_late[`ii']
		replace lnum = `r(N)' in `ii'
	}
	gen dnum = lnum-enum
	sum dnum, meanonly
	local dplus = `r(max)'
	local dminus = `r(min)'
	sum m_late if dnum==`dplus'
	local dpx = `r(mean)'
	sum enum if dnum==`dplus'
	local dpe = `r(mean)'
	sum lnum if dnum==`dplus'
	local dpl = `r(mean)'
	sum m_late if dnum==`dminus'
	local dmx = `r(mean)'
	sum enum if dnum==`dminus'
	local dme = `r(mean)'
	sum lnum if dnum==`dminus'
	local dml = `r(mean)'
	
	local dpa = (`dpe'+`dpl')/2
	local dma = (`dme'+`dml')/2

	if ("`graph'"=="graph") {
	
		local f 1
		gen mer = (floor(`f'*m_early)+0.5)/`f'
		replace mer = round(m_early)
		gen mlr = (floor(`f'*m_late)+0.5)/`f'
		replace mlr = round(m_late)
		bys mlr: egen mld = total(ism`isnum'~=`isnval')
		local g = -10
		local dy = 1
		local dt = 0.5
		sort m_late m_early
		gen xf = sum(`h'/2)/_N-`h'
		sort m_early m_late
		gen ny = _n
		gen yf = sum(`h'/2)/_N-`h'
				
		local offset = `h'/20
		local dpa = `dpa'*`h'/2/_N-`h'+`offset'
		local dma = `dma'*`h'/2/_N-`h'
		local dpx = `dpx'
		local dmx = `dmx'-`offset'
		local asy = (`dma'+`dpa')/2
		local asx = `dpx'+(`dpx'-`dmx')/2
		local g -100
		local nframe 0
		
		local color0 "0 148 130"
		local color1 "0 1 85"
		local color2  "0 79 167"
		local color3 "255 95 0"%75
		
		capture: mkdir `export'
		if ("`noframes'"~="noframes") {
			capture: mkdir `export'/PNG
			local alist: dir "`export'/PNG" files "*.png"
			qui foreach af of local alist {
				local fn `=substr("`af'",length("`export'")+1,3)'
				macro li _af _fn
				if (`fn'>3*`ndt'+4) {
					noi di as text `"! rm -rf `export'/PNG/`af'"'
					! rm -rf `export'/PNG/`af'
				}
			}
			capture: mkdir `export'/PNG
			gen yc = .
			gen tf = (_n-1)/(_N-1)
			gen dist = m_late-yf
			gen g = cond(dist<0,-1,1)*(`g')
			gen dt = sqrt(dist*2/(-g))
			gen t0 = tf-dt
			sum t0, meanonly
			local tmin = `r(min)'
			local dt = (1-`tmin')/`ndt'
			local capt Here is `=cond("`actual'"=="actual","the actual","some simulated")' data
			local captf `capt'
			if ("`showframes'"=="showframes") {
				local captf `nframe': `capt'
			}
			*
			*	Frame #0
			*	 - bare data
			*
			if (`: list nframe in repair') {
				twoway ///		
					(scatter m_late m_early if abs(m_late)<`h' & abs(m_early)<`h', color(`color1') msize(vtiny)) ///
					, xsc(r(-`h' `h')) ysc(r(-`h' `h')) ///
						xlab(-`h'(`dh')`h') ylab(-`h'(`dh')`h') legend(off) aspect(1) ///
						/* xline(`cut', lc(ceprgraymedium)) */ xti(" " "Margin on early votes in precinct") ///
						yti("Margin on late votes in precinct") ///
						caption(`"`captf'"')
				local frname = "0"*(3-length("`nframe'"))+"`nframe'"
				graph export `export'/PNG/`export'`frname'.png, replace
			}
			local capt Add lines to the right of each observation
			*
			*	Frames #1 to N
			*	 - initial lines to nearly complete CDF
			*
			forvalues ts = 0/`=`ndt'-1' {
				local ++nframe
				local captf `capt'
				if ("`showframes'"=="showframes") {
					local captf `nframe': `capt'
				}
				local time = `dt'*`ts'+`tmin'
				replace yc = max(yf,m_late+(`g')*(max(0,`time'-t0)^2)/2)
				if (`: list nframe in repair') {
					twoway ///		
						(scatter m_late m_early if abs(m_late)<`h' & abs(m_early)<`h', color(`color1') msize(vtiny)) ///
						(dropline m_early yc if abs(m_early)<`h' & abs(yc)<`h', color(`color1') base(`h') horiz msize(vtiny) lw(vvthin)) ///	
						, xsc(r(-`h' `h')) ysc(r(-`h' `h')) ///
							xlab(-`h'(`dh')`h') ylab(-`h'(`dh')`h') legend(off) aspect(1) ///
							/* xline(`cut', lc(ceprgraymedium)) */ xti(" " "Margin on early votes in precinct") ///
							yti("Margin on late votes in precinct") ///
							caption(`"`captf'"')
					local frname = "0"*(3-length("`nframe'"))+"`nframe'"
					graph export `export'/PNG/`export'`frname'.png, replace
				}
				local capt Let the lines fall
			}
			local ++nframe
			local captf `capt'
			if ("`showframes'"=="showframes") {
				local captf `nframe': `capt'
			}
			replace yc = yf
			*
			*	Frame #(N+1)
			*	 - first CDF
			*
			if (`: list nframe in repair') {
				twoway ///		
					(scatter m_late m_early if abs(m_late)<`h' & abs(m_early)<`h', color(`color1') msize(vtiny)) ///
					(dropline m_early yc if abs(m_early)<`h' & abs(yc)<`h', color(`color1') base(`h') horiz msize(vtiny) lw(vvthin)) ///	
					, xsc(r(-`h' `h')) ysc(r(-`h' `h')) ///
						xlab(-`h'(`dh')`h') ylab(-`h'(`dh')`h') legend(off) aspect(1) ///
						/* xline(`cut', lc(ceprgraymedium)) */ xti(" " "Margin on early votes in precinct") ///
						yti("Margin on late votes in precinct") ///
						caption(`"`captf'"')
				local frname = "0"*(3-length("`nframe'"))+"`nframe'"
				graph export `export'/PNG/`export'`frname'.png, replace
			}
			*
			*	Frames #(N+2) to (2N+1)
			*	 - fading out (2N+1 is fully faded)
			*
			forvalues ts=1/`ndt' {
				local ++nframe
				local captf `capt'
				if ("`showframes'"=="showframes") {
					local captf `nframe': `capt'
				}
				if (`: list nframe in repair') {
					twoway ///		
						(scatter m_late m_early if abs(m_late)<`h' & abs(m_early)<`h', color(`color1') msize(vtiny)) ///
						(dropline m_early yc if abs(m_early)<`h' & abs(yc)<`h', color(`color1'%`=round(100*(1-`ts'/`ndt'))') base(`h') horiz msize(vtiny) lw(vvthin)) ///	
						(function -`h', range(-`h' `=max(-`h',`memin')') color(`color1')) ///
						(function -`h'/2, range(`=min(`h',`memax')' `h') color(`color1')) ///
						(line yc m_early if abs(m_early)<`h' & abs(yc)<`h', color(`color1')) ///	
						, xsc(r(-`h' `h')) ysc(r(-`h' `h')) ///
							xlab(-`h'(`dh')`h') ylab(-`h'(`dh')`h') legend(off) aspect(1) ///
							/* xline(`cut', lc(ceprgraymedium)) */ xti(" " "Margin on early votes in precinct") ///
							yti("Margin on late votes in precinct") ///
							caption(`"`captf'"')
					local frname = "0"*(3-length("`nframe'"))+"`nframe'"
					graph export `export'/PNG/`export'`frname'.png, replace
				}
			}

			local ++nframe
			local capt "Swap the axes"
			local captf `capt'
			if ("`showframes'"=="showframes") {
				local captf `nframe': `capt'
			}
			gen xc = m_early
			sort m_late m_early
			replace tf = (_n-1)/(_N-1)
			replace dist = m_early-xf
			replace g = cond(dist<0,-1,1)*(`g')
			replace dt = sqrt(dist*2/(-g))
			replace t0 = tf-dt
			sum t0, meanonly
			local tmin = `r(min)'
			local dt = (1-`tmin')/`ndt'
			sort m_early m_late
			*
			*	Frame #(2N+2)
			*	 - swapped axes
			*
			if (`: list nframe in repair') {
				twoway ///		
					(scatter m_early m_late if abs(m_late)<`h' & abs(m_early)<`h', color(`color1') msize(vtiny)) ///
					(function -`h', range(-`h' `=max(-`h',`memin')') color(`color1')) ///
					(function -`h'/2, range(`=min(`h',`memax')' `h') color(`color1')) ///
					(line yc m_early if abs(m_early)<`h' & abs(yc)<`h', color(`color1')) ///	
					, xsc(r(-`h' `h')) ysc(r(-`h' `h')) ///
						xlab(-`h'(`dh')`h') ylab(-`h'(`dh')`h') legend(off) aspect(1) ///
						/* xline(`cut', lc(ceprgraymedium)) */ xti(" " "Margin on late votes in precinct") ///
						yti("Margin on early votes in precinct") ///
						caption(`"`captf'"')
				local frname = "0"*(3-length("`nframe'"))+"`nframe'"
				graph export `export'/PNG/`export'`frname'.png, replace
			}
			local capt Add lines to the right of each point
			*
			*	Frames #(2N+3) to (3N+2)
			*	 - intial lines to nearly complete CDF
			*
			forvalues ts = 0/`=`ndt'-1' {
				local ++nframe
				local captf `capt'
				if ("`showframes'"=="showframes") {
					local captf `nframe': `capt'
				}
				local time = `dt'*`ts'+`tmin'
				replace xc = max(xf,m_early+(`g')*(max(0,`time'-t0)^2)/2)
				if (`: list nframe in repair') {
					twoway ///		
						(scatter m_early m_late if abs(m_late)<`h' & abs(m_early)<`h', color(`color2') msize(vtiny)) ///
						(dropline m_late xc if abs(m_late)<`h' & abs(xc)<`h', color(`color2') base(`h') horiz msize(vtiny) lw(vvthin)) ///	
						(function -`h', range(-`h' `=max(-`h',`memin')') color(`color1')) ///
						(function -`h'/2, range(`=min(`h',`memax')' `h') color(`color1')) ///
						(line yc m_early if abs(m_early)<`h' & abs(yc)<`h', color(`color1')) ///	
						, xsc(r(-`h' `h')) ysc(r(-`h' `h')) ///
							xlab(-`h'(`dh')`h') ylab(-`h'(`dh')`h') legend(off) aspect(1) ///
							/* xline(`cut', lc(ceprgraymedium)) */ xti(" " "Margin on late votes in precinct") ///
							yti("Margin on early votes in precinct") ///
							caption(`"`captf'"')
					local frname = "0"*(3-length("`nframe'"))+"`nframe'"
					graph export `export'/PNG/`export'`frname'.png, replace
				}
				local capt Let the lines fall
				
			}
			local ++nframe
			replace xc = xf
			*
			*	Frame #(3N+3)
			*	 - complete CDF
			*
			if (`: list nframe in repair') {
				twoway ///		
					(scatter m_early m_late if abs(m_late)<`h' & abs(m_early)<`h', color(`color2') msize(vtiny)) ///
					(dropline m_late xc if abs(m_late)<`h' & abs(xc)<`h', color(`color2') base(`h') horiz msize(vtiny) lw(vvthin)) ///	
					(function -`h', range(-`h' `=max(-`h',`memin')') color(`color1')) ///
					(function -`h'/2, range(`=min(`h',`memax')' `h') color(`color1')) ///
					(line yc m_early if abs(m_early)<`h' & abs(yc)<`h', color(`color1') ) ///	
					, xsc(r(-`h' `h')) ysc(r(-`h' `h')) ///
						xlab(-`h'(`dh')`h') ylab(-`h'(`dh')`h') legend(off) aspect(1) ///
						/* xline(`cut', lc(ceprgraymedium)) */ xti(" " "Margin on late votes in precinct") ///
						yti("Margin on early votes in precinct") ///
						caption(`"`captf'"')
				local frname = "0"*(3-length("`nframe'"))+"`nframe'"
				graph export `export'/PNG/`export'`frname'.png, replace
			}
			local ++nframe
			di "`nodplus' `nodminus'"
			local pcom
			local ccom
			if ("`nodplus'"=="") {
				local pcom `asy' `asx' `dpa' `dpx'
				local ccom (scatter m_early m_late if abs(m_late)<`h' & abs(m_early)<`h' & m_late<=`dpx', color(`color3') msize(vtiny))
			}
			if ("`nodminus'"=="") {
				local pcom `pcom' `asy' `asx' `dma' `dmx'
				local ccom `ccom' (scatter m_early m_late if abs(m_late)<`h' & abs(m_early)<`h' & m_late>=`dmx', color(`color3') msize(vtiny))
			}
			local ccom (scatter m_early m_late if abs(m_late)<`h' & abs(m_early)<`h' & m_early>=`cut', color(`color3') msize(tiny))
			local ccom
			if ("`pcom'"~="") {
				local pcom (pcarrowi `pcom' (9) "`fraudlab'", mlabcol(`color3') color(`color3'))
			}
			if ("`pcom'"=="") {
				local fraudalso No evidence of difference: D= `dft', p = `rpt'
			}
			else {
				local fraudalso Early and late differ: D= `dft', p = `rpt'
			}
			if ("`fraudalso'"~="") {
				if (`"`fraudnote'"'~=`""') {
					local fraudnote `fraudnote' (`fraudalso')
				}
				else {
					local fraudnote `fraudalso'
				}
			}
			local captf `fraudnote'
			if ("`showframes'"=="showframes") {
				local captf `nframe': `fraudnote'
			}
			*
			*	Frame #(3N+4)
			*	 - summary frame
			*
			if (`: list nframe in repair') {
				twoway ///		
					(scatter m_early m_late if abs(m_late)<`h' & abs(m_early)<`h', color(`color2') msize(vtiny)) ///
					(dropline m_late xc if abs(m_late)<`h' & abs(xc)<`h', color(`color2') base(`h') horiz msize(vtiny) lw(vvthin)) ///
					(function -`h', range(-`h' `=max(-`h',`memin')') color(`color1')) ///
					(function -`h'/2, range(`=min(`h',`memax')' `h') color(`color1')) ///
					(line yc m_early if abs(m_early)<`h' & abs(yc)<`h', color(`color1') ) ///	
					`pcom' `ccom' ///
					, xline(`=cond(`dplus'>-`dminus',`dpx',`dmx'+`offset')') xsc(r(-`h' `h')) ysc(r(-`h' `h')) ///
						xlab(-`h'(`dh')`h') ylab(-`h'(`dh')`h') legend(off) aspect(1) ///
						/*xline(`cut', lc(ceprgraymedium)) */ xti(" " "Margin on late votes in precinct") ///
						yti("Margin on early votes in precinct") ///
						caption(`"`captf'"')
				local frname = "0"*(3-length("`nframe'"))+"`nframe'"
				graph export `export'/PNG/`export'`frname'.png, replace
			}
			
		}
		
		if ("`novideo'"~="novideo") {
			if("`nogif'"=="nogif") {
				! /usr/local/bin/ffmpeg ///
					-framerate 60 ///
					-i ./`export'/PNG/`export'%03d.png ///
					-filter_complex ' ///
					   [0:0] loop=loop=100:size=1:start=`=3*`ndt'+4' [v0] , ///
						[v0] loop=loop=15:size=1:start=`=3*`ndt'+3' [v1] , ///
						[v1] loop=loop=15:size=1:start=`=2*`ndt'+3' [v2] , ///
						[v2] loop=loop=15:size=1:start=`=2*`ndt'+2' [v3] , ///
						[v3] loop=loop=15:size=1:start=`=2*`ndt'+1' [vx] , ///
						[vx] loop=loop=15:size=1:start=`=`ndt'+1' [v5] , ///
						[v5] loop=loop=100:size=1:start=1 [v6] , ///
						[v6] loop=loop=100:size=1:start=0 [v7]' ///
					-map '[v7]' ./`export'/`export'.mpg -y
			}
			else {
				! /usr/local/bin/ffmpeg ///
					-framerate 24 ///
					-i ./`export'/PNG/`export'%03d.png ///
					-filter_complex ' ///
					   [0:0] loop=loop=96:size=1:start=`=3*`ndt'+4' [v0] , ///
						[v0] loop=loop=6:size=1:start=`=3*`ndt'+3' [v1] , ///
						[v1] loop=loop=6:size=1:start=`=2*`ndt'+3' [v2] , ///
						[v2] loop=loop=6:size=1:start=`=2*`ndt'+2' [v3] , ///
						[v3] loop=loop=6:size=1:start=`=2*`ndt'+1' [vx] , ///
						[vx] loop=loop=6:size=1:start=`=`ndt'+1' [v5] , ///
						[v5] loop=loop=24:size=1:start=1 [v6] , ///
						[v6] loop=loop=48:size=1:start=0 [v7]' ///
					-map '[v7]' ./`export'/`export'.mpg -y		
				! /usr/local/bin/ffmpeg -i ./`export'/`export'.mpg -r 24 ./`export'/`export'.gif -y
			}
		}
		
	}

	*	Repeat KS tests for easy viewing
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
*	Produce videos
*
makeVideo, ov export(vid1) zero h(30) dh(10)
makeVideo, ov export(vid2) va(50) vb(50) h(50)
makeVideo, ov export(vid3) va(10) vb(10)
makeVideo, left cut(0) ov export(vid4) va(10) vb(10)
makeVideo, left cut(0) new ov export(vid4b) va(10) vb(10)
makeVideo, actual ov export(vid5)
makeVideo, actual left cut(0) ov export(vid6)
makeVideo, actual left new cut(0) ov export(vid6b)
makeVideo, right cut(0) ov export(vid7) va(10) vb(10)
makeVideo, right new cut(0) ov export(vid7b) va(10) vb(10)
