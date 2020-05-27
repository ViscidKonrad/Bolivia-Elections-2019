set more off
pause off
clear all
set type double
*	This program will seek and create files in the current working directory
*	Please adjust as desired
cd ~/Escobari

capture: program drop getTiming
program define getTiming

	syntax anything(name=type) [, REPLACE]
	
	*
	*	This program loops through all reports, dropping duplicates
	*		(where nothing changed since prioor report) from the list
	*
	
	capture: use timing_`type', clear
	if (_rc~=0 | "`replace'"=="replace") {
		clear
		local alist: dir "Summaries/`type'" files "acta.*"
		local flist: dir "Summaries/`type'" files "acta.*.xlsx"
		*	all reports (Stata format)
		local flist: list alist - flist
		local flist: list sort flist
		macro li _flist
		di `:list sizeof flist'
		local duplist
		local lN 0
		qui foreach f of local flist {
			if (`lN'>0) {
				capture: cf _all using Summaries/`type'/`f'
				if (_rc==0) {
					if (`r(Nsum)'==0) {
						local duplist: list duplist | f
					}
				}
			}
			else {
				local duplist: list duplist | f
			}
			use Summaries/`type'/`f', clear
			noi count
			local lN `r(N)'
		}
		local flist: list flist - duplist
		local flist: list sort flist
		clear
		set obs `:list sizeof flist'
		gen ts = ""
		local nc 1
		foreach a of local flist {
			replace ts = "`a'" in `nc'
			local ++nc
		}
		save timing_`type', replace
	}
	
end

*	Timestamp each tally sheet with the first time it is 
*		REPORTED in the 180-second reports for each count	
capture: use timing, clear
if (_rc~=0)  {
	clear
	tempfile trep computo
	gen NúmeroMesa = .
	save `trep', emptyok
	save `computo', emptyok
	save timing, replace
	foreach type in trep computo {
		getTiming `type' // , replace
		levelsof ts, local(flist) clean
		noi foreach f of local flist {
			use Summaries/`type'/`f', clear
			keep if regexm(Ele,"Pres")
			keep NúmeroMesa
			gen `type' = clock(substr("`f'",6,.),"YMDhms")
			merge 1:1 NúmeroMesa using ``type'', nogen update replace
			noi count
			save ``type'', replace
		}
		merge 1:1 NúmeroMesa using timing, nogen
		save timing, replace
	}
}

*
*	Election data
*
use Summaries/computo/acta.2019.10.25.21.09.30, clear
keep if regexm(Ele,"Pres")
compress
merge n:1 NúmeroMesa using timing, nogen

*	Randomize order to break ties
gen tr = runiform()
gen cr = runiform()
sort trep tr
gen tnum = _n
sort comp cr
gen cnum = _n
format %7.0fc tnum cnum

*	Capital cities
local dlist Beni Chuquisaca Cochabamba Paz Oruro Pando Potosí Cruz Tarija
local mlist Trinidad Sucre Cochabamba Señora Oruro Cobija Potosí Sierra Tarija
gen isCap = 0
forvalues ii=1/`:list sizeof dlist' {
	di `"replace isCap = regexm(Dep,"`:word `ii' of `dlist''") & regexm(Muni,"`:word `ii' of `mlist''") if isCap==0"'
	replace isCap = regexm(Dep,"`:word `ii' of `dlist''") & regexm(Muni,"`:word `ii' of `mlist''") if isCap==0
}
* ... or El Alto
gen isbig = isCap | (regexm(Dep,"Paz") & regexm(Muni,"El Alto") )

*	Make six time slices of the TREP data
egen slice = cut(tnum), at(`=34556-6*5580'(5580)34556) icodes
replace slice = slice+1


*	Make histograms of computo timimg for each
sum sumV
local tsumV = `r(sum)'
forvalues ii=1/5 {
	hist cnum if slice==`ii' [w=1], pstyle(p`ii') freq yti("") ylab(0) xlab(0(10000)30000) xti("Slice `ii'") name(slice`ii')
}
hist cnum if slice==6 [w=1], color(red) freq yti("") ylab(0) xlab(0(10000)30000) xti("Slice 6") name(slice6, replace)
graph combine slice1 slice2 slice3 slice4 slice5 slice6, ycommon
graph export allslices.png, replace

