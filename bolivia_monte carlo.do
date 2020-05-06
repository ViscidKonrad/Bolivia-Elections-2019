set more off
pause off
clear all
set type double
*	This program will create files in the current working directory
*	Please adjust as desired
*cd ~/Bolivia

*	Apologies, orginal RNG seed has been lost, but makes very little difference
*set seed <seed>

capture: program drop getLatestTimestamp
program define getLatestTimestamp, rclass

	*	This program pulls the latest timestamp from the website
	*		Either from trep (early count) or computo (final)
	*	NOTE: WEBSITE HAS BEEN ERASED. THIS PROGRAM IS NO LONGER USEFUL

	args source

	*	Pull a response from the server
	local PURL https://`source'.oep.org.bo/resul/resulPais/1/1
	gen str240 d=""
	copy `"`PURL'"' p1
	insheetjson using p1, showresponse flatten
	insheetjson d using p1, tableselector("resulLugar") col("pubResul")
	
	*	Parse the timestamp elements and place in filename order
	split d
	split d1, parse(/)
	split d2, parse(:)
	local t = d13[1]
	foreach v in 12 11 21 22 23 {
		local t = "`t'"+"."+d`v'[1]
	}
	macro li _t
	
	return local timestamp = "`t'"
	
end

capture: program drop getSummary
program define getSummary

	*	This program simply gets the Excel results by acta based on the source
	*		and timestamp provided

	args source timestamp

	local filename acta.`timestamp'
	capture: use Summaries/`source'/`filename', clear
	if (_rc~=0) {
		capture: confirm file Summaries/`source'/`filename'.xlsx
		if (_rc~=0) {
			di as error "NOTE: WEBSITE HAS BEEN ERASED. PLEASE MANUALLY SUPPLY DATA FILE"
			/*
			local URL https://`source'.oep.org.bo/PubResul/`filename'.xlsx
			capture: mkdir Summaries
			capture: mkdir Summaries/`source'
			copy `"`URL'"' Summaries/`source'/`filename'.xlsx, replace
			*/
		}
		import excel using Summaries/`source'/`filename'.xlsx, clear firstrow case(preserve)
		compress
		save Summaries/`source'/`filename', replace
	}

end

capture: program drop mergeSummaries
program define mergeSummaries

	*	This program simply appends the presidential votes for two counts
	*		from quick-count timestamp and a final timestamp

	args timestamp1 timestamp2
	
	capture: use Summaries/merged, clear
	if (_rc~=0) {
		tempfile tf
		getSummary trep `timestamp1'
		keep if Elección=="Presidente y Vicepresidente"
		gen time = 0
		save `tf'
		getSummary computo `timestamp2'
		keep if Elección=="Presidente y Vicepresidente"
		gen time = 1
		append using `tf'
		save Summaries/merged, replace
	}

end

capture: program drop doDataPrep
program define doDataPrep

	syntax [, REPLACE]
	
	capture: use Summaries/MCdata, clear
	if (_rc~=0 | "`replace'"=="replace") {

		*
		*	Fetch the data
		*
		/*
		*	Get the latest timestamp
		*	NOTE: WEBSITE HAS BEEN ERASED. THIS CODE IS NO LONGER USEFUL
		getLatestTimestamp computo
		local timestamp2 `r(timestamp)'
		*/
		*	Official timestamp with 100% counted
		local timestamp2 2019.10.25.21.09.30
		*	Quick-count during the break
		local timestamp1 2019.10.20.19.40.57
		mergeSummaries `timestamp1' `timestamp2'
		drop Cir
		sum

		*
		*	Basic data maintenance
		*

		*	Aggregate the minor-party votes
		egen sumVálidos = rowtotal(CC-PANBOL)
		gen others = sumV-(CC+MAS)
		drop FPV-UCS F-Nulos Esta Elec
		
		*	Fix naming discrepancies in favor of the longer name and reshape
		gen ml = -length(Mun)
		gen ll = -length(Loc)
		gen rl = -length(Rec)
		bys NúmeroMesa CódigoMesa (ml): replace Mun = Mun[1]
		bys NúmeroMesa CódigoMesa (ll): replace Loc = Loc[1]
		bys NúmeroMesa CódigoMesa (rl): replace Rec = Rec[1]
		drop ml ll rl
		reshape wide CC MAS others sumV, i(NúmeroMesa CódigoMesa) j(time)
		
		*	Encode the geographies
		gen foreign = País~="Bolivia"
		egen fcode = group(foreign)
		egen pcode = group(fcode País), missing
		egen dcode = group(pcode Númerodepartamento), missing
		egen pdcode = group(dcode Prov), missing
		egen mcode = group(pdcode Númeromunicipio), missing
		egen lcode = group(mcode Loc), missing
		egen rcode = group(lcode R), missing
		egen scode = group(rcode NúmeroMesa CódigoMesa), missing
				
		save Summaries/MCdata, replace
		
	}
	
end

capture: program drop doBoliviaMCprep
program define doBoliviaMCprep

	capture: use Summaries/PrepData, clear
	if (_rc~=0) {

		doDataPrep

		*	sample tags one counted acta for each geography
		gen ssample = ~mi(CC0)
		gen scodex = scode if ssample
		local lg s
		foreach g in r l m pd d p f {
			*	Tag representative acta for each geography
			egen `g'sample = tag(`g'code) if `lg'sample
			*
			*	Key MC variables here:
			*		codex holds the replacement geography at each level
			*		rcodex will be the code for the replacement precinct
			*		scodex will be the code for the replacement acta if necessary
			*
			bys `g'code: egen `g'codex = total(`g'sample), missing
			replace `g'codex = cond(`g'codex,`g'code,.,.)	
			local lg `g'
		}
		
		*	generate lists of possible replacement geographies for MC sampling
		*	 - we want to connect these to each geography, so every acta in each
		*		(country) has a list of possible (departments) from which to draw
		*	 - when we set a replacement (country) for an acta, we will update the
		*		replacement (department) list as well
		local lg f
		qui foreach gc in p d pd m l r s {
			gen `gc'list = ""
			levelsof `lg'code, local(`lg'l) clean
			foreach g of local `lg'l {
				noi di "`lg'=`g' `gc''s"
				levelsof `gc'code if `lg'code==`g' & `gc'sample, local(gl) clean
				replace `gc'list = "`gl'" if `lg'code==`g'
			}
			gen `gc'len = wordcount(`gc'list)
			local lg `gc'
		}
		
		save Summaries/PrepData, replace
	}


end


capture: program drop doMCiter
program define doMCiter

	syntax [, Beta Gamma Sheet]

	*
	*	Single Iteration
	*
	*	Assign replacement geographies
	*		If method is sheet, need to assign all the way down to acta
	local gprefs p d pd m l r
	if ("`sheet'"=="sheet") {
		local gprefs `gprefs' s
	}
	local lg f
	foreach g in `gprefs' {
		*	Update to available replacement geographies
		bys `lg'codex (`lg'sample): replace `g'list = `g'list[_N]
		by `lg'codex (`lg'sample): replace `g'len = `g'len[_N]
		*	Generic geographic indicator for replacement
		tostring `g'codex, generate(g_i)
		*
		*	Key MC operation right here:
		*		randomly pick a replacement geography if codex is missing
		*
		replace g_i = word(`g'list,runiformint(1,`g'len)) if mi(`g'codex)
		destring g_i, replace
		bys `g'code (`g'sample): replace `g'codex = g_i[_N]
		capture: drop g_i
		local lg `g'
	}
	*	Assign replacement parameters and simulate votes
	bys `lg'codex (`lg'sample): egen nx = total(cond(~mi(CC0),1,.))
	bys `lg'codex (`lg'sample): egen Ix = total(cond(~mi(CC0),Ins,.))
	foreach p in CC MASIPSP others {
		bys `lg'codex (`lg'sample): egen V`p'x = total(cond(~mi(CC0),`p'0,.))
	}
	*	Simulate votes
	if ("`sheet'"=="sheet") {
		foreach p in CC MASIPSP others {
			gen `p'sheet = V`p'x*Ins/Ix
		}
		order CCs MASIPSPs otherss, last
	}
	if ("`gamma'"=="gamma") {
		*
		*	Note that Gamma-Poisson is not quite right in that
		*		there is no guarantee we wont have more votes than eligible voters
		*	 - Additional robustness test
		*
		foreach p in CC MASIPSP others {
			if (1) {
				*	Poisson rate per eligible voter is based on an improper
				*		Bayesian prior
				gen par_`p'gamma = rgamma(V`p'x,1/(0+Ix))
				replace par_`p'gamma = 0 if mi(par_`p'gamma)
				bys rcode: gen `p'gamma = rpoisson(Ins*par_`p'gamma[1])
				replace `p'gamma = 0 if mi(`p'gamma)
			}
			else {
				*	Poisson rate per acta in te precinct
				gen par_`p'gamma = rgamma(0.001+V`p'x,1/(1/1000+nx))
				bys rcode: gen `p'gamma = rpoisson(par_`p'gamma[1])
			}
		}
		order CCg MASIPSPg othersg, last
	}
	if ("`beta'"=="beta") {
		*
		*	For Beta-Binomial, we are going to make sure that ordering of the parties
		*	does not matter by randomly ordering with every iteration
		*
		*	 - Primary robustness test
		*
		local betalast CC MASIPSP others
		local betafirst = word(`"`betalast'"',runiformint(1,3))
		local betalast: list betalast - betafirst
		local betanext = word(`"`betalast'"',runiformint(1,2))
		local betalast: list betalast - betanext
		macro li _betafirst _betanext _betalast
		gen first = "`betafirst'"
		gen next = "`betanext'"
		gen last = "`betalast'"

		*	For the first party, the binomial probability of an eligible voter
		*		is beta(V,I-V) where V is the total votes for the party in the
		*		representative precinct and I is the eligible voters in the precinct
		*		 - effectively Bayesian with an improper Haldane prior
		gen par_`betafirst'beta = rbeta(V`betafirst'x, Ix                              -V`betafirst'x)
		*	For the second party, the binomial probability is computed similarly,
		*		but based on the eligible voters who did NOT vote for the first party
		gen par_`betanext'beta  = rbeta(V`betanext'x ,(Ix- V`betafirst'x)              -V`betanext'x)
		*	And likewise the third, based on eligible voters who did not vote for the first two
		gen par_`betalast'beta  = rbeta(V`betalast'x ,(Ix-(V`betafirst'x+V`betanext'x))-V`betalast'x)
		
		*	We want the same draw binomial probability draw across all actas
		foreach p in CC MASIPSP others {
			*	clean up where the draw is missing. Should be zero in all cases
			replace par_`p'beta = 0 if mi(par_`p'beta)
			bys rcode: replace par_`p'beta = par_`p'beta[1]
		}
		
		*	Now assign the actual votes based on the actual base of eligible voters
		*		remembering to reduce the base with each party according to the votes
		*		already assigned
		*	Also, clean up any missings as we proceed
		gen `betafirst'beta = rbinomial(Ins                                 ,par_`betafirst'beta)
		replace `betafirst'beta = 0 if mi(`betafirst'beta)
		gen `betanext'beta =  rbinomial(Ins- `betafirst'beta                ,par_`betanext'beta)
		replace `betanext'beta = 0 if mi(`betanext'beta)
		gen `betalast'beta =  rbinomial(Ins-(`betafirst'beta+`betanext'beta),par_`betalast'beta)
		replace `betalast'beta = 0 if mi(`betalast'beta)

	}


end

capture: program drop doBoliviaMC
program define doBoliviaMC

	syntax [anything(name=run)] [, Beta Gamma Sheet REPLACE TEST Inner(integer 50) Outer(integer 10)]

	*
	*	Monte Carlo projections based on TREP
	*
	*	Options:
	*		run is an identifier; results will be saved in Summaries`run'
	*
	*		beta (Beta-Binomial based on representative precinct)
	*		gamma (Gamma-Poisson based on representative precinct)
	*		sheet (MC sampling of available actas in representative precinct)
	*		<none of above> (NOT MC) Impute votes per eligible voter based on
	*			average across smallest geography with counted actas
	*
	*		replace overwrites the run with new simulation;
	*			otherwise if run has been performed, the program simply loads 
	*			the previous results of the run
	*
	*		test simply performs one iteration (if beta gamma or sheet)
	*			but does not aggregate-- for diagnostic purposes
	*
	*		inner is the number of MC iterations in each batch to be saved
	*		outer is the number of MC batches to be saved
	*			(inner x outer = total number of iterations)
	*
	
	*	test must imply replace so that the iteration will be performed
	*		it will break before replacing any saved simulations
	if ("`test'"=="test") {
		local replace replace
	}
	
	if ("`beta'`gamma'`sheet'"=="") {
		doDataPrep, replace
		*	Do a simple point estimate
		foreach p in CC MASIPSP others {
			*	Simulated values
			gen `p'x = `p'0
		}
		foreach g in r l m pd d p f {
			*	In each geography, total eligible votes on quick-counted actas
			bys `g'code: egen `g'I = total(cond(~mi(CC0),Inscritos,.)), missing
			foreach p in CC MASIPSP others {
				*	In each geography, compute party vote rate on quick-counted actas
				bys `g'code: egen `g'`p'r = total(`p'0/`g'I), missing
				by `g'code: replace `p'x = Ins*`g'`p'r if mi(`p'x)
			}
		}

		gen counted = ~mi(CC0)
		collapse (sum) *x *0, by(counted)
		gen margin0 = 100*(MASIPSP0-CC0)/(MASIPSP0+CC0+others0)
		gen marginx = 100*(MASIPSPx-CCx)/(MASIPSPx+CCx+othersx)
		li
		
	}
	else {
		capture: use Simulations`run'/FULL, clear
		if (_rc~=0 | "`replace'"=="replace") {
			*
			*	Monte Carlo
			*
			tempfile ti
			qui forvalues ii=1/`outer' {
				*	We might have already done this batch...
				capture: use Simulations`run'/`ii', clear
				if (_rc~=0 | "`replace'"=="replace") {
					tempfile tj
					forvalues jj=1/`inner' {
					
						noi di as text "Imputation number: " as result (`ii'-1)*`inner'+`jj'
						
						doBoliviaMCprep
						doMCiter, `beta' `gamma' `sheet'
						
						if ("`test'"=="test") {
							local s `beta'`gamma'`sheet'
							gen masshare0 = 100*MASIPSP0/(MASIPSP0+CC0+others0)
							gen masshare1 = 100*MASIPSP1/(MASIPSP1+CC1+others1)
							gen masshare`s' = 100*MASIPSP`s'/(MASIPSP`s'+CC`s'+others`s')
							scatter masshare`s' masshare1
							noi sum
							mata: XXX
						}
						
						local byvars
						if ("`beta'"=="beta") {
							local byvars by(first next last)
						}
						collapse (sum) CC* MASIPSP* others*, `byvars'
						noi li
						if (`jj'>1) {
							append using `tj'
						}
						save `tj', replace
					}
					*	save this batch
					capture: mkdir Simulations`run'
					save Simulations`run'/`ii', replace
				}
				*	Add this batch of to the full batch
				if (`ii'>1) {
					append using `ti'
				}
				save `ti', replace
				*	Calculate the margins and report on the distribution
				foreach s in 0 1 `beta' `gamma' `sheet' {
					gen margin`s' = 100*(MASIPSP`s'-CC`s')/(MASIPSP`s'+CC`s'+others`s')
				}
				*	Histogram
				discard
				local otypes CC0 CC1
				unab type: CC*
				local type: list type-otypes
				local type = substr("`type'",3,.)
				sum margin`type', meanonly
				hist margin`type', caption(`"`=_N' iterations of imputation type `type'"') ///
					xti("MAS-IPSP - CC (% of valid votes)") yti("") ///
					freq xlab(`=ceil(10*`r(min)')/10'(0.1)`=floor(10*`r(max)')/10')
			}
			capture: drop margin*
			*	Save the full batch of simulations
			save Simulations`run'/FULL, replace
		}
		local otypes CC0 CC1
		unab type: CC*
		local type: list type-otypes
		local type = substr("`type'",3,.)
		*	Calculate MAS margins
		gen margin = 100*(MASIPSP`type'-CC`type')/(MASIPSP`type'+CC`type'+others`type')
		*	Histogram
		sum margin, meanonly
		hist margin, /*caption(`"`=_N' iterations of imputation type `type'"')*/ ///
			xti("MAS-IPSP - CC (% of valid votes)") yti("") ///
			freq xlab(`=ceil(10*`r(min)')/10'(0.1)`=floor(10*`r(max)')/10') ///
			ti("Hello there")
		graph export Simulations`run'/margins_`type'.pdf, replace
		*	Reporting
		if ("`type'"=="beta") {
			* Check that Beta-Binomial party ordering doesn't matter for beta by
			*	disaggregating results by ordering
			table first next, c(mean margin)
		}
			codebook margin
	}
	
end

*	Options are:
*		sheet (MC sampling of available actas in representative precinct)
*		beta (Beta-Binomial based on representative precinct)
*		gamma (Gamma-Poisson based on representative precinct)
*		<empty> (NOT MC) Impute votes per eligible voter based on average across
*				smallest geography with counted actas
local type sheet
*	May be just about anything here. Used to save simulations in case
*		you want to come back to them
local run 0

doBoliviaMC `run', `type' inner(10) outer(5)
