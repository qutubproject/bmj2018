* Figures for Effective UHC

	global directory "/Users/bbdaniels/GitHub/bmj2018/"

	global graph_opts bgcolor(white) title("") note(, justification(left) color(black) ///
		span pos(7)) title(, justification(left) color(black) span pos(11)) ///
		subtitle(, justification(left) color(black) span pos(11)) ///
		graphregion(color(white)) ylab(,angle(0) nogrid) ytit("") ///
		xtit(,placement(left) justification(left)) yscale(noline) xscale(noline) legend(region(lc(none) fc(none)))

	global graph_opts1 bgcolor(white) graphregion(color(white)) legend(region(lc(none) fc(none))) ///
		ylab(,angle(0) nogrid) title(, justification(left) color(black) span pos(11)) subtitle(, justification(left) color(black))

* Figure 1: Combined time use

	* Vietnam

		use "${directory}/vietnam_po.dta" , clear

		egen id = group(FACILITY_ID DOCTOR_ID location_type)

		collapse (sum) po_time (firstnm) location_type , by(id)

		gen study = "Vietnam Commune"
		replace study = "Vietnam District" if location_type == 2

		gen facilitycode = "V" + string(id)
		rename po_time tottime

		keep facilitycode study tottime

		tempfile vietnam
			save `vietnam' , replace

	* India

		use "${directory}/knowdo_data.dta" , clear

		gen tottime = .

		replace tottime = po_time * po_patients
		replace tottime = prov_time * prov_caseload if tottime == .

		table study , c(mean tottime count tottime)

		keep if tottime != .

		replace study = "Birbhum, India" if regexm(study,"Birbhum")
		replace study = `""Madhya Pradesh, India" "(Public Clinics)""' if regexm(study,"Mad") & substudy == 2
		replace study = `""Madhya Pradesh, India" "(Private Clinics)""' if regexm(study,"Mad") & substudy == 1

		collapse (mean) tottime (firstnm) study , by(facilitycode)

	* Analysis

		append using `vietnam'

		gen hours = tottime/60

		graph bar hours , $graph_opts1 over(study) ///
			blabel(bar, format(%9.1f)) ///
			hor ylab(1 "1 Hour" 2 "2 Hours" 3 "3 Hours") bar(1,fi(100) lc(black) lw(thin)) ///
			ytit("Average Daily Time Seeing Patients {&rarr}",placement(left) justification(left))

			graph export "${directory}/figure_1.png" , replace width(1000)

* Figure 2: SDI vignettes

		use "${directory}/SDI_analysis.dta" , clear

		drop if provider_cadre == 2 | provider_cadre == 4 | provider_cadre == .
		drop if country == "Tanzania-2014"
		replace country = "Tanzania" if country == "Tanzania-2016"

	* Proportions of medical officers < kenyan nurse mean

		qui su competence_mle if country == "Kenya" & provider_cadre == 3 , d
			local theNurses = `r(mean)'	// .553483
			gen check = competence_mle < 0.553483

	* Graph (10-25-50-75-90 pctiles)

		collapse (p10) p10=competence_mle (p25) p25=competence_mle (p50) p50=competence_mle ///
			(p75) p75=competence_mle (p90) p90=competence_mle ///
			, by(provider_cadre country)

		reshape long p, i(provider_cadre country)
			replace country = "Kenya (N = 372)" if regexm(country,"Kenya")
			replace country = "Madagascar (N = 588)" if regexm(country,"Madagascar")
			replace country = "Nigeria (N = 1,579)" if regexm(country,"Nigeria")
			replace country = "Tanzania (N = 224)" if regexm(country,"Tanzania")
			replace country = "Uganda (N = 432)" if regexm(country,"Uganda")

		graph box p ///
			,  hor over(provider_cadre) over(country ) ///
			legend(order(0 "Professional Cadre:" 1 "Medical Officer" 2 "Nurse") r(1) symxsize(small) symysize(small)  pos(6) ring(1)) ///
			asy $graph_opts1 ylab(-1 "-1 SD" 0 "SDI Mean" .553483 "Median" 1 "+1 SD" 2 "+2 SD" 3 "+3 SD", labsize(vsmall)) ytit("") note("") ///
			lintensity(.5) yline(.553483 , lc(black) lp(dash)) ///
			box(1 , fi(0) lc(maroon) lw(medthick)) box(2, fc(white) lc(navy) lw(medthick))

			graph export "${directory}/figure_2.png" , replace width(1000)

* Figure 3: Vietnam Vignettes

	use "${directory}/vignettes_analysis.dta"  , clear

	global graph_opts title(, justification(left) color(black) span pos(11)) ///
	  	graphregion(color(white)) ylab(,angle(0) nogrid notick) xscale(noline) yscale(noline) yline(0 , lc(black)) ///
	  	xtit(,placement(left) justification(left)) legend(region(lc(none) fc(none)))

	global hist_opts ylab(, angle(0) axis(2)) yscale(noline alt axis(2)) ///
	  	ytit(, axis(2)) ytit(, axis(1)) yscale(off axis(2)) yscale(alt)

	qui su theta_mle

	gen score = theta_mle - `r(min)'

	gen bach = roster_6a8 > 4

	tw ///
	 	(kdensity score if bach == 0 , lp(dash) lc(maroon) yaxis(2)) ///
	  	(kdensity score if bach == 1 , lp(dash) lc(navy) yaxis(2)) ///
	  	(histogram score if bach == 0 , freq w(.1) recast(scatter) msize(small) mc(maroon)) ///
	  	(histogram score if bach == 1 , freq w(.1) recast(scatter) msize(small) mc(navy)) ///
	  	, legend(symxsize(small) order(0 "" 0 "" 0 "{bf: Degree:}" 3 "Intermediate, Undergrad, or Bachelors (N=575)" 4 "Specialist or Masters (N=431)") c(1) pos(11) ring(0)) $graph_opts $hist_opts ///
	  	xtit("Knowledge Score {&rarr}") xlab(0(1)7) yline(10 20 30 , lc(gs12) lp(dot)) xsize(7)

	  	graph export "${directory}/figure_3.png" , replace width(1000)

* Figure 4: Combined know-do

		use "$directory/knowdo_data.dta" , clear

		drop if type == "Baseline Vignette"
		keep if ///
			(	  regexm(study,"China") ///
				| regexm(study,"Delhi") ///
			) ///
			& ///
			(	  regexm(case,"Diarrhea") | regexm(case,"TB1") )

		egen id = group(facilitycode case)
		xtset id type_code
			gen check = l.treat_correct
			gen check2 = f.treat_correct
			drop if check == . & check2 == .

		replace type = "Vignette" if regexm(type,"Vignette")
		replace type = "Standardized Patient" if !regexm(type,"Vignette")
		replace study = "Birbhum" if  regexm(study,"Birbhum")
		replace case = "Diarrhea (ORS)" if regexm(case,"Diarrhea")
		replace case = "Tuberculosis (AFB or CXR)" if regexm(case,"TB1")
		replace study = `""Madhya" "Pradesh""' if regexm(study,"Madhya Pradesh")

		drop if child == 1

		keep study case type treat_correct

		collapse (mean) treat_correct , by(study case type)

		set obs 10 // Bihar paper: https://jamanetwork.com/journals/jamapediatrics/fullarticle/2118580
			replace study = "Bihar" in 9
			replace study = "Bihar" in 10
			replace case = "Diarrhea (ORS)" in 9
			replace case = "Diarrhea (ORS)" in 10
			replace type = "Vignette" in 9
			replace type = "Standardized Patient" in 10
			replace treat_correct = 1 - 0.209 - 0.006 in 9
			replace treat_correct = 1 - 0.719 - 0.073 in 10

		graph bar treat_correct ///
			, over(type) asy bargap(20) over(study) over(case) nofill ///
			blabel(bar, format(%9.2f)) ///
			$graph_opts1 bar(1 , lc(black) lw(thin) fi(100)) bar(2 , lc(black) lw(thin) fi(100)) ///
			legend(r(1) order(0 "Measurement:" 1 "Standardized Patient" 2 "Clinical Vignette")) ///
			ytit("Providers ordering correct treatment {&rarr}", placement(bottom) justification(left)) ylab($pct)

			graph export "${directory}/figure_4.png" , replace width(1000)

* Have a lovely day!
