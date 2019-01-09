
(* :Title: PopulationGrowth                 *)
(* :Author: Arne Eide                       *)
(* :Summary:
    This package contains
    population growth functions
    based on von Bertalanffy's
    growth equation and Baranov's
    martality equation
    (Beverton and Holt growth)
    and Surplus production models.
*)
(* :Context: EconMult`PopulationGrowth`     *)
(* :Package Version: 2.0                    *)
(* :Copyright: Arne Eide                    *)
(* :History:
	v1.0 (May 27 2004) Written by Arne Eide
	v1.1 (Jun 03 2004) Corrected error in AgeOfMaxGrowth
	                   Included discrete time options
	                   (DiscreteTime, TimeStep and BiomassSum)
	v1.2 (Jun 04 2004) Corrected minor error in PopulationBiomass
	                   Included BevertonHoltRecruitment and RickerRecruitment
	v1.3 (Jun 07 2004) Included Recruitment as a new function
	                   and RecruitmentFunction as an option in PopulationGrowth
	v1.4 (Jun 08 2004) Included ConstantRecruitment and new options,
	                   CatchAge and OldestAge
	v1.5 (Jun 15 2004) Included Maturation and CatchSelection

	v2.0 (Jun 26 2004) Merged with SurplusProduction
	
	v3.0 (Jun 03 2010) Minor changes in codes, major additions in documentation. PGk has been renamned PGkk.
	
	v3.1 (Apr 04 2010) PopulationBiomassF is renamed to BevertonHoltModel.

	(c) 2004 Arne Eide
 *)
(* :Keywords:
	Population Growth,
	Bioeconomics,
	Fisheries Management                    *)
(* :Source:                                 *)
(* :Warning: None.                          *)
(* :Mathematica Version: 7.0                *)
(* :Limitation: None.                       *)
(* :Discussion:                             *)


BeginPackage["EconMult`PopulationGrowth`"]


(*========================================*)
(*            Population Growth           *)
(*========================================*)

PopulationGrowth::usage                = "PopulationGrowth - the name of this package."


(*----------------------------------------*)
(*             Option values              *)
(*----------------------------------------*)

(*    Default values: Dummy variables     *)
Recruits::usage                        = "Recruits is an option for PopulationGrowth. Number of recruits, often referred to as R. Default value PGR"
SpawningBiomass::usage                 = "SpawningBiomass is an option for PopulationGrowth. Spawning biomass, often referred to as SB. Default value PGSB"
GrowthRate::usage                      = "GrowthRate is an option for PopulationGrowth. Individual growth rate, often referred to as k. Default value PGkk."
MortalityRate::usage                   = "MortalityRate is an option for PopulationGrowth. Natural mortality rate, often referred to as M. Default value PGM."
FishingMortalityRate::usage            = "FishingMortalityRate is an option for PopulationGrowth. Fishing mortality rate, often referred to as F or f. Default value PGF."
MaxWeight::usage                       = "MaxWeight is an option for PopulationGrowth. Theoretical maximal individual weight, often referred to as w8 or W8. Default value PGW8."
MaxLength::usage                       = "MaxLength is an option for PopulationGrowth. Theoretical maximal individual length, often referred to as l8 or L8. Default value PGL8."
InitialAge::usage                      = "InitialAge is an option for PopulationGrowth. Theoretical age of individual weight equal zero, often referred to as t0. Default value PGt0."
RecruitmentAge::usage                  = "RecruitmentAge is an option for PopulationGrowth. Age of recruitment (see SpawningBiomass), often referred to as tr or tR. Default value PGtR."
OldestAge::usage                       = "OldestAge is an option for PopulationGrowth, giving the oldest age represented in the population. Default value of Oldest Age is PGt8."
MaturationAge::usage                   = "MaturationAge is an option for PopulationGrowth, giving the age of 50% mature biomass. Default value of MaturationAge is PGtm."
MaturationStartAge::usage              = "MaturationStartAge is an option for PopulationGrowth, giving the age of first maturation. Default value of MaturationAge is PGtms."
WeightLengthRelation::usage            = "WeightLengthRelation is an option for PopulationGrowth, normaly equal 3 (cubic relation), gives the dimension relation between length and weight. Default value PGb."
WeightLengthParameter::usage           = "WeightLengthParameter is an option for PopulationGrowth, parameter of the LengthGrowth equation, often referred to as d. Default value PGd"
FirstCatchAge::usage                   = "FirstCatchAge is an option for PopulationGrowth, giving the age of first catch in stock. Default value PGtcs."
CatchAge::usage                        = "CatchAge is an option for PopulationGrowth, giving the age of first catch in stock or 50% recruitment to catchable stock. Default value PGtc."
CatchSelectionFunctionBend::usage      = "CatchSelectionFunctionBend is an option of PopulationGrowth with default value 2"

(*    Default values: True/False         *)
UseWeight::usage                       = "UseWeight is an option for PopulationGrowth with value True or False. UseWeight=True (default) forces the use of MaxWeight."
Fishing::usage                         = "Fishing is an option for PopulationGrowth with value True or False. Fishing=True (default) includes fishing mortality (FishingMortalityRate)."
DiscreteTime::usage                    = "DiscreteTime is an option for PopulationGrowth with value True or False. DiscreteTime=True forces discrete time calculations in PopulationBiomass. Default value of DiscreteTime is False."
BiomassSum::usage                      = "BiomassSum is an option for PopulationGrowth in case of discrete time calculations (see DiscreteTime). Default value of BiomassSum is True."

(*    Other Default values               *)
PopulationModel::usage                 = "PopulationModel is an option for PopulationGrowth giving the model type. Default value is BevertonHoltModel."
CurrentBiomass::usage				   = "CurrentBiomass is an option in PopulationGrowth with default value PGX."
TimeStep::usage                        = "TimeStep is an option for PopulationGrowth giving the time step interval in case of discrete time calculations (see DiscreteTime). Default value of TimeStep is 1."
BiomassIncluded::usage                 = "BiomassIncluded is an option for PopulationGrowth, controlling the output of BevertonHoltModel. Default value of BiomassIncluded is 'Fishable', forcing BevertonHoltModel to show only fishable stock biomass. Other possible values of BiomassIncluded are 'All' (total biomass), 'NotFishable' and 'AllParts' (list of not fishable and fishable fractions)."
Maturation::usage                      = "Maturation is an option for PopulationGrowth,"
CatchSelection::usage                  = "CatchSelection is an option for PopulationGrowth,"
CatchStartPercentage::usage            = ""
MaturationStartPercentage::usage       = ""
RecruitmentFunction::usage             = "RecruitmentFunction  is an option in PopulationGrowth giving the recruitment function. Default value is ConstantRecruitment."
MaximumRecruitment::usage              = "MaximumRecruitment is an option of the recruitment functions in PopulationGrowth representing number of maximum recruits. Default value is PGmR."
RecruitmentExponent::usage             = "RecruitmentExponent is an option of the recruitment functions in PopulationGrowth representing an exponential term of all biomasses. Default value is 1."
MaximumRecruitmentBiomass::usage       = "MaximumRecruitmentBiomass is an option of the recruitment functions in PopulationGrowth representing the spawning biomass of maximum recruitment in RickerRecruitment, and two times the half value in BevertonHoltRecruitment. Default value is PGmRb"
(*----------------------------------------*)

(*   Possible option values of Maturation           *)
Sharp::usage                           = "Sharp is the default value of Maturation (Option in PopulationGrowth)."
Logistic::usage                        = "Logistic (or LogisticGrowth) is an option value of Maturation (Option in PopulationGrowth)."
LogisticGrowth::usage                  = "LogisticGrowth (or Logistic) is an option value of Maturation (Option in PopulationGrowth)."
SelectionFunction::usage               = "SelectionFunction[t,...]"
CatchSelectionFunction::usage          = "CatchSelectionFunction[t, opts]"
MaturationFunction::usage              = "MaturationFunction[t, opts]"

(*   Possible option values of BiomassIncluded      *)
Fishable::usage                        = "Fishable is the default value of BiomassIncluded (Option in PopulationGrowth)."
AllParts::usage                        = "AllParts is an option value of BiomassIncluded (Option in PopulationGrowth)."
NotFishable::usage                     = "NotFishable is an option value of BiomassIncluded (Option in PopulationGrowth)."

(*   Possible option values of RecruitmentFunction  *)
ConstantRecruitment::usage             = "ConstantRecruitment is the default value of RecruitmentFunction (Option in PopulationGrowth)."
BevertonHoltRecruitment::usage         = "BevertonHoltRecruitment is an option value of RecruitmentFunction (Option in PopulationGrowth)."
RickerRecruitment::usage               = "RickerRecruitment is an option value of RecruitmentFunction (Option in PopulationGrowth)."
(*--------------------------------------------------*)


(*----------------------------------------*)
(*               Functions                *)
(*----------------------------------------*)


vonBertalanffyLengthGrowth::usage      = "vonBertalanffyLengthGrowth[t, opts] gives the differential equation of length as a function of age (t)."
vonBertalanffyLength::usage            = "vonBertalanffyLength[t] is the solution of vonBertalanffyLengthGrowth[t, opts] and gives the individual length as a function of age (t). The solution is implemented in IndividualWeight."

BaranovMortality::usage                = "BaranovMortality[t, opts] gives the differential equation of number of individs as a function of age (t)."
BaranovNumbers::usage                  = "BaranovNumbers[t] is the solution of BaranovMortality[t, opts] and gives the individ numbers as a function of age (t). The solution is implemented in IndividNumbers."

LengthGrowth::usage                    = "LengthGrowth[t, opts] gives the individual length at age t."
IndividualWeight::usage                = "IndividualWeight[t, opts] gives the individual weight at age t."

IndividNumbers::usage                  = "IndividNumbers[t, opts] (often referred to as n) gives the number of individs in the cohort at age t."

CohortBiomass::usage                   = "CohortBiomass[t, opts] gives biomass of cohort at age t."
AgeOfMaxGrowth::usage                  = "AgeOfMaxGrowth[opts] gives the individual age of maximum biomass of the cohort."
MaximumBiomassGrowth::usage            = "MaximumBiomassGrowth[opts] gives the maximum biomass of a cohort (see also AgeOfMaxGrowth)."
EquilibriumBiomass::usage              = "EquilibriumBiomass[opts] is the natural population biomass equilibrium (without fishing), often referred to as the environmental carrying capacity K."

PopulationBiomass::usage               = "PopulationBiomass[t, opts] gives biomass sum of population between RecruitmentAge and age t."
PopulationBiomassOfMaxGrowth::usage    = "PopulationBiomassOfMaxGrowth[opts] gives the population biomass corresponding to the maximum biomass growth when no fishing."
PopulationFractionBiomass::usage       = "PopulationFractionBiomass[opts] is a function used by BevertonHoltModel, giving the biomass of the population fraction between the ages of tc and t8."
BevertonHoltModel::usage              = "BevertonHoltModel[opts] gives the population biomass when fishing at given tc (age of first catch), t8 (oldest year class) and fishing mortality (see FishingMortalityRate)."

FishingMortalityEquilibrium::usage     = "FishingMortalityEquilibrium[X, opts] gives the equilibrium fishing mortality rate for a given population biomass (X) and age of first catch (tc)."
YieldOfBiomass::usage                  = "YieldOfBiomass[X, opts] gives the total equilibrium catch as a function of population biomass (X) and age of first catch (tc)."
TotalCatch::usage                      = "TotalCatch[opts] gives the total catch."

Recruitment::usage                     = "Recruitment[spawning biomass, opts] is the recruitment function in PopulationGrowth."


(*----------------------------------------*)
(*               Notation                 *)
(*----------------------------------------*)

Notation::usage                        = "Notation[exp] converts exp to be displayed in TraditionalForm and the dummy variables to standard symbols."
SimplifyNotation::usage                = "SimplifyNotation[exp] converts exp to be displayed in TraditionalForm and the dummy variables to standard symbols (by using Notation), and simplify exp by using FullSimplify."



(*----------------------------------------*)
(*            Dummy variables             *)
(*----------------------------------------*)

PGX::usage                             = "PGX is the dummy variable of PopulationBiomass."
PGSB::usage                            = "PGSB is the dummy variable of SpawningBiomass."

PGkk::usage                             = "PGkk is the dummy variable of GrowthRate."
PGM::usage                             = "PGM is the dummy variable of MortalityRate."
PGF::usage                             = "PGF is the dummy variable of FishingMortalityRate."

PGd::usage                             = "PGd is the dummy variable of WeightLengthParameter"
PGb::usage                             = "PGb is the dummy variable of WeightLengthRelation."
PGW8::usage                            = "PGW8 is the dummy variable of MaxWeight."
PGL8::usage                            = "PGL8 is the dummy variable of MaxLength."

PGtR::usage                            = "PGtR is the dummy variable of RecruitmentAge."
PGt0::usage                            = "PGt0 is the dummy variable of InitialAge."
PGt8::usage                            = "PGt8 is the dummy variable of OldestAge."
PGtm::usage                            = "PGtm is the dummy variable of MaturationAge."
PGtms::usage                           = "PGtms is the dummy variable of MaturationStartAge."
PGtc::usage                            = "PGtc is the dummy variable of CatchAge."
PGtcs::usage                           = "PGtcs is the dummy variable of FirstCatchAge"
PGt8::usage                            = "PGt8 is the dummy variable of maximum age in population."

PGK::usage                             = "PGK is the dummy variable of BiomassMaximum."
PGMSY::usage                           = "PGMSY is the dummy variable of MaximumSustainableYield."
PGq::usage                             = "PGq is the dummy variable of CatchabilityCoefficient."
PGrr::usage                             = "PGrr is the dummy variable of IntrinsicGrowthRate."
PGmm::usage                             = "PGmm is the dummy variable of RichardsPellaTomlinsonParameter."

PGR::usage                             = "PGR is the dummy variable of Recruits."
PGmR::usage                            = "PGmR is the dummy variable of MaximumRecruitment."
PGmRb::usage                           = "PGmRb is the dummy variable of MaximumRecruitmentBiomass."
PGRe::usage                            = "PGRe is the dummy variable of RecruitmentExponent."

PGma::usage


(*========================================*)
(*            Surplus Production          *)
(*========================================*)


(*----------------------------------------*)
(*             Option values              *)
(*----------------------------------------*)

(*    Default values: Dummy variables     *)
BiomassMaximum::usage                  = "BiomassMaximum is an option of SurplusProduction with default value 'PGK'"
CatchabilityCoefficient::usage         = "CatchabilityCoefficient is an option of SurplusProduction with default value 'PGq'"
IntrinsicGrowthRate::usage             = "IntrinsicGrowthRate is an option of SurplusProduction with default value 'PGrr'"
RichardsPellaTomlinsonParameter::usage = "RichardsPellaTomlinsonParameter is an option of SurplusProduction with default value 'PGmm'"
MaximumSustainableYield::usage         = "MaximumSustainableYield is an option of SurplusProduction with default value 'PGMSY'"

(*    Other Default values               *)
UseMSY::usage                          = "UseMSY is an option of SurplusProduction with default value 'True'"
GrowthModel::usage                     = "GrowthModel is an option of SurplusProduction with default value 'VerhulstSchaefer'"

(*   Possible option values of GrowthModel          *)
$SurplusProductionModels::usage        = "$SurplusProductionModels is a package variable containing the currently implemented growth models in the SurplusProduction package"
VerhulstSchaefer::usage                = "VerhulstSchaefer is the default value of the SurplusProduction option GrowthModel."
QuasiBevertonHolt::usage               = "QuasiBevertonHolt is a valid value of the SurplusProduction option GrowthModel."
GompertzFox::usage                     = "GompertzFox is a valid value of the SurplusProduction option GrowthModel."
RichardsPellaTomlinson::usage          = "RichardsPellaTomlinson is a valid value of the SurplusProduction option GrowthModel."
(*--------------------------------------------------*)


(*----------------------------------------*)
(*               Functions                *)
(*----------------------------------------*)

SurplusProduction::usage               = "SurplusProduction[x, opts] gives the surplus growth related to stock biomass x."

QuasiBevertonHoltCatch::usage          = "QuasiBevertonHoltCatch[f, opts] gives the equilibrium QuasiBevertonHoltCatch of fishing effort 'f'. Default options is the SurplusProduction options."
QuasiBevertonHoltGrowth::usage         = "QuasiBevertonHoltGrowth[f, opts] gives the equilibrium QuasiBevertonHoltGrowth of fishing effort 'f'. Default options is the SurplusProduction options."
GompertzFoxCatch::usage                = "GompertzFoxCatch[f, opts] gives the equilibrium GompertzFoxCatch of fishing effort 'f'. Default options is the SurplusProduction options."
GompertzFoxGrowth::usage               = "GompertzFoxGrowth[f, opts] gives the equilibrium QuasiBevertonHoltCatch of fishing effort 'f'. Default options is the SurplusProduction options."
RichardsPellaTomlinsonCatch::usage     = "RichardsPellaTomlinsonCatch[f, opts] gives the equilibrium RichardsPellaTomlinsonCatch of fishing effort 'f'. Default options is the SurplusProduction options."
RichardsPellaTomlinsonGrowth::usage    = "RichardsPellaTomlinsonGrowth[f, opts] gives the equilibrium RichardsPellaTomlinsonGrowth of fishing effort 'f'. Default options is the SurplusProduction options."
VerhulstSchaeferCatch::usage           = "VerhulstSchaeferCatch[f, opts] gives the equilibrium VerhulstSchaeferCatch of fishing effort 'f'. Default options is the SurplusProduction options."
VerhulstSchaeferGrowth::usage          = "VerhulstSchaeferGrowth[f, opts] gives the equilibrium VerhulstSchaeferGrowth of fishing effort 'f'. Default options is the SurplusProduction options."


(*   To be removed??   *)

MaxBiomassGrowth::usage                = "MaxBiomassGrowth may be removed...."
PopulationBiomassGrowth::usage         = "PopulationBiomassGrowth may be removed..."
IndividAge::usage                      = "Individual age, often referred to as t. may be removed...."


Begin["`Private`"]




Options[PopulationGrowth] =

            {
			WeightLengthParameter           -> PGd,           (* PGW8*PGL8^(-PGb) *)
			WeightLengthRelation            -> PGb,
			RichardsPellaTomlinsonParameter -> PGmm,
			GrowthRate                      :> PGkk,
			SpawningBiomass                 :> PGSB,
			Recruits                        :> PGR,
			MortalityRate                   :> PGM,
			FishingMortalityRate            :> PGF,
			MaxWeight                       -> PGW8,
			MaxLength                       -> PGL8,          (*(PGW8/PGd)^(-PGb) *)
			RecruitmentAge                  -> PGtR,
			InitialAge                      -> PGt0,
			CatchAge                        -> PGtc,
			FirstCatchAge                   -> PGtcs,
			OldestAge                       -> PGt8,
			MaturationStartAge              -> PGtms,
			MaturationAge                   -> PGtm,
			MaximumRecruitment              -> PGmR,
			MaximumRecruitmentBiomass       -> PGmRb,
			UseWeight                       -> True,
			Fishing                         -> True,
			DiscreteTime                    -> False,
			BiomassSum                      -> True,
			TimeStep                        -> 1,
			RecruitmentExponent             -> 1,
			BiomassIncluded                 -> Fishable,
			RecruitmentFunction             -> ConstantRecruitment,
			Maturation                      -> Sharp,
			CatchSelection                  -> Sharp,
			CatchStartPercentage            -> 0.1,
			MaturationStartPercentage       -> 0.1,
			CatchSelectionFunctionBend      -> 2,
	
			PopulationModel                 -> BevertonHoltModel,
			CurrentBiomass                  -> PGX,
	        GrowthModel                     -> VerhulstSchaefer,
	        RichardsPellaTomlinsonParameter -> PGmm,
	        MaximumSustainableYield         -> PGMSY,
	        CatchabilityCoefficient         -> PGq,
	        BiomassMaximum                  -> PGK,
	        IntrinsicGrowthRate             -> PGrr,
	        UseMSY                          -> True

            };


PopulationGrowth[opts___] :=

           Module[
              {d,b,m,k,S,R,M,F,W8,L8,tR,t0,tc,t8,uw,f,dt,bs,ts,bi,mr,mrb,re,rf,ma,cs,tms,tm,sfb,tcs,csp,msp,pm,cb,gm,ptp,msy,q,bm,r,umsy},
	      {d,b,m,k,S,R,M,F,W8,L8,tR,t0,tc,t8,uw,f,dt,bs,ts,bi,mr,mrb,re,rf,ma,cs,tms,tm,sfb,tcs,csp,msp,pm,cb,gm,ptp,msy,q,bm,r,umsy} =
	      {
			WeightLengthParameter,
			WeightLengthRelation,
			RichardsPellaTomlinsonParameter,
			GrowthRate,
			SpawningBiomass,
			Recruits,
			MortalityRate,
			FishingMortalityRate,
			MaxWeight,
			MaxLength,
			RecruitmentAge,
			InitialAge,
			CatchAge,
			OldestAge,
			UseWeight,
			Fishing,
			DiscreteTime,
			BiomassSum,
			TimeStep,
			BiomassIncluded,
			MaximumRecruitment,
			MaximumRecruitmentBiomass,
			RecruitmentExponent,
			RecruitmentFunction,
			Maturation,
			CatchSelection,
			MaturationStartAge,
			MaturationAge,
			CatchSelectionFunctionBend,
			FirstCatchAge,
			CatchStartPercentage,
			MaturationStartPercentage,
	
			PopulationModel,
			CurrentBiomass,
	        GrowthModel,
	        RichardsPellaTomlinsonParameter,
	        MaximumSustainableYield,
	        CatchabilityCoefficient,
	        BiomassMaximum,
	        IntrinsicGrowthRate,
	        UseMSY
              } /. {opts} /. Options[PopulationGrowth];


               (* If[!f, pm = PopulationBiomass];*)

	        pm[opts]
	    ];


Options[Recruitment] =
            {
	        Recruits                    	-> PGR,
			MaximumRecruitment              -> PGmR,
			MaximumRecruitmentBiomass       -> PGmRb,
	        RecruitmentExponent         	-> 1
	    };


Options[SurplusProduction] =
	    {
	        GrowthModel                     -> VerhulstSchaefer,
			CurrentBiomass                  -> PGX,
	        RichardsPellaTomlinsonParameter -> PGmm,
	        MaximumSustainableYield         -> PGMSY,
	        CatchabilityCoefficient         -> PGq,
	        BiomassMaximum                  -> PGK,
	        IntrinsicGrowthRate             -> PGrr,
	        WeightLengthRelation            -> PGb,
	        UseMSY                          -> True

	    };


optionslist =
	      {

		WeightLengthParameter,
		WeightLengthRelation,
		RichardsPellaTomlinsonParameter,
		GrowthRate,
		SpawningBiomass,
		Recruits,
		MortalityRate,
		FishingMortalityRate,
		MaxWeight,
		MaxLength,
		RecruitmentAge,
		InitialAge,
		CatchAge,
		OldestAge,
		UseWeight,
		Fishing,
		DiscreteTime,
		BiomassSum,
		TimeStep,
		BiomassIncluded,
		MaximumRecruitment,
		MaximumRecruitmentBiomass,
		RecruitmentExponent,
		RecruitmentFunction,
		Maturation,
		CatchSelection,
		MaturationStartAge,
		MaturationAge,
		CatchSelectionFunctionBend,
		FirstCatchAge,
		CatchStartPercentage,
		MaturationStartPercentage
	      };




$SurplusProductionModels=
    {
      VerhulstSchaefer,
      GompertzFox,
      RichardsPellaTomlinson,
      QuasiBevertonHolt
      };

(*----------------------------------------*)
(*          Length and Weight Growth      *)
(*          (LengthGrowth)                *)
(*----------------------------------------*)


vonBertalanffyLengthGrowth[t_, opts___] :=
           Module[
              {d,b,m,k,S,R,M,F,W8,L8,tR,t0,tc,t8,uw,f,dt,bs,ts,bi,mr,mrb,re,rf,ma,cs,tms,tm,sfb,tcs,csp,msp},
	      {d,b,m,k,S,R,M,F,W8,L8,tR,t0,tc,t8,uw,f,dt,bs,ts,bi,mr,mrb,re,rf,ma,cs,tms,tm,sfb,tcs,csp,msp} =
	      optionslist /. {opts} /. Options[PopulationGrowth];

	      If[uw, L8=(W8/d)^(1/b)];

	      (vonBertalanffyLength'[t] == k(L8 - vonBertalanffyLength[t]))

	      ];


BaranovMortality[t_, opts___] :=
           Module[
              {d,b,m,k,S,R,M,F,W8,L8,tR,t0,tc,t8,uw,f,dt,bs,ts,bi,mr,mrb,re,rf,ma,cs,tms,tm,sfb,tcs,csp,msp},
	      {d,b,m,k,S,R,M,F,W8,L8,tR,t0,tc,t8,uw,f,dt,bs,ts,bi,mr,mrb,re,rf,ma,cs,tms,tm,sfb,tcs,csp,msp} =
	      optionslist /. {opts} /. Options[PopulationGrowth];

		      If[!f, F=0];

		      If[CatchSelection===Sharp,
			 (BaranovNumbers'[t] == -(F+M)*BaranovNumbers[t]),
			 (BaranovNumbers'[t] == -(F+M)*BaranovNumbers[t])
		      ]
	      ];




LengthGrowth[IndividAge_, opts___] :=
           Module[
              {d,b,m,k,S,R,M,F,W8,L8,tR,t0,tc,t8,uw,f,dt,bs,ts,bi,mr,mrb,re,rf,ma,cs,tms,tm,sfb,tcs,csp,msp},
	      {d,b,m,k,S,R,M,F,W8,L8,tR,t0,tc,t8,uw,f,dt,bs,ts,bi,mr,mrb,re,rf,ma,cs,tms,tm,sfb,tcs,csp,msp} =
	      optionslist /. {opts} /. Options[PopulationGrowth];

	      If[uw, L8=(W8/d)^(1/b)];

	      L8*(1 - E^(-k*(IndividAge - t0)))
	    ];


IndividualWeight[IndividAge_, opts___] :=
           Module[
              {d,b,m,k,S,R,M,F,W8,L8,tR,t0,tc,t8,uw,f,dt,bs,ts,bi,mr,mrb,re,rf,ma,cs,tms,tm,sfb,tcs,csp,msp},
	      {d,b,m,k,S,R,M,F,W8,L8,tR,t0,tc,t8,uw,f,dt,bs,ts,bi,mr,mrb,re,rf,ma,cs,tms,tm,sfb,tcs,csp,msp} =
	      optionslist /. {opts} /. Options[PopulationGrowth];
	      If[uw,
	        W8*(1 - E^(-k*(IndividAge - t0)))^b,
	        d*LengthGrowth[IndividAge, opts]^b
	      ]
	    ];



(*----------------------------------------*)
(*          Mortality                     *)
(*          (due to Baranov)              *)
(*----------------------------------------*)


IndividNumbers[IndividAge_, opts___] :=
           Module[
              {d,b,m,k,S,R,M,F,W8,L8,tR,t0,tc,t8,uw,f,dt,bs,ts,bi,mr,mrb,re,rf,ma,cs,tms,tm,sfb,tcs,csp,msp},
	      {d,b,m,k,S,R,M,F,W8,L8,tR,t0,tc,t8,uw,f,dt,bs,ts,bi,mr,mrb,re,rf,ma,cs,tms,tm,sfb,tcs,csp,msp} =
	      optionslist /. {opts} /. Options[PopulationGrowth];

	      If[!f, F=0];

	      If[rf===ConstantRecruitment,
	         R/E^((M+F)*(IndividAge - tR)),
	         Recruitment[S, opts]/E^((M+F)*(IndividAge - tR))
	      ]
	    ];


(*         Biomass of one cohort            *)


CohortBiomass[IndividAge_, opts___] := IndividNumbers[IndividAge, opts] * IndividualWeight[IndividAge, opts];


(*     Age of maximum CohortBiomass of cohort     *)

AgeOfMaxGrowth[opts___] :=
             Module[
              {d,b,m,k,S,R,M,F,W8,L8,tR,t0,tc,t8,uw,f,dt,bs,ts,bi,mr,mrb,re,rf,ma,cs,tms,tm,sfb,tcs,csp,msp},
	      {d,b,m,k,S,R,M,F,W8,L8,tR,t0,tc,t8,uw,f,dt,bs,ts,bi,mr,mrb,re,rf,ma,cs,tms,tm,sfb,tcs,csp,msp} =
	      optionslist /. {opts} /. Options[PopulationGrowth];

	      If[!f, F=0];

	      t0 + Log[1 + (b*k)/(M+F)]/k
	      ];


(*        Maximum CohortBiomass of cohort         *)

MaxBiomassGrowth[opts___] :=
            Module[
              {d,b,m,k,S,R,M,F,W8,L8,tR,t0,tc,t8,uw,f,dt,bs,ts,bi,mr,mrb,re,rf,ma,cs,tms,tm,sfb,tcs,csp,msp},
	      {d,b,m,k,S,R,M,F,W8,L8,tR,t0,tc,t8,uw,f,dt,bs,ts,bi,mr,mrb,re,rf,ma,cs,tms,tm,sfb,tcs,csp,msp} =
	      optionslist /. {opts} /. Options[PopulationGrowth];

	       If[rf===ConstantRecruitment,

	         E^(m*(k*(t0 + tR) + Log[m] - Log[b + m]))*(b/(b + m))^b*R*W8,

	         E^(m*(k*(t0 + tR) + Log[m] - Log[b + m]))*(b/(b + m))^b*Recruitment[S, opts]*W8
	      ]
	    ];


(*       Equilibrium CohortBiomass of stock       *)


EquilibriumBiomass[opts___] :=
           Module[
              {d,b,m,k,S,R,M,F,W8,L8,tR,t0,tc,t8,uw,f,dt,bs,ts,bi,mr,mrb,re,rf,ma,cs,tms,tm,sfb,tcs,csp,msp},
	      {d,b,m,k,S,R,M,F,W8,L8,tR,t0,tc,t8,uw,f,dt,bs,ts,bi,mr,mrb,re,rf,ma,cs,tms,tm,sfb,tcs,csp,msp} =
	      optionslist /. {opts} /. Options[PopulationGrowth];

	      If[rf === BevertonHoltRecruitment || rf === RickerRecruitment, R = Recruitment[S, opts]];

	      If[!uw, W8=d*L8^b];

	      If[!f, F=0];

                 If[
                    b===3,
                    -(E^((F + M)*tR)*(-(F + M)^(-1) + (3*E^(k*t0))/(F + k + M) - (3*E^(2*k*t0))/(F + 2*k + M) + E^(3*k*t0)/(F + 3*k + M))*R*W8),

                    If[k===M+F,

                      (E^(k*(-t0 + tR))*(1 - (1 - E^(k*t0))^b + E^(k*t0)*(1 - E^(k*t0))^b)*R*W8)/((1 + b)*k),

                      (R*W8*Beta[E^(k*t0), (F + M)/k, 1 + b])/(E^((F + M)*(t0 - tR))*k)
                    ]
                 ]

           ];


(*----------------------------------------*)
(*             Biomass of stock           *)
(*----------------------------------------*)

PopulationBiomass[IndividAge_, opts___] :=
           Module[
              {d,b,m,k,S,R,M,F,W8,L8,tR,t0,tc,t8,uw,f,dt,bs,ts,bi,mr,mrb,re,rf,ma,cs,tms,tm,sfb,tcs,csp,msp, t, cfs},
	      {d,b,m,k,S,R,M,F,W8,L8,tR,t0,tc,t8,uw,f,dt,bs,ts,bi,mr,mrb,re,rf,ma,cs,tms,tm,sfb,tcs,csp,msp} =
	      optionslist /. {opts} /. Options[PopulationGrowth];

	      If[rf === BevertonHoltRecruitment || rf === RickerRecruitment, R = Recruitment[S, opts]];

	      If[!uw, W8=d*L8^b];

	      If[!f, F=0];

	      If[dt,

	         cfs = Table[CohortBiomass[t, opts], {t, 0, IndividAge, ts}];
	         If[bs, ts*Apply[Plus, cfs], cfs, cfs],



		  If[IndividAge===Infinity,

		     If[b===3,
		       -(E^(M*tR)*(-(F + M)^(-1) + (3*E^(k*t0))/(F + k + M) - (3*E^(2*k*t0))/(F + 2*k + M) + E^(3*k*t0)/(F + 3*k + M))*R*W8),

		       (R*W8*Beta[E^(k*t0), (F + M)/k, 1 + b])/(E^((F + M)*(t0 - tR))*k)
		     ],


              If[b===3,
                  -(E^((F + M)*tR)*((-1 + E^(-((F + M)*IndividAge)))/(F + M) - (3*E^(k*t0)*(-1 + E^(-((F + k + M)*IndividAge))))/(F + k + M) +
                   (3*E^(2*k*t0)*(-1 + E^(-((F + 2*k + M)*IndividAge))))/(F + 2*k + M) - (E^(3*k*t0)*(-1 + E^(-((F + 3*k + M)*IndividAge))))/(F + 3*k + M))*R*W8),

		      If[
			 t0===0,

                         If[M===k,

                         (E^(k*(tR - (1 + b)*IndividAge))*(-1 + E^(k*IndividAge))^(1 + b)*R*W8)/((1 + b)*k),

			 (E^((F + M)*tR)*R*W8*(Beta[E^(k*IndividAge), -((F + b*k + M)/k), 1 + b] *
                           Gamma[-((F - k + M)/k)] - Gamma[1 + b]*Gamma[-((F + b*k + M)/k)]))/((-1)^b*k*Gamma[-((F - k + M)/k)])
                           ],

			 If[k === (M + F),

			    ((-(E^(k*(t0 + IndividAge + b*IndividAge))*(1 - E^(k*t0))^(1 + b)) +
			       E^(k*t0)*(-E^(k*t0) + E^(k*IndividAge))^(1 + b))*R*W8)/(E^(k*(2*t0 - tR + IndividAge + b*IndividAge))*(1 + b)*k),


			       If[

			         NumberQ[b] && NumberQ[k] && NumberQ[R] && NumberQ[M] && NumberQ[F] && NumberQ[tR] && NumberQ[t0] &&
			         (NumberQ[W8] || (NumberQ[L8] && NumberQ[d])),

			         NIntegrate[
			           CohortBiomass[t, opts],
			           {t, 0, IndividAge}
			        ],

			        PGX
			       ]

			 ]

		      ]

		   ]

                ]]
             ];


(*
NBevertonHoltModel[opts___] :=
           Module[
              {d,b,m,k,S,R,M,F,W8,L8,tR,t0,tc,t8,uw,f,dt,bs,ts,bi,mr,mrb,re,rf,ma,cs,tms,tm,sfb,tcs,csp,msp, t, listdata, cfs},
	      {d,b,m,k,S,R,M,F,W8,L8,tR,t0,tc,t8,uw,f,dt,bs,ts,bi,mr,mrb,re,rf,ma,cs,tms,tm,sfb,tcs,csp,msp} =
	      optionslist /. {opts} /. Options[PopulationGrowth];



	      If[rf === BevertonHoltRecruitment || rf === RickerRecruitment, R = Recruitment[S, opts]];
	      If[!uw, W8=d*L8^b];
	      If[!f, F=0];
	      temporaryF = F;


	      If[dt,
	        cfs = Table[CohortBiomass[t, FishingMortalityRate :> temporaryF * CatchSelectionFunction[t, opts], opts], {t, tR, t8, ts}];
	        If[bs, ts*Apply[Plus, cfs], cfs, cfs],


		NIntegrate[
		   CohortBiomass[t, FishingMortalityRate :> temporaryF * CatchSelectionFunction[t, opts], opts],
		   {t, tR, t8},
		   MaxRecursion->12
		]
	      ]

           ];

*)

PopulationBiomassOfMaxGrowth[opts___] := PopulationFractionBiomass[CatchAge->0, OldestAge->AgeOfMaxGrowth[opts, Fishing -> False], Fishing -> False, opts];


MaximumBiomassGrowth[opts___]         := CohortBiomass[AgeOfMaxGrowth[opts], opts];


PopulationFractionBiomass[opts___] :=
           Module[
              {d,b,m,k,S,R,M,F,W8,L8,tR,t0,tc,t8,uw,f,dt,bs,ts,bi,mr,mrb,re,rf,ma,cs,tms,tm,sfb,tcs,csp,msp,pfb,cs1},
	      {d,b,m,k,S,R,M,F,W8,L8,tR,t0,tc,t8,uw,f,dt,bs,ts,bi,mr,mrb,re,rf,ma,cs,tms,tm,sfb,tcs,csp,msp} =
	      optionslist /. {opts} /. Options[PopulationGrowth];

	      If[rf === BevertonHoltRecruitment || rf === RickerRecruitment, R = Recruitment[S, opts]];

	       If[dt,

                 cs1 = Table[CohortBiomass[t+tc, opts], {t, 0, Evaluate[t8-tc], ts}];
	         If[bs, pfb = ts*Apply[Plus, cs1], pfb = cs1, pfb = cs1],


	         If[!uw, W8=d*L8^b];
	         If[!f, F=0];


	          If[t8===Infinity && b===3,


	            pfb = -(E^(M*(tR - tc))*(-(F + M)^(-1) + (3*E^(k*(t0 - tc)))/(F + k + M) -
			         (3*E^(2*k*(t0 - tc)))/(F + 2*k + M) + E^(3*k*(t0 - tc))/(F + 3*k + M))*R*W8),


			If[k === (M+F),

			   pfb = (R*W8*(t8 - tc)*((-E^(k*t0) + E^(k*t8))^(1 + b)/(E^(k*(t0  - tR + t8 + b*t8))*(t8 - tc)) +
			         (-E^(k* t0) + E^(k*tc))^(1 + b)/(E^(k*(t0 - tR + tc + b*tc))*(-t8 + tc))))/((1 + b)*k),

		      If[tc===0,

			pfb = PopulationBiomass[t8, opts],



				 If[b===3,

				   pfb = -(E^(M*tR + F*tc)*(1/(E^((F + M)*t8)*(F + M)) - 1/(E^((F + M)*tc)*(F + M)) - (3*E^(k*(t0 - t8) - (F + M)*t8))/
					  (F + k + M) + (3*E^(k*(t0 - tc) - (F + M)*tc))/(F + k + M) + (3*E^(2*k*(t0 - t8) - (F + M)*t8))/(F + 2*k + M) -
					  (3*E^(2*k*(t0 - tc) - (F + M)*tc))/(F + 2*k + M) - E^(3*k*(t0 - t8) - (F + M)*t8)/(F + 3*k + M) +
					   E^(3*k*(t0 - tc) - (F + M)*tc)/(F + 3*k + M))*R*W8),



				   If[t0===0,

				           pfb = (E^(M*tR - I*b*Pi + F*tc)*R*W8*(

						(*    This term probably equals 0 when t8=Infinity  *)

					   If[t8===Infinity, 0, Beta[E^(k*t8), -((F + b*k + M)/k), 1 + b]]

					   - Beta[E^(k*tc), -((F + b*k + M)/k), 1 + b]))/k,

			                   If[
                                             NumberQ[b] && NumberQ[k] && NumberQ[R] && NumberQ[M] && NumberQ[F] && NumberQ[tR] && NumberQ[t0] &&
			                       (NumberQ[W8] || (NumberQ[L8] && NumberQ[d])),



					     pfb = NIntegrate[E^(M*(tR - tc) - (F + M)*(t - tc))*(1 - E^(-(k*(-t0 + t))))^b*R*W8, {t, tc, t8}


			                   ],

			                   pfb = PGX


			            ]

				 ]
                       ]
		   ]
                  ]
                   ]
                   ];

                   If[NumericQ[pfb], pfb = Re[pfb]];

                   pfb

               ];



BevertonHoltModel[opts___] :=
            Module[
              {d,b,m,k,S,R,M,F,W8,L8,tR,t0,tc,t8,uw,f,dt,bs,ts,bi,mr,mrb,re,rf,ma,cs,tms,tm,sfb,tcs,csp,msp,stock1,stock2,out},
	      {d,b,m,k,S,R,M,F,W8,L8,tR,t0,tc,t8,uw,f,dt,bs,ts,bi,mr,mrb,re,rf,ma,cs,tms,tm,sfb,tcs,csp,msp} =
	      optionslist /. {opts} /. Options[PopulationGrowth];

	      If[!f, stock1=PopulationBiomass[t8, opts]; stock2=0,

		      If[!uw, W8=d*L8^b];
		      If[tcs > tc, tcs = tc];

		      If[
			 bi === Fishable,
			 stock1={},

			 If[tc===0,
			    stock1 = 0,
			    If[dt,
			       stock1 = PopulationFractionBiomass[CatchAge -> 0, OldestAge -> tc-ts, Fishing -> False, opts],
			       stock1 = PopulationFractionBiomass[CatchAge -> 0, OldestAge -> tc,    Fishing -> False, opts]
			    ]
			 ]
		      ];

		      If[cs === Sharp || tc === tcs,
			 stock2 = PopulationFractionBiomass[
				      CatchAge   -> tc,
				      OldestAge  -> t8,
				      Fishing    -> True,
				      opts
				   ],


			 If[dt,
			   stock2 = Table[CohortBiomass[t+tc, FishingMortalityRate:>F*CatchSelectionFunction[t+tc, opts], opts], {t, 0, Evaluate[t8-tc], ts}];
			   If[bs, stock2 = ts * Apply[Plus, stock2]],

			   stock2 = NIntegrate[E^(M*(tR-tc)-(F*CatchSelectionFunction[t, opts] + M)*(t-tc))*(1 - E^(-(k*(-t0+t))))^b*R*W8, {t, tc, t8}]
			 ]
		      ]];

		      If[bi === Fishable,        out = stock2];
		      If[bi === NotFishable,     out = stock1];

		      If[bi === All,
			 If[ListQ[stock2] && ListQ[stock1],
			     out = Join[stock1, stock2],
			     out =  stock1 + stock2
			 ]
		      ];

		      If[bi === AllParts, out = {stock1,  stock2}];

		   out

	    ];

(*
LogisticGrowth[t_, x0_, m_, r_, mb_] :=

                    Module[{out},


                      If[m === 1,

                        out = mb*(x0/mb)^E^(-(r*t)),

                        out = (((-1 + E^(r*t))*mb^(1 - m)^(-1) + x0^(1 - m)^(-1))/E^(r*t))^(1 - m),

                        out = PGma
                      ];

                      If[Head[out]===Complex, out = 0];
                      out
                    ];

*)

SelectionFunction[t_, tc_, tcs_, s_:.1] :=

                    Module[{out},


                      If[tc === tcs,

             (*          out = Which[t < tc, 0, t >= tc, 1],  *)

                        out = If[t < tc, 0, 1, 1],

                        out = 1 - (1 + (s/(1 - s))^((t - tc)/(-tc + tcs)))^(-1)

                      ];

                      out
                    ];



CatchSelectionFunction[t_, opts___] :=

                    Module[
                       {tc,cs,tcs,csp},
                       {tc,cs,tcs,csp} =
	               {
		       CatchAge,
		       CatchSelection,
		       FirstCatchAge,
		       CatchStartPercentage
                      } /. {opts} /. Options[PopulationGrowth];

                      If[cs===Sharp, tcs=tc];
                      SelectionFunction[t, tc, tcs, csp]

                    ];


 MaturationFunction[t_, opts___] :=

                     Module[
                        {ma,tms,tm,msp},
                        {ma,tms,tm,msp} =
 	                {
 		        Maturation,
 		        MaturationStartAge,
 		        MaturationAge,
 		        MaturationStartPercentage
                        } /. {opts} /. Options[PopulationGrowth];

                        If[ma===Sharp, tms=tm];
                        SelectionFunction[t, tm, tms, msp]

                     ];


(*

SelectionFunction[t_, cs_, tc_, tcs_, sfb_, opts___] :=

                    Module[
                       {r, x0=.00001, mb=1, out},


                      If[cs===Sharp,

                         out = Which[t < tc, 0, t >= tc, 1],

			      If[tc > tcs,

				If[sfb === 1,
				   r = -(Log[Log[1/(2*mb)]/Log[x0/mb]]/(tc-tcs)),
				   r = Log[(-1 + mb^(-1 + sfb)^(-1)/x0^(-1 + sfb)^(-1))/(-1 + 2^(-1 + sfb)^(-1)*mb^(-1 + sfb)^(-1))]/(tc-tcs),
				   PGrr
				];
				out = LogisticGrowth[t-tcs, x0, sfb, r, mb],

				If[tc===tcs,
				     out = Which[t < tc, 0, t >= tc, 1],
				     out = PGma
				]
			      ]
		      ];

                      Max[Min[out,1],0]

                    ];

CatchSelectionFunction[t_, opts___] :=

                    Module[
                       {tc,cs,sfb,tcs,csp,msp},
                       {tc,cs,sfb,tcs,csp,msp} =
	               {
		       CatchAge,
		       CatchSelection,
		       CatchSelectionFunctionBend,
		       FirstCatchAge
                      } /. {opts} /. Options[PopulationGrowth];

                      SelectionFunction[t, cs, tc, tcs, sfb]

                    ];


 MaturationFunction[t_, opts___] :=

                     Module[
                        {ma,tms,tm,mfb},
                        {ma,tms,tm,mfb} =
 	                {
 		        Maturation,
 		        MaturationStartAge,
 		        MaturationAge,
 		        MaturationFunctionBend
                        } /. {opts} /. Options[PopulationGrowth];

                        SelectionFunction[t, ma, tm, tms, mfb]

                     ];

*)


TotalCatch[opts___] :=
            Module[
	      {d,b,m,k,S,R,M,F,W8,L8,tR,t0,tc,t8,uw,f,dt,bs,ts,bi,mr,mrb,re,rf,ma,cs,tms,tm,sfb,tcs,csp,msp, out},
	      {d,b,m,k,S,R,M,F,W8,L8,tR,t0,tc,t8,uw,f,dt,bs,ts,bi,mr,mrb,re,rf,ma,cs,tms,tm,sfb,tcs,csp,msp} =
	      optionslist /. {opts} /. Options[PopulationGrowth];


	      If[cs===Sharp,
	         out = F * PopulationGrowth[BiomassIncluded->Fishable, opts],

	         If[dt,
	            out = Table[F*CatchSelectionFunction[t, opts] * CohortBiomass[t, FishingMortalityRate:>F*CatchSelectionFunction[t, opts], opts], {t, tR, t8, ts}];
	            If[bs, out = ts * Apply[Plus, out]],

(*	            out = NIntegrate[
	                    F*CatchSelectionFunction[
	                                t, opts
	                      ] * ( E^(M*(tR-tc)-(F*CatchSelectionFunction[t, opts] + M)*(t-tc))*(1 - E^(-(k*(-t0+t))))^b*R*W8 ), {t, tcs, t8}]

*)
                    out = NIntegrate[
                                 F * CatchSelectionFunction[t, opts] *
                                 CohortBiomass[
                                     t,
                                     Fishing              -> True,
                                     FishingMortalityRate :> Evaluate[F * CatchSelectionFunction[t, opts]],
                                     opts
                                 ],
                                 {t, 0, t8}
                          ]


                 ]
	      ];
	      out
	    ];



FishingMortalityEquilibrium[X_, opts___] :=


           Module[
	      {d,b,m,k,S,R,M,F,W8,L8,tR,t0,tc,t8,uw,f,dt,bs,ts,bi,mr,mrb,re,rf,ma,cs,tms,tm,sfb,tcs,csp,msp},
	      {d,b,m,k,S,R,M,F,W8,L8,tR,t0,tc,t8,uw,f,dt,bs,ts,bi,mr,mrb,re,rf,ma,cs,tms,tm,sfb,tcs,csp,msp} =
	      optionslist /. {opts} /. Options[PopulationGrowth];

	      If[rf === BevertonHoltRecruitment || rf === RickerRecruitment, R = Recruitment[S, opts]];


	      If[!uw, W8=d*L8^b];


              If[tc===0 && t0===0 && b===3,

                     (-3*k*X - 2*M*X + Sqrt[5*k^2*X^2 + 4*X*Sqrt[6*E^(M*tR)*k^3*R*W8*X + k^4*X^2]])/(2*X),



			If[tc===0 && b===3,

			-(-(E^(M*tR)*R*W8) + 3*E^(k*t0 + M*tR)*R*W8 - 3*E^(2*k*t0 + M*tR)*R*W8 +
			      E^(3*k*t0 + M*tR)*R*W8 + 6*k*X + 4*M*X)/(4*X) +
			   Sqrt[(-(E^(M*tR)*R*W8) + 3*E^(k*t0 + M*tR)*R*W8 - 3*E^(2*k*t0 + M*tR)*R*W8 +
				 E^(3*k*t0 + M*tR)*R*W8 + 6*k*X + 4*M*X)^2/(4*X^2) -
			      (2*(-6*E^(M*tR)*k*R*W8 + 15*E^(k*t0 + M*tR)*k*R*W8 -
				 12*E^(2*k*t0 + M*tR)*k*R*W8 + 3*E^(3*k*t0 + M*tR)*k*R*W8 -
				 3*E^(M*tR)*M*R*W8 + 9*E^(k*t0 + M*tR)*M*R*W8 - 9*E^(2*k*t0 + M*tR)*M*R*
				  W8 + 3*E^(3*k*t0 + M*tR)*M*R*W8 + 11*k^2*X + 18*k*M*X + 6*M^2*X))/
			       (3*X) + (2^(1/3)*k^2*(3*E^(2*M*tR)*R^2*W8^2 - 27*E^(k*t0 + 2*M*tR)*R^2*
				  W8^2 + 81*E^(2*k*t0 + 2*M*tR)*R^2*W8^2 - 114*E^(3*k*t0 + 2*M*tR)*R^2*
				  W8^2 + 81*E^(4*k*t0 + 2*M*tR)*R^2*W8^2 - 27*E^(5*k*t0 + 2*M*tR)*R^2*
				  W8^2 + 3*E^(6*k*t0 + 2*M*tR)*R^2*W8^2 + 12*E^(M*tR)*k*R*W8*X -
				 48*E^(k*t0 + M*tR)*k*R*W8*X - 48*E^(2*k*t0 + M*tR)*k*R*W8*X +
				 12*E^(3*k*t0 + M*tR)*k*R*W8*X + 13*k^2*X^2))/
			       (3*X*(2*(-6*E^(M*tR)*k*R*W8 + 15*E^(k*t0 + M*tR)*k*R*W8 -
				     12*E^(2*k*t0 + M*tR)*k*R*W8 + 3*E^(3*k*t0 + M*tR)*k*R*W8 -
				     3*E^(M*tR)*M*R*W8 + 9*E^(k*t0 + M*tR)*M*R*W8 - 9*E^(2*k*t0 + M*tR)*
				      M*R*W8 + 3*E^(3*k*t0 + M*tR)*M*R*W8 + 11*k^2*X + 18*k*M*X +
				     6*M^2*X)^3 - 9*(-(E^(M*tR)*R*W8) + 3*E^(k*t0 + M*tR)*R*W8 -
				    3*E^(2*k*t0 + M*tR)*R*W8 + E^(3*k*t0 + M*tR)*R*W8 + 6*k*X + 4*M*X)*
				   (-6*E^(M*tR)*k*R*W8 + 15*E^(k*t0 + M*tR)*k*R*W8 -
				    12*E^(2*k*t0 + M*tR)*k*R*W8 + 3*E^(3*k*t0 + M*tR)*k*R*W8 -
				    3*E^(M*tR)*M*R*W8 + 9*E^(k*t0 + M*tR)*M*R*W8 - 9*E^(2*k*t0 + M*tR)*M*
				     R*W8 + 3*E^(3*k*t0 + M*tR)*M*R*W8 + 11*k^2*X + 18*k*M*X + 6*M^2*X)*
				   (-11*E^(M*tR)*k^2*R*W8 + 18*E^(k*t0 + M*tR)*k^2*R*W8 -
				    9*E^(2*k*t0 + M*tR)*k^2*R*W8 + 2*E^(3*k*t0 + M*tR)*k^2*R*W8 -
				    12*E^(M*tR)*k*M*R*W8 + 30*E^(k*t0 + M*tR)*k*M*R*W8 -
				    24*E^(2*k*t0 + M*tR)*k*M*R*W8 + 6*E^(3*k*t0 + M*tR)*k*M*R*W8 -
				    3*E^(M*tR)*M^2*R*W8 + 9*E^(k*t0 + M*tR)*M^2*R*W8 -
				    9*E^(2*k*t0 + M*tR)*M^2*R*W8 + 3*E^(3*k*t0 + M*tR)*M^2*R*W8 +
				    6*k^3*X + 22*k^2*M*X + 18*k*M^2*X + 4*M^3*X) +
				  27*X*(-11*E^(M*tR)*k^2*R*W8 + 18*E^(k*t0 + M*tR)*k^2*R*W8 -
				     9*E^(2*k*t0 + M*tR)*k^2*R*W8 + 2*E^(3*k*t0 + M*tR)*k^2*R*W8 -
				     12*E^(M*tR)*k*M*R*W8 + 30*E^(k*t0 + M*tR)*k*M*R*W8 -
				     24*E^(2*k*t0 + M*tR)*k*M*R*W8 + 6*E^(3*k*t0 + M*tR)*k*M*R*W8 -
				     3*E^(M*tR)*M^2*R*W8 + 9*E^(k*t0 + M*tR)*M^2*R*W8 -
				     9*E^(2*k*t0 + M*tR)*M^2*R*W8 + 3*E^(3*k*t0 + M*tR)*M^2*R*W8 +
				     6*k^3*X + 22*k^2*M*X + 18*k*M^2*X + 4*M^3*X)^2 +
				  27*(-(E^(M*tR)*R*W8) + 3*E^(k*t0 + M*tR)*R*W8 - 3*E^(2*k*t0 + M*tR)*R*
				      W8 + E^(3*k*t0 + M*tR)*R*W8 + 6*k*X + 4*M*X)^2*
				   (-6*E^(M*tR)*k^3*R*W8 - 11*E^(M*tR)*k^2*M*R*W8 + 18*E^(k*t0 + M*tR)*
				     k^2*M*R*W8 - 9*E^(2*k*t0 + M*tR)*k^2*M*R*W8 + 2*E^(3*k*t0 + M*tR)*
				     k^2*M*R*W8 - 6*E^(M*tR)*k*M^2*R*W8 + 15*E^(k*t0 + M*tR)*k*M^2*R*
				     W8 - 12*E^(2*k*t0 + M*tR)*k*M^2*R*W8 + 3*E^(3*k*t0 + M*tR)*k*M^2*R*
				     W8 - E^(M*tR)*M^3*R*W8 + 3*E^(k*t0 + M*tR)*M^3*R*W8 -
				    3*E^(2*k*t0 + M*tR)*M^3*R*W8 + E^(3*k*t0 + M*tR)*M^3*R*W8 +
				    6*k^3*M*X + 11*k^2*M^2*X + 6*k*M^3*X + M^4*X) -
				  72*X*(-6*E^(M*tR)*k*R*W8 + 15*E^(k*t0 + M*tR)*k*R*W8 -
				    12*E^(2*k*t0 + M*tR)*k*R*W8 + 3*E^(3*k*t0 + M*tR)*k*R*W8 -
				    3*E^(M*tR)*M*R*W8 + 9*E^(k*t0 + M*tR)*M*R*W8 - 9*E^(2*k*t0 + M*tR)*M*
				     R*W8 + 3*E^(3*k*t0 + M*tR)*M*R*W8 + 11*k^2*X + 18*k*M*X + 6*M^2*X)*
				   (-6*E^(M*tR)*k^3*R*W8 - 11*E^(M*tR)*k^2*M*R*W8 + 18*E^(k*t0 + M*tR)*
				     k^2*M*R*W8 - 9*E^(2*k*t0 + M*tR)*k^2*M*R*W8 + 2*E^(3*k*t0 + M*tR)*
				     k^2*M*R*W8 - 6*E^(M*tR)*k*M^2*R*W8 + 15*E^(k*t0 + M*tR)*k*M^2*R*
				     W8 - 12*E^(2*k*t0 + M*tR)*k*M^2*R*W8 + 3*E^(3*k*t0 + M*tR)*k*M^2*R*
				     W8 - E^(M*tR)*M^3*R*W8 + 3*E^(k*t0 + M*tR)*M^3*R*W8 -
				    3*E^(2*k*t0 + M*tR)*M^3*R*W8 + E^(3*k*t0 + M*tR)*M^3*R*W8 +
				    6*k^3*M*X + 11*k^2*M^2*X + 6*k*M^3*X + M^4*X) +
				  Sqrt[-4*(3*E^(2*M*tR)*k^2*R^2*W8^2 - 27*E^(k*t0 + 2*M*tR)*k^2*R^2*
					W8^2 + 81*E^(2*k*t0 + 2*M*tR)*k^2*R^2*W8^2 - 114*
					E^(3*k*t0 + 2*M*tR)*k^2*R^2*W8^2 + 81*E^(4*k*t0 + 2*M*tR)*k^2*
					R^2*W8^2 - 27*E^(5*k*t0 + 2*M*tR)*k^2*R^2*W8^2 + 3*
					E^(6*k*t0 + 2*M*tR)*k^2*R^2*W8^2 + 12*E^(M*tR)*k^3*R*W8*X - 48*
					E^(k*t0 + M*tR)*k^3*R*W8*X - 48*E^(2*k*t0 + M*tR)*k^3*R*W8*X +
				       12*E^(3*k*t0 + M*tR)*k^3*R*W8*X + 13*k^4*X^2)^3 +
				    (2*(-6*E^(M*tR)*k*R*W8 + 15*E^(k*t0 + M*tR)*k*R*W8 -
					 12*E^(2*k*t0 + M*tR)*k*R*W8 + 3*E^(3*k*t0 + M*tR)*k*R*W8 -
					 3*E^(M*tR)*M*R*W8 + 9*E^(k*t0 + M*tR)*M*R*W8 -
					 9*E^(2*k*t0 + M*tR)*M*R*W8 + 3*E^(3*k*t0 + M*tR)*M*R*W8 +
					 11*k^2*X + 18*k*M*X + 6*M^2*X)^3 - 9*(-(E^(M*tR)*R*W8) +
					3*E^(k*t0 + M*tR)*R*W8 - 3*E^(2*k*t0 + M*tR)*R*W8 +
					E^(3*k*t0 + M*tR)*R*W8 + 6*k*X + 4*M*X)*(-6*E^(M*tR)*k*R*W8 +
					15*E^(k*t0 + M*tR)*k*R*W8 - 12*E^(2*k*t0 + M*tR)*k*R*W8 +
					3*E^(3*k*t0 + M*tR)*k*R*W8 - 3*E^(M*tR)*M*R*W8 +
					9*E^(k*t0 + M*tR)*M*R*W8 - 9*E^(2*k*t0 + M*tR)*M*R*W8 +
					3*E^(3*k*t0 + M*tR)*M*R*W8 + 11*k^2*X + 18*k*M*X + 6*M^2*X)*(
					-11*E^(M*tR)*k^2*R*W8 + 18*E^(k*t0 + M*tR)*k^2*R*W8 -
					9*E^(2*k*t0 + M*tR)*k^2*R*W8 + 2*E^(3*k*t0 + M*tR)*k^2*R*W8 -
					12*E^(M*tR)*k*M*R*W8 + 30*E^(k*t0 + M*tR)*k*M*R*W8 -
					24*E^(2*k*t0 + M*tR)*k*M*R*W8 + 6*E^(3*k*t0 + M*tR)*k*M*R*W8 -
					3*E^(M*tR)*M^2*R*W8 + 9*E^(k*t0 + M*tR)*M^2*R*W8 -
					9*E^(2*k*t0 + M*tR)*M^2*R*W8 + 3*E^(3*k*t0 + M*tR)*M^2*R*W8 +
					6*k^3*X + 22*k^2*M*X + 18*k*M^2*X + 4*M^3*X) +
				      27*X*(-11*E^(M*tR)*k^2*R*W8 + 18*E^(k*t0 + M*tR)*k^2*R*W8 -
					 9*E^(2*k*t0 + M*tR)*k^2*R*W8 + 2*E^(3*k*t0 + M*tR)*k^2*R*W8 -
					 12*E^(M*tR)*k*M*R*W8 + 30*E^(k*t0 + M*tR)*k*M*R*W8 -
					 24*E^(2*k*t0 + M*tR)*k*M*R*W8 + 6*E^(3*k*t0 + M*tR)*k*M*R*W8 -
					 3*E^(M*tR)*M^2*R*W8 + 9*E^(k*t0 + M*tR)*M^2*R*W8 -
					 9*E^(2*k*t0 + M*tR)*M^2*R*W8 + 3*E^(3*k*t0 + M*tR)*M^2*R*W8 +
					 6*k^3*X + 22*k^2*M*X + 18*k*M^2*X + 4*M^3*X)^2 +
				      27*(-(E^(M*tR)*R*W8) + 3*E^(k*t0 + M*tR)*R*W8 -
					 3*E^(2*k*t0 + M*tR)*R*W8 + E^(3*k*t0 + M*tR)*R*W8 + 6*k*X +
					 4*M*X)^2*(-6*E^(M*tR)*k^3*R*W8 - 11*E^(M*tR)*k^2*M*R*W8 +
					18*E^(k*t0 + M*tR)*k^2*M*R*W8 - 9*E^(2*k*t0 + M*tR)*k^2*M*R*W8 +
					2*E^(3*k*t0 + M*tR)*k^2*M*R*W8 - 6*E^(M*tR)*k*M^2*R*W8 +
					15*E^(k*t0 + M*tR)*k*M^2*R*W8 - 12*E^(2*k*t0 + M*tR)*k*M^2*R*
					 W8 + 3*E^(3*k*t0 + M*tR)*k*M^2*R*W8 - E^(M*tR)*M^3*R*W8 +
					3*E^(k*t0 + M*tR)*M^3*R*W8 - 3*E^(2*k*t0 + M*tR)*M^3*R*W8 +
					E^(3*k*t0 + M*tR)*M^3*R*W8 + 6*k^3*M*X + 11*k^2*M^2*X +
					6*k*M^3*X + M^4*X) - 72*X*(-6*E^(M*tR)*k*R*W8 +
					15*E^(k*t0 + M*tR)*k*R*W8 - 12*E^(2*k*t0 + M*tR)*k*R*W8 +
					3*E^(3*k*t0 + M*tR)*k*R*W8 - 3*E^(M*tR)*M*R*W8 +
					9*E^(k*t0 + M*tR)*M*R*W8 - 9*E^(2*k*t0 + M*tR)*M*R*W8 +
					3*E^(3*k*t0 + M*tR)*M*R*W8 + 11*k^2*X + 18*k*M*X + 6*M^2*X)*(
					-6*E^(M*tR)*k^3*R*W8 - 11*E^(M*tR)*k^2*M*R*W8 +
					18*E^(k*t0 + M*tR)*k^2*M*R*W8 - 9*E^(2*k*t0 + M*tR)*k^2*M*R*W8 +
					2*E^(3*k*t0 + M*tR)*k^2*M*R*W8 - 6*E^(M*tR)*k*M^2*R*W8 +
					15*E^(k*t0 + M*tR)*k*M^2*R*W8 - 12*E^(2*k*t0 + M*tR)*k*M^2*R*
					 W8 + 3*E^(3*k*t0 + M*tR)*k*M^2*R*W8 - E^(M*tR)*M^3*R*W8 +
					3*E^(k*t0 + M*tR)*M^3*R*W8 - 3*E^(2*k*t0 + M*tR)*M^3*R*W8 +
					E^(3*k*t0 + M*tR)*M^3*R*W8 + 6*k^3*M*X + 11*k^2*M^2*X +
					6*k*M^3*X + M^4*X))^2])^(1/3)) +
			      (2*(-6*E^(M*tR)*k*R*W8 + 15*E^(k*t0 + M*tR)*k*R*W8 - 12*E^(2*k*t0 + M*tR)*
				     k*R*W8 + 3*E^(3*k*t0 + M*tR)*k*R*W8 - 3*E^(M*tR)*M*R*W8 +
				    9*E^(k*t0 + M*tR)*M*R*W8 - 9*E^(2*k*t0 + M*tR)*M*R*W8 +
				    3*E^(3*k*t0 + M*tR)*M*R*W8 + 11*k^2*X + 18*k*M*X + 6*M^2*X)^3 -
				 9*(-(E^(M*tR)*R*W8) + 3*E^(k*t0 + M*tR)*R*W8 - 3*E^(2*k*t0 + M*tR)*R*
				    W8 + E^(3*k*t0 + M*tR)*R*W8 + 6*k*X + 4*M*X)*(-6*E^(M*tR)*k*R*W8 +
				   15*E^(k*t0 + M*tR)*k*R*W8 - 12*E^(2*k*t0 + M*tR)*k*R*W8 +
				   3*E^(3*k*t0 + M*tR)*k*R*W8 - 3*E^(M*tR)*M*R*W8 + 9*E^(k*t0 + M*tR)*M*
				    R*W8 - 9*E^(2*k*t0 + M*tR)*M*R*W8 + 3*E^(3*k*t0 + M*tR)*M*R*W8 +
				   11*k^2*X + 18*k*M*X + 6*M^2*X)*(-11*E^(M*tR)*k^2*R*W8 +
				   18*E^(k*t0 + M*tR)*k^2*R*W8 - 9*E^(2*k*t0 + M*tR)*k^2*R*W8 +
				   2*E^(3*k*t0 + M*tR)*k^2*R*W8 - 12*E^(M*tR)*k*M*R*W8 +
				   30*E^(k*t0 + M*tR)*k*M*R*W8 - 24*E^(2*k*t0 + M*tR)*k*M*R*W8 +
				   6*E^(3*k*t0 + M*tR)*k*M*R*W8 - 3*E^(M*tR)*M^2*R*W8 +
				   9*E^(k*t0 + M*tR)*M^2*R*W8 - 9*E^(2*k*t0 + M*tR)*M^2*R*W8 +
				   3*E^(3*k*t0 + M*tR)*M^2*R*W8 + 6*k^3*X + 22*k^2*M*X + 18*k*M^2*X +
				   4*M^3*X) + 27*X*(-11*E^(M*tR)*k^2*R*W8 + 18*E^(k*t0 + M*tR)*k^2*R*
				     W8 - 9*E^(2*k*t0 + M*tR)*k^2*R*W8 + 2*E^(3*k*t0 + M*tR)*k^2*R*W8 -
				    12*E^(M*tR)*k*M*R*W8 + 30*E^(k*t0 + M*tR)*k*M*R*W8 -
				    24*E^(2*k*t0 + M*tR)*k*M*R*W8 + 6*E^(3*k*t0 + M*tR)*k*M*R*W8 -
				    3*E^(M*tR)*M^2*R*W8 + 9*E^(k*t0 + M*tR)*M^2*R*W8 -
				    9*E^(2*k*t0 + M*tR)*M^2*R*W8 + 3*E^(3*k*t0 + M*tR)*M^2*R*W8 +
				    6*k^3*X + 22*k^2*M*X + 18*k*M^2*X + 4*M^3*X)^2 +
				 27*(-(E^(M*tR)*R*W8) + 3*E^(k*t0 + M*tR)*R*W8 - 3*E^(2*k*t0 + M*tR)*R*
				     W8 + E^(3*k*t0 + M*tR)*R*W8 + 6*k*X + 4*M*X)^2*
				  (-6*E^(M*tR)*k^3*R*W8 - 11*E^(M*tR)*k^2*M*R*W8 + 18*E^(k*t0 + M*tR)*
				    k^2*M*R*W8 - 9*E^(2*k*t0 + M*tR)*k^2*M*R*W8 + 2*E^(3*k*t0 + M*tR)*
				    k^2*M*R*W8 - 6*E^(M*tR)*k*M^2*R*W8 + 15*E^(k*t0 + M*tR)*k*M^2*R*W8 -
				   12*E^(2*k*t0 + M*tR)*k*M^2*R*W8 + 3*E^(3*k*t0 + M*tR)*k*M^2*R*W8 -
				   E^(M*tR)*M^3*R*W8 + 3*E^(k*t0 + M*tR)*M^3*R*W8 - 3*E^(2*k*t0 + M*tR)*
				    M^3*R*W8 + E^(3*k*t0 + M*tR)*M^3*R*W8 + 6*k^3*M*X + 11*k^2*M^2*X +
				   6*k*M^3*X + M^4*X) - 72*X*(-6*E^(M*tR)*k*R*W8 + 15*E^(k*t0 + M*tR)*k*
				    R*W8 - 12*E^(2*k*t0 + M*tR)*k*R*W8 + 3*E^(3*k*t0 + M*tR)*k*R*W8 -
				   3*E^(M*tR)*M*R*W8 + 9*E^(k*t0 + M*tR)*M*R*W8 - 9*E^(2*k*t0 + M*tR)*M*
				    R*W8 + 3*E^(3*k*t0 + M*tR)*M*R*W8 + 11*k^2*X + 18*k*M*X + 6*M^2*X)*
				  (-6*E^(M*tR)*k^3*R*W8 - 11*E^(M*tR)*k^2*M*R*W8 + 18*E^(k*t0 + M*tR)*
				    k^2*M*R*W8 - 9*E^(2*k*t0 + M*tR)*k^2*M*R*W8 + 2*E^(3*k*t0 + M*tR)*
				    k^2*M*R*W8 - 6*E^(M*tR)*k*M^2*R*W8 + 15*E^(k*t0 + M*tR)*k*M^2*R*W8 -
				   12*E^(2*k*t0 + M*tR)*k*M^2*R*W8 + 3*E^(3*k*t0 + M*tR)*k*M^2*R*W8 -
				   E^(M*tR)*M^3*R*W8 + 3*E^(k*t0 + M*tR)*M^3*R*W8 - 3*E^(2*k*t0 + M*tR)*
				    M^3*R*W8 + E^(3*k*t0 + M*tR)*M^3*R*W8 + 6*k^3*M*X + 11*k^2*M^2*X +
				   6*k*M^3*X + M^4*X) + Sqrt[-4*(3*E^(2*M*tR)*k^2*R^2*W8^2 -
				      27*E^(k*t0 + 2*M*tR)*k^2*R^2*W8^2 + 81*E^(2*k*t0 + 2*M*tR)*k^2*R^
					2*W8^2 - 114*E^(3*k*t0 + 2*M*tR)*k^2*R^2*W8^2 +
				      81*E^(4*k*t0 + 2*M*tR)*k^2*R^2*W8^2 - 27*E^(5*k*t0 + 2*M*tR)*k^2*R^
					2*W8^2 + 3*E^(6*k*t0 + 2*M*tR)*k^2*R^2*W8^2 + 12*E^(M*tR)*k^
					3*R*W8*X - 48*E^(k*t0 + M*tR)*k^3*R*W8*X - 48*E^(2*k*t0 + M*tR)*
				       k^3*R*W8*X + 12*E^(3*k*t0 + M*tR)*k^3*R*W8*X + 13*k^4*X^2)^3 +
				   (2*(-6*E^(M*tR)*k*R*W8 + 15*E^(k*t0 + M*tR)*k*R*W8 -
					12*E^(2*k*t0 + M*tR)*k*R*W8 + 3*E^(3*k*t0 + M*tR)*k*R*W8 -
					3*E^(M*tR)*M*R*W8 + 9*E^(k*t0 + M*tR)*M*R*W8 -
					9*E^(2*k*t0 + M*tR)*M*R*W8 + 3*E^(3*k*t0 + M*tR)*M*R*W8 +
					11*k^2*X + 18*k*M*X + 6*M^2*X)^3 - 9*(-(E^(M*tR)*R*W8) + 3*
					E^(k*t0 + M*tR)*R*W8 - 3*E^(2*k*t0 + M*tR)*R*W8 +
				       E^(3*k*t0 + M*tR)*R*W8 + 6*k*X + 4*M*X)*(-6*E^(M*tR)*k*R*W8 + 15*
					E^(k*t0 + M*tR)*k*R*W8 - 12*E^(2*k*t0 + M*tR)*k*R*W8 + 3*
					E^(3*k*t0 + M*tR)*k*R*W8 - 3*E^(M*tR)*M*R*W8 + 9*E^(k*t0 + M*tR)*
					M*R*W8 - 9*E^(2*k*t0 + M*tR)*M*R*W8 + 3*E^(3*k*t0 + M*tR)*M*R*
					W8 + 11*k^2*X + 18*k*M*X + 6*M^2*X)*(-11*E^(M*tR)*k^2*R*W8 + 18*
					E^(k*t0 + M*tR)*k^2*R*W8 - 9*E^(2*k*t0 + M*tR)*k^2*R*W8 + 2*
					E^(3*k*t0 + M*tR)*k^2*R*W8 - 12*E^(M*tR)*k*M*R*W8 + 30*
					E^(k*t0 + M*tR)*k*M*R*W8 - 24*E^(2*k*t0 + M*tR)*k*M*R*W8 + 6*
					E^(3*k*t0 + M*tR)*k*M*R*W8 - 3*E^(M*tR)*M^2*R*W8 + 9*
					E^(k*t0 + M*tR)*M^2*R*W8 - 9*E^(2*k*t0 + M*tR)*M^2*R*W8 + 3*
					E^(3*k*t0 + M*tR)*M^2*R*W8 + 6*k^3*X + 22*k^2*M*X + 18*k*M^2*
					X + 4*M^3*X) + 27*X*(-11*E^(M*tR)*k^2*R*W8 + 18*E^(k*t0 + M*tR)*
					 k^2*R*W8 - 9*E^(2*k*t0 + M*tR)*k^2*R*W8 + 2*E^(3*k*t0 + M*tR)*
					 k^2*R*W8 - 12*E^(M*tR)*k*M*R*W8 + 30*E^(k*t0 + M*tR)*k*M*R*W8 -
					24*E^(2*k*t0 + M*tR)*k*M*R*W8 + 6*E^(3*k*t0 + M*tR)*k*M*R*W8 -
					3*E^(M*tR)*M^2*R*W8 + 9*E^(k*t0 + M*tR)*M^2*R*W8 -
					9*E^(2*k*t0 + M*tR)*M^2*R*W8 + 3*E^(3*k*t0 + M*tR)*M^2*R*W8 +
					6*k^3*X + 22*k^2*M*X + 18*k*M^2*X + 4*M^3*X)^2 +
				     27*(-(E^(M*tR)*R*W8) + 3*E^(k*t0 + M*tR)*R*W8 - 3*E^(2*k*t0 + M*tR)*
					 R*W8 + E^(3*k*t0 + M*tR)*R*W8 + 6*k*X + 4*M*X)^2*
				      (-6*E^(M*tR)*k^3*R*W8 - 11*E^(M*tR)*k^2*M*R*W8 + 18*
					E^(k*t0 + M*tR)*k^2*M*R*W8 - 9*E^(2*k*t0 + M*tR)*k^2*M*R*W8 + 2*
					E^(3*k*t0 + M*tR)*k^2*M*R*W8 - 6*E^(M*tR)*k*M^2*R*W8 + 15*
					E^(k*t0 + M*tR)*k*M^2*R*W8 - 12*E^(2*k*t0 + M*tR)*k*M^2*R*W8 + 3*
					E^(3*k*t0 + M*tR)*k*M^2*R*W8 - E^(M*tR)*M^3*R*W8 + 3*
					E^(k*t0 + M*tR)*M^3*R*W8 - 3*E^(2*k*t0 + M*tR)*M^3*R*W8 +
				       E^(3*k*t0 + M*tR)*M^3*R*W8 + 6*k^3*M*X + 11*k^2*M^2*X + 6*k*M^3*
					X + M^4*X) - 72*X*(-6*E^(M*tR)*k*R*W8 + 15*E^(k*t0 + M*tR)*k*R*
					W8 - 12*E^(2*k*t0 + M*tR)*k*R*W8 + 3*E^(3*k*t0 + M*tR)*k*R*W8 -
				       3*E^(M*tR)*M*R*W8 + 9*E^(k*t0 + M*tR)*M*R*W8 - 9*
					E^(2*k*t0 + M*tR)*M*R*W8 + 3*E^(3*k*t0 + M*tR)*M*R*W8 + 11*k^2*
					X + 18*k*M*X + 6*M^2*X)*(-6*E^(M*tR)*k^3*R*W8 - 11*E^(M*tR)*k^2*
					M*R*W8 + 18*E^(k*t0 + M*tR)*k^2*M*R*W8 - 9*E^(2*k*t0 + M*tR)*k^2*
					M*R*W8 + 2*E^(3*k*t0 + M*tR)*k^2*M*R*W8 - 6*E^(M*tR)*k*M^2*R*
					W8 + 15*E^(k*t0 + M*tR)*k*M^2*R*W8 - 12*E^(2*k*t0 + M*tR)*k*M^2*
					R*W8 + 3*E^(3*k*t0 + M*tR)*k*M^2*R*W8 - E^(M*tR)*M^3*R*W8 + 3*
					E^(k*t0 + M*tR)*M^3*R*W8 - 3*E^(2*k*t0 + M*tR)*M^3*R*W8 +
				       E^(3*k*t0 + M*tR)*M^3*R*W8 + 6*k^3*M*X + 11*k^2*M^2*X + 6*k*M^3*
					X + M^4*X))^2])^(1/3)/(3*2^(1/3)*X)]/2 +
			   Sqrt[(-(E^(M*tR)*R*W8) + 3*E^(k*t0 + M*tR)*R*W8 - 3*E^(2*k*t0 + M*tR)*R*W8 +
				 E^(3*k*t0 + M*tR)*R*W8 + 6*k*X + 4*M*X)^2/(2*X^2) -
			      (4*(-6*E^(M*tR)*k*R*W8 + 15*E^(k*t0 + M*tR)*k*R*W8 -
				 12*E^(2*k*t0 + M*tR)*k*R*W8 + 3*E^(3*k*t0 + M*tR)*k*R*W8 -
				 3*E^(M*tR)*M*R*W8 + 9*E^(k*t0 + M*tR)*M*R*W8 - 9*E^(2*k*t0 + M*tR)*M*R*
				  W8 + 3*E^(3*k*t0 + M*tR)*M*R*W8 + 11*k^2*X + 18*k*M*X + 6*M^2*X))/
			       (3*X) - (2^(1/3)*k^2*(3*E^(2*M*tR)*R^2*W8^2 - 27*E^(k*t0 + 2*M*tR)*R^2*
				  W8^2 + 81*E^(2*k*t0 + 2*M*tR)*R^2*W8^2 - 114*E^(3*k*t0 + 2*M*tR)*R^2*
				  W8^2 + 81*E^(4*k*t0 + 2*M*tR)*R^2*W8^2 - 27*E^(5*k*t0 + 2*M*tR)*R^2*
				  W8^2 + 3*E^(6*k*t0 + 2*M*tR)*R^2*W8^2 + 12*E^(M*tR)*k*R*W8*X -
				 48*E^(k*t0 + M*tR)*k*R*W8*X - 48*E^(2*k*t0 + M*tR)*k*R*W8*X +
				 12*E^(3*k*t0 + M*tR)*k*R*W8*X + 13*k^2*X^2))/
			       (3*X*(2*(-6*E^(M*tR)*k*R*W8 + 15*E^(k*t0 + M*tR)*k*R*W8 -
				     12*E^(2*k*t0 + M*tR)*k*R*W8 + 3*E^(3*k*t0 + M*tR)*k*R*W8 -
				     3*E^(M*tR)*M*R*W8 + 9*E^(k*t0 + M*tR)*M*R*W8 - 9*E^(2*k*t0 + M*tR)*
				      M*R*W8 + 3*E^(3*k*t0 + M*tR)*M*R*W8 + 11*k^2*X + 18*k*M*X +
				     6*M^2*X)^3 - 9*(-(E^(M*tR)*R*W8) + 3*E^(k*t0 + M*tR)*R*W8 -
				    3*E^(2*k*t0 + M*tR)*R*W8 + E^(3*k*t0 + M*tR)*R*W8 + 6*k*X + 4*M*X)*
				   (-6*E^(M*tR)*k*R*W8 + 15*E^(k*t0 + M*tR)*k*R*W8 -
				    12*E^(2*k*t0 + M*tR)*k*R*W8 + 3*E^(3*k*t0 + M*tR)*k*R*W8 -
				    3*E^(M*tR)*M*R*W8 + 9*E^(k*t0 + M*tR)*M*R*W8 - 9*E^(2*k*t0 + M*tR)*M*
				     R*W8 + 3*E^(3*k*t0 + M*tR)*M*R*W8 + 11*k^2*X + 18*k*M*X + 6*M^2*X)*
				   (-11*E^(M*tR)*k^2*R*W8 + 18*E^(k*t0 + M*tR)*k^2*R*W8 -
				    9*E^(2*k*t0 + M*tR)*k^2*R*W8 + 2*E^(3*k*t0 + M*tR)*k^2*R*W8 -
				    12*E^(M*tR)*k*M*R*W8 + 30*E^(k*t0 + M*tR)*k*M*R*W8 -
				    24*E^(2*k*t0 + M*tR)*k*M*R*W8 + 6*E^(3*k*t0 + M*tR)*k*M*R*W8 -
				    3*E^(M*tR)*M^2*R*W8 + 9*E^(k*t0 + M*tR)*M^2*R*W8 -
				    9*E^(2*k*t0 + M*tR)*M^2*R*W8 + 3*E^(3*k*t0 + M*tR)*M^2*R*W8 +
				    6*k^3*X + 22*k^2*M*X + 18*k*M^2*X + 4*M^3*X) +
				  27*X*(-11*E^(M*tR)*k^2*R*W8 + 18*E^(k*t0 + M*tR)*k^2*R*W8 -
				     9*E^(2*k*t0 + M*tR)*k^2*R*W8 + 2*E^(3*k*t0 + M*tR)*k^2*R*W8 -
				     12*E^(M*tR)*k*M*R*W8 + 30*E^(k*t0 + M*tR)*k*M*R*W8 -
				     24*E^(2*k*t0 + M*tR)*k*M*R*W8 + 6*E^(3*k*t0 + M*tR)*k*M*R*W8 -
				     3*E^(M*tR)*M^2*R*W8 + 9*E^(k*t0 + M*tR)*M^2*R*W8 -
				     9*E^(2*k*t0 + M*tR)*M^2*R*W8 + 3*E^(3*k*t0 + M*tR)*M^2*R*W8 +
				     6*k^3*X + 22*k^2*M*X + 18*k*M^2*X + 4*M^3*X)^2 +
				  27*(-(E^(M*tR)*R*W8) + 3*E^(k*t0 + M*tR)*R*W8 - 3*E^(2*k*t0 + M*tR)*R*
				      W8 + E^(3*k*t0 + M*tR)*R*W8 + 6*k*X + 4*M*X)^2*
				   (-6*E^(M*tR)*k^3*R*W8 - 11*E^(M*tR)*k^2*M*R*W8 + 18*E^(k*t0 + M*tR)*
				     k^2*M*R*W8 - 9*E^(2*k*t0 + M*tR)*k^2*M*R*W8 + 2*E^(3*k*t0 + M*tR)*
				     k^2*M*R*W8 - 6*E^(M*tR)*k*M^2*R*W8 + 15*E^(k*t0 + M*tR)*k*M^2*R*
				     W8 - 12*E^(2*k*t0 + M*tR)*k*M^2*R*W8 + 3*E^(3*k*t0 + M*tR)*k*M^2*R*
				     W8 - E^(M*tR)*M^3*R*W8 + 3*E^(k*t0 + M*tR)*M^3*R*W8 -
				    3*E^(2*k*t0 + M*tR)*M^3*R*W8 + E^(3*k*t0 + M*tR)*M^3*R*W8 +
				    6*k^3*M*X + 11*k^2*M^2*X + 6*k*M^3*X + M^4*X) -
				  72*X*(-6*E^(M*tR)*k*R*W8 + 15*E^(k*t0 + M*tR)*k*R*W8 -
				    12*E^(2*k*t0 + M*tR)*k*R*W8 + 3*E^(3*k*t0 + M*tR)*k*R*W8 -
				    3*E^(M*tR)*M*R*W8 + 9*E^(k*t0 + M*tR)*M*R*W8 - 9*E^(2*k*t0 + M*tR)*M*
				     R*W8 + 3*E^(3*k*t0 + M*tR)*M*R*W8 + 11*k^2*X + 18*k*M*X + 6*M^2*X)*
				   (-6*E^(M*tR)*k^3*R*W8 - 11*E^(M*tR)*k^2*M*R*W8 + 18*E^(k*t0 + M*tR)*
				     k^2*M*R*W8 - 9*E^(2*k*t0 + M*tR)*k^2*M*R*W8 + 2*E^(3*k*t0 + M*tR)*
				     k^2*M*R*W8 - 6*E^(M*tR)*k*M^2*R*W8 + 15*E^(k*t0 + M*tR)*k*M^2*R*
				     W8 - 12*E^(2*k*t0 + M*tR)*k*M^2*R*W8 + 3*E^(3*k*t0 + M*tR)*k*M^2*R*
				     W8 - E^(M*tR)*M^3*R*W8 + 3*E^(k*t0 + M*tR)*M^3*R*W8 -
				    3*E^(2*k*t0 + M*tR)*M^3*R*W8 + E^(3*k*t0 + M*tR)*M^3*R*W8 +
				    6*k^3*M*X + 11*k^2*M^2*X + 6*k*M^3*X + M^4*X) +
				  Sqrt[-4*(3*E^(2*M*tR)*k^2*R^2*W8^2 - 27*E^(k*t0 + 2*M*tR)*k^2*R^2*
					W8^2 + 81*E^(2*k*t0 + 2*M*tR)*k^2*R^2*W8^2 - 114*
					E^(3*k*t0 + 2*M*tR)*k^2*R^2*W8^2 + 81*E^(4*k*t0 + 2*M*tR)*k^2*
					R^2*W8^2 - 27*E^(5*k*t0 + 2*M*tR)*k^2*R^2*W8^2 + 3*
					E^(6*k*t0 + 2*M*tR)*k^2*R^2*W8^2 + 12*E^(M*tR)*k^3*R*W8*X - 48*
					E^(k*t0 + M*tR)*k^3*R*W8*X - 48*E^(2*k*t0 + M*tR)*k^3*R*W8*X +
				       12*E^(3*k*t0 + M*tR)*k^3*R*W8*X + 13*k^4*X^2)^3 +
				    (2*(-6*E^(M*tR)*k*R*W8 + 15*E^(k*t0 + M*tR)*k*R*W8 -
					 12*E^(2*k*t0 + M*tR)*k*R*W8 + 3*E^(3*k*t0 + M*tR)*k*R*W8 -
					 3*E^(M*tR)*M*R*W8 + 9*E^(k*t0 + M*tR)*M*R*W8 -
					 9*E^(2*k*t0 + M*tR)*M*R*W8 + 3*E^(3*k*t0 + M*tR)*M*R*W8 +
					 11*k^2*X + 18*k*M*X + 6*M^2*X)^3 - 9*(-(E^(M*tR)*R*W8) +
					3*E^(k*t0 + M*tR)*R*W8 - 3*E^(2*k*t0 + M*tR)*R*W8 +
					E^(3*k*t0 + M*tR)*R*W8 + 6*k*X + 4*M*X)*(-6*E^(M*tR)*k*R*W8 +
					15*E^(k*t0 + M*tR)*k*R*W8 - 12*E^(2*k*t0 + M*tR)*k*R*W8 +
					3*E^(3*k*t0 + M*tR)*k*R*W8 - 3*E^(M*tR)*M*R*W8 +
					9*E^(k*t0 + M*tR)*M*R*W8 - 9*E^(2*k*t0 + M*tR)*M*R*W8 +
					3*E^(3*k*t0 + M*tR)*M*R*W8 + 11*k^2*X + 18*k*M*X + 6*M^2*X)*(
					-11*E^(M*tR)*k^2*R*W8 + 18*E^(k*t0 + M*tR)*k^2*R*W8 -
					9*E^(2*k*t0 + M*tR)*k^2*R*W8 + 2*E^(3*k*t0 + M*tR)*k^2*R*W8 -
					12*E^(M*tR)*k*M*R*W8 + 30*E^(k*t0 + M*tR)*k*M*R*W8 -
					24*E^(2*k*t0 + M*tR)*k*M*R*W8 + 6*E^(3*k*t0 + M*tR)*k*M*R*W8 -
					3*E^(M*tR)*M^2*R*W8 + 9*E^(k*t0 + M*tR)*M^2*R*W8 -
					9*E^(2*k*t0 + M*tR)*M^2*R*W8 + 3*E^(3*k*t0 + M*tR)*M^2*R*W8 +
					6*k^3*X + 22*k^2*M*X + 18*k*M^2*X + 4*M^3*X) +
				      27*X*(-11*E^(M*tR)*k^2*R*W8 + 18*E^(k*t0 + M*tR)*k^2*R*W8 -
					 9*E^(2*k*t0 + M*tR)*k^2*R*W8 + 2*E^(3*k*t0 + M*tR)*k^2*R*W8 -
					 12*E^(M*tR)*k*M*R*W8 + 30*E^(k*t0 + M*tR)*k*M*R*W8 -
					 24*E^(2*k*t0 + M*tR)*k*M*R*W8 + 6*E^(3*k*t0 + M*tR)*k*M*R*W8 -
					 3*E^(M*tR)*M^2*R*W8 + 9*E^(k*t0 + M*tR)*M^2*R*W8 -
					 9*E^(2*k*t0 + M*tR)*M^2*R*W8 + 3*E^(3*k*t0 + M*tR)*M^2*R*W8 +
					 6*k^3*X + 22*k^2*M*X + 18*k*M^2*X + 4*M^3*X)^2 +
				      27*(-(E^(M*tR)*R*W8) + 3*E^(k*t0 + M*tR)*R*W8 -
					 3*E^(2*k*t0 + M*tR)*R*W8 + E^(3*k*t0 + M*tR)*R*W8 + 6*k*X +
					 4*M*X)^2*(-6*E^(M*tR)*k^3*R*W8 - 11*E^(M*tR)*k^2*M*R*W8 +
					18*E^(k*t0 + M*tR)*k^2*M*R*W8 - 9*E^(2*k*t0 + M*tR)*k^2*M*R*W8 +
					2*E^(3*k*t0 + M*tR)*k^2*M*R*W8 - 6*E^(M*tR)*k*M^2*R*W8 +
					15*E^(k*t0 + M*tR)*k*M^2*R*W8 - 12*E^(2*k*t0 + M*tR)*k*M^2*R*
					 W8 + 3*E^(3*k*t0 + M*tR)*k*M^2*R*W8 - E^(M*tR)*M^3*R*W8 +
					3*E^(k*t0 + M*tR)*M^3*R*W8 - 3*E^(2*k*t0 + M*tR)*M^3*R*W8 +
					E^(3*k*t0 + M*tR)*M^3*R*W8 + 6*k^3*M*X + 11*k^2*M^2*X +
					6*k*M^3*X + M^4*X) - 72*X*(-6*E^(M*tR)*k*R*W8 +
					15*E^(k*t0 + M*tR)*k*R*W8 - 12*E^(2*k*t0 + M*tR)*k*R*W8 +
					3*E^(3*k*t0 + M*tR)*k*R*W8 - 3*E^(M*tR)*M*R*W8 +
					9*E^(k*t0 + M*tR)*M*R*W8 - 9*E^(2*k*t0 + M*tR)*M*R*W8 +
					3*E^(3*k*t0 + M*tR)*M*R*W8 + 11*k^2*X + 18*k*M*X + 6*M^2*X)*(
					-6*E^(M*tR)*k^3*R*W8 - 11*E^(M*tR)*k^2*M*R*W8 +
					18*E^(k*t0 + M*tR)*k^2*M*R*W8 - 9*E^(2*k*t0 + M*tR)*k^2*M*R*W8 +
					2*E^(3*k*t0 + M*tR)*k^2*M*R*W8 - 6*E^(M*tR)*k*M^2*R*W8 +
					15*E^(k*t0 + M*tR)*k*M^2*R*W8 - 12*E^(2*k*t0 + M*tR)*k*M^2*R*
					 W8 + 3*E^(3*k*t0 + M*tR)*k*M^2*R*W8 - E^(M*tR)*M^3*R*W8 +
					3*E^(k*t0 + M*tR)*M^3*R*W8 - 3*E^(2*k*t0 + M*tR)*M^3*R*W8 +
					E^(3*k*t0 + M*tR)*M^3*R*W8 + 6*k^3*M*X + 11*k^2*M^2*X +
					6*k*M^3*X + M^4*X))^2])^(1/3)) -
			      (2*(-6*E^(M*tR)*k*R*W8 + 15*E^(k*t0 + M*tR)*k*R*W8 - 12*E^(2*k*t0 + M*tR)*
				     k*R*W8 + 3*E^(3*k*t0 + M*tR)*k*R*W8 - 3*E^(M*tR)*M*R*W8 +
				    9*E^(k*t0 + M*tR)*M*R*W8 - 9*E^(2*k*t0 + M*tR)*M*R*W8 +
				    3*E^(3*k*t0 + M*tR)*M*R*W8 + 11*k^2*X + 18*k*M*X + 6*M^2*X)^3 -
				 9*(-(E^(M*tR)*R*W8) + 3*E^(k*t0 + M*tR)*R*W8 - 3*E^(2*k*t0 + M*tR)*R*
				    W8 + E^(3*k*t0 + M*tR)*R*W8 + 6*k*X + 4*M*X)*(-6*E^(M*tR)*k*R*W8 +
				   15*E^(k*t0 + M*tR)*k*R*W8 - 12*E^(2*k*t0 + M*tR)*k*R*W8 +
				   3*E^(3*k*t0 + M*tR)*k*R*W8 - 3*E^(M*tR)*M*R*W8 + 9*E^(k*t0 + M*tR)*M*
				    R*W8 - 9*E^(2*k*t0 + M*tR)*M*R*W8 + 3*E^(3*k*t0 + M*tR)*M*R*W8 +
				   11*k^2*X + 18*k*M*X + 6*M^2*X)*(-11*E^(M*tR)*k^2*R*W8 +
				   18*E^(k*t0 + M*tR)*k^2*R*W8 - 9*E^(2*k*t0 + M*tR)*k^2*R*W8 +
				   2*E^(3*k*t0 + M*tR)*k^2*R*W8 - 12*E^(M*tR)*k*M*R*W8 +
				   30*E^(k*t0 + M*tR)*k*M*R*W8 - 24*E^(2*k*t0 + M*tR)*k*M*R*W8 +
				   6*E^(3*k*t0 + M*tR)*k*M*R*W8 - 3*E^(M*tR)*M^2*R*W8 +
				   9*E^(k*t0 + M*tR)*M^2*R*W8 - 9*E^(2*k*t0 + M*tR)*M^2*R*W8 +
				   3*E^(3*k*t0 + M*tR)*M^2*R*W8 + 6*k^3*X + 22*k^2*M*X + 18*k*M^2*X +
				   4*M^3*X) + 27*X*(-11*E^(M*tR)*k^2*R*W8 + 18*E^(k*t0 + M*tR)*k^2*R*
				     W8 - 9*E^(2*k*t0 + M*tR)*k^2*R*W8 + 2*E^(3*k*t0 + M*tR)*k^2*R*W8 -
				    12*E^(M*tR)*k*M*R*W8 + 30*E^(k*t0 + M*tR)*k*M*R*W8 -
				    24*E^(2*k*t0 + M*tR)*k*M*R*W8 + 6*E^(3*k*t0 + M*tR)*k*M*R*W8 -
				    3*E^(M*tR)*M^2*R*W8 + 9*E^(k*t0 + M*tR)*M^2*R*W8 -
				    9*E^(2*k*t0 + M*tR)*M^2*R*W8 + 3*E^(3*k*t0 + M*tR)*M^2*R*W8 +
				    6*k^3*X + 22*k^2*M*X + 18*k*M^2*X + 4*M^3*X)^2 +
				 27*(-(E^(M*tR)*R*W8) + 3*E^(k*t0 + M*tR)*R*W8 - 3*E^(2*k*t0 + M*tR)*R*
				     W8 + E^(3*k*t0 + M*tR)*R*W8 + 6*k*X + 4*M*X)^2*
				  (-6*E^(M*tR)*k^3*R*W8 - 11*E^(M*tR)*k^2*M*R*W8 + 18*E^(k*t0 + M*tR)*
				    k^2*M*R*W8 - 9*E^(2*k*t0 + M*tR)*k^2*M*R*W8 + 2*E^(3*k*t0 + M*tR)*
				    k^2*M*R*W8 - 6*E^(M*tR)*k*M^2*R*W8 + 15*E^(k*t0 + M*tR)*k*M^2*R*W8 -
				   12*E^(2*k*t0 + M*tR)*k*M^2*R*W8 + 3*E^(3*k*t0 + M*tR)*k*M^2*R*W8 -
				   E^(M*tR)*M^3*R*W8 + 3*E^(k*t0 + M*tR)*M^3*R*W8 - 3*E^(2*k*t0 + M*tR)*
				    M^3*R*W8 + E^(3*k*t0 + M*tR)*M^3*R*W8 + 6*k^3*M*X + 11*k^2*M^2*X +
				   6*k*M^3*X + M^4*X) - 72*X*(-6*E^(M*tR)*k*R*W8 + 15*E^(k*t0 + M*tR)*k*
				    R*W8 - 12*E^(2*k*t0 + M*tR)*k*R*W8 + 3*E^(3*k*t0 + M*tR)*k*R*W8 -
				   3*E^(M*tR)*M*R*W8 + 9*E^(k*t0 + M*tR)*M*R*W8 - 9*E^(2*k*t0 + M*tR)*M*
				    R*W8 + 3*E^(3*k*t0 + M*tR)*M*R*W8 + 11*k^2*X + 18*k*M*X + 6*M^2*X)*
				  (-6*E^(M*tR)*k^3*R*W8 - 11*E^(M*tR)*k^2*M*R*W8 + 18*E^(k*t0 + M*tR)*
				    k^2*M*R*W8 - 9*E^(2*k*t0 + M*tR)*k^2*M*R*W8 + 2*E^(3*k*t0 + M*tR)*
				    k^2*M*R*W8 - 6*E^(M*tR)*k*M^2*R*W8 + 15*E^(k*t0 + M*tR)*k*M^2*R*W8 -
				   12*E^(2*k*t0 + M*tR)*k*M^2*R*W8 + 3*E^(3*k*t0 + M*tR)*k*M^2*R*W8 -
				   E^(M*tR)*M^3*R*W8 + 3*E^(k*t0 + M*tR)*M^3*R*W8 - 3*E^(2*k*t0 + M*tR)*
				    M^3*R*W8 + E^(3*k*t0 + M*tR)*M^3*R*W8 + 6*k^3*M*X + 11*k^2*M^2*X +
				   6*k*M^3*X + M^4*X) + Sqrt[-4*(3*E^(2*M*tR)*k^2*R^2*W8^2 -
				      27*E^(k*t0 + 2*M*tR)*k^2*R^2*W8^2 + 81*E^(2*k*t0 + 2*M*tR)*k^2*R^
					2*W8^2 - 114*E^(3*k*t0 + 2*M*tR)*k^2*R^2*W8^2 +
				      81*E^(4*k*t0 + 2*M*tR)*k^2*R^2*W8^2 - 27*E^(5*k*t0 + 2*M*tR)*k^2*R^
					2*W8^2 + 3*E^(6*k*t0 + 2*M*tR)*k^2*R^2*W8^2 + 12*E^(M*tR)*k^
					3*R*W8*X - 48*E^(k*t0 + M*tR)*k^3*R*W8*X - 48*E^(2*k*t0 + M*tR)*
				       k^3*R*W8*X + 12*E^(3*k*t0 + M*tR)*k^3*R*W8*X + 13*k^4*X^2)^3 +
				   (2*(-6*E^(M*tR)*k*R*W8 + 15*E^(k*t0 + M*tR)*k*R*W8 -
					12*E^(2*k*t0 + M*tR)*k*R*W8 + 3*E^(3*k*t0 + M*tR)*k*R*W8 -
					3*E^(M*tR)*M*R*W8 + 9*E^(k*t0 + M*tR)*M*R*W8 -
					9*E^(2*k*t0 + M*tR)*M*R*W8 + 3*E^(3*k*t0 + M*tR)*M*R*W8 +
					11*k^2*X + 18*k*M*X + 6*M^2*X)^3 - 9*(-(E^(M*tR)*R*W8) + 3*
					E^(k*t0 + M*tR)*R*W8 - 3*E^(2*k*t0 + M*tR)*R*W8 +
				       E^(3*k*t0 + M*tR)*R*W8 + 6*k*X + 4*M*X)*(-6*E^(M*tR)*k*R*W8 + 15*
					E^(k*t0 + M*tR)*k*R*W8 - 12*E^(2*k*t0 + M*tR)*k*R*W8 + 3*
					E^(3*k*t0 + M*tR)*k*R*W8 - 3*E^(M*tR)*M*R*W8 + 9*E^(k*t0 + M*tR)*
					M*R*W8 - 9*E^(2*k*t0 + M*tR)*M*R*W8 + 3*E^(3*k*t0 + M*tR)*M*R*
					W8 + 11*k^2*X + 18*k*M*X + 6*M^2*X)*(-11*E^(M*tR)*k^2*R*W8 + 18*
					E^(k*t0 + M*tR)*k^2*R*W8 - 9*E^(2*k*t0 + M*tR)*k^2*R*W8 + 2*
					E^(3*k*t0 + M*tR)*k^2*R*W8 - 12*E^(M*tR)*k*M*R*W8 + 30*
					E^(k*t0 + M*tR)*k*M*R*W8 - 24*E^(2*k*t0 + M*tR)*k*M*R*W8 + 6*
					E^(3*k*t0 + M*tR)*k*M*R*W8 - 3*E^(M*tR)*M^2*R*W8 + 9*
					E^(k*t0 + M*tR)*M^2*R*W8 - 9*E^(2*k*t0 + M*tR)*M^2*R*W8 + 3*
					E^(3*k*t0 + M*tR)*M^2*R*W8 + 6*k^3*X + 22*k^2*M*X + 18*k*M^2*
					X + 4*M^3*X) + 27*X*(-11*E^(M*tR)*k^2*R*W8 + 18*E^(k*t0 + M*tR)*
					 k^2*R*W8 - 9*E^(2*k*t0 + M*tR)*k^2*R*W8 + 2*E^(3*k*t0 + M*tR)*
					 k^2*R*W8 - 12*E^(M*tR)*k*M*R*W8 + 30*E^(k*t0 + M*tR)*k*M*R*W8 -
					24*E^(2*k*t0 + M*tR)*k*M*R*W8 + 6*E^(3*k*t0 + M*tR)*k*M*R*W8 -
					3*E^(M*tR)*M^2*R*W8 + 9*E^(k*t0 + M*tR)*M^2*R*W8 -
					9*E^(2*k*t0 + M*tR)*M^2*R*W8 + 3*E^(3*k*t0 + M*tR)*M^2*R*W8 +
					6*k^3*X + 22*k^2*M*X + 18*k*M^2*X + 4*M^3*X)^2 +
				     27*(-(E^(M*tR)*R*W8) + 3*E^(k*t0 + M*tR)*R*W8 - 3*E^(2*k*t0 + M*tR)*
					 R*W8 + E^(3*k*t0 + M*tR)*R*W8 + 6*k*X + 4*M*X)^2*
				      (-6*E^(M*tR)*k^3*R*W8 - 11*E^(M*tR)*k^2*M*R*W8 + 18*
					E^(k*t0 + M*tR)*k^2*M*R*W8 - 9*E^(2*k*t0 + M*tR)*k^2*M*R*W8 + 2*
					E^(3*k*t0 + M*tR)*k^2*M*R*W8 - 6*E^(M*tR)*k*M^2*R*W8 + 15*
					E^(k*t0 + M*tR)*k*M^2*R*W8 - 12*E^(2*k*t0 + M*tR)*k*M^2*R*W8 + 3*
					E^(3*k*t0 + M*tR)*k*M^2*R*W8 - E^(M*tR)*M^3*R*W8 + 3*
					E^(k*t0 + M*tR)*M^3*R*W8 - 3*E^(2*k*t0 + M*tR)*M^3*R*W8 +
				       E^(3*k*t0 + M*tR)*M^3*R*W8 + 6*k^3*M*X + 11*k^2*M^2*X + 6*k*M^3*
					X + M^4*X) - 72*X*(-6*E^(M*tR)*k*R*W8 + 15*E^(k*t0 + M*tR)*k*R*
					W8 - 12*E^(2*k*t0 + M*tR)*k*R*W8 + 3*E^(3*k*t0 + M*tR)*k*R*W8 -
				       3*E^(M*tR)*M*R*W8 + 9*E^(k*t0 + M*tR)*M*R*W8 - 9*
					E^(2*k*t0 + M*tR)*M*R*W8 + 3*E^(3*k*t0 + M*tR)*M*R*W8 + 11*k^2*
					X + 18*k*M*X + 6*M^2*X)*(-6*E^(M*tR)*k^3*R*W8 - 11*E^(M*tR)*k^2*
					M*R*W8 + 18*E^(k*t0 + M*tR)*k^2*M*R*W8 - 9*E^(2*k*t0 + M*tR)*k^2*
					M*R*W8 + 2*E^(3*k*t0 + M*tR)*k^2*M*R*W8 - 6*E^(M*tR)*k*M^2*R*
					W8 + 15*E^(k*t0 + M*tR)*k*M^2*R*W8 - 12*E^(2*k*t0 + M*tR)*k*M^2*
					R*W8 + 3*E^(3*k*t0 + M*tR)*k*M^2*R*W8 - E^(M*tR)*M^3*R*W8 + 3*
					E^(k*t0 + M*tR)*M^3*R*W8 - 3*E^(2*k*t0 + M*tR)*M^3*R*W8 +
				       E^(3*k*t0 + M*tR)*M^3*R*W8 + 6*k^3*M*X + 11*k^2*M^2*X + 6*k*M^3*
					X + M^4*X))^2])^(1/3)/(3*2^(1/3)*X) +
			      (-((-(E^(M*tR)*R*W8) + 3*E^(k*t0 + M*tR)*R*W8 - 3*E^(2*k*t0 + M*tR)*R*W8 +
				    E^(3*k*t0 + M*tR)*R*W8 + 6*k*X + 4*M*X)^3/X^3) +
				(4*(-(E^(M*tR)*R*W8) + 3*E^(k*t0 + M*tR)*R*W8 - 3*E^(2*k*t0 + M*tR)*R*
				    W8 + E^(3*k*t0 + M*tR)*R*W8 + 6*k*X + 4*M*X)*(-6*E^(M*tR)*k*R*W8 +
				   15*E^(k*t0 + M*tR)*k*R*W8 - 12*E^(2*k*t0 + M*tR)*k*R*W8 +
				   3*E^(3*k*t0 + M*tR)*k*R*W8 - 3*E^(M*tR)*M*R*W8 + 9*E^(k*t0 + M*tR)*M*
				    R*W8 - 9*E^(2*k*t0 + M*tR)*M*R*W8 + 3*E^(3*k*t0 + M*tR)*M*R*W8 +
				   11*k^2*X + 18*k*M*X + 6*M^2*X))/X^2 -
				(8*(-11*E^(M*tR)*k^2*R*W8 + 18*E^(k*t0 + M*tR)*k^2*R*W8 -
				   9*E^(2*k*t0 + M*tR)*k^2*R*W8 + 2*E^(3*k*t0 + M*tR)*k^2*R*W8 -
				   12*E^(M*tR)*k*M*R*W8 + 30*E^(k*t0 + M*tR)*k*M*R*W8 -
				   24*E^(2*k*t0 + M*tR)*k*M*R*W8 + 6*E^(3*k*t0 + M*tR)*k*M*R*W8 -
				   3*E^(M*tR)*M^2*R*W8 + 9*E^(k*t0 + M*tR)*M^2*R*W8 -
				   9*E^(2*k*t0 + M*tR)*M^2*R*W8 + 3*E^(3*k*t0 + M*tR)*M^2*R*W8 +
				   6*k^3*X + 22*k^2*M*X + 18*k*M^2*X + 4*M^3*X))/X)/
			       (4*Sqrt[(-(E^(M*tR)*R*W8) + 3*E^(k*t0 + M*tR)*R*W8 - 3*E^(2*k*t0 + M*tR)*
				      R*W8 + E^(3*k*t0 + M*tR)*R*W8 + 6*k*X + 4*M*X)^2/(4*X^2) -
				  (2*(-6*E^(M*tR)*k*R*W8 + 15*E^(k*t0 + M*tR)*k*R*W8 -
				     12*E^(2*k*t0 + M*tR)*k*R*W8 + 3*E^(3*k*t0 + M*tR)*k*R*W8 -
				     3*E^(M*tR)*M*R*W8 + 9*E^(k*t0 + M*tR)*M*R*W8 - 9*E^(2*k*t0 + M*tR)*
				      M*R*W8 + 3*E^(3*k*t0 + M*tR)*M*R*W8 + 11*k^2*X + 18*k*M*X +
				     6*M^2*X))/(3*X) + (2^(1/3)*k^2*(3*E^(2*M*tR)*R^2*W8^2 -
				     27*E^(k*t0 + 2*M*tR)*R^2*W8^2 + 81*E^(2*k*t0 + 2*M*tR)*R^2*W8^2 -
				     114*E^(3*k*t0 + 2*M*tR)*R^2*W8^2 + 81*E^(4*k*t0 + 2*M*tR)*R^2*
				      W8^2 - 27*E^(5*k*t0 + 2*M*tR)*R^2*W8^2 + 3*E^(6*k*t0 + 2*M*tR)*R^2*
				      W8^2 + 12*E^(M*tR)*k*R*W8*X - 48*E^(k*t0 + M*tR)*k*R*W8*X -
				     48*E^(2*k*t0 + M*tR)*k*R*W8*X + 12*E^(3*k*t0 + M*tR)*k*R*W8*X +
				     13*k^2*X^2))/(3*X*(2*(-6*E^(M*tR)*k*R*W8 + 15*E^(k*t0 + M*tR)*k*R*
					  W8 - 12*E^(2*k*t0 + M*tR)*k*R*W8 + 3*E^(3*k*t0 + M*tR)*k*R*
					  W8 - 3*E^(M*tR)*M*R*W8 + 9*E^(k*t0 + M*tR)*M*R*W8 -
					 9*E^(2*k*t0 + M*tR)*M*R*W8 + 3*E^(3*k*t0 + M*tR)*M*R*W8 +
					 11*k^2*X + 18*k*M*X + 6*M^2*X)^3 - 9*(-(E^(M*tR)*R*W8) +
					3*E^(k*t0 + M*tR)*R*W8 - 3*E^(2*k*t0 + M*tR)*R*W8 +
					E^(3*k*t0 + M*tR)*R*W8 + 6*k*X + 4*M*X)*(-6*E^(M*tR)*k*R*W8 +
					15*E^(k*t0 + M*tR)*k*R*W8 - 12*E^(2*k*t0 + M*tR)*k*R*W8 +
					3*E^(3*k*t0 + M*tR)*k*R*W8 - 3*E^(M*tR)*M*R*W8 +
					9*E^(k*t0 + M*tR)*M*R*W8 - 9*E^(2*k*t0 + M*tR)*M*R*W8 +
					3*E^(3*k*t0 + M*tR)*M*R*W8 + 11*k^2*X + 18*k*M*X + 6*M^2*X)*(
					-11*E^(M*tR)*k^2*R*W8 + 18*E^(k*t0 + M*tR)*k^2*R*W8 -
					9*E^(2*k*t0 + M*tR)*k^2*R*W8 + 2*E^(3*k*t0 + M*tR)*k^2*R*W8 -
					12*E^(M*tR)*k*M*R*W8 + 30*E^(k*t0 + M*tR)*k*M*R*W8 -
					24*E^(2*k*t0 + M*tR)*k*M*R*W8 + 6*E^(3*k*t0 + M*tR)*k*M*R*W8 -
					3*E^(M*tR)*M^2*R*W8 + 9*E^(k*t0 + M*tR)*M^2*R*W8 -
					9*E^(2*k*t0 + M*tR)*M^2*R*W8 + 3*E^(3*k*t0 + M*tR)*M^2*R*W8 +
					6*k^3*X + 22*k^2*M*X + 18*k*M^2*X + 4*M^3*X) +
				      27*X*(-11*E^(M*tR)*k^2*R*W8 + 18*E^(k*t0 + M*tR)*k^2*R*W8 -
					 9*E^(2*k*t0 + M*tR)*k^2*R*W8 + 2*E^(3*k*t0 + M*tR)*k^2*R*W8 -
					 12*E^(M*tR)*k*M*R*W8 + 30*E^(k*t0 + M*tR)*k*M*R*W8 -
					 24*E^(2*k*t0 + M*tR)*k*M*R*W8 + 6*E^(3*k*t0 + M*tR)*k*M*R*W8 -
					 3*E^(M*tR)*M^2*R*W8 + 9*E^(k*t0 + M*tR)*M^2*R*W8 -
					 9*E^(2*k*t0 + M*tR)*M^2*R*W8 + 3*E^(3*k*t0 + M*tR)*M^2*R*W8 +
					 6*k^3*X + 22*k^2*M*X + 18*k*M^2*X + 4*M^3*X)^2 +
				      27*(-(E^(M*tR)*R*W8) + 3*E^(k*t0 + M*tR)*R*W8 -
					 3*E^(2*k*t0 + M*tR)*R*W8 + E^(3*k*t0 + M*tR)*R*W8 + 6*k*X +
					 4*M*X)^2*(-6*E^(M*tR)*k^3*R*W8 - 11*E^(M*tR)*k^2*M*R*W8 +
					18*E^(k*t0 + M*tR)*k^2*M*R*W8 - 9*E^(2*k*t0 + M*tR)*k^2*M*R*W8 +
					2*E^(3*k*t0 + M*tR)*k^2*M*R*W8 - 6*E^(M*tR)*k*M^2*R*W8 +
					15*E^(k*t0 + M*tR)*k*M^2*R*W8 - 12*E^(2*k*t0 + M*tR)*k*M^2*R*
					 W8 + 3*E^(3*k*t0 + M*tR)*k*M^2*R*W8 - E^(M*tR)*M^3*R*W8 +
					3*E^(k*t0 + M*tR)*M^3*R*W8 - 3*E^(2*k*t0 + M*tR)*M^3*R*W8 +
					E^(3*k*t0 + M*tR)*M^3*R*W8 + 6*k^3*M*X + 11*k^2*M^2*X +
					6*k*M^3*X + M^4*X) - 72*X*(-6*E^(M*tR)*k*R*W8 +
					15*E^(k*t0 + M*tR)*k*R*W8 - 12*E^(2*k*t0 + M*tR)*k*R*W8 +
					3*E^(3*k*t0 + M*tR)*k*R*W8 - 3*E^(M*tR)*M*R*W8 +
					9*E^(k*t0 + M*tR)*M*R*W8 - 9*E^(2*k*t0 + M*tR)*M*R*W8 +
					3*E^(3*k*t0 + M*tR)*M*R*W8 + 11*k^2*X + 18*k*M*X + 6*M^2*X)*(
					-6*E^(M*tR)*k^3*R*W8 - 11*E^(M*tR)*k^2*M*R*W8 +
					18*E^(k*t0 + M*tR)*k^2*M*R*W8 - 9*E^(2*k*t0 + M*tR)*k^2*M*R*W8 +
					2*E^(3*k*t0 + M*tR)*k^2*M*R*W8 - 6*E^(M*tR)*k*M^2*R*W8 +
					15*E^(k*t0 + M*tR)*k*M^2*R*W8 - 12*E^(2*k*t0 + M*tR)*k*M^2*R*
					 W8 + 3*E^(3*k*t0 + M*tR)*k*M^2*R*W8 - E^(M*tR)*M^3*R*W8 +
					3*E^(k*t0 + M*tR)*M^3*R*W8 - 3*E^(2*k*t0 + M*tR)*M^3*R*W8 +
					E^(3*k*t0 + M*tR)*M^3*R*W8 + 6*k^3*M*X + 11*k^2*M^2*X +
					6*k*M^3*X + M^4*X) + Sqrt[-4*(3*E^(2*M*tR)*k^2*R^2*W8^2 -
					   27*E^(k*t0 + 2*M*tR)*k^2*R^2*W8^2 + 81*E^(2*k*t0 + 2*M*tR)*
					    k^2*R^2*W8^2 - 114*E^(3*k*t0 + 2*M*tR)*k^2*R^2*W8^2 +
					   81*E^(4*k*t0 + 2*M*tR)*k^2*R^2*W8^2 - 27*E^(5*k*t0 + 2*M*tR)*
					    k^2*R^2*W8^2 + 3*E^(6*k*t0 + 2*M*tR)*k^2*R^2*W8^2 +
					   12*E^(M*tR)*k^3*R*W8*X - 48*E^(k*t0 + M*tR)*k^3*R*W8*X -
					   48*E^(2*k*t0 + M*tR)*k^3*R*W8*X + 12*E^(3*k*t0 + M*tR)*k^3*R*
					    W8*X + 13*k^4*X^2)^3 + (2*(-6*E^(M*tR)*k*R*W8 +
					     15*E^(k*t0 + M*tR)*k*R*W8 - 12*E^(2*k*t0 + M*tR)*k*R*W8 +
					     3*E^(3*k*t0 + M*tR)*k*R*W8 - 3*E^(M*tR)*M*R*W8 +
					     9*E^(k*t0 + M*tR)*M*R*W8 - 9*E^(2*k*t0 + M*tR)*M*R*W8 +
					     3*E^(3*k*t0 + M*tR)*M*R*W8 + 11*k^2*X + 18*k*M*X + 6*M^2*X)^
					    3 - 9*(-(E^(M*tR)*R*W8) + 3*E^(k*t0 + M*tR)*R*W8 -
					    3*E^(2*k*t0 + M*tR)*R*W8 + E^(3*k*t0 + M*tR)*R*W8 + 6*k*X +
					    4*M*X)*(-6*E^(M*tR)*k*R*W8 + 15*E^(k*t0 + M*tR)*k*R*W8 -
					    12*E^(2*k*t0 + M*tR)*k*R*W8 + 3*E^(3*k*t0 + M*tR)*k*R*W8 -
					    3*E^(M*tR)*M*R*W8 + 9*E^(k*t0 + M*tR)*M*R*W8 -
					    9*E^(2*k*t0 + M*tR)*M*R*W8 + 3*E^(3*k*t0 + M*tR)*M*R*W8 +
					    11*k^2*X + 18*k*M*X + 6*M^2*X)*(-11*E^(M*tR)*k^2*R*W8 +
					    18*E^(k*t0 + M*tR)*k^2*R*W8 - 9*E^(2*k*t0 + M*tR)*k^2*R*W8 +
					    2*E^(3*k*t0 + M*tR)*k^2*R*W8 - 12*E^(M*tR)*k*M*R*W8 +
					    30*E^(k*t0 + M*tR)*k*M*R*W8 - 24*E^(2*k*t0 + M*tR)*k*M*R*
					     W8 + 6*E^(3*k*t0 + M*tR)*k*M*R*W8 - 3*E^(M*tR)*M^2*R*W8 +
					    9*E^(k*t0 + M*tR)*M^2*R*W8 - 9*E^(2*k*t0 + M*tR)*M^2*R*W8 +
					    3*E^(3*k*t0 + M*tR)*M^2*R*W8 + 6*k^3*X + 22*k^2*M*X +
					    18*k*M^2*X + 4*M^3*X) + 27*X*(-11*E^(M*tR)*k^2*R*W8 +
					     18*E^(k*t0 + M*tR)*k^2*R*W8 - 9*E^(2*k*t0 + M*tR)*k^2*R*
					      W8 + 2*E^(3*k*t0 + M*tR)*k^2*R*W8 - 12*E^(M*tR)*k*M*R*W8 +
					     30*E^(k*t0 + M*tR)*k*M*R*W8 - 24*E^(2*k*t0 + M*tR)*k*M*R*
					      W8 + 6*E^(3*k*t0 + M*tR)*k*M*R*W8 - 3*E^(M*tR)*M^2*R*W8 +
					     9*E^(k*t0 + M*tR)*M^2*R*W8 - 9*E^(2*k*t0 + M*tR)*M^2*R*W8 +
					     3*E^(3*k*t0 + M*tR)*M^2*R*W8 + 6*k^3*X + 22*k^2*M*X +
					     18*k*M^2*X + 4*M^3*X)^2 + 27*(-(E^(M*tR)*R*W8) +
					     3*E^(k*t0 + M*tR)*R*W8 - 3*E^(2*k*t0 + M*tR)*R*W8 +
					     E^(3*k*t0 + M*tR)*R*W8 + 6*k*X + 4*M*X)^2*(-6*E^(M*tR)*k^3*
					     R*W8 - 11*E^(M*tR)*k^2*M*R*W8 + 18*E^(k*t0 + M*tR)*k^2*M*R*
					     W8 - 9*E^(2*k*t0 + M*tR)*k^2*M*R*W8 + 2*E^(3*k*t0 + M*tR)*
					     k^2*M*R*W8 - 6*E^(M*tR)*k*M^2*R*W8 + 15*E^(k*t0 + M*tR)*k*
					     M^2*R*W8 - 12*E^(2*k*t0 + M*tR)*k*M^2*R*W8 +
					    3*E^(3*k*t0 + M*tR)*k*M^2*R*W8 - E^(M*tR)*M^3*R*W8 +
					    3*E^(k*t0 + M*tR)*M^3*R*W8 - 3*E^(2*k*t0 + M*tR)*M^3*R*W8 +
					    E^(3*k*t0 + M*tR)*M^3*R*W8 + 6*k^3*M*X + 11*k^2*M^2*X +
					    6*k*M^3*X + M^4*X) - 72*X*(-6*E^(M*tR)*k*R*W8 +
					    15*E^(k*t0 + M*tR)*k*R*W8 - 12*E^(2*k*t0 + M*tR)*k*R*W8 +
					    3*E^(3*k*t0 + M*tR)*k*R*W8 - 3*E^(M*tR)*M*R*W8 +
					    9*E^(k*t0 + M*tR)*M*R*W8 - 9*E^(2*k*t0 + M*tR)*M*R*W8 +
					    3*E^(3*k*t0 + M*tR)*M*R*W8 + 11*k^2*X + 18*k*M*X + 6*M^2*X)*
					   (-6*E^(M*tR)*k^3*R*W8 - 11*E^(M*tR)*k^2*M*R*W8 +
					    18*E^(k*t0 + M*tR)*k^2*M*R*W8 - 9*E^(2*k*t0 + M*tR)*k^2*M*R*
					     W8 + 2*E^(3*k*t0 + M*tR)*k^2*M*R*W8 - 6*E^(M*tR)*k*M^2*R*
					     W8 + 15*E^(k*t0 + M*tR)*k*M^2*R*W8 - 12*E^(2*k*t0 + M*tR)*k*
					     M^2*R*W8 + 3*E^(3*k*t0 + M*tR)*k*M^2*R*W8 - E^(M*tR)*M^3*R*
					     W8 + 3*E^(k*t0 + M*tR)*M^3*R*W8 - 3*E^(2*k*t0 + M*tR)*M^3*R*
					     W8 + E^(3*k*t0 + M*tR)*M^3*R*W8 + 6*k^3*M*X + 11*k^2*M^2*
					     X + 6*k*M^3*X + M^4*X))^2])^(1/3)) +
				  (2*(-6*E^(M*tR)*k*R*W8 + 15*E^(k*t0 + M*tR)*k*R*W8 -
					12*E^(2*k*t0 + M*tR)*k*R*W8 + 3*E^(3*k*t0 + M*tR)*k*R*W8 -
					3*E^(M*tR)*M*R*W8 + 9*E^(k*t0 + M*tR)*M*R*W8 -
					9*E^(2*k*t0 + M*tR)*M*R*W8 + 3*E^(3*k*t0 + M*tR)*M*R*W8 +
					11*k^2*X + 18*k*M*X + 6*M^2*X)^3 - 9*(-(E^(M*tR)*R*W8) + 3*
					E^(k*t0 + M*tR)*R*W8 - 3*E^(2*k*t0 + M*tR)*R*W8 +
				       E^(3*k*t0 + M*tR)*R*W8 + 6*k*X + 4*M*X)*(-6*E^(M*tR)*k*R*W8 + 15*
					E^(k*t0 + M*tR)*k*R*W8 - 12*E^(2*k*t0 + M*tR)*k*R*W8 + 3*
					E^(3*k*t0 + M*tR)*k*R*W8 - 3*E^(M*tR)*M*R*W8 + 9*E^(k*t0 + M*tR)*
					M*R*W8 - 9*E^(2*k*t0 + M*tR)*M*R*W8 + 3*E^(3*k*t0 + M*tR)*M*R*
					W8 + 11*k^2*X + 18*k*M*X + 6*M^2*X)*(-11*E^(M*tR)*k^2*R*W8 + 18*
					E^(k*t0 + M*tR)*k^2*R*W8 - 9*E^(2*k*t0 + M*tR)*k^2*R*W8 + 2*
					E^(3*k*t0 + M*tR)*k^2*R*W8 - 12*E^(M*tR)*k*M*R*W8 + 30*
					E^(k*t0 + M*tR)*k*M*R*W8 - 24*E^(2*k*t0 + M*tR)*k*M*R*W8 + 6*
					E^(3*k*t0 + M*tR)*k*M*R*W8 - 3*E^(M*tR)*M^2*R*W8 + 9*
					E^(k*t0 + M*tR)*M^2*R*W8 - 9*E^(2*k*t0 + M*tR)*M^2*R*W8 + 3*
					E^(3*k*t0 + M*tR)*M^2*R*W8 + 6*k^3*X + 22*k^2*M*X + 18*k*M^2*
					X + 4*M^3*X) + 27*X*(-11*E^(M*tR)*k^2*R*W8 + 18*E^(k*t0 + M*tR)*
					 k^2*R*W8 - 9*E^(2*k*t0 + M*tR)*k^2*R*W8 + 2*E^(3*k*t0 + M*tR)*
					 k^2*R*W8 - 12*E^(M*tR)*k*M*R*W8 + 30*E^(k*t0 + M*tR)*k*M*R*W8 -
					24*E^(2*k*t0 + M*tR)*k*M*R*W8 + 6*E^(3*k*t0 + M*tR)*k*M*R*W8 -
					3*E^(M*tR)*M^2*R*W8 + 9*E^(k*t0 + M*tR)*M^2*R*W8 -
					9*E^(2*k*t0 + M*tR)*M^2*R*W8 + 3*E^(3*k*t0 + M*tR)*M^2*R*W8 +
					6*k^3*X + 22*k^2*M*X + 18*k*M^2*X + 4*M^3*X)^2 +
				     27*(-(E^(M*tR)*R*W8) + 3*E^(k*t0 + M*tR)*R*W8 - 3*E^(2*k*t0 + M*tR)*
					 R*W8 + E^(3*k*t0 + M*tR)*R*W8 + 6*k*X + 4*M*X)^2*
				      (-6*E^(M*tR)*k^3*R*W8 - 11*E^(M*tR)*k^2*M*R*W8 + 18*
					E^(k*t0 + M*tR)*k^2*M*R*W8 - 9*E^(2*k*t0 + M*tR)*k^2*M*R*W8 + 2*
					E^(3*k*t0 + M*tR)*k^2*M*R*W8 - 6*E^(M*tR)*k*M^2*R*W8 + 15*
					E^(k*t0 + M*tR)*k*M^2*R*W8 - 12*E^(2*k*t0 + M*tR)*k*M^2*R*W8 + 3*
					E^(3*k*t0 + M*tR)*k*M^2*R*W8 - E^(M*tR)*M^3*R*W8 + 3*
					E^(k*t0 + M*tR)*M^3*R*W8 - 3*E^(2*k*t0 + M*tR)*M^3*R*W8 +
				       E^(3*k*t0 + M*tR)*M^3*R*W8 + 6*k^3*M*X + 11*k^2*M^2*X + 6*k*M^3*
					X + M^4*X) - 72*X*(-6*E^(M*tR)*k*R*W8 + 15*E^(k*t0 + M*tR)*k*R*
					W8 - 12*E^(2*k*t0 + M*tR)*k*R*W8 + 3*E^(3*k*t0 + M*tR)*k*R*W8 -
				       3*E^(M*tR)*M*R*W8 + 9*E^(k*t0 + M*tR)*M*R*W8 - 9*
					E^(2*k*t0 + M*tR)*M*R*W8 + 3*E^(3*k*t0 + M*tR)*M*R*W8 + 11*k^2*
					X + 18*k*M*X + 6*M^2*X)*(-6*E^(M*tR)*k^3*R*W8 - 11*E^(M*tR)*k^2*
					M*R*W8 + 18*E^(k*t0 + M*tR)*k^2*M*R*W8 - 9*E^(2*k*t0 + M*tR)*k^2*
					M*R*W8 + 2*E^(3*k*t0 + M*tR)*k^2*M*R*W8 - 6*E^(M*tR)*k*M^2*R*
					W8 + 15*E^(k*t0 + M*tR)*k*M^2*R*W8 - 12*E^(2*k*t0 + M*tR)*k*M^2*
					R*W8 + 3*E^(3*k*t0 + M*tR)*k*M^2*R*W8 - E^(M*tR)*M^3*R*W8 + 3*
					E^(k*t0 + M*tR)*M^3*R*W8 - 3*E^(2*k*t0 + M*tR)*M^3*R*W8 +
				       E^(3*k*t0 + M*tR)*M^3*R*W8 + 6*k^3*M*X + 11*k^2*M^2*X + 6*k*M^3*
					X + M^4*X) + Sqrt[-4*(3*E^(2*M*tR)*k^2*R^2*W8^2 -
					  27*E^(k*t0 + 2*M*tR)*k^2*R^2*W8^2 + 81*E^(2*k*t0 + 2*M*tR)*k^2*
					   R^2*W8^2 - 114*E^(3*k*t0 + 2*M*tR)*k^2*R^2*W8^2 +
					  81*E^(4*k*t0 + 2*M*tR)*k^2*R^2*W8^2 - 27*E^(5*k*t0 + 2*M*tR)*
					   k^2*R^2*W8^2 + 3*E^(6*k*t0 + 2*M*tR)*k^2*R^2*W8^2 +
					  12*E^(M*tR)*k^3*R*W8*X - 48*E^(k*t0 + M*tR)*k^3*R*W8*X -
					  48*E^(2*k*t0 + M*tR)*k^3*R*W8*X + 12*E^(3*k*t0 + M*tR)*k^3*R*
					   W8*X + 13*k^4*X^2)^3 + (2*(-6*E^(M*tR)*k*R*W8 +
					    15*E^(k*t0 + M*tR)*k*R*W8 - 12*E^(2*k*t0 + M*tR)*k*R*W8 +
					    3*E^(3*k*t0 + M*tR)*k*R*W8 - 3*E^(M*tR)*M*R*W8 +
					    9*E^(k*t0 + M*tR)*M*R*W8 - 9*E^(2*k*t0 + M*tR)*M*R*W8 +
					    3*E^(3*k*t0 + M*tR)*M*R*W8 + 11*k^2*X + 18*k*M*X + 6*M^2*X)^
					   3 - 9*(-(E^(M*tR)*R*W8) + 3*E^(k*t0 + M*tR)*R*W8 -
					   3*E^(2*k*t0 + M*tR)*R*W8 + E^(3*k*t0 + M*tR)*R*W8 + 6*k*X +
					   4*M*X)*(-6*E^(M*tR)*k*R*W8 + 15*E^(k*t0 + M*tR)*k*R*W8 -
					   12*E^(2*k*t0 + M*tR)*k*R*W8 + 3*E^(3*k*t0 + M*tR)*k*R*W8 -
					   3*E^(M*tR)*M*R*W8 + 9*E^(k*t0 + M*tR)*M*R*W8 -
					   9*E^(2*k*t0 + M*tR)*M*R*W8 + 3*E^(3*k*t0 + M*tR)*M*R*W8 +
					   11*k^2*X + 18*k*M*X + 6*M^2*X)*(-11*E^(M*tR)*k^2*R*W8 +
					   18*E^(k*t0 + M*tR)*k^2*R*W8 - 9*E^(2*k*t0 + M*tR)*k^2*R*W8 +
					   2*E^(3*k*t0 + M*tR)*k^2*R*W8 - 12*E^(M*tR)*k*M*R*W8 +
					   30*E^(k*t0 + M*tR)*k*M*R*W8 - 24*E^(2*k*t0 + M*tR)*k*M*R*W8 +
					   6*E^(3*k*t0 + M*tR)*k*M*R*W8 - 3*E^(M*tR)*M^2*R*W8 +
					   9*E^(k*t0 + M*tR)*M^2*R*W8 - 9*E^(2*k*t0 + M*tR)*M^2*R*W8 +
					   3*E^(3*k*t0 + M*tR)*M^2*R*W8 + 6*k^3*X + 22*k^2*M*X +
					   18*k*M^2*X + 4*M^3*X) + 27*X*(-11*E^(M*tR)*k^2*R*W8 +
					    18*E^(k*t0 + M*tR)*k^2*R*W8 - 9*E^(2*k*t0 + M*tR)*k^2*R*W8 +
					    2*E^(3*k*t0 + M*tR)*k^2*R*W8 - 12*E^(M*tR)*k*M*R*W8 +
					    30*E^(k*t0 + M*tR)*k*M*R*W8 - 24*E^(2*k*t0 + M*tR)*k*M*R*
					     W8 + 6*E^(3*k*t0 + M*tR)*k*M*R*W8 - 3*E^(M*tR)*M^2*R*W8 +
					    9*E^(k*t0 + M*tR)*M^2*R*W8 - 9*E^(2*k*t0 + M*tR)*M^2*R*W8 +
					    3*E^(3*k*t0 + M*tR)*M^2*R*W8 + 6*k^3*X + 22*k^2*M*X +
					    18*k*M^2*X + 4*M^3*X)^2 + 27*(-(E^(M*tR)*R*W8) +
					    3*E^(k*t0 + M*tR)*R*W8 - 3*E^(2*k*t0 + M*tR)*R*W8 +
					    E^(3*k*t0 + M*tR)*R*W8 + 6*k*X + 4*M*X)^2*(-6*E^(M*tR)*k^3*R*
					    W8 - 11*E^(M*tR)*k^2*M*R*W8 + 18*E^(k*t0 + M*tR)*k^2*M*R*
					    W8 - 9*E^(2*k*t0 + M*tR)*k^2*M*R*W8 + 2*E^(3*k*t0 + M*tR)*
					    k^2*M*R*W8 - 6*E^(M*tR)*k*M^2*R*W8 + 15*E^(k*t0 + M*tR)*k*
					    M^2*R*W8 - 12*E^(2*k*t0 + M*tR)*k*M^2*R*W8 +
					   3*E^(3*k*t0 + M*tR)*k*M^2*R*W8 - E^(M*tR)*M^3*R*W8 +
					   3*E^(k*t0 + M*tR)*M^3*R*W8 - 3*E^(2*k*t0 + M*tR)*M^3*R*W8 +
					   E^(3*k*t0 + M*tR)*M^3*R*W8 + 6*k^3*M*X + 11*k^2*M^2*X +
					   6*k*M^3*X + M^4*X) - 72*X*(-6*E^(M*tR)*k*R*W8 +
					   15*E^(k*t0 + M*tR)*k*R*W8 - 12*E^(2*k*t0 + M*tR)*k*R*W8 +
					   3*E^(3*k*t0 + M*tR)*k*R*W8 - 3*E^(M*tR)*M*R*W8 +
					   9*E^(k*t0 + M*tR)*M*R*W8 - 9*E^(2*k*t0 + M*tR)*M*R*W8 +
					   3*E^(3*k*t0 + M*tR)*M*R*W8 + 11*k^2*X + 18*k*M*X + 6*M^2*X)*
					  (-6*E^(M*tR)*k^3*R*W8 - 11*E^(M*tR)*k^2*M*R*W8 +
					   18*E^(k*t0 + M*tR)*k^2*M*R*W8 - 9*E^(2*k*t0 + M*tR)*k^2*M*R*
					    W8 + 2*E^(3*k*t0 + M*tR)*k^2*M*R*W8 - 6*E^(M*tR)*k*M^2*R*
					    W8 + 15*E^(k*t0 + M*tR)*k*M^2*R*W8 - 12*E^(2*k*t0 + M*tR)*k*
					    M^2*R*W8 + 3*E^(3*k*t0 + M*tR)*k*M^2*R*W8 - E^(M*tR)*M^3*R*
					    W8 + 3*E^(k*t0 + M*tR)*M^3*R*W8 - 3*E^(2*k*t0 + M*tR)*M^3*R*
					    W8 + E^(3*k*t0 + M*tR)*M^3*R*W8 + 6*k^3*M*X + 11*k^2*M^2*X +
				   6*k*M^3*X + M^4*X))^2])^(1/3)/(3*2^(1/3)*X)])]/2,









	If[t0===0 && b===3,



	-(E^(-3*k*tc - M*(tc - tR))*(R*W8 - 3*E^(k*tc)*R*W8 + 3*E^(2*k*tc)*R*W8 -
	       E^(3*k*tc)*R*W8 + 6*E^(3*k*tc + M*(tc - tR))*k*X +
	       4*E^(3*k*tc + M*(tc - tR))*M*X))/(4*X) +
	   Sqrt[(E^(-6*k*tc - 2*M*(tc - tR))*(R*W8 - 3*E^(k*tc)*R*W8 +
	          3*E^(2*k*tc)*R*W8 - E^(3*k*tc)*R*W8 + 6*E^(3*k*tc + M*(tc - tR))*k*X +
	          4*E^(3*k*tc + M*(tc - tR))*M*X)^2)/(4*X^2) -
	      (2*E^(-3*k*tc - M*(tc - tR))*(3*k*R*W8 - 12*E^(k*tc)*k*R*W8 +
	         15*E^(2*k*tc)*k*R*W8 - 6*E^(3*k*tc)*k*R*W8 + 3*M*R*W8 -
	         9*E^(k*tc)*M*R*W8 + 9*E^(2*k*tc)*M*R*W8 - 3*E^(3*k*tc)*M*R*W8 +
	         11*E^(3*k*tc + M*(tc - tR))*k^2*X + 18*E^(3*k*tc + M*(tc - tR))*k*M*X +
	         6*E^(3*k*tc + M*(tc - tR))*M^2*X))/(3*X) +
	      (2^(1/3)*E^(-6*k*tc - (-9*k*tc - 3*M*(tc - tR))/3 - 2*M*(tc - tR))*k^2*
	        (3*R^2*W8^2 - 27*E^(k*tc)*R^2*W8^2 + 81*E^(2*k*tc)*R^2*W8^2 -
	         114*E^(3*k*tc)*R^2*W8^2 + 81*E^(4*k*tc)*R^2*W8^2 -
	         27*E^(5*k*tc)*R^2*W8^2 + 3*E^(6*k*tc)*R^2*W8^2 +
	         12*E^(3*k*tc + M*(tc - tR))*k*R*W8*X - 48*E^(4*k*tc + M*(tc - tR))*k*R*
	          W8*X - 48*E^(5*k*tc + M*(tc - tR))*k*R*W8*X +
	         12*E^(6*k*tc + M*(tc - tR))*k*R*W8*X + 13*E^(6*k*tc + 2*M*(tc - tR))*
	          k^2*X^2))/(3*X*(2*(3*k*R*W8 - 12*E^(k*tc)*k*R*W8 + 15*E^(2*k*tc)*k*R*
	              W8 - 6*E^(3*k*tc)*k*R*W8 + 3*M*R*W8 - 9*E^(k*tc)*M*R*W8 +
	             9*E^(2*k*tc)*M*R*W8 - 3*E^(3*k*tc)*M*R*W8 +
	             11*E^(3*k*tc + M*(tc - tR))*k^2*X + 18*E^(3*k*tc + M*(tc - tR))*k*M*
	              X + 6*E^(3*k*tc + M*(tc - tR))*M^2*X)^3 -
	          9*(R*W8 - 3*E^(k*tc)*R*W8 + 3*E^(2*k*tc)*R*W8 - E^(3*k*tc)*R*W8 +
	            6*E^(3*k*tc + M*(tc - tR))*k*X + 4*E^(3*k*tc + M*(tc - tR))*M*X)*
	           (3*k*R*W8 - 12*E^(k*tc)*k*R*W8 + 15*E^(2*k*tc)*k*R*W8 -
	            6*E^(3*k*tc)*k*R*W8 + 3*M*R*W8 - 9*E^(k*tc)*M*R*W8 +
	            9*E^(2*k*tc)*M*R*W8 - 3*E^(3*k*tc)*M*R*W8 +
	            11*E^(3*k*tc + M*(tc - tR))*k^2*X + 18*E^(3*k*tc + M*(tc - tR))*k*M*
	             X + 6*E^(3*k*tc + M*(tc - tR))*M^2*X)*(2*k^2*R*W8 -
	            9*E^(k*tc)*k^2*R*W8 + 18*E^(2*k*tc)*k^2*R*W8 - 11*E^(3*k*tc)*k^2*R*
	             W8 + 6*k*M*R*W8 - 24*E^(k*tc)*k*M*R*W8 + 30*E^(2*k*tc)*k*M*R*W8 -
	            12*E^(3*k*tc)*k*M*R*W8 + 3*M^2*R*W8 - 9*E^(k*tc)*M^2*R*W8 +
	            9*E^(2*k*tc)*M^2*R*W8 - 3*E^(3*k*tc)*M^2*R*W8 +
	            6*E^(3*k*tc + M*(tc - tR))*k^3*X + 22*E^(3*k*tc + M*(tc - tR))*k^2*M*
	             X + 18*E^(3*k*tc + M*(tc - tR))*k*M^2*X +
	            4*E^(3*k*tc + M*(tc - tR))*M^3*X) + 27*E^(3*k*tc + M*(tc - tR))*X*
	           (2*k^2*R*W8 - 9*E^(k*tc)*k^2*R*W8 + 18*E^(2*k*tc)*k^2*R*W8 -
	             11*E^(3*k*tc)*k^2*R*W8 + 6*k*M*R*W8 - 24*E^(k*tc)*k*M*R*W8 +
	             30*E^(2*k*tc)*k*M*R*W8 - 12*E^(3*k*tc)*k*M*R*W8 + 3*M^2*R*W8 -
	             9*E^(k*tc)*M^2*R*W8 + 9*E^(2*k*tc)*M^2*R*W8 - 3*E^(3*k*tc)*M^2*R*
	              W8 + 6*E^(3*k*tc + M*(tc - tR))*k^3*X +
	             22*E^(3*k*tc + M*(tc - tR))*k^2*M*X + 18*E^(3*k*tc + M*(tc - tR))*k*
	              M^2*X + 4*E^(3*k*tc + M*(tc - tR))*M^3*X)^2 +
	          27*(R*W8 - 3*E^(k*tc)*R*W8 + 3*E^(2*k*tc)*R*W8 - E^(3*k*tc)*R*W8 +
	             6*E^(3*k*tc + M*(tc - tR))*k*X + 4*E^(3*k*tc + M*(tc - tR))*M*X)^2*
	           (-6*E^(3*k*tc)*k^3*R*W8 + 2*k^2*M*R*W8 - 9*E^(k*tc)*k^2*M*R*W8 +
	            18*E^(2*k*tc)*k^2*M*R*W8 - 11*E^(3*k*tc)*k^2*M*R*W8 + 3*k*M^2*R*W8 -
	            12*E^(k*tc)*k*M^2*R*W8 + 15*E^(2*k*tc)*k*M^2*R*W8 -
	            6*E^(3*k*tc)*k*M^2*R*W8 + M^3*R*W8 - 3*E^(k*tc)*M^3*R*W8 +
	            3*E^(2*k*tc)*M^3*R*W8 - E^(3*k*tc)*M^3*R*W8 +
	            6*E^(3*k*tc + M*(tc - tR))*k^3*M*X + 11*E^(3*k*tc + M*(tc - tR))*k^2*
	             M^2*X + 6*E^(3*k*tc + M*(tc - tR))*k*M^3*X +
	            E^(3*k*tc + M*(tc - tR))*M^4*X) - 72*E^(3*k*tc + M*(tc - tR))*X*
	           (3*k*R*W8 - 12*E^(k*tc)*k*R*W8 + 15*E^(2*k*tc)*k*R*W8 -
	            6*E^(3*k*tc)*k*R*W8 + 3*M*R*W8 - 9*E^(k*tc)*M*R*W8 +
	            9*E^(2*k*tc)*M*R*W8 - 3*E^(3*k*tc)*M*R*W8 +
	            11*E^(3*k*tc + M*(tc - tR))*k^2*X + 18*E^(3*k*tc + M*(tc - tR))*k*M*
	             X + 6*E^(3*k*tc + M*(tc - tR))*M^2*X)*(-6*E^(3*k*tc)*k^3*R*W8 +
	            2*k^2*M*R*W8 - 9*E^(k*tc)*k^2*M*R*W8 + 18*E^(2*k*tc)*k^2*M*R*W8 -
	            11*E^(3*k*tc)*k^2*M*R*W8 + 3*k*M^2*R*W8 - 12*E^(k*tc)*k*M^2*R*W8 +
	            15*E^(2*k*tc)*k*M^2*R*W8 - 6*E^(3*k*tc)*k*M^2*R*W8 + M^3*R*W8 -
	            3*E^(k*tc)*M^3*R*W8 + 3*E^(2*k*tc)*M^3*R*W8 - E^(3*k*tc)*M^3*R*W8 +
	            6*E^(3*k*tc + M*(tc - tR))*k^3*M*X + 11*E^(3*k*tc + M*(tc - tR))*k^2*
	             M^2*X + 6*E^(3*k*tc + M*(tc - tR))*k*M^3*X +
	            E^(3*k*tc + M*(tc - tR))*M^4*X) +
	          Sqrt[-4*(3*k^2*R^2*W8^2 - 27*E^(k*tc)*k^2*R^2*W8^2 + 81*E^(2*k*tc)*k^2*
	                R^2*W8^2 - 114*E^(3*k*tc)*k^2*R^2*W8^2 + 81*E^(4*k*tc)*k^2*R^2*
	                W8^2 - 27*E^(5*k*tc)*k^2*R^2*W8^2 + 3*E^(6*k*tc)*k^2*R^2*W8^2 +
	               12*E^(3*k*tc + M*(tc - tR))*k^3*R*W8*X - 48*E^(4*k*tc +
	                  M*(tc - tR))*k^3*R*W8*X - 48*E^(5*k*tc + M*(tc - tR))*k^3*R*W8*
	                X + 12*E^(6*k*tc + M*(tc - tR))*k^3*R*W8*X + 13*
	                E^(6*k*tc + 2*M*(tc - tR))*k^4*X^2)^3 +
	            (2*(3*k*R*W8 - 12*E^(k*tc)*k*R*W8 + 15*E^(2*k*tc)*k*R*W8 -
	                 6*E^(3*k*tc)*k*R*W8 + 3*M*R*W8 - 9*E^(k*tc)*M*R*W8 +
	                 9*E^(2*k*tc)*M*R*W8 - 3*E^(3*k*tc)*M*R*W8 +
	                 11*E^(3*k*tc + M*(tc - tR))*k^2*X + 18*E^(3*k*tc + M*(tc - tR))*
	                  k*M*X + 6*E^(3*k*tc + M*(tc - tR))*M^2*X)^3 -
	              9*(R*W8 - 3*E^(k*tc)*R*W8 + 3*E^(2*k*tc)*R*W8 - E^(3*k*tc)*R*W8 +
	                6*E^(3*k*tc + M*(tc - tR))*k*X + 4*E^(3*k*tc + M*(tc - tR))*M*X)*
	               (3*k*R*W8 - 12*E^(k*tc)*k*R*W8 + 15*E^(2*k*tc)*k*R*W8 -
	                6*E^(3*k*tc)*k*R*W8 + 3*M*R*W8 - 9*E^(k*tc)*M*R*W8 +
	                9*E^(2*k*tc)*M*R*W8 - 3*E^(3*k*tc)*M*R*W8 +
	                11*E^(3*k*tc + M*(tc - tR))*k^2*X + 18*E^(3*k*tc + M*(tc - tR))*
	                 k*M*X + 6*E^(3*k*tc + M*(tc - tR))*M^2*X)*(2*k^2*R*W8 -
	                9*E^(k*tc)*k^2*R*W8 + 18*E^(2*k*tc)*k^2*R*W8 - 11*E^(3*k*tc)*k^2*
	                 R*W8 + 6*k*M*R*W8 - 24*E^(k*tc)*k*M*R*W8 + 30*E^(2*k*tc)*k*M*R*
	                 W8 - 12*E^(3*k*tc)*k*M*R*W8 + 3*M^2*R*W8 - 9*E^(k*tc)*M^2*R*
	                 W8 + 9*E^(2*k*tc)*M^2*R*W8 - 3*E^(3*k*tc)*M^2*R*W8 +
	                6*E^(3*k*tc + M*(tc - tR))*k^3*X + 22*E^(3*k*tc + M*(tc - tR))*
	                 k^2*M*X + 18*E^(3*k*tc + M*(tc - tR))*k*M^2*X +
	                4*E^(3*k*tc + M*(tc - tR))*M^3*X) + 27*E^(3*k*tc + M*(tc - tR))*
	               X*(2*k^2*R*W8 - 9*E^(k*tc)*k^2*R*W8 + 18*E^(2*k*tc)*k^2*R*W8 -
	                 11*E^(3*k*tc)*k^2*R*W8 + 6*k*M*R*W8 - 24*E^(k*tc)*k*M*R*W8 +
	                 30*E^(2*k*tc)*k*M*R*W8 - 12*E^(3*k*tc)*k*M*R*W8 + 3*M^2*R*W8 -
	                 9*E^(k*tc)*M^2*R*W8 + 9*E^(2*k*tc)*M^2*R*W8 - 3*E^(3*k*tc)*M^2*
	                  R*W8 + 6*E^(3*k*tc + M*(tc - tR))*k^3*X +
	                 22*E^(3*k*tc + M*(tc - tR))*k^2*M*X + 18*E^(3*k*tc +
	                    M*(tc - tR))*k*M^2*X + 4*E^(3*k*tc + M*(tc - tR))*M^3*X)^2 +
	              27*(R*W8 - 3*E^(k*tc)*R*W8 + 3*E^(2*k*tc)*R*W8 - E^(3*k*tc)*R*W8 +
	                 6*E^(3*k*tc + M*(tc - tR))*k*X + 4*E^(3*k*tc + M*(tc - tR))*M*
	                  X)^2*(-6*E^(3*k*tc)*k^3*R*W8 + 2*k^2*M*R*W8 - 9*E^(k*tc)*k^2*M*
	                 R*W8 + 18*E^(2*k*tc)*k^2*M*R*W8 - 11*E^(3*k*tc)*k^2*M*R*W8 +
	                3*k*M^2*R*W8 - 12*E^(k*tc)*k*M^2*R*W8 + 15*E^(2*k*tc)*k*M^2*R*
	                 W8 - 6*E^(3*k*tc)*k*M^2*R*W8 + M^3*R*W8 - 3*E^(k*tc)*M^3*R*W8 +
	                3*E^(2*k*tc)*M^3*R*W8 - E^(3*k*tc)*M^3*R*W8 +
	                6*E^(3*k*tc + M*(tc - tR))*k^3*M*X + 11*E^(3*k*tc + M*(tc - tR))*
	                 k^2*M^2*X + 6*E^(3*k*tc + M*(tc - tR))*k*M^3*X +
	                E^(3*k*tc + M*(tc - tR))*M^4*X) - 72*E^(3*k*tc + M*(tc - tR))*X*(
	                3*k*R*W8 - 12*E^(k*tc)*k*R*W8 + 15*E^(2*k*tc)*k*R*W8 -
	                6*E^(3*k*tc)*k*R*W8 + 3*M*R*W8 - 9*E^(k*tc)*M*R*W8 +
	                9*E^(2*k*tc)*M*R*W8 - 3*E^(3*k*tc)*M*R*W8 +
	                11*E^(3*k*tc + M*(tc - tR))*k^2*X + 18*E^(3*k*tc + M*(tc - tR))*
	                 k*M*X + 6*E^(3*k*tc + M*(tc - tR))*M^2*X)*(-6*E^(3*k*tc)*k^3*R*
	                 W8 + 2*k^2*M*R*W8 - 9*E^(k*tc)*k^2*M*R*W8 + 18*E^(2*k*tc)*k^2*M*
	                 R*W8 - 11*E^(3*k*tc)*k^2*M*R*W8 + 3*k*M^2*R*W8 - 12*E^(k*tc)*k*
	                 M^2*R*W8 + 15*E^(2*k*tc)*k*M^2*R*W8 - 6*E^(3*k*tc)*k*M^2*R*W8 +
	                M^3*R*W8 - 3*E^(k*tc)*M^3*R*W8 + 3*E^(2*k*tc)*M^3*R*W8 -
	                E^(3*k*tc)*M^3*R*W8 + 6*E^(3*k*tc + M*(tc - tR))*k^3*M*X +
	                11*E^(3*k*tc + M*(tc - tR))*k^2*M^2*X + 6*E^(3*k*tc +
	                   M*(tc - tR))*k*M^3*X + E^(3*k*tc + M*(tc - tR))*M^4*X))^2])^
	         (1/3)) + (E^((-9*k*tc - 3*M*(tc - tR))/3)*
	        (2*(3*k*R*W8 - 12*E^(k*tc)*k*R*W8 + 15*E^(2*k*tc)*k*R*W8 -
	             6*E^(3*k*tc)*k*R*W8 + 3*M*R*W8 - 9*E^(k*tc)*M*R*W8 +
	             9*E^(2*k*tc)*M*R*W8 - 3*E^(3*k*tc)*M*R*W8 +
	             11*E^(3*k*tc + M*(tc - tR))*k^2*X + 18*E^(3*k*tc + M*(tc - tR))*k*M*
	              X + 6*E^(3*k*tc + M*(tc - tR))*M^2*X)^3 -
	          9*(R*W8 - 3*E^(k*tc)*R*W8 + 3*E^(2*k*tc)*R*W8 - E^(3*k*tc)*R*W8 +
	            6*E^(3*k*tc + M*(tc - tR))*k*X + 4*E^(3*k*tc + M*(tc - tR))*M*X)*
	           (3*k*R*W8 - 12*E^(k*tc)*k*R*W8 + 15*E^(2*k*tc)*k*R*W8 -
	            6*E^(3*k*tc)*k*R*W8 + 3*M*R*W8 - 9*E^(k*tc)*M*R*W8 +
	            9*E^(2*k*tc)*M*R*W8 - 3*E^(3*k*tc)*M*R*W8 +
	            11*E^(3*k*tc + M*(tc - tR))*k^2*X + 18*E^(3*k*tc + M*(tc - tR))*k*M*
	             X + 6*E^(3*k*tc + M*(tc - tR))*M^2*X)*(2*k^2*R*W8 -
	            9*E^(k*tc)*k^2*R*W8 + 18*E^(2*k*tc)*k^2*R*W8 - 11*E^(3*k*tc)*k^2*R*
	             W8 + 6*k*M*R*W8 - 24*E^(k*tc)*k*M*R*W8 + 30*E^(2*k*tc)*k*M*R*W8 -
	            12*E^(3*k*tc)*k*M*R*W8 + 3*M^2*R*W8 - 9*E^(k*tc)*M^2*R*W8 +
	            9*E^(2*k*tc)*M^2*R*W8 - 3*E^(3*k*tc)*M^2*R*W8 +
	            6*E^(3*k*tc + M*(tc - tR))*k^3*X + 22*E^(3*k*tc + M*(tc - tR))*k^2*M*
	             X + 18*E^(3*k*tc + M*(tc - tR))*k*M^2*X +
	            4*E^(3*k*tc + M*(tc - tR))*M^3*X) + 27*E^(3*k*tc + M*(tc - tR))*X*
	           (2*k^2*R*W8 - 9*E^(k*tc)*k^2*R*W8 + 18*E^(2*k*tc)*k^2*R*W8 -
	             11*E^(3*k*tc)*k^2*R*W8 + 6*k*M*R*W8 - 24*E^(k*tc)*k*M*R*W8 +
	             30*E^(2*k*tc)*k*M*R*W8 - 12*E^(3*k*tc)*k*M*R*W8 + 3*M^2*R*W8 -
	             9*E^(k*tc)*M^2*R*W8 + 9*E^(2*k*tc)*M^2*R*W8 - 3*E^(3*k*tc)*M^2*R*
	              W8 + 6*E^(3*k*tc + M*(tc - tR))*k^3*X +
	             22*E^(3*k*tc + M*(tc - tR))*k^2*M*X + 18*E^(3*k*tc + M*(tc - tR))*k*
	              M^2*X + 4*E^(3*k*tc + M*(tc - tR))*M^3*X)^2 +
	          27*(R*W8 - 3*E^(k*tc)*R*W8 + 3*E^(2*k*tc)*R*W8 - E^(3*k*tc)*R*W8 +
	             6*E^(3*k*tc + M*(tc - tR))*k*X + 4*E^(3*k*tc + M*(tc - tR))*M*X)^2*
	           (-6*E^(3*k*tc)*k^3*R*W8 + 2*k^2*M*R*W8 - 9*E^(k*tc)*k^2*M*R*W8 +
	            18*E^(2*k*tc)*k^2*M*R*W8 - 11*E^(3*k*tc)*k^2*M*R*W8 + 3*k*M^2*R*W8 -
	            12*E^(k*tc)*k*M^2*R*W8 + 15*E^(2*k*tc)*k*M^2*R*W8 -
	            6*E^(3*k*tc)*k*M^2*R*W8 + M^3*R*W8 - 3*E^(k*tc)*M^3*R*W8 +
	            3*E^(2*k*tc)*M^3*R*W8 - E^(3*k*tc)*M^3*R*W8 +
	            6*E^(3*k*tc + M*(tc - tR))*k^3*M*X + 11*E^(3*k*tc + M*(tc - tR))*k^2*
	             M^2*X + 6*E^(3*k*tc + M*(tc - tR))*k*M^3*X +
	            E^(3*k*tc + M*(tc - tR))*M^4*X) - 72*E^(3*k*tc + M*(tc - tR))*X*
	           (3*k*R*W8 - 12*E^(k*tc)*k*R*W8 + 15*E^(2*k*tc)*k*R*W8 -
	            6*E^(3*k*tc)*k*R*W8 + 3*M*R*W8 - 9*E^(k*tc)*M*R*W8 +
	            9*E^(2*k*tc)*M*R*W8 - 3*E^(3*k*tc)*M*R*W8 +
	            11*E^(3*k*tc + M*(tc - tR))*k^2*X + 18*E^(3*k*tc + M*(tc - tR))*k*M*
	             X + 6*E^(3*k*tc + M*(tc - tR))*M^2*X)*(-6*E^(3*k*tc)*k^3*R*W8 +
	            2*k^2*M*R*W8 - 9*E^(k*tc)*k^2*M*R*W8 + 18*E^(2*k*tc)*k^2*M*R*W8 -
	            11*E^(3*k*tc)*k^2*M*R*W8 + 3*k*M^2*R*W8 - 12*E^(k*tc)*k*M^2*R*W8 +
	            15*E^(2*k*tc)*k*M^2*R*W8 - 6*E^(3*k*tc)*k*M^2*R*W8 + M^3*R*W8 -
	            3*E^(k*tc)*M^3*R*W8 + 3*E^(2*k*tc)*M^3*R*W8 - E^(3*k*tc)*M^3*R*W8 +
	            6*E^(3*k*tc + M*(tc - tR))*k^3*M*X + 11*E^(3*k*tc + M*(tc - tR))*k^2*
	             M^2*X + 6*E^(3*k*tc + M*(tc - tR))*k*M^3*X +
	            E^(3*k*tc + M*(tc - tR))*M^4*X) +
	          Sqrt[-4*(3*k^2*R^2*W8^2 - 27*E^(k*tc)*k^2*R^2*W8^2 + 81*E^(2*k*tc)*k^2*
	                R^2*W8^2 - 114*E^(3*k*tc)*k^2*R^2*W8^2 + 81*E^(4*k*tc)*k^2*R^2*
	                W8^2 - 27*E^(5*k*tc)*k^2*R^2*W8^2 + 3*E^(6*k*tc)*k^2*R^2*W8^2 +
	               12*E^(3*k*tc + M*(tc - tR))*k^3*R*W8*X - 48*E^(4*k*tc +
	                  M*(tc - tR))*k^3*R*W8*X - 48*E^(5*k*tc + M*(tc - tR))*k^3*R*W8*
	                X + 12*E^(6*k*tc + M*(tc - tR))*k^3*R*W8*X + 13*
	                E^(6*k*tc + 2*M*(tc - tR))*k^4*X^2)^3 +
	            (2*(3*k*R*W8 - 12*E^(k*tc)*k*R*W8 + 15*E^(2*k*tc)*k*R*W8 -
	                 6*E^(3*k*tc)*k*R*W8 + 3*M*R*W8 - 9*E^(k*tc)*M*R*W8 +
	                 9*E^(2*k*tc)*M*R*W8 - 3*E^(3*k*tc)*M*R*W8 +
	                 11*E^(3*k*tc + M*(tc - tR))*k^2*X + 18*E^(3*k*tc + M*(tc - tR))*
	                  k*M*X + 6*E^(3*k*tc + M*(tc - tR))*M^2*X)^3 -
	              9*(R*W8 - 3*E^(k*tc)*R*W8 + 3*E^(2*k*tc)*R*W8 - E^(3*k*tc)*R*W8 +
	                6*E^(3*k*tc + M*(tc - tR))*k*X + 4*E^(3*k*tc + M*(tc - tR))*M*X)*
	               (3*k*R*W8 - 12*E^(k*tc)*k*R*W8 + 15*E^(2*k*tc)*k*R*W8 -
	                6*E^(3*k*tc)*k*R*W8 + 3*M*R*W8 - 9*E^(k*tc)*M*R*W8 +
	                9*E^(2*k*tc)*M*R*W8 - 3*E^(3*k*tc)*M*R*W8 +
	                11*E^(3*k*tc + M*(tc - tR))*k^2*X + 18*E^(3*k*tc + M*(tc - tR))*
	                 k*M*X + 6*E^(3*k*tc + M*(tc - tR))*M^2*X)*(2*k^2*R*W8 -
	                9*E^(k*tc)*k^2*R*W8 + 18*E^(2*k*tc)*k^2*R*W8 - 11*E^(3*k*tc)*k^2*
	                 R*W8 + 6*k*M*R*W8 - 24*E^(k*tc)*k*M*R*W8 + 30*E^(2*k*tc)*k*M*R*
	                 W8 - 12*E^(3*k*tc)*k*M*R*W8 + 3*M^2*R*W8 - 9*E^(k*tc)*M^2*R*
	                 W8 + 9*E^(2*k*tc)*M^2*R*W8 - 3*E^(3*k*tc)*M^2*R*W8 +
	                6*E^(3*k*tc + M*(tc - tR))*k^3*X + 22*E^(3*k*tc + M*(tc - tR))*
	                 k^2*M*X + 18*E^(3*k*tc + M*(tc - tR))*k*M^2*X +
	                4*E^(3*k*tc + M*(tc - tR))*M^3*X) + 27*E^(3*k*tc + M*(tc - tR))*
	               X*(2*k^2*R*W8 - 9*E^(k*tc)*k^2*R*W8 + 18*E^(2*k*tc)*k^2*R*W8 -
	                 11*E^(3*k*tc)*k^2*R*W8 + 6*k*M*R*W8 - 24*E^(k*tc)*k*M*R*W8 +
	                 30*E^(2*k*tc)*k*M*R*W8 - 12*E^(3*k*tc)*k*M*R*W8 + 3*M^2*R*W8 -
	                 9*E^(k*tc)*M^2*R*W8 + 9*E^(2*k*tc)*M^2*R*W8 - 3*E^(3*k*tc)*M^2*
	                  R*W8 + 6*E^(3*k*tc + M*(tc - tR))*k^3*X +
	                 22*E^(3*k*tc + M*(tc - tR))*k^2*M*X + 18*E^(3*k*tc +
	                    M*(tc - tR))*k*M^2*X + 4*E^(3*k*tc + M*(tc - tR))*M^3*X)^2 +
	              27*(R*W8 - 3*E^(k*tc)*R*W8 + 3*E^(2*k*tc)*R*W8 - E^(3*k*tc)*R*W8 +
	                 6*E^(3*k*tc + M*(tc - tR))*k*X + 4*E^(3*k*tc + M*(tc - tR))*M*
	                  X)^2*(-6*E^(3*k*tc)*k^3*R*W8 + 2*k^2*M*R*W8 - 9*E^(k*tc)*k^2*M*
	                 R*W8 + 18*E^(2*k*tc)*k^2*M*R*W8 - 11*E^(3*k*tc)*k^2*M*R*W8 +
	                3*k*M^2*R*W8 - 12*E^(k*tc)*k*M^2*R*W8 + 15*E^(2*k*tc)*k*M^2*R*
	                 W8 - 6*E^(3*k*tc)*k*M^2*R*W8 + M^3*R*W8 - 3*E^(k*tc)*M^3*R*W8 +
	                3*E^(2*k*tc)*M^3*R*W8 - E^(3*k*tc)*M^3*R*W8 +
	                6*E^(3*k*tc + M*(tc - tR))*k^3*M*X + 11*E^(3*k*tc + M*(tc - tR))*
	                 k^2*M^2*X + 6*E^(3*k*tc + M*(tc - tR))*k*M^3*X +
	                E^(3*k*tc + M*(tc - tR))*M^4*X) - 72*E^(3*k*tc + M*(tc - tR))*X*(
	                3*k*R*W8 - 12*E^(k*tc)*k*R*W8 + 15*E^(2*k*tc)*k*R*W8 -
	                6*E^(3*k*tc)*k*R*W8 + 3*M*R*W8 - 9*E^(k*tc)*M*R*W8 +
	                9*E^(2*k*tc)*M*R*W8 - 3*E^(3*k*tc)*M*R*W8 +
	                11*E^(3*k*tc + M*(tc - tR))*k^2*X + 18*E^(3*k*tc + M*(tc - tR))*
	                 k*M*X + 6*E^(3*k*tc + M*(tc - tR))*M^2*X)*(-6*E^(3*k*tc)*k^3*R*
	                 W8 + 2*k^2*M*R*W8 - 9*E^(k*tc)*k^2*M*R*W8 + 18*E^(2*k*tc)*k^2*M*
	                 R*W8 - 11*E^(3*k*tc)*k^2*M*R*W8 + 3*k*M^2*R*W8 - 12*E^(k*tc)*k*
	                 M^2*R*W8 + 15*E^(2*k*tc)*k*M^2*R*W8 - 6*E^(3*k*tc)*k*M^2*R*W8 +
	                M^3*R*W8 - 3*E^(k*tc)*M^3*R*W8 + 3*E^(2*k*tc)*M^3*R*W8 -
	                E^(3*k*tc)*M^3*R*W8 + 6*E^(3*k*tc + M*(tc - tR))*k^3*M*X +
	                11*E^(3*k*tc + M*(tc - tR))*k^2*M^2*X + 6*E^(3*k*tc +
	                   M*(tc - tR))*k*M^3*X + E^(3*k*tc + M*(tc - tR))*M^4*X))^2])^
	         (1/3))/(3*2^(1/3)*X)]/2 +
	   Sqrt[(E^(-6*k*tc - 2*M*(tc - tR))*(R*W8 - 3*E^(k*tc)*R*W8 +
	          3*E^(2*k*tc)*R*W8 - E^(3*k*tc)*R*W8 + 6*E^(3*k*tc + M*(tc - tR))*k*X +
	          4*E^(3*k*tc + M*(tc - tR))*M*X)^2)/(2*X^2) -
	      (4*E^(-3*k*tc - M*(tc - tR))*(3*k*R*W8 - 12*E^(k*tc)*k*R*W8 +
	         15*E^(2*k*tc)*k*R*W8 - 6*E^(3*k*tc)*k*R*W8 + 3*M*R*W8 -
	         9*E^(k*tc)*M*R*W8 + 9*E^(2*k*tc)*M*R*W8 - 3*E^(3*k*tc)*M*R*W8 +
	         11*E^(3*k*tc + M*(tc - tR))*k^2*X + 18*E^(3*k*tc + M*(tc - tR))*k*M*X +
	         6*E^(3*k*tc + M*(tc - tR))*M^2*X))/(3*X) -
	      (2^(1/3)*E^(-6*k*tc - (-9*k*tc - 3*M*(tc - tR))/3 - 2*M*(tc - tR))*k^2*
	        (3*R^2*W8^2 - 27*E^(k*tc)*R^2*W8^2 + 81*E^(2*k*tc)*R^2*W8^2 -
	         114*E^(3*k*tc)*R^2*W8^2 + 81*E^(4*k*tc)*R^2*W8^2 -
	         27*E^(5*k*tc)*R^2*W8^2 + 3*E^(6*k*tc)*R^2*W8^2 +
	         12*E^(3*k*tc + M*(tc - tR))*k*R*W8*X - 48*E^(4*k*tc + M*(tc - tR))*k*R*
	          W8*X - 48*E^(5*k*tc + M*(tc - tR))*k*R*W8*X +
	         12*E^(6*k*tc + M*(tc - tR))*k*R*W8*X + 13*E^(6*k*tc + 2*M*(tc - tR))*
	          k^2*X^2))/(3*X*(2*(3*k*R*W8 - 12*E^(k*tc)*k*R*W8 + 15*E^(2*k*tc)*k*R*
	              W8 - 6*E^(3*k*tc)*k*R*W8 + 3*M*R*W8 - 9*E^(k*tc)*M*R*W8 +
	             9*E^(2*k*tc)*M*R*W8 - 3*E^(3*k*tc)*M*R*W8 +
	             11*E^(3*k*tc + M*(tc - tR))*k^2*X + 18*E^(3*k*tc + M*(tc - tR))*k*M*
	              X + 6*E^(3*k*tc + M*(tc - tR))*M^2*X)^3 -
	          9*(R*W8 - 3*E^(k*tc)*R*W8 + 3*E^(2*k*tc)*R*W8 - E^(3*k*tc)*R*W8 +
	            6*E^(3*k*tc + M*(tc - tR))*k*X + 4*E^(3*k*tc + M*(tc - tR))*M*X)*
	           (3*k*R*W8 - 12*E^(k*tc)*k*R*W8 + 15*E^(2*k*tc)*k*R*W8 -
	            6*E^(3*k*tc)*k*R*W8 + 3*M*R*W8 - 9*E^(k*tc)*M*R*W8 +
	            9*E^(2*k*tc)*M*R*W8 - 3*E^(3*k*tc)*M*R*W8 +
	            11*E^(3*k*tc + M*(tc - tR))*k^2*X + 18*E^(3*k*tc + M*(tc - tR))*k*M*
	             X + 6*E^(3*k*tc + M*(tc - tR))*M^2*X)*(2*k^2*R*W8 -
	            9*E^(k*tc)*k^2*R*W8 + 18*E^(2*k*tc)*k^2*R*W8 - 11*E^(3*k*tc)*k^2*R*
	             W8 + 6*k*M*R*W8 - 24*E^(k*tc)*k*M*R*W8 + 30*E^(2*k*tc)*k*M*R*W8 -
	            12*E^(3*k*tc)*k*M*R*W8 + 3*M^2*R*W8 - 9*E^(k*tc)*M^2*R*W8 +
	            9*E^(2*k*tc)*M^2*R*W8 - 3*E^(3*k*tc)*M^2*R*W8 +
	            6*E^(3*k*tc + M*(tc - tR))*k^3*X + 22*E^(3*k*tc + M*(tc - tR))*k^2*M*
	             X + 18*E^(3*k*tc + M*(tc - tR))*k*M^2*X +
	            4*E^(3*k*tc + M*(tc - tR))*M^3*X) + 27*E^(3*k*tc + M*(tc - tR))*X*
	           (2*k^2*R*W8 - 9*E^(k*tc)*k^2*R*W8 + 18*E^(2*k*tc)*k^2*R*W8 -
	             11*E^(3*k*tc)*k^2*R*W8 + 6*k*M*R*W8 - 24*E^(k*tc)*k*M*R*W8 +
	             30*E^(2*k*tc)*k*M*R*W8 - 12*E^(3*k*tc)*k*M*R*W8 + 3*M^2*R*W8 -
	             9*E^(k*tc)*M^2*R*W8 + 9*E^(2*k*tc)*M^2*R*W8 - 3*E^(3*k*tc)*M^2*R*
	              W8 + 6*E^(3*k*tc + M*(tc - tR))*k^3*X +
	             22*E^(3*k*tc + M*(tc - tR))*k^2*M*X + 18*E^(3*k*tc + M*(tc - tR))*k*
	              M^2*X + 4*E^(3*k*tc + M*(tc - tR))*M^3*X)^2 +
	          27*(R*W8 - 3*E^(k*tc)*R*W8 + 3*E^(2*k*tc)*R*W8 - E^(3*k*tc)*R*W8 +
	             6*E^(3*k*tc + M*(tc - tR))*k*X + 4*E^(3*k*tc + M*(tc - tR))*M*X)^2*
	           (-6*E^(3*k*tc)*k^3*R*W8 + 2*k^2*M*R*W8 - 9*E^(k*tc)*k^2*M*R*W8 +
	            18*E^(2*k*tc)*k^2*M*R*W8 - 11*E^(3*k*tc)*k^2*M*R*W8 + 3*k*M^2*R*W8 -
	            12*E^(k*tc)*k*M^2*R*W8 + 15*E^(2*k*tc)*k*M^2*R*W8 -
	            6*E^(3*k*tc)*k*M^2*R*W8 + M^3*R*W8 - 3*E^(k*tc)*M^3*R*W8 +
	            3*E^(2*k*tc)*M^3*R*W8 - E^(3*k*tc)*M^3*R*W8 +
	            6*E^(3*k*tc + M*(tc - tR))*k^3*M*X + 11*E^(3*k*tc + M*(tc - tR))*k^2*
	             M^2*X + 6*E^(3*k*tc + M*(tc - tR))*k*M^3*X +
	            E^(3*k*tc + M*(tc - tR))*M^4*X) - 72*E^(3*k*tc + M*(tc - tR))*X*
	           (3*k*R*W8 - 12*E^(k*tc)*k*R*W8 + 15*E^(2*k*tc)*k*R*W8 -
	            6*E^(3*k*tc)*k*R*W8 + 3*M*R*W8 - 9*E^(k*tc)*M*R*W8 +
	            9*E^(2*k*tc)*M*R*W8 - 3*E^(3*k*tc)*M*R*W8 +
	            11*E^(3*k*tc + M*(tc - tR))*k^2*X + 18*E^(3*k*tc + M*(tc - tR))*k*M*
	             X + 6*E^(3*k*tc + M*(tc - tR))*M^2*X)*(-6*E^(3*k*tc)*k^3*R*W8 +
	            2*k^2*M*R*W8 - 9*E^(k*tc)*k^2*M*R*W8 + 18*E^(2*k*tc)*k^2*M*R*W8 -
	            11*E^(3*k*tc)*k^2*M*R*W8 + 3*k*M^2*R*W8 - 12*E^(k*tc)*k*M^2*R*W8 +
	            15*E^(2*k*tc)*k*M^2*R*W8 - 6*E^(3*k*tc)*k*M^2*R*W8 + M^3*R*W8 -
	            3*E^(k*tc)*M^3*R*W8 + 3*E^(2*k*tc)*M^3*R*W8 - E^(3*k*tc)*M^3*R*W8 +
	            6*E^(3*k*tc + M*(tc - tR))*k^3*M*X + 11*E^(3*k*tc + M*(tc - tR))*k^2*
	             M^2*X + 6*E^(3*k*tc + M*(tc - tR))*k*M^3*X +
	            E^(3*k*tc + M*(tc - tR))*M^4*X) +
	          Sqrt[-4*(3*k^2*R^2*W8^2 - 27*E^(k*tc)*k^2*R^2*W8^2 + 81*E^(2*k*tc)*k^2*
	                R^2*W8^2 - 114*E^(3*k*tc)*k^2*R^2*W8^2 + 81*E^(4*k*tc)*k^2*R^2*
	                W8^2 - 27*E^(5*k*tc)*k^2*R^2*W8^2 + 3*E^(6*k*tc)*k^2*R^2*W8^2 +
	               12*E^(3*k*tc + M*(tc - tR))*k^3*R*W8*X - 48*E^(4*k*tc +
	                  M*(tc - tR))*k^3*R*W8*X - 48*E^(5*k*tc + M*(tc - tR))*k^3*R*W8*
	                X + 12*E^(6*k*tc + M*(tc - tR))*k^3*R*W8*X + 13*
	                E^(6*k*tc + 2*M*(tc - tR))*k^4*X^2)^3 +
	            (2*(3*k*R*W8 - 12*E^(k*tc)*k*R*W8 + 15*E^(2*k*tc)*k*R*W8 -
	                 6*E^(3*k*tc)*k*R*W8 + 3*M*R*W8 - 9*E^(k*tc)*M*R*W8 +
	                 9*E^(2*k*tc)*M*R*W8 - 3*E^(3*k*tc)*M*R*W8 +
	                 11*E^(3*k*tc + M*(tc - tR))*k^2*X + 18*E^(3*k*tc + M*(tc - tR))*
	                  k*M*X + 6*E^(3*k*tc + M*(tc - tR))*M^2*X)^3 -
	              9*(R*W8 - 3*E^(k*tc)*R*W8 + 3*E^(2*k*tc)*R*W8 - E^(3*k*tc)*R*W8 +
	                6*E^(3*k*tc + M*(tc - tR))*k*X + 4*E^(3*k*tc + M*(tc - tR))*M*X)*
	               (3*k*R*W8 - 12*E^(k*tc)*k*R*W8 + 15*E^(2*k*tc)*k*R*W8 -
	                6*E^(3*k*tc)*k*R*W8 + 3*M*R*W8 - 9*E^(k*tc)*M*R*W8 +
	                9*E^(2*k*tc)*M*R*W8 - 3*E^(3*k*tc)*M*R*W8 +
	                11*E^(3*k*tc + M*(tc - tR))*k^2*X + 18*E^(3*k*tc + M*(tc - tR))*
	                 k*M*X + 6*E^(3*k*tc + M*(tc - tR))*M^2*X)*(2*k^2*R*W8 -
	                9*E^(k*tc)*k^2*R*W8 + 18*E^(2*k*tc)*k^2*R*W8 - 11*E^(3*k*tc)*k^2*
	                 R*W8 + 6*k*M*R*W8 - 24*E^(k*tc)*k*M*R*W8 + 30*E^(2*k*tc)*k*M*R*
	                 W8 - 12*E^(3*k*tc)*k*M*R*W8 + 3*M^2*R*W8 - 9*E^(k*tc)*M^2*R*
	                 W8 + 9*E^(2*k*tc)*M^2*R*W8 - 3*E^(3*k*tc)*M^2*R*W8 +
	                6*E^(3*k*tc + M*(tc - tR))*k^3*X + 22*E^(3*k*tc + M*(tc - tR))*
	                 k^2*M*X + 18*E^(3*k*tc + M*(tc - tR))*k*M^2*X +
	                4*E^(3*k*tc + M*(tc - tR))*M^3*X) + 27*E^(3*k*tc + M*(tc - tR))*
	               X*(2*k^2*R*W8 - 9*E^(k*tc)*k^2*R*W8 + 18*E^(2*k*tc)*k^2*R*W8 -
	                 11*E^(3*k*tc)*k^2*R*W8 + 6*k*M*R*W8 - 24*E^(k*tc)*k*M*R*W8 +
	                 30*E^(2*k*tc)*k*M*R*W8 - 12*E^(3*k*tc)*k*M*R*W8 + 3*M^2*R*W8 -
	                 9*E^(k*tc)*M^2*R*W8 + 9*E^(2*k*tc)*M^2*R*W8 - 3*E^(3*k*tc)*M^2*
	                  R*W8 + 6*E^(3*k*tc + M*(tc - tR))*k^3*X +
	                 22*E^(3*k*tc + M*(tc - tR))*k^2*M*X + 18*E^(3*k*tc +
	                    M*(tc - tR))*k*M^2*X + 4*E^(3*k*tc + M*(tc - tR))*M^3*X)^2 +
	              27*(R*W8 - 3*E^(k*tc)*R*W8 + 3*E^(2*k*tc)*R*W8 - E^(3*k*tc)*R*W8 +
	                 6*E^(3*k*tc + M*(tc - tR))*k*X + 4*E^(3*k*tc + M*(tc - tR))*M*
	                  X)^2*(-6*E^(3*k*tc)*k^3*R*W8 + 2*k^2*M*R*W8 - 9*E^(k*tc)*k^2*M*
	                 R*W8 + 18*E^(2*k*tc)*k^2*M*R*W8 - 11*E^(3*k*tc)*k^2*M*R*W8 +
	                3*k*M^2*R*W8 - 12*E^(k*tc)*k*M^2*R*W8 + 15*E^(2*k*tc)*k*M^2*R*
	                 W8 - 6*E^(3*k*tc)*k*M^2*R*W8 + M^3*R*W8 - 3*E^(k*tc)*M^3*R*W8 +
	                3*E^(2*k*tc)*M^3*R*W8 - E^(3*k*tc)*M^3*R*W8 +
	                6*E^(3*k*tc + M*(tc - tR))*k^3*M*X + 11*E^(3*k*tc + M*(tc - tR))*
	                 k^2*M^2*X + 6*E^(3*k*tc + M*(tc - tR))*k*M^3*X +
	                E^(3*k*tc + M*(tc - tR))*M^4*X) - 72*E^(3*k*tc + M*(tc - tR))*X*(
	                3*k*R*W8 - 12*E^(k*tc)*k*R*W8 + 15*E^(2*k*tc)*k*R*W8 -
	                6*E^(3*k*tc)*k*R*W8 + 3*M*R*W8 - 9*E^(k*tc)*M*R*W8 +
	                9*E^(2*k*tc)*M*R*W8 - 3*E^(3*k*tc)*M*R*W8 +
	                11*E^(3*k*tc + M*(tc - tR))*k^2*X + 18*E^(3*k*tc + M*(tc - tR))*
	                 k*M*X + 6*E^(3*k*tc + M*(tc - tR))*M^2*X)*(-6*E^(3*k*tc)*k^3*R*
	                 W8 + 2*k^2*M*R*W8 - 9*E^(k*tc)*k^2*M*R*W8 + 18*E^(2*k*tc)*k^2*M*
	                 R*W8 - 11*E^(3*k*tc)*k^2*M*R*W8 + 3*k*M^2*R*W8 - 12*E^(k*tc)*k*
	                 M^2*R*W8 + 15*E^(2*k*tc)*k*M^2*R*W8 - 6*E^(3*k*tc)*k*M^2*R*W8 +
	                M^3*R*W8 - 3*E^(k*tc)*M^3*R*W8 + 3*E^(2*k*tc)*M^3*R*W8 -
	                E^(3*k*tc)*M^3*R*W8 + 6*E^(3*k*tc + M*(tc - tR))*k^3*M*X +
	                11*E^(3*k*tc + M*(tc - tR))*k^2*M^2*X + 6*E^(3*k*tc +
	                   M*(tc - tR))*k*M^3*X + E^(3*k*tc + M*(tc - tR))*M^4*X))^2])^
	         (1/3)) - (E^((-9*k*tc - 3*M*(tc - tR))/3)*
	        (2*(3*k*R*W8 - 12*E^(k*tc)*k*R*W8 + 15*E^(2*k*tc)*k*R*W8 -
	             6*E^(3*k*tc)*k*R*W8 + 3*M*R*W8 - 9*E^(k*tc)*M*R*W8 +
	             9*E^(2*k*tc)*M*R*W8 - 3*E^(3*k*tc)*M*R*W8 +
	             11*E^(3*k*tc + M*(tc - tR))*k^2*X + 18*E^(3*k*tc + M*(tc - tR))*k*M*
	              X + 6*E^(3*k*tc + M*(tc - tR))*M^2*X)^3 -
	          9*(R*W8 - 3*E^(k*tc)*R*W8 + 3*E^(2*k*tc)*R*W8 - E^(3*k*tc)*R*W8 +
	            6*E^(3*k*tc + M*(tc - tR))*k*X + 4*E^(3*k*tc + M*(tc - tR))*M*X)*
	           (3*k*R*W8 - 12*E^(k*tc)*k*R*W8 + 15*E^(2*k*tc)*k*R*W8 -
	            6*E^(3*k*tc)*k*R*W8 + 3*M*R*W8 - 9*E^(k*tc)*M*R*W8 +
	            9*E^(2*k*tc)*M*R*W8 - 3*E^(3*k*tc)*M*R*W8 +
	            11*E^(3*k*tc + M*(tc - tR))*k^2*X + 18*E^(3*k*tc + M*(tc - tR))*k*M*
	             X + 6*E^(3*k*tc + M*(tc - tR))*M^2*X)*(2*k^2*R*W8 -
	            9*E^(k*tc)*k^2*R*W8 + 18*E^(2*k*tc)*k^2*R*W8 - 11*E^(3*k*tc)*k^2*R*
	             W8 + 6*k*M*R*W8 - 24*E^(k*tc)*k*M*R*W8 + 30*E^(2*k*tc)*k*M*R*W8 -
	            12*E^(3*k*tc)*k*M*R*W8 + 3*M^2*R*W8 - 9*E^(k*tc)*M^2*R*W8 +
	            9*E^(2*k*tc)*M^2*R*W8 - 3*E^(3*k*tc)*M^2*R*W8 +
	            6*E^(3*k*tc + M*(tc - tR))*k^3*X + 22*E^(3*k*tc + M*(tc - tR))*k^2*M*
	             X + 18*E^(3*k*tc + M*(tc - tR))*k*M^2*X +
	            4*E^(3*k*tc + M*(tc - tR))*M^3*X) + 27*E^(3*k*tc + M*(tc - tR))*X*
	           (2*k^2*R*W8 - 9*E^(k*tc)*k^2*R*W8 + 18*E^(2*k*tc)*k^2*R*W8 -
	             11*E^(3*k*tc)*k^2*R*W8 + 6*k*M*R*W8 - 24*E^(k*tc)*k*M*R*W8 +
	             30*E^(2*k*tc)*k*M*R*W8 - 12*E^(3*k*tc)*k*M*R*W8 + 3*M^2*R*W8 -
	             9*E^(k*tc)*M^2*R*W8 + 9*E^(2*k*tc)*M^2*R*W8 - 3*E^(3*k*tc)*M^2*R*
	              W8 + 6*E^(3*k*tc + M*(tc - tR))*k^3*X +
	             22*E^(3*k*tc + M*(tc - tR))*k^2*M*X + 18*E^(3*k*tc + M*(tc - tR))*k*
	              M^2*X + 4*E^(3*k*tc + M*(tc - tR))*M^3*X)^2 +
	          27*(R*W8 - 3*E^(k*tc)*R*W8 + 3*E^(2*k*tc)*R*W8 - E^(3*k*tc)*R*W8 +
	             6*E^(3*k*tc + M*(tc - tR))*k*X + 4*E^(3*k*tc + M*(tc - tR))*M*X)^2*
	           (-6*E^(3*k*tc)*k^3*R*W8 + 2*k^2*M*R*W8 - 9*E^(k*tc)*k^2*M*R*W8 +
	            18*E^(2*k*tc)*k^2*M*R*W8 - 11*E^(3*k*tc)*k^2*M*R*W8 + 3*k*M^2*R*W8 -
	            12*E^(k*tc)*k*M^2*R*W8 + 15*E^(2*k*tc)*k*M^2*R*W8 -
	            6*E^(3*k*tc)*k*M^2*R*W8 + M^3*R*W8 - 3*E^(k*tc)*M^3*R*W8 +
	            3*E^(2*k*tc)*M^3*R*W8 - E^(3*k*tc)*M^3*R*W8 +
	            6*E^(3*k*tc + M*(tc - tR))*k^3*M*X + 11*E^(3*k*tc + M*(tc - tR))*k^2*
	             M^2*X + 6*E^(3*k*tc + M*(tc - tR))*k*M^3*X +
	            E^(3*k*tc + M*(tc - tR))*M^4*X) - 72*E^(3*k*tc + M*(tc - tR))*X*
	           (3*k*R*W8 - 12*E^(k*tc)*k*R*W8 + 15*E^(2*k*tc)*k*R*W8 -
	            6*E^(3*k*tc)*k*R*W8 + 3*M*R*W8 - 9*E^(k*tc)*M*R*W8 +
	            9*E^(2*k*tc)*M*R*W8 - 3*E^(3*k*tc)*M*R*W8 +
	            11*E^(3*k*tc + M*(tc - tR))*k^2*X + 18*E^(3*k*tc + M*(tc - tR))*k*M*
	             X + 6*E^(3*k*tc + M*(tc - tR))*M^2*X)*(-6*E^(3*k*tc)*k^3*R*W8 +
	            2*k^2*M*R*W8 - 9*E^(k*tc)*k^2*M*R*W8 + 18*E^(2*k*tc)*k^2*M*R*W8 -
	            11*E^(3*k*tc)*k^2*M*R*W8 + 3*k*M^2*R*W8 - 12*E^(k*tc)*k*M^2*R*W8 +
	            15*E^(2*k*tc)*k*M^2*R*W8 - 6*E^(3*k*tc)*k*M^2*R*W8 + M^3*R*W8 -
	            3*E^(k*tc)*M^3*R*W8 + 3*E^(2*k*tc)*M^3*R*W8 - E^(3*k*tc)*M^3*R*W8 +
	            6*E^(3*k*tc + M*(tc - tR))*k^3*M*X + 11*E^(3*k*tc + M*(tc - tR))*k^2*
	             M^2*X + 6*E^(3*k*tc + M*(tc - tR))*k*M^3*X +
	            E^(3*k*tc + M*(tc - tR))*M^4*X) +
	          Sqrt[-4*(3*k^2*R^2*W8^2 - 27*E^(k*tc)*k^2*R^2*W8^2 + 81*E^(2*k*tc)*k^2*
	                R^2*W8^2 - 114*E^(3*k*tc)*k^2*R^2*W8^2 + 81*E^(4*k*tc)*k^2*R^2*
	                W8^2 - 27*E^(5*k*tc)*k^2*R^2*W8^2 + 3*E^(6*k*tc)*k^2*R^2*W8^2 +
	               12*E^(3*k*tc + M*(tc - tR))*k^3*R*W8*X - 48*E^(4*k*tc +
	                  M*(tc - tR))*k^3*R*W8*X - 48*E^(5*k*tc + M*(tc - tR))*k^3*R*W8*
	                X + 12*E^(6*k*tc + M*(tc - tR))*k^3*R*W8*X + 13*
	                E^(6*k*tc + 2*M*(tc - tR))*k^4*X^2)^3 +
	            (2*(3*k*R*W8 - 12*E^(k*tc)*k*R*W8 + 15*E^(2*k*tc)*k*R*W8 -
	                 6*E^(3*k*tc)*k*R*W8 + 3*M*R*W8 - 9*E^(k*tc)*M*R*W8 +
	                 9*E^(2*k*tc)*M*R*W8 - 3*E^(3*k*tc)*M*R*W8 +
	                 11*E^(3*k*tc + M*(tc - tR))*k^2*X + 18*E^(3*k*tc + M*(tc - tR))*
	                  k*M*X + 6*E^(3*k*tc + M*(tc - tR))*M^2*X)^3 -
	              9*(R*W8 - 3*E^(k*tc)*R*W8 + 3*E^(2*k*tc)*R*W8 - E^(3*k*tc)*R*W8 +
	                6*E^(3*k*tc + M*(tc - tR))*k*X + 4*E^(3*k*tc + M*(tc - tR))*M*X)*
	               (3*k*R*W8 - 12*E^(k*tc)*k*R*W8 + 15*E^(2*k*tc)*k*R*W8 -
	                6*E^(3*k*tc)*k*R*W8 + 3*M*R*W8 - 9*E^(k*tc)*M*R*W8 +
	                9*E^(2*k*tc)*M*R*W8 - 3*E^(3*k*tc)*M*R*W8 +
	                11*E^(3*k*tc + M*(tc - tR))*k^2*X + 18*E^(3*k*tc + M*(tc - tR))*
	                 k*M*X + 6*E^(3*k*tc + M*(tc - tR))*M^2*X)*(2*k^2*R*W8 -
	                9*E^(k*tc)*k^2*R*W8 + 18*E^(2*k*tc)*k^2*R*W8 - 11*E^(3*k*tc)*k^2*
	                 R*W8 + 6*k*M*R*W8 - 24*E^(k*tc)*k*M*R*W8 + 30*E^(2*k*tc)*k*M*R*
	                 W8 - 12*E^(3*k*tc)*k*M*R*W8 + 3*M^2*R*W8 - 9*E^(k*tc)*M^2*R*
	                 W8 + 9*E^(2*k*tc)*M^2*R*W8 - 3*E^(3*k*tc)*M^2*R*W8 +
	                6*E^(3*k*tc + M*(tc - tR))*k^3*X + 22*E^(3*k*tc + M*(tc - tR))*
	                 k^2*M*X + 18*E^(3*k*tc + M*(tc - tR))*k*M^2*X +
	                4*E^(3*k*tc + M*(tc - tR))*M^3*X) + 27*E^(3*k*tc + M*(tc - tR))*
	               X*(2*k^2*R*W8 - 9*E^(k*tc)*k^2*R*W8 + 18*E^(2*k*tc)*k^2*R*W8 -
	                 11*E^(3*k*tc)*k^2*R*W8 + 6*k*M*R*W8 - 24*E^(k*tc)*k*M*R*W8 +
	                 30*E^(2*k*tc)*k*M*R*W8 - 12*E^(3*k*tc)*k*M*R*W8 + 3*M^2*R*W8 -
	                 9*E^(k*tc)*M^2*R*W8 + 9*E^(2*k*tc)*M^2*R*W8 - 3*E^(3*k*tc)*M^2*
	                  R*W8 + 6*E^(3*k*tc + M*(tc - tR))*k^3*X +
	                 22*E^(3*k*tc + M*(tc - tR))*k^2*M*X + 18*E^(3*k*tc +
	                    M*(tc - tR))*k*M^2*X + 4*E^(3*k*tc + M*(tc - tR))*M^3*X)^2 +
	              27*(R*W8 - 3*E^(k*tc)*R*W8 + 3*E^(2*k*tc)*R*W8 - E^(3*k*tc)*R*W8 +
	                 6*E^(3*k*tc + M*(tc - tR))*k*X + 4*E^(3*k*tc + M*(tc - tR))*M*
	                  X)^2*(-6*E^(3*k*tc)*k^3*R*W8 + 2*k^2*M*R*W8 - 9*E^(k*tc)*k^2*M*
	                 R*W8 + 18*E^(2*k*tc)*k^2*M*R*W8 - 11*E^(3*k*tc)*k^2*M*R*W8 +
	                3*k*M^2*R*W8 - 12*E^(k*tc)*k*M^2*R*W8 + 15*E^(2*k*tc)*k*M^2*R*
	                 W8 - 6*E^(3*k*tc)*k*M^2*R*W8 + M^3*R*W8 - 3*E^(k*tc)*M^3*R*W8 +
	                3*E^(2*k*tc)*M^3*R*W8 - E^(3*k*tc)*M^3*R*W8 +
	                6*E^(3*k*tc + M*(tc - tR))*k^3*M*X + 11*E^(3*k*tc + M*(tc - tR))*
	                 k^2*M^2*X + 6*E^(3*k*tc + M*(tc - tR))*k*M^3*X +
	                E^(3*k*tc + M*(tc - tR))*M^4*X) - 72*E^(3*k*tc + M*(tc - tR))*X*(
	                3*k*R*W8 - 12*E^(k*tc)*k*R*W8 + 15*E^(2*k*tc)*k*R*W8 -
	                6*E^(3*k*tc)*k*R*W8 + 3*M*R*W8 - 9*E^(k*tc)*M*R*W8 +
	                9*E^(2*k*tc)*M*R*W8 - 3*E^(3*k*tc)*M*R*W8 +
	                11*E^(3*k*tc + M*(tc - tR))*k^2*X + 18*E^(3*k*tc + M*(tc - tR))*
	                 k*M*X + 6*E^(3*k*tc + M*(tc - tR))*M^2*X)*(-6*E^(3*k*tc)*k^3*R*
	                 W8 + 2*k^2*M*R*W8 - 9*E^(k*tc)*k^2*M*R*W8 + 18*E^(2*k*tc)*k^2*M*
	                 R*W8 - 11*E^(3*k*tc)*k^2*M*R*W8 + 3*k*M^2*R*W8 - 12*E^(k*tc)*k*
	                 M^2*R*W8 + 15*E^(2*k*tc)*k*M^2*R*W8 - 6*E^(3*k*tc)*k*M^2*R*W8 +
	                M^3*R*W8 - 3*E^(k*tc)*M^3*R*W8 + 3*E^(2*k*tc)*M^3*R*W8 -
	                E^(3*k*tc)*M^3*R*W8 + 6*E^(3*k*tc + M*(tc - tR))*k^3*M*X +
	                11*E^(3*k*tc + M*(tc - tR))*k^2*M^2*X + 6*E^(3*k*tc +
	                   M*(tc - tR))*k*M^3*X + E^(3*k*tc + M*(tc - tR))*M^4*X))^2])^
	         (1/3))/(3*2^(1/3)*X) +
	      (-((E^(-9*k*tc - 3*M*(tc - tR))*(R*W8 - 3*E^(k*tc)*R*W8 +
	             3*E^(2*k*tc)*R*W8 - E^(3*k*tc)*R*W8 + 6*E^(3*k*tc + M*(tc - tR))*k*
	              X + 4*E^(3*k*tc + M*(tc - tR))*M*X)^3)/X^3) +
	        (4*E^(-6*k*tc - 2*M*(tc - tR))*(R*W8 - 3*E^(k*tc)*R*W8 +
	           3*E^(2*k*tc)*R*W8 - E^(3*k*tc)*R*W8 + 6*E^(3*k*tc + M*(tc - tR))*k*
	            X + 4*E^(3*k*tc + M*(tc - tR))*M*X)*(3*k*R*W8 - 12*E^(k*tc)*k*R*W8 +
	           15*E^(2*k*tc)*k*R*W8 - 6*E^(3*k*tc)*k*R*W8 + 3*M*R*W8 -
	           9*E^(k*tc)*M*R*W8 + 9*E^(2*k*tc)*M*R*W8 - 3*E^(3*k*tc)*M*R*W8 +
	           11*E^(3*k*tc + M*(tc - tR))*k^2*X + 18*E^(3*k*tc + M*(tc - tR))*k*M*
	            X + 6*E^(3*k*tc + M*(tc - tR))*M^2*X))/X^2 -
	        (8*E^(-3*k*tc - M*(tc - tR))*(2*k^2*R*W8 - 9*E^(k*tc)*k^2*R*W8 +
	           18*E^(2*k*tc)*k^2*R*W8 - 11*E^(3*k*tc)*k^2*R*W8 + 6*k*M*R*W8 -
	           24*E^(k*tc)*k*M*R*W8 + 30*E^(2*k*tc)*k*M*R*W8 - 12*E^(3*k*tc)*k*M*R*
	            W8 + 3*M^2*R*W8 - 9*E^(k*tc)*M^2*R*W8 + 9*E^(2*k*tc)*M^2*R*W8 -
	           3*E^(3*k*tc)*M^2*R*W8 + 6*E^(3*k*tc + M*(tc - tR))*k^3*X +
	           22*E^(3*k*tc + M*(tc - tR))*k^2*M*X + 18*E^(3*k*tc + M*(tc - tR))*k*
	            M^2*X + 4*E^(3*k*tc + M*(tc - tR))*M^3*X))/X)/
	       (4*Sqrt[(E^(-6*k*tc - 2*M*(tc - tR))*(R*W8 - 3*E^(k*tc)*R*W8 +
	              3*E^(2*k*tc)*R*W8 - E^(3*k*tc)*R*W8 + 6*E^(3*k*tc + M*(tc - tR))*k*
	               X + 4*E^(3*k*tc + M*(tc - tR))*M*X)^2)/(4*X^2) -
	          (2*E^(-3*k*tc - M*(tc - tR))*(3*k*R*W8 - 12*E^(k*tc)*k*R*W8 +
	             15*E^(2*k*tc)*k*R*W8 - 6*E^(3*k*tc)*k*R*W8 + 3*M*R*W8 -
	             9*E^(k*tc)*M*R*W8 + 9*E^(2*k*tc)*M*R*W8 - 3*E^(3*k*tc)*M*R*W8 +
	             11*E^(3*k*tc + M*(tc - tR))*k^2*X + 18*E^(3*k*tc + M*(tc - tR))*k*M*
	              X + 6*E^(3*k*tc + M*(tc - tR))*M^2*X))/(3*X) +
	          (2^(1/3)*E^(-6*k*tc - (-9*k*tc - 3*M*(tc - tR))/3 - 2*M*(tc - tR))*k^2*
	            (3*R^2*W8^2 - 27*E^(k*tc)*R^2*W8^2 + 81*E^(2*k*tc)*R^2*W8^2 -
	             114*E^(3*k*tc)*R^2*W8^2 + 81*E^(4*k*tc)*R^2*W8^2 -
	             27*E^(5*k*tc)*R^2*W8^2 + 3*E^(6*k*tc)*R^2*W8^2 +
	             12*E^(3*k*tc + M*(tc - tR))*k*R*W8*X - 48*E^(4*k*tc + M*(tc - tR))*
	              k*R*W8*X - 48*E^(5*k*tc + M*(tc - tR))*k*R*W8*X +
	             12*E^(6*k*tc + M*(tc - tR))*k*R*W8*X + 13*E^(6*k*tc +
	                2*M*(tc - tR))*k^2*X^2))/
	           (3*X*(2*(3*k*R*W8 - 12*E^(k*tc)*k*R*W8 + 15*E^(2*k*tc)*k*R*W8 -
	                 6*E^(3*k*tc)*k*R*W8 + 3*M*R*W8 - 9*E^(k*tc)*M*R*W8 +
	                 9*E^(2*k*tc)*M*R*W8 - 3*E^(3*k*tc)*M*R*W8 +
	                 11*E^(3*k*tc + M*(tc - tR))*k^2*X + 18*E^(3*k*tc + M*(tc - tR))*
	                  k*M*X + 6*E^(3*k*tc + M*(tc - tR))*M^2*X)^3 -
	              9*(R*W8 - 3*E^(k*tc)*R*W8 + 3*E^(2*k*tc)*R*W8 - E^(3*k*tc)*R*W8 +
	                6*E^(3*k*tc + M*(tc - tR))*k*X + 4*E^(3*k*tc + M*(tc - tR))*M*X)*
	               (3*k*R*W8 - 12*E^(k*tc)*k*R*W8 + 15*E^(2*k*tc)*k*R*W8 -
	                6*E^(3*k*tc)*k*R*W8 + 3*M*R*W8 - 9*E^(k*tc)*M*R*W8 +
	                9*E^(2*k*tc)*M*R*W8 - 3*E^(3*k*tc)*M*R*W8 +
	                11*E^(3*k*tc + M*(tc - tR))*k^2*X + 18*E^(3*k*tc + M*(tc - tR))*
	                 k*M*X + 6*E^(3*k*tc + M*(tc - tR))*M^2*X)*(2*k^2*R*W8 -
	                9*E^(k*tc)*k^2*R*W8 + 18*E^(2*k*tc)*k^2*R*W8 - 11*E^(3*k*tc)*k^2*
	                 R*W8 + 6*k*M*R*W8 - 24*E^(k*tc)*k*M*R*W8 + 30*E^(2*k*tc)*k*M*R*
	                 W8 - 12*E^(3*k*tc)*k*M*R*W8 + 3*M^2*R*W8 - 9*E^(k*tc)*M^2*R*
	                 W8 + 9*E^(2*k*tc)*M^2*R*W8 - 3*E^(3*k*tc)*M^2*R*W8 +
	                6*E^(3*k*tc + M*(tc - tR))*k^3*X + 22*E^(3*k*tc + M*(tc - tR))*
	                 k^2*M*X + 18*E^(3*k*tc + M*(tc - tR))*k*M^2*X +
	                4*E^(3*k*tc + M*(tc - tR))*M^3*X) + 27*E^(3*k*tc + M*(tc - tR))*
	               X*(2*k^2*R*W8 - 9*E^(k*tc)*k^2*R*W8 + 18*E^(2*k*tc)*k^2*R*W8 -
	                 11*E^(3*k*tc)*k^2*R*W8 + 6*k*M*R*W8 - 24*E^(k*tc)*k*M*R*W8 +
	                 30*E^(2*k*tc)*k*M*R*W8 - 12*E^(3*k*tc)*k*M*R*W8 + 3*M^2*R*W8 -
	                 9*E^(k*tc)*M^2*R*W8 + 9*E^(2*k*tc)*M^2*R*W8 - 3*E^(3*k*tc)*M^2*
	                  R*W8 + 6*E^(3*k*tc + M*(tc - tR))*k^3*X +
	                 22*E^(3*k*tc + M*(tc - tR))*k^2*M*X + 18*E^(3*k*tc +
	                    M*(tc - tR))*k*M^2*X + 4*E^(3*k*tc + M*(tc - tR))*M^3*X)^2 +
	              27*(R*W8 - 3*E^(k*tc)*R*W8 + 3*E^(2*k*tc)*R*W8 - E^(3*k*tc)*R*W8 +
	                 6*E^(3*k*tc + M*(tc - tR))*k*X + 4*E^(3*k*tc + M*(tc - tR))*M*
	                  X)^2*(-6*E^(3*k*tc)*k^3*R*W8 + 2*k^2*M*R*W8 - 9*E^(k*tc)*k^2*M*
	                 R*W8 + 18*E^(2*k*tc)*k^2*M*R*W8 - 11*E^(3*k*tc)*k^2*M*R*W8 +
	                3*k*M^2*R*W8 - 12*E^(k*tc)*k*M^2*R*W8 + 15*E^(2*k*tc)*k*M^2*R*
	                 W8 - 6*E^(3*k*tc)*k*M^2*R*W8 + M^3*R*W8 - 3*E^(k*tc)*M^3*R*W8 +
	                3*E^(2*k*tc)*M^3*R*W8 - E^(3*k*tc)*M^3*R*W8 +
	                6*E^(3*k*tc + M*(tc - tR))*k^3*M*X + 11*E^(3*k*tc + M*(tc - tR))*
	                 k^2*M^2*X + 6*E^(3*k*tc + M*(tc - tR))*k*M^3*X +
	                E^(3*k*tc + M*(tc - tR))*M^4*X) - 72*E^(3*k*tc + M*(tc - tR))*X*(
	                3*k*R*W8 - 12*E^(k*tc)*k*R*W8 + 15*E^(2*k*tc)*k*R*W8 -
	                6*E^(3*k*tc)*k*R*W8 + 3*M*R*W8 - 9*E^(k*tc)*M*R*W8 +
	                9*E^(2*k*tc)*M*R*W8 - 3*E^(3*k*tc)*M*R*W8 +
	                11*E^(3*k*tc + M*(tc - tR))*k^2*X + 18*E^(3*k*tc + M*(tc - tR))*
	                 k*M*X + 6*E^(3*k*tc + M*(tc - tR))*M^2*X)*(-6*E^(3*k*tc)*k^3*R*
	                 W8 + 2*k^2*M*R*W8 - 9*E^(k*tc)*k^2*M*R*W8 + 18*E^(2*k*tc)*k^2*M*
	                 R*W8 - 11*E^(3*k*tc)*k^2*M*R*W8 + 3*k*M^2*R*W8 - 12*E^(k*tc)*k*
	                 M^2*R*W8 + 15*E^(2*k*tc)*k*M^2*R*W8 - 6*E^(3*k*tc)*k*M^2*R*W8 +
	                M^3*R*W8 - 3*E^(k*tc)*M^3*R*W8 + 3*E^(2*k*tc)*M^3*R*W8 -
	                E^(3*k*tc)*M^3*R*W8 + 6*E^(3*k*tc + M*(tc - tR))*k^3*M*X +
	                11*E^(3*k*tc + M*(tc - tR))*k^2*M^2*X + 6*E^(3*k*tc +
	                   M*(tc - tR))*k*M^3*X + E^(3*k*tc + M*(tc - tR))*M^4*X) +
	              Sqrt[-4*(3*k^2*R^2*W8^2 - 27*E^(k*tc)*k^2*R^2*W8^2 + 81*E^(2*k*tc)*
	                    k^2*R^2*W8^2 - 114*E^(3*k*tc)*k^2*R^2*W8^2 + 81*E^(4*k*tc)*
	                    k^2*R^2*W8^2 - 27*E^(5*k*tc)*k^2*R^2*W8^2 + 3*E^(6*k*tc)*k^2*
	                    R^2*W8^2 + 12*E^(3*k*tc + M*(tc - tR))*k^3*R*W8*X -
	                   48*E^(4*k*tc + M*(tc - tR))*k^3*R*W8*X - 48*E^(5*k*tc +
	                      M*(tc - tR))*k^3*R*W8*X + 12*E^(6*k*tc + M*(tc - tR))*k^3*
	                    R*W8*X + 13*E^(6*k*tc + 2*M*(tc - tR))*k^4*X^2)^3 +
	                (2*(3*k*R*W8 - 12*E^(k*tc)*k*R*W8 + 15*E^(2*k*tc)*k*R*W8 -
	                     6*E^(3*k*tc)*k*R*W8 + 3*M*R*W8 - 9*E^(k*tc)*M*R*W8 +
	                     9*E^(2*k*tc)*M*R*W8 - 3*E^(3*k*tc)*M*R*W8 +
	                     11*E^(3*k*tc + M*(tc - tR))*k^2*X + 18*E^(3*k*tc +
	                        M*(tc - tR))*k*M*X + 6*E^(3*k*tc + M*(tc - tR))*M^2*X)^
	                    3 - 9*(R*W8 - 3*E^(k*tc)*R*W8 + 3*E^(2*k*tc)*R*W8 -
	                    E^(3*k*tc)*R*W8 + 6*E^(3*k*tc + M*(tc - tR))*k*X +
	                    4*E^(3*k*tc + M*(tc - tR))*M*X)*(3*k*R*W8 - 12*E^(k*tc)*k*R*
	                     W8 + 15*E^(2*k*tc)*k*R*W8 - 6*E^(3*k*tc)*k*R*W8 +
	                    3*M*R*W8 - 9*E^(k*tc)*M*R*W8 + 9*E^(2*k*tc)*M*R*W8 -
	                    3*E^(3*k*tc)*M*R*W8 + 11*E^(3*k*tc + M*(tc - tR))*k^2*X +
	                    18*E^(3*k*tc + M*(tc - tR))*k*M*X + 6*E^(3*k*tc +
	                       M*(tc - tR))*M^2*X)*(2*k^2*R*W8 - 9*E^(k*tc)*k^2*R*W8 +
	                    18*E^(2*k*tc)*k^2*R*W8 - 11*E^(3*k*tc)*k^2*R*W8 +
	                    6*k*M*R*W8 - 24*E^(k*tc)*k*M*R*W8 + 30*E^(2*k*tc)*k*M*R*W8 -
	                    12*E^(3*k*tc)*k*M*R*W8 + 3*M^2*R*W8 - 9*E^(k*tc)*M^2*R*W8 +
	                    9*E^(2*k*tc)*M^2*R*W8 - 3*E^(3*k*tc)*M^2*R*W8 +
	                    6*E^(3*k*tc + M*(tc - tR))*k^3*X + 22*E^(3*k*tc +
	                       M*(tc - tR))*k^2*M*X + 18*E^(3*k*tc + M*(tc - tR))*k*M^2*
	                     X + 4*E^(3*k*tc + M*(tc - tR))*M^3*X) +
	                  27*E^(3*k*tc + M*(tc - tR))*X*(2*k^2*R*W8 - 9*E^(k*tc)*k^2*R*
	                      W8 + 18*E^(2*k*tc)*k^2*R*W8 - 11*E^(3*k*tc)*k^2*R*W8 +
	                     6*k*M*R*W8 - 24*E^(k*tc)*k*M*R*W8 + 30*E^(2*k*tc)*k*M*R*
	                      W8 - 12*E^(3*k*tc)*k*M*R*W8 + 3*M^2*R*W8 - 9*E^(k*tc)*M^2*
	                      R*W8 + 9*E^(2*k*tc)*M^2*R*W8 - 3*E^(3*k*tc)*M^2*R*W8 +
	                     6*E^(3*k*tc + M*(tc - tR))*k^3*X + 22*E^(3*k*tc +
	                        M*(tc - tR))*k^2*M*X + 18*E^(3*k*tc + M*(tc - tR))*k*M^2*
	                      X + 4*E^(3*k*tc + M*(tc - tR))*M^3*X)^2 +
	                  27*(R*W8 - 3*E^(k*tc)*R*W8 + 3*E^(2*k*tc)*R*W8 - E^(3*k*tc)*R*
	                      W8 + 6*E^(3*k*tc + M*(tc - tR))*k*X + 4*E^(3*k*tc +
	                        M*(tc - tR))*M*X)^2*(-6*E^(3*k*tc)*k^3*R*W8 +
	                    2*k^2*M*R*W8 - 9*E^(k*tc)*k^2*M*R*W8 + 18*E^(2*k*tc)*k^2*M*R*
	                     W8 - 11*E^(3*k*tc)*k^2*M*R*W8 + 3*k*M^2*R*W8 - 12*E^(k*tc)*
	                     k*M^2*R*W8 + 15*E^(2*k*tc)*k*M^2*R*W8 - 6*E^(3*k*tc)*k*M^2*
	                     R*W8 + M^3*R*W8 - 3*E^(k*tc)*M^3*R*W8 + 3*E^(2*k*tc)*M^3*R*
	                     W8 - E^(3*k*tc)*M^3*R*W8 + 6*E^(3*k*tc + M*(tc - tR))*k^3*M*
	                     X + 11*E^(3*k*tc + M*(tc - tR))*k^2*M^2*X +
	                    6*E^(3*k*tc + M*(tc - tR))*k*M^3*X + E^(3*k*tc +
	                       M*(tc - tR))*M^4*X) - 72*E^(3*k*tc + M*(tc - tR))*X*
	                   (3*k*R*W8 - 12*E^(k*tc)*k*R*W8 + 15*E^(2*k*tc)*k*R*W8 -
	                    6*E^(3*k*tc)*k*R*W8 + 3*M*R*W8 - 9*E^(k*tc)*M*R*W8 +
	                    9*E^(2*k*tc)*M*R*W8 - 3*E^(3*k*tc)*M*R*W8 +
	                    11*E^(3*k*tc + M*(tc - tR))*k^2*X + 18*E^(3*k*tc +
	                       M*(tc - tR))*k*M*X + 6*E^(3*k*tc + M*(tc - tR))*M^2*X)*
	                   (-6*E^(3*k*tc)*k^3*R*W8 + 2*k^2*M*R*W8 - 9*E^(k*tc)*k^2*M*R*
	                     W8 + 18*E^(2*k*tc)*k^2*M*R*W8 - 11*E^(3*k*tc)*k^2*M*R*W8 +
	                    3*k*M^2*R*W8 - 12*E^(k*tc)*k*M^2*R*W8 + 15*E^(2*k*tc)*k*M^2*
	                     R*W8 - 6*E^(3*k*tc)*k*M^2*R*W8 + M^3*R*W8 - 3*E^(k*tc)*M^3*
	                     R*W8 + 3*E^(2*k*tc)*M^3*R*W8 - E^(3*k*tc)*M^3*R*W8 +
	                    6*E^(3*k*tc + M*(tc - tR))*k^3*M*X + 11*E^(3*k*tc +
	                       M*(tc - tR))*k^2*M^2*X + 6*E^(3*k*tc + M*(tc - tR))*k*M^3*
	                     X + E^(3*k*tc + M*(tc - tR))*M^4*X))^2])^(1/3)) +
	          (E^((-9*k*tc - 3*M*(tc - tR))/3)*(2*(3*k*R*W8 - 12*E^(k*tc)*k*R*W8 +
	                 15*E^(2*k*tc)*k*R*W8 - 6*E^(3*k*tc)*k*R*W8 + 3*M*R*W8 -
	                 9*E^(k*tc)*M*R*W8 + 9*E^(2*k*tc)*M*R*W8 - 3*E^(3*k*tc)*M*R*W8 +
	                 11*E^(3*k*tc + M*(tc - tR))*k^2*X + 18*E^(3*k*tc + M*(tc - tR))*
	                  k*M*X + 6*E^(3*k*tc + M*(tc - tR))*M^2*X)^3 -
	              9*(R*W8 - 3*E^(k*tc)*R*W8 + 3*E^(2*k*tc)*R*W8 - E^(3*k*tc)*R*W8 +
	                6*E^(3*k*tc + M*(tc - tR))*k*X + 4*E^(3*k*tc + M*(tc - tR))*M*X)*
	               (3*k*R*W8 - 12*E^(k*tc)*k*R*W8 + 15*E^(2*k*tc)*k*R*W8 -
	                6*E^(3*k*tc)*k*R*W8 + 3*M*R*W8 - 9*E^(k*tc)*M*R*W8 +
	                9*E^(2*k*tc)*M*R*W8 - 3*E^(3*k*tc)*M*R*W8 +
	                11*E^(3*k*tc + M*(tc - tR))*k^2*X + 18*E^(3*k*tc + M*(tc - tR))*
	                 k*M*X + 6*E^(3*k*tc + M*(tc - tR))*M^2*X)*(2*k^2*R*W8 -
	                9*E^(k*tc)*k^2*R*W8 + 18*E^(2*k*tc)*k^2*R*W8 - 11*E^(3*k*tc)*k^2*
	                 R*W8 + 6*k*M*R*W8 - 24*E^(k*tc)*k*M*R*W8 + 30*E^(2*k*tc)*k*M*R*
	                 W8 - 12*E^(3*k*tc)*k*M*R*W8 + 3*M^2*R*W8 - 9*E^(k*tc)*M^2*R*
	                 W8 + 9*E^(2*k*tc)*M^2*R*W8 - 3*E^(3*k*tc)*M^2*R*W8 +
	                6*E^(3*k*tc + M*(tc - tR))*k^3*X + 22*E^(3*k*tc + M*(tc - tR))*
	                 k^2*M*X + 18*E^(3*k*tc + M*(tc - tR))*k*M^2*X +
	                4*E^(3*k*tc + M*(tc - tR))*M^3*X) + 27*E^(3*k*tc + M*(tc - tR))*
	               X*(2*k^2*R*W8 - 9*E^(k*tc)*k^2*R*W8 + 18*E^(2*k*tc)*k^2*R*W8 -
	                 11*E^(3*k*tc)*k^2*R*W8 + 6*k*M*R*W8 - 24*E^(k*tc)*k*M*R*W8 +
	                 30*E^(2*k*tc)*k*M*R*W8 - 12*E^(3*k*tc)*k*M*R*W8 + 3*M^2*R*W8 -
	                 9*E^(k*tc)*M^2*R*W8 + 9*E^(2*k*tc)*M^2*R*W8 - 3*E^(3*k*tc)*M^2*
	                  R*W8 + 6*E^(3*k*tc + M*(tc - tR))*k^3*X +
	                 22*E^(3*k*tc + M*(tc - tR))*k^2*M*X + 18*E^(3*k*tc +
	                    M*(tc - tR))*k*M^2*X + 4*E^(3*k*tc + M*(tc - tR))*M^3*X)^2 +
	              27*(R*W8 - 3*E^(k*tc)*R*W8 + 3*E^(2*k*tc)*R*W8 - E^(3*k*tc)*R*W8 +
	                 6*E^(3*k*tc + M*(tc - tR))*k*X + 4*E^(3*k*tc + M*(tc - tR))*M*
	                  X)^2*(-6*E^(3*k*tc)*k^3*R*W8 + 2*k^2*M*R*W8 - 9*E^(k*tc)*k^2*M*
	                 R*W8 + 18*E^(2*k*tc)*k^2*M*R*W8 - 11*E^(3*k*tc)*k^2*M*R*W8 +
	                3*k*M^2*R*W8 - 12*E^(k*tc)*k*M^2*R*W8 + 15*E^(2*k*tc)*k*M^2*R*
	                 W8 - 6*E^(3*k*tc)*k*M^2*R*W8 + M^3*R*W8 - 3*E^(k*tc)*M^3*R*W8 +
	                3*E^(2*k*tc)*M^3*R*W8 - E^(3*k*tc)*M^3*R*W8 +
	                6*E^(3*k*tc + M*(tc - tR))*k^3*M*X + 11*E^(3*k*tc + M*(tc - tR))*
	                 k^2*M^2*X + 6*E^(3*k*tc + M*(tc - tR))*k*M^3*X +
	                E^(3*k*tc + M*(tc - tR))*M^4*X) - 72*E^(3*k*tc + M*(tc - tR))*X*(
	                3*k*R*W8 - 12*E^(k*tc)*k*R*W8 + 15*E^(2*k*tc)*k*R*W8 -
	                6*E^(3*k*tc)*k*R*W8 + 3*M*R*W8 - 9*E^(k*tc)*M*R*W8 +
	                9*E^(2*k*tc)*M*R*W8 - 3*E^(3*k*tc)*M*R*W8 +
	                11*E^(3*k*tc + M*(tc - tR))*k^2*X + 18*E^(3*k*tc + M*(tc - tR))*
	                 k*M*X + 6*E^(3*k*tc + M*(tc - tR))*M^2*X)*(-6*E^(3*k*tc)*k^3*R*
	                 W8 + 2*k^2*M*R*W8 - 9*E^(k*tc)*k^2*M*R*W8 + 18*E^(2*k*tc)*k^2*M*
	                 R*W8 - 11*E^(3*k*tc)*k^2*M*R*W8 + 3*k*M^2*R*W8 - 12*E^(k*tc)*k*
	                 M^2*R*W8 + 15*E^(2*k*tc)*k*M^2*R*W8 - 6*E^(3*k*tc)*k*M^2*R*W8 +
	                M^3*R*W8 - 3*E^(k*tc)*M^3*R*W8 + 3*E^(2*k*tc)*M^3*R*W8 -
	                E^(3*k*tc)*M^3*R*W8 + 6*E^(3*k*tc + M*(tc - tR))*k^3*M*X +
	                11*E^(3*k*tc + M*(tc - tR))*k^2*M^2*X + 6*E^(3*k*tc +
	                   M*(tc - tR))*k*M^3*X + E^(3*k*tc + M*(tc - tR))*M^4*X) +
	              Sqrt[-4*(3*k^2*R^2*W8^2 - 27*E^(k*tc)*k^2*R^2*W8^2 + 81*E^(2*k*tc)*
	                    k^2*R^2*W8^2 - 114*E^(3*k*tc)*k^2*R^2*W8^2 + 81*E^(4*k*tc)*
	                    k^2*R^2*W8^2 - 27*E^(5*k*tc)*k^2*R^2*W8^2 + 3*E^(6*k*tc)*k^2*
	                    R^2*W8^2 + 12*E^(3*k*tc + M*(tc - tR))*k^3*R*W8*X -
	                   48*E^(4*k*tc + M*(tc - tR))*k^3*R*W8*X - 48*E^(5*k*tc +
	                      M*(tc - tR))*k^3*R*W8*X + 12*E^(6*k*tc + M*(tc - tR))*k^3*
	                    R*W8*X + 13*E^(6*k*tc + 2*M*(tc - tR))*k^4*X^2)^3 +
	                (2*(3*k*R*W8 - 12*E^(k*tc)*k*R*W8 + 15*E^(2*k*tc)*k*R*W8 -
	                     6*E^(3*k*tc)*k*R*W8 + 3*M*R*W8 - 9*E^(k*tc)*M*R*W8 +
	                     9*E^(2*k*tc)*M*R*W8 - 3*E^(3*k*tc)*M*R*W8 +
	                     11*E^(3*k*tc + M*(tc - tR))*k^2*X + 18*E^(3*k*tc +
	                        M*(tc - tR))*k*M*X + 6*E^(3*k*tc + M*(tc - tR))*M^2*X)^
	                    3 - 9*(R*W8 - 3*E^(k*tc)*R*W8 + 3*E^(2*k*tc)*R*W8 -
	                    E^(3*k*tc)*R*W8 + 6*E^(3*k*tc + M*(tc - tR))*k*X +
	                    4*E^(3*k*tc + M*(tc - tR))*M*X)*(3*k*R*W8 - 12*E^(k*tc)*k*R*
	                     W8 + 15*E^(2*k*tc)*k*R*W8 - 6*E^(3*k*tc)*k*R*W8 +
	                    3*M*R*W8 - 9*E^(k*tc)*M*R*W8 + 9*E^(2*k*tc)*M*R*W8 -
	                    3*E^(3*k*tc)*M*R*W8 + 11*E^(3*k*tc + M*(tc - tR))*k^2*X +
	                    18*E^(3*k*tc + M*(tc - tR))*k*M*X + 6*E^(3*k*tc +
	                       M*(tc - tR))*M^2*X)*(2*k^2*R*W8 - 9*E^(k*tc)*k^2*R*W8 +
	                    18*E^(2*k*tc)*k^2*R*W8 - 11*E^(3*k*tc)*k^2*R*W8 +
	                    6*k*M*R*W8 - 24*E^(k*tc)*k*M*R*W8 + 30*E^(2*k*tc)*k*M*R*W8 -
	                    12*E^(3*k*tc)*k*M*R*W8 + 3*M^2*R*W8 - 9*E^(k*tc)*M^2*R*W8 +
	                    9*E^(2*k*tc)*M^2*R*W8 - 3*E^(3*k*tc)*M^2*R*W8 +
	                    6*E^(3*k*tc + M*(tc - tR))*k^3*X + 22*E^(3*k*tc +
	                       M*(tc - tR))*k^2*M*X + 18*E^(3*k*tc + M*(tc - tR))*k*M^2*
	                     X + 4*E^(3*k*tc + M*(tc - tR))*M^3*X) +
	                  27*E^(3*k*tc + M*(tc - tR))*X*(2*k^2*R*W8 - 9*E^(k*tc)*k^2*R*
	                      W8 + 18*E^(2*k*tc)*k^2*R*W8 - 11*E^(3*k*tc)*k^2*R*W8 +
	                     6*k*M*R*W8 - 24*E^(k*tc)*k*M*R*W8 + 30*E^(2*k*tc)*k*M*R*
	                      W8 - 12*E^(3*k*tc)*k*M*R*W8 + 3*M^2*R*W8 - 9*E^(k*tc)*M^2*
	                      R*W8 + 9*E^(2*k*tc)*M^2*R*W8 - 3*E^(3*k*tc)*M^2*R*W8 +
	                     6*E^(3*k*tc + M*(tc - tR))*k^3*X + 22*E^(3*k*tc +
	                        M*(tc - tR))*k^2*M*X + 18*E^(3*k*tc + M*(tc - tR))*k*M^2*
	                      X + 4*E^(3*k*tc + M*(tc - tR))*M^3*X)^2 +
	                  27*(R*W8 - 3*E^(k*tc)*R*W8 + 3*E^(2*k*tc)*R*W8 - E^(3*k*tc)*R*
	                      W8 + 6*E^(3*k*tc + M*(tc - tR))*k*X + 4*E^(3*k*tc +
	                        M*(tc - tR))*M*X)^2*(-6*E^(3*k*tc)*k^3*R*W8 +
	                    2*k^2*M*R*W8 - 9*E^(k*tc)*k^2*M*R*W8 + 18*E^(2*k*tc)*k^2*M*R*
	                     W8 - 11*E^(3*k*tc)*k^2*M*R*W8 + 3*k*M^2*R*W8 - 12*E^(k*tc)*
	                     k*M^2*R*W8 + 15*E^(2*k*tc)*k*M^2*R*W8 - 6*E^(3*k*tc)*k*M^2*
	                     R*W8 + M^3*R*W8 - 3*E^(k*tc)*M^3*R*W8 + 3*E^(2*k*tc)*M^3*R*
	                     W8 - E^(3*k*tc)*M^3*R*W8 + 6*E^(3*k*tc + M*(tc - tR))*k^3*M*
	                     X + 11*E^(3*k*tc + M*(tc - tR))*k^2*M^2*X +
	                    6*E^(3*k*tc + M*(tc - tR))*k*M^3*X + E^(3*k*tc +
	                       M*(tc - tR))*M^4*X) - 72*E^(3*k*tc + M*(tc - tR))*X*
	                   (3*k*R*W8 - 12*E^(k*tc)*k*R*W8 + 15*E^(2*k*tc)*k*R*W8 -
	                    6*E^(3*k*tc)*k*R*W8 + 3*M*R*W8 - 9*E^(k*tc)*M*R*W8 +
	                    9*E^(2*k*tc)*M*R*W8 - 3*E^(3*k*tc)*M*R*W8 +
	                    11*E^(3*k*tc + M*(tc - tR))*k^2*X + 18*E^(3*k*tc +
	                       M*(tc - tR))*k*M*X + 6*E^(3*k*tc + M*(tc - tR))*M^2*X)*
	                   (-6*E^(3*k*tc)*k^3*R*W8 + 2*k^2*M*R*W8 - 9*E^(k*tc)*k^2*M*R*
	                     W8 + 18*E^(2*k*tc)*k^2*M*R*W8 - 11*E^(3*k*tc)*k^2*M*R*W8 +
	                    3*k*M^2*R*W8 - 12*E^(k*tc)*k*M^2*R*W8 + 15*E^(2*k*tc)*k*M^2*
	                     R*W8 - 6*E^(3*k*tc)*k*M^2*R*W8 + M^3*R*W8 - 3*E^(k*tc)*M^3*
	                     R*W8 + 3*E^(2*k*tc)*M^3*R*W8 - E^(3*k*tc)*M^3*R*W8 +
	                    6*E^(3*k*tc + M*(tc - tR))*k^3*M*X + 11*E^(3*k*tc +
	                       M*(tc - tR))*k^2*M^2*X + 6*E^(3*k*tc + M*(tc - tR))*k*M^3*
	                     X + E^(3*k*tc + M*(tc - tR))*M^4*X))^2])^(1/3))/
           (3*2^(1/3)*X)])]/2,


           PGF


	]]]


	];





YieldOfBiomass[X_, opts___] := X * FishingMortalityEquilibrium[X, opts]




(*         Recruitment Functions            *)


BevertonHoltRecruitment[spawningbio_, opts___] :=

            Module[
              {mr,mrb,re},
	      {mr,mrb,re} =
	      {
	      MaximumRecruitment,
	      MaximumRecruitmentBiomass,
	      RecruitmentExponent} /. {opts} /. Options[Recruitment];


	      mr * spawningbio^re/((mrb/2)^re + spawningbio^re)

            ];


RickerRecruitment[spawningbio_, opts___] :=

            Module[
              {mr,mrb,re},
	      {mr,mrb,re} =
	      {
	      MaximumRecruitment,
	      MaximumRecruitmentBiomass,
	      RecruitmentExponent} /. {opts} /. Options[Recruitment];

	     (E^(1 - spawningbio^re/(mrb^re)) * mr * spawningbio^re)/(mrb^re)

            ];


ConstantRecruitment[spawningbio_, opts___] :=
            Module[
              {R},
	      {R} = {Recruits} /. {opts} /. Options[Recruitment];
              R
            ];


Recruitment[spawningbio_, opts___] :=

            Module[
              {d,b,m,k,S,R,M,F,W8,L8,tR,t0,tc,t8,uw,f,dt,bs,ts,bi,mr,mrb,re,rf,ma,cs,tms,tm,sfb,tcs,csp,msp},
	      {d,b,m,k,S,R,M,F,W8,L8,tR,t0,tc,t8,uw,f,dt,bs,ts,bi,mr,mrb,re,rf,ma,cs,tms,tm,sfb,tcs,csp,msp} =
	      optionslist /. {opts} /. Options[PopulationGrowth];

	      rf[spawningbio, opts]

            ];



(*-------------------------------------*)


(*****************************************)
(*       Growth Model Options            *)
(*****************************************)




(*****************************************)
(*  Growth  and Catch Model Algorithms   *)
(*****************************************)


(*           GompertzFox                 *)

GompertzFoxGrowth[x_, opts___] :=
	Module[{usemsy, msyopt, maxbio, ropts},

		{
		usemsy, msyopt, maxbio, ropts
		} =
		{
		UseMSY,
		MaximumSustainableYield,
		BiomassMaximum,
		IntrinsicGrowthRate
		}  /. {opts} /. Options[SurplusProduction];
		If[usemsy,
			-E msyopt (x/maxbio) Log[x/maxbio],
			-ropts x Log[x/maxbio]
		]
	];


GompertzFoxCatch[ff_,opts___]:=
	Module[{usemsy,msyopt,ccopt,maxbio,ropts},

		{
		usemsy,msyopt,ccopt,maxbio,ropts
		} =
		{
		UseMSY,
		MaximumSustainableYield,
		CatchabilityCoefficient,
		BiomassMaximum,
		IntrinsicGrowthRate
		}  /.{opts}/. Options[SurplusProduction];
		If[usemsy,
			ccopt*ff*maxbio * E^(-ff*maxbio*ccopt/(E*msyopt)),
			ccopt*ff*maxbio * E^(-ff*ccopt/ropts)
		]
	];



(*            VerhulstSchaefer            *)

VerhulstSchaeferGrowth[x_, opts___] :=
	Module[{usemsy, msyopt, maxbio, ropts},

		{
		usemsy, msyopt, maxbio, ropts
		} =
		{
		UseMSY,
		MaximumSustainableYield,
		BiomassMaximum,
		IntrinsicGrowthRate
		}  /. {opts} /. Options[SurplusProduction];

		If[usemsy,
			4 msyopt (x/maxbio)(1-x/maxbio),
			ropts x (1-x/maxbio)
		]
	];


VerhulstSchaeferCatch[ff_,opts___]:=
	Module[{usemsy,msyopt,ccopt,maxbio,ropts},

		{
		usemsy,msyopt,ccopt,maxbio,ropts
		} =
		{
		UseMSY,
		MaximumSustainableYield,
		CatchabilityCoefficient,
		BiomassMaximum,
		IntrinsicGrowthRate
		}  /.{opts}/. Options[SurplusProduction];
		If[usemsy,
			ccopt*ff*maxbio - ff maxbio^2 ccopt/(4 msyopt),
			ccopt*ff*maxbio - ff maxbio ccopt/ropts
		]
	];




(*           RichardsPellaTomlinson        *)

RichardsPellaTomlinsonGrowth[x_, opts___] :=
	Module[
		{rptpar, usemsy, msyopt, maxbio, ropts},

		{
		rptpar, usemsy, msyopt, maxbio, ropts
		} =
		{
		RichardsPellaTomlinsonParameter,
		UseMSY,
		MaximumSustainableYield,
		BiomassMaximum,
		IntrinsicGrowthRate
		}  /. {opts} /. Options[SurplusProduction];

		If[rptpar === 0,
			If[usemsy,

				msyopt -msyopt x/maxbio,

				ropts (-maxbio + x)
			],

			If[rptpar === 1 ,

				GompertzFoxGrowth[x, opts],

				If[usemsy,
					msyopt/(rptpar^(1/(1-rptpar))-rptpar^(rptpar/(1-rptpar))) * (x/maxbio) * (1-(x/maxbio)^(rptpar-1)),
					ropts*x*(1-(x/maxbio)^(rptpar-1))

				]
			]
		]
	];



RichardsPellaTomlinsonCatch[ff_, opts___] :=
	Module[
		{rptpar, usemsy, msyopt, ccopt, maxbio, ropts},

		{
		rptpar, usemsy, msyopt, ccopt, maxbio, ropts
		} =
		{
		RichardsPellaTomlinsonParameter,
		UseMSY,
		MaximumSustainableYield,
		CatchabilityCoefficient,
		BiomassMaximum,
		IntrinsicGrowthRate
		}  /. {opts} /. Options[SurplusProduction];

		If[rptpar === 0,

			If[usemsy,
				(ff maxbio msyopt ccopt)/(msyopt + ff maxbio ccopt),
				(ff maxbio ccopt ropts)/(-ff ccopt + ropts)
			],


			If[rptpar === 1 && usemsy, Exp[-ff maxbio ccopt/(E msyopt)]ff maxbio ccopt,

				If[rptpar === 1 ,

					GompertzFoxCatch[x, opts],

					If[usemsy,
						ccopt*f*(maxbio*(1-ff maxbio ccopt (rptpar-1) rptpar^(-rptpar/(rptpar-1))/msyopt)^(1/(rptpar-1))),
						ccopt*f*(maxbio*(1-ff ccopt/ropts)^(1/(rptpar-1)))
					]
				]
			]
		]
	];





(*     QuasiBevertonHolt (when ind.gr.rate=mort.rate)   *)


QuasiBevertonHoltGrowth[x_, opts___] :=

	Module[{lwrel, usemsy, msyopt, maxbio, ropts},

		{
		lwrel, usemsy, msyopt, maxbio, ropts
		} =
		{
		WeightLengthRelation,
		UseMSY,
		MaximumSustainableYield,
		BiomassMaximum,
		IntrinsicGrowthRate
		}  /. {opts} /. Options[SurplusProduction];


		RichardsPellaTomlinsonGrowth[
			x,
			RichardsPellaTomlinsonParameter	-> lwrel/(1+lwrel),
			UseMSY				-> usemsy,
			MaximumSustainableYield		-> msyopt,
			BiomassMaximum			-> maxbio,
			IntrinsicGrowthRate		-> ropts
		]


	];


QuasiBevertonHoltCatch[ff_, opts___] :=

	Module[{lwrel, usemsy, msyopt, maxbio, ropts},

		{
		lwrel, usemsy, msyopt, maxbio, ropts
		} =
		{
		WeightLengthRelation,
		UseMSY,
		MaximumSustainableYield,
		BiomassMaximum,
		IntrinsicGrowthRate
		}  /. {opts} /. Options[SurplusProduction];


		RichardsPellaTomlinsonCatch[
			ff,
			RichardsPellaTomlinsonParameter	-> lwrel/(1+lwrel),
			UseMSY				-> usemsy,
			MaximumSustainableYield		-> msyopt,
			BiomassMaximum			-> maxbio,
			IntrinsicGrowthRate		-> ropts
		]


	];



(*****************************************)
(*          SurplusProduction            *)
(*****************************************)

SurplusProduction[opts___]:=
	Module[{model,x,rptpar,usemsy,msyopt,ccopt,maxbio,ropts,lwrel},

		{
		model,x,rptpar,usemsy,msyopt,ccopt,maxbio,ropts,lwrel
		} =
		{
		GrowthModel,
		CurrentBiomass,
		RichardsPellaTomlinsonParameter,
		UseMSY,
		MaximumSustainableYield,
		CatchabilityCoefficient,
		BiomassMaximum,
		IntrinsicGrowthRate,
		WeightLengthRelation
		}  /.{opts}/. Options[SurplusProduction];

		ToExpression[ToString[model]<>"Growth"][x, opts]
	];



(*-------------------------------------*)




Notation[exp_] :=

	Module[{},
	  TraditionalForm[
	    MatrixForm[
	      exp /. {
	        PGX                   -> "X",
	        PGR                   -> "R",
	        PGSB                  -> "S",
	        PGkk                   -> "k",
	        PGM                   -> "M",
	        PGF                   -> "F",
	        PGW8                  -> Subscript["W",\[Infinity]],
	        PGL8                  -> Subscript["L",\[Infinity]],
	        PGb                   -> "b",
	        PGd                   -> "d",
	        PGt0                  -> Subscript["t",0],
	        PGtR                  -> Subscript["t","R"],
	        PGtc                  -> Subscript["t","c"],
	        PGtcs                 -> Subscript["t","cs"],
	        PGt8                  -> Subscript["t",\[Infinity]],
	        PGmR                  -> Subscript["R","max"],
	        PGmRb                 -> Subscript["S","hv"],
	        PGRe                  -> "s",
	        PGmm                   -> "m",
	        PGrr                   -> "r",
	        PGMSY                 -> "MSY",
	        PGq                   -> "q",
	        PGK                   -> "K",
	        PGtm                  -> Subscript["t","m"],
	        PGtms                 -> Subscript["t","ms"],
	        PGma                  -> Subscript["m","%"],

	        CohortBiomass         -> "x",
	        AgeOfMaxGrowth        -> Subscript["t","mbc"],
	        MaxBiomassGrowth      -> "mbc",

	        vonBertalanffyLength  -> "L",
	        BaranovNumbers        -> "N",
	        IndividAge            -> "t",
	        IndividualWeight      -> "W"

	      }
	    ]
	  ]
	];

SimplifyNotation[exp_] :=
                Notation[
                   FullSimplify[
                      exp,
                      {
			PGR    > 0,
			PGR    < Infinity,
			PGSB   > 0,
			PGSB   < Infinity,
			PGkk    > 0,
			PGkk    < Infinity,
			PGM    > 0,
			PGM    < Infinity,
			PGF    > 0,
			PGF    < Infinity,
			PGW8   > 0,
			PGW8   < Infinity,
			PGL8   > 0,
			PGL8   < Infinity,
			PGb    > 1,
			PGb    < Infinity,
			PGd    > 0,
			PGd    < Infinity,
			PGtR   >=0,
			PGtR   < Infinity,
			PGt8   > 0,
			PGt8   <=Infinity,
			PGtc   >=0,
			PGtc   < Infinity,
			PGt0   < 1,
			PGmR   > 0,
			PGmR   < Infinity,
			PGmRb  > 0,
			PGmRb  < Infinity,
			PGRe   > 0,
			PGRe   < Infinity,
			PGmm    < Infinity,
			PGmm    > 0,
			PGMSY  < Infinity,
			PGMSY  > 0,
			PGrr    < Infinity,
			PGrr    > 0,
			PGq    < Infinity,
			PGq    > 0,
			PGK    < Infinity,
			PGK    > 0,
			PGtm   <=PGt8,
			PGtm   >=0,
			PGtms  <=PGtm,
			PGtms  >=0,
			PGtcs  <=PGtc,
			PGtcs  >=0,

                        Element[{
			    PGR,
			    PGSB,
			    PGkk,
			    PGM,
			    PGF,
			    PGW8,
			    PGL8,
			    PGb,
			    PGd,
			    IndividAge,
			    PGtR,
			    PGt0,
			    PGtc,
			    PGt8,
			    PGmR,
			    PGmRb,
			    PGRe,
			    PGtm,
			    PGtms,
			    PGtcs,

			    PGmm,
			    PGMSY,
			    PGrr,
			    PGq,
			    PGK
			    },
			    Reals
                      ]
                      }
                   ]
                ];


End[]


EndPackage[]

