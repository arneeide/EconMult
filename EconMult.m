(* :Title: EconMult              *)
(* :Author: Arne Eide            *)
(* :Summary:
This package contains functions to the ECONMULT model.
*)
(* :Context: EconMult`EconMult`  *)
(* :Package Version: 4.0.1         *)
(* :Copyright: Arne Eide         *)
(* :History:
	Version 1.0 by Arne Eide 1991
	Version 2.0 by Arne Eide 1994
	Version 3.1 by Arne Eide March 26 2004
*)
(* :Keywords:
	Fleet model,
	Bioeconomics,
	Fisheries Management
*)
(* :Source:                      *)
(* :Warning: None.               *)
(* :Mathematica Version: 5.0     *)
(* :Limitation: None.            *)
(* :Discussion:                  *)

BeginPackage["EconMult`EconMult`", 
	"EconMult`PopulationGrowth`"]


EconMult::usage                 = "EconMult[] runs the EconMult model through one period (see EMyearPeriod). EconMult is design to perform numerical computations."
EMdimensionsQ::usage            = "EMdimensionsQ[] checks out consistency of parameter dimensions. It uses the EconMult options."
EMunitScale::usage              = "EMunitScale is a scaling factor in EconMult (scalar). (In EconSimp)"
EMbiomass::usage                = "EMbiomass is the input biomass vector in EconMult."
EMbiomassCatch::usage           = "EMbiomassCatch[] is the output catch biomass vector in EconMult. It uses the EconMult options."
EMeffortCorrection::usage       = "EMeffortCorrection is a procedure for adjusting effort according to quota rules."
EMfleetNameList::usage          = "EMfleetNameList is a vector of text string element containing a description of all fleet groups. (In EconSimp)"
EMfleetCorrection::usage        = "EMfleetCorrection[] makes use of pEMfleetCorrection to correct fleet sizes."
EMerrorCode::usage              = "EMerrorCode value is by delfaul 0, but is set to 1 when consistity test of dimensions shows negative result."
EMmaxCatchRate::usage           = "EMmaxCatchRate should be a number equal or less than 1 and gives the maximum catch percentage of stock biomass. Normal management rules will not activate  EMmaxCatchRate."
EMvesselCatch::usage            = "EMvesselCatch[] gives EconMult calculated vessel catch.It uses the EconMult options."
EMvesselBioUnitCatch::usage     = "EMvesselBioUnitCatch[]"
EMfleetCatch::usage             = "EMfleetCatch[] gives EconMult calculated fleet catch.It uses the EconMult options."
EMfleetRelatedCatch::usage      = "EMfleetRelatedCatch[]"
EMfleetQuotaCatch::usage        = "EMfleetQuotaCatch[]"
EMtotalQuotaCatch::usage        = ""
EMfleetRelatedQuotaCatch::usage = ""
EMfisheryCatch::usage           = "EMfisheryCatch[] gives EconMult calculated fishery catch.It uses the EconMult options."
EMvesselCM::usage               = "EMvesselCM[] gives EconMult calculated vessel contribution margins.It uses the EconMult options."
EMfleetCM::usage                = "EMfleetCM[] gives EconMult calculated fleet contribution margins.It uses the EconMult options."
EMfisheryCM::usage              = "EMfisheryCM[] gives EconMult calculated contribution margins of fisheries.It uses the EconMult options."
EMvesselDiscardCM::usage        = ""
EMvesselProfit::usage           = "EMvesselProfit[] gives EconMult calculated vessel profits.It uses the EconMult options."
EMfleetProfit::usage            = "EMfleetProfit[] gives EconMult calculated fleet profits.It uses the EconMult options."
EMfisheryProfit::usage          = "EMfisheryProfit[] gives EconMult calculated profits of fisheries.It uses the EconMult options."
EMvesselDiscardProfit::usage    = ""
EMfleetDiscardProfit::usage     = ""
EMfisheryDiscardProfit::usage   = ""
EMvesselRevenue::usage          = "EMvesselRevenue[] gives EconMult calculated vessel revenues.It uses the EconMult options."
EMfleetRevenue::usage           = "EMfleetRevenue[] gives EconMult calculated fleet revenues.It uses the EconMult options."
EMfisheryRevenue::usage         = "EMfisheryRevenue[] gives EconMult calculated revenues of fisheries.It uses the EconMult options."
EMbiomassRevenue::usage         = "EMbiomassRevenue[] is the output revenue vector in EconMult corresponding to the input biomass EMbiomass. EMbiomassRevenue uses the EconMult options."
EMvesselVC::usage               = "EMvesselVC[] gives EconMult calculated variable vessel costs.It uses the EconMult options."
EMfleetVC::usage                = "EMfleetVC[] gives EconMult calculated variable fleet costs.It uses the EconMult options."
EMfisheryVC::usage              = "EMfisheryVC[] gives EconMult calculated variable costs of fisheries.It uses the EconMult options."
EMpath::usage                   = "EMpath specifies the local path to source files of EconMult."
EMshrimpCatch::usage            = "Initial Shrimp Catch. Default value: 0."
EMtargetCatch::usage            = "EMtargetCatch[] is a vector containing the total catch of the targeted species. It uses the EconMult options."
EMtargetNameList::usage         = "EMtargetNameList is a vector containing the names of the targeted species."
EMtempVesselCatch::usage        = "EMtempVesselCatch is a temporary catch matrix created by EMvesselCatch to prevent unnecessary  reruns of EMvesselCatch."
EMtempDaysOfFishing::usage      = ""

EMstoreUnitsInit::usage         = ""

EMquotaCorrection::usage        = ""

asp::usage
asp1::usage
asp2::usage

EMcatchabilityDiscard::usage    = "EMcatchabilityDiscard[val] gives pEMcatchability for val=0, val=n gives value 0 to the first n catchability rates in each fishery, while val=-n gives value 0 to the n last rates. EMcatchabilityDiscard[{n1. n2}] gives value 0 to the catchability rates for the bio units n1 through n2."
EMdiscard::usage
EMdiscardMatrix::usage          = "EMdiscardMatrix[val] gives value 0 to the first val bio units in each fishery, while val=-n gives value 0 to the val last. EMdiscardMatrix[{n1. n2}] gives value 0 units n1 through n2. Default value is 1 for all biounits."



(*  OPTIONS FOR EconMult:*)
EMn::usage                      = "EMn is an option for EconMult that specifies number of vessels."
EMp::usage                      = "EMp is an option for EconMult that specifies unit prices of harvest."
EMq::usage                      = "EMq is an option for EconMult that specifies catchability rates of vessels."
EMrbu::usage                    = "EMrbu is an option for EconMult that specifies related bio units."
EMfc::usage                     = "EMfc is an option for EconMult that specifies fixed cost."
EMvcc::usage                    = "EMvcc is an option for EconMult that specifies variable cost of harvest."
EMvec::usage                    = "EMvec is an option for EconMult that specifies variable cost of effort."
EMx::usage                      = "EMx is an option for EconMult that specifies ecosystem biomass units."
EMa::usage                      = "EMa is an option for EconMult that specifies elasticities of efforts."
EMbeta::usage                   = "EMbeta is an option for EconMult that specifies elasticities of biomasses."
EMe::usage                      = "EMe is an option for EconMult that specifies number of fishing days."

EMbiomassGrowth::usage          = "EMbiomassGrowth is an option for EconMult that specifies the biological growth model. EMbiomassGrowth can contain a single value or an array of length equal EMinterval. Default value of EMbiomassGrowth is Automatic."
EMquotaType::usage              = "EMquotaType is an option for EconMult specifying the type of quota management. EMquotaType can hav the value Fixed (default) or ConstantF."

(*  BOOLEAN SWITCHES:             *)
bEMallcohorts::usage            = "bEMallcohorts is a boolean switch to include all cohorts or not (True/False)."
bEMcollect::usage               = "bEMcollect is a boolean switch to collect model variables or not (True/False)."
bEMfleetDynamics::usage         = "bEMfleetDynamics is a boolean switch to include fleet dynamics or not (True/False)."
bEMherringCatch::usage          = "bEMherringCatch is a boolean switch to include catch of young herring or not (True/False)."
bEMherringIncluded::usage       = "bEMherringIncluded is a boolean switch to include herring in the ecosystem or not (True/False)."
bEMseparateCohorts::usage       = "bEMseparateCohorts is a boolean switch to separate between year classes or not (True/False)."
bEMseparateMature::usage        = "bEMseparateMature is a boolean switch to separate between matures and immatures or not (True/False)."
bEMstrongQuota::usage           = "bEMstrongQuota is a boolean switch to set up strict quota management or not (True/False)."
bEMtempCatch::usage             = "bEMtempCatch is a boolean switch to make use of a temporary catch vector (EMtempVesselCatch) or not (True/False)."
bEMuseCM::usage                 = "bEMuseCM is a boolean switch to model company decisions on the base of positive/negative contribution margins or not (True/False)."
bEMuseTAC::usage                = "bEMuseTAC is a boolean switch to use quota management or not (True/False)."
EMprintOut::usage               = "EMprintOut is a boolean switch to print out EconMult messages or not (True/False)."
EMelOfEffortIsOneQ::usage        = "EMelOfEffortIsOneQ is a boolean switch to assume linear catch in effort or not (True/False)."

(*  PARAMETER VALUES:             *)
pEMbioUnits::usage              = "pEMbioUnits is a scalar that should be equal the length of the biomass vector EMbiomass."
pEMcatchability::usage          = "pEMcatchability is the default value of the EconMult option EMq specifying the catchability rates of vessel."
pEMelOfBiomass::usage           = "pEMelOfBiomass is the default value of the EconMult option EMbeta specifying the elasticities of biomasses."
pEMelOfEffort::usage            = "pEMelOfEffort is the default value of the EconMult option EMa specifying the elasticities of efforts."
pEMfixedCost::usage             = "pEMfixedCost is the default value of the EconMult option EMfc specifying the fixed costs."
pEMfleetCorrection::usage       = "pEMfleetCorrection is correcting EMfleet by multiplying the default values of the two matrixes. It needs therefore to have the same dimenison as EMfleet."
pEMfleetUnits::usage            = "pEMfleetUnits is a scalar that should be equal the length of the transposed EMfleet matrix."
pEMnIn::usage                   = "pEMnIn is a value (entry value) giving the persentage increase in fleet size in case of poitive contribution value when fleet dynamics are activated."
pEMnOut::usage                  = "pEMnOut is a value (exit value) giving the persentage decrease in fleet size in case of negative contribution value when fleet dynamics are activated."
pEMtargetedSpecies::usage       = "pEMtargetedSpecies is a scalar that should be equal the length of the tensor EMrelatedBioUnits."
pEMvariableCatchCost::usage     = "pEMvariableCatchCost is the default value of the EconMult option EMvcc specifying the variable unit cost of harvest."
pEMvariableEffortCost::usage    = "pEMvariableEffortCost is the default value of the EconMult option EMvec specifying the variable unit cost of effort."

EMdaysOfFishing::usage          = "EMdaysOfFishing is the default value of the EconMult option EMe specifying the number of fishing days."
EMfleet::usage                  = "EMfleet is the default value of the EconMult option EMn specifying the number of vessels."
EMrelatedBioUnits::usage        = "EMrelatedBioUnits is the default value of the EconMult option EMrbu specifying the related biomass units. Usually EMrelatedBioUnits is a tensor."
EMunitPrice::usage              = "EMunitPrice is the default value of the EconMult option EMp specifying the unit prices of harvest."
Fixed::usage                    = ""
ConstantF                       = ""

(*  MANAGEMENT MODULES:           *)
mEMquota::usage                 = "mEMquota is a vector of quota values in quota regulated fisheries defined by mEMquotaResolution."
mEMquotaInitial::usage
mEMquotaResolution::usage       = "mEMquotaResolution has the stucture {{sp1, {f1}}, {sp1, {f2}}, {sp2, {f1, f2}}, ...} indicating the quota relations between species (sp1, sp2, ...) and fleet groups (f1, f2, ...)."

(*  STORING UNITS:                *)
sEMbiomassVector::usage
sEMtargetCatchVector::usage
sEMbiomassCatchVector::usage

(* COLLECTING FROM STORING UNITS: *)
hEMyearTargetCatch::usage
hEMyearTargetBiomass::usage

(*  TEMPORARY VARIABLES:          *)
tEMetemp::usage
tEMtest::usage
tEMiter::usage

x::usage
a::usage
b::usage
q::usage
e::usage
n::usage
p::usage
vc::usage
ve::usage
fc::usage
Q::usage
t::usage


EMeInit::usage                  = "(In EconSimp)"
EMcollect::usage                = "(In EconSimp)"
EMcollectInit::usage            = "(In EconSimp)"

EMinterval::usage               = "EMinterval is a scalar giving the number of equal time period (time step of EconMult simulations) of a year."
EMyearPeriod::usage             = "EMyearPeriod is period no. i of EMinterval periods per year."
EMsetup::usage                  = "EMsetup[rel, fleetno, int] is the EconMult meta model setup. 'rel' is a list of related bio units, 'fleetno' is an integer giving the number of fleet groups and 'int' an integer giving the number of periods within a year (see EMinterval)."

(* DUMMY VARIABLES AND PARAMETERS  *)

BioUnit::usage                  = "BioUnit[i] is an EconMult dummy variable of EMbiomass[[i]]"
BioUnitAtTime::usage            = "BioUnitAtTime[i, t] is an EconMult dummy variable of EMbiomass[[i]] at YearPeriod t"
FleetUnit::usage                = "FleetUnit[i,j] is an EconMult dummy variable of EMFleet[[i,j]]"
DaysOfFishing::usage            = "DaysOfFishing[i,j] is an EconMult dummy variable of EMdaysOfFishing[[i,j]]"
VariableCatchCost::usage        = "VariableCatchCost[i,j] is an EconMult dummy variable ofEMvariableCatchCost[[i,j]]"
VariableEffortCost::usage       = "VariableEffortCost[i,j] is an EconMult dummy variable of EMvariableEffortCost[[i,j]]"
FixedCost::usage                = "FixedCost[i,j] is an EconMult dummy variable of EMfixedCost[[i,j]]"
ElasticityOfEffort::usage       = "ElasticityOfEffort[i,j,k] is an EconMult dummy variable of EMelasticityOfEffort[[i,j,k]]"
ElasticityOfBiomass::usage      = "ElasticityOfBiomass[i,j,k] is an EconMult dummy variable of EMelasticityOfBiomass[[i,j,k]]"
Catchability::usage             = "Catchability[i,j,k] is an EconMult dummy variable of EMcatchability[[i,j,k]]"
UnitPrice::usage                = "UnitPrice[i,j,k] is an EconMult dummy variable of EMunitPrice[[i,j,k]]"
Quota::usage                    = "Quota[i] is an EconMult dummy variable of quota value i."
YearPeriod::usage               = "YearPeriod[i] is an EconMult dummy variable of EMyearPeriod i."

EMnotation::usage               = "Variables:"<>ToString[
                                    TraditionalForm[
                                      TableForm[
                                      {
                                        {Subscript[x,i],":","Biomass of biomass unit i"},
                                        {Subscript[x,i,t],":","Biomass of biomass unit i at time t"},
                                        {Subscript[e, i, j],":","Number of fishing days, targeting species i, fleet j"},
                                        {Subscript[n, i, j],":","Number of vessels, targeting species i, fleet j"},
                                        {Subscript[t, i],":","Time period i (within a year)"}
                                      }
                                      ]
                                    ]
                                  ]<>"\n\nParameters:"<>ToString[
                                    TraditionalForm[
                                      TableForm[
                                      {
                                        {Subscript[a, i, j, k],":","Elasticity of Effort, targeting species i,\nfleet j, biomass unit k"},
                                        {Subscript[b, i, j, k],":","Elasticity of Biomass, targeting species i,\nfleet j, biomass unit k"},
                                        {Subscript[q, i, j, k],":","Catchability coefficient of targeting species i,\nfleet j, biomass unit k"},
                                        {Subscript[p, i, j, k],":","Unit price of catch of targeting species i,\nfleet j, biomass unit k"},
                                        {Subscript[vc, i, j],":","Unit cost of catch, targeting species i, fleet j"},
                                        {Subscript[ve, i, j],":","Unit cost of effort, targeting species i, fleet j"},
                                        {Subscript[fc, i, j],":","Fixed costs, targeting species i, fleet j"},
                                        {Subscript[Q, i],":","Quota no. i (defined by mEMquotaResoultion) "}
                                      }
                                      ]
                                    ]
                                  ]


Begin["`Private`"]

(* Referring to EconSimp (using AggMult) *)
EMshrimpCatch     = 0;		(* Initial Shrimp Catch *)
EMmaxCatchRate    = 0.8;	(* Maximum Allowable Catch Rate *)
(*  ....end of EconSimp input            *)

(*****************************************)
(*                                       *)
(*             Storing Units             *)
(*                                       *)
(*****************************************)


EMstoreUnitsInit := (

	sEMbiomassVector             = {};   (* Storing biomass information             *)
	sEMtargetCatchVector         = {};   (* Storing target catch information        *)
	sEMbiomassCatchVector        = {};   (* Storing biomass catch information       *)
	);


(*****************************************)
(*                                       *)
(*     Default boolean swith values      *)
(*                                       *)
(*****************************************)

bEMallcohorts      = True;	(* Include all cohorts                           *)
bEMtempCatch       = False;	(* Include temporary catch vector                *)
bEMcollect         = True;	(* Collect Model Variables                       *)
bEMherringIncluded = True;	(* Include Herring                               *)
bEMherringCatch    = False;     (* Exclude catch of herring                      *)
bEMuseTAC          = True;	(* Include Quota Management                      *)
bEMuseCM           = True;	(* Use Contribution Margin to Choose Effort      *)
bEMseparateCohorts = True;	(* Separate Between Cohorts                      *)
bEMseparateMature  = False;	(* Do not separate Between Matures and Immatures *)
bEMfleetDynamics   = True;      (* Include Fleet Dynamics                        *)
bEMstrongQuota     = False;     (* Do not use Strick Quota Management            *)
EMelOfEffortIsOneQ = False;	(* Set Elasticity of Effort equal 1              *)
EMprintOut         = True;      (* Switch On printout from EconMult              *)


(*****************************************)
(*                                       *)
(*           EconMult Options            *)
(*                                       *)
(*****************************************)

Options[EconMult] =

	{
	EMa             :> pEMelOfEffort,
	EMbeta			:> pEMelOfBiomass,
	EMx             :> EMbiomass,
	EMe             :> EMdaysOfFishing,
	EMq             :> pEMcatchability,
	EMn             :> EMfleet,
	EMrbu           :> EMrelatedBioUnits,
	EMp             :> EMunitPrice,
	EMfc            :> pEMfixedCost,
	EMvec           :> pEMvariableEffortCost,
	EMvcc           :> pEMvariableCatchCost,
	EMbiomassGrowth -> Automatic,
	EMquotaType     -> Fixed
	};


(*****************************************)
(*                                       *)
(*            Catch Functions            *)
(*                                       *)
(*****************************************)

EMdiscard         = 0;

EMvesselCatch[opts___] :=

	Module[
		{
		modlist0,
		modlist1,
		a, beta, x, e, q, n, rbu, p, fc, vec, vcc
		},
		{a, beta, x, e, q, n, rbu, p, fc, vec, vcc} =
		{
		EMa, EMbeta, EMx, EMe, EMq, EMn, EMrbu, EMp, EMfc, EMvec, EMvcc
		} /. {opts}  /. Options[EconMult];

		modlist0 =
			If[
				EMelOfEffortIsOneQ,
				q * Transpose[x^Transpose[beta, {2, 3, 1}], {3, 1, 2}] * e,
				q * Transpose[x^Transpose[beta, {2, 3, 1}], {3, 1, 2}] * e * n^(Transpose[a, {1, 2, 3}]-1)
			];

		modlist1 = Apply[Plus, Apply[Plus, Transpose[Transpose[modlist0 * n]], 1]];



                If[
                	MemberQ[Negative /@ (x - modlist1), True],
                	modlist0 =
                		modlist0 *
                		Table[
                			1 - Max[0, #] & /@ ((modlist1 - x)/(modlist1 /. {0 -> .001, 0. -> .001}))*EMmaxCatchRate,
                			{pEMtargetedSpecies},
                			{pEMfleetUnits}
                		]
                ];

                EMtempVesselCatch = modlist0
	];


EMvesselBioUnitCatch[opts___]:=
  Module[
    {a,beta,x,e,q,n,rbu,p,fc,vec,vcc},
    {a,beta,x,e,q,n,rbu,p,fc,vec,vcc}={EMa,EMbeta,EMx,EMe,EMq,EMn,EMrbu,EMp,EMfc,
    EMvec,EMvcc}/.{opts}/.Options[EconMult];

    If[bEMtempCatch,
      Apply[Plus, Transpose[EMtempVesselCatch, {3, 2, 1}], {2, 2}],
      Apply[Plus, Transpose[EMvesselCatch[opts], {3, 2, 1}], {2, 2}]
      ]
    ];


EMfleetCatch[opts___]:=
  Module[
    {a,beta,x,e,q,n,rbu,p,fc,vec,vcc},
    {a,beta,x,e,q,n,rbu,p,fc,vec,vcc}={EMa,EMbeta,EMx,EMe,EMq,EMn,EMrbu,EMp,EMfc,
    EMvec,EMvcc}/.{opts}/.Options[EconMult];

    If[bEMtempCatch,
      Transpose[Transpose[EMtempVesselCatch*n]],
      Transpose[Transpose[EMvesselCatch[opts]*n]]
      ]
    ];


EMfleetRelatedCatch[opts___]:=
  Module[
    {a,beta,x,e,q,n,rbu,p,fc,vec,vcc},
    {a,beta,x,e,q,n,rbu,p,fc,vec,vcc}={EMa,EMbeta,EMx,EMe,EMq,EMn,EMrbu,EMp,EMfc,
    EMvec,EMvcc}/.{opts}/.Options[EconMult];
    EMfleet*(Apply[Plus, Part[EMvesselBioUnitCatch[opts], #]] & /@ EMrelatedBioUnits)
    ];


EMfleetQuotaCatch[opts___]:=
	Module[
	  {a,beta,x,e,q,n,rbu,p,fc,vec,vcc,pp},

	  {a,beta,x,e,q,n,rbu,p,fc,vec,vcc}={EMa,EMbeta,EMx,EMe,EMq,EMn,EMrbu,EMp,EMfc,
	  EMvec,EMvcc}/.{opts}/.Options[EconMult];

	  If[bEMtempCatch,EMvesselCatch[opts]];

	  pp = {#[[2]], EMrelatedBioUnits[[#[[1]]]]} & /@ (mEMquotaResolution /. {All->Range[pEMfleetUnits]});
	  Table[
	    Apply[Plus, Flatten[Part[(Apply[Plus, #] & /@ # & /@ Transpose[EMfleetCatch[opts], {3, 1, 2}]), Sequence @@ pp[[i]]]]],
	    {i, Length[pp]}
	  ]
	];


EMtotalQuotaCatch[opts___]:=
	Module[
	  {a,beta,x,e,q,n,rbu,p,fc,vec,vcc, etemp,
	  l1 = Transpose[mEMquotaResolution][[1]],
	  l2
	  },

	  {a,beta,x,e,q,n,rbu,p,fc,vec,vcc}={EMa,EMbeta,EMx,EMe,EMq,EMn,EMrbu,EMp,EMfc,EMvec,EMvcc}/.{opts}/.Options[EconMult];

	  etemp = e;

          If[bEMtempCatch,EMvesselCatch[EMe :> etemp]];

          EMeffortCorrection[EMe :> etemp];

	  l2 = EMfleetQuotaCatch[EMe :> (etemp*EMquotaCorrection)];

	  If[
	    Length[l2] === Length[l1],
	    Apply[Plus, Transpose[#][[2]] & /@ Split[Sort[Table[{l1[[i]], l2[[i]]}, {i, Length[l1]}]], #1[[1]] === #2[[1]] &], 1]
	  ]
	];


EMfleetRelatedQuotaCatch[opts___]:=
  Module[
    {a,beta,x,e,q,n,rbu,p,fc,vec,vcc},
    {a,beta,x,e,q,n,rbu,p,fc,vec,vcc}={EMa,EMbeta,EMx,EMe,EMq,EMn,EMrbu,EMp,EMfc,
    EMvec,EMvcc}/.{opts}/.Options[EconMult];

    If[bEMtempCatch,EMvesselCatch[opts]];

    Transpose[
       Table[
          Apply[Plus, Part[#, EMrelatedBioUnits[[jj]]]] & /@ EMfleetCatch[opts][[ii]],
          {ii, Length[EMrelatedBioUnits]},
          {jj, Length[EMrelatedBioUnits]}
       ],
       {1, 3, 2}
    ]
  ];


EMbiomassCatch[opts___]:=
  Module[
    {a,beta,x,e,q,n,rbu,p,fc,vec,vcc},
    {a,beta,x,e,q,n,rbu,p,fc,vec,vcc}={EMa,EMbeta,EMx,EMe,EMq,EMn,EMrbu,EMp,EMfc,
    EMvec,EMvcc}/.{opts}/.Options[EconMult];
    If[bEMtempCatch,
      Apply[Plus,Apply[Plus,Transpose[Transpose[EMtempVesselCatch*n]],1]],
      Apply[Plus,Apply[Plus,EMfleetCatch[opts],1]]
      ]
    ];


EMfisheryCatch[opts___]:=
  Module[
    {a,beta,x,e,q,n,rbu,p,fc,vec,vcc},
    {a,beta,x,e,q,n,rbu,p,fc,vec,vcc}={EMa,EMbeta,EMx,EMe,EMq,EMn,EMrbu,EMp,EMfc,
    EMvec,EMvcc}/.{opts}/.Options[EconMult];
    If[bEMtempCatch,
      Apply[Plus,Transpose[Transpose[Transpose[EMtempVesselCatch*n]],{
            1,3,2}],1],
      Apply[Plus,Transpose[EMfleetCatch[opts],{1,3,2}],1]
      ]
    ];


EMtargetCatch[opts___]:=
  Module[
    {a,beta,x,e,q,n,rbu,p,fc,vec,vcc,modlist},
    {a,beta,x,e,q,n,rbu,p,fc,vec,vcc}={EMa,EMbeta,EMx,EMe,EMq,EMn,EMrbu,EMp,EMfc,
    EMvec,EMvcc}/.{opts}/.Options[EconMult];
     If[bEMtempCatch,
      Apply[
      	Plus,
      	Apply[
      		Plus,
      		Apply[
      			Plus,
      			Transpose[
      				Transpose[EMtempVesselCatch * n]
      			],
      			1
      		]
      	][[#]]& /@rbu, 1
      ],
      modlist = EMbiomassCatch[opts];
      Apply[Plus,modlist[[#]]& /@ rbu, 1]
     ]
    ];


(*****************************************)
(*                                       *)
(*               Economics               *)
(*                                       *)
(*****************************************)


(*****************************************)
(*               Revenue                 *)
(*****************************************)

EMvesselRevenue[opts___]:=
  Module[
    {a,beta,x,e,q,n,rbu,p,fc,vec,vcc},
    {a,beta,x,e,q,n,rbu,p,fc,vec,vcc}={EMa,EMbeta,EMx,EMe,EMq,EMn,EMrbu,EMp,EMfc,
    EMvec,EMvcc}/.{opts}/.Options[EconMult];
    p*EMvesselCatch[opts]
    ];


EMfisheryRevenue[opts___]:=
  Module[
    {a,beta,x,e,q,n,rbu,p,fc,vec,vcc},
    {a,beta,x,e,q,n,rbu,p,fc,vec,vcc}={EMa,EMbeta,EMx,EMe,EMq,EMn,EMrbu,EMp,EMfc,
    EMvec,EMvcc}/.{opts}/.Options[EconMult];
    Transpose[Transpose[EMvesselRevenue[opts]*n]]
    ];


EMbiomassRevenue[opts___]:=
  Module[
    {a,beta,x,e,q,n,rbu,p,fc,vec,vcc},
    {a,beta,x,e,q,n,rbu,p,fc,vec,vcc}={EMa,EMbeta,EMx,EMe,EMq,EMn,EMrbu,EMp,EMfc,
    EMvec,EMvcc}/.{opts}/.Options[EconMult];
    Apply[Plus,Transpose[EMfisheryRevenue[opts],{3,2,1}],2]
    ];


EMfleetRevenue[opts___]:=
  Module[
    {a,beta,x,e,q,n,rbu,p,fc,vec,vcc},
    {a,beta,x,e,q,n,rbu,p,fc,vec,vcc}={EMa,EMbeta,EMx,EMe,EMq,EMn,EMrbu,EMp,EMfc,
    EMvec,EMvcc}/.{opts}/.Options[EconMult];
    Apply[Plus,Transpose[EMfisheryRevenue[opts],{1,3,2}],1]
    ];


(*****************************************)
(*         Contribution Margin           *)
(*****************************************)

EMvesselCM[opts___]:=
  Module[
    {a,beta,x,e,q,n,rbu,p,fc,vec,vcc},
    {a,beta,x,e,q,n,rbu,p,fc,vec,vcc}={EMa,EMbeta,EMx,EMe,EMq,EMn,EMrbu,EMp,EMfc,
    EMvec,EMvcc}/.{opts}/.Options[EconMult];
    Apply[Plus,Transpose[EMvesselRevenue[opts],{1,3,2}],1]-vec*e-
            vcc*Apply[Plus,Transpose[EMvesselCatch[EMq :> EMcatchabilityDiscard[0], opts],{1,3,2}],1]
    ];


EMfisheryCM[opts___]:=
  Module[
    {a,beta,x,e,q,n,rbu,p,fc,vec,vcc},
    {a,beta,x,e,q,n,rbu,p,fc,vec,vcc}={EMa,EMbeta,EMx,EMe,EMq,EMn,EMrbu,EMp,EMfc,
    EMvec,EMvcc}/.{opts}/.Options[EconMult];
    Transpose[Transpose[EMvesselCM[opts]*n]]
    ];


EMfleetCM[opts___]:=
  Module[
    {a,beta,x,e,q,n,rbu,p,fc,vec,vcc},
    {a,beta,x,e,q,n,rbu,p,fc,vec,vcc}={EMa,EMbeta,EMx,EMe,EMq,EMn,EMrbu,EMp,EMfc,
    EMvec,EMvcc}/.{opts}/.Options[EconMult];
    Apply[Plus,Transpose[EMfisheryCM[opts]],1]
    ];


(*****************************************)
(*             Variable Cost             *)
(*****************************************)

EMvesselVC[opts___]:=
  Module[
    {a,beta,x,e,q,n,rbu,p,fc,vec,vcc},
    {a,beta,x,e,q,n,rbu,p,fc,vec,vcc}={EMa,EMbeta,EMx,EMe,EMq,EMn,EMrbu,EMp,EMfc,
    EMvec,EMvcc}/.{opts}/.Options[EconMult];
    vec*e+vcc*Apply[Plus,Transpose[EMvesselCatch[EMq :> EMcatchabilityDiscard[0], opts],{1,3,2}],1]
    ];


EMfisheryVC[opts___]:=
  Module[
    {a,beta,x,e,q,n,rbu,p,fc,vec,vcc},
    {a,beta,x,e,q,n,rbu,p,fc,vec,vcc}={EMa,EMbeta,EMx,EMe,EMq,EMn,EMrbu,EMp,EMfc,
    EMvec,EMvcc}/.{opts}/.Options[EconMult];
    Transpose[Transpose[EMvesselVC[opts]*n]]
    ];


EMfleetVC[opts___]:=
  Module[
    {a,beta,x,e,q,n,rbu,p,fc,vec,vcc},
    {a,beta,x,e,q,n,rbu,p,fc,vec,vcc}={EMa,EMbeta,EMx,EMe,EMq,EMn,EMrbu,EMp,EMfc,
    EMvec,EMvcc}/.{opts}/.Options[EconMult];
    Apply[Plus,Transpose[EMfisheryVC[opts]],1]
    ];


(*****************************************)
(*             Resource Rent             *)
(*****************************************)

EMvesselProfit[opts___]:=
  Module[
    {a,beta,x,e,q,n,rbu,p,fc,vec,vcc},
    {a,beta,x,e,q,n,rbu,p,fc,vec,vcc}={EMa,EMbeta,EMx,EMe,EMq,EMn,EMrbu,EMp,EMfc,
    EMvec,EMvcc}/.{opts}/.Options[EconMult];
    Apply[
        Plus,Transpose[EMvesselRevenue[opts],{1,3,2}],1]-fc-EMvesselVC[opts]
    ];



EMfisheryProfit[opts___]:=
  Module[
    {a,beta,x,e,q,n,rbu,p,fc,vec,vcc},
    {a,beta,x,e,q,n,rbu,p,fc,vec,vcc}={EMa,EMbeta,EMx,EMe,EMq,EMn,EMrbu,EMp,EMfc,
    EMvec,EMvcc}/.{opts}/.Options[EconMult];
    Transpose[Transpose[EMvesselProfit[opts]*n]]
    ];



EMfleetProfit[opts___]:=
  Module[
    {a,beta,x,e,q,n,rbu,p,fc,vec,vcc},
    {a,beta,x,e,q,n,rbu,p,fc,vec,vcc}={EMa,EMbeta,EMx,EMe,EMq,EMn,EMrbu,EMp,EMfc,
    EMvec,EMvcc}/.{opts}/.Options[EconMult];
    Apply[Plus,Transpose[EMfisheryProfit[opts]],1]
    ];

EMvesselDiscardProfit[opts___]:=
  Module[
    {a,beta,x,e,q,n,rbu,p,fc,vec,vcc},
    {a,beta,x,e,q,n,rbu,p,fc,vec,vcc}={EMa,EMbeta,EMx,EMe,EMq,EMn,EMrbu,EMp,EMfc,
    EMvec,EMvcc}/.{opts}/.Options[EconMult];
    Apply[
        Plus,Transpose[EMdiscardMatrix[EMdiscard]*EMvesselRevenue[opts],{1,3,2}],1]-fc-EMvesselVC[opts]
    ];

EMfisheryDiscardProfit[opts___]:=
  Module[
    {a,beta,x,e,q,n,rbu,p,fc,vec,vcc},
    {a,beta,x,e,q,n,rbu,p,fc,vec,vcc}={EMa,EMbeta,EMx,EMe,EMq,EMn,EMrbu,EMp,EMfc,
    EMvec,EMvcc}/.{opts}/.Options[EconMult];
    Transpose[Transpose[EMvesselDiscardProfit[opts]*n]]
    ];

EMfleetDiscardProfit[opts___]:=
  Module[
    {a,beta,x,e,q,n,rbu,p,fc,vec,vcc},
    {a,beta,x,e,q,n,rbu,p,fc,vec,vcc}={EMa,EMbeta,EMx,EMe,EMq,EMn,EMrbu,EMp,EMfc,
    EMvec,EMvcc}/.{opts}/.Options[EconMult];
    Apply[Plus,Transpose[EMfisheryDiscardProfit[opts]],1]
    ];


EMvesselDiscardCM[opts___] := EMvesselDiscardProfit[opts] + pEMfixedCost;


(*****************************************)
(*                                       *)
(*           Effort Adjustments          *)
(*                                       *)
(*****************************************)

(*****************************************)
(*                Discard                *)
(*****************************************)

(*
EMcatchabilityDiscard[i_?IntegerQ] :=
	Module[{ii, transq},
	  If[Abs[i] > pEMbioUnits, ii = pEMbioUnits, ii = Abs[i]];
	  transq = Transpose[pEMcatchability, {3, 2, 1}];
	  Table[
	    transq[[j]] = (# /. {_ -> 0} & /@ # & /@ transq[[j]]),
	    Evaluate[
	      If[
	        Positive[i],
	        {j, ii},
	        {j,pEMbioUnits, 1 + pEMbioUnits - ii, -1}
	      ]
	    ]
	  ];
	  Transpose[transq, {3, 2, 1}]
        ];

EMcatchabilityDiscard[{0, 0}] := EMcatchabilityDiscard[0];

EMcatchabilityDiscard[{n1_, n2_}] :=
	Module[
	  {transq, a = n1, b = n2},
	  If[a > b, {a, b} = {b, a}];
	  If[a <= b && b <= pEMbioUnits && a >= 0,
            transq = Transpose[pEMcatchability, {3, 2, 1}];
            Table[
              transq[[j]] = (# /. {_ -> 0} & /@ # & /@ transq[[j]]),
              {j, a, b}
            ]
	  ];
	  Transpose[transq, {3, 2, 1}]
	];

*)


EMcatchabilityDiscard[m_] := pEMcatchability * EMdiscardMatrix[m];


EMdiscardMatrix[i_?IntegerQ] :=
	Module[{mat=(1 & /@ # & /@ # & /@ pEMcatchability),ii, transq},
	  If[Abs[i] > pEMbioUnits, ii = pEMbioUnits, ii = Abs[i]];
          transq = Transpose[mat, {3, 2, 1}];
	  Table[
	    transq[[j]] = (# /. {_ -> 0} & /@ # & /@ transq[[j]]),
	    Evaluate[
	      If[
	        Positive[i],
	        {j, ii},
	        {j,pEMbioUnits, 1 + pEMbioUnits - ii, -1}
	      ]
	    ]
	  ];
	  Transpose[transq, {3, 2, 1}]
        ];

EMdiscardMatrix[{0, 0}] := EMdiscardMatrix[0];

EMdiscardMatrix[{n1_?NumberQ, n2_?NumberQ}] :=

	Module[
	  {mat=(1 & /@ # & /@ # & /@ pEMcatchability),transq, a = n1, b = n2},
	  If[a > b, {a, b} = {b, a}];
	  If[a <= b && b <= pEMbioUnits && a >= 0,
            transq = Transpose[mat, {3, 2, 1}];
            Table[
              transq[[j]] = (# /. {_ -> 0} & /@ # & /@ transq[[j]]),
              {j, a, b}
            ]
	  ];
	  Transpose[transq, {3, 2, 1}]
	];


EMdiscardMatrix[{ll__?ListQ}] :=

	  Times @@ (EMdiscardMatrix[#] & /@ {ll});



(*****************************************)
(*        Fishing Days Correction        *)
(*****************************************)

EMeffortCorrection[opts___] :=

	Module[
		{a,beta,x,e,q,n,rbu,p,fc,vec,vcc,typetac},

		{a,beta,x,e,q,n,rbu,p,fc,vec,vcc,typetac} =
		{EMa,EMbeta,EMx,EMe,EMq,EMn,EMrbu,EMp,EMfc,EMvec,EMvcc,EMquotaType} /. {opts} /. Options[EconMult];

	        EMtempDaysOfFishing = tEMetemp;

		(*  Adjusting numbers of fishing days according to contribution margins   *)

                If[
		  bEMuseCM,
		  EMvesselCatch[EMe :> EMtempDaysOfFishing];
		  EMtempDaysOfFishing =
		    ReplacePart[
		      EMdaysOfFishing,
		      0,
		      Position[Negative /@ EMvesselCM[EMe :> EMtempDaysOfFishing], True]
		    ]
	        ];


	        (*  Adjusting numbers of fishing days according to quota values  *)

	        If[
		  bEMuseTAC,
		  EMvesselCatch[EMe :> EMtempDaysOfFishing];

		  asp =
		    Split[
		      Sort[
		        {
	                Part[
		          mEMquota/(EMfleetQuotaCatch[EMe :> EMtempDaysOfFishing] /. {0 -> 1, 0. -> 1}),
		          #
		        ],
		        Sequence @@ Part[mEMquotaResolution, #]
		        } & /@
		        Flatten[
		          Position[
		            Negative /@ (mEMquota - EMfleetQuotaCatch[EMe :> EMtempDaysOfFishing]),
		            True
		          ]
		        ],
		        #1[[2]] < #2[[2]] &
		      ],
		      #1[[2]] == #2[[2]] &
		    ] /. {All -> Range[pEMfleetUnits]};

		   asp1 = (
		     Transpose /@
		       Split[
		         #1, #1[[1]] === #2[[1]] &] & /@ (
			   Sort /@ (Join @@ # & /@ (
		             Table[
		               {#[[3, i]], #[[1]]},
		               {i, Length[#[[3]]]}
		             ] & /@ # & /@ asp
		             ))
		          )
		      );

                   asp2 = (Plus @@ # & /@ {#[[1, 1]], #[[2]]} & /@ # & /@ asp1);
                   EMquotaCorrection = Table[1, {pEMtargetedSpecies}, {pEMfleetUnits}];
                   Table[((EMquotaCorrection[[i, #[[1]]]] = Min[1, #[[2]]]) & /@ asp2[[i]]), {i, Length[asp2]}];
                   EMquotaCorrection = Chop[(Min[1, Max[#, 0]] & /@ #) & /@ EMquotaCorrection];
		   EMtempDaysOfFishing = EMquotaCorrection * EMtempDaysOfFishing;

	        ];  (* end if loop  useTAC  *)

	        If[
	          !bEMuseTAC && !bEMuseCM,
	          EMvesselCatch[EMe :> EMtempDaysOfFishing]
	        ];

		tEMetemp = EMtempDaysOfFishing
	  ];


(*****************************************)
(*         Fleet Unit Correction         *)
(*****************************************)


EMfleetCorrection[opts___] :=

   	Module[
   		{a,beta,x,e,q,n,rbu,p,fc,vec,vcc},

   		{a,beta,x,e,q,n,rbu,p,fc,vec,vcc} =
   		{EMa,EMbeta,EMx,EMe,EMq,EMn,EMrbu,EMp,EMfc,EMvec,EMvcc} /. {opts} /. Options[EconMult];


                If[bEMtempCatch, EMvesselCatch[opts]];

                If[ListQ[pEMnOut],

                  (pEMfleetCorrection[[#[[1]], #[[2]]]] =
		     pEMfleetCorrection[[#[[1]], #[[2]]]] * (1-pEMnOut[[#[[2]]]]))& /@ Position[Negative /@ EMvesselProfit[opts], True],


	          (pEMfleetCorrection[[#[[1]], #[[2]]]] =
	             pEMfleetCorrection[[#[[1]], #[[2]]]] * (1-pEMnOut))& /@ Position[Negative /@ EMvesselProfit[opts], True]
	        ];

	        If[ListQ[pEMnIn],

    	          (pEMfleetCorrection[[#[[1]], #[[2]]]] =
    	             pEMfleetCorrection[[#[[1]], #[[2]]]] * (1+pEMnIn[[#[[2]]]]))& /@ Position[Positive /@ EMvesselProfit[opts], True],

                  (pEMfleetCorrection[[#[[1]], #[[2]]]] =
    	            pEMfleetCorrection[[#[[1]], #[[2]]]] * (1+pEMnIn))& /@ Position[Positive /@ EMvesselProfit[opts], True]
    	        ];


    	        pEMfleetCorrection
         ];





EMfleetCorrection[opts___] :=

   	Module[
   		{a,beta,x,e,q,n,rbu,p,fc,vec,vcc,typetac},

   		{a,beta,x,e,q,n,rbu,p,fc,vec,vcc,typetac} =
   		{EMa,EMbeta,EMx,EMe,EMq,EMn,EMrbu,EMp,EMfc,EMvec,EMvcc,EMquotaType} /. {opts} /. Options[EconMult];


                If[bEMtempCatch,EMvesselCatch[opts]];

	        (pEMfleetCorrection[[#[[1]], #[[2]]]] =
	            pEMfleetCorrection[[#[[1]], #[[2]]]] * (1-pEMnOut))& /@ Position[Negative /@ EMvesselProfit[opts], True];

    	        (pEMfleetCorrection[[#[[1]], #[[2]]]] =
    	            pEMfleetCorrection[[#[[1]], #[[2]]]] * (1+pEMnIn))& /@ Position[Positive /@ EMvesselProfit[opts], True];

    	        pEMfleetCorrection
         ];



(*****************************************)
(*                                       *)
(*  Checking Consistency in Dimensions   *)
(*                                       *)
(*****************************************)

EMdimensionsQ[opts___] :=

	Module[
		{a,beta,x,e,q,n,rbu,p,fc,vec,vcc},

		{a,beta,x,e,q,n,rbu,p,fc,vec,vcc} =
		{EMa,EMbeta,EMx,EMe,EMq,EMn,EMrbu,EMp,EMfc,EMvec,EMvcc} /. {opts} /. Options[EconMult];

		pEMbioUnits=Length[x];
		pEMfleetUnits=Length[n//Transpose];
		pEMtargetedSpecies=Length[rbu];

		If[
			!Dimensions[a] === {pEMtargetedSpecies,pEMfleetUnits,pEMbioUnits},
			EMerrorCode=1;
			Print["a is out of range. Dimension ", {pEMtargetedSpecies, pEMfleetUnits,pEMbioUnits}," was expected."]
		];

		If[
			!Dimensions[beta]==={pEMtargetedSpecies,pEMfleetUnits,pEMbioUnits},
			EMerrorCode=1;
			Print["beta is out of range. Dimension ",{pEMtargetedSpecies, pEMfleetUnits,pEMbioUnits}," was expected."]
		];

		If[
			!Dimensions[e]==={pEMtargetedSpecies,pEMfleetUnits},
			EMerrorCode=1;
			Print["e is out of range. \ Dimension ",{pEMtargetedSpecies,pEMfleetUnits}," was expected."]
		];

		If[
			!Dimensions[q]==={pEMtargetedSpecies,pEMfleetUnits,pEMbioUnits},
			EMerrorCode=1;
			Print["q is out of range. Dimension ",{pEMtargetedSpecies,pEMfleetUnits, pEMbioUnits}," was expected."]
		];

		If[
			!Sort[Flatten[rbu]]===Range[pEMbioUnits],
			EMerrorCode=1;
			Print["rbu is out of range. \ ",pEMbioUnits," bio units were expected."]
		];

		If[
			!Dimensions[p]==={pEMtargetedSpecies,pEMfleetUnits,pEMbioUnits},
			EMerrorCode=1;
			Print["p is out of range. \ Dimension ",{pEMtargetedSpecies,pEMfleetUnits,pEMbioUnits}," was expected."]
		];

		If[
			!Dimensions[fc]==={pEMtargetedSpecies,pEMfleetUnits},
			EMerrorCode=1;
			Print["fc is out of range. \ Dimension ",{pEMtargetedSpecies,pEMfleetUnits}," was expected."]
		];

		If[
			!Dimensions[vec]==={pEMtargetedSpecies,pEMfleetUnits},
			EMerrorCode=1;
			Print["vec is out of range. Dimension ",{pEMtargetedSpecies,pEMfleetUnits}," was expected."]
		];

		If[
			!Dimensions[vcc]==={pEMtargetedSpecies,pEMfleetUnits},
			EMerrorCode=1;
			Print["vcc is out of range. \ Dimension ",{pEMtargetedSpecies,pEMfleetUnits}," was expected."]
		];

		If[
			EMprintOut,
			If[
				!EMerrorCode===1,
				Print["--- Harvest and Economic \ parameters are checked out to be consistent in dimensions ---"]
			]
		];

	];



EconMult[opts___] :=

	Module[
		{a,beta,x,e,q,n,rbu,p,fc,vec,vcc,gm,typetac},

		{a,beta,x,e,q,n,rbu,p,fc,vec,vcc,gm,typetac} =
		{EMa,EMbeta,EMx,EMe,EMq,EMn,EMrbu,EMp,EMfc,EMvec,EMvcc,EMbiomassGrowth,EMquotaType} /. {opts} /. Options[EconMult];

		  If[
		    First[EMyearPeriod] === YearPeriod[1],
		    mEMquota            =   mEMquotaInitial;
		    EMtempDaysOfFishing =   EMdaysOfFishing;
		    tEMetemp            =   EMdaysOfFishing,

		    tEMetemp            =   EMtempDaysOfFishing
		  ];

		  EMeffortCorrection[EMe :> tEMetemp];
		  EMyearPeriod = RotateLeft[EMyearPeriod];
		  EMvesselCatch[EMe :> tEMetemp];

		  If[typetac===Fixed,     mEMquota = mEMquota - EMfleetQuotaCatch[EMe :> tEMetemp]];
		  If[typetac===ConstantF, mEMquota = mEMquota - EMfleetQuotaCatch[EMe :> tEMetemp]];


		  AppendTo[sEMbiomassVector, Apply[Plus, #] & /@ (EMbiomass[[#]] & /@ EMrelatedBioUnits)];
		  AppendTo[sEMtargetCatchVector, EMtargetCatch[EMe :> tEMetemp]];
		  AppendTo[sEMbiomassCatchVector, EMbiomassCatch[EMe :> tEMetemp]];

		  If[
		    gm===Automatic,
		    run++;
		    Table[BioUnit[i] = BioUnitAtTime[i, run], {i, Length[EMbiomass]}],
		    Evaluate[gm]
		  ]

	];


EMsetup[relbio_?ListQ, fleetno_?IntegerQ, int_?IntegerQ] :=

	Module[
		{relbio0 = Sort[Flatten[relbio]], bu},
		EMrelatedBioUnits     = relbio;
		pEMfleetUnits         = fleetno;
		EMinterval            = int;
		EMstoreUnitsInit;
		run=0;
		If[Range[Length[relbio0]] === relbio0,
			pEMtargetedSpecies    = Length[EMrelatedBioUnits];
			EMbiomass             = Table[BioUnit[i],{i,Length[relbio0]}];
			pEMbioUnits           = Length[EMbiomass];
			EMfleet               = Table[FleetUnit[i,j],{i,pEMtargetedSpecies},{j,pEMfleetUnits}];
			pEMelOfEffort         = Table[ElasticityOfEffort[i,j,k],{i,pEMtargetedSpecies}, {j,pEMfleetUnits}, {k,pEMbioUnits}];
			pEMelOfBiomass        = Table[ElasticityOfBiomass[i,j,k],{i,pEMtargetedSpecies}, {j,pEMfleetUnits}, {k,pEMbioUnits}];
			EMdaysOfFishing       = Table[DaysOfFishing[i,j],{i,pEMtargetedSpecies}, {j,pEMfleetUnits}];
			pEMcatchability       = Table[Catchability[i,j,k],{i,pEMtargetedSpecies}, {j,pEMfleetUnits}, {k,pEMbioUnits}];
			EMunitPrice           = Table[UnitPrice[i,j,k],{i,pEMtargetedSpecies}, {j,pEMfleetUnits}, {k,pEMbioUnits}];
			pEMfixedCost          = Table[FixedCost[i,j],{i,pEMtargetedSpecies}, {j,pEMfleetUnits}];
			pEMvariableEffortCost = Table[VariableEffortCost[i,j],{i,pEMtargetedSpecies}, {j,pEMfleetUnits}];
			pEMvariableCatchCost  = Table[VariableCatchCost[i,j],{i,pEMtargetedSpecies}, {j,pEMfleetUnits}];
			mEMquotaResolution    = Table[{i, Range[pEMfleetUnits]}, {i, pEMtargetedSpecies}];
			mEMquotaInitial       = Table[Quota[i], {i, Length[mEMquotaResolution]}];
			EMyearPeriod          = Table[YearPeriod[i], {i, EMinterval}];
			tEMetemp              = EMdaysOfFishing;
			pEMfleetCorrection    = Table[1,{i,pEMtargetedSpecies},{j,pEMfleetUnits}];
			pEMnOut               = .1;
			pEMnIn                = .1;
			bu                    = Length[Flatten[EMrelatedBioUnits]];
			Print["EconMult structure:\n",
			        pEMtargetedSpecies, " targeted species (pEMtargetedSpecies)\n",
				bu, " biounit"<>If[bu>1,"s",""]<>" (EMrelatedBioUnits)\n",
				fleetno, " fleet group"<>If[fleetno>1,"s",""]<>" (pEMfleetUnits)\n",
				int, " simulation period"<>If[int>1,"s",""]<>" per year (EMinterval)"
			],
			Print["Error in ", relbio, ". Combinations of ", {Range[Length[relbio0]]}, " was expected."]
		]
	];


EMsetup[] :=
	Module[
		{},
		If[
			!ListQ[x],
			Print["No biomass vector was found. Assuming single value"];
			EMrelatedBioUnits = {{1}}
		];
		If[
			!ListQ[EMfleet],
			Print["No fleet vector was found. Assuming single value"];
			pEMfleetUnits = 1
		];
		If[
			!ValueQ[EMinterval],
			Print["No period interval was found. Assuming single value"];
			EMinterval = 1
		];

		EMsetup[EMrelatedBioUnits, pEMfleetUnits, EMinterval]
	];


EMsetup[fleetno_:1, int_:1] := EMsetup[{{1}}, fleetno, int];

hEMyearTargetCatch   := Apply[Plus, #] & /@ # & /@ Transpose[Partition[sEMtargetCatchVector, EMinterval], {1, 3, 2}];

hEMyearTargetBiomass := Apply[Plus, #] & /@ # & /@ Transpose[Partition[sEMbiomassVector, EMinterval], {1, 3, 2}];


EMnotation[exp_] :=

	Module[{},
	  TraditionalForm[
	    MatrixForm[
	      exp /. {
	        BioUnit[i_]                     -> Subscript[x, i],
	        BioUnitAtTime[i_,j_]            -> Subscript[x, i, j],
	        ElasticityOfEffort[i_, j_, k_]  -> Subscript[a, i, j, k],
	        ElasticityOfBiomass[i_, j_, k_] -> Subscript[b, i, j, k],
	        Catchability[i_, j_, k_]        -> Subscript[q, i, j, k],
	        DaysOfFishing[i_, j_]           -> Subscript[e, i, j],
	        FleetUnit[i_, j_]               -> Subscript[n, i, j],
	        UnitPrice[i_, j_, k_]           -> Subscript[p, i, j, k],
	        VariableCatchCost[i_, j_]       -> Subscript[vc, i, j],
	        VariableEffortCost[i_, j_]      -> Subscript[ve, i, j],
	        FixedCost[i_, j_]               -> Subscript[fc, i, j],
	        Quota[i_]                       -> Subscript[Q, i],
	        YearPeriod[i_]                  -> Subscript[t, i]
	      }
	    ]
	  ]
	];



End[]


EndPackage[]
