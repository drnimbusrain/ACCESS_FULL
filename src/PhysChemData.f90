!======================================================================================================================!
!                                                                                                                      !
!     Program:      ACCESS                                                                                             !
!                   Atmospheric Chemistry and Canopy Exchange Simulation System                                        !
!                   Full BVOC chemistry version                                                                        !
!                                                                                                                      !
!     Version:      3.0.0                                                                                              !
!                                                                                                                      !
!======================================================================================================================!
!                                                                                                                      !
!     Contact:      Rick D. Saylor, PhD                                                                                !
!                   Physical Scientist                                                                                 !
!                   U. S. Department of Commerce                                                                       !
!                   National Oceanic and Atmospheric Administration                                                    !
!                   Air Resources Laboratory                                                                           !
!                   Atmospheric Turbulence and Diffusion Division                                                      !
!                   456 S. Illinois Ave                                                                                !
!                   Oak Ridge, TN 37830                                                                                !
!                   email: Rick.Saylor@noaa.gov                                                                        !
!                                                                                                                      !
!**********************************************************************************************************************!
!                   NOTE: See Legal Notice in Main.f90                                                                 !
!**********************************************************************************************************************!
!                                                                                                                      !
!     Module:       PhysChemData                                                                                       !
!                                                                                                                      !
!     Description:  physical chemistry data                                                                            !
!                                                                                                                      !
!======================================================================================================================!
module PhysChemData
  use GlobalData
  implicit none

  private SetMolecDiffSTP, SetEffHenrysLawCoeffs, SetReactivityParams, SetMolecMass
  public SetPhysChemData, &
         MolecDiff, mdiffh2o, &
         esat_wp, esat, qsat, &
         desdt, des2dt2, lambda, &
         EffHenrysLawCoeff, ReactivityParam

contains

!**********************************************************************************************************************!
! subroutine SetPhysChemData - initialization routine for physical chemical
!                              data
!**********************************************************************************************************************!
subroutine SetPhysChemData()

  ! initialize molecular diffusivities at STP
  call SetMolecDiffSTP()

  ! effective Henry's Law coefficients
  call SetEffHenrysLawCoeffs()

  ! reactivity parameter
  call SetReactivityParams()

  ! molecular mass
  call SetMolecMass()

end subroutine SetPhysChemData

!**********************************************************************************************************************!
! subroutine SetMolecDiffSTP - set molecular diffusivity data (cm2/s) for
!                              all species at O degC and 1 atm (Nguyen et al. 2015. PNAS 112 (5) E392-E401)
!**********************************************************************************************************************!
subroutine SetMolecDiffSTP()
  integer(kind=i4) :: l

  ! first set everything to default
  do l=1,ntotal
    mdiffstp(l)=mdiffstp_default
  end do

  ! now specify values for species with reliable data
  mdiffstp(iNO)         = 0.1802_dp
  mdiffstp(iNO2)        = 0.1361_dp
  mdiffstp(iO3)         = 0.1444_dp
  mdiffstp(iHONO)       = 0.1349_dp
  mdiffstp(iHNO4)       = 0.1041_dp    ! set same as HNO3
  mdiffstp(iHNO3)       = 0.1041_dp
  mdiffstp(iN2O5)       = 0.0808_dp
  mdiffstp(iCO)         = 0.1807_dp
  mdiffstp(iH2O2)       = 0.1300_dp
  mdiffstp(iCH4)        = 0.1952_dp
  mdiffstp(iMO2)        = 0.1297_dp
  mdiffstp(iOP1)        = 0.1200_dp
  mdiffstp(iMOH)        = 0.1297_dp 
  mdiffstp(iNO3)        = 0.1153_dp
  mdiffstp(iO3P)        = 0.2773_dp
  mdiffstp(iO1D)        = 0.2773_dp
  mdiffstp(iHO)         = 0.2543_dp
  mdiffstp(iHO2)        = 0.2000_dp
  mdiffstp(iORA1)       = 0.1340_dp
  mdiffstp(iHAC)        = 0.1060_dp
  mdiffstp(iPAA)        = 0.1040_dp
  mdiffstp(iDHMOB)      = 0.0892_dp
  mdiffstp(iHPALD)      = 0.0845_dp
  mdiffstp(iISHP)       = 0.0837_dp
  mdiffstp(iIEPOX)      = 0.0837_dp
  mdiffstp(iPROPNN)     = 0.0834_dp
  mdiffstp(iISOPNB)     = 0.0750_dp
  mdiffstp(iISOPND)     = 0.0750_dp
  mdiffstp(iMACRN)      = 0.0745_dp
  mdiffstp(iMVKN)       = 0.0745_dp
  mdiffstp(iISNP)       = 0.0712_dp

  return
end subroutine SetMolecDiffSTP

!**********************************************************************************************************************!
! subroutine SetEffHenrysLawCoeffs - set Henry's Law coefficients (M/atm) for all species 
!                                    (Nguyen et al. 2015. PNAS 112 (5) E392-E401)
!**********************************************************************************************************************!
subroutine SetEffHenrysLawCoeffs()
  integer(kind=i4) :: l

  ! first set everything to default
  do l=1,ntotal
    hstar(l) = hstar_default
  end do

  ! specify values for species with data
  hstar(iNO)           = 0.0019_dp
  hstar(iNO2)          = 0.0120_dp
  hstar(iO3)           = 0.0103_dp
  hstar(iHONO)         = 2.6D+05
  hstar(iHNO4)         = 1.0D+07
  hstar(iHNO3)         = 3.2D+13
  hstar(iN2O5)         = 1.0D+14   ! irreversible hydrolysis assumed
  hstar(iCO)           = 9.8D-04
  hstar(iH2O2)         = 8.4D+04
  hstar(iCH4)          = 1.4D-03
  hstar(iMO2)          = 6.9D+02   ! HO2 value assumed
  hstar(iOP1)          = 3.0D+02
  hstar(iHCHO)         = 4.9D+03
  hstar(iONIT)         = 2.0D+00
  hstar(iMOH)          = 2.2D+02
  hstar(iNO3)          = 3.8D-02
  hstar(iO3P)          = 3.8D-02   ! NO3 value assumed
  hstar(iO1D)          = 3.8D-02   ! NO3 value assumed
  hstar(iHO)           = 3.9D+01
  hstar(iHO2)          = 6.9D+02
  hstar(iORA1)         = 5.6D+03
  hstar(iHAC)          = 2.0D+03
  hstar(iPAA)          = 5.2D+02
  hstar(iDHMOB)        = 2.0D+03
  hstar(iHPALD)        = 4.0D+04
  hstar(iISHP)         = 7.0D+07
  hstar(iIEPOX)        = 7.0D+07
  hstar(iPROPNN)       = 1.0D+04
  hstar(iISOPNB)       = 5.0D+03
  hstar(iISOPND)       = 5.0D+03
  hstar(iMACRN)        = 6.0D+03
  hstar(iMVKN)         = 6.0D+03
  hstar(iISNP)         = 5.0D+03
  return
end subroutine SetEffHenrysLawCoeffs

!**********************************************************************************************************************!
! subroutine SetReactivityParams - set reactivivity parameters for all
!                                  species (as in Wesley 1989) (and Nguyen et al. 2015. PNAS 112 (5) E392-E401)
!**********************************************************************************************************************!
subroutine SetReactivityParams()
  integer(kind=i4) :: l

  ! first set everything to default
  do l=1,ntotal
    f0(l) = f0_default
  end do

  ! specify values for species with data
  f0(iNO)           = 0.0_dp
  f0(iNO2)          = 0.1_dp
  f0(iO3)           = 1.0_dp
  f0(iHONO)         = 0.1_dp
  f0(iHNO4)         = 0.0_dp
  f0(iHNO3)         = 0.0_dp
  f0(iN2O5)         = 0.0_dp
  f0(iCO)           = 0.0_dp
  f0(iH2O2)         = 1.0_dp
  f0(iCH4)          = 0.0_dp
  f0(iMO2)          = 0.1_dp
  f0(iOP1)          = 0.0_dp
  f0(iHCHO)         = 0.0_dp
  f0(iONIT)         = 0.1_dp
  f0(iMOH)          = 0.0_dp
  f0(iNO3)          = 0.0_dp
  f0(iO3P)          = 0.0_dp
  f0(iO1D)          = 0.0_dp
  f0(iHO)           = 0.1_dp
  f0(iHO2)          = 0.1_dp
  f0(iORA1)         = 0.0_dp
  f0(iHAC)          = 0.0_dp
  f0(iPAA)          = 0.1_dp
  f0(iDHMOB)        = 0.0_dp
  f0(iHPALD)        = 0.0_dp
  f0(iISHP)         = 0.0_dp
  f0(iIEPOX)        = 0.0_dp
  f0(iPROPNN)       = 0.0_dp
  f0(iISOPNB)       = 0.0_dp
  f0(iISOPND)       = 0.0_dp
  f0(iMACRN)        = 0.0_dp
  f0(iMVKN)         = 0.0_dp
  f0(iISNP)         = 0.0_dp

  return
end subroutine SetReactivityParams

!**********************************************************************************************************************!
! subroutine SetMolecMass - set molecular mass of all species
!**********************************************************************************************************************!
subroutine SetMolecMass()
  integer(kind=4) :: l

  ! dummy value for now
  do l=1,ntotal
    molecmass(l)  = 1.0_dp
  end do

  return
end subroutine SetMolecMass

!**********************************************************************************************************************!
! function MolecDiff - public function to access molecular diffusivities
!                      (cm2/s) at a given temperature and pressure
!**********************************************************************************************************************!
function MolecDiff(ispec, tkx, pmbx)
  integer(kind=i4), intent(in) :: ispec
  real(kind=dp), intent(in)    :: tkx
  real(kind=dp), intent(in)    :: pmbx
  real(kind=dp)                :: MolecDiff

  MolecDiff = mdiffstp(ispec)*(1013.25_dp/pmbx)*((tkx/298.15_dp)**1.81)
  return
end function MolecDiff

!**********************************************************************************************************************!
! function mdiffh2o - calculate the molecular diffusivity of water vapor
!                     in air
! 
!    Tracy, C. R.; W. R. Weich; W. P. Porter (1980) Properties of Air: A
!    Manual for Use in Biophysical Ecology, Third Ed., Tech. Rep. No. 1;
!    Laboratory for Biophysical Ecology, University of Wisconsin, Madison, WI.
!
!**********************************************************************************************************************!
function mdiffh2o(tki, pmbi)
  real(kind=dp), intent(in) :: tki
  real(kind=dp), intent(in) :: pmbi
  real(kind=dp)             :: mdiffh2o
  mdiffh2o=0.226_dp*((tki/273.15_dp)**1.81_dp)*(1000.0_dp/pmbi)
  return
end function mdiffh2o

!**********************************************************************************************************************!
! function esat_wp - calculate the saturation vapor pressure of water at a
!                 given temperature
!
!    Wagner, W.; A. Pruss (1993) International Equations for the Saturation
!    Properties of Ordinary Water Substance. Revised According to the 
!    International Temperature Scale of 1990. Addendum to J. Phys. Chem. Ref.
!    Data 16, 893 (1987), J. Phys. Chem. Ref. Data, 22, no.3, 783-787.
!
!**********************************************************************************************************************!
function esat_wp(tki)
  real(kind=dp), intent(in) :: tki   ! temperature (K)
  real(kind=dp) :: esat_wp           ! saturation vapor pressure (kPa)
  real(kind=dp), parameter :: a1=-7.85951783
  real(kind=dp), parameter :: a2= 1.84408259
  real(kind=dp), parameter :: a3=-11.7866497
  real(kind=dp), parameter :: a4= 22.6807411
  real(kind=dp), parameter :: a5=-15.9618719
  real(kind=dp), parameter :: a6= 1.80122502
  real(kind=dp), parameter :: pc=22064.0
  real(kind=dp), parameter :: tc=647.096
  real(kind=dp)            :: tau
  real(kind=dp)            :: s
  tau=1.0_dp - (tki/tc)
  s=a1*tau+a2*(tau**1.5)+a3*(tau**3.0)+a4*(tau**3.5)+a5*(tau**4.0)+a6*(tau**7.5) 
  esat_wp = pc*exp(s*tc/tki)
  return
end function esat_wp

!**********************************************************************************************************************!
! function esat - calculate the saturation vapor pressure (kPa) of water at a
!                 given temperature
!
!    Rogers, R. R., and Yau, M. K. (1989) A Short Course in Cloud Physics,
!    3rd Ed., Butterworth-Heinemann, Burlington, MA. (p16)
!    Valid over -30C <= T <= 35C
!
!**********************************************************************************************************************!
function esat(tki)
  real(kind=dp), intent(in) :: tki     ! temperature (K)
  real(kind=dp)             :: esat    ! saturation vapor pressure (kPa)
  real(kind=dp)             :: tc      ! temperature (C)

  tc = tki - 273.15
  esat = 0.6112*exp(17.67*tc/(tc+243.5))
  return 
end function esat

!**********************************************************************************************************************!
! function qsat - calculate the saturation molar concentration (moles/cm3) of water vapor at a
!                 given temperature
!**********************************************************************************************************************!
function qsat(tki)
  real(kind=dp), intent(in) :: tki     ! temperature (K)
  real(kind=dp)             :: qsat    ! saturation molar concentration of water vapor (moles/m3)
  real(kind=dp)             :: es      ! saturation vapor pressure of water (mb)

  es = esat(tki)*10.0                  ! kPa -> mb
  qsat = (es/(0.081314*tki))*1.0E-06   ! from ideal gas law (and convert from moles/m3 to moles/cm3)

end function qsat
!**********************************************************************************************************************!
! function desdt(tki) - calculate the first derivative of es wrt T at a
!                       given temperature
!
!    Derived from the esat expression in ...
!    Rogers, R. R., and Yau, M. K. (1989) A Short Course in Cloud Physics,
!    3rd Ed., Butterworth-Heinemann, Burlington, MA. (p16)
!    Valid over -30C <= T <= 35C
!
!**********************************************************************************************************************!
function desdt(tki)
  real(kind=dp), intent(in) :: tki     ! temperature (K)
  real(kind=dp)             :: desdt   ! des/dt (kPa/C)
  real(kind=dp)             :: tc      ! temperature (C)

  tc = tki - 273.15
  desdt = esat(tki)*4302.6/((tc+243.5)**2.0)
  return
end function desdt

!**********************************************************************************************************************!
! function des2dt2(tki) - calculate the second derivative of es wrt T at a
!                         given temperature
!
!    Derived from the esat expression in ...
!    Rogers, R. R., and Yau, M. K. (1989) A Short Course in Cloud Physics,
!    3rd Ed., Butterworth-Heinemann, Burlington, MA. (p16)
!    Valid over -30C <= T <= 35C
!
!**********************************************************************************************************************!
function des2dt2(tki)
  real(kind=dp), intent(in) :: tki     ! temperature (K)
  real(kind=dp)             :: des2dt2 ! des/dt (kPa/C)
  real(kind=dp)             :: tc      ! temperature (C)
  real(kind=dp)             :: et    

  tc = tki - 273.15
  et = dexp(17.67*tc/(tc+243.5))
  des2dt2 = 2629.75*(3815.6-2.0*tc)*et/((tc+243.5)**4.0)
  return
end function des2dt2

!**********************************************************************************************************************!
! function lambda - calculate the latent heat of evaporation for H2O at a
!                   given temperature
!
!    Polynomial fit of data in Table 2.1 (p. 16) of ...
!    Rogers, R. R., and Yau, M. K. (1989) A Short Course in Cloud Physics,
!    3rd Ed., Butterworth-Heinemann, Burlington, MA. (p16)
!    Valid over -40C <= T <= 40C
!
!**********************************************************************************************************************!
function lambda(tki)
  real(kind=dp), intent(in) :: tki      ! temperature (K)
  real(kind=dp)             :: lambda   ! latent heat of evaporation (J/mol)
  real(kind=dp)             :: tc       ! temperature (C)
  
  tc = tki - 273.15
  lambda = (2500.8 - 2.36*tc + 0.0016*tc*tc - 0.00006*tc*tc*tc)*18.0
  return
end function lambda

!**********************************************************************************************************************!
! function EffHenrysLawCoeff - public function to access Henry's Law
!                              coefficients (M/atm) - may eventually provide
!                              temperature dependence 
!**********************************************************************************************************************!
function EffHenrysLawCoeff(ispec)
  integer(kind=i4), intent(in) :: ispec
  real(kind=dp)                :: EffHenrysLawCoeff

  EffHenrysLawCoeff = hstar(ispec)
  return
end function EffHenrysLawCoeff

!**********************************************************************************************************************!
! function ReactivityParam - public function to access reactivity parameters
!                            (dimensionless)
!**********************************************************************************************************************!
function ReactivityParam(ispec)
  integer(kind=i4), intent(in) :: ispec
  real(kind=dp)                :: ReactivityParam

  ReactivityParam = f0(ispec)
  return
end function ReactivityParam

end module PhysChemData
!======================================================================================================================!