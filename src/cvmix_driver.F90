!BOI

! !TITLE: In-code documentation for CVMix
! !AUTHORS: Many contributors from GFDL, LANL, and NCAR
! !AFFILIATION: GFDL, LANL, and NCAR
! !DATE: \today

!EOI
!BOP

! !ROUTINE: cvmix_driver

! !DESCRIPTION: The stand-alone driver for the CVMix package. This driver
!  reads in the cvmix\_nml namelist to determine what type of mixing has
!  been requested, and also reads in mixing-specific parameters from a
!  mixingtype\_nml namelist.
!\\
!\\

! !INTERFACE:

Program cvmix_driver

! !USES:

  use cvmix_kinds_and_types, only : cvmix_r8,                                 &
                                    cvmix_message_type,                       &
                                    cvmix_zero,                               &
                                    cvmix_strlen
  use cvmix_log,             only : cvmix_log_init,                           &
                                    cvmix_status,                             &
                                    cvmix_log_verbose,                        &
                                    cvmix_log_namelist,                       &
                                    cvmix_log_error,                          &
                                    cvmix_log_erase
  use cvmix_background_drv,  only : cvmix_BL_driver
  use cvmix_kpp_drv,         only : cvmix_kpp_driver
  use cvmix_log_drv,         only : cvmix_log_driver
  use cvmix_io,              only : cvmix_print_log

!EOP
!BOC

  integer :: nlev, max_nlev, StatusLevel
  real(kind=cvmix_r8) :: ocn_depth 
  character(len=cvmix_strlen) :: mix_type, Verbosity, Message
  type(cvmix_message_type), pointer :: CVMixLog
  character(len=12), parameter :: RoutineName = "cvmix_driver"

  namelist/cvmix_nml/mix_type, nlev, max_nlev, ocn_depth, Verbosity

  nullify(CVMixLog)

  mix_type = 'unset'
  nlev = 0
  max_nlev = 0
  ocn_depth = cvmix_zero
  Verbosity = 'Warning'
  read(*, nml=cvmix_nml)
  if (max_nlev.eq.0) then
    max_nlev = nlev
  end if
  select case (trim(Verbosity))
    case ('Verbose')
      StatusLevel = cvmix_status%Verbose
    case ('EchoNamelist')
      StatusLevel = cvmix_status%EchoNamelist
    case ('Diagnostic')
      StatusLevel = cvmix_status%Diagnostic
    case ('Warning')
      StatusLevel = cvmix_status%Warning
    case ('Error')
      StatusLevel = cvmix_status%Error
    case DEFAULT
      call cvmix_log_init()
      write(Message,'(2A)') trim(Verbosity), " is not a valid Verbosity"
      call cvmix_log_error(CVMixLog, Message, "Main", RoutineName)
      StatusLevel = cvmix_status%Warning
  end select
  call cvmix_log_init(StatusLevel)
  call cvmix_print_log(CVMixLog)
  call cvmix_log_erase(CVMixLog)

  call cvmix_log_namelist(CVMixLog, mix_type, "mix_type", "Main", RoutineName)
  call cvmix_log_namelist(CVMixLog, nlev, "nlev", "Main", RoutineName)
  call cvmix_log_namelist(CVMixLog, max_nlev, "max_nlev", "Main", RoutineName)
  call cvmix_log_namelist(CVMixLog, ocn_depth, "ocn_depth", "Main",           &
                          RoutineName)
  call cvmix_log_namelist(CVMixLog, Verbosity, "Verbosity", "Main",           &
                          RoutineName)
                     
  call cvmix_print_log(CVMixLog)
  call cvmix_log_erase(CVMixLog)

  select case (trim(mix_type))
    case ('BryanLewis')
      call cvmix_BL_driver(nlev, max_nlev, ocn_depth, CVMixLog)
    case ('shear')
      call cvmix_shear_driver(nlev, max_nlev)
    case ('tidal')
      call cvmix_tidal_driver()
    case ('ddiff')
      call cvmix_ddiff_driver(2*nlev, 2*max_nlev)
    case ('kpp')
      call cvmix_kpp_driver(CVMixLog)
    case ('log')
      call cvmix_log_driver()
      call cvmix_log_init(StatusLevel)
    case DEFAULT
      write(Message, "(A,A,A)") "mix_type = '", trim(mix_type),               &
                                "' is not supported by this driver."
      call cvmix_log_error(CVMixLog, Message, "Main", RoutineName)
  end select

  write(Message, "(A,A)") "Completed run with mix_type = ", trim(mix_type)
  call cvmix_log_verbose(CVMixLog, Message, "Main", RoutineName)
  call cvmix_print_log(CVMixLog)
  call cvmix_log_erase(CVMixLog)

End Program cvmix_driver
