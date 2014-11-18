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
  use cvmix_messages,        only : cvmix_message_init,                       &
                                    cvmix_status,                             &
                                    cvmix_new_log,                            &
                                    cvmix_erase_log,                          &
                                    cvmix_message_append
  use cvmix_background_drv,  only : cvmix_BL_driver
  use cvmix_log_drv,         only : cvmix_log_driver
  use cvmix_io,              only : cvmix_print_log

!EOP
!BOC

  integer :: nlev, max_nlev
  real(kind=cvmix_r8) :: ocn_depth 
  character(len=cvmix_strlen) :: mix_type, Verbosity, Message
  type(cvmix_message_type), pointer :: CVMixLog, SingleLog

  namelist/cvmix_nml/mix_type, nlev, max_nlev, ocn_depth, Verbosity

  nullify(CVMixLog)
  nullify(SingleLog)

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
      call cvmix_message_init(cvmix_status%Verbose)
    case ('Diagnostic')
      call cvmix_message_init(cvmix_status%Diagnostic)
    case ('EchoNamelist')
      call cvmix_message_init(cvmix_status%EchoNamelist)
    case ('Warning')
      call cvmix_message_init(cvmix_status%Warning)
    case ('Error')
      call cvmix_message_init(cvmix_status%Error)
    case DEFAULT
      call cvmix_message_init()
      write(Message,'(A, A)') trim(Verbosity), " is not a valid choice for Verbosity"
      call cvmix_new_log(SingleLog, cvmix_status%Error, trim(Message),        &
                         "Main", "cvmix_driver")
      call cvmix_message_append(CVMixLog, SingleLog)
      call cvmix_erase_log(SingleLog)
  end select
  call cvmix_print_log(CVMixLog)
  call cvmix_erase_log(CVMixLog)

  write(Message, '(A,I0)') "Active Levels: ", nlev
  call cvmix_new_log(SingleLog, cvmix_status%EchoNamelist, trim(Message),     &
                     "Main", "cvmix_driver")
  call cvmix_message_append(CVMixLog, SingleLog)
  call cvmix_erase_log(SingleLog)
  write(Message, '(A,I0)') "Levels allocated in memory: ", max_nlev
  call cvmix_new_log(SingleLog, cvmix_status%EchoNamelist, trim(Message),     &
                     "Main", "cvmix_driver")
  call cvmix_message_append(CVMixLog, SingleLog)
  call cvmix_erase_log(SingleLog)

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
      call cvmix_kpp_driver()
    case ('log')
      call cvmix_log_driver()
    case DEFAULT
      print*, "WARNING: mix_type = '", trim(mix_type), "' is not supported by this driver."
  end select

  call cvmix_print_log(CVMixLog)

End Program cvmix_driver
