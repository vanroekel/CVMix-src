module cvmix_log_drv

!BOP
!\newpage
! !MODULE: cvmix_log_drv
!
! !AUTHOR: 
!  Michael N. Levy, NCAR (mlevy@ucar.edu)
!
! !DESCRIPTION:
!  This module tests various routines in the cvmix\_messages module.
!\\
!\\

! !USES:

    use cvmix_kinds_and_types, only : cvmix_message_type
    use cvmix_log,             only : cvmix_log_init,                         &
                                      cvmix_log_verbose,                      &
                                      cvmix_log_namelist,                     &
                                      cvmix_log_warning,                      &
                                      cvmix_log_diagnostic,                   &
                                      cvmix_log_error,                        &
                                      cvmix_log_erase,                        &
                                      cvmix_status
    use cvmix_io,              only : cvmix_print_log

!EOP

  implicit none
  private
  save

!BOP

! !PUBLIC MEMBER FUNCTIONS:

  public :: cvmix_log_driver

!EOP

  character(len=13), parameter :: ModName = "cvmix_log_drv"

contains

!BOP
! !ROUTINE: cvmix_log_driver

! !DESCRIPTION: A routine to test that the linked-list implementation used to
!  log messages from CVMix works correctly.
!\\
!\\

! !INTERFACE:

  subroutine cvmix_log_driver()

!EOP
!BOC

    type(cvmix_message_type), pointer :: rootlog

    nullify(rootlog)
    write(*,*) ""

    ! Set status to Verbose, print log
    call cvmix_log_init(cvmix_status%Verbose)
    call cvmix_test_log_writing(rootlog)
    write(*,"(A)") "Logging level = Verbose"
    write(*,"(A)") "----"
    call cvmix_print_log(rootlog, StopOnError=.false.)
    call cvmix_log_erase(rootlog)
    write(*,*) ""

    ! Set status to EchoNamelist, print log
    call cvmix_log_init(cvmix_status%EchoNamelist)
    call cvmix_test_log_writing(rootlog)
    write(*,"(A)") "Logging level = EchoNamelist"
    write(*,"(A)") "----"
    call cvmix_print_log(rootlog, StopOnError=.false.)
    call cvmix_log_erase(rootlog)
    write(*,*) ""

    ! Set status to Diagnostic, print log
    call cvmix_log_init(cvmix_status%Diagnostic)
    call cvmix_test_log_writing(rootlog)
    write(*,"(A)") "Logging level = Diagnostic"
    write(*,"(A)") "----"
    call cvmix_print_log(rootlog, StopOnError=.false.)
    call cvmix_log_erase(rootlog)
    write(*,*) ""

    ! Set status to Warning, print log
    call cvmix_log_init(cvmix_status%Warning)
    call cvmix_test_log_writing(rootlog)
    write(*,"(A)") "Logging level = Warning"
    write(*,"(A)") "----"
    call cvmix_print_log(rootlog, StopOnError=.false.)
    call cvmix_log_erase(rootlog)
    write(*,*) ""

    ! Set status to Error, print log
    call cvmix_log_init(cvmix_status%Error)
    call cvmix_test_log_writing(rootlog)
    write(*,"(A)") "Logging level = Error"
    write(*,"(A)") "----"
    call cvmix_print_log(rootlog, StopOnError=.false.)
    call cvmix_log_erase(rootlog)
    write(*,*) ""

!EOC

  end subroutine cvmix_log_driver

  subroutine cvmix_test_log_writing(MessageLog)

    type(cvmix_message_type), intent(inout), pointer :: MessageLog

    character(len=22), parameter :: SubName = "cvmix_test_log_writing"

    call cvmix_log_verbose(MessageLog, "Example verbose message", ModName,    &
                           SubName)
    call cvmix_log_namelist(MessageLog, .true., "NamelistMessage",            &
                            ModName, SubName)
    call cvmix_log_diagnostic(MessageLog, "Example diagnostic message",       &
                              ModName, SubName)
    call cvmix_log_warning(MessageLog, "Example warning message", ModName,    &
                           SubName)
    call cvmix_log_error(MessageLog, "Example error message", ModName,        &
                         SubName)

  end subroutine cvmix_test_log_writing

end module cvmix_log_drv
