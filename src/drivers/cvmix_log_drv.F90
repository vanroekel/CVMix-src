!BOP
!\newpage
! !ROUTINE: cvmix_log_driver

! !DESCRIPTION: A routine to test that the linked-list implementation used to
!  log messages from CVMix works correctly.
!\\
!\\

! !INTERFACE:

Subroutine cvmix_log_driver()

! !USES:

  use cvmix_kinds_and_types, only : cvmix_message_type
  use cvmix_messages,        only : cvmix_new_log,                            &
                                    cvmix_message_append,                     &
                                    cvmix_erase_log,                          &
                                    cvmix_status
  use cvmix_io,              only : cvmix_print_log

  Implicit None

!EOP
!BOC

  type(cvmix_message_type), pointer :: rootlog, nextlog, thirdlog

  nullify(rootlog)
  nullify(nextlog)

  ! Create initial log type
  call cvmix_new_log(rootlog) 
  rootlog%StatusCode     = cvmix_status%Success
  rootlog%Message        = "Hello, world"
  rootlog%ModuleName     = "cvmix_log_drv" 
  rootlog%SubroutineName = "cvmix_log_driver"

  ! Create a new log entry that is identical to the entry we just created
  ! Then manually change a couple of values
  call cvmix_new_log(nextlog, cvmix_status%Verbose, "Verbose message",        &
                     "cvmix_log_drv", "cvmix_log_driver")
  call cvmix_new_log(thirdlog, nextlog)
  thirdlog%StatusCode = cvmix_status%Warning
  thirdlog%Message    = "Warning message"

  ! Link messages together
  call cvmix_message_append(rootlog, nextlog)
  call cvmix_message_append(rootlog, thirdlog)
  call cvmix_erase_log(thirdlog)
  nextlog%StatusCode = cvmix_status%Error
  nextlog%Message    = "Error message"
  call cvmix_message_append(rootlog, nextlog)
  call cvmix_erase_log(nextlog)

  write(*,"(A)") "Printing contents of log:"
  write(*,"(A)") "----"
  call cvmix_print_log(rootlog, 0)
  call cvmix_erase_log(rootlog)
  write(*,"(A)") "----"
  print*, "Printing contents of [empty] log:"
  ! Should be empty (and should not seg-fault)
  call cvmix_print_log(rootlog, 0)
  call cvmix_print_log(nextlog, 0)
  call cvmix_print_log(thirdlog, 0)

!EOC

End Subroutine cvmix_log_driver


