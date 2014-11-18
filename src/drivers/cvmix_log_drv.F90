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
                                    cvmix_message_init,                       &
                                    cvmix_message_append,                     &
                                    cvmix_erase_log,                          &
                                    cvmix_status
  use cvmix_io,              only : cvmix_print_log

  Implicit None

!EOP
!BOC

  type(cvmix_message_type), pointer :: rootlog, nextlog, thirdlog
  integer :: i

  nullify(rootlog)
  nullify(nextlog)
  nullify(thirdlog)

  ! Create initial log type
  do i=0,1
    if (i.eq.0) then
      write(*,"(A)") "Logging without running cvmix_message_init..."
    else
      write(*,"(A)") ""
      write(*,"(A)") "Logging after running cvmix_message_init(0)"
    end if
    call cvmix_new_log(rootlog) 
    rootlog%StatusCode     = cvmix_status%Verbose
    rootlog%Message        = "This message was manually entered"
    rootlog%ModuleName     = "cvmix_log_drv" 
    rootlog%SubroutineName = "cvmix_log_driver"
    call cvmix_new_log(nextlog, cvmix_status%Diagnostic,                      &
                       "Verbose message called through cvmix_new_log",        &
                       "cvmix_log_drv", "cvmix_log_driver")

    ! Create a new log entry that is identical to the entry we just created
    ! Then manually change a couple of values
    call cvmix_new_log(thirdlog, nextlog)
    if (associated(thirdlog)) then
      thirdlog%StatusCode = cvmix_status%Warning
      thirdlog%Message    = "Warning message if nextlog exists"
    end if

    ! Link messages together
    call cvmix_message_append(rootlog, nextlog)
    call cvmix_message_append(rootlog, thirdlog)
    call cvmix_erase_log(thirdlog)
    if (associated(nextlog)) then
      nextlog%StatusCode = cvmix_status%Error
      nextlog%Message    = "Error message if nextlog exists"
      call cvmix_message_append(rootlog, nextlog)
      call cvmix_erase_log(nextlog)
    end if

    write(*,"(A)") "Printing contents of log:"
    write(*,"(A)") "----"
    call cvmix_print_log(rootlog)
    call cvmix_erase_log(rootlog)
    call cvmix_print_log(nextlog)
    call cvmix_erase_log(nextlog)
    write(*,"(A)") "----"
    write(*,"(A)") "Printing contents of [empty] log:"
    ! Should be empty (and should not seg-fault)
    call cvmix_print_log(rootlog)
    call cvmix_print_log(nextlog)
    call cvmix_print_log(thirdlog)
    call cvmix_message_init(0)
  end do

!EOC

End Subroutine cvmix_log_driver


