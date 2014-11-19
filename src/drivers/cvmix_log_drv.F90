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
    use cvmix_messages,        only : cvmix_new_log,                          &
                                      cvmix_message_init,                     &
                                      cvmix_message_append,                   &
                                      cvmix_erase_log,                        &
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

contains

!BOP
! !ROUTINE: cvmix_log_driver

! !DESCRIPTION: A routine to test that the linked-list implementation used to
!  log messages from CVMix works correctly.
!\\
!\\

! !INTERFACE:

  Subroutine cvmix_log_driver()

!EOP
!BOC

    type(cvmix_message_type), pointer :: rootlog, log2, log3
    integer :: i

    nullify(rootlog)
    nullify(log2)
    nullify(log3)

    ! Create initial log type
    do i=0,1
      if (i.eq.0) then
        write(*,"(A)") "cvmix_message_init has not been run..."
      else
        call cvmix_message_init(0)
        write(*,"(A)") ""
        write(*,"(A)") "called cvmix_message_init(0)"
      end if

      ! Print an empty log (should be empty, should not seg fault!)
      write(*,"(A)") "Printing contents of [empty] log:"
      call cvmix_print_log(rootlog,  StopOnError=.false.)
      call cvmix_print_log(log2,  StopOnError=.false.)
      call cvmix_print_log(log3, StopOnError=.false.)
      call cvmix_new_log(rootlog)

      ! Set rootlog by hand
      rootlog%StatusCode     = cvmix_status%Verbose
      rootlog%Message        = "Manually entered messsage in rootlog with" // &
                               " status level Verbose"
      rootlog%ModuleName     = "cvmix_log_drv" 
      rootlog%SubroutineName = "cvmix_log_driver"

      ! Set log2 using cvmix_new_log interface
      ! Note that if cvmix_messages::MinMessageLevel > Diagnostic (as is the
      ! case if cvmix_message_init has not been called) then this does not
      ! touch log2.
      call cvmix_new_log(log2, cvmix_status%Diagnostic, "Message with " // &
                         "status level Diagnostic entered via cvmix_new_log", &
                         "cvmix_log_drv", "cvmix_log_driver")
      if (i.eq.0) then
        ! log2 should still be empty!
        call cvmix_print_log(log2)
      end if

      ! Create a new log entry that is identical to the entry we just created
      call cvmix_new_log(log3, log2)
      ! If log2 was not NULL (i.e. MinMessageLevel<=Diagnostic), change a
      ! few values in the copy we just made; otherwise log3 is also NULL
      if (associated(log3)) then
        log3%StatusCode = cvmix_status%Warning
        log3%Message    = "Not a real warning message (but log3 exists)"
      end if

      ! Link messages together
      call cvmix_message_append(rootlog, log2)
      call cvmix_message_append(rootlog, log3)
      call cvmix_erase_log(log3)
      if (associated(log2)) then
        log2%StatusCode = cvmix_status%Error
        log2%Message    = "Not a real error message (but log2 exists)"
        call cvmix_message_append(rootlog, log2)
        call cvmix_erase_log(log2)
      end if
      call cvmix_message_append(rootlog, log2)
      call cvmix_erase_log(log2)

      write(*,"(A)") "Printing contents of log:"
      write(*,"(A)") "----"
      call cvmix_print_log(rootlog, StopOnError=.false.)
      call cvmix_erase_log(rootlog)
      write(*,"(A)") "----"
    end do

!EOC

  End Subroutine cvmix_log_driver

End Module cvmix_log_drv
