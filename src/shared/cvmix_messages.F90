module cvmix_messages

!BOP
!\newpage
! !MODULE: cvmix_messages
!
! !AUTHOR: 
!  Michael N. Levy, NCAR (mlevy@ucar.edu)
!
! !DESCRIPTION:
!  This module contains routines manage the linked lists used to control
!  message logs in CVMix.
!\\
!\\

! !USES:
  use cvmix_kinds_and_types, only : cvmix_r8,                                 &
                                    cvmix_strlen,                             & 
                                    cvmix_message_type

!EOP

  implicit none
  private
  save

  ! cvmix_verbosity_levels is basically an enumeration of the possible status
  ! levels returned by cvmix_message_type
  type, private :: cvmix_verbosity_levels
    integer :: Verbose
    integer :: EchoNamelist
    integer :: Diagnostic
    integer :: Warning
    integer :: Error
  end type cvmix_verbosity_levels

!BOP

! !DEFINED PARAMETERS:

  ! cvmix_verbosity_level is a private datatype used to define these params
  type(cvmix_verbosity_levels), parameter, public ::                          &
                               cvmix_status=cvmix_verbosity_levels(0,1,2,3,4)

! !PUBLIC MEMBER FUNCTIONS:

  public :: cvmix_message_init
  public :: cvmix_erase_log
  public :: cvmix_log_verbose
  public :: cvmix_log_namelist
  public :: cvmix_log_diagnostic
  public :: cvmix_log_warning
  public :: cvmix_log_error

  interface cvmix_log_namelist
    module procedure cvmix_log_namelist_int
    module procedure cvmix_log_namelist_r8
    module procedure cvmix_log_namelist_str
    module procedure cvmix_log_namelist_bool
  end interface cvmix_log_namelist

  ! Private variable that is set in cvmix_message_init -- if cvmix_message_init
  ! is not run, then nothing is written into message log
  integer :: MinMessageLevel = cvmix_status%Error+1

!EOP

  interface cvmix_new_log
    module procedure cvmix_new_log_copy
    module procedure cvmix_new_log_low
  end interface cvmix_new_log

contains

!BOP

! !IROUTINE: cvmix_message_init
! !INTERFACE:

  subroutine cvmix_message_init(MessageLevel)

! !DESCRIPTION:
!  Initialize module to enable writing CVMix message logs
!\\
!\\

! !INPUT PARAMETERS:
    integer, optional, intent(in) :: MessageLevel

!EOP
!BOC

    if (present(MessageLevel)) then
      MinMessageLevel = MessageLevel
    else
      MinMessageLevel = cvmix_status%Warning
    end if

!EOC

  end subroutine cvmix_message_init

!BOP

! !IROUTINE: cvmix_erase_log
! !INTERFACE:

  subroutine cvmix_erase_log(self)

! !DESCRIPTION:
!  Deletes all entries from a linked list
!\\
!\\

! !INPUT PARAMETERS:
    type(cvmix_message_type), pointer, intent(inout) :: self

!EOP
!BOC

    type(cvmix_message_type), pointer :: current, next

    current => self
    do while (associated(current))
      next => current%next
      deallocate(current)
      nullify(current)
      current => next
    end do

    if (associated(self)) &
      nullify(self)

!EOC

  end subroutine cvmix_erase_log

!BOP

! !IROUTINE: cvmix_log_verbose
! !INTERFACE:

  subroutine cvmix_log_verbose(self, Message, ModuleName, RoutineName)

! !DESCRIPTION:
!  Adds a verbose entry to a log
!\\
!\\

! !INPUT PARAMETERS:
    type(cvmix_message_type), pointer, intent(inout) :: self
    character(len=*),                  intent(in)    :: Message, ModuleName,  &
                                                        RoutineName

!EOP
!BOC

    type(cvmix_message_type), pointer :: NewEntry

    call cvmix_new_log(NewEntry, cvmix_status%Verbose, Message, ModuleName,   &
                       RoutineName)
    call cvmix_message_append(self, NewEntry)
    call cvmix_erase_log(NewEntry)

!EOC

  end subroutine cvmix_log_verbose

!BOP

! !IROUTINE: cvmix_log_namelist_int
! !INTERFACE:

  subroutine cvmix_log_namelist_int(self, val, VarName, ModuleName,           &
                                    RoutineName)

! !DESCRIPTION:
!  Prints "varname = val" to log for integer val
!\\
!\\

! !INPUT PARAMETERS:
    type(cvmix_message_type), pointer, intent(inout) :: self
    integer,                           intent(in)    :: val
    character(len=*),                  intent(in)    :: VarName, ModuleName,  &
                                                        RoutineName

!EOP
!BOC

    type(cvmix_message_type), pointer :: NewEntry
    character(len=cvmix_strlen)       :: Message

    write(Message, "(2A,I0)") trim(VarName), " = ", val

    call cvmix_new_log(NewEntry, cvmix_status%EchoNamelist, Message,          &
                       ModuleName, RoutineName)

    call cvmix_message_append(self, NewEntry)
    call cvmix_erase_log(NewEntry)

!EOC

  end subroutine cvmix_log_namelist_int

!BOP

! !IROUTINE: cvmix_log_namelist_r8
! !INTERFACE:

  subroutine cvmix_log_namelist_r8(self, val, VarName, ModuleName,            &
                                   RoutineName)

! !DESCRIPTION:
!  Prints "varname = val" to log for real val
!\\
!\\

! !INPUT PARAMETERS:
    type(cvmix_message_type), pointer, intent(inout) :: self
    real(cvmix_r8),                    intent(in)    :: val
    character(len=*),                  intent(in)    :: VarName, ModuleName,  &
                                                        RoutineName

!EOP
!BOC

    type(cvmix_message_type), pointer :: NewEntry
    character(len=cvmix_strlen)       :: Message

    write(Message, "(2A,E10.3E2)") trim(VarName), " = ", val

    call cvmix_new_log(NewEntry, cvmix_status%EchoNamelist, Message,          &
                       ModuleName, RoutineName)

    call cvmix_message_append(self, NewEntry)
    call cvmix_erase_log(NewEntry)

!EOC

  end subroutine cvmix_log_namelist_r8

!BOP

! !IROUTINE: cvmix_log_namelist_str
! !INTERFACE:

  subroutine cvmix_log_namelist_str(self, val, VarName, ModuleName,           &
                                    RoutineName)

! !DESCRIPTION:
!  Prints "varname = val" to log for string val
!\\
!\\

! !INPUT PARAMETERS:
    type(cvmix_message_type), pointer, intent(inout) :: self
    character(len=*),                  intent(in)    :: val
    character(len=*),                  intent(in)    :: VarName, ModuleName,  &
                                                        RoutineName

!EOP
!BOC

    type(cvmix_message_type), pointer :: NewEntry
    character(len=cvmix_strlen)       :: Message

    write(Message, "(4A)") trim(VarName), " = '", trim(val), "'"

    call cvmix_new_log(NewEntry, cvmix_status%EchoNamelist, Message,          &
                       ModuleName, RoutineName)

    call cvmix_message_append(self, NewEntry)
    call cvmix_erase_log(NewEntry)

!EOC

  end subroutine cvmix_log_namelist_str

!BOP

! !IROUTINE: cvmix_log_namelist_bool
! !INTERFACE:

  subroutine cvmix_log_namelist_bool(self, val, VarName, ModuleName,          &
                                     RoutineName)

! !DESCRIPTION:
!  Prints "varname = val" to log (with status "EchoNamelist") for logical val
!\\
!\\

! !INPUT PARAMETERS:
    type(cvmix_message_type), pointer, intent(inout) :: self
    logical,                           intent(in)    :: val
    character(len=*),                  intent(in)    :: VarName, ModuleName,  &
                                                        RoutineName

!EOP
!BOC

    type(cvmix_message_type), pointer :: NewEntry
    character(len=cvmix_strlen)       :: Message

    if (val) then
      write(Message, "(2A)") trim(VarName), " = .true."
    else
      write(Message, "(2A)") trim(VarName), " = .false."
    end if

    call cvmix_new_log(NewEntry, cvmix_status%EchoNamelist, Message,          &
                       ModuleName, RoutineName)

    call cvmix_message_append(self, NewEntry)
    call cvmix_erase_log(NewEntry)

!EOC

  end subroutine cvmix_log_namelist_bool

!BOP

! !IROUTINE: cvmix_log_diagnostic
! !INTERFACE:

  subroutine cvmix_log_diagnostic(self, Message, ModuleName, RoutineName)

! !DESCRIPTION:
!  Adds a warning entry to a log
!\\
!\\

! !INPUT PARAMETERS:
    type(cvmix_message_type), pointer, intent(inout) :: self
    character(len=*),                  intent(in)    :: Message, ModuleName,  &
                                                        RoutineName

!EOP
!BOC

    type(cvmix_message_type), pointer :: NewEntry

    call cvmix_new_log(NewEntry, cvmix_status%Diagnostic, Message,            &
                       ModuleName, RoutineName)
    call cvmix_message_append(self, NewEntry)
    call cvmix_erase_log(NewEntry)

!EOC

  end subroutine cvmix_log_diagnostic

!BOP

! !IROUTINE: cvmix_log_warning
! !INTERFACE:

  subroutine cvmix_log_warning(self, Message, ModuleName, RoutineName)

! !DESCRIPTION:
!  Adds a warning entry to a log
!\\
!\\

! !INPUT PARAMETERS:
    type(cvmix_message_type), pointer, intent(inout) :: self
    character(len=*),                  intent(in)    :: Message, ModuleName,  &
                                                        RoutineName

!EOP
!BOC

    type(cvmix_message_type), pointer :: NewEntry

    call cvmix_new_log(NewEntry, cvmix_status%Warning, Message, ModuleName,   &
                       RoutineName)
    call cvmix_message_append(self, NewEntry)
    call cvmix_erase_log(NewEntry)

!EOC

  end subroutine cvmix_log_warning

!BOP

! !IROUTINE: cvmix_log_error
! !INTERFACE:

  subroutine cvmix_log_error(self, Message, ModuleName, RoutineName)

! !DESCRIPTION:
!  Adds an error entry to a log
!\\
!\\

! !INPUT PARAMETERS:
    type(cvmix_message_type), pointer, intent(inout) :: self
    character(len=*),                  intent(in)    :: Message, ModuleName,  &
                                                        RoutineName

!EOP
!BOC

    type(cvmix_message_type), pointer :: NewEntry

    call cvmix_new_log(NewEntry, cvmix_status%Error, Message, ModuleName,     &
                       RoutineName)
    call cvmix_message_append(self, NewEntry)
    call cvmix_erase_log(NewEntry)

!EOC

  end subroutine cvmix_log_error

!!!! PRIVATE ROUTINES !!!!

!BOP

! !IROUTINE: cvmix_new_log_copy
! !INTERFACE:

  subroutine cvmix_new_log_copy(self, newdata)

! !DESCRIPTION:
!  Copy a message log from one cvmix\_message\_type to another
!\\
!\\

! !INPUT PARAMETERS:
    type(cvmix_message_type), pointer,           intent(inout) :: self
    type(cvmix_message_type), pointer, optional, intent(in)    :: newdata

!EOP
!BOC

    if(present(newdata)) then
      if (associated(newdata)) then
        call cvmix_new_log(self, newdata%StatusCode, newdata%Message,         &
                           newdata%ModuleName, newdata%SubroutineName)
      else
        nullify(self)
      end if
    else
      allocate(self)
      nullify(self%next)
    end if

!EOC

  end subroutine cvmix_new_log_copy

!BOP

! !IROUTINE: cvmix_new_log_low
! !INTERFACE:

  subroutine cvmix_new_log_low(self, StatusCode, Message, ModuleName,         &
                               SubroutineName)

! !DESCRIPTION:
!  Write status / message information into a message log
!\\
!\\

! !INPUT PARAMETERS:
    type(cvmix_message_type), pointer, intent(inout) :: self
    integer,                           intent(in)    :: StatusCode
    character(len=*),                  intent(in)    :: Message, ModuleName,  &
                                                        SubroutineName

!EOP
!BOC

    allocate(self)
    nullify(self%next)

    if (StatusCode.ge.MinMessageLevel) then
      self%StatusCode     = StatusCode
      self%Message        = Message
      self%ModuleName     = ModuleName
      self%SubroutineName = SubroutineName
    else
      call cvmix_erase_log(self)
    end if

!EOC

  end subroutine cvmix_new_log_low

!BOP

! !IROUTINE: cvmix_message_append
! !INTERFACE:

  subroutine cvmix_message_append(self, newdata)

! !DESCRIPTION:
!  Append a single message entry onto the end of an existing linked list (note
!  that this does not append two linked lists, it only copies the first entry
!  from the second list!)
!\\
!\\

! !INPUT PARAMETERS:
    type(cvmix_message_type), pointer, intent(inout) :: self, newdata

    type(cvmix_message_type), pointer :: current

!EOP
!BOC

    if (.not.associated(self)) then
      if (associated(newdata)) then
        call cvmix_new_log(self, newdata)
      end if
    else
      current => self
      do while (associated(current%next))
        current => current%next
      end do
      call cvmix_new_log(current%next, newdata)
    end if

!EOC

  end subroutine cvmix_message_append

end module cvmix_messages
