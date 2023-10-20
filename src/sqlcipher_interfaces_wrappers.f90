#define LOGPRINT print '(a1, a, a1, i4, a2, a)', '[', __FILE__, ':', __LINE__, '] ', 
#define ERRLOG write ( 0, * ) __STAMP__, 

module sqlcipher_interfaces_wrappers

  use, intrinsic :: iso_c_binding
  use sqlcipher_interfaces

  implicit none
  private

  public :: print_values
  public :: print_error
  public :: journal_mode_wal
  public :: error_log_callback
  public :: exec_callback
  public :: update_callback

  ! tkd added
  type, public :: sql_col_ty

    character(30) :: nm
    character(30) :: ty
    character(60) :: nmty

  end type

  ! tkd added
  type, public :: sql_ty

    type(sql_col_ty), allocatable :: cols(:)

    character(255) :: database = 'NA'
    !character(:), allocatable :: database
    character(255) :: table = 'NA'
    type(c_ptr)    :: db     ! SQLite database.
    type(c_ptr)    :: stmt   ! SQLite statement.
    integer        :: nr, nc ! Number of rows, columns

    contains

      procedure :: open_db
      procedure :: close_db
      procedure :: begin_db
      procedure :: commit_db
      procedure :: final_db
      procedure :: step  => step_sql
      procedure :: prep  => prep_sql
      procedure :: exec  => exec_sql
      procedure :: reset => reset_sql
      procedure :: print => print_sql
      !procedure :: count_rows => count_rows_sql

  end type

contains

  subroutine open_db ( this, database )

    class(sql_ty), intent(inout)        :: this
    character(*),  intent(in), optional :: database
    integer rc

    if ( present(database) ) then
      this%database = trim(database)
    end if

    rc = sqlite3_open ( trim(this%database)//c_null_char, this%db )

    if ( rc /= SQLITE_OK ) then
      LOGPRINT '*** Error on opening '//trim(this%database)
      stop
    !else
    !  LOGPRINT 'Databese: '//trim(this%database)//' is opened'
    end if

  end subroutine

  subroutine close_db ( this )

    class(sql_ty), intent(inout) :: this
    integer rc

    rc = sqlite3_close ( this%db )

    if ( rc /= SQLITE_OK ) then
      LOGPRINT '*** Error on closing '//trim(this%database)
      stop
!    else
!      LOGPRINT 'Databese: '//trim(this%database)//' is closed'
    end if

  end subroutine

  subroutine begin_db ( this )

    class(sql_ty), intent(inout) :: this
    character(:), allocatable    :: errmsg
    character(*), parameter      :: QUERY = 'BEGIN TRANSACTION'
    integer rc

    rc = sqlite3_exec ( this%db, QUERY, &
                        c_null_funptr, c_null_ptr, errmsg )

    if ( rc /= SQLITE_OK ) then
      LOGPRINT '*** Error on '//trim(QUERY)//': ', trim(errmsg)
      rc = sqlite3_close ( this%db )
!    else
!      LOGPRINT 'Transaction begins'
    end if

  end subroutine

  subroutine commit_db ( this )

    class(sql_ty), intent(inout) :: this
    character(:), allocatable    :: errmsg
    character(*), parameter      :: QUERY = 'COMMIT'
    integer rc

    rc = sqlite3_exec ( this%db, QUERY, &
                        c_null_funptr, c_null_ptr, errmsg )

    if ( rc /= SQLITE_OK ) then
      LOGPRINT '*** Error on '//trim(QUERY)//': ', trim(errmsg)
      rc = sqlite3_close ( this%db )
    !else
    !  LOGPRINT 'Transaction ends'
    end if

  end subroutine

  subroutine final_db ( this )

    class(sql_ty), intent(inout) :: this
    integer rc

    rc = sqlite3_finalize ( this%stmt )

    if ( rc /= SQLITE_OK ) then
      LOGPRINT '*** Error on FINALIZATION'
      rc = sqlite3_close ( this%db )
      stop
!    else
!      LOGPRINT 'Database is finalized'
    end if

  end subroutine

  subroutine exec_sql ( this, query )

    class(sql_ty), intent(inout) :: this
    character(*),  intent(in)    :: query
    character(:), allocatable    :: errmsg ! Error message.
    integer rc

    rc = sqlite3_exec ( this%db, query, &
                        c_null_funptr, c_null_ptr, errmsg )

    if ( rc /= SQLITE_OK ) then
      LOGPRINT '*** Error on '//trim(query)//': ', trim(errmsg)
      rc = sqlite3_close ( this%db )
    end if

  end subroutine

  subroutine prep_sql ( this, query )

    class(sql_ty), intent(inout) :: this
    character(*),  intent(in)    :: query
    integer rc

    rc = sqlite3_prepare ( this%db, query, this%stmt )

    if ( rc /= SQLITE_OK ) then
      LOGPRINT '*** Error on PREPARATION: '//trim(query)
      rc = sqlite3_close ( this%db )
      stop
!    else
!      LOGPRINT 'SQL is prepared'
    end if

  end subroutine

  subroutine reset_sql ( this )

    class(sql_ty), intent(inout) :: this
    integer rc

    rc = sqlite3_reset ( this%stmt )

    if ( rc /= SQLITE_OK ) then
      LOGPRINT '*** Error on RESET'
      rc = sqlite3_close ( this%db )
      stop
    end if

  end subroutine

  logical function step_sql ( this )

    class(sql_ty), intent(inout) :: this

    step_sql = sqlite3_step ( this%stmt ) /= SQLITE_DONE

  end function

!  subroutine count_rows_sql ( this, table )
!
!    class(sql_ty), intent(inout) :: this
!    character(*),  intent(in)    :: table
!
!    call this%prep ( 'SELECT COUNT(*) FROM '//trim(table) )
!
!    do while ( .not. this%step () )
!
!      this%nr = sqlite3_column_int ( this%stmt, 0 )
!
!      if ( this%nr == 0 ) stop '*** Error on counting rows (No rows)'
!
!      print *, 'Number of rows:', this%nr
!
!      call this%final_db
!
!      exit
!
!    end do
!
!  end subroutine

  subroutine print_sql ( this )

    class(sql_ty), intent(inout) :: this
    integer nc, i
    integer, parameter :: MAX_ROWS = 100

    nc = sqlite3_column_count ( this%stmt )

    print '(a, i0, "  "$)', 'Number of columns:', nc

    print '(a, ",", *(a, :, ","))', &
      ( sqlite3_column_name ( this%stmt, i ), i = 1, nc )

    i = 1
    do while ( .not. this%step () .and. i < MAX_ROWS )

      call print_values ( this%stmt, nc )

      i = i + 1

    end do

  end subroutine

!    subroutine print_error ( rc, func, errmsg )
!
!      integer,                       intent(in)    :: rc
!      character(len=*),              intent(in)    :: func
!      character(len=:), allocatable, intent(inout) :: errmsg
!
!      if ( rc /= SQLITE_OK ) print '(a, "(): ", a)', trim(func), errmsg
!      if ( allocated(errmsg) ) deallocate ( errmsg )
!
!    end subroutine print_error

    integer function journal_mode_wal(db) result(rc)
        !! Enables WAL mode.
        type(c_ptr), intent(inout)         :: db
        character(len=:), allocatable      :: buf
        integer                            :: err
        type(c_ptr)                        :: stmt

        rc = -1

        sql_block: block
            err = sqlite3_prepare_v2(db, "PRAGMA journal_mode=WAL", stmt)
            if (err /= SQLITE_OK) exit sql_block

            err = sqlite3_step(stmt)
            if (err /= SQLITE_ROW) exit sql_block

            buf = sqlite3_column_text(stmt, 0)
            if (.not. allocated(buf)) exit sql_block
            if (buf /= 'wal') exit sql_block

            rc = 0
        end block sql_block

        err = sqlite3_finalize(stmt)
    end function journal_mode_wal

    subroutine print_error(rc, func, errmsg)
      integer,                       intent(in)    :: rc
      character(len=*),              intent(in)    :: func
      character(len=:), allocatable, intent(inout) :: errmsg

        if (rc == SQLITE_OK) return

        if (allocated(errmsg)) then
            print '(a, "(): ", a)', trim(func), errmsg
            deallocate (errmsg)
            return
        end if

        print '(a, "(): unknown error")', trim(func)
    end subroutine print_error

    subroutine print_values(stmt, ncols)
        type(c_ptr), intent(inout)     :: stmt
        integer,     intent(in)        :: ncols
        integer                        :: col_type
        integer                        :: i
        character(len=:), allocatable  :: buf

        do i = 0, ncols - 1
            col_type = sqlite3_column_type(stmt, i)

            select case (col_type)
                case (SQLITE_INTEGER)
                    write (*, '(i12)', advance='no') sqlite3_column_int(stmt, i)

                case (SQLITE_FLOAT)
                    write (*, '(f0.8)', advance='no') sqlite3_column_double(stmt, i)

                case (SQLITE_TEXT)
                    buf = sqlite3_column_text(stmt, i)
                    if (allocated(buf)) then
                        write (*, '(a12)', advance='no') buf
                        deallocate (buf)
                    end if

                case default
                    write (*, '(" not implemented")', advance='no')
            end select
        end do

        print *
    end subroutine print_values

    integer(kind=c_int) function exec_callback(client_data, argc, argv, cols) bind(c)
        !! Callback function for `sqlite3_exec()` that just prints the passed
        !! row of the SQL query.
        type(c_ptr),         intent(in), value :: client_data
        integer(kind=c_int), intent(in), value :: argc
        type(c_ptr),         intent(in)        :: argv(*)
        type(c_ptr),         intent(in)        :: cols(*)
        character(len=:), allocatable          :: buf
        integer                                :: i

        exec_callback = 1 ! No more rows on error.

        print '("--- There are ", i0, " values in selected row")', argc
        if (argc == 0) return

        do i = 1, argc
            if (.not. c_associated(argv(i))) cycle
            call c_f_str_ptr(argv(i), buf)
            print '("VALUE: ", a)', buf
            buf = ' '
        end do

        exec_callback = 0
    end function exec_callback

    ! void error_log_callback(void *udp, int err_code, const char *err_msg)
    subroutine error_log_callback(udp, err_code, err_msg) bind(c)
        type(c_ptr),         intent(in), value :: udp
        integer(kind=c_int), intent(in), value :: err_code
        type(c_ptr),         intent(in), value :: err_msg
        character(len=:), allocatable          :: msg

        call c_f_str_ptr(err_msg, msg)
        print '(a)', repeat('-', 64)
        print '("ERROR ", i0, ": ", a)', err_code, msg
        print '(a)', repeat('-', 64)
        if (allocated(msg)) deallocate (msg)
    end subroutine error_log_callback

    ! void update_callback(void* udp, int type, const char *db_name, const char *tbl_name, sqlite3_int64 rowid)
    subroutine update_callback(udp, type, db_name, tbl_name, rowid) bind(c)
        !! Callback routine that is called whenever a row is inserted, updated,
        !! or deleted in the database. Has to be registered with
        !! `sqlite3_update_hook()`.
        type(c_ptr),             intent(in), value :: udp
        integer(kind=c_int),     intent(in), value :: type
        type(c_ptr),             intent(in), value :: db_name
        type(c_ptr),             intent(in), value :: tbl_name
        integer(kind=c_int64_t), intent(in), value :: rowid
        character(len=:), allocatable              :: db_str, tbl_str

        call c_f_str_ptr(db_name, db_str)
        call c_f_str_ptr(tbl_name, tbl_str)

        select case (type)
            case (SQLITE_INSERT)
                print '("Row ", i0, " has been added to table ", a, " in database ", a, "!")', &
                    rowid, tbl_str, db_str

            case (SQLITE_UPDATE)
                print '("Row ", i0, " in table ", a, " of database ", a, " has been updated!")', &
                    rowid, tbl_str, db_str

            case (SQLITE_DELETE)
                print '("Row ", i0, " in table ", a, " of database ", a, " has been deleted!")', &
                    rowid, tbl_str, db_str
        end select

        if (allocated(db_str)) deallocate (db_str)
        if (allocated(tbl_str)) deallocate (tbl_str)
    end subroutine update_callback

end module sqlcipher_interfaces_wrappers
