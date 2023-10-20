program test_sqlcipher

    use, intrinsic :: iso_c_binding
    use :: sqlcipher_interfaces
    use :: sqlcipher_interfaces_wrappers

    implicit none (type, external)

    character(len=*), parameter :: DB_FILE  = 'test.db'
    character(len=*), parameter :: DB_TABLE = 'test_table'
    character(len=*), parameter :: DB_KEY   = 'test_key'

    character(len=:), allocatable :: db_name ! Database name.
    character(len=:), allocatable :: errmsg  ! Error message.
    integer                       :: rc      ! Return code.
    type(c_ptr)                   :: db      ! SQLite database.
    type(c_ptr)                   :: stmt    ! SQLite statement.
    type(c_ptr)                   :: udp     ! User-data pointer.

    ! Set configuration to single thread.
    rc = sqlite3_config(SQLITE_CONFIG_SINGLETHREAD)
    if (rc /= SQLITE_OK) stop 'sqlite3_config(): failed'

    rc = sqlite3_config(SQLITE_CONFIG_LOG, c_funloc(error_log_callback), c_null_ptr)
    if (rc /= SQLITE_OK) stop 'sqlite3_config(): failed'

    print '("SQLite library version: ", a)', sqlite3_libversion()
    print '("SQLite source ID: ", a)', sqlite3_sourceid()

    ! Open SQLite database.
    rc = sqlite3_open(DB_FILE//c_null_char, db)
    if (rc /= SQLITE_OK) stop 'sqlite3_open(): failed'

    ! tkd added
    ! Apply encryption to SQLCipher database.
    rc = sqlite3_key(db, DB_KEY, len(DB_KEY))
    if (rc /= SQLITE_OK) stop 'sqlite3_key(): failed'

    db_name = sqlite3_db_name(db, 0)
    if (.not. allocated(db_name)) stop 'sqlite3_db_name(): failed'
    print '("DB name: ", a)', db_name

    ! Testing logging.
    call sqlite3_log(1, 'TEST LOG')

    ! Enable WAL mode.
    print '("Turning WAL mode on ...")'
    rc = journal_mode_wal(db)
    if (rc /= SQLITE_OK) print '("Unable to set WAL mode")'

    ! Register update hook.
    udp = sqlite3_update_hook(db, c_funloc(update_callback), c_null_ptr)

    ! Query SQLite version.
    rc = sqlite3_prepare(db, "SELECT SQLITE_VERSION()", stmt)
    if (rc /= SQLITE_OK) print '("sqlite3_prepare(): failed")'

    if (rc /= SQLITE_OK) then
        print '("Failed to fetch data: ", a)', sqlite3_errmsg(db)
    else
        rc = sqlite3_step(stmt)

        if (rc == SQLITE_ROW) then
            print '("SQLite version from query: ", a)', sqlite3_column_text(stmt, 0)
        end if
    end if

    rc = sqlite3_finalize(stmt)
    if (rc /= SQLITE_OK) print '("sqlite3_finalize(): failed")'

    ! Create table.
    rc = sqlite3_exec(db, "CREATE TABLE " // DB_TABLE // " (" // &
                          "id     INTEGER PRIMARY KEY," // &
                          "string VARCHAR(32)," // &
                          "value  INTEGER)", &
                      c_null_funptr, c_null_ptr, errmsg)
    call print_error(rc, 'sqlite3_exec', errmsg)

    ! Insert values.
    rc = sqlite3_exec(db, "INSERT INTO " // DB_TABLE // "(string, value) VALUES('one', 12345)", &
                      c_null_funptr, c_null_ptr, errmsg)
    call print_error(rc, 'sqlite3_exec', errmsg)

    ! Prepare statement.
    rc = sqlite3_prepare_v2(db, "INSERT INTO " // DB_TABLE // "(string, value) VALUES(?, ?)", stmt)
    if (rc /= SQLITE_OK) print '("sqlite3_prepare_v2(): failed")'

    ! Bind values.
    rc = sqlite3_bind_text(stmt, 1, 'two')
    if (rc /= SQLITE_OK) print '("sqlite3_bind_text(): failed")'

    rc = sqlite3_bind_int(stmt, 2, 987654321)
    if (rc /= SQLITE_OK) print '("sqlite3_bind_int(): failed")'

    ! Insert values.
    rc = sqlite3_step(stmt)
    if (rc /= SQLITE_DONE) print '("sqlite3_step(): failed")'

    ! Reset statement, add more values.
    rc = sqlite3_reset(stmt)
    if (rc /= SQLITE_OK) print '("sqlite3_reset(): failed")'
    rc = sqlite3_bind_text(stmt, 1, 'three')
    if (rc /= SQLITE_OK) print '("sqlite3_bind_text(): failed")'
    rc = sqlite3_bind_int(stmt, 2, 192837465)
    if (rc /= SQLITE_OK) print '("sqlite3_bind_int(): failed")'
    rc = sqlite3_step(stmt)
    if (rc /= SQLITE_DONE) print '("sqlite3_step(): failed")'

    ! Clean up.
    rc = sqlite3_finalize(stmt)
    if (rc /= SQLITE_OK) print '("sqlite3_finalize(): failed")'

    ! Read values.
    print '(/, "--- TESTING PREPARE/STEP")'
    rc = sqlite3_prepare_v2(db, "SELECT * FROM " // DB_TABLE, stmt)
    if (rc /= SQLITE_OK) print '("sqlite3_prepare_v2(): failed")'

    do while (sqlite3_step(stmt) /= SQLITE_DONE)
        call print_values(stmt, 3)
    end do

    rc = sqlite3_finalize(stmt)
    call print_error(rc, 'sqlite3_finalize', errmsg)

    ! Read values using callback function.
    print '(/, "--- TESTING CALLBACK FUNCTION")'
    rc = sqlite3_exec(db, "SELECT * FROM " // DB_TABLE, &
                      c_funloc(exec_callback), c_null_ptr, errmsg)
    call print_error(rc, 'sqlite3_exec', errmsg)

    ! Close SQLite handle.
    rc = sqlite3_close(db)
    if (rc /= SQLITE_OK) stop 'sqlite3_close(): failed'
    if (c_associated(db)) stop 'sqlite3_close(): pointer not NULL'

end program test_sqlcipher
