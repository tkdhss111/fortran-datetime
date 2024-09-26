program unit_test1
  use dt_mo
  implicit none
  type(dt_ty)                :: dt
  type(dt_ty),   allocatable :: dts(:), dts_hours(:)
  character(19), allocatable :: datetimes(:)
  integer, allocatable       :: poss(:)
  integer                    :: i, u, n, pos
  character(19)              :: datetime

  !
  ! strftime datetime test
  !
  dt = dt%now()
  print '(a)', 'strftime: %Y-%m-%d 01:00'
  datetime = strftime ( dt, '%Y-%m-%d %H:01:00' )
  print '(a, a)', 'datatime: ', datetime

  !
  ! Intel datetime test
  !
  print '(a)', 'read datatime with T: 2008-01-01T00:00:00'
  dt = strptime ( '2008-01-01T00:00:00' )
  print '(a, a)', 'datatime with T: ', dt%datetime

  !
  ! dt_ty object 
  !
  print '(a)', repeat('=', 79)
  print '(a)', 'Procedure name: type(dt_ty) and strptime'
  print '(a)', 'Check         : Time string and integer members'
  print '(a)', repeat('=', 79)

  ! Test current time
  print '(a)', "Current time"
  dt = dt%now ()
  print '(a40, a30)', "dt%now (): ", dt%datetime 
  print '(a)', ''

  ! Test user-defined time
  print '(a)', "User-defined time"
  dt = strptime ( '2020-12-31 23:04:05' )
  print '(a40, a30)', "strptime ( '2020-12-31 23:04:05' ): ", dt%datetime 
  print '(a)', ''

  ! Test user-defined time with format
  print '(a)', "User-defined time with format"
  dt = strptime ( '2020-12-31 23:04:05' )
  dt = strptime ( '201231', fmt0 = '%y%m%d' )
  print '(a40, a30)', "strptime ( '201231', fmt0 = '%y%m%d' ): ", dt%datetime 
  print '(a)', ''

  ! Test dt_ty object via character array
  print '(a)', "dt_ty obset setup via charactger array"
  datetimes = [ '2020-01-03 03:04:05', '2001-02-03 04:05:06' ]
  dts = strptime ( datetimes )
  n = size(dts)
  print '(*(1x, a19))',       "datetime : ", [(dts(i)%datetime,  i = 1, n)]
  print '(a)', ''

  ! Test dt_ty object via integer array ( get_datetime function test )
  print '(a)', "dt_ty obset setup via integer array ( get_datetime function test )"
  datetimes = get_datetime ( yr0=[2020, 2012, 2000], mo0=[2,3,4], dy0=[1,4,6], hr0=[4,0,5],mi0=[1,3,5], sc0=[0,20,30] ) 
  dts = strptime ( datetimes )
  print '(*(1x, a19))',       "datetime : ", [(dts(i)%datetime,  i = 1, n)]
  print '(a)', ''

  dts = seq_dt ( strptime ( '2020-12-31 23:04:05' ), strptime ( '2021-01-02 23:04:05' ), by = '1 day' )

  dts_hours = seq_dt ( strptime ( '2010-06-08 00:00:00' ), &
                       strptime ( '2030-12-31 23:00:00' ), by = '1 hour' )

  n = size(dts)

  print '(a)', ''
  print '(a)', repeat('-', 79)
  print '(a)', "String members of dt_ty"
  print '(a)', repeat('-', 79)
  print '(*(1x, a19))',       "date     : ", [(dts(i)%date,  i = 1, n)]
  print '(*(1x, a19))',       "time     : ", [(dts(i)%time,  i = 1, n)]
  print '(*(1x, a19))',       "yyyy     : ", [(dts(i)%yyyy,  i = 1, n)]
  print '(*(1x, a19))',       "yy       : ", [(dts(i)%yy,    i = 1, n)]
  print '(*(1x, a19))',       "mm       : ", [(dts(i)%mm,    i = 1, n)]
  print '(*(1x, a19))',       "dd       : ", [(dts(i)%dd,    i = 1, n)]
  print '(*(1x, a19))',       "hh       : ", [(dts(i)%hh,    i = 1, n)]
  print '(*(1x, a19))',       "nn       : ", [(dts(i)%nn,    i = 1, n)]
  print '(*(1x, a19))',       "ss       : ", [(dts(i)%ss,    i = 1, n)]
  print '(1x, a19, *(1x, a19))', "day of the week : ", [(LAB_WDAY(dts(i)%dow),  i = 1, n)]

  print '(a)', ''

  print '(a)', repeat('-', 79)
  print '(a)',  "Integer members of dt_ty"
  print '(a)', repeat('-', 79)
  print '(1x, a19, *(1x, i19))', "yr      : ", [(dts(i)%yr,   i = 1, n)]
  print '(1x, a19, *(1x, i19))', "mo      : ", [(dts(i)%mo,   i = 1, n)]
  print '(1x, a19, *(1x, i19))', "dy      : ", [(dts(i)%dy,   i = 1, n)]
  print '(1x, a19, *(1x, i19))', "hr      : ", [(dts(i)%hr,   i = 1, n)]
  print '(1x, a19, *(1x, i19))', "mi      : ", [(dts(i)%mi,   i = 1, n)]
  print '(1x, a19, *(1x, i19))', "sc      : ", [(dts(i)%sc,   i = 1, n)]
  print '(1x, a19, *(1x, i19))', "leap    : ", [(dts(i)%leap, i = 1, n)]
  print '(1x, a19, *(1x, i19))', "doy     : ", [(dts(i)%doy,  i = 1, n)]
  print '(1x, a19, *(1x, i19))', "dow     : ", [(dts(i)%dow,  i = 1, n)]
  print '(a)', ''

  !
  ! Strings manupulations
  !
  print '(a)', repeat('=', 79)
  print '(a)', 'Procedure name: strftime and strptime'
  print '(a)', 'Check         : Time strings manupulation'
  print '(a)', repeat('=', 79)

  print '(1x, a50, a30)', "strftime ( dt, '%y' ): ", strftime ( dt, '%y' )
  print '(1x, a50, a30)', "strftime ( dt, '%Y-%m-%d %H:%M:%S (%b) UTC' ): ", strftime ( dt, '%Y-%m-%d %H:%M:%S (%b) UTC' )
  print '(1x, a50, a30)', "strftime ( dt, '%Y-%m-%d %H:%M:%S' ): ",          strftime ( dt, '%Y-%m-%d %H:%M:%S' )
  print '(1x, a50, a30)', "strftime ( dt, '%Y-%m-%d' ): ",                   strftime ( dt, '%Y-%m-%d' )
  print '(1x, a50, a30)', "strftime ( dt, '%y-%m-%d' ): ",                   strftime ( dt, '%y-%m-%d' )
  print '(1x, a50, a30)', "strftime ( dt, '%Y%m%d' ): ",                     strftime ( dt, '%Y%m%d' )
  print '(1x, a50, a30)', "strftime ( dt, 'on %m/%d, %Y' ): ",               strftime ( dt, 'on %m/%d, %Y' )
  print '(1x, a50, a30)', "strftime ( dt, '%H 時 %m/%d, %Y' ): ",            strftime ( dt, '%H 時 %m/%d, %Y' )

  dt = strptime ( '2010-01-01 02:04', '%Y-%m-%d %H:%M' )
  print '(a)', 'strptime ( "2010-01-01 02:04", "%Y-%m-%d %H:%M" ): ', dt%datetime

  dt = strptime ( '04:02 2010/01/01', '%H:%M %Y/%m/%d' )
  print '(a)', 'strptime ( "04:02 2010/01/01", "%H:%M %Y/%m/%d" ): ', dt%datetime
  print '(a)', ''

  !
  ! Addition and subtraction of datetime 
  !
  print '(a)', repeat('=', 79)
  print '(a)', 'Procedure name: plus_dt and minus_dt'
  print '(a)', 'Check         : time addition and subtraction'
  print '(a)', repeat('=', 79)

  print *, "Before addition dt%datetime: ", dt%datetime 
  dt = dt%plus ( days = 5 )
  print '(a40, a30)', "dt%plus ( days = 5 ): ", dt%datetime 

  dt = dt%plus ( hrs = 5 )
  print '(a40, a30)', "dt%plus ( hrs = 5 ): ", dt%datetime 

  dt = dt%plus ( mins = 5 )
  print '(a40, a30)', "dt%plus ( mins = 5 ): ", dt%datetime 

  dt = dt%plus ( secs = 5 )
  print '(a40, a30)', "dt%plus ( secs = 5 ): ", dt%datetime 

  dt = dt%plus ( days = 4, hrs = 3, mins = 2, secs = 1 )
  print '(a50, a20)', "dt%plus ( days = 4, hrs = 3, mins = 2, secs = 1 ): ", dt%datetime 

  dt = dt%minus ( days = 15 )
  print '(a40, a30)', "dt%minus ( days = 15 ): ", dt%datetime 

  dt = dt%minus ( hrs = 5 )
  print '(a40, a30)', "dt%minus ( hrs = 5 ): ", dt%datetime 

  dt = dt%minus ( mins = 5 )
  print '(a40, a30)', "dt%minus ( mins = 5 ): ", dt%datetime 

  dt = dt%minus ( secs = 5 )
  print '(a40, a30)', "dt%minus ( secs = 5 ): ", dt%datetime 

  dt = dt%minus ( days = 34, hrs = 33, mins = 72, secs = 81 )
  print '(a50, a20)', "dt%minus (days=34, hrs=33, mins=72, secs=81): ", dt%datetime 

  !
  ! Time difference tests
  !
  print '(a)', repeat('=', 79)
  print '(a)', 'Procedure name: diff_secs and diff_dt'
  print '(a)', 'Check         : Date and time difference'
  print '(a)', repeat('=', 79)

  call test_diff_dt ( datetimes = [ '2000-01-01 00:00:00', '2000-01-02 00:00:00' ] )
  call test_diff_dt ( datetimes = [ '2000-01-01 00:00:00', '2000-02-01 00:00:00' ] )
  call test_diff_dt ( datetimes = [ '2000-01-01 00:00:00', '2001-01-01 00:00:00' ] )
  call test_diff_dt ( datetimes = [ '2000-01-01 00:00:00', '2001-02-03 04:05:06' ] )
  call test_diff_dt ( datetimes = [ '2000-12-31 10:50:11', '2001-02-05 01:40:10' ] )
  call test_diff_dt ( datetimes = [ '2000-12-31 10:50:11', '2011-02-05 01:40:10' ] )

  !
  ! Date and time series tests
  !
  print '(a)', repeat('=', 79)
  print '(a)', 'Procedure name: seq_dt'
  print '(a)', 'Check         : Date and time series creation'
  print '(a)', repeat('=', 79)

  call test_seq_dt2 ( by = '1 day', tunit = 60*60*24, datetimes = [ '2020-08-19 01:00:00', '2020-08-21 23:30:00' ] )
  call test_seq_dt ( by = '5 days', tunit = 60*60*24, datetimes = [ '2000-12-31 10:00:00', '2001-02-05 00:00:00' ] )
  call test_seq_dt ( by = '5 hrs',  tunit = 60*60,    datetimes = [ '2000-12-31 15:00:00', '2001-01-02 04:00:00' ] )
  call test_seq_dt ( by = '5 mins', tunit = 60,       datetimes = [ '2000-12-31 23:50:00', '2001-01-01 00:06:00' ] )
  call test_seq_dt ( by = '5 secs', tunit = 1,        datetimes = [ '2000-12-31 23:59:50', '2001-01-01 00:00:02' ] )

  ! Print test for large date and time series
  dts = seq_dt ( dt_fr = strptime ('2000-01-01 00:00:00'), dt_to = strptime ('2000-01-01 01:00:00'), by = '1 sec' )
  call print_dts (dts)

  !
  ! Writing position tests
  !
  dt = strptime ( '2008-01-01 00:00:00' )
  pos = get_pos_rec ( dt )
  print '(a, i0)', 'Writing position by 30 secs from the epoch: ', pos

  pos = get_pos_rec ( '2008-01-02 00:00:00' )
  print '(a, i0)', 'Writing position by 30 secs from the epoch: ', pos

  poss = get_pos_rec ( ['2008-01-02 00:00:00', '2008-01-03 00:00:00'] )
  print '(a, i0, 1x, i0)', 'Writing position by 30 secs from the epoch: ', poss

  open ( newunit = u, file = 'test.csv', form = 'unformatted', access = 'direct', recl = 5 )

  write ( u, rec = get_pos_rec ( '2008-01-01 00:00:00' ) ) 'test1'
  write ( u, rec = get_pos_rec ( '2008-01-01 00:30:00' ) ) 'test2'
  write ( u, rec = get_pos_rec ( '2008-01-01 01:00:00' ) ) 'test3'
  write ( u, rec = get_pos_rec ( '2008-01-01 02:00:00' ) ) 'test5'
  write ( u, rec = get_pos_rec ( '2008-01-01 05:00:00' ) ) 'test9'

  close (u)

  ! On spot test
  dts = seq_dt ( strptime ( '2008-06-25 00:00:00' ), strptime ( '2024-09-24 00:00:00' ), by = '1 day' )
  print *, 'size(dts): ', size(dts), ', dts(size(dts))%datetime: ', dts(size(dts))%datetime

contains

  subroutine test_diff_dt ( datetimes )

    character(*)             :: datetimes(:)
    type(dt_ty)              :: dts0(2)
    type(diff_dt_ty)        :: n
    integer nsecs

    dts0 = strptime ( datetimes )

    n = diff_dt ( dts0(1), dts0(2) )

    print '(a)', repeat('-', 79)
    print '(a)', 'diff_dt from '//dts0(1)%datetime//' to '//dts0(2)%datetime
    print '(a)', repeat('-', 79)

    print '(1x, a, i0, a, i0, a, i0, a, i0, a)', &
    'Time difference: ', n%days, 'days, ', n%hrs, 'hrs, ', n%mins, 'mins, ', n%secs, 'secs.'

    nsecs = dts0(2) - dts0(1)
    print '(1x, a, i0)', "nsecs: ", nsecs
    print '(a)', ''

  end subroutine

  subroutine test_seq_dt ( by, datetimes, tunit )

    character(*)             :: by
    character(*)             :: datetimes(:)
    type(dt_ty)              :: dts0(2)
    type(dt_ty), allocatable :: dts(:)
    integer                  :: nsecs, tunit

    dts0 = strptime ( datetimes )

    print '(a)', repeat('-', 79)
    print '(a)', 'seq_dt from '//dts0(1)%datetime//' to '//dts0(2)%datetime//' by '//by
    print '(a)', repeat('-', 79)

    dts = seq_dt ( dt_fr = dts0(1), dt_to = dts0(2), by = by )

    nsecs = dts0(2) - dts0(1)
    print '(1x, a)', [(dts(i)%datetime, i = 1, size(dts))]
    print '(1x, a, i0)', "nsecs: ", nsecs
    print '(1x, a, i0)', 'nunits in seq: ', nsecs / tunit
    print '(a)', ''

  end subroutine

  subroutine test_seq_dt2 ( by, datetimes, tunit )

    character(*)             :: by
    character(*)             :: datetimes(:)
    type(dt_ty)              :: dts0(2)
    type(dt_ty), allocatable :: dts(:)
    integer                  :: nsecs, tunit

    dts0 = strptime ( datetimes )

    print '(a)', repeat('-', 79)
    print '(a)', 'seq_dt from '//dts0(1)%datetime//' to '//dts0(2)%datetime//' by '//by
    print '(a)', repeat('-', 79)

    dts = seq_dt ( dt_fr = dts0(1), dt_to = dts0(2), by = by )

    nsecs = dts0(2) - dts0(1)
    print '(1x, a)', [(dts(i)%date, i = 1, size(dts))]
    print '(1x, a, i0)', "nsecs: ", nsecs
    print '(1x, a, i0)', 'nunits in seq: ', nsecs / tunit
    print '(a)', ''

  end subroutine

end program
