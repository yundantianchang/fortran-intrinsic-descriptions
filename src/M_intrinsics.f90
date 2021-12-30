!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
module M_intrinsics
implicit none
private
public help_intrinsics
!interface help_intrinsics
!   module procedure help_intrinsics_all
!   module procedure help_intrinsics_one
!end interface help_intrinsics
contains
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
function help_intrinsics(name,prefix,topic,m_help) result (textblock)
character(len=*),intent(in)                       :: name
logical,intent(in),optional                       :: prefix
logical,intent(in),optional                       :: topic
logical,intent(in),optional                       :: m_help
character(len=256),allocatable                    :: textblock(:)
character(len=:),allocatable                      :: a, b, c
integer                                           :: i, p, pg
   select case(name)
   case('','manual','intrinsics','fortranmanual','fortran_manual')
      textblock=help_intrinsics_all(prefix,topic,m_help)
   case('fortran','toc')
      textblock=help_intrinsics_section()
      do i=1,size(textblock)
         p = index(textblock(i), '[')
         pg = index(textblock(i), ']')
         if(p.gt.0.and.pg.gt.p)then
          a=textblock(i)(:p-1)
          b=textblock(i)(p:pg)
          c=textblock(i)(pg+1:)
          textblock(i)=b//' '//a//c
         endif
      enddo
      call sort_name(textblock)
   case default
      textblock=help_intrinsics_one(name,prefix,topic,m_help)
   end select
end function help_intrinsics
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
function help_intrinsics_section() result (textblock)

!@(#) grab lines in NAME section and append them to generate an index of manpages

character(len=256),allocatable  :: textblock(:)
character(len=256),allocatable  :: add(:)
character(len=256),allocatable  :: label
character(len=10)               :: cnum
integer                         :: i
integer                         :: icount
logical                         :: is_label
logical                         :: grab
   allocate(textblock(0))
   icount=1
   do
      write(cnum,'(i0)') icount
      add=help_intrinsics_one(cnum)
      if( size(add) .eq. 0 ) exit
      label=''
      grab=.false.
      is_label=.false.
      do i=1,size(add)
         if(add(i).ne.'')then
            is_label=verify(add(i)(1:1),'ABCDEFGHIJKLMNOPQRSTUVWXYZ ') == 0 &
            .and. verify(trim(add(i)),'ABCDEFGHIJKLMNOPQRSTUVWXYZ ') == 0
         endif
         if(add(i).eq.'')then
            ! skip
         elseif(is_label.and.add(i).eq.'NAME')then
            grab=.true.
         elseif(is_label)then
            exit
         elseif(grab)then
            label=adjustl(trim(label))//' '//adjustl(compact(trim(add(i))))
         endif
      enddo
      textblock=[character(len=256) :: textblock,compact(label)]
      icount=icount + 1
   enddo
end function help_intrinsics_section
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
function help_intrinsics_all(prefix,topic,m_help) result (textblock)
logical,intent(in),optional     :: prefix
logical,intent(in),optional     :: topic
logical,intent(in),optional     :: m_help
character(len=256),allocatable  :: textblock(:)
character(len=256),allocatable  :: header(:)
character(len=256),allocatable  :: add(:)
character(len=10)               :: cnum
integer                         :: icount
   allocate(textblock(0))
   icount=1
   do
      write(cnum,'(i0)') icount
      add=help_intrinsics_one(cnum,prefix,topic,m_help)
      if( size(add) .eq. 0 ) exit
      textblock=[character(len=256) :: textblock,add]
      icount=icount + 1
   enddo
   if(present(m_help))then
      if(m_help)then
         header=[ character(len=256) :: &
         '================================================================================',    &
         'SUMMARY',    &
         ' The primary Fortran topics are',    &
         ' tan                   tanh                      this_image',    &
         '']
         textblock=[header,textblock]
      endif
   endif
end function help_intrinsics_all
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
function help_intrinsics_one(name,prefix,topic,m_help) result (textblock)
character(len=*),intent(in)      :: name
logical,intent(in),optional      :: prefix
logical,intent(in),optional      :: m_help
logical,intent(in),optional      :: topic
character(len=256),allocatable   :: textblock(:)
character(len=:),allocatable     :: shortname
integer                          :: i
select case(name)

case('1','abs')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'ABS(3) - [NUMERIC] Absolute value', &
'', &
'SYNTAX', &
'', &
'      result = abs(a)', &
'', &
'       TYPE(kind=KIND) elemental function abs(a)', &
'', &
'       TYPE(kind=KIND),intent(in) :: a', &
'', &
'where the TYPE and KIND is determined by the type and type attributes of', &
'A, which may be any _real_, _integer_, or _complex_ value.', &
'', &
'If the type of A is _cmplx_ the type returned will be _real_ with the', &
'same kind as the _real_ part of the input value.', &
'', &
'Otherwise the returned type will be the same type as A.', &
'', &
'DESCRIPTION', &
'', &
'ABS(A) computes the absolute value of numeric argument A.', &
'', &
'In mathematics, the absolute value or modulus of a real number X,', &
'denoted |X|, is the magnitude of X without regard to its sign.', &
'', &
'The absolute value of a number may be thought of as its distance from', &
'zero, which is the definition used by ABS(3) when dealing with _complex_', &
'values (_see below_).', &
'', &
'ARGUMENTS', &
'', &
'    A', &
'        the type of the argument shall be an _integer_, _real_, or', &
'        _complex_ scalar or array.', &
'', &
'RETURNS', &
'', &
'If A is of type _integer_ or _real_, the value of the result is |A| and', &
'of the same type and kind as the input argument.', &
'', &
'(Take particular note) if A is _complex_ with value (X, Y), the result', &
'is a _real_ equal to a processor-dependent approximation to SQRT(X**2 +', &
'Y**2) computed without undue overflow or underflow.', &
'', &
'EXAMPLES', &
'', &
'Sample program:', &
'', &
'    program demo_abs', &
'    implicit none', &
'    integer           :: i = -1', &
'    real              :: x = -1.0', &
'    complex           :: z = (-3.0,-4.0)', &
'    doubleprecision   :: rr = -45.78d+00', &
'    character(len=*),parameter :: &', &
'     frmt =  ''(1x,a15,1x," In: ",g0,            T51," Out: ",g0)'', &', &
'     frmtc = ''(1x,a15,1x," In: (",g0,",",g0,")",T51," Out: ",g0)''', &
'    integer,parameter :: dp=kind(0.0d0)', &
'    integer,parameter :: sp=kind(0.0)', &
'', &
'        write(*, frmt)  ''integer         '',  i, abs(i)', &
'        write(*, frmt)  ''real            '',  x, abs(x)', &
'        write(*, frmt)  ''doubleprecision '', rr, abs(rr)', &
'        write(*, frmtc) ''complex         '',  z, abs(z)', &
'        !', &
'        !', &
'        write(*, *)', &
'        write(*, *) ''abs is elemental: '', abs([20,  0,  -1,  -3,  100])', &
'        write(*, *)', &
'        write(*, *) ''abs range test : '', abs(huge(0)), abs(-huge(0))', &
'        write(*, *) ''abs range test : '', abs(huge(0.0)), abs(-huge(0.0))', &
'        write(*, *) ''abs range test : '', abs(tiny(0.0)), abs(-tiny(0.0))', &
'', &
'        write(*, *) ''returned real kind:'', cmplx(30.0_dp,40.0_dp,kind=dp), &', &
'                                      kind(cmplx(30.0_dp,40.0_dp,kind=dp))', &
'        write(*, *) ''returned real kind:'', cmplx(30.0_dp,40.0_dp),&', &
'                                      kind(cmplx(30.0_dp,40.0_dp))', &
'        write(*, *) ''returned real kind:'', cmplx(30.0_sp,40.0_sp),&', &
'                                      kind(cmplx(30.0_sp,40.0_sp))', &
'', &
'        write(*, *)', &
'        write(*, *) ''distance of <XX,YY> from zero is'', &', &
'                   & distance(30.0_dp,40.0_dp)', &
'', &
'        contains', &
'', &
'        real(kind=dp) elemental function distance(x,y)', &
'        real(kind=dp),intent(in) :: x,y', &
'           ! dusty corners:', &
'           ! note that KIND=DP is NOT optional', &
'           ! if the desired result is KIND=dp.', &
'           ! See cmplx(3).', &
'           distance=abs( cmplx(x,y,kind=dp) )', &
'        end function distance', &
'    end program demo_abs', &
'', &
'Results:', &
'', &
'        integer          In: -1                        Out: 1', &
'        real             In: -1.00000000               Out: 1.00000000', &
'        doubleprecision  In: -45.780000000000001       Out: 45.780000000000001', &
'        complex          In: (-3.00000000,-4.00000000) Out: 5.00000000', &
'', &
'        abs is elemental:     20     0     1     3   100', &
'', &
'        abs range test :   2147483647  2147483647', &
'        abs range test :    3.40282347E+38   3.40282347E+38', &
'        abs range test :    1.17549435E-38   1.17549435E-38', &
'        returned real kind: (30.000000000000000,40.000000000000000) 8', &
'        returned real kind: (30.0000000,40.0000000) 4', &
'        returned real kind: (30.0000000,40.0000000) 4', &
'', &
'        distance of <XX,YY> from zero is   50.000000000000000', &
'', &
'STANDARD', &
'', &
'FORTRAN 77 and later', &
'', &
'fortran-lang intrinsic descriptions (@urbanjost)', &
'']

shortname="abs"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('2','achar')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'ACHAR(3) - [CHARACTER:CONVERSION] returns a character in a specified', &
'position in the ASCII collating sequence', &
'', &
'SYNTAX', &
'', &
'      result = achar(i,kind=KIND)', &
'', &
'        character(len=1) elemental function :: achar(i,kind=KIND)', &
'', &
'        integer(kind=KIND),intent(in) :: i', &
'        integer(kind=KIND),intent(in),optional :: kind', &
'', &
'where KIND may be any supported kind value for _integer_ types.', &
'', &
'DESCRIPTION', &
'', &
'ACHAR(I) returns the character located at position I (commonly called', &
'the _ADE_ or ASCII Decimal Equivalent) in the ASCII collating sequence.', &
'', &
'The ACHAR(3) function is often used for generating in-band escape', &
'sequences to control terminal attributes.', &
'', &
'       write(*,''(*(a))'')achar(27),''[2J''', &
'', &
'will clear the screen on an ANSI-compatible terminal display, for', &
'example.', &
'', &
'ARGUMENTS', &
'', &
'    I', &
'        the _integer_ value to convert to an ASCII character, in the', &
'        range 0 to 127.', &
'', &
'    KIND', &
'        (optional) an _integer_ initialization expression indicating the', &
'        kind parameter of the result.', &
'', &
'RETURNS', &
'', &
'The return value is the requested character of type _character_ with a', &
'length of one. If the KIND argument is present, the return value is of', &
'the specified kind and of the default kind otherwise.', &
'', &
'EXAMPLES', &
'', &
'    program demo_achar', &
'    use,intrinsic::iso_fortran_env,only:int8,int16,int32,int64', &
'    implicit none', &
'    integer :: i', &
'       i=65', &
'       write(*,''("decimal     =",i0)'')i', &
'       write(*,''("character   =",a1)'')achar(i)', &
'       write(*,''("binary      =",b0)'')achar(i)', &
'       write(*,''("octal       =",o0)'')achar(i)', &
'       write(*,''("hexadecimal =",z0)'')achar(i)', &
'', &
'       write(*,''(8(i3,1x,a,1x),/)'')(i,achar(i), i=32,126)', &
'', &
'       write(*,''(a)'')upper(''Mixed Case'')', &
'    contains', &
'    ! a classic use of achar(3) is to convert the case of a string', &
'', &
'    elemental pure function upper(str) result (string)', &
'    !', &
'    !$@(#) upper(3f): function to return a trimmed uppercase-only string', &
'    !', &
'    ! input string to convert to all uppercase', &
'    character(*), intent(in)      :: str', &
'    ! output string that contains no miniscule letters', &
'    character(len(str))           :: string', &
'    integer                       :: i, iend', &
'    integer,parameter             :: toupper = iachar(''A'')-iachar(''a'')', &
'       iend=len_trim(str)', &
'       ! initialize output string to trimmed input string', &
'       string = str(:iend)', &
'       ! process each letter in the string', &
'       do concurrent (i = 1:iend)', &
'           select case (str(i:i))', &
'           ! located miniscule letter', &
'           case (''a'':''z'')', &
'              ! change miniscule to majuscule letter', &
'              string(i:i) = achar(iachar(str(i:i))+toupper)', &
'           end select', &
'       enddo', &
'    end function upper', &
'    end program demo_achar', &
'', &
'Results:', &
'', &
'       decimal     =65', &
'       character   =A', &
'       binary      =1000001', &
'       octal       =101', &
'       hexadecimal =41', &
'        32    33 !  34 "  35 #  36 $  37 %  38 &  39 ''', &
'', &
'        40 (  41 )  42 *  43 +  44 ,  45 -  46 .  47 /', &
'', &
'        48 0  49 1  50 2  51 3  52 4  53 5  54 6  55 7', &
'', &
'        56 8  57 9  58 :  59 ;  60 <  61 =  62 >  63 ?', &
'', &
'        64 @  65 A  66 B  67 C  68 D  69 E  70 F  71 G', &
'', &
'        72 H  73 I  74 J  75 K  76 L  77 M  78 N  79 O', &
'', &
'        80 P  81 Q  82 R  83 S  84 T  85 U  86 V  87 W', &
'', &
'        88 X  89 Y  90 Z  91 [  92 \  93 ]  94 ^  95 _', &
'', &
'        96 `  97 a  98 b  99 c 100 d 101 e 102 f 103 g', &
'', &
'       104 h 105 i 106 j 107 k 108 l 109 m 110 n 111 o', &
'', &
'       112 p 113 q 114 r 115 s 116 t 117 u 118 v 119 w', &
'', &
'       120 x 121 y 122 z 123 { 124 | 125 } 126 ~', &
'       MIXED CASE', &
'', &
'NOTE', &
'', &
'The ADEs (ASCII Decimal Equivalents) for ASCII are', &
'', &
'    *-------*-------*-------*-------*-------*-------*-------*-------*', &
'    | 00 nul| 01 soh| 02 stx| 03 etx| 04 eot| 05 enq| 06 ack| 07 bel|', &
'    | 08 bs | 09 ht | 10 nl | 11 vt | 12 np | 13 cr | 14 so | 15 si |', &
'    | 16 dle| 17 dc1| 18 dc2| 19 dc3| 20 dc4| 21 nak| 22 syn| 23 etb|', &
'    | 24 can| 25 em | 26 sub| 27 esc| 28 fs | 29 gs | 30 rs | 31 us |', &
'    | 32 sp | 33  ! | 34  " | 35  # | 36  $ | 37  % | 38  & | 39  '' |', &
'    | 40  ( | 41  ) | 42  * | 43  + | 44  , | 45  - | 46  . | 47  / |', &
'    | 48  0 | 49  1 | 50  2 | 51  3 | 52  4 | 53  5 | 54  6 | 55  7 |', &
'    | 56  8 | 57  9 | 58  : | 59  ; | 60  < | 61  = | 62  > | 63  ? |', &
'    | 64  @ | 65  A | 66  B | 67  C | 68  D | 69  E | 70  F | 71  G |', &
'    | 72  H | 73  I | 74  J | 75  K | 76  L | 77  M | 78  N | 79  O |', &
'    | 80  P | 81  Q | 82  R | 83  S | 84  T | 85  U | 86  V | 87  W |', &
'    | 88  X | 89  Y | 90  Z | 91  [ | 92  \ | 93  ] | 94  ^ | 95  _ |', &
'    | 96  ` | 97  a | 98  b | 99  c |100  d |101  e |102  f |103  g |', &
'    |104  h |105  i |106  j |107  k |108  l |109  m |110  n |111  o |', &
'    |112  p |113  q |114  r |115  s |116  t |117  u |118  v |119  w |', &
'    |120  x |121  y |122  z |123  { |124  | |125  } |126  ~ |127 del|', &
'    *-------*-------*-------*-------*-------*-------*-------*-------*', &
'', &
'STANDARD', &
'', &
'FORTRAN 77 and later, with KIND argument Fortran 2003 and later', &
'', &
'SEE ALSO', &
'', &
'CHAR(3), IACHAR(3), ICHAR(3)', &
'', &
'RESOURCES', &
'', &
'-   ANSI escape sequences', &
'-   M_attr module for controlling ANSI-compatible terminals', &
'', &
'fortran-lang intrinsic descriptions (@urbanjost)', &
'']

shortname="achar"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('3','acos')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'ACOS(3) - [MATHEMATICS:TRIGONOMETRIC] arccosine (inverse cosine)', &
'function', &
'', &
'SYNTAX', &
'', &
'      result = acos(x)', &
'', &
'       TYPE(kind=KIND),elemental :: acos', &
'', &
'       TYPE(kind=KIND,intent(in) :: x', &
'', &
'where TYPE may be _real_ or _complex_ and KIND may be any KIND supported', &
'by the associated type.', &
'', &
'DESCRIPTION', &
'', &
'ACOS(X) computes the arccosine of X (inverse of COS(X)).', &
'', &
'ARGUMENTS', &
'', &
'    X', &
'        Must be type _real_ or _complex_. If the type is _real_, the', &
'        value must satisfy |X| <= 1.', &
'', &
'RETURNS', &
'', &
'The return value is of the same type and kind as X. The _real_ part of', &
'the result is in radians and lies in the range 0 <= ACOS(X%RE) <= PI .', &
'', &
'EXAMPLES', &
'', &
'Sample program:', &
'', &
'    program demo_acos', &
'    use, intrinsic :: iso_fortran_env, only : real_kinds,real32,real64,real128', &
'    implicit none', &
'    character(len=*),parameter :: all=''(*(g0,1x))''', &
'    real(kind=real64) :: x = 0.866_real64', &
'    real(kind=real64),parameter :: d2r=acos(-1.0_real64)/180.0_real64', &
'', &
'        print all,''acos('',x,'') is '', acos(x)', &
'        print all,''90 degrees is '', d2r*90.0_real64, '' radians''', &
'        print all,''180 degrees is '', d2r*180.0_real64, '' radians''', &
'        print all,''for reference &', &
'        &PI ~ 3.14159265358979323846264338327950288419716939937510''', &
'        print all,''elemental'',acos([-1.0,-0.5,0.0,0.50,1.0])', &
'', &
'    end program demo_acos', &
'', &
'Results:', &
'', &
'       acos( .8660000000000000 ) is  .5236495809318289', &
'       90 degrees is  1.570796326794897  radians', &
'       180 degrees is  3.141592653589793  radians', &
'       for reference PI ~ 3.14159265358979323846264338327950288419716939937510', &
'       elemental 3.141593 2.094395 1.570796 1.047198 .000000', &
'', &
'STANDARD', &
'', &
'FORTRAN 77 and later; for a _complex_ argument - Fortran 2008 and later', &
'', &
'SEE ALSO', &
'', &
'-   wikipedia: inverse trigonometric functions', &
'', &
'Inverse function: COS(3)', &
'', &
'fortran-lang intrinsic descriptions (@urbanjost)', &
'']

shortname="acos"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('4','acosh')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'ACOSH(3) - [MATHEMATICS:TRIGONOMETRIC] Inverse hyperbolic cosine', &
'function', &
'', &
'SYNTAX', &
'', &
'      result = acosh(x)', &
'', &
'       TYPE(kind=KIND),elemental :: acosh', &
'', &
'       TYPE(kind=KIND,intent(in) :: x', &
'', &
'where TYPE may be _real_ or _complex_ and KIND may be any KIND supported', &
'by the associated type.', &
'', &
'DESCRIPTION', &
'', &
'ACOSH(X) computes the inverse hyperbolic cosine of X in radians.', &
'', &
'ARGUMENTS', &
'', &
'    X', &
'        the type shall be _real_ or _complex_.', &
'', &
'RETURNS', &
'', &
'The return value has the same type and kind as X.', &
'', &
'If X is _complex_, the imaginary part of the result is in radians and', &
'lies between', &
'', &
'  0 <= AIMAG(ACOSH(X)) <= PI', &
'', &
'EXAMPLES', &
'', &
'Sample program:', &
'', &
'    program demo_acosh', &
'    use,intrinsic :: iso_fortran_env, only : dp=>real64,sp=>real32', &
'    implicit none', &
'    real(kind=dp), dimension(3) :: x = [ 1.0d0, 2.0d0, 3.0d0 ]', &
'       write (*,*) acosh(x)', &
'    end program demo_acosh', &
'', &
'Results:', &
'', &
'     0.000000000000000E+000   1.31695789692482        1.76274717403909', &
'', &
'STANDARD', &
'', &
'Fortran 2008 and later', &
'', &
'SEE ALSO', &
'', &
'-   Wikipedia:hyperbolic functions', &
'', &
'Inverse function: COSH(3)', &
'', &
'fortran-lang intrinsic descriptions (@urbanjost)', &
'']

shortname="acosh"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('5','adjustl')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'ADJUSTL(3) - [CHARACTER:WHITESPACE] Left-adjust a string', &
'', &
'SYNTAX', &
'', &
'        result = adjustl(string)', &
'', &
'         character(len=(len(string)) elemental function adjustr(a)', &
'', &
'         character(len=*),intent(in) :: string', &
'', &
'DESCRIPTION', &
'', &
'ADJUSTL(STRING) will left-adjust a string by removing leading spaces.', &
'Spaces are inserted at the end of the string as needed.', &
'', &
'ARGUMENTS', &
'', &
'    STRING', &
'        the type shall be _character_.', &
'', &
'RETURNS', &
'', &
'The return value is of type _character_ and of the same kind as STRING', &
'where leading spaces are removed and the same number of spaces are', &
'inserted on the end of STRING.', &
'', &
'EXAMPLES', &
'', &
'Sample program:', &
'', &
'    program demo_adjustl', &
'    implicit none', &
'    character(len=20) :: str = ''   sample string''', &
'    character(len=:),allocatable :: astr', &
'        !', &
'        ! basic use', &
'        str = adjustl(str)', &
'        write(*,''("[",a,"]")'') str, trim(str)', &
'        !', &
'        ! an allocatable string stays the same length', &
'        ! and is not trimmed.', &
'        astr=''    allocatable string   ''', &
'        write(*,''("[",a,"]")'') adjustl(astr)', &
'        !', &
'    end program demo_adjustl', &
'', &
'Results:', &
'', &
'       [sample string       ]', &
'       [sample string]', &
'       [allocatable string       ]', &
'', &
'STANDARD', &
'', &
'Fortran 95 and later', &
'', &
'SEE ALSO', &
'', &
'ADJUSTR(3)', &
'', &
'fortran-lang intrinsic descriptions (@urbanjost)', &
'']

shortname="adjustl"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('6','adjustr')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'ADJUSTR(3) - [CHARACTER:WHITESPACE] Right-adjust a string', &
'', &
'SYNTAX', &
'', &
'        result = adjustr(string)', &
'', &
'         character(len=(len(string)) elemental function adjustr(a)', &
'', &
'         character(len=*),intent(in) :: string', &
'', &
'DESCRIPTION', &
'', &
'ADJUSTR(STRING) will right-adjust a string by removing trailing spaces.', &
'Spaces are inserted at the start of the string as needed.', &
'', &
'ARGUMENTS', &
'', &
'    STRING', &
'        the type shall be _character_.', &
'', &
'RETURNS', &
'', &
'The return value is of type _character_ and of the same kind as STRING', &
'where trailing spaces are removed and the same number of spaces are', &
'inserted at the start of STRING.', &
'', &
'EXAMPLES', &
'', &
'Sample program:', &
'', &
'    program demo_adjustr', &
'    implicit none', &
'    integer :: right', &
'    character(len=*),parameter :: bracket=''("[",a,"]")''', &
'    character(len=20) :: str = '' sample string ''', &
'    character(len=:),allocatable :: astr', &
'       call number_line()', &
'       !', &
'       ! basic usage', &
'       str = adjustr(str)', &
'       write(*,bracket) str', &
'', &
'       ! exploring usage:', &
'       ! An allocatable string and arbitrary margin.', &
'       ! Set a right margin and adjust to it. Note', &
'       ! this would truncate if the margin is less', &
'       ! than the length of STR', &
'       right=50', &
'       astr=adjustr(str//repeat('' '',max(0,right-len(str))))', &
'       write(*,bracket) astr', &
'       !', &
'       call number_line()', &
'       !', &
'    contains', &
'       subroutine number_line()', &
'       ! print a short number line', &
'          write(*,bracket)repeat(''1234567890'',5)', &
'       end subroutine number_line', &
'    end program demo_adjustr', &
'', &
'Results:', &
'', &
'       [12345678901234567890123456789012345678901234567890]', &
'       [       sample string]', &
'       [                                     sample string]', &
'       [12345678901234567890123456789012345678901234567890]', &
'', &
'STANDARD', &
'', &
'Fortran 95 and later', &
'', &
'SEE ALSO', &
'', &
'ADJUSTL(3)', &
'', &
'fortran-lang intrinsic descriptions (@urbanjost)', &
'']

shortname="adjustr"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('7','aimag')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'AIMAG(3) - [TYPE:NUMERIC] Imaginary part of complex number (GFDL)', &
'', &
'SYNTAX', &
'', &
'        result = aimag(z)', &
'', &
'         complex(kind=KIND),elemental :: aimag', &
'', &
'         complex(kind=KIND),intent(in) :: z', &
'', &
'DESCRIPTION', &
'', &
'AIMAG(Z) yields the imaginary part of complex argument Z.', &
'', &
'ARGUMENTS', &
'', &
'    Z', &
'        The type of the argument shall be _complex_.', &
'', &
'RETURNS', &
'', &
'The return value is of type _real_ with the kind type parameter of the', &
'argument.', &
'', &
'EXAMPLES', &
'', &
'Sample program:', &
'', &
'    program demo_aimag', &
'    use, intrinsic :: iso_fortran_env, only : real_kinds, &', &
'     & real32, real64, real128', &
'    implicit none', &
'    complex(kind=real32) z4', &
'    complex(kind=real64) z8', &
'        z4 = cmplx(1.e0, 2.e0)', &
'        z8 = cmplx(3.e0_real64, 4.e0_real64,kind=real64)', &
'        print *, aimag(z4), aimag(z8)', &
'        ! an elemental function can be passed an array', &
'        print *', &
'        print *, [z4,z4/2.0,z4+z4,z4**3]', &
'        print *', &
'        print *, aimag([z4,z4/2.0,z4+z4,z4**3])', &
'    end program demo_aimag', &
'', &
'Results:', &
'', &
'      2.000000       4.00000000000000', &
'', &
'     (1.000000,2.000000) (0.5000000,1.000000) (2.000000,4.000000)', &
'     (-11.00000,-2.000000)', &
'', &
'           2.000000       1.000000       4.000000      -2.000000', &
'', &
'STANDARD', &
'', &
'FORTRAN 77 and later', &
'', &
'fortran-lang intrinsic descriptions', &
'']

shortname="aimag"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('8','aint')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'AINT(3) - [NUMERIC] Truncate to a whole number (GFDL)', &
'', &
'SYNTAX', &
'', &
'    result = aint(x)', &
'', &
'       real(kind=kind(x)),elemental  :: aint', &
'', &
'       real(kind=kind(x)),intent(in) :: x', &
'', &
'or', &
'', &
'    result = aint(x, KIND)', &
'', &
'       real(kind=KIND),elemental     :: aint', &
'', &
'       integer,intent(in),optional   :: KIND', &
'       real(kind=kind(x)),intent(in) :: x', &
'', &
'DESCRIPTION', &
'', &
'AINT(X, KIND) truncates its argument to a whole number.', &
'', &
'ARGUMENTS', &
'', &
'    X', &
'        the type of the argument shall be _real_.', &
'', &
'    KIND', &
'        (optional) an _integer_ initialization expression indicating the', &
'        kind parameter of the result.', &
'', &
'RETURNS', &
'', &
'The return value is of type _real_ with the kind type parameter of the', &
'argument if the optional KIND is absent; otherwise, the kind type', &
'parameter will be given by KIND. If the magnitude of X is less than one,', &
'AINT(X) returns zero. If the magnitude is equal to or greater than one', &
'then it returns the largest whole number that does not exceed its', &
'magnitude. The sign is the same as the sign of X.', &
'', &
'EXAMPLES', &
'', &
'Sample program:', &
'', &
'    program demo_aint', &
'    use, intrinsic :: iso_fortran_env, only : real32, real64', &
'    implicit none', &
'    real(kind=real32) :: x4', &
'    real(kind=real64) :: x8', &
'', &
'       x4 = 4.3210_real32', &
'       x8 = 4.3210_real64', &
'       print *, aint(x4), aint(x8)', &
'       print *', &
'       ! elemental', &
'       print *,aint([ &', &
'        &  -2.7,  -2.5, -2.2, -2.0, -1.5, -1.0, -0.5, &', &
'        &  0.0,   &', &
'        &  +0.5,  +1.0, +1.5, +2.0, +2.2, +2.5, +2.7  ])', &
'', &
'    end program demo_aint', &
'', &
'Results:', &
'', &
'         4.00000000       4.0000000000000000', &
'', &
'        -2.00000000      -2.00000000      -2.00000000      -2.00000000', &
'        -1.00000000      -1.00000000      -0.00000000       0.00000000', &
'         0.00000000       1.00000000       1.00000000       2.00000000', &
'         2.00000000       2.00000000       2.00000000', &
'', &
'STANDARD', &
'', &
'FORTRAN 77 and later', &
'', &
'SEE ALSO', &
'', &
'ANINT(3), INT(3), NINT(3), SELECTED_INT_KIND(3), CEILING(3), FLOOR(3)', &
'', &
'fortran-lang intrinsic descriptions', &
'']

shortname="aint"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('9','all')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'ALL(3) - [ARRAY REDUCTION] determines if all the values are true (GFDL)', &
'', &
'SYNTAX', &
'', &
'    result = all(mask, dim)', &
'', &
'DESCRIPTION', &
'', &
'Logical conjunction of elements of MASK along dimension DIM.', &
'', &
'"ALL(MASK, DIM)" determines if all the values are true in MASK in the', &
'array along dimension DIM.', &
'', &
'ARGUMENTS', &
'', &
'    MASK', &
'        shall be a logical array. That is, the type of the argument', &
'        shall be _logical_ and it shall not be scalar.', &
'', &
'    DIM', &
'        (optional) DIM shall be a scalar integer with a value that lies', &
'        between one and the rank of MASK. The corresponding actual', &
'        argument shall not be an optional dummy argument.', &
'', &
'RETURNS', &
'', &
'"ALL(MASK)" returns a scalar value of type _logical_ where the kind type', &
'parameter is the same as the kind type parameter of MASK. If DIM is', &
'present, then ALL(MASK, DIM) returns an array with the rank of MASK', &
'minus 1. The shape is determined from the shape of MASK where the DIM', &
'dimension is elided.', &
'', &
'1.  ALL(MASK) is true if all elements of MASK are true. It also is true', &
'    if MASK has zero size; otherwise, it is false.', &
'', &
'2.  If the rank of MASK is one, then ALL(MASK, DIM) is equivalent to', &
'    ALL(MASK). If the rank is greater than one, then ALL(MASK, DIM) is', &
'    determined by applying ALL() to the array sections.', &
'', &
'3.  Result Characteristics. The result is of type _logical_ with the', &
'    same kind type parameter as MASK. It is scalar if DIM is absent or N', &
'    = 1; otherwise, the result has rank N - 1 and shape [D1 , D2 , . . .', &
'    , DDIM-1 , DDIM+1 , . . . , DN ] where [D1 , D2 , . . . , DN ] is', &
'    the shape of MASK.', &
'', &
'4.  Result Value.', &
'', &
'    Case (i): : The result of ALL(MASK) has the value true if all', &
'    elements of MASK are true or if MASK has size zero, and the result', &
'    has value false if any element of MASK is false.', &
'', &
'    Case (ii): : If MASK has rank one, ALL(MASK,DIM) is equal to', &
'    ALL(MASK). Otherwise, the value of element (S1 , S2 , . . . , SDIM-1', &
'    , SDIM+1 , . . . , SN ) of all (MASK, DIM) is equal to ALL(MASK (S1', &
'    , S2 , . . . , SDIM-1 , :, SDIM+1 , . . . , SN )).', &
'', &
'EXAMPLES', &
'', &
'Sample program:', &
'', &
'    program demo_all', &
'    implicit none', &
'    logical l', &
'       l = all([.true., .true., .true.])', &
'       print *, l', &
'       call section', &
'', &
'    contains', &
'', &
'    subroutine section', &
'    integer a(2,3), b(2,3)', &
'      a = 1', &
'      b = 1', &
'      b(2,2) = 2', &
'      print *, all(a .eq. b, 1)', &
'      print *, all(a .eq. b, 2)', &
'    end subroutine section', &
'    end program demo_all', &
'', &
'Results:', &
'', &
'        T', &
'        T F T', &
'        T F', &
'', &
'Case (i):', &
'', &
'         The value of all([.TRUE., .FALSE., .TRUE.]) is false.', &
'', &
'Case (ii):', &
'', &
'                              1|3|5', &
'       If B is the array      -+-+-', &
'                              2|4|6', &
'', &
'                              0|3|5', &
'       and C is the array     -+-+-', &
'                              7|4|8', &
'', &
'       then all(B /= C, DIM = 1) is', &
'', &
'          [true, false, false]', &
'', &
'and ALL(B /= C, DIM = 2) is', &
'', &
'            [false, false].', &
'', &
'STANDARD', &
'', &
'Fortran 95 and later', &
'', &
'fortran-lang intrinsic descriptions', &
'']

shortname="all"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('10','allocated')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'ALLOCATED(3) - [ARRAY INQUIRY] Status of an allocatable entity (GFDL)', &
'', &
'SYNTAX', &
'', &
'      result = allocated(array)', &
'', &
'or', &
'', &
'      result = allocated(scalar)', &
'', &
'DESCRIPTION', &
'', &
'ALLOCATED(ARRAY) and ALLOCATED(SCALAR) check the allocation status of', &
'ARRAY and SCALAR, respectively.', &
'', &
'ARGUMENTS', &
'', &
'    ARRAY', &
'        the argument shall be an _allocatable_ array.', &
'', &
'    SCALAR', &
'        the argument shall be an _allocatable_ scalar.', &
'', &
'RETURNS', &
'', &
'The return value is a scalar _logical_ with the default logical kind', &
'type parameter. If the argument is allocated then the result is .true.;', &
'otherwise, it returns .false..', &
'', &
'EXAMPLES', &
'', &
'Sample program:', &
'', &
'    program demo_allocated', &
'    use,intrinsic :: iso_fortran_env, only : dp=>real64,sp=>real32', &
'    implicit none', &
'    integer :: i = 4', &
'    real(kind=sp), allocatable :: x(:)', &
'', &
'       if (allocated(x) .eqv. .false.) allocate(x(i))', &
'', &
'    end program demo_allocated', &
'', &
'STANDARD', &
'', &
'Fortran 95 and later. Note, the scalar= keyword and allocatable scalar', &
'entities are available in Fortran 2003 and later.', &
'', &
'SEE ALSO', &
'', &
'MOVE_ALLOC(3)', &
'', &
'fortran-lang intrinsic descriptions', &
'']

shortname="allocated"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('11','anint')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'ANINT(3) - [NUMERIC] Nearest whole number (GFDL)', &
'', &
'SYNTAX', &
'', &
'    result = anint(a, kind)', &
'', &
'DESCRIPTION', &
'', &
'ANINT(A [, KIND]) rounds its argument to the nearest whole number.', &
'', &
'ARGUMENTS', &
'', &
'    A', &
'        the type of the argument shall be _real_.', &
'', &
'    KIND', &
'        (optional) an _integer_ initialization expression indicating the', &
'        kind parameter of the result.', &
'', &
'RETURNS', &
'', &
'The return value is of type real with the kind type parameter of the', &
'argument if the optional KIND is absent; otherwise, the kind type', &
'parameter will be given by KIND. If A is greater than zero, ANINT(A)', &
'returns AINT(A + 0.5). If A is less than or equal to zero then it', &
'returns AINT(A - 0.5).', &
'', &
'EXAMPLES', &
'', &
'Sample program:', &
'', &
'    program demo_anint', &
'    use, intrinsic :: iso_fortran_env, only : real_kinds, &', &
'    & real32, real64, real128', &
'    implicit none', &
'    real(kind=real32) :: x4', &
'    real(kind=real64) :: x8', &
'', &
'       x4 = 1.234E0_real32', &
'       x8 = 4.321_real64', &
'       print *, anint(x4), dnint(x8)', &
'       x8 = anint(x4,kind=real64)', &
'       print *, x8', &
'       print *', &
'       ! elemental', &
'       print *,anint([ &', &
'        & -2.7,  -2.5, -2.2, -2.0, -1.5, -1.0, -0.5, &', &
'        &  0.0, &', &
'        & +0.5,  +1.0, +1.5, +2.0, +2.2, +2.5, +2.7  ])', &
'', &
'    end program demo_anint', &
'', &
'Results:', &
'', &
'        1.00000000       4.0000000000000000', &
'        1.0000000000000000', &
'', &
'       -3.00000000      -3.00000000      -2.00000000      -2.00000000', &
'       -2.00000000      -1.00000000      -1.00000000       0.00000000', &
'        1.00000000       1.00000000       2.00000000       2.00000000', &
'        2.00000000       3.00000000       3.00000000', &
'', &
'STANDARD', &
'', &
'FORTRAN 77 and later', &
'', &
'SEE ALSO', &
'', &
'AINT(3), INT(3), NINT(3), SELECTED_INT_KIND(3), CEILING(3), FLOOR(3)', &
'', &
'fortran-lang intrinsic descriptions', &
'']

shortname="anint"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('12','any')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'ANY(3) - [ARRAY REDUCTION] determines if any of the values in the', &
'logical array are true. (GFDL)', &
'', &
'SYNTAX', &
'', &
'    result = any(mask, dim)', &
'', &
'DESCRIPTION', &
'', &
'ANY(MASK, DIM) determines if any of the values in the logical array MASK', &
'along dimension DIM are .TRUE..', &
'', &
'ARGUMENTS', &
'', &
'    MASK', &
'        the type of the argument shall be _logical_ and it shall not be', &
'        scalar.', &
'', &
'    DIM', &
'        (optional) dim shall be a scalar integer with a value that lies', &
'        between one and the rank of mask.', &
'', &
'RETURNS', &
'', &
'ANY(MASK) returns a scalar value of type _logical_ where the kind type', &
'parameter is the same as the kind type parameter of MASK. If DIM is', &
'present, then ANY(MASK, DIM) returns an array with the rank of MASK', &
'minus 1. The shape is determined from the shape of MASK where the DIM', &
'dimension is elided.', &
'', &
'1.  ANY(MASK) is true if any element of MASK is true; otherwise, it is', &
'    .FALSE.. It also is false if MASK has zero size.', &
'', &
'2.  If the rank of MASK is one, then ANY(MASK, DIM) is equivalent to', &
'    ANY(MASK). If the rank is greater than one, then ANY(MASK, DIM) is', &
'    determined by applying ANY() to the array sections.', &
'', &
'EXAMPLES', &
'', &
'Sample program:', &
'', &
'    program demo_any', &
'    implicit none', &
'    logical l', &
'       l = any([.true., .true., .true.])', &
'       print *, l', &
'       call section', &
'       contains', &
'         subroutine section', &
'         integer a(2,3), b(2,3)', &
'           a = 1', &
'           b = 1', &
'           b(2,2) = 2', &
'           print *, any(a .eq. b, 1)', &
'           print *, any(a .eq. b, 2)', &
'         end subroutine section', &
'    end program demo_any', &
'', &
'Results:', &
'', &
'        T', &
'        T T T', &
'        T T', &
'', &
'STANDARD', &
'', &
'Fortran 95 and later', &
'', &
'fortran-lang intrinsic descriptions', &
'']

shortname="any"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('13','asin')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'ASIN(3) - [MATHEMATICS:TRIGONOMETRIC] Arcsine function', &
'', &
'SYNTAX', &
'', &
'    result = asin(x)', &
'', &
'        elemental TYPE(kind=KIND) function asin(x)', &
'        TYPE(kind=KIND) :: x', &
'', &
'where the returned value has the kind of the input value and TYPE may be', &
'_real_ or _complex_', &
'', &
'DESCRIPTION', &
'', &
'ASIN(X) computes the arcsine of its argument X.', &
'', &
'The arcsine is the inverse function of the sine function. It is commonly', &
'used in trigonometry when trying to find the angle when the lengths of', &
'the hypotenuse and the opposite side of a right triangle are known.', &
'', &
'ARGUMENTS', &
'', &
'    X', &
'        The type shall be either _real_ and a magnitude that is less', &
'        than or equal to one; or be _complex_.', &
'', &
'RETURNS', &
'', &
'    RESULT', &
'        The return value is of the same type and kind as X. The real', &
'        part of the result is in radians and lies in the range -PI/2 <=', &
'        ASIN(X) <= PI/2.', &
'', &
'EXAMPLES', &
'', &
'The arcsine will allow you to find the measure of a right angle when you', &
'know the ratio of the side opposite the angle to the hypotenuse.', &
'', &
'So if you knew that a train track rose 1.25 vertical miles on a track', &
'that was 50 miles long, you could determine the average angle of incline', &
'of the track using the arcsine. Given', &
'', &
'     sin(theta) = 1.25 miles/50 miles (opposite/hypotenuse)', &
'', &
'    program demo_asin', &
'    use, intrinsic :: iso_fortran_env, only : dp=>real64', &
'    implicit none', &
'    ! value to convert degrees to radians', &
'    real(kind=dp),parameter :: D2R=acos(-1.0_dp)/180.0_dp', &
'    real(kind=dp)           :: angle, rise, run', &
'    character(len=*),parameter :: all=''(*(g0,1x))''', &
'      ! given sine(theta) = 1.25 miles/50 miles (opposite/hypotenuse)', &
'      ! then taking the arcsine of both sides of the equality yields', &
'      ! theta = arcsine(1.25 miles/50 miles) ie. arcsine(opposite/hypotenuse)', &
'      rise=1.250_dp', &
'      run=50.00_dp', &
'      angle = asin(rise/run)', &
'      print all, ''angle of incline(radians) = '', angle', &
'      angle = angle/D2R', &
'      print all, ''angle of incline(degrees) = '', angle', &
'', &
'      print all, ''percent grade='',rise/run*100.0_dp', &
'    end program demo_asin', &
'', &
'Results:', &
'', &
'        angle of incline(radians) =    2.5002604899361139E-002', &
'        angle of incline(degrees) =    1.4325437375665075', &
'        percent grade=   2.5000000000000000', &
'', &
'The percentage grade is the slope, written as a percent. To calculate', &
'the slope you divide the rise by the run. In the example the rise is', &
'1.25 mile over a run of 50 miles so the slope is 1.25/50 = 0.025.', &
'Written as a percent this is 2.5 %.', &
'', &
'For the US, two and 1/2 percent is generally thought of as the upper', &
'limit. This means a rise of 2.5 feet when going 100 feet forward. In the', &
'US this was the maximum grade on the first major US railroad, the', &
'Baltimore and Ohio. Note curves increase the frictional drag on a train', &
'reducing the allowable grade.', &
'', &
'STANDARD', &
'', &
'FORTRAN 77 and later, for a complex argument Fortran 2008 or later', &
'', &
'SEE ALSO', &
'', &
'-   wikipedia: inverse trigonometric functions', &
'', &
'Inverse function: SIN(3)', &
'', &
'fortran-lang intrinsic descriptions (@urbanjost)', &
'']

shortname="asin"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('14','asinh')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'ASINH(3) - [MATHEMATICS:TRIGONOMETRIC] Inverse hyperbolic sine function', &
'(GFDL)', &
'', &
'SYNTAX', &
'', &
'    result = asinh(x)', &
'', &
'        elemental TYPE(kind=KIND) function asinh(x)', &
'        TYPE(kind=KIND) :: x', &
'', &
'Where the returned value has the kind of the input value and TYPE may be', &
'_real_ or _complex_', &
'', &
'DESCRIPTION', &
'', &
'ASINH(X) computes the inverse hyperbolic sine of X.', &
'', &
'ARGUMENTS', &
'', &
'    X', &
'        The type shall be _real_ or _complex_.', &
'', &
'RETURNS', &
'', &
'The return value is of the same type and kind as X. If X is _complex_,', &
'the imaginary part of the result is in radians and lies between -PI/2 <=', &
'AIMAG(ASINH(X)) <= PI/2.', &
'', &
'EXAMPLES', &
'', &
'Sample program:', &
'', &
'    program demo_asinh', &
'    use,intrinsic :: iso_fortran_env, only : dp=>real64,sp=>real32', &
'    implicit none', &
'    real(kind=dp), dimension(3) :: x = [ -1.0d0, 0.0d0, 1.0d0 ]', &
'', &
'        write (*,*) asinh(x)', &
'', &
'    end program demo_asinh', &
'', &
'Results:', &
'', &
'      -0.88137358701954305  0.0000000000000000  0.88137358701954305', &
'', &
'STANDARD', &
'', &
'Fortran 2008 and later', &
'', &
'SEE ALSO', &
'', &
'-   Wikipedia:hyperbolic functions', &
'', &
'Inverse function: SINH(3)', &
'', &
'fortran-lang intrinsic descriptions', &
'']

shortname="asinh"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('15','associated')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'ASSOCIATED(3) - [STATE] Status of a pointer or pointer/target pair', &
'(GFDL)', &
'', &
'SYNTAX', &
'', &
'    result = associated(pointer, target)', &
'', &
'DESCRIPTION', &
'', &
'ASSOCIATED(POINTER [, TARGET]) determines the status of the pointer', &
'POINTER or if POINTER is associated with the target TARGET.', &
'', &
'ARGUMENTS', &
'', &
'    POINTER', &
'        POINTER shall have the _pointer_ attribute and it can be of any', &
'        type.', &
'', &
'    TARGET', &
'        (Optional) TARGET shall be a pointer or a target. It must have', &
'        the same type, kind type parameter, and array rank as POINTER.', &
'', &
'The association status of neither POINTER nor TARGET shall be undefined.', &
'', &
'RETURNS', &
'', &
'ASSOCIATED(POINTER) returns a scalar value of type _logical_. There are', &
'several cases:', &
'', &
'1.  When the optional TARGET is not present then ASSOCIATED(POINTER) is', &
'    true if POINTER is associated with a target; otherwise, it returns', &
'    false.', &
'', &
'2.  If TARGET is present and a scalar target, the result is true if', &
'    TARGET is not a zero-sized storage sequence and the target', &
'    associated with POINTER occupies the same storage units. If POINTER', &
'    is disassociated, the result is false.', &
'', &
'3.  If TARGET is present and an array target, the result is true if', &
'    TARGET and POINTER have the same shape, are not zero-sized arrays,', &
'    are arrays whose elements are not zero-sized storage sequences, and', &
'    TARGET and POINTER occupy the same storage units in array element', &
'    order.', &
'', &
'    As in case 2, the result is false, if POINTER is disassociated.', &
'', &
'4.  If TARGET is present and an scalar pointer, the result is true if', &
'    TARGET is associated with POINTER, the target associated with TARGET', &
'    are not zero-sized storage sequences and occupy the same storage', &
'    units.', &
'', &
'    The result is .FALSE., if either TARGET or POINTER is disassociated.', &
'', &
'5.  If TARGET is present and an array pointer, the result is true if', &
'    target associated with POINTER and the target associated with TARGET', &
'    have the same shape, are not zero-sized arrays, are arrays whose', &
'    elements are not zero-sized storage sequences, and TARGET and', &
'    POINTER occupy the same storage units in array element order. The', &
'    result is false, if either TARGET or POINTER is disassociated.', &
'', &
'EXAMPLES', &
'', &
'Sample program:', &
'', &
'    program demo_associated', &
'    implicit none', &
'    real, target  :: tgt(2) = [1., 2.]', &
'    real, pointer :: ptr(:)', &
'       ptr => tgt', &
'       if (associated(ptr)     .eqv. .false.) &', &
'       & stop ''POINTER NOT ASSOCIATED''', &
'       if (associated(ptr,tgt) .eqv. .false.) &', &
'       & stop ''POINTER NOT ASSOCIATED TO TARGET''', &
'    end program demo_associated', &
'', &
'STANDARD', &
'', &
'Fortran 95 and later', &
'', &
'SEE ALSO', &
'', &
'NULL(3)', &
'', &
'fortran-lang intrinsic descriptions', &
'']

shortname="associated"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('16','atan2')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'ATAN2(3) - [MATHEMATICS:TRIGONOMETRIC] Arctangent function', &
'', &
'SYNTAX', &
'', &
'    result = atan2(y, x)', &
'', &
'DESCRIPTION', &
'', &
'ATAN2(Y, X) computes the arctangent of the complex number ( X + i Y ) .', &
'', &
'This function can be used to transform from Cartesian into polar', &
'coordinates and allows to determine the angle in the correct quadrant.', &
'To convert from Cartesian Coordinates (X,Y) to polar coordinates', &
'', &
'(r,theta): $$ \begin{aligned} r &= \sqrt{x2 + Y2} \ \theta &=', &
'\tan**{-1}(y / x) \end{aligned} $$', &
'', &
'ARGUMENTS', &
'', &
'    Y', &
'        The type shall be _real_.', &
'', &
'    X', &
'        The type and kind type parameter shall be the same as Y. If Y is', &
'        zero, then X must be nonzero.', &
'', &
'RETURNS', &
'', &
'The return value has the same type and kind type parameter as Y. It is', &
'the principal value of the complex number (X + I, Y). If x is nonzero,', &
'then it lies in the range -PI <= ATAN(X) <= PI. The sign is positive if', &
'Y is positive. If Y is zero, then the return value is zero if X is', &
'strictly positive, PI if X is negative and Y is positive zero (or the', &
'processor does not handle signed zeros), and -PI if X is negative and Y', &
'is negative zero. Finally, if X is zero, then the magnitude of the', &
'result is PI/2.', &
'', &
'EXAMPLES', &
'', &
'Sample program:', &
'', &
'    program demo_atan2', &
'    use,intrinsic :: iso_fortran_env, only : dp=>real64,sp=>real32', &
'    implicit none', &
'    real(kind=sp) :: x = 1.e0_sp, y = 0.5e0_sp, z', &
'       z = atan2(y,x)', &
'       write(*,*)x,y,z', &
'    end program demo_atan2', &
'', &
'Results:', &
'', &
'          1.00000000      0.500000000      0.463647604', &
'', &
'STANDARD', &
'', &
'FORTRAN 77 and later', &
'', &
'fortran-lang intrinsic descriptions', &
'']

shortname="atan2"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('17','atan')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'ATAN(3) - [MATHEMATICS:TRIGONOMETRIC] Arctangent function', &
'', &
'SYNTAX', &
'', &
'      - result = __atan(y, x)__', &
'', &
'       TYPE(kind=KIND):: atan', &
'       TYPE(kind=KIND,intent(in) :: x', &
'       TYPE(kind=KIND,intent(in),optional :: y', &
'', &
'where TYPE may be _real_ or _complex_ and KIND may be any KIND supported', &
'by the associated type. If Y is present X is _real`.', &
'', &
'DESCRIPTION', &
'', &
'ATAN(X) computes the arctangent of X.', &
'', &
'ARGUMENTS', &
'', &
'    X', &
'        The type shall be _real_ or _complex_; if Y is present, X shall', &
'        be _real_.', &
'', &
'    Y', &
'        Shall be of the same type and kind as X. If X is zero, Y must', &
'        not be zero.', &
'', &
'RETURNS', &
'', &
'The returned value is of the same type and kind as X. If Y is present,', &
'the result is identical to ATAN2(Y,X). Otherwise, it is the arc tangent', &
'of X, where the real part of the result is in radians and lies in the', &
'range -PI/2 <= ATAN(X) <= PI/2', &
'', &
'EXAMPLES', &
'', &
'Sample program:', &
'', &
'    program demo_atan', &
'    use, intrinsic :: iso_fortran_env, only : real_kinds, &', &
'     & real32, real64, real128', &
'    implicit none', &
'    character(len=*),parameter :: all=''(*(g0,1x))''', &
'    real(kind=real64),parameter :: &', &
'     Deg_Per_Rad = 57.2957795130823208767981548_real64', &
'    real(kind=real64) :: x', &
'        x=2.866_real64', &
'        print all, atan(x)', &
'', &
'        print all, atan( 2.0d0, 2.0d0),atan( 2.0d0, 2.0d0)*Deg_Per_Rad', &
'        print all, atan( 2.0d0,-2.0d0),atan( 2.0d0,-2.0d0)*Deg_Per_Rad', &
'        print all, atan(-2.0d0, 2.0d0),atan(-2.0d0, 2.0d0)*Deg_Per_Rad', &
'        print all, atan(-2.0d0,-2.0d0),atan(-2.0d0,-2.0d0)*Deg_Per_Rad', &
'', &
'    end program demo_atan', &
'', &
'Results:', &
'', &
'       1.235085437457879', &
'       .7853981633974483 45.00000000000000', &
'       2.356194490192345 135.0000000000000', &
'       -.7853981633974483 -45.00000000000000', &
'       -2.356194490192345 -135.0000000000000', &
'', &
'STANDARD', &
'', &
'FORTRAN 77 and later for a complex argument; and for two arguments', &
'Fortran 2008 or later', &
'', &
'SEE ALSO', &
'', &
'-   wikipedia: inverse trigonometric functions', &
'', &
'ATAN2(3), TAN(3)', &
'', &
'fortran-lang intrinsic descriptions (@urbanjost)', &
'']

shortname="atan"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('18','atanh')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'ATANH(3) - [MATHEMATICS:TRIGONOMETRIC] Inverse hyperbolic tangent', &
'function (GFDL)', &
'', &
'SYNTAX', &
'', &
'    result = atanh(x)', &
'', &
'DESCRIPTION', &
'', &
'ATANH(X) computes the inverse hyperbolic tangent of X.', &
'', &
'ARGUMENTS', &
'', &
'    X', &
'        The type shall be _real_ or _complex_.', &
'', &
'RETURNS', &
'', &
'The return value has same type and kind as X. If X is _complex_, the', &
'imaginary part of the result is in radians and lies between', &
'', &
'-PI/2 <= AIMAG(ATANH(X)) <= PI/2', &
'', &
'EXAMPLES', &
'', &
'Sample program:', &
'', &
'    program demo_atanh', &
'    implicit none', &
'    real, dimension(3) :: x = [ -1.0, 0.0, 1.0 ]', &
'', &
'       write (*,*) atanh(x)', &
'', &
'    end program demo_atanh', &
'', &
'Results:', &
'', &
'       -Infinity   0.00000000             Infinity', &
'', &
'STANDARD', &
'', &
'Fortran 2008 and later', &
'', &
'SEE ALSO', &
'', &
'-   Wikipedia:hyperbolic functions', &
'', &
'Inverse function: TANH(3)', &
'', &
'fortran-lang intrinsic descriptions', &
'']

shortname="atanh"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('19','atomic_add')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'ATOMIC_ADD(3) - [ATOMIC] Atomic ADD operation (GFDL)', &
'', &
'SYNTAX', &
'', &
'    call atomic_add (atom, value, stat)', &
'', &
'DESCRIPTION', &
'', &
'ATOMIC_AD(ATOM, VALUE) atomically adds the value of VAR to the variable', &
'ATOM. When STAT is present and the invocation was successful, it is', &
'assigned the value 0. If it is present and the invocation has failed, it', &
'is assigned a positive value; in particular, for a coindexed ATOM, if', &
'the remote image has stopped, it is assigned the value of', &
'iso_fortran_env''s stat_stopped_image and if the remote image has failed,', &
'the value stat_failed_image.', &
'', &
'ARGUMENTS', &
'', &
'    ATOM', &
'        Scalar coarray or coindexed variable of integer type with', &
'        atomic_int_kind kind.', &
'', &
'    VALUE', &
'        Scalar of the same type as ATOM. If the kind is different, the', &
'        value is converted to the kind of ATOM.', &
'', &
'    STAT', &
'        (optional) Scalar default-kind integer variable.', &
'', &
'EXAMPLES', &
'', &
'Sample program:', &
'', &
'    program demo_atomic_add', &
'    use iso_fortran_env', &
'    implicit none', &
'    integer(atomic_int_kind) :: atom[*]', &
'       call atomic_add (atom[1], this_image())', &
'    end program demo_atomic_add', &
'', &
'STANDARD', &
'', &
'TS 18508 or later', &
'', &
'SEE ALSO', &
'', &
'ATOMIC_DEFINE(3), ATOMIC_FETCH_ADD(3), ATOMIC_AND(3), ATOMIC_OR(3),', &
'ATOMIC_XOR(3) ISO_FORTRAN_ENV(3),', &
'', &
'fortran-lang intrinsic descriptions', &
'']

shortname="atomic_add"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('20','atomic_and')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'ATOMIC_AND(3) - [ATOMIC:BIT MANIPULATION] Atomic bitwise AND operation', &
'(GFDL)', &
'', &
'SYNTAX', &
'', &
'    call atomic_and(atom, value, stat)', &
'', &
'DESCRIPTION', &
'', &
'ATOMIC_AND(ATOM, VALUE) atomically defines ATOM with the bitwise AND', &
'between the values of ATOM and VALUE. When STAT is present and the', &
'invocation was successful, it is assigned the value 0. If it is present', &
'and the invocation has failed, it is assigned a positive value; in', &
'particular, for a coindexed ATOM, if the remote image has stopped, it is', &
'assigned the value of iso_fortran_env''s stat_stopped_image and if the', &
'remote image has failed, the value stat_failed_image.', &
'', &
'ARGUMENTS', &
'', &
'    ATOM', &
'        Scalar coarray or coindexed variable of integer type with', &
'        atomic_int_kind kind.', &
'', &
'    VALUE', &
'        Scalar of the same type as ATOM. If the kind is different, the', &
'        value is converted to the kind of ATOM.', &
'', &
'    STAT', &
'        (optional) Scalar default-kind integer variable.', &
'', &
'EXAMPLES', &
'', &
'Sample program:', &
'', &
'    program demo_atomic_and', &
'    use iso_fortran_env', &
'    implicit none', &
'    integer(atomic_int_kind) :: atom[*]', &
'       call atomic_and(atom[1], int(b''10100011101''))', &
'    end program demo_atomic_and', &
'', &
'STANDARD', &
'', &
'TS 18508 or later', &
'', &
'SEE ALSO', &
'', &
'ATOMIC_FETCH_AND(3), ATOMIC_DEFINE(3), ATOMIC_REF(3), ATOMIC_CAS(3),', &
'ISO_FORTRAN_ENV(3), ATOMIC_ADD(3), ATOMIC_OR(3), ATOMIC_XOR(3)', &
'', &
'fortran-lang intrinsic descriptions', &
'']

shortname="atomic_and"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('21','atomic_cas')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'ATOMIC_CAS(3) - [ATOMIC] Atomic compare and swap (GFDL)', &
'', &
'SYNTAX', &
'', &
'    call atomic_cas (atom, old, compare, new, stat)', &
'', &
'DESCRIPTION', &
'', &
'atomic_cas compares the variable ATOM with the value of COMPARE; if the', &
'value is the same, ATOM is set to the value of NEW. Additionally, OLD is', &
'set to the value of ATOM that was used for the comparison. When STAT is', &
'present and the invocation was successful, it is assigned the value 0.', &
'If it is present and the invocation has failed, it is assigned a', &
'positive value; in particular, for a coindexed ATOM, if the remote image', &
'has stopped, it is assigned the value of iso_fortran_env''s', &
'stat_stopped_image and if the remote image has failed, the value', &
'stat_failed_image.', &
'', &
'ARGUMENTS', &
'', &
'    ATOM', &
'        Scalar coarray or coindexed variable of either integer type with', &
'        atomic_int_kind kind or logical type with atomic_logical_kind', &
'        kind.', &
'', &
'    OLD', &
'        Scalar of the same type and kind as ATOM.', &
'', &
'    COMPARE', &
'        Scalar variable of the same type and kind as ATOM.', &
'', &
'    NEW', &
'        Scalar variable of the same type as ATOM. If kind is different,', &
'        the value is converted to the kind of ATOM.', &
'', &
'    STAT', &
'        (optional) Scalar default-kind integer variable.', &
'', &
'EXAMPLES', &
'', &
'Sample program:', &
'', &
'    program demo_atomic_cas', &
'    use iso_fortran_env', &
'    implicit none', &
'    logical(atomic_logical_kind) :: atom[*], prev', &
'       call atomic_cas(atom[1], prev, .false., .true.)', &
'    end program demo_atomic_cas', &
'', &
'STANDARD', &
'', &
'TS 18508 or later', &
'', &
'SEE ALSO', &
'', &
'ATOMIC_DEFINE(3), ATOMIC_REF(3), ISO_FORTRAN_ENV(3)', &
'', &
'fortran-lang intrinsic descriptions', &
'']

shortname="atomic_cas"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('22','atomic_define')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'ATOMIC_DEFINE(3) - [ATOMIC] Setting a variable atomically (GFDL)', &
'', &
'SYNTAX', &
'', &
'    call atomic_define (atom, value, stat)', &
'', &
'       subroutine atomic_define(atom, value, stat)', &
'       TYPE(kind=KIND) :: atom', &
'       TYPE(kind=KIND) :: value', &
'       integer,intent(out),optional :: stat', &
'', &
'DESCRIPTION', &
'', &
'ATOMIC_DEFINE(ATOM, VALUE) defines the variable ATOM with the value', &
'VALUE atomically. When STAT is present and the invocation was', &
'successful, it is assigned the value 0. If it is present and the', &
'invocation has failed, it is assigned a positive value; in particular,', &
'for a coindexed ATOM, if the remote image has stopped, it is assigned', &
'the value of iso_fortran_env''s stat_stopped_image and if the remote', &
'image has failed, the value stat_failed_image.', &
'', &
'ARGUMENTS', &
'', &
'    ATOM', &
'        Scalar coarray or coindexed variable of either integer type with', &
'        atomic_int_kind kind or logical type with atomic_logical_kind', &
'        kind.', &
'', &
'    VALUE', &
'        Scalar of the same type as ATOM. If the kind is different, the', &
'        value is converted to the kind of ATOM.', &
'', &
'    STAT', &
'        (optional) Scalar default-kind integer variable.', &
'', &
'EXAMPLES', &
'', &
'Sample program:', &
'', &
'    program demo_atomic_define', &
'    use iso_fortran_env', &
'    implicit none', &
'    integer(atomic_int_kind) :: atom[*]', &
'        call atomic_define(atom[1], this_image())', &
'    end program demo_atomic_define', &
'', &
'STANDARD', &
'', &
'Fortran 2008 and later; with STAT, TS 18508 or later', &
'', &
'SEE ALSO', &
'', &
'ATOMIC_REF(3), ATOMIC_CAS(3), ISO_FORTRAN_ENV(3), ATOMIC_ADD(3),', &
'ATOMIC_AND(3), ATOMIC_OR(3), ATOMIC_XOR(3)', &
'', &
'fortran-lang intrinsic descriptions', &
'']

shortname="atomic_define"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('23','atomic_fetch_add')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'ATOMIC_FETCH_ADD(3) - [ATOMIC] Atomic ADD operation with prior fetch', &
'(GFDL)', &
'', &
'SYNTAX', &
'', &
'    call atomic_fetch_add(atom, value, old, stat)', &
'', &
'DESCRIPTION', &
'', &
'ATOMIC_FETCH_ADD(ATOM, VALUE, OLD) atomically stores the value of ATOM', &
'in OLD and adds the value of VAR to the variable ATOM. When STAT is', &
'present and the invocation was successful, it is assigned the value 0.', &
'If it is present and the invocation has failed, it is assigned a', &
'positive value; in particular, for a coindexed ATOM, if the remote image', &
'has stopped, it is assigned the value of iso_fortran_env''s', &
'stat_stopped_image and if the remote image has failed, the value', &
'stat_failed_image.', &
'', &
'ARGUMENTS', &
'', &
'    ATOM', &
'        Scalar coarray or coindexed variable of integer type with', &
'        atomic_int_kind kind. atomic_logical_kind kind.', &
'', &
'    VALUE', &
'        Scalar of the same type as ATOM. If the kind is different, the', &
'        value is converted to the kind of ATOM.', &
'', &
'    OLD', &
'        Scalar of the same type and kind as ATOM.', &
'', &
'    STAT', &
'        (optional) Scalar default-kind integer variable.', &
'', &
'EXAMPLES', &
'', &
'Sample program:', &
'', &
'    program demo_atomic_fetch_add', &
'    use iso_fortran_env', &
'    implicit none', &
'    integer(atomic_int_kind) :: atom[*], old', &
'       call atomic_add(atom[1], this_image(), old)', &
'    end program demo_atomic_fetch_add', &
'', &
'STANDARD', &
'', &
'TS 18508 or later', &
'', &
'SEE ALSO', &
'', &
'ATOMIC_DEFINE(3), ATOMIC_ADD(3), ISO_FORTRAN_ENV(3),', &
'', &
'ATOMIC_FETCH_AND(3), ATOMIC_FETCH_OR(3),', &
'', &
'ATOMIC_FETCH_XOR(3)', &
'', &
'fortran-lang intrinsic descriptions', &
'']

shortname="atomic_fetch_add"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('24','atomic_fetch_and')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'ATOMIC_FETCH_AND(3) - [ATOMIC:BIT MANIPULATION] Atomic bitwise AND', &
'operation with prior fetch (GFDL)', &
'', &
'SYNTAX', &
'', &
'    call atomic_fetch_and(atom, value, old, stat)', &
'', &
'DESCRIPTION', &
'', &
'ATOMIC_FETCH_AND(ATOM, VALUE, OLD) atomically stores the value of ATOM', &
'in OLD and defines ATOM with the bitwise AND between the values of ATOM', &
'and VALUE. When STAT is present and the invocation was successful, it is', &
'assigned the value 0. If it is present and the invocation has failed, it', &
'is assigned a positive value; in particular, for a coindexed ATOM, if', &
'the remote image has stopped, it is assigned the value of', &
'iso_fortran_env''s stat_stopped_image and if the remote image has failed,', &
'the value stat_failed_image.', &
'', &
'ARGUMENTS', &
'', &
'    ATOM', &
'        Scalar coarray or coindexed variable of integer type with', &
'        atomic_int_kind kind.', &
'', &
'    VALUE', &
'        Scalar of the same type as ATOM. If the kind is different, the', &
'        value is converted to the kind of ATOM.', &
'', &
'    OLD', &
'        Scalar of the same type and kind as ATOM.', &
'', &
'    STAT', &
'        (optional) Scalar default-kind integer variable.', &
'', &
'EXAMPLES', &
'', &
'Sample program:', &
'', &
'    program demo_atomic_fetch_and', &
'    use iso_fortran_env', &
'    implicit none', &
'    integer(atomic_int_kind) :: atom[*], old', &
'       call atomic_fetch_and (atom[1], int(b''10100011101''), old)', &
'    end program demo_atomic_fetch_and', &
'', &
'STANDARD', &
'', &
'TS 18508 or later', &
'', &
'SEE ALSO', &
'', &
'ATOMIC_DEFINE(3), ATOMIC_AND(3), ISO_FORTRAN_ENV(3),', &
'', &
'ATOMIC_FETCH_ADD(3), ATOMIC_FETCH_OR(3),', &
'', &
'ATOMIC_FETCH_XOR(3)', &
'', &
'fortran-lang intrinsic descriptions', &
'']

shortname="atomic_fetch_and"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('25','atomic_fetch_or')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'ATOMIC_FETCH_OR(3) - [ATOMIC:BIT MANIPULATION] Atomic bitwise OR', &
'operation with prior fetch (GFDL)', &
'', &
'SYNTAX', &
'', &
'    call atomic_fetch_or(atom, value, old, stat)', &
'', &
'DESCRIPTION', &
'', &
'ATOMIC_FETCH_OR(ATOM, VALUE, OLD) atomically stores the value of ATOM in', &
'OLD and defines ATOM with the bitwise OR between the values of ATOM and', &
'VALUE. When STAT is present and the invocation was successful, it is', &
'assigned the value 0. If it is present and the invocation has failed, it', &
'is assigned a positive value; in particular, for a coindexed ATOM, if', &
'the remote image has stopped, it is assigned the value of', &
'iso_fortran_env''s stat_stopped_image and if the remote image has failed,', &
'the value stat_failed_image.', &
'', &
'ARGUMENTS', &
'', &
'    ATOM', &
'        Scalar coarray or coindexed variable of integer type with', &
'        atomic_int_kind kind.', &
'', &
'    VALUE', &
'        Scalar of the same type as ATOM. If the kind is different, the', &
'        value is converted to the kind of ATOM.', &
'', &
'    OLD', &
'        Scalar of the same type and kind as ATOM.', &
'', &
'    STAT', &
'        (optional) Scalar default-kind integer variable.', &
'', &
'EXAMPLES', &
'', &
'Sample program:', &
'', &
'    program demo_atomic_fetch_or', &
'    use iso_fortran_env', &
'    implicit none', &
'    integer(atomic_int_kind) :: atom[*], old', &
'       call atomic_fetch_or(atom[1], int(b''10100011101''), old)', &
'    end program demo_atomic_fetch_or', &
'', &
'STANDARD', &
'', &
'TS 18508 or later', &
'', &
'SEE ALSO', &
'', &
'ATOMIC_DEFINE(3), ATOMIC_OR(3), ISO_FORTRAN_ENV(3),', &
'', &
'ATOMIC_FETCH_ADD(3), ATOMIC_FETCH_AND(3),', &
'', &
'ATOMIC_FETCH_XOR(3)', &
'', &
'fortran-lang intrinsic descriptions', &
'']

shortname="atomic_fetch_or"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('26','atomic_fetch_xor')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'ATOMIC_FETCH_XOR(3) - [ATOMIC:BIT MANIPULATION] Atomic bitwise XOR', &
'operation with prior fetch (GFDL)', &
'', &
'SYNTAX', &
'', &
'    call atomic_fetch_xor (atom, value, old, stat)', &
'', &
'DESCRIPTION', &
'', &
'ATOMIC_FETCH_XOR(ATOM, VALUE, OLD) atomically stores the value of ATOM', &
'in OLD and defines ATOM with the bitwise XOR between the values of ATOM', &
'and VALUE. When STAT is present and the invocation was successful, it is', &
'assigned the value 0. If it is present and the invocation has failed, it', &
'is assigned a positive value; in particular, for a coindexed ATOM, if', &
'the remote image has stopped, it is assigned the value of', &
'iso_fortran_env''s stat_stopped_image and if the remote image has failed,', &
'the value stat_failed_image.', &
'', &
'ARGUMENTS', &
'', &
'    ATOM', &
'        Scalar coarray or coindexed variable of integer type with', &
'        atomic_int_kind kind.', &
'', &
'    VALUE', &
'        Scalar of the same type as ATOM. If the kind is different, the', &
'        value is converted to the kind of ATOM.', &
'', &
'    OLD', &
'        Scalar of the same type and kind as ATOM.', &
'', &
'    STAT', &
'        (optional) Scalar default-kind integer variable.', &
'', &
'EXAMPLES', &
'', &
'Sample program:', &
'', &
'    program demo_atomic_fetch_xor', &
'    use iso_fortran_env', &
'    implicit none', &
'    integer(atomic_int_kind) :: atom[*], old', &
'       call atomic_fetch_xor (atom[1], int(b''10100011101''), old)', &
'    end program demo_atomic_fetch_xor', &
'', &
'STANDARD', &
'', &
'TS 18508 or later', &
'', &
'SEE ALSO', &
'', &
'ATOMIC_DEFINE(3), ATOMIC_XOR(3), ISO_FORTRAN_ENV(3),', &
'', &
'ATOMIC_FETCH_ADD(3), ATOMIC_FETCH_AND(3),', &
'', &
'ATOMIC_FETCH_OR(3)', &
'', &
'fortran-lang intrinsic descriptions', &
'']

shortname="atomic_fetch_xor"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('27','atomic_or')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'ATOMIC_OR(3) - [ATOMIC:BIT MANIPULATION] Atomic bitwise OR operation', &
'(GFDL)', &
'', &
'SYNTAX', &
'', &
'    call atomic_or__(atom, value, stat)', &
'', &
'DESCRIPTION', &
'', &
'ATOMIC_OR(ATOM, VALUE) atomically defines ATOM with the bitwise OR', &
'between the values of ATOM and VALUE. When STAT is present and the', &
'invocation was successful, it is assigned the value 0. If it is present', &
'and the invocation has failed, it is assigned a positive value; in', &
'particular, for a coindexed ATOM, if the remote image has stopped, it is', &
'assigned the value of iso_fortran_env''s stat_stopped_image and if the', &
'remote image has failed, the value stat_failed_image.', &
'', &
'ARGUMENTS', &
'', &
'    ATOM', &
'        Scalar coarray or coindexed variable of integer type with', &
'        atomic_int_kind kind.', &
'', &
'    VALUE', &
'        Scalar of the same type as ATOM. If the kind is different, the', &
'        value is converted to the kind of ATOM.', &
'', &
'    STAT', &
'        (optional) Scalar default-kind integer variable.', &
'', &
'EXAMPLES', &
'', &
'Sample program:', &
'', &
'    program demo_atomic_or', &
'    use iso_fortran_env', &
'    implicit none', &
'    integer(atomic_int_kind) :: atom[*]', &
'       call atomic_or(atom[1], int(b''10100011101''))', &
'    end program demo_atomic_or', &
'', &
'STANDARD', &
'', &
'TS 18508 or later', &
'', &
'SEE ALSO', &
'', &
'ATOMIC_DEFINE(3), ATOMIC_FETCH_OR(3),', &
'', &
'ISO_FORTRAN_ENV(3), ATOMIC_ADD(3), ATOMIC_OR(3),', &
'', &
'ATOMIC_XOR(3)', &
'', &
'fortran-lang intrinsic descriptions', &
'']

shortname="atomic_or"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('28','atomic_ref')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'ATOMIC_REF(3) - [ATOMIC] Obtaining the value of a variable atomically', &
'(GFDL)', &
'', &
'SYNTAX', &
'', &
'    call atomic_ref(value, atom, stat)', &
'', &
'DESCRIPTION', &
'', &
'ATOMIC_REF(VALUE, ATOM) atomically assigns the value of the variable', &
'ATOM to VALUE. When STAT is present and the invocation was successful,', &
'it is assigned the value 0. If it is present and the invocation has', &
'failed, it is assigned a positive value; in particular, for a coindexed', &
'ATOM, if the remote image has stopped, it is assigned the value of', &
'iso_fortran_env''s STAT_STOPPED_IMAGE and if the remote image has failed,', &
'the value STAT_FAILED_IMAGE.', &
'', &
'ARGUMENTS', &
'', &
'    VALUE', &
'        Scalar of the same type as ATOM. If the kind is different, the', &
'        value is converted to the kind of ATOM.', &
'', &
'    ATOM', &
'        Scalar coarray or coindexed variable of either integer type with', &
'        atomic_int_kind kind or logical type with atomic_logical_kind', &
'        kind.', &
'', &
'    STAT', &
'        (optional) Scalar default-kind integer variable.', &
'', &
'EXAMPLES', &
'', &
'Sample program:', &
'', &
'    program demo_atomic_ref', &
'    use iso_fortran_env', &
'    implicit none', &
'    logical(atomic_logical_kind) :: atom[*]', &
'    logical :: val', &
'       call atomic_ref( val, atom[1] )', &
'       ! ```', &
'       call atomic_ref( val, atom[1] )', &
'       if (val) then', &
'          print *, "Obtained"', &
'       endif', &
'    end program demo_atomic_ref', &
'', &
'STANDARD', &
'', &
'Fortran 2008 and later; with STAT, TS 18508 or later', &
'', &
'SEE ALSO', &
'', &
'ATOMIC_DEFINE(3), ATOMIC_CAS(3), ISO_FORTRAN_ENV(3),', &
'', &
'ATOMIC_FETCH_ADD(3), ATOMIC_FETCH_AND(3),', &
'', &
'ATOMIC_FETCH_OR(3), ATOMIC_FETCH_XOR(3)', &
'', &
'fortran-lang intrinsic descriptions', &
'']

shortname="atomic_ref"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('29','atomic_xor')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'ATOMIC_XOR(3) - [ATOMIC:BIT MANIPULATION] Atomic bitwise OR operation', &
'(GFDL)', &
'', &
'SYNTAX', &
'', &
'    call atomic_xor(atom, value, stat)', &
'', &
'DESCRIPTION', &
'', &
'ATOMIC_XOR(ATOM, VALUE) atomically defines ATOM with the bitwise XOR', &
'between the values of ATOM and VALUE. When STAT is present and the', &
'invocation was successful, it is assigned the value 0. If it is present', &
'and the invocation has failed, it is assigned a positive value; in', &
'particular, for a coindexed ATOM, if the remote image has stopped, it is', &
'assigned the value of iso_fortran_env''s stat_stopped_image and if the', &
'remote image has failed, the value stat_failed_image.', &
'', &
'ARGUMENTS', &
'', &
'    ATOM', &
'        Scalar coarray or coindexed variable of integer type with', &
'        atomic_int_kind kind.', &
'', &
'    VALUE', &
'        Scalar of the same type as ATOM. If the kind is different, the', &
'        value is converted to the kind of ATOM.', &
'', &
'    STAT', &
'        (optional) Scalar default-kind integer variable.', &
'', &
'EXAMPLES', &
'', &
'Sample program:', &
'', &
'    program demo_atomic_xor', &
'    use iso_fortran_env', &
'    implicit none', &
'    integer(atomic_int_kind) :: atom[*]', &
'       call atomic_xor(atom[1], int(b''10100011101''))', &
'    end program demo_atomic_xor', &
'', &
'STANDARD', &
'', &
'TS 18508 or later', &
'', &
'SEE ALSO', &
'', &
'ATOMIC_DEFINE(3), ATOMIC_FETCH_XOR(3), ISO_FORTRAN_ENV(3),', &
'ATOMIC_ADD(3), ATOMIC_OR(3), ATOMIC_XOR(3)', &
'', &
'fortran-lang intrinsic descriptions', &
'']

shortname="atomic_xor"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('30','bessel_j0')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'BESSEL_J0(3) - [MATHEMATICS] Bessel function of the first kind of order', &
'0 (GFDL)', &
'', &
'SYNTAX', &
'', &
'        result = bessel_j0(x)', &
'', &
'DESCRIPTION', &
'', &
'BESSEL_J0(X) computes the Bessel function of the first kind of order 0', &
'of X.', &
'', &
'ARGUMENTS', &
'', &
'    X', &
'        The type shall be _real_.', &
'', &
'RETURNS', &
'', &
'The return value is of type _real_ and lies in the range -0.4027 <=', &
'BESSEL(0,X) <= 1. It has the same kind as X.', &
'', &
'EXAMPLES', &
'', &
'Sample program:', &
'', &
'    program demo_besj0', &
'    use, intrinsic :: iso_fortran_env, only : real_kinds, &', &
'    & real32, real64, real128', &
'       implicit none', &
'       real(kind=real64) :: x = 0.0_real64', &
'       x = bessel_j0(x)', &
'       write(*,*)x', &
'    end program demo_besj0', &
'', &
'Results:', &
'', &
'          1.0000000000000000', &
'', &
'STANDARD', &
'', &
'Fortran 2008 and later', &
'', &
'SEE ALSO', &
'', &
'BESSEL_J1(3), BESSEL_JN(3), BESSEL_Y0(3), BESSEL_Y1(3), BESSEL_YN(3)', &
'', &
'fortran-lang intrinsic descriptions', &
'']

shortname="bessel_j0"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('31','bessel_j1')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'BESSEL_J1(3) - [MATHEMATICS] Bessel function of the first kind of order', &
'1 (GFDL)', &
'', &
'SYNTAX', &
'', &
'        result = bessel_j1(x)', &
'', &
'DESCRIPTION', &
'', &
'BESSEL_J1(X) computes the Bessel function of the first kind of order 1', &
'of X.', &
'', &
'ARGUMENTS', &
'', &
'    X', &
'        The type shall be _real_.', &
'', &
'RETURNS', &
'', &
'The return value is of type _real_ and lies in the range -0.5818 <=', &
'BESSEL(0,X) <= 0.5818 . It has the same kind as X.', &
'', &
'EXAMPLES', &
'', &
'Sample program:', &
'', &
'    program demo_besj1', &
'    use, intrinsic :: iso_fortran_env, only : real_kinds, &', &
'     & real32, real64, real128', &
'    implicit none', &
'    real(kind=real64) :: x = 1.0_real64', &
'       x = bessel_j1(x)', &
'       write(*,*)x', &
'    end program demo_besj1', &
'', &
'Results:', &
'', &
'         0.44005058574493350', &
'', &
'STANDARD', &
'', &
'Fortran 2008 and later', &
'', &
'SEE ALSO', &
'', &
'BESSEL_J0(3), BESSEL_JN(3), BESSEL_Y0(3), BESSEL_Y1(3), BESSEL_YN(3)', &
'', &
'fortran-lang intrinsic descriptions', &
'']

shortname="bessel_j1"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('32','bessel_jn')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'BESSEL_JN(3) - [MATHEMATICS] Bessel function of the first kind (GFDL)', &
'', &
'SYNTAX', &
'', &
'      result = bessel_jn(n, x)', &
'', &
'      result = bessel_jn(n1, n2, x)', &
'', &
'DESCRIPTION', &
'', &
'BESSEL_JN(N, X) computes the Bessel function of the first kind of order', &
'N of X. If N and X are arrays, their ranks and shapes shall conform.', &
'', &
'BESSEL_JN(N1, N2, X) returns an array with the Bessel function|Bessel', &
'functions of the first kind of the orders N1 to N2.', &
'', &
'ARGUMENTS', &
'', &
'    N', &
'        Shall be a scalar or an array of type _integer_.', &
'', &
'    N1', &
'        Shall be a non-negative scalar of type _integer_.', &
'', &
'    N2', &
'        Shall be a non-negative scalar of type _integer_.', &
'', &
'    X', &
'        Shall be a scalar or an array of type _real_. For BESSEL_JN(N1,', &
'        N2, X) it shall be scalar.', &
'', &
'RETURNS', &
'', &
'The return value is a scalar of type _real_. It has the same kind as X.', &
'', &
'EXAMPLES', &
'', &
'Sample program:', &
'', &
'    program demo_besjn', &
'    use, intrinsic :: iso_fortran_env, only : real_kinds, &', &
'       & real32, real64, real128', &
'    implicit none', &
'    real(kind=real64) :: x = 1.0_real64', &
'        x = bessel_jn(5,x)', &
'        write(*,*)x', &
'    end program demo_besjn', &
'', &
'Results:', &
'', &
'          2.4975773021123450E-004', &
'', &
'STANDARD', &
'', &
'Fortran 2008 and later', &
'', &
'SEE ALSO', &
'', &
'BESSEL_J0(3), BESSEL_J1(3), BESSEL_Y0(3), BESSEL_Y1(3), BESSEL_YN(3)', &
'', &
'fortran-lang intrinsic descriptions', &
'']

shortname="bessel_jn"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('33','bessel_y0')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'BESSEL_Y0(3) - [MATHEMATICS] Bessel function of the second kind of order', &
'0 (GFDL)', &
'', &
'SYNTAX', &
'', &
'        result = bessel_y0(x)', &
'', &
'DESCRIPTION', &
'', &
'BESSEL_Y0(X) computes the Bessel function of the second kind of order 0', &
'of X.', &
'', &
'ARGUMENTS', &
'', &
'    X', &
'        The type shall be _real_.', &
'', &
'RETURNS', &
'', &
'The return value is of type _real_. It has the same kind as X.', &
'', &
'EXAMPLES', &
'', &
'Sample program:', &
'', &
'    program demo_besy0', &
'    use, intrinsic :: iso_fortran_env, only : real_kinds, &', &
'    & real32, real64, real128', &
'    implicit none', &
'      real(kind=real64) :: x = 0.0_real64', &
'      x = bessel_y0(x)', &
'      write(*,*)x', &
'    end program demo_besy0', &
'', &
'Results:', &
'', &
'                        -Infinity', &
'', &
'STANDARD', &
'', &
'Fortran 2008 and later', &
'', &
'SEE ALSO', &
'', &
'BESSEL_J0(3), BESSEL_J1(3), BESSEL_JN(3), BESSEL_Y1(3), BESSEL_YN(3)', &
'', &
'fortran-lang intrinsic descriptions', &
'']

shortname="bessel_y0"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('34','bessel_y1')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'BESSEL_Y1(3) - [MATHEMATICS] Bessel function of the second kind of order', &
'1 (GFDL)', &
'', &
'SYNTAX', &
'', &
'        result = bessel_y1(x)', &
'', &
'DESCRIPTION', &
'', &
'BESSEL_Y1(X) computes the Bessel function of the second kind of order 1', &
'of X.', &
'', &
'ARGUMENTS', &
'', &
'    X', &
'        The type shall be _real_.', &
'', &
'RETURNS', &
'', &
'The return value is _real_. It has the same kind as X.', &
'', &
'EXAMPLES', &
'', &
'Sample program:', &
'', &
'    program demo_besy1', &
'    use, intrinsic :: iso_fortran_env, only : real_kinds, &', &
'    & real32, real64, real128', &
'    implicit none', &
'      real(kind=real64) :: x = 1.0_real64', &
'      write(*,*)x, bessel_y1(x)', &
'    end program demo_besy1', &
'', &
'STANDARD', &
'', &
'Fortran 2008 and later', &
'', &
'SEE ALSO', &
'', &
'BESSEL_J0(3), BESSEL_J1(3), BESSEL_JN(3), BESSEL_Y0(3), BESSEL_YN(3)', &
'', &
'fortran-lang intrinsic descriptions', &
'']

shortname="bessel_y1"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('35','bessel_yn')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'BESSEL_YN(3) - [MATHEMATICS] Bessel function of the second kind (GFDL)', &
'', &
'SYNTAX', &
'', &
'      result = bessel_yn(n, x)', &
'', &
'      result = bessel_yn(n1, n2, x)', &
'', &
'DESCRIPTION', &
'', &
'BESSEL_YN(N, X) computes the Bessel function of the second kind of order', &
'N of X. If N and X are arrays, their ranks and shapes shall conform.', &
'', &
'BESSEL_YN(N1, N2, X) returns an array with the Bessel function|Bessel', &
'functions of the first kind of the orders N1 to N2.', &
'', &
'ARGUMENTS', &
'', &
'    N', &
'        Shall be a scalar or an array of type _integer_.', &
'', &
'    N1', &
'        Shall be a non-negative scalar of type _integer_.', &
'', &
'    N2', &
'        Shall be a non-negative scalar of type _integer_.', &
'', &
'    X', &
'        Shall be a scalar or an array of type _real_; for BESSEL_YN(N1,', &
'        N2, X) it shall be scalar.', &
'', &
'RETURNS', &
'', &
'The return value is _real_. It has the same kind as X.', &
'', &
'EXAMPLES', &
'', &
'Sample program:', &
'', &
'    program demo_besyn', &
'    use, intrinsic :: iso_fortran_env, only : real_kinds, &', &
'    & real32, real64, real128', &
'    implicit none', &
'    real(kind=real64) :: x = 1.0_real64', &
'      write(*,*) x,bessel_yn(5,x)', &
'    end program demo_besyn', &
'', &
'Results:', &
'', &
'          1.0000000000000000       -260.40586662581222', &
'', &
'STANDARD', &
'', &
'Fortran 2008 and later', &
'', &
'SEE ALSO', &
'', &
'BESSEL_J0(3), BESSEL_J1(3), BESSEL_JN(3), BESSEL_Y0(3), BESSEL_Y1(3)', &
'', &
'fortran-lang intrinsic descriptions', &
'']

shortname="bessel_yn"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('36','bge')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'BGE(3) - [BIT:COMPARE] Bitwise greater than or equal to (GFDL)', &
'', &
'SYNTAX', &
'', &
'        result = bge(i, j)', &
'', &
'DESCRIPTION', &
'', &
'Determines whether an integer is bitwise greater than or equal to', &
'another.', &
'', &
'ARGUMENTS', &
'', &
'    I', &
'        Shall be of _integer_ type.', &
'', &
'    J', &
'        Shall be of _integer_ type, and of the same kind as I.', &
'', &
'RETURNS', &
'', &
'The return value is of type _logical_ and of the default kind.', &
'', &
'STANDARD', &
'', &
'Fortran 2008 and later', &
'', &
'SEE ALSO', &
'', &
'BGT(3), BLE(3), BLT(3)', &
'', &
'fortran-lang intrinsic descriptions', &
'']

shortname="bge"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('37','bgt')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'BGT(3) - [BIT:COMPARE] Bitwise greater than (GFDL)', &
'', &
'SYNTAX', &
'', &
'        result = bgt(i, j)', &
'', &
'DESCRIPTION', &
'', &
'Determines whether an integer is bitwise greater than another.', &
'', &
'ARGUMENTS', &
'', &
'    I', &
'        Shall be of _integer_ type or a BOZ literal constant.', &
'', &
'    J', &
'        Shall be of _integer_ type, and of the same kind as I; or a BOZ', &
'        literal constant.', &
'', &
'RETURNS', &
'', &
'The return value is of type _logical_ and of the default kind. The', &
'result is true if the sequence of bits represented by _i_ is greater', &
'than the sequence of bits represented by _j_, otherwise the result is', &
'false.', &
'', &
'STANDARD', &
'', &
'Fortran 2008 and later', &
'', &
'SEE ALSO', &
'', &
'BGE(3),, BLE(3),, BLT(3)', &
'', &
'fortran-lang intrinsic descriptions', &
'']

shortname="bgt"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('38','bit_size')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'BIT_SIZE(3) - [BIT:INQUIRY] Bit size inquiry function (GFDL)', &
'', &
'SYNTAX', &
'', &
'    result = bit_size(i)', &
'', &
'DESCRIPTION', &
'', &
'BIT_SIZE(I) returns the number of bits (integer precision plus sign bit)', &
'represented by the type of I.', &
'', &
'ARGUMENTS', &
'', &
'    I', &
'        The type shall be _integer_.', &
'', &
'RETURNS', &
'', &
'The return value is of type _integer_ of the same type as I.', &
'', &
'EXAMPLES', &
'', &
'Sample program:', &
'', &
'    program demo_bit_size', &
'    use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64', &
'    implicit none', &
'    integer(kind=int64)          :: answer', &
'    integer                      :: ilen', &
'        write(*,''(i0)'')bit_size(bit_size(0_int8))', &
'        write(*,''(i0)'')bit_size(bit_size(0_int16))', &
'        write(*,''(i0)'')bit_size(bit_size(0_int32))', &
'        write(*,''(i0)'')bit_size(bit_size(0_int64))', &
'        answer=0_int64', &
'        ilen=999', &
'        ! notice use of INT(3)', &
'        ilen=min(ilen,int(bit_size(answer)))', &
'        ! arguments to MIN(3) would be of different TYPES', &
'        !ilen=min(ilen,bit_size(answer))', &
'        write(*,''(i0)'')ilen', &
'    end program demo_bit_size', &
'', &
'Expected output:', &
'', &
'       8', &
'       16', &
'       32', &
'       64', &
'       64', &
'', &
'STANDARD', &
'', &
'Fortran 95 and later', &
'', &
'fortran-lang intrinsic descriptions', &
'']

shortname="bit_size"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('39','ble')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'BLE(3) - [BIT:COMPARE] Bitwise less than or equal to (GFDL)', &
'', &
'SYNTAX', &
'', &
'        result = ble(i, j)', &
'', &
'DESCRIPTION', &
'', &
'Determines whether an integer is bitwise less than or equal to another.', &
'', &
'ARGUMENTS', &
'', &
'    I', &
'        Shall be of _integer_ type.', &
'', &
'    J', &
'        Shall be of _integer_ type, and of the same kind as I.', &
'', &
'RETURNS', &
'', &
'The return value is of type _logical_ and of the default kind.', &
'', &
'STANDARD', &
'', &
'Fortran 2008 and later', &
'', &
'SEE ALSO', &
'', &
'BGE(3),, BGT(3),, BLT(3)', &
'', &
'fortran-lang intrinsic descriptions', &
'']

shortname="ble"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('40','blt')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'BLT(3) - [BIT:COMPARE] Bitwise less than (GFDL)', &
'', &
'SYNTAX', &
'', &
'        result = blt(i, j)', &
'', &
'DESCRIPTION', &
'', &
'Determines whether an integer is bitwise less than another.', &
'', &
'ARGUMENTS', &
'', &
'    I', &
'        Shall be of _integer_ type.', &
'', &
'    J', &
'        Shall be of _integer_ type, and of the same kind as I.', &
'', &
'RETURNS', &
'', &
'The return value is of type _logical_ and of the default kind.', &
'', &
'STANDARD', &
'', &
'Fortran 2008 and later', &
'', &
'SEE ALSO', &
'', &
'BGE(3), BGT(3), BLE(3)', &
'', &
'fortran-lang intrinsic descriptions', &
'']

shortname="blt"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('41','btest')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'BTEST(3) - [BIT:INQUIRY] Tests a bit of an _integer_ value.', &
'', &
'SYNTAX', &
'', &
'       result = btest(i, pos)', &
'', &
'        integer(kind=KIND) elemental function btest(i,pos)', &
'        integer,intent(in)  :: i', &
'        logical,intent(out) :: pos', &
'', &
'where KIND is any _integer_ kind supported by the programming', &
'environment.', &
'', &
'DESCRIPTION', &
'', &
'BTEST(I,POS) returns logical .TRUE. if the bit at POS in I is set.', &
'', &
'ARGUMENTS', &
'', &
'    I', &
'        The type shall be _integer_.', &
'', &
'    POS', &
'        The bit position to query. it must be a valid position for the', &
'        value I; ie. 0 <= POS <= BIT_SIZE(I) .', &
'', &
'    A value of zero refers to the least significant bit.', &
'', &
'RETURNS', &
'', &
'The result is a _logical_ that has the value .TRUE. if bit position POS', &
'of I has the value 1 and the value .FALSE. if bit POS of I has the value', &
'0.', &
'', &
'EXAMPLES', &
'', &
'Sample program:', &
'', &
'    program demo_btest', &
'    implicit none', &
'    integer :: i, j, pos, a(2,2)', &
'    logical :: bool', &
'    character(len=*),parameter :: g=''(*(g0))''', &
'', &
'         i = 32768 + 1024 + 64', &
'        write(*,''(a,i0,"=>",b32.32,/)'')''Looking at the integer: '',i', &
'', &
'        ! looking one bit at a time from LOW BIT TO HIGH BIT', &
'        write(*,g)''from bit 0 to bit '',bit_size(i),''==>''', &
'        do pos=0,bit_size(i)-1', &
'            bool = btest(i, pos)', &
'            write(*,''(l1)'',advance=''no'')bool', &
'        enddo', &
'        write(*,*)', &
'', &
'        ! a binary format the hard way.', &
'        ! Note going from bit_size(i) to zero.', &
'        write(*,*)', &
'        write(*,g)''so for '',i,'' with a bit size of '',bit_size(i)', &
'        write(*,''(b32.32)'')i', &
'        write(*,g)merge(''^'',''_'',[(btest(i,j),j=bit_size(i)-1,0,-1)])', &
'        write(*,*)', &
'        write(*,g)''and for '',-i,'' with a bit size of '',bit_size(i)', &
'        write(*,''(b32.32)'')-i', &
'        write(*,g)merge(''^'',''_'',[(btest(-i,j),j=bit_size(i)-1,0,-1)])', &
'', &
'        ! elemental:', &
'        !', &
'        a(1,:)=[ 1, 2 ]', &
'        a(2,:)=[ 3, 4 ]', &
'        write(*,*)', &
'        write(*,''(a,/,*(i2,1x,i2,/))'')''given the array a ...'',a', &
'        ! the second bit of all the values in a', &
'        write(*,''(a,/,*(l2,1x,l2,/))'')''the value of btest (a, 2)'',btest(a,2)', &
'        ! bits 1,2,3,4 of the value 2', &
'        write(*,''(a,/,*(l2,1x,l2,/))'')''the value of btest (2, a)'',btest(2,a)', &
'    end program demo_btest', &
'', &
'Results:', &
'', &
'    Looking at the integer: 33856=>11111111111111110111101111000000', &
'', &
'    00000000000000001000010001000000', &
'    11111111111111110111101111000000', &
'    1000010001000000', &
'    11111111111111110111101111000000', &
'    from bit 0 to bit 32==>', &
'    FFFFFFTFFFTFFFFTFFFFFFFFFFFFFFFF', &
'', &
'    so for 33856 with a bit size of 32', &
'    00000000000000001000010001000000', &
'    ________________^____^___^______', &
'', &
'    and for -33856 with a bit size of 32', &
'    11111111111111110111101111000000', &
'    ^^^^^^^^^^^^^^^^_^^^^_^^^^______', &
'', &
'    given the array a ...', &
'     1  3', &
'     2  4', &
'', &
'    the value of btest (a, 2)', &
'     F  F', &
'     F  T', &
'', &
'    the value of btest (2, a)', &
'     T  F', &
'     F  F', &
'', &
'STANDARD', &
'', &
'Fortran 95 and later', &
'', &
'SEE ALSO', &
'', &
'IEOR(3), IBCLR(3), NOT(3), IBCLR(3), IBITS(3), IBSET(3), IAND(3),', &
'IOR(3), IEOR(3), MVBITS(3)', &
'', &
'fortran-lang intrinsic descriptions (@urbanjost)', &
'']

shortname="btest"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('42','c_associated')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'C_ASSOCIATED(3) - [ISO_C_BINDING] Status of a C pointer (GFDL)', &
'', &
'SYNTAX', &
'', &
'    result = c_associated(c_prt_1, c_ptr_2)', &
'', &
'DESCRIPTION', &
'', &
'C_ASSOCIATED(C_PRT_1[, C_PTR_2]) determines the status of the C pointer', &
'c_ptr_1 or if c_ptr_1 is associated with the target c_ptr_2.', &
'', &
'ARGUMENTS', &
'', &
'    C_PTR_1', &
'        Scalar of the type c_ptr or c_funptr.', &
'', &
'    C_PTR_2', &
'        (Optional) Scalar of the same type as c_ptr_1.', &
'', &
'RETURNS', &
'', &
'The return value is of type _logical_; it is .false. if either c_ptr_1', &
'is a C NULL pointer or if c_ptr1 and c_ptr_2 point to different', &
'addresses.', &
'', &
'EXAMPLES', &
'', &
'Sample program:', &
'', &
'    program demo_c_associated', &
'', &
'    contains', &
'', &
'    subroutine association_test(a,b)', &
'    use iso_c_binding, only: c_associated, c_loc, c_ptr', &
'    implicit none', &
'    real, pointer :: a', &
'    type(c_ptr) :: b', &
'       if(c_associated(b, c_loc(a))) &', &
'          stop ''b and a do not point to same target''', &
'    end subroutine association_test', &
'', &
'    end program demo_c_associated', &
'', &
'STANDARD', &
'', &
'Fortran 2003 and later', &
'', &
'SEE ALSO', &
'', &
'C_LOC(3), C_FUNLOC(3), ISO_C_BINDING(3)', &
'', &
'fortran-lang intrinsic descriptions', &
'']

shortname="c_associated"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('43','ceiling')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'CEILING(3) - [NUMERIC] Integer ceiling function (GFDL)', &
'', &
'SYNTAX', &
'', &
'    result = ceiling(a, kind)', &
'', &
'       integer(kind=KIND) elemental function ceiling(a,kind)', &
'       real(kind=ANY),intent(in)   :: a', &
'       integer,intent(in),optional :: kind', &
'', &
'DESCRIPTION', &
'', &
'CEILING(A) returns the least integer greater than or equal to A.', &
'', &
'ARGUMENTS', &
'', &
'    A', &
'        The type shall be _real_.', &
'', &
'    KIND', &
'        An _integer_ initialization expression indicating the kind', &
'        parameter of the result.', &
'', &
'RETURNS', &
'', &
'The return value is of type INTEGER(kind) if KIND is present and a', &
'default-kind _integer_ otherwise.', &
'', &
'The result is undefined if it cannot be represented in the specified', &
'integer type.', &
'', &
'EXAMPLES', &
'', &
'Sample program:', &
'', &
'    program demo_ceiling', &
'    implicit none', &
'    real :: x = 63.29', &
'    real :: y = -63.59', &
'       print *, ceiling(x)', &
'       print *, ceiling(y)', &
'       ! elemental', &
'       print *,ceiling([ &', &
'       &  -2.7,  -2.5, -2.2, -2.0, -1.5, -1.0, -0.5, &', &
'       &  0.0,   &', &
'       &  +0.5,  +1.0, +1.5, +2.0, +2.2, +2.5, +2.7  ])', &
'    end program demo_ceiling', &
'', &
'Results:', &
'', &
'       64', &
'      -63', &
'       -2      -2      -2      -2      -1      -1', &
'        0       0       1       1       2       2', &
'        3       3       3', &
'', &
'STANDARD', &
'', &
'Fortran 95 and later', &
'', &
'SEE ALSO', &
'', &
'FLOOR(3), NINT(3)', &
'', &
'AINT(3), ANINT(3), INT(3), SELECTED_INT_KIND(3)', &
'', &
'fortran-lang intrinsic descriptions', &
'']

shortname="ceiling"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('44','c_f_pointer')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'C_F_POINTER(3) - [ISO_C_BINDING] Convert C into Fortran pointer (GFDL)', &
'', &
'SYNTAX', &
'', &
'    call c_f_pointer(cptr, fptr, shape)', &
'', &
'DESCRIPTION', &
'', &
'C_F_POINTER(CPTR, FPTR[, SHAPE]) Assign the target, the C pointer, CPTR', &
'to the Fortran pointer FPTR and specify its shape.', &
'', &
'ARGUMENTS', &
'', &
'    CPTR', &
'        scalar of the type c_ptr. It is INTENT(IN).', &
'', &
'    FPTR', &
'        pointer interoperable with CPTR. it is INTENT(OUT).', &
'', &
'    SHAPE', &
'        (Optional) Rank-one array of type _integer_ with INTENT(IN) . It', &
'        shall be present if and only if FPTR is an array. The size must', &
'        be equal to the rank of FPTR.', &
'', &
'EXAMPLES', &
'', &
'Sample program:', &
'', &
'    program demo_c_f_pointer', &
'    use iso_c_binding', &
'    implicit none', &
'    interface', &
'       subroutine my_routine(p) bind(c,name=''myC_func'')', &
'          import :: c_ptr', &
'          type(c_ptr), intent(out) :: p', &
'       end subroutine', &
'    end interface', &
'    type(c_ptr) :: cptr', &
'    real,pointer :: a(:)', &
'       call my_routine(cptr)', &
'       call c_f_pointer(cptr, a, [12])', &
'    end program demo_c_f_pointer', &
'', &
'STANDARD', &
'', &
'Fortran 2003 and later', &
'', &
'SEE ALSO', &
'', &
'C_LOC(3), C_F_PROCPOINTER(3), ISO_C_BINDING(3)', &
'', &
'fortran-lang intrinsic descriptions', &
'']

shortname="c_f_pointer"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('45','c_f_procpointer')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'C_F_PROCPOINTER(3) - [ISO_C_BINDING] Convert C into Fortran procedure', &
'pointer (GFDL)', &
'', &
'SYNTAX', &
'', &
'    call c_f_procpointer(cptr, fptr)', &
'', &
'DESCRIPTION', &
'', &
'C_F_PROCPOINTER(CPTR, FPTR) assigns the target of the C function pointer', &
'CPTR to the Fortran procedure pointer FPTR.', &
'', &
'ARGUMENTS', &
'', &
'    CPTR', &
'        scalar of the type c_funptr. It is INTENT(IN).', &
'', &
'    FPTR', &
'        procedure pointer interoperable with CPTR. It is INTENT(OUT).', &
'', &
'EXAMPLES', &
'', &
'Sample program:', &
'', &
'    program demo_c_f_procpointer', &
'    use iso_c_binding', &
'    implicit none', &
'    abstract interface', &
'       function func(a)', &
'       import :: c_float', &
'       real(c_float), intent(in) :: a', &
'       real(c_float) :: func', &
'       end function', &
'    end interface', &
'    interface', &
'       function getIterFunc() bind(c,name="getIterFunc")', &
'       import :: c_funptr', &
'       type(c_funptr) :: getIterFunc', &
'       end function', &
'    end interface', &
'    type(c_funptr) :: cfunptr', &
'    procedure(func), pointer :: myFunc', &
'       cfunptr = getIterFunc()', &
'       call c_f_procpointer(cfunptr, myFunc)', &
'    end program demo_c_f_procpointer', &
'', &
'STANDARD', &
'', &
'Fortran 2003 and later', &
'', &
'SEE ALSO', &
'', &
'C_LOC(3), C_F_POINTER(3), ISO_C_BINDING(3)', &
'', &
'fortran-lang intrinsic descriptions', &
'']

shortname="c_f_procpointer"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('46','c_funloc')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'C_FUNLOC(3) - [ISO_C_BINDING] Obtain the C address of a procedure (GFDL)', &
'', &
'SYNTAX', &
'', &
'    result = c_funloc(x)', &
'', &
'DESCRIPTION', &
'', &
'C_FUNLOC(X) determines the C address of the argument.', &
'', &
'ARGUMENTS', &
'', &
'    X', &
'        Interoperable function or pointer to such function.', &
'', &
'RETURNS', &
'', &
'The return value is of type c_funptr and contains the C address of the', &
'argument.', &
'', &
'EXAMPLES', &
'', &
'Sample program:', &
'', &
'    ! program demo_c_funloc and module', &
'    module x', &
'    use iso_c_binding', &
'    implicit none', &
'    contains', &
'    subroutine sub(a) bind(c)', &
'    real(c_float) :: a', &
'       a = sqrt(a)+5.0', &
'    end subroutine sub', &
'    end module x', &
'    !', &
'    program demo_c_funloc', &
'    use iso_c_binding', &
'    use x', &
'    implicit none', &
'    interface', &
'       subroutine my_routine(p) bind(c,name=''myC_func'')', &
'         import :: c_funptr', &
'         type(c_funptr), intent(in) :: p', &
'       end subroutine', &
'    end interface', &
'       call my_routine(c_funloc(sub))', &
'    !', &
'    end program demo_c_funloc', &
'', &
'STANDARD', &
'', &
'Fortran 2003 and later', &
'', &
'SEE ALSO', &
'', &
'C_ASSOCIATED(3), C_LOC(3), C_F_POINTER(3),', &
'', &
'C_F_PROCPOINTER(3), ISO_C_BINDING(3)', &
'', &
'fortran-lang intrinsic descriptions', &
'']

shortname="c_funloc"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('47','char')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'CHAR(3) - [CHARACTER] Character conversion function (GFDL)', &
'', &
'SYNTAX', &
'', &
'    result = char(i, kind)', &
'       elemental integer function char(i,kind)', &
'', &
'        integer(kind=KIND),intent(in) :: c', &
'        integer,intent(in),optional :: KIND', &
'', &
'DESCRIPTION', &
'', &
'CHAR(I, KIND) returns the character represented by the integer I.', &
'', &
'ARGUMENTS', &
'', &
'    I', &
'        The type shall be _integer_.', &
'', &
'    KIND', &
'        (Optional) An _integer_ initialization expression indicating the', &
'        kind parameter of the result.', &
'', &
'RETURNS', &
'', &
'The return value is of type _character_', &
'', &
'EXAMPLES', &
'', &
'Sample program:', &
'', &
'    program demo_char', &
'    implicit none', &
'    integer :: i = 74', &
'    character(1) :: c', &
'        c = char(i)', &
'        print *, i, c ! returns ''J''', &
'    end program demo_char', &
'', &
'Results:', &
'', &
'                 74 J', &
'', &
'NOTE', &
'', &
'See ICHAR(3) for a discussion of converting between numerical values and', &
'formatted string representations.', &
'', &
'STANDARD', &
'', &
'FORTRAN 77 and later', &
'', &
'SEE ALSO', &
'', &
'ACHAR(3), IACHAR(3), ICHAR(3)', &
'', &
'Functions that perform operations on character strings, return lengths', &
'of arguments, and search for certain arguments:', &
'', &
'-   ELEMENTAL: ADJUSTL(3), ADJUSTR(3), INDEX(3), SCAN(3), VERIFY(3)', &
'', &
'-   NONELEMENTAL: LEN_TRIM(3), LEN(3), REPEAT(3), TRIM(3)', &
'', &
'fortran-lang intrinsic descriptions', &
'']

shortname="char"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('48','c_loc')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'C_LOC(3) - [ISO_C_BINDING] Obtain the C address of an object (GFDL)', &
'', &
'SYNTAX', &
'', &
'    result = c_loc(x)', &
'', &
'DESCRIPTION', &
'', &
'C_LOC(X) determines the C address of the argument.', &
'', &
'ARGUMENTS', &
'', &
'    X', &
'        Shall have either the _pointer_ or _target_ attribute. It shall', &
'        not be a coindexed object. It shall either be a variable with', &
'        interoperable type and kind type parameters, or be a scalar,', &
'        nonpolymorphic variable with no length type parameters.', &
'', &
'RETURNS', &
'', &
'The return value is of type c_ptr and contains the C address of the', &
'argument.', &
'', &
'EXAMPLES', &
'', &
'Sample program:', &
'', &
'       subroutine association_test(a,b)', &
'       use iso_c_binding, only: c_associated, c_loc, c_ptr', &
'       implicit none', &
'       real, pointer :: a', &
'       type(c_ptr) :: b', &
'         if(c_associated(b, c_loc(a))) &', &
'            stop ''b and a do not point to same target''', &
'       end subroutine association_test', &
'', &
'STANDARD', &
'', &
'Fortran 2003 and later', &
'', &
'SEE ALSO', &
'', &
'C_ASSOCIATED(3), C_FUNLOC(3), C_F_POINTER(3),', &
'', &
'C_F_PROCPOINTER(3), ISO_C_BINDING(3)', &
'', &
'fortran-lang intrinsic descriptions', &
'']

shortname="c_loc"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('49','cmplx')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'CMPLX(3) - [TYPE:NUMERIC] Complex conversion function (GFDL)', &
'', &
'SYNTAX', &
'', &
'    result = cmplx(x, y, kind)', &
'', &
'       complex elemental function :: cmplx', &
'       TYPE(kind=KIND),intent(in), x', &
'       TYPE(kind=KIND),intent(in),optional, y', &
'       integer,intent(in),optional :: kind', &
'', &
'DESCRIPTION', &
'', &
'To convert numeric variables to complex, use the CMPLX(3) function.', &
'Constants can be used to define a complex variable using the syntax', &
'', &
'          z8 = (1.2345678901234567d0, 1.2345678901234567d0)', &
'', &
'but this will not work for variables. You must use the CMPLX(3)', &
'function.', &
'', &
'CMPLX(X [, Y [, KIND]]) returns a complex number where X is converted to', &
'the _real_ component. If X is _complex_ then Y must not be present. If Y', &
'is present it is converted to the imaginary component. If Y is not', &
'present then the imaginary component is set to 0.0.', &
'', &
'CMPLX(3) AND DOUBLE PRECISION', &
'', &
'The Fortran 90 language defines CMPLX(3) as always returning a result', &
'that is type COMPLEX(KIND=KIND(0.0)).', &
'', &
'This means `CMPLX(D1,D2)'', where `D1'' and `D2'' are _doubleprecision_, is', &
'treated as: fortran', &
'', &
'          cmplx(sngl(d1), sngl(d2))', &
'', &
'_doubleprecision complex_ numbers require specifying a precision.', &
'', &
'It was necessary for Fortran 90 to specify this behavior for', &
'_doubleprecision_ arguments, since that is the behavior mandated by', &
'FORTRAN 77.', &
'', &
'So Fortran 90 extends the CMPLX(3) intrinsic by adding an extra argument', &
'used to specify the desired kind of complex result.', &
'', &
'          integer,parameter :: dp=kind(0.0d0)', &
'          complex(kind=dp) :: z8', &
'          !', &
'          ! NO: result is just the precision of default _real_ values', &
'          !     because KIND parameter is not specified', &
'          !', &
'          ! note this was stored with default real precision', &
'          z8 = cmplx(1.2345678901234567d0, 1.2345678901234567d0)', &
'          print *, ''NO, Z8='',z8,real(z8),aimag(z8)', &
'          z8 = cmplx(1.2345678901234567e0_dp, 1.2345678901234567e0_dp)', &
'          ! again, note components are just _real_', &
'          print *, ''NO, Z8='',z8,real(z8),aimag(z8)', &
'          !', &
'          ! YES', &
'          !', &
'          ! kind= makes it work', &
'          z8 = cmplx(1.2345678901234567d0, 1.2345678901234567d0,kind=dp)', &
'          print *, ''YES, Z8='',z8,real(z8),aimag(z8)', &
'', &
'F2018 COMPONENT SYNTAX The real and imaginary parts of a complex entity', &
'can be accessed independently with a component-like syntax in f2018:', &
'', &
'A complex-part-designator is', &
'', &
'``fortran designator % RE or designator % IM. ```', &
'', &
'Where the designator is of complex type.', &
'', &
'So designator%RE designates the real part of a complex value,', &
'designator%IM designates the imaginary part of complex value. The type', &
'of a complex-part-designator is _real_, and its kind and shape are those', &
'of the designator.', &
'', &
'The following are examples of complex part designators:', &
'', &
'           impedance%re           !-- Same value as _real_(impedance)', &
'           fft%im                 !-- Same value as AIMAG(fft)', &
'           x%im = 0.0             !-- Sets the imaginary part of x to zero', &
'', &
'ARGUMENTS', &
'', &
'-   X The type may be _integer_, _real_, or _complex_.', &
'', &
'-   Y (Optional; only allowed if X is not _complex_.). May be _integer_', &
'    or _real_.', &
'', &
'-   KIND (Optional) An _integer_ initialization expression indicating', &
'    the kind parameter of the result.', &
'', &
'RETURNS', &
'', &
'The return value is of _complex_ type, with a kind equal to KIND if it', &
'is specified. If KIND is not specified, the result is of the default', &
'_complex_ kind, regardless of the kinds of X and Y.', &
'', &
'EXAMPLES', &
'', &
'Sample program:', &
'', &
'    program demo_aimag', &
'    implicit none', &
'    integer,parameter :: dp=kind(0.0d0)', &
'    complex          :: z4', &
'    complex(kind=dp) :: z8', &
'       z4 = cmplx(1.23456789, 1.23456789)', &
'       print *, ''Z4='',z4', &
'       ! using kind=dp makes it keep DOUBLEPRECISION precision', &
'       z8 = cmplx(1.2345678901234567d0, 1.2345678901234567d0,kind=dp)', &
'       print *, ''Z8='',z8', &
'       ! NOTE:', &
'       ! The following is intuitive and works without calling cmplx(3)', &
'       ! but does not work for variables just constants', &
'       z8 = (1.2345678901234567d0 , 1.2345678901234567d0 )', &
'       print *, ''Z8 defined with constants='',z8', &
'    end program demo_aimag', &
'', &
'Typical Results:', &
'', &
'        Z4= (1.23456788,1.23456788)', &
'        Z8= (1.2345678901234567,1.2345678901234567)', &
'        Z8 defined with constants= (1.2345678901234567,1.2345678901234567)', &
'', &
'SEE ALSO', &
'', &
'-   AIMAG(3) - Imaginary part of complex number', &
'', &
'-   CMPLX(3) - Complex conversion function', &
'', &
'-   CONJG(3) - Complex conjugate function', &
'', &
'-   REAL(3) - Convert to real type', &
'', &
'STANDARD', &
'', &
'FORTRAN 77 and later', &
'', &
'fortran-lang intrinsic descriptions', &
'']

shortname="cmplx"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('50','co_broadcast')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'CO_BROADCAST(3) - [COLLECTIVE] Copy a value to all images the current', &
'set of images (GFDL)', &
'', &
'SYNTAX', &
'', &
'    call co_broadcast(a, source_image, stat, errmsg)', &
'', &
'DESCRIPTION', &
'', &
'CO_BROADCAST(3) copies the value of argument A on the image with image', &
'index source_image to all images in the current team. A becomes defined', &
'as if by intrinsic assignment. If the execution was successful and STAT', &
'is present, it is assigned the value zero. If the execution failed, STAT', &
'gets assigned a nonzero value and, if present, ERRMSG gets assigned a', &
'value describing the occurred error.', &
'', &
'ARGUMENTS', &
'', &
'    A', &
'        INTENT(INOUT) argument; shall have the same dynamic type and', &
'        type parameters on all images of the current team. If it is an', &
'        array, it shall have the same shape on all images.', &
'', &
'    SOURCE_IMAGE', &
'        a scalar integer expression. It shall have the same the same', &
'        value on all images and refer to an image of the current team.', &
'', &
'    STAT', &
'        (optional) a scalar integer variable', &
'', &
'    ERRMSG', &
'        (optional) a scalar character variable', &
'', &
'EXAMPLES', &
'', &
'Sample program:', &
'', &
'    program demo_co_broadcast', &
'    implicit none', &
'    integer :: val(3)', &
'       if (this_image() == 1) then', &
'          val = [1, 5, 3]', &
'       endif', &
'       call co_broadcast (val, source_image=1)', &
'       print *, this_image(), ":", val', &
'    end program demo_co_broadcast', &
'', &
'SEE ALSO', &
'', &
'CO_MAX(3), CO_MIN(3), CO_SUM(3), CO_REDUCE(3)', &
'', &
'fortran-lang intrinsic descriptions', &
'']

shortname="co_broadcast"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('51','co_lbound')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'CO_LBOUND(3) - [COLLECTIVE] Lower codimension bounds of an array (GFDL)', &
'', &
'SYNTAX', &
'', &
'    result = co_lbound(coarray, dim, kind)', &
'', &
'DESCRIPTION', &
'', &
'Returns the lower bounds of a coarray, or a single lower cobound along', &
'the DIM codimension.', &
'', &
'ARGUMENTS', &
'', &
'    ARRAY', &
'        Shall be an coarray, of any type.', &
'', &
'    DIM', &
'        (Optional) Shall be a scalar _integer_.', &
'', &
'    KIND', &
'        (Optional) An _integer_ initialization expression indicating the', &
'        kind parameter of the result.', &
'', &
'RETURNS', &
'', &
'The return value is of type _integer_ and of kind KIND. If KIND is', &
'absent, the return value is of default integer kind. If DIM is absent,', &
'the result is an array of the lower cobounds of COARRAY. If DIM is', &
'present, the result is a scalar corresponding to the lower cobound of', &
'the array along that codimension.', &
'', &
'STANDARD', &
'', &
'Fortran 2008 and later', &
'', &
'SEE ALSO', &
'', &
'CO_UBOUND(3), LBOUND(3)', &
'', &
'fortran-lang intrinsic descriptions', &
'']

shortname="co_lbound"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('52','co_max')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'CO_MAX(3) - [COLLECTIVE] Maximal value on the current set of images', &
'(GFDL)', &
'', &
'SYNTAX', &
'', &
'    call co_max(a, result_image, stat, errmsg)', &
'', &
'DESCRIPTION', &
'', &
'co_max determines element-wise the maximal value of A on all images of', &
'the current team. If result_image is present, the maximum values are', &
'returned in A on the specified image only and the value of A on the', &
'other images become undefined. If result_image is not present, the value', &
'is returned on all images. If the execution was successful and STAT is', &
'present, it is assigned the value zero. If the execution failed, STAT', &
'gets assigned a nonzero value and, if present, ERRMSG gets assigned a', &
'value describing the occurred error.', &
'', &
'ARGUMENTS', &
'', &
'    A', &
'        shall be an integer, real or character variable, which has the', &
'        same type and type parameters on all images of the team.', &
'', &
'    RESULT_IMAGE', &
'        (optional) a scalar integer expression; if present, it shall', &
'        have the same the same value on all images and refer to an image', &
'        of the current team.', &
'', &
'    STAT', &
'        (optional) a scalar integer variable', &
'', &
'    ERRMSG', &
'        (optional) a scalar character variable', &
'', &
'EXAMPLES', &
'', &
'Sample program:', &
'', &
'    program demo_co_max', &
'    implicit none', &
'    integer :: val', &
'       val = this_image()', &
'       call co_max(val, result_image=1)', &
'       if (this_image() == 1) then', &
'         write(*,*) "Maximal value", val  ! prints num_images()', &
'       endif', &
'    end program demo_co_max', &
'', &
'Results:', &
'', &
'        Maximal value           2', &
'', &
'STANDARD', &
'', &
'TS 18508 or later', &
'', &
'SEE ALSO', &
'', &
'CO_MIN(3), CO_SUM(3), CO_REDUCE(3), CO_BROADCAST(3)', &
'', &
'fortran-lang intrinsic descriptions', &
'']

shortname="co_max"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('53','co_min')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'CO_MIN(3) - [COLLECTIVE] Minimal value on the current set of images', &
'(GFDL)', &
'', &
'SYNTAX', &
'', &
'    call co_min(a, result_image, stat, errmsg)', &
'', &
'DESCRIPTION', &
'', &
'co_min determines element-wise the minimal value of A on all images of', &
'the current team. If result_image is present, the minimal values are', &
'returned in A on the specified image only and the value of A on the', &
'other images become undefined. If result_image is not present, the value', &
'is returned on all images. If the execution was successful and STAT is', &
'present, it is assigned the value zero. If the execution failed, STAT', &
'gets assigned a nonzero value and, if present, ERRMSG gets assigned a', &
'value describing the occurred error.', &
'', &
'ARGUMENTS', &
'', &
'    A', &
'        shall be an integer, real or character variable, which has the', &
'        same type and type parameters on all images of the team.', &
'', &
'    RESULT_IMAGE', &
'        (optional) a scalar integer expression; if present, it shall', &
'        have the same the same value on all images and refer to an image', &
'        of the current team.', &
'', &
'    STAT', &
'        (optional) a scalar integer variable', &
'', &
'    ERRMSG', &
'        (optional) a scalar character variable', &
'', &
'EXAMPLES', &
'', &
'Sample program:', &
'', &
'    program demo_co_min', &
'    implicit none', &
'    integer :: val', &
'       val = this_image()', &
'       call co_min(val, result_image=1)', &
'       if (this_image() == 1) then', &
'         write(*,*) "Minimal value", val  ! prints 1', &
'       endif', &
'    end program demo_co_min', &
'', &
'STANDARD', &
'', &
'TS 18508 or later', &
'', &
'SEE ALSO', &
'', &
'CO_MAX(3), CO_SUM(3), CO_REDUCE(3), CO_BROADCAST(3)', &
'', &
'fortran-lang intrinsic descriptions', &
'']

shortname="co_min"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('54','command_argument_count')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'COMMAND_ARGUMENT_COUNT(3) - [SYSTEM:COMMAND LINE] Get number of command', &
'line arguments', &
'', &
'SYNTAX', &
'', &
'        result = command_argument_count()', &
'', &
'         integer function command_argument_count() result(count)', &
'         integer :: count', &
'', &
'DESCRIPTION', &
'', &
'COMMAND_ARGUMENT_COUNT() returns the number of arguments passed on the', &
'command line when the containing program was invoked.', &
'', &
'ARGUMENTS', &
'', &
'None', &
'', &
'RETURNS', &
'', &
'    COUNT', &
'        The return value is of type default _integer_. It is the number', &
'        of arguments passed on the command line when the program was', &
'        invoked.', &
'', &
'EXAMPLES', &
'', &
'Sample program:', &
'', &
'    program demo_command_argument_count', &
'    implicit none', &
'    integer :: count', &
'       count = command_argument_count()', &
'       print *, count', &
'    end program demo_command_argument_count', &
'', &
'Sample output:', &
'', &
'       # the command verb does not count', &
'       ./test_command_argument_count', &
'           0', &
'       # quoted strings may count as one argument', &
'       ./test_command_argument_count count arguments', &
'           2', &
'       ./test_command_argument_count ''count arguments''', &
'           1', &
'', &
'STANDARD', &
'', &
'Fortran 2003 and later', &
'', &
'SEE ALSO', &
'', &
'GET_COMMAND(3), GET_COMMAND_ARGUMENT(3)', &
'', &
'fortran-lang intrinsic descriptions (@urbanjost)', &
'']

shortname="command_argument_count"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('55','compiler_options')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'COMPILER_OPTIONS(3) - [COMPILER INQUIRY] Options passed to the compiler', &
'(GFDL)', &
'', &
'SYNTAX', &
'', &
'    str = compiler_options()', &
'', &
'DESCRIPTION', &
'', &
'compiler_options returns a string with the options used for compiling.', &
'', &
'ARGUMENTS', &
'', &
'None.', &
'', &
'RETURNS', &
'', &
'The return value is a default-kind string with system-dependent length.', &
'It contains the compiler flags used to compile the file, which called', &
'the compiler_options intrinsic.', &
'', &
'EXAMPLES', &
'', &
'Sample program:', &
'', &
'    program demo_compiler_version', &
'    use, intrinsic :: iso_fortran_env, only : compiler_version, compiler_options', &
'    implicit none', &
'       print ''(4a)'', &', &
'          ''This file was compiled by '', &', &
'          compiler_version(),           &', &
'          '' using the options '',        &', &
'          compiler_options()', &
'    end program demo_compiler_version', &
'', &
'Results:', &
'', &
'       This file was compiled by GCC version 5.4.0 using the options', &
'       -I /usr/include/w32api -I /home/urbanjs/V600/lib/CYGWIN64_GFORTRAN', &
'       -mtune=generic -march=x86-64 -g -Wunused -Wuninitialized -Wall', &
'       -std=f2008 -fbounds-check -fbacktrace -finit-real=nan', &
'       -fno-range-check -frecord-marker=4', &
'       -J /home/urbanjs/V600/lib/CYGWIN64_GFORTRAN', &
'', &
'STANDARD', &
'', &
'Fortran 2008', &
'', &
'SEE ALSO', &
'', &
'COMPILER_VERSION(3), ISO_FORTRAN_ENV(7)', &
'', &
'fortran-lang intrinsic descriptions', &
'']

shortname="compiler_options"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('56','compiler_version')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'COMPILER_VERSION(3) - [COMPILER INQUIRY] Compiler version string (GFDL)', &
'', &
'SYNTAX', &
'', &
'    str = compiler_version()', &
'', &
'DESCRIPTION', &
'', &
'COMPILER_VERSION(3) returns a string containing the name and version of', &
'the compiler.', &
'', &
'ARGUMENTS', &
'', &
'None.', &
'', &
'RETURNS', &
'', &
'The return value is a default-kind string with system-dependent length.', &
'It contains the name of the compiler and its version number.', &
'', &
'EXAMPLES', &
'', &
'Sample program:', &
'', &
'    program demo_compiler_version', &
'    use, intrinsic :: iso_fortran_env, only : compiler_version, compiler_options', &
'    implicit none', &
'       print ''(4a)'', &', &
'          ''This file was compiled by '', &', &
'          compiler_version(),           &', &
'          '' using the options '',        &', &
'          compiler_options()', &
'    end program demo_compiler_version', &
'', &
'Results:', &
'', &
'       This file was compiled by GCC version 5.4.0 using the options', &
'       -I /usr/include/w32api -I /home/urbanjs/V600/lib/CYGWIN64_GFORTRAN', &
'       -mtune=generic -march=x86-64 -g -Wunused -Wuninitialized -Wall', &
'       -std=f2008 -fbounds-check -fbacktrace -finit-real=nan', &
'       -fno-range-check -frecord-marker=4', &
'       -J /home/urbanjs/V600/lib/CYGWIN64_GFORTRAN', &
'', &
'STANDARD', &
'', &
'Fortran 2008', &
'', &
'SEE ALSO', &
'', &
'COMPILER_OPTIONS(3), ISO_FORTRAN_ENV(7)', &
'', &
'fortran-lang intrinsic descriptions', &
'']

shortname="compiler_version"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('57','conjg')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'CONJG(3) - [NUMERIC] Complex conjugate of a complex value', &
'', &
'SYNTAX', &
'', &
'    z = conjg(z)', &
'', &
'       complex(kind=K) elemental function conjg(z)', &
'       complex(kind=K),intent(in) :: z', &
'', &
'where K is the kind of the parameter Z', &
'', &
'DESCRIPTION', &
'', &
'CONJG(Z) returns the complex conjugate of the _complex_ value Z.', &
'', &
'In mathematics, the complex conjugate of a complex_ number is the number', &
'with an equal real part and an imaginary part equal in magnitude but', &
'opposite in sign.', &
'', &
'That is, If Z is (X, Y) then the result is (X, -Y).', &
'', &
'For matrices of complex numbers, CONJG(ARRAY) represents the', &
'element-by-element conjugation of ARRAY; not the conjugate transpose of', &
'ARRAY .', &
'', &
'ARGUMENTS', &
'', &
'    Z', &
'        The type shall be _complex_.', &
'', &
'RETURNS', &
'', &
'The return value is of type _complex_.', &
'', &
'EXAMPLES', &
'', &
'Sample program:', &
'', &
'    program demo_conjg', &
'    use, intrinsic :: iso_fortran_env, only : real_kinds, &', &
'    & real32, real64, real128', &
'    implicit none', &
'    complex :: z = (2.0, 3.0)', &
'    complex(kind=real64) :: dz = (   &', &
'       &  1.2345678901234567_real64, &', &
'       & -1.2345678901234567_real64)', &
'    complex :: arr(3,3)', &
'    integer :: i', &
'', &
'        print *, z', &
'        z= conjg(z)', &
'        print *, z', &
'        print *', &
'', &
'        print *, dz', &
'        dz = conjg(dz)', &
'        print *, dz', &
'        print *', &
'', &
'        ! the function is elemental so it can take arrays', &
'        arr(1,:)=[(-1.0, 2.0),( 3.0, 4.0),( 5.0,-6.0)]', &
'        arr(2,:)=[( 7.0,-8.0),( 8.0, 9.0),( 9.0, 9.0)]', &
'        arr(3,:)=[( 1.0, 9.0),( 2.0, 0.0),(-3.0,-7.0)]', &
'', &
'        write(*,*)''original''', &
'        write(*,''(3("(",g8.2,",",g8.2,")",1x))'')(arr(i,:),i=1,3)', &
'        arr = conjg(arr)', &
'        write(*,*)''conjugate''', &
'        write(*,''(3("(",g8.2,",",g8.2,")",1x))'')(arr(i,:),i=1,3)', &
'', &
'    end program demo_conjg', &
'', &
'Results:', &
'', &
'     (2.000000,3.000000)', &
'     (2.000000,-3.000000)', &
'', &
'     (1.23456789012346,-1.23456789012346)', &
'     (1.23456789012346,1.23456789012346)', &
'', &
'     original', &
'    (-1.0    , 2.0    ) ( 3.0    , 4.0    ) ( 5.0    ,-6.0    )', &
'    ( 7.0    ,-8.0    ) ( 8.0    , 9.0    ) ( 9.0    , 9.0    )', &
'    ( 1.0    , 9.0    ) ( 2.0    , 0.0    ) (-3.0    ,-7.0    )', &
'', &
'     conjugate', &
'    (-1.0    ,-2.0    ) ( 3.0    ,-4.0    ) ( 5.0    , 6.0    )', &
'    ( 7.0    , 8.0    ) ( 8.0    ,-9.0    ) ( 9.0    ,-9.0    )', &
'    ( 1.0    ,-9.0    ) ( 2.0    , 0.0    ) (-3.0    , 7.0    )', &
'', &
'STANDARD', &
'', &
'FORTRAN 77 and later', &
'', &
'fortran-lang intrinsic descriptions (@urbanjost)', &
'']

shortname="conjg"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('58','co_reduce')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'CO_REDUCE(3) - [COLLECTIVE] Reduction of values on the current set of', &
'images (GFDL)', &
'', &
'SYNTAX', &
'', &
'    call co_reduce(a, operation, result_image, stat, errmsg)', &
'', &
'DESCRIPTION', &
'', &
'co_reduce determines element-wise the reduction of the value of A on all', &
'images of the current team. The pure function passed as OPERATION is', &
'used to pairwise reduce the values of A by passing either the value of A', &
'of different images or the result values of such a reduction as', &
'argument. If A is an array, the reduction is done element wise. If', &
'result_image is present, the result values are returned in A on the', &
'specified image only and the value of A on the other images become', &
'undefined. If result_image is not present, the value is returned on all', &
'images. If the execution was successful and STAT is present, it is', &
'assigned the value zero. If the execution failed, STAT gets assigned a', &
'nonzero value and, if present, ERRMSG gets assigned a value describing', &
'the occurred error.', &
'', &
'ARGUMENTS', &
'', &
'    A', &
'        is an INTENT(INOUT) argument and shall be nonpolymorphic. If it', &
'        is allocatable, it shall be allocated; if it is a pointer, it', &
'        shall be associated. A shall have the same type and type', &
'        parameters on all images of the team; if it is an array, it', &
'        shall have the same shape on all images.', &
'', &
'    OPERATION', &
'        pure function with two scalar nonallocatable arguments, which', &
'        shall be nonpolymorphic and have the same type and type', &
'        parameters as A. The function shall return a nonallocatable', &
'        scalar of the same type and type parameters as A. The function', &
'        shall be the same on all images and with regards to the', &
'        arguments mathematically commutative and associative. Note that', &
'        OPERATION may not be an elemental', &
'', &
'    -   FUNCTION, UNLESS IT IS AN INTRINSIC FUNCTION. result_image', &
'', &
'    -   (optional) a scalar integer expression; if present, it shall', &
'        have the same the same value on all images and refer to an image', &
'        of the current team.', &
'', &
'    STAT', &
'        (optional) a scalar integer variable', &
'', &
'    ERRMSG', &
'        (optional) a scalar character variable', &
'', &
'EXAMPLES', &
'', &
'Sample program:', &
'', &
'    program demo_co_reduce', &
'    implicit none', &
'    integer :: val', &
'', &
'       val = this_image()', &
'       call co_reduce(val, myprod, 1)', &
'       if (this_image() == 1) then', &
'          write(*,*) "Product value", val  ! prints num_images() factorial', &
'       endif', &
'', &
'    contains', &
'', &
'    pure function myprod(a, b)', &
'       integer, value :: a, b', &
'       integer :: myprod', &
'       myprod = a * b', &
'    end function myprod', &
'', &
'    end program demo_co_reduce', &
'', &
'NOTE', &
'', &
'While the rules permit in principle an intrinsic function, none of the', &
'intrinsics in the standard fulfill the criteria of having a specific', &
'function, which takes two arguments of the same type and returning that', &
'type as a result.', &
'', &
'STANDARD', &
'', &
'TS 18508 or later', &
'', &
'SEE ALSO', &
'', &
'CO_MIN(3), CO_MAX(3), CO_SUM(3), CO_BROADCAST(3)', &
'', &
'fortran-lang intrinsic descriptions', &
'']

shortname="co_reduce"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('59','cos')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'COS(3) - [MATHEMATICS:TRIGONOMETRIC] Cosine function', &
'', &
'SYNTAX', &
'', &
'    result = cos(x)', &
'', &
'       TYPE(kind=KIND),elemental :: cos', &
'       TYPE(kind=KIND,intent(in) :: x', &
'', &
'where TYPE may be _real_ or _complex_ and KIND may be any KIND supported', &
'by the associated type.', &
'', &
'DESCRIPTION', &
'', &
'COS(X) computes the cosine of an angle X given the size of the angle in', &
'radians.', &
'', &
'The cosine of a _real_ value is the ratio of the adjacent side to the', &
'hypotenuse of a right-angled triangle.', &
'', &
'ARGUMENTS', &
'', &
'    X', &
'        The type shall be _real_ or _complex_. X is assumed to be in', &
'        radians.', &
'', &
'RETURNS', &
'', &
'The return value is of the same type and kind as X.', &
'', &
'If X is of the type _real_, the return value lies in the range -1 <=', &
'COS(X) <= 1 .', &
'', &
'EXAMPLES', &
'', &
'Sample program:', &
'', &
'    program demo_cos', &
'    implicit none', &
'    doubleprecision,parameter :: PI=atan(1.0d0)*4.0d0', &
'       write(*,*)''COS(0.0)='',cos(0.0)', &
'       write(*,*)''COS(PI)='',cos(PI)', &
'       write(*,*)''COS(PI/2.0d0)='',cos(PI/2.0d0),'' EPSILON='',epsilon(PI)', &
'       write(*,*)''COS(2*PI)='',cos(2*PI)', &
'       write(*,*)''COS(-2*PI)='',cos(-2*PI)', &
'       write(*,*)''COS(-2000*PI)='',cos(-2000*PI)', &
'       write(*,*)''COS(3000*PI)='',cos(3000*PI)', &
'    end program demo_cos', &
'', &
'Results:', &
'', &
'       COS(0.0)=        1.00000000', &
'       COS(PI)=        -1.0000000000000000', &
'       COS(PI/2.0d0)=   6.1232339957367660E-017', &
'       EPSILON=         2.2204460492503131E-016', &
'       COS(2*PI)=       1.0000000000000000', &
'       COS(-2*PI)=      1.0000000000000000', &
'       COS(-2000*PI)=   1.0000000000000000', &
'', &
'STANDARD', &
'', &
'FORTRAN 77 and later', &
'', &
'SEE ALSO', &
'', &
'-   Wikipedia:sine and cosine', &
'', &
'ACOS(3), SIN(3), TAN(3)', &
'', &
'fortran-lang intrinsic descriptions', &
'']

shortname="cos"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('60','cosh')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'COSH(3) - [MATHEMATICS:TRIGONOMETRIC] Hyperbolic cosine function (GFDL)', &
'', &
'SYNTAX', &
'', &
'        result = cosh(x)', &
'', &
'         TYPE(kind=KIND) elemental function cosh(x)', &
'         TYPE(kind=KIND),intent(in) :: x', &
'', &
'where TYPE may be _real_ or _complex_ and KIND may be any supported kind', &
'for the associated type. The returned VALUE will be the same type and', &
'kind as the input value X.', &
'', &
'DESCRIPTION', &
'', &
'COSH(X) computes the hyperbolic cosine of X.', &
'', &
'ARGUMENTS', &
'', &
'    X', &
'        The type shall be _real_ or _complex_.', &
'', &
'RETURNS', &
'', &
'The return value has same type and kind as X. If X is _complex_, the', &
'imaginary part of the result is in radians.', &
'', &
'If X is _real_, the return value has a lower bound of one, COSH(X) >= 1.', &
'', &
'EXAMPLES', &
'', &
'Sample program:', &
'', &
'    program demo_cosh', &
'    use, intrinsic :: iso_fortran_env, only : &', &
'     & real_kinds, real32, real64, real128', &
'    implicit none', &
'    real(kind=real64) :: x = 1.0_real64', &
'        x = cosh(x)', &
'    end program demo_cosh', &
'', &
'STANDARD', &
'', &
'FORTRAN 77 and later, for a complex argument - Fortran 2008 or later', &
'', &
'SEE ALSO', &
'', &
'-   Wikipedia:hyperbolic functions', &
'', &
'Inverse function: ACOSH(3)', &
'', &
'fortran-lang intrinsic descriptions', &
'']

shortname="cosh"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('61','co_sum')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'CO_SUM(3) - [COLLECTIVE] Sum of values on the current set of images', &
'(GFDL)', &
'', &
'SYNTAX', &
'', &
'    call co_sum(a, result_image, stat, errmsg)', &
'', &
'DESCRIPTION', &
'', &
'co_sum sums up the values of each element of A on all images of the', &
'current team. If result_image is present, the summed-up values are', &
'returned in A on the specified image only and the value of A on the', &
'other images become undefined. If result_image is not present, the value', &
'is returned on all images. If the execution was successful and STAT is', &
'present, it is assigned the value zero. If the execution failed, STAT', &
'gets assigned a nonzero value and, if present, ERRMSG gets assigned a', &
'value describing the occurred error.', &
'', &
'ARGUMENTS', &
'', &
'    A', &
'        shall be an integer, real or complex variable, which has the', &
'        same type and type parameters on all images of the team.', &
'', &
'    RESULT_IMAGE', &
'        (optional) a scalar integer expression; if present, it shall', &
'        have the same the same value on all images and refer to an image', &
'        of the current team.', &
'', &
'    STAT', &
'        (optional) a scalar integer variable', &
'', &
'    ERRMSG', &
'        (optional) a scalar character variable', &
'', &
'EXAMPLES', &
'', &
'Sample program:', &
'', &
'    program demo_co_sum', &
'    implicit none', &
'    integer :: val', &
'       val = this_image()', &
'       call co_sum(val, result_image=1)', &
'       if (this_image() == 1) then', &
'          ! prints (n**2 + n)/2, with n = num_images()', &
'          write(*,*) "The sum is ", val', &
'       endif', &
'    end program demo_co_sum', &
'', &
'Results:', &
'', &
'        The sum is            1', &
'', &
'STANDARD', &
'', &
'TS 18508 or later', &
'', &
'SEE ALSO', &
'', &
'CO_MAX(3), CO_MIN(3), CO_REDUCE(3), CO_BROADCAST(3)', &
'', &
'fortran-lang intrinsic descriptions', &
'']

shortname="co_sum"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('62','co_ubound')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'CO_UBOUND(3) - [COLLECTIVE] Upper codimension bounds of an array (GFDL)', &
'', &
'SYNTAX', &
'', &
'    result = co_ubound(coarray, dim, kind)', &
'', &
'DESCRIPTION', &
'', &
'Returns the upper cobounds of a coarray, or a single upper cobound along', &
'the DIM codimension.', &
'', &
'ARGUMENTS', &
'', &
'    ARRAY', &
'        Shall be an coarray, of any type.', &
'', &
'    DIM', &
'        (Optional) Shall be a scalar _integer_.', &
'', &
'    KIND', &
'        (Optional) An _integer_ initialization expression indicating the', &
'        kind parameter of the result.', &
'', &
'RETURNS', &
'', &
'The return value is of type _integer_ and of kind KIND. If KIND is', &
'absent, the return value is of default integer kind. If DIM is absent,', &
'the result is an array of the lower cobounds of COARRAY. If DIM is', &
'present, the result is a scalar corresponding to the lower cobound of', &
'the array along that codimension.', &
'', &
'STANDARD', &
'', &
'Fortran 2008 and later', &
'', &
'SEE ALSO', &
'', &
'CO_LBOUND(3), LBOUND(3), UBOUND(3)', &
'', &
'fortran-lang intrinsic descriptions', &
'']

shortname="co_ubound"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('63','count')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'COUNT(3) - [ARRAY REDUCTION] Count function (GFDL)', &
'', &
'SYNTAX', &
'', &
'    result = count(mask, dim, kind)', &
'', &
'DESCRIPTION', &
'', &
'Counts the number of .TRUE. elements in a logical MASK, or, if the DIM', &
'argument is supplied, counts the number of elements along each row of', &
'the array in the DIM direction. If the array has zero size, or all of', &
'the elements of MASK are false, then the result is 0.', &
'', &
'ARGUMENTS', &
'', &
'    MASK', &
'        The type shall be _logical_.', &
'', &
'    DIM', &
'        (Optional) The type shall be _integer_.', &
'', &
'    KIND', &
'        (Optional) An _integer_ initialization expression indicating the', &
'        kind parameter of the result.', &
'', &
'RETURNS', &
'', &
'The return value is of type _integer_ and of kind KIND. If KIND is', &
'absent, the return value is of default integer kind. If DIM is present,', &
'the result is an array with a rank one less than the rank of ARRAY, and', &
'a size corresponding to the shape of ARRAY with the DIM dimension', &
'removed.', &
'', &
'EXAMPLES', &
'', &
'Sample program:', &
'', &
'    program demo_count', &
'    implicit none', &
'    integer, dimension(2,3) :: a, b', &
'    logical, dimension(2,3) :: mymask', &
'          a = reshape( [ 1, 2, 3, 4, 5, 6 ], [ 2, 3 ])', &
'          b = reshape( [ 0, 7, 3, 4, 5, 8 ], [ 2, 3 ])', &
'          print ''(3i3)'', a(1,:)', &
'          print ''(3i3)'', a(2,:)', &
'          print *', &
'          print ''(3i3)'', b(1,:)', &
'          print ''(3i3)'', b(2,:)', &
'          print *', &
'          mymask = a.ne.b', &
'          print ''(3l3)'', mymask(1,:)', &
'          print ''(3l3)'', mymask(2,:)', &
'          print *', &
'          print ''(3i3)'', count(mymask)', &
'          print *', &
'          print ''(3i3)'', count(mymask, 1)', &
'          print *', &
'          print ''(3i3)'', count(mymask, 2)', &
'    end program demo_count', &
'', &
'Expected Results:', &
'', &
'      1  3  5', &
'      2  4  6', &
'', &
'      0  3  5', &
'      7  4  8', &
'', &
'      T  F  F', &
'      T  F  T', &
'', &
'      3', &
'', &
'      2  0  1', &
'', &
'      1  2', &
'', &
'STANDARD', &
'', &
'Fortran 95 and later, with KIND argument - Fortran 2003 and later', &
'', &
'fortran-lang intrinsic descriptions', &
'']

shortname="count"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('64','cpu_time')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'CPU_TIME(3) - [SYSTEM:TIME] return CPU processor time in seconds', &
'', &
'SYNTAX', &
'', &
'         call cpu_time(time)', &
'         real,intent(out) :: time', &
'', &
'DESCRIPTION', &
'', &
'Returns a _real_ value representing the elapsed CPU time in seconds.', &
'This is useful for testing segments of code to determine execution time.', &
'', &
'The exact definition of time is left imprecise because of the', &
'variability in what different processors are able to provide.', &
'', &
'If no time source is available, TIME is set to a negative value.', &
'', &
'Note that TIME may contain a system dependent, arbitrary offset and may', &
'not start with 0.0. For cpu_time the absolute value is meaningless. Only', &
'differences between subsequent calls, as shown in the example below,', &
'should be used.', &
'', &
'A processor for which a single result is inadequate (for example, a', &
'parallel processor) might choose to provide an additional version for', &
'which time is an array.', &
'', &
'RETURNS', &
'', &
'    TIME', &
'        The type shall be _real_ with INTENT(OUT). It is assigned a', &
'        processor-dependent approximation to the processor time in', &
'        seconds. If the processor cannot return a meaningful time, a', &
'        processor-dependent negative value', &
'', &
'    -   IS RETURNED. The start time is left imprecise because the', &
'        purpose is to time sections of code, as in the example. This', &
'        might or might not include system overhead time.', &
'', &
'EXAMPLES', &
'', &
'Sample program:', &
'', &
'    program demo_cpu_time', &
'    implicit none', &
'    real :: start, finish', &
'       !', &
'       call cpu_time(start)', &
'       ! put code to test here', &
'       call cpu_time(finish)', &
'       !', &
'       ! writes processor time taken by the piece of code.', &
'       print ''("Processor Time = ",f6.3," seconds.")'',finish-start', &
'    end program demo_cpu_time', &
'', &
'Results:', &
'', &
'       Processor Time =  0.000 seconds.', &
'', &
'STANDARD', &
'', &
'Fortran 95 and later', &
'', &
'SEE ALSO', &
'', &
'SYSTEM_CLOCK(3), DATE_AND_TIME(3)', &
'', &
'fortran-lang intrinsic descriptions (@urbanjost)', &
'']

shortname="cpu_time"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('65','cshift')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'CSHIFT(3) - [TRANSFORMATIONAL] Circular shift elements of an array', &
'(GFDL)', &
'', &
'SYNTAX', &
'', &
'    result = cshift(array, shift, dim)', &
'', &
'DESCRIPTION', &
'', &
'CSHIFT(ARRAY, SHIFT [, DIM]) performs a circular shift on elements of', &
'ARRAY along the dimension of DIM. If DIM is omitted it is taken to be 1.', &
'DIM is a scalar of type _integer_ in the range of 1 <= DIM <= N, where', &
'"n" is the rank of ARRAY. If the rank of ARRAY is one, then all elements', &
'of ARRAY are shifted by SHIFT places. If rank is greater than one, then', &
'all complete rank one sections of ARRAY along the given dimension are', &
'shifted. Elements shifted out one end of each rank one section are', &
'shifted back in the other end.', &
'', &
'ARGUMENTS', &
'', &
'    ARRAY', &
'        Shall be an array of any type.', &
'', &
'    SHIFT', &
'        The type shall be _integer_.', &
'', &
'    DIM', &
'        The type shall be _integer_.', &
'', &
'RETURNS', &
'', &
'Returns an array of same type and rank as the ARRAY argument.', &
'', &
'EXAMPLES', &
'', &
'Sample program:', &
'', &
'    program demo_cshift', &
'    implicit none', &
'    integer, dimension(3,3) :: a', &
'        a = reshape( [ 1, 2, 3, 4, 5, 6, 7, 8, 9 ], [ 3, 3 ])', &
'        print ''(3i3)'', a(1,:)', &
'        print ''(3i3)'', a(2,:)', &
'        print ''(3i3)'', a(3,:)', &
'        a = cshift(a, SHIFT=[1, 2, -1], DIM=2)', &
'        print *', &
'        print ''(3i3)'', a(1,:)', &
'        print ''(3i3)'', a(2,:)', &
'        print ''(3i3)'', a(3,:)', &
'    end program demo_cshift', &
'', &
'Results:', &
'', &
'         1  4  7', &
'         2  5  8', &
'         3  6  9', &
'', &
'         4  7  1', &
'         8  2  5', &
'         9  3  6', &
'', &
'STANDARD', &
'', &
'Fortran 95 and later', &
'', &
'fortran-lang intrinsic descriptions', &
'']

shortname="cshift"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('66','c_sizeof')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'C_SIZEOF(3) - [ISO_C_BINDING] Size in bytes of an expression (GFDL)', &
'', &
'SYNTAX', &
'', &
'    n = c_sizeof(x)', &
'', &
'DESCRIPTION', &
'', &
'C_SIZEOF(X) calculates the number of bytes of storage the expression X', &
'occupies.', &
'', &
'ARGUMENTS', &
'', &
'    X', &
'        The argument shall be an interoperable data entity.', &
'', &
'RETURNS', &
'', &
'The return value is of type integer and of the system-dependent kind', &
'c_size_t (from the _iso_c_binding_ module). Its value is the number of', &
'bytes occupied by the argument. If the argument has the _pointer_', &
'attribute, the number of bytes of the storage area pointed to is', &
'returned. If the argument is of a derived type with _pointer_ or', &
'_allocatable_ components, the return value does not account for the', &
'sizes of the data pointed to by these components.', &
'', &
'EXAMPLES', &
'', &
'Sample program:', &
'', &
'    program demo_c_sizeof', &
'    use iso_c_binding', &
'    implicit none', &
'    real(c_float) :: r, s(5)', &
'       print *, (c_sizeof(s)/c_sizeof(r) == 5)', &
'    end program demo_c_sizeof', &
'', &
'Results:', &
'', &
'        T', &
'', &
'The example will print .true. unless you are using a platform where', &
'default _real_ variables are unusually padded.', &
'', &
'STANDARD', &
'', &
'Fortran 2008', &
'', &
'SEE ALSO', &
'', &
'STORAGE_SIZE(3)', &
'', &
'fortran-lang intrinsic descriptions', &
'']

shortname="c_sizeof"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('67','date_and_time')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'DATE_AND_TIME(3) - [SYSTEM:TIME] gets current time', &
'', &
'SYNTAX', &
'', &
'        subroutine date_and_time(date, time, zone, values)', &
'', &
'         character(len=8),intent(out),optional :: date', &
'         character(len=10),intent(out),optional :: time', &
'         character(len=5),intent(out),optional :: zone', &
'         integer,intent(out),optional :: values(8)', &
'', &
'DESCRIPTION', &
'', &
'DATE_AND_TIME(DATE, TIME, ZONE, VALUES) gets the corresponding date and', &
'time information from the real-time system clock.', &
'', &
'Unavailable time and date _character_ parameters return blanks.', &
'', &
'ARGUMENTS', &
'', &
'    DATE', &
'        The type shall be _character(len=8)_ or larger, and of default', &
'        kind. DATE has the form ccyymmdd.', &
'', &
'    TIME', &
'        The type shall be _character(len=10)_ or larger, and of default', &
'        kind. TIME has the form hhmmss.sss.', &
'', &
'    ZONE', &
'        The type shall be _character(len=5)_ or larger, and of default', &
'        kind. ZONE has form (+-)hhmm, representing the difference with', &
'        respect to Coordinated Universal Time (UTC).', &
'', &
'    VALUES', &
'        An _integer_ array of eight elements. On return VALUES contains:', &
'', &
'    - VALUES(1)', &
'        The year', &
'        -   VALUES(2)', &
'', &
'        The month', &
'        -   VALUES(3)', &
'', &
'        The day of the month', &
'        -   VALUES(4)', &
'', &
'        Time difference with UTC in minutes', &
'        -   VALUES(5)', &
'', &
'        The hour of the day', &
'        -   VALUES(6)', &
'', &
'        The minutes of the hour', &
'        -   VALUES(7)', &
'', &
'        The seconds of the minute', &
'        -   VALUES(8)', &
'', &
'        The milliseconds of the second', &
'', &
'EXAMPLES', &
'', &
'Sample program:', &
'', &
'    program demo_time_and_date', &
'    implicit none', &
'    character(len=8)     :: date', &
'    character(len=10)    :: time', &
'    character(len=5)     :: zone', &
'    integer,dimension(8) :: values', &
'', &
'        call date_and_time(date,time,zone,values)', &
'', &
'        ! using keyword arguments', &
'        call date_and_time(DATE=date,TIME=time,ZONE=zone)', &
'        print ''(*(g0))'',''DATE="'',date,''" TIME="'',time,''" ZONE="'',zone,''"''', &
'', &
'        call date_and_time(VALUES=values)', &
'        write(*,''(i5,a)'') &', &
'         & values(1),'' - The year'', &', &
'         & values(2),'' - The month'', &', &
'         & values(3),'' - The day of the month'', &', &
'         & values(4),'' - Time difference with UTC in minutes'', &', &
'         & values(5),'' - The hour of the day'', &', &
'         & values(6),'' - The minutes of the hour'', &', &
'         & values(7),'' - The seconds of the minute'', &', &
'         & values(8),'' - The milliseconds of the second''', &
'    end program demo_time_and_date', &
'', &
'Results:', &
'', &
'       DATE="20201222" TIME="165738.779" ZONE="-0500"', &
'        2020 - The year', &
'          12 - The month', &
'          22 - The day of the month', &
'        -300 - Time difference with UTC in minutes', &
'          16 - The hour of the day', &
'          57 - The minutes of the hour', &
'          38 - The seconds of the minute', &
'         779 - The milliseconds of the second', &
'', &
'STANDARD', &
'', &
'Fortran 95 and later', &
'', &
'SEE ALSO', &
'', &
'CPU_TIME(3), SYSTEM_CLOCK(3)', &
'', &
'RESOURCES', &
'', &
'date and time conversion, formatting and computation', &
'', &
'-   M_time', &
'-   datetime', &
'-   datetime-fortran', &
'', &
'fortran-lang intrinsic descriptions (@urbanjost)', &
'']

shortname="date_and_time"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('68','dble')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'DBLE(3) - [TYPE:NUMERIC] Double conversion function (GFDL)', &
'', &
'SYNTAX', &
'', &
'    result = dble(a)', &
'', &
'DESCRIPTION', &
'', &
'DBLE(A) Converts A to double precision real type.', &
'', &
'ARGUMENTS', &
'', &
'    A', &
'        The type shall be _integer_, _real_, or _complex_.', &
'', &
'RETURNS', &
'', &
'The return value is of type _doubleprecision_.', &
'', &
'EXAMPLES', &
'', &
'Sample program:', &
'', &
'    program demo_dble', &
'    implicit none', &
'    real:: x = 2.18', &
'    integer :: i = 5', &
'    complex :: z = (2.3,1.14)', &
'       print *, dble(x), dble(i), dble(z)', &
'    end program demo_dble', &
'', &
'Results:', &
'', &
'      2.1800000667572021  5.0000000000000000   2.2999999523162842', &
'', &
'STANDARD', &
'', &
'FORTRAN 77 and later', &
'', &
'SEE ALSO', &
'', &
'FLOAT(3), REAL(3)', &
'', &
'fortran-lang intrinsic descriptions', &
'']

shortname="dble"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('69','digits')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'DIGITS(3) - [NUMERIC MODEL] Significant digits function (GFDL)', &
'', &
'SYNTAX', &
'', &
'    result = digits(x)', &
'', &
'DESCRIPTION', &
'', &
'DIGITS(X) returns the number of significant digits of the internal model', &
'representation of X. For example, on a system using a 32-bit floating', &
'point representation, a default real number would likely return 24.', &
'', &
'ARGUMENTS', &
'', &
'    X', &
'        The type may be _integer_ or _real_.', &
'', &
'RETURNS', &
'', &
'The return value is of type _integer_.', &
'', &
'EXAMPLES', &
'', &
'Sample program:', &
'', &
'    program demo_digits', &
'    implicit none', &
'    integer :: i = 12345', &
'    real :: x = 3.143', &
'    doubleprecision :: y = 2.33d0', &
'       print *,''default integer:'', digits(i)', &
'       print *,''default real:   '', digits(x)', &
'       print *,''default doubleprecision:'', digits(y)', &
'    end program demo_digits', &
'', &
'Typical Results:', &
'', &
'        default integer:                  31', &
'        default real:                     24', &
'        default doubleprecision:          53', &
'', &
'STANDARD', &
'', &
'Fortran 95 and later', &
'', &
'SEE ALSO', &
'', &
'EPSILON(3), EXPONENT(3), FRACTION(3), HUGE(3), MAXEXPONENT(3),', &
'MINEXPONENT(3), NEAREST(3), PRECISION(3), RADIX(3), RANGE(3),', &
'RRSPACING(3), SCALE(3), SET_EXPONENT(3), SPACING(3), TINY(3)', &
'', &
'fortran-lang intrinsic descriptions', &
'']

shortname="digits"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('70','dim')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'DIM(3) - [NUMERIC] Positive difference (GFDL)', &
'', &
'SYNTAX', &
'', &
'    result = dim(x, y)', &
'', &
'DESCRIPTION', &
'', &
'DIM(X,Y) returns the difference X - Y if the result is positive;', &
'otherwise returns zero.', &
'', &
'ARGUMENTS', &
'', &
'    X', &
'        The type shall be _integer_ or _real_', &
'', &
'    Y', &
'        The type shall be the same type and kind as X.', &
'', &
'RETURNS', &
'', &
'The return value is of type _integer_ or _real_.', &
'', &
'EXAMPLES', &
'', &
'Sample program:', &
'', &
'    program demo_dim', &
'    use, intrinsic :: iso_fortran_env, only : real_kinds, &', &
'    & real32, real64, real128', &
'    implicit none', &
'    integer :: i', &
'    real(kind=real64) :: x', &
'        i = dim(4, 15)', &
'        x = dim(4.345_real64, 2.111_real64)', &
'        print *, i', &
'        print *, x', &
'    end program demo_dim', &
'', &
'Results:', &
'', &
'                  0', &
'          2.2339999999999995', &
'', &
'STANDARD', &
'', &
'FORTRAN 77 and later', &
'', &
'fortran-lang intrinsic descriptions', &
'']

shortname="dim"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('71','dot_product')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'DOT_PRODUCT(3) - [TRANSFORMATIONAL] Dot product function (GFDL)', &
'', &
'SYNTAX', &
'', &
'    result = dot_product(vector_a, vector_b)', &
'', &
'DESCRIPTION', &
'', &
'DOT_PRODUCT(VECTOR_A, VECTOR_B) computes the dot product multiplication', &
'of two vectors vector_a and vector_b. The two vectors may be either', &
'numeric or logical and must be arrays of rank one and of equal size. If', &
'the vectors are _integer_ or _real_, the result is', &
'SUM(VECTOR_A*VECTOR_B). If the vectors are _complex_, the result is', &
'SUM(CONJG(VECTOR_A)*VECTOR_B). If the vectors are _logical_, the result', &
'is ANY(VECTOR_A .AND. VECTOR_B).', &
'', &
'ARGUMENTS', &
'', &
'    VECTOR_A', &
'        The type shall be numeric or _logical_, rank 1.', &
'', &
'    VECTOR_B', &
'        The type shall be numeric if vector_a is of numeric type or', &
'        _logical_ if vector_a is of type _logical_. vector_b shall be a', &
'        rank-one array.', &
'', &
'RETURNS', &
'', &
'If the arguments are numeric, the return value is a scalar of numeric', &
'type, _integer_, _real_, or _complex_. If the arguments are _logical_,', &
'the return value is .true. or .false..', &
'', &
'EXAMPLES', &
'', &
'Sample program:', &
'', &
'    program demo_dot_prod', &
'    implicit none', &
'        integer, dimension(3) :: a, b', &
'        a = [ 1, 2, 3 ]', &
'        b = [ 4, 5, 6 ]', &
'        print ''(3i3)'', a', &
'        print *', &
'        print ''(3i3)'', b', &
'        print *', &
'        print *, dot_product(a,b)', &
'    end program demo_dot_prod', &
'', &
'Results:', &
'', &
'         1  2  3', &
'', &
'         4  5  6', &
'', &
'                 32', &
'', &
'STANDARD', &
'', &
'Fortran 95 and later', &
'', &
'fortran-lang intrinsic descriptions', &
'']

shortname="dot_product"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('72','dprod')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'DPROD(3) - [NUMERIC] Double product function (GFDL)', &
'', &
'SYNTAX', &
'', &
'    result = dprod(x, y)', &
'', &
'DESCRIPTION', &
'', &
'DPROD(X,Y) produces a higher _doubleprecision_ product of default _real_', &
'numbers X and Y.', &
'', &
'The result has a value equal to a processor-dependent approximation to', &
'the product of X and Y. It is recommended that the processor compute the', &
'product in double precision, rather than in single precision and then', &
'converted to double precision.', &
'', &
'    X', &
'        shall be default real.', &
'', &
'    Y', &
'        shall be default real.', &
'', &
'The setting of compiler options specifying _real_ size can affect this', &
'function.', &
'', &
'ARGUMENTS', &
'', &
'    X', &
'        Must be of default _real(kind=kind(0.0))_ type', &
'', &
'    Y', &
'        Must have the same type and kind parameters as X', &
'', &
'RETURNS', &
'', &
'The return value is of type _real(kind=kind(0.0d0))_.', &
'', &
'EXAMPLES', &
'', &
'Sample program:', &
'', &
'    program demo_dprod', &
'    use, intrinsic :: iso_fortran_env, only : real_kinds, &', &
'    & real32, real64, real128', &
'    implicit none', &
'    integer,parameter :: dp=kind(0.0d0)', &
'    real :: x = 5.2', &
'    real :: y = 2.3', &
'    real(kind=dp) :: dd', &
'       dd = dprod(x,y)', &
'       print *, dd, x*y, kind(x), kind(dd), kind(dprod(x,y))', &
'       ! interesting comparisons', &
'       print *, 52*23', &
'       print *, 52*23/100.0', &
'       print *, 52*23/100.0d0', &
'', &
'       !! common extension is to take doubleprecision arguments', &
'       !! and return higher precision', &
'       bigger: block', &
'       doubleprecision :: xx = 5.2d0', &
'       doubleprecision :: yy = 2.3d0', &
'       real(kind=real128) :: ddd', &
'       !ddd = dprod(xx,yy)', &
'       !print *, ddd, xx*yy, kind(xx), kind(ddd), kind(dprod(xx,yy))', &
'       endblock bigger', &
'', &
'    end program demo_dprod', &
'', &
'Results:', &
'', &
'       11.959999313354501 11.9599991 4 8 8', &
'            1196', &
'       11.9600000', &
'       11.960000000000001', &
'', &
'STANDARD', &
'', &
'FORTRAN 77 and later', &
'', &
'fortran-lang intrinsic descriptions', &
'']

shortname="dprod"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('73','dshiftl')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'DSHIFTL(3) - [BIT:COPY] combines bits of arguments I and J (GFDL)', &
'', &
'SYNTAX', &
'', &
'    result = dshiftl(i, j, shift)', &
'', &
'DESCRIPTION', &
'', &
'DSHIFTL(I, J, SHIFT) combines bits of I and J. The rightmost SHIFT bits', &
'of the result are the leftmost SHIFT bits of J, and the remaining bits', &
'are the rightmost bits of I.', &
'', &
'ARGUMENTS', &
'', &
'    I', &
'        Shall be of type _integer_.', &
'', &
'    J', &
'        Shall be of type _integer_, and of the same kind as I.', &
'', &
'    SHIFT', &
'        Shall be of type _integer_.', &
'', &
'RETURNS', &
'', &
'The return value has same type and kind as I.', &
'', &
'STANDARD', &
'', &
'Fortran 2008 and later', &
'', &
'SEE ALSO', &
'', &
'DSHIFTR(3)', &
'', &
'fortran-lang intrinsic descriptions', &
'']

shortname="dshiftl"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('74','dshiftr')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'DSHIFTR(3) - [BIT:COPY] combines bits of arguments I and J (GFDL)', &
'', &
'SYNTAX', &
'', &
'    result = dshiftr(i, j, shift)', &
'', &
'DESCRIPTION', &
'', &
'DSHIFTR(I, J, SHIFT) combines bits of I and J. The leftmost SHIFT bits', &
'of the result are the rightmost SHIFT bits of I, and the remaining bits', &
'are the leftmost bits of J.', &
'', &
'ARGUMENTS', &
'', &
'    I', &
'        Shall be of type _integer_.', &
'', &
'    J', &
'        Shall be of type _integer_, and of the same kind as I.', &
'', &
'    SHIFT', &
'        Shall be of type _integer_.', &
'', &
'RETURNS', &
'', &
'The return value has same type and kind as I.', &
'', &
'STANDARD', &
'', &
'Fortran 2008 and later', &
'', &
'SEE ALSO', &
'', &
'DSHIFTL(3)', &
'', &
'fortran-lang intrinsic descriptions', &
'']

shortname="dshiftr"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('75','eoshift')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'EOSHIFT(3) - [TRANSFORMATIONAL] End-off shift elements of an array', &
'(GFDL)', &
'', &
'SYNTAX', &
'', &
'    result = eoshift(array, shift, boundary, dim)', &
'', &
'DESCRIPTION', &
'', &
'EOSHIFT(ARRAY, SHIFT[, BOUNDARY, DIM]) performs an end-off shift on', &
'elements of ARRAY along the dimension of DIM. If DIM is omitted it is', &
'taken to be 1. DIM is a scalar of type _integer_ in the range of 1 <=', &
'DIM <= N where "N" is the rank of ARRAY. If the rank of ARRAY is one,', &
'then all elements of ARRAY are shifted by SHIFT places. If rank is', &
'greater than one, then all complete rank one sections of ARRAY along the', &
'given dimension are shifted. Elements shifted out one end of each rank', &
'one section are dropped. If BOUNDARY is present then the corresponding', &
'value of from BOUNDARY is copied back in the other end. If BOUNDARY is', &
'not present then the following are copied in depending on the type of', &
'ARRAY.', &
'', &
'*Array Type* - *Boundary Value*', &
'', &
'-   Numeric 0 of the type and kind of ARRAY', &
'', &
'-   Logical .false.', &
'', &
'-   CHARACTER(LEN) LEN blanks', &
'', &
'ARGUMENTS', &
'', &
'    ARRAY', &
'        May be any type, not scalar.', &
'', &
'    SHIFT', &
'        The type shall be _integer_.', &
'', &
'    BOUNDARY', &
'        Same type as ARRAY.', &
'', &
'    DIM', &
'        The type shall be _integer_.', &
'', &
'RETURNS', &
'', &
'Returns an array of same type and rank as the ARRAY argument.', &
'', &
'EXAMPLES', &
'', &
'Sample program:', &
'', &
'    program demo_eoshift', &
'    implicit none', &
'        integer, dimension(3,3) :: a', &
'        a = reshape( [ 1, 2, 3, 4, 5, 6, 7, 8, 9 ], [ 3, 3 ])', &
'        print ''(3i3)'', a(1,:)', &
'        print ''(3i3)'', a(2,:)', &
'        print ''(3i3)'', a(3,:)', &
'        a = eoshift(a, SHIFT=[1, 2, 1], BOUNDARY=-5, DIM=2)', &
'        print *', &
'        print ''(3i3)'', a(1,:)', &
'        print ''(3i3)'', a(2,:)', &
'        print ''(3i3)'', a(3,:)', &
'    end program demo_eoshift', &
'', &
'Results:', &
'', &
'         1  4  7', &
'         2  5  8', &
'         3  6  9', &
'', &
'         4  7 -5', &
'         8 -5 -5', &
'         6  9 -5', &
'', &
'STANDARD', &
'', &
'Fortran 95 and later', &
'', &
'fortran-lang intrinsic descriptions', &
'']

shortname="eoshift"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('76','epsilon')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'EPSILON(3) - [NUMERIC MODEL] Epsilon function', &
'', &
'SYNTAX', &
'', &
'    result = epsilon(x)', &
'', &
'DESCRIPTION', &
'', &
'EPSILON(X) returns the floating point relative accuracy. It is the', &
'nearly negligible number relative to 1 such that 1+ LITTLE_NUMBER is not', &
'equal to 1; or more precisely', &
'', &
'       real( 1.0, kind(x)) + epsilon(x) /=  real( 1.0, kind(x))', &
'', &
'It may be thought of as the distance from 1.0 to the next largest', &
'floating point number.', &
'', &
'One use of EPSILON(3) is to select a _delta_ value for algorithms that', &
'search until the calculation is within _delta_ of an estimate.', &
'', &
'If _delta_ is too small the algorithm might never halt, as a computation', &
'summing values smaller than the decimal resolution of the data type does', &
'not change.', &
'', &
'ARGUMENTS', &
'', &
'    X', &
'        The type shall be _real_.', &
'', &
'RETURNS', &
'', &
'The return value is of the same type as the argument.', &
'', &
'EXAMPLES', &
'', &
'Sample program:', &
'', &
'    program demo_epsilon', &
'    use,intrinsic :: iso_fortran_env, only : dp=>real64,sp=>real32', &
'    implicit none', &
'    real(kind=sp) :: x = 3.143', &
'    real(kind=dp) :: y = 2.33d0', &
'', &
'       ! so if x is of type real32, epsilon(x) has the value 2**-23', &
'       print *, epsilon(x)', &
'       ! note just the type and kind of x matter, not the value', &
'       print *, epsilon(huge(x))', &
'       print *, epsilon(tiny(x))', &
'', &
'       ! the value changes with the kind of the real value though', &
'       print *, epsilon(y)', &
'', &
'       ! adding and subtracting epsilon(x) changes x', &
'       write(*,*)x == x + epsilon(x)', &
'       write(*,*)x == x - epsilon(x)', &
'', &
'       ! these next two comparisons will be .true. !', &
'       write(*,*)x == x + epsilon(x) * 0.999999', &
'       write(*,*)x == x - epsilon(x) * 0.999999', &
'', &
'       ! you can calculate epsilon(1.0d0)', &
'       write(*,*)my_dp_eps()', &
'', &
'    contains', &
'', &
'    function my_dp_eps()', &
'    ! calculate the epsilon value of a machine the hard way', &
'    real(kind=dp) :: t', &
'    real(kind=dp) :: my_dp_eps', &
'', &
'       ! starting with a value of 1, keep dividing the value', &
'       ! by 2 until no change is detected. Note that with', &
'       ! infinite precision this would be an infinite loop,', &
'       ! but floating point values in Fortran have a defined', &
'       ! and limited precision.', &
'       my_dp_eps = 1.0d0', &
'       SET_ST: do', &
'          my_dp_eps = my_dp_eps/2.0d0', &
'          t = 1.0d0 + my_dp_eps', &
'          if (t <= 1.0d0) exit', &
'       enddo SET_ST', &
'       my_dp_eps = 2.0d0*my_dp_eps', &
'', &
'    end function my_dp_eps', &
'', &
'    end program demo_epsilon', &
'', &
'Results:', &
'', &
'      1.1920929E-07', &
'      1.1920929E-07', &
'      1.1920929E-07', &
'      2.220446049250313E-016', &
'     F', &
'     F', &
'     T', &
'     T', &
'      2.220446049250313E-016', &
'', &
'STANDARD', &
'', &
'Fortran 95 and later', &
'', &
'SEE ALSO', &
'', &
'DIGITS(3), EXPONENT(3), FRACTION(3), HUGE(3), MAXEXPONENT(3),', &
'MINEXPONENT(3), NEAREST(3), PRECISION(3), RADIX(3), RANGE(3),', &
'RRSPACING(3), SCALE(3), SET_EXPONENT(3), SPACING(3), TINY(3)', &
'', &
'fortran-lang intrinsic descriptions (@urbanjost)', &
'']

shortname="epsilon"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('77','erf')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'ERF(3) - [MATHEMATICS] Error function (GFDL)', &
'', &
'SYNTAX', &
'', &
'    result = erf(x)', &
'', &
'DESCRIPTION', &
'', &
'ERF(x) computes the error function of X, defined as $$ \text{erf}(x) =', &
'\frac{2}{\sqrt{\pi}} \int_0^x e^({-T)2} dt. $$', &
'', &
'ARGUMENTS', &
'', &
'    X', &
'        The type shall be _real_.', &
'', &
'RETURNS', &
'', &
'The return value is of type _real_, of the same kind as X and lies in', &
'the range -1 <= ERF(x) <= 1 .', &
'', &
'EXAMPLES', &
'', &
'Sample program:', &
'', &
'    program demo_erf', &
'    use, intrinsic :: iso_fortran_env, only : real_kinds, &', &
'     & real32, real64, real128', &
'    implicit none', &
'    real(kind=real64) :: x = 0.17_real64', &
'        write(*,*)x, erf(x)', &
'    end program demo_erf', &
'', &
'Results:', &
'', &
'         0.17000000000000001       0.18999246120180879', &
'', &
'STANDARD', &
'', &
'Fortran 2008 and later', &
'', &
'See also', &
'', &
'ERFC(3)', &
'', &
'-   Wikipedia:error function', &
'', &
'fortran-lang intrinsic descriptions', &
'']

shortname="erf"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('78','erfc')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'ERFC(3) - [MATHEMATICS] Complementary error function (GFDL)', &
'', &
'SYNTAX', &
'', &
'    result = erfc(x)', &
'', &
'       elemental function erfc(x)', &
'       real(kind=KIND) :: erfc', &
'       real(kind=KIND),intent(in) :: x', &
'', &
'DESCRIPTION', &
'', &
'ERFC(x) computes the complementary error function of X. Simpy put this', &
'is equivalent to 1 - ERF(X), but ERFC is provided because of the extreme', &
'loss of relative accuracy if ERF(X) is called for large X and the result', &
'is subtracted from 1.', &
'', &
'ERFC(X) is defined as', &
'', &
'$$ \text{erfc}(x) = 1 - \text{erf}(x) = 1 - \frac{2}{\sqrt{\pi}}', &
'\int_x^{\infty} e^({-t)2} dt. $$', &
'', &
'ARGUMENTS', &
'', &
'    X', &
'        The type shall be _real_.', &
'', &
'RETURNS', &
'', &
'The return value is of type _real_ and of the same kind as X. It lies in', &
'the range', &
'', &
'  0 <= ERFC(x) <= 2.', &
'', &
'EXAMPLES', &
'', &
'Sample program:', &
'', &
'    program demo_erfc', &
'    use, intrinsic :: iso_fortran_env, only : &', &
'     & real_kinds, real32, real64, real128', &
'    implicit none', &
'    real(kind=real64) :: x = 0.17_real64', &
'        write(*,*)x, erfc(x)', &
'    end program demo_erfc', &
'', &
'Results:', &
'', &
'         0.17000000000000001       0.81000753879819121', &
'', &
'STANDARD', &
'', &
'Fortran 2008 and later', &
'', &
'See also', &
'', &
'ERF(3)', &
'', &
'-   Wikipedia:error function', &
'', &
'fortran-lang intrinsic descriptions @urbanjost', &
'']

shortname="erfc"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('79','erfc_scaled')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'ERFC_SCALED(3) - [MATHEMATICS] Error function (GFDL)', &
'', &
'SYNTAX', &
'', &
'    result = erfc_scaled(x)', &
'', &
'DESCRIPTION', &
'', &
'ERFC_SCALED(x) computes the exponentially-scaled complementary error', &
'function of X:', &
'', &
'$$ e**{x**2} \frac{2}{\sqrt{\pi}} \int_{x}**{\infty} e**{-T**2} dt. $$', &
'', &
'ARGUMENTS', &
'', &
'    X', &
'        The type shall be _real_.', &
'', &
'RETURNS', &
'', &
'The return value is of type _real_ and of the same kind as X.', &
'', &
'EXAMPLES', &
'', &
'Sample program:', &
'', &
'    program demo_erfc_scaled', &
'    implicit none', &
'    real(kind(0.0d0)) :: x = 0.17d0', &
'       x = erfc_scaled(x)', &
'       print *, x', &
'    end program demo_erfc_scaled', &
'', &
'Results:', &
'', &
'         0.83375830214998126', &
'', &
'STANDARD', &
'', &
'Fortran 2008 and later', &
'', &
'fortran-lang intrinsic descriptions', &
'']

shortname="erfc_scaled"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('80','event_query')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'EVENT_QUERY(3) - [COLLECTIVE] Query whether a coarray event has occurred', &
'(GFDL)', &
'', &
'SYNTAX', &
'', &
'    call event_query(event, count, stat)', &
'', &
'DESCRIPTION', &
'', &
'EVENT_QUERY assigns the number of events to COUNT which have been posted', &
'to the EVENT variable and not yet been removed by calling EVENT_WAIT.', &
'When STAT is present and the invocation was successful, it is assigned', &
'the value 0. If it is present and the invocation has failed, it is', &
'assigned a positive value and COUNT is assigned the value -1.', &
'', &
'ARGUMENTS', &
'', &
'    EVENT', &
'        (intent(in)) Scalar of type event_type, defined in', &
'        iso_fortran_env; shall not be coindexed.', &
'', &
'    COUNT', &
'        (intent(out))Scalar integer with at least the precision of', &
'        default _integer_.', &
'', &
'    STAT', &
'        (OPTIONAL) Scalar default-kind _integer_ variable.', &
'', &
'EXAMPLES', &
'', &
'Sample program:', &
'', &
'    program demo_event_query', &
'    use iso_fortran_env', &
'    implicit none', &
'    type(event_type) :: event_value_has_been_set[*]', &
'    integer :: cnt', &
'       if (this_image() == 1) then', &
'          call event_query(event_value_has_been_set, cnt)', &
'          if (cnt > 0) write(*,*) "Value has been set"', &
'       elseif (this_image() == 2) then', &
'          event post(event_value_has_been_set[1])', &
'       endif', &
'    end program demo_event_query', &
'', &
'STANDARD', &
'', &
'TS 18508 or later', &
'', &
'fortran-lang intrinsic descriptions', &
'']

shortname="event_query"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('81','execute_command_line')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'EXECUTE_COMMAND_LINE(3) - [SYSTEM:PROCESSES] Execute a shell command', &
'(GFDL)', &
'', &
'SYNTAX', &
'', &
'       subroutine execute_command_line(command, wait, exitstat, cmdstat, cmdmsg)', &
'', &
'        character(len=*),intent(in)  :: command', &
'        logical,intent(in),optional  :: wait', &
'        integer,intent(out),optional :: exitstat', &
'        integer,intent(out),optional :: cmdstat', &
'        character(len=*),intent(out),optional :: cmdmsg', &
'', &
'DESCRIPTION', &
'', &
'The COMMAND argument is passed to the shell and executed. (The shell is', &
'generally SH(1) on Unix systems, and cmd.exe on Windows.) If WAIT is', &
'present and has the value .FALSE., the execution of the command is', &
'asynchronous if the system supports it; otherwise, the command is', &
'executed synchronously.', &
'', &
'The three last arguments allow the user to get status information. After', &
'synchronous execution, EXITSTAT contains the integer exit code of the', &
'command, as returned by SYSTEM. CMDSTAT is set to zero if the command', &
'line was executed (whatever its exit status was). CMDMSG is assigned an', &
'error message if an error has occurred.', &
'', &
'Note that the system call need not be thread-safe. It is the', &
'responsibility of the user to ensure that the system is not called', &
'concurrently if required.', &
'', &
'When the command is executed synchronously, EXECUTE_COMMAND_LINE returns', &
'after the command line has completed execution. Otherwise,', &
'EXECUTE_COMMAND_LINE returns without waiting.', &
'', &
'ARGUMENTS', &
'', &
'    COMMAND', &
'        a default _character_ scalar containing the command line to be', &
'        executed. The interpretation is programming-environment', &
'        dependent.', &
'', &
'    WAIT', &
'        (Optional) a default _logical_ scalar. If WAIT is present with', &
'        the value .false., and the processor supports asynchronous', &
'        execution of the command, the command is executed', &
'        asynchronously; otherwise it is executed synchronously.', &
'', &
'    EXITSTAT', &
'        (Optional) an _integer_ of the default kind with INTENT(INOUT).', &
'        If the command is executed synchronously, it is assigned the', &
'        value of the processor-dependent exit status. Otherwise, the', &
'        value of EXITSTAT is unchanged.', &
'', &
'    CMDSTAT', &
'        (Optional) an _integer_ of default kind with INTENT(INOUT). If', &
'        an error condition occurs and CMDSTAT is not present, error', &
'        termination of execution of the image is initiated.', &
'', &
'    It is assigned the value -1 if the processor does not support', &
'    command line execution, a processor-dependent positive value if an', &
'    error condition occurs, or the value -2 if no error condition occurs', &
'    but WAIT is present with the value false and the processor does not', &
'    support asynchronous execution. Otherwise it is assigned the', &
'    value 0.', &
'', &
'    CMDMSG', &
'        (Optional) a _character_ scalar of the default kind. It is an', &
'        INTENT (INOUT) argument.If an error condition occurs, it is', &
'        assigned a processor-dependent explanatory message.Otherwise, it', &
'        is unchanged.', &
'', &
'EXAMPLES', &
'', &
'Sample program:', &
'', &
'    program demo_exec', &
'    implicit none', &
'       integer :: i', &
'', &
'       call execute_command_line("external_prog.exe", exitstat=i)', &
'       print *, "Exit status of external_prog.exe was ", i', &
'', &
'       call execute_command_line("reindex_files.exe", wait=.false.)', &
'       print *, "Now reindexing files in the background"', &
'    end program demo_exec', &
'', &
'NOTE', &
'', &
'Because this intrinsic is making a system call, it is very system', &
'dependent. Its behavior with respect to signaling is processor', &
'dependent. In particular, on POSIX-compliant systems, the SIGINT and', &
'SIGQUIT signals will be ignored, and the SIGCHLD will be blocked. As', &
'such, if the parent process is terminated, the child process might not', &
'be terminated alongside.', &
'', &
'STANDARD', &
'', &
'Fortran 2008 and later', &
'', &
'fortran-lang intrinsic descriptions', &
'']

shortname="execute_command_line"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('82','exp')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'EXP(3) - [MATHEMATICS] Exponential function', &
'', &
'SYNTAX', &
'', &
'    result = exp(x)', &
'', &
'DESCRIPTION', &
'', &
'EXP(x) computes the base "_e_" exponential of X where "_e_" is _Euler''s', &
'constant_.', &
'', &
'If X is of type _complex_, its imaginary part is regarded as a value in', &
'radians such that (see _Euler''s formula_):', &
'', &
'if CX=(RE,IM) then __exp(cx)=exp(re)*cmplx(cos(im),sin(im))__', &
'', &
'Since EXP(3) is the inverse function of LOG(3) the maximum valid', &
'magnitude of the _real_ component of X is LOG(HUGE(X)).', &
'', &
'ARGUMENTS', &
'', &
'    X', &
'        The type shall be _real_ or _complex_.', &
'', &
'RETURNS', &
'', &
'The value of the result is E**X where E is Euler''s constant.', &
'', &
'The return value has the same type and kind as X.', &
'', &
'EXAMPLES', &
'', &
'Sample program:', &
'', &
'    program demo_exp', &
'    implicit none', &
'    real :: x , re, im', &
'    complex :: cx', &
'', &
'       x = 1.0', &
'       write(*,*)"Euler''s constant is approximately",exp(x)', &
'', &
'       !! complex values', &
'       ! given', &
'       re=3.0', &
'       im=4.0', &
'       cx=cmplx(re,im)', &
'', &
'       ! complex results from complex arguments are Related to Euler''s formula', &
'       write(*,*)''given the complex value '',cx', &
'       write(*,*)''exp(x) is'',exp(cx)', &
'       write(*,*)''is the same as'',exp(re)*cmplx(cos(im),sin(im))', &
'', &
'       ! exp(3) is the inverse function of log(3) so', &
'       ! the real compoenent of the input must be less than or equal to', &
'       write(*,*)''maximum real real component'',log(huge(0.0))', &
'       ! or for double precision', &
'       write(*,*)''maximum doubleprecision real component'',log(huge(0.0d0))', &
'', &
'       ! but since the imaginary component is passed to the cos(3) and sin(3)', &
'       ! functions the imaginary component can be any real value', &
'', &
'    end program demo_exp', &
'', &
'Results:', &
'', &
'     Euler''s constant is approximately   2.718282', &
'     given the complex value  (3.000000,4.000000)', &
'     exp(x) is (-13.12878,-15.20078)', &
'     is the same as (-13.12878,-15.20078)', &
'     maximum real real component   88.72284', &
'     maximum doubleprecision real component   709.782712893384', &
'', &
'STANDARD', &
'', &
'FORTRAN 77 and later', &
'', &
'SEE ALSO', &
'', &
'-   LOG(3)', &
'', &
'-   Wikipedia:Exponential function', &
'', &
'-   Wikipedia:Euler''s formula', &
'', &
'fortran-lang intrinsic descriptions (@urbanjost)', &
'']

shortname="exp"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('83','exponent')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'EXPONENT(3) - [MODEL_COMPONENTS] Exponent function (GFDL)', &
'', &
'SYNTAX', &
'', &
'    result = exponent(x)', &
'', &
'DESCRIPTION', &
'', &
'EXPONENT(x) returns the value of the exponent part of X. If X is zero', &
'the value returned is zero.', &
'', &
'ARGUMENTS', &
'', &
'    X', &
'        The type shall be _real_.', &
'', &
'RETURNS', &
'', &
'The return value is of type default _integer_.', &
'', &
'EXAMPLES', &
'', &
'Sample program:', &
'', &
'    program demo_exponent', &
'    implicit none', &
'    real :: x = 1.0', &
'    integer :: i', &
'       i = exponent(x)', &
'       print *, i', &
'       print *, exponent(0.0)', &
'    end program demo_exponent', &
'', &
'Results:', &
'', &
'                  1', &
'                  0', &
'', &
'STANDARD', &
'', &
'Fortran 95 and later', &
'', &
'SEE ALSO', &
'', &
'DIGITS(3), EPSILON(3), FRACTION(3), HUGE(3), MAXEXPONENT(3),', &
'MINEXPONENT(3), NEAREST(3), PRECISION(3), RADIX(3), RANGE(3),', &
'RRSPACING(3), SCALE(3), SET_EXPONENT(3), SPACING(3), TINY(3)', &
'', &
'fortran-lang intrinsic descriptions', &
'']

shortname="exponent"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('84','extends_type_of')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'EXTENDS_TYPE_OF(3) - [STATE] determine if the dynamic type of A is an', &
'extension of the dynamic type of MOLD. (GFDL)', &
'', &
'SYNTAX', &
'', &
'    result=extends_type_of(a, mold)', &
'', &
'DESCRIPTION', &
'', &
'EXTENDS_TYPE_OF(3) is .TRUE. if and only if the dynamic type of A is an', &
'extension of the dynamic type of MOLD.', &
'', &
'OPTIONS', &
'', &
'    A', &
'        shall be an object of extensible type. If it is a pointer, it', &
'        shall not have an undefined association status.', &
'', &
'    MOLD', &
'        shall be an object of extensible type. If it is a pointer, it', &
'        shall not have an undefined association status.', &
'', &
'RETURNS', &
'', &
'    RESULT', &
'        Default logical scalar.', &
'', &
'    VALUE', &
'        If MOLD is unlimited polymorphic and is either a disassociated', &
'        pointer or unallocated allocatable variable, the result is true;', &
'        otherwise if A is unlimited polymorphic and is either a', &
'        disassociated pointer or unallocated allocatable variable, the', &
'        result is false; otherwise the result is true if and only if the', &
'        dynamic type of A is an extension type of the dynamic type of', &
'        MOLD.', &
'', &
'    The dynamic type of a disassociated pointer or unallocated', &
'    allocatable variable is its declared type.', &
'', &
'EXAMPLES', &
'', &
'fortran-lang intrinsic descriptions', &
'']

shortname="extends_type_of"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('85','findloc')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'FINDLOC(3) - [ARRAY:LOCATION] Location of first element of ARRAY', &
'identified by MASK along dimension DIM having a value (GFDL)', &
'', &
'SYNTAX', &
'', &
'    findloc (array, value, dim, mask, kind, back)', &
'', &
'    or', &
'', &
'    findloc(array, value, mask, kind, back)', &
'', &
'DESCRIPTION', &
'', &
'Location of the first element of ARRAY identified by MASK along', &
'dimension DIM having a value equal to VALUE.', &
'', &
'If both ARRAY and VALUE are of type logical, the comparison is performed', &
'with the .EQV. operator; otherwise, the comparison is performed with the', &
'== operator. If the value of the comparison is true, that element of', &
'ARRAY matches VALUE.', &
'', &
'If only one element matches VALUE, that element''s subscripts are', &
'returned. Otherwise, if more than one element matches VALUE and BACK is', &
'absent or present with the value false, the element whose subscripts are', &
'returned is the first such element, taken in array element order. If', &
'BACK is present with the value true, the element whose subscripts are', &
'returned is the last such element, taken in array element order.', &
'', &
'OPTIONS', &
'', &
'    ARRAY', &
'        shall be an array of intrinsic type.', &
'', &
'    VALUE', &
'        shall be scalar and in type conformance with ARRAY, as specified', &
'        in Table 7.3 for relational intrinsic operations 7.1.5.5.2).', &
'', &
'    DIM', &
'        shall be an integer scalar with a value in the range 1 DIM n,', &
'        where n is the rank of ARRAY. The corresponding actual argument', &
'        shall not be an optional dummy argument.', &
'', &
'    MASK', &
'        (optional) shall be of type logical and shall be conformable', &
'        with ARRAY.', &
'', &
'    KIND', &
'        (optional) shall be a scalar integer initialization expression.', &
'', &
'    BACK', &
'        (optional) shall be a logical scalar.', &
'', &
'RETURNS', &
'', &
'Result Characteristics. Integer. If KIND is present, the kind type', &
'parameter is that specified by the value of KIND; otherwise the kind', &
'type parameter is that of default integer type. If DIM does not appear,', &
'the result is an array of rank one and of size equal to the rank of', &
'ARRAY; otherwise, the result is of rank n - 1 and shape', &
'', &
'       [d1 , d2 , . . . , dDIM-1 , dDIM+1 , . . . , dn ]', &
'', &
'where', &
'', &
'       [d1 , d2 , . . . , dn ]', &
'', &
'is the shape of ARRAY.', &
'', &
'RETURNS', &
'', &
'-   CASE (I): The result of FINDLOC (ARRAY, VALUE) is a rank-one array', &
'    whose element values are the values of the subscripts of an element', &
'    of ARRAY whose value matches VALUE. If there is such a value, the', &
'    ith subscript returned lies in the range 1 to ei , where ei is the', &
'    extent of the ith dimension of ARRAY. If no elements match VALUE or', &
'    ARRAY has size zero, all elements of the result are zero.', &
'', &
'-   CASE (II): the result of FINDLOC (ARRAY, VALUE, MASK = MASK) is a', &
'    rank-one array whose element values are the values of the subscripts', &
'    of an element of ARRAY, corresponding to a true element of MASK,', &
'    whose value matches VALUE. If there is such a value, the ith', &
'    subscript returned lies in the range 1 to ei , where ei is the', &
'    extent of the ith dimension of ARRAY. If no elements match VALUE,', &
'    ARRAY has size zero, or every element of MASK has the value false,', &
'    all elements of the result are zero.', &
'', &
'-   CASE (III): If ARRAY has rank one, the result of', &
'', &
'          findloc (array, value, dim=dim [, mask = mask])', &
'', &
'is a scalar whose value is equal to that of the first element of', &
'', &
'          findloc (array, value [, mask = mask])', &
'', &
'Otherwise, the value of element', &
'', &
'          (s1 , s2 , . . . , sDIM-1 , sDIM+1 , . . . , sn )', &
'', &
'of the result is equal to', &
'', &
'          findloc (array (s1, s2, ..., sdim-1, :, sdim+1, ..., sn ), &', &
'          value, dim=1 [, mask = mask (s1, s2, ..., sdim-1, :,', &
'                          sdim+1 , ... , sn )]).', &
'', &
'EXAMPLES', &
'', &
'-   CASE (I): The value of', &
'', &
'            findloc ([2, 6, 4, 6,], value = 6)', &
'', &
'is [2], and the value of', &
'', &
'            findloc ([2, 6, 4, 6], value = 6, back = .true.)', &
'', &
'is [4].', &
'', &
'-   CASE (II): If A has the value', &
'', &
'          0 -5  7 7', &
'          3  4 -1 2', &
'          1  5  6 7', &
'', &
'and M has the value', &
'', &
'           T T F T', &
'           T T F T', &
'           T T F T', &
'', &
'          findloc (a, 7, mask = m)', &
'', &
'has the value [1, 4] and', &
'', &
'          findloc (a, 7, mask = m, back = .true.)', &
'', &
'has the value [3, 4]. This is independent of the declared lower bounds', &
'for A .', &
'', &
'-   CASE (III): The value of', &
'', &
'          findloc ([2, 6, 4], value = 6, dim = 1)', &
'', &
'is 2. If B has the value', &
'', &
'           1 2 -9', &
'           2 2  6', &
'', &
'  findloc (b, VALUE = 2, dim = 1)', &
'', &
'has the value [2, 1, 0] and', &
'', &
'          findloc (b, value = 2, dim = 2)', &
'', &
'has the value [2, 1]. This is independent of the declared lower bounds', &
'for B.', &
'', &
'fortran-lang intrinsic descriptions', &
'']

shortname="findloc"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('86','floor')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'FLOOR(3) - [NUMERIC] Integer floor function (GFDL)', &
'', &
'SYNTAX', &
'', &
'    result = floor(a, kind)', &
'', &
'DESCRIPTION', &
'', &
'FLOOR(a) returns the greatest integer less than or equal to A. That is,', &
'it picks the whole number at or to the left of the value on the number', &
'line -HUGE(INT(A,KIND=KIND)) to HUGE(INT(A),KIND=KIND).', &
'', &
'ARGUMENTS', &
'', &
'    A', &
'        The type shall be _real_.', &
'', &
'    KIND', &
'        (Optional) An _integer_ initialization expression indicating the', &
'        kind parameter of the result.', &
'', &
'RETURNS', &
'', &
'The return value is of type _integer(kind)_ if KIND is present and of', &
'default-kind _integer_ otherwise.', &
'', &
'The result is undefined if it cannot be represented in the specified', &
'integer type.', &
'', &
'EXAMPLES', &
'', &
'Sample program:', &
'', &
'    program demo_floor', &
'    implicit none', &
'    real :: x = 63.29', &
'    real :: y = -63.59', &
'        print *, x, floor(x)', &
'        print *, y, floor(y)', &
'       ! elemental', &
'       print *,floor([ &', &
'       &  -2.7,  -2.5, -2.2, -2.0, -1.5, -1.0, -0.5, &', &
'       &  0.0,   &', &
'       &  +0.5,  +1.0, +1.5, +2.0, +2.2, +2.5, +2.7  ])', &
'    end program demo_floor', &
'', &
'Results:', &
'', &
'       63.2900009   63', &
'      -63.5900002  -64', &
'       -3     -3     -3     -2     -2     -1', &
'       -1      0      0      1      1      2', &
'        2      2      2', &
'', &
'STANDARD', &
'', &
'Fortran 95 and later', &
'', &
'SEE ALSO', &
'', &
'CEILING(3), NINT(3)', &
'', &
'AINT(3), ANINT(3), INT(3), SELECTED_INT_KIND(3)', &
'', &
'fortran-lang intrinsic descriptions (@urbanjost)', &
'']

shortname="floor"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('87','fraction')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'FRACTION(3) - [MODEL_COMPONENTS] Fractional part of the model', &
'representation (GFDL)', &
'', &
'SYNTAX', &
'', &
'    y = fraction(x)', &
'', &
'DESCRIPTION', &
'', &
'FRACTION(X) returns the fractional part of the model representation of', &
'X.', &
'', &
'ARGUMENTS', &
'', &
'    X', &
'        The type of the argument shall be a _real_.', &
'', &
'RETURNS', &
'', &
'The return value is of the same type and kind as the argument. The', &
'fractional part of the model representation of X is returned; it is X *', &
'RADIX(X)**(-EXPONENT(X)).', &
'', &
'EXAMPLES', &
'', &
'Sample program:', &
'', &
'    program demo_fraction', &
'    implicit none', &
'    real :: x', &
'       x = 178.1387e-4', &
'       print *, fraction(x), x * radix(x)**(-exponent(x))', &
'    end program demo_fraction', &
'', &
'Results:', &
'', &
'         0.570043862      0.570043862', &
'', &
'STANDARD', &
'', &
'Fortran 95 and later', &
'', &
'SEE ALSO', &
'', &
'DIGITS(3), EPSILON(3), EXPONENT(3), HUGE(3), MAXEXPONENT(3),', &
'MINEXPONENT(3), NEAREST(3), PRECISION(3), RADIX(3), RANGE(3),', &
'RRSPACING(3), SCALE(3), SET_EXPONENT(3), SPACING(3), TINY(3)', &
'', &
'fortran-lang intrinsic descriptions', &
'']

shortname="fraction"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('88','gamma')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'GAMMA(3) - [MATHEMATICS] Gamma function, which yields factorials for', &
'positive whole numbers', &
'', &
'SYNTAX', &
'', &
'    x = gamma(x)', &
'', &
'DESCRIPTION', &
'', &
'GAMMA(X) computes Gamma of X. For positive whole number values of N the', &
'Gamma function can be used to calculate factorials, as (N-1)! ==', &
'GAMMA(REAL(N)). That is', &
'', &
'    n! == gamma(real(n+1))', &
'', &
'$$ \GAMMA(x) = \int_0**\infty t**{x-1}{\mathrm{e}}**{-T}\,{\mathrm{d}}t', &
'$$', &
'', &
'ARGUMENTS', &
'', &
'    X', &
'        Shall be of type _real_ and neither zero nor a negative integer.', &
'', &
'RETURNS', &
'', &
'The return value is of type _real_ of the same kind as _x_.', &
'', &
'EXAMPLES', &
'', &
'Sample program:', &
'', &
'    program demo_gamma', &
'    use, intrinsic :: iso_fortran_env, only : wp=>real64', &
'    implicit none', &
'    real :: x, xa(4)', &
'    integer :: i', &
'', &
'       x = gamma(1.0)', &
'       write(*,*)''gamma(1.0)='',x', &
'', &
'       ! elemental', &
'       xa=gamma([1.0,2.0,3.0,4.0])', &
'       write(*,*)xa', &
'       write(*,*)', &
'', &
'       ! gamma(3) is related to the factorial function', &
'       do i=1,20', &
'          ! check value is not too big for default integer type', &
'          if(factorial(i).gt.huge(0))then', &
'             write(*,*)i,factorial(i)', &
'          else', &
'             write(*,*)i,factorial(i),int(factorial(i))', &
'          endif', &
'       enddo', &
'       ! more factorials', &
'       FAC: block', &
'       integer,parameter :: n(*)=[0,1,5,11,170]', &
'       integer :: j', &
'          do j=1,size(n)', &
'             write(*,''(*(g0,1x))'')''factorial of'', n(j),'' is '', &', &
'              & product([(real(i,kind=wp),i=1,n(j))]),  &', &
'              & gamma(real(n(j)+1,kind=wp))', &
'          enddo', &
'       endblock FAC', &
'', &
'    contains', &
'', &
'    function factorial(i) result(f)', &
'    integer,parameter :: dp=kind(0d0)', &
'    integer,intent(in) :: i', &
'    real :: f', &
'       if(i.le.0)then', &
'          write(*,''(*(g0))'')''<ERROR> gamma(3) function value '',i,'' <= 0''', &
'          stop      ''<STOP> bad value in gamma function''', &
'       endif', &
'       f=gamma(real(i+1))', &
'    end function factorial', &
'', &
'    end program demo_gamma', &
'', &
'Results:', &
'', &
'        gamma(1.0)=   1.000000', &
'          1.000000       1.000000       2.000000       6.000000', &
'', &
'                  1   1.000000               1', &
'                  2   2.000000               2', &
'                  3   6.000000               6', &
'                  4   24.00000              24', &
'                  5   120.0000             120', &
'                  6   720.0000             720', &
'                  7   5040.000            5040', &
'                  8   40320.00           40320', &
'                  9   362880.0          362880', &
'                 10   3628800.         3628800', &
'                 11  3.9916800E+07    39916800', &
'                 12  4.7900160E+08   479001600', &
'                 13  6.2270208E+09', &
'                 14  8.7178289E+10', &
'                 15  1.3076744E+12', &
'                 16  2.0922791E+13', &
'                 17  3.5568741E+14', &
'                 18  6.4023735E+15', &
'                 19  1.2164510E+17', &
'                 20  2.4329020E+18', &
'       factorial of 0  is  1.000000000000000 1.000000000000000', &
'       factorial of 1  is  1.000000000000000 1.000000000000000', &
'       factorial of 5  is  120.0000000000000 120.0000000000000', &
'       factorial of 11  is  39916800.00000000 39916800.00000000', &
'       factorial of 170  is  .7257415615307994E+307 .7257415615307999E+307', &
'', &
'STANDARD', &
'', &
'Fortran 2008 and later', &
'', &
'SEE ALSO', &
'', &
'Logarithm of the Gamma function: LOG_GAMMA(3)', &
'', &
'Wikipedia: Gamma_function', &
'', &
'fortran-lang intrinsic descriptions', &
'']

shortname="gamma"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('89','get_command')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'GET_COMMAND(3) - [SYSTEM:COMMAND LINE] Get the entire command line', &
'', &
'SYNTAX', &
'', &
'       call get_command(command, length, status)', &
'', &
'        subroutine get_command(command,length,status)', &
'        character(len=*),intent(out),optional :: command', &
'        integer,intent(out),optional :: length', &
'        integer,intent(out),optional :: status', &
'', &
'DESCRIPTION', &
'', &
'Retrieve the entire command line that was used to invoke the program.', &
'', &
'Note that what is typed on the command line is often processed by a', &
'shell. The shell typically processes special characters and white space', &
'before passing it to the program. The processing can typically be turned', &
'off by turning off globbing or quoting the command line arguments and/or', &
'changing the default field separators, but this should rarely be', &
'necessary.', &
'', &
'RETURNS', &
'', &
'    COMMAND', &
'        Shall be of type _character_ and of default kind. If COMMAND is', &
'        present, stores the entire command line that was used to invoke', &
'        the program in COMMAND.', &
'', &
'    LENGTH', &
'        Shall be of type _integer_ and of default kind. If LENGTH is', &
'        present, it is assigned the length of the command line.', &
'', &
'    STATUS', &
'        Shall be of type _integer_ and of default kind. If STATUS is', &
'        present, it is assigned 0 upon success of the command, -1 if', &
'        COMMAND is too short to store the command line, or a positive', &
'        value in case of an error.', &
'', &
'EXAMPLES', &
'', &
'Sample program:', &
'', &
'    program demo_get_command', &
'    implicit none', &
'    integer                      :: COMMAND_LINE_LENGTH', &
'    character(len=:),allocatable :: COMMAND_LINE', &
'       ! get command line length', &
'       call get_command(length=COMMAND_LINE_LENGTH)', &
'       ! allocate string big enough to hold command line', &
'       allocate(character(len=COMMAND_LINE_LENGTH) :: COMMAND_LINE)', &
'       ! get command line as a string', &
'       call get_command(command=COMMAND_LINE)', &
'       ! trim leading spaces just in case', &
'       COMMAND_LINE=adjustl(COMMAND_LINE)', &
'       write(*,''("OUTPUT:",a)'')COMMAND_LINE', &
'    end program demo_get_command', &
'', &
'Results:', &
'', &
'         # note that shell expansion removes some of the whitespace', &
'         # without quotes', &
'         ./test_get_command  arguments    on command   line to   echo', &
'', &
'         OUTPUT:./test_get_command arguments on command line to echo', &
'', &
'         # using the bash shell with single quotes', &
'         ./test_get_command  ''arguments  *><`~[]!{}?"\''| ''', &
'', &
'         OUTPUT:./test_get_command arguments  *><`~[]!{}?"''|', &
'', &
'STANDARD', &
'', &
'Fortran 2003 and later', &
'', &
'SEE ALSO', &
'', &
'GET_COMMAND_ARGUMENT(3), COMMAND_ARGUMENT_COUNT(3)', &
'', &
'fortran-lang intrinsic descriptions (@urbanjost)', &
'']

shortname="get_command"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('90','get_command_argument')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'GET_COMMAND_ARGUMENT(3) - [SYSTEM:COMMAND LINE] Get command line', &
'arguments', &
'', &
'SYNTAX', &
'', &
'         call get_command_argument(number, value, length, status)', &
'', &
'         subroutine get_command_argument(number,value,length.status)', &
'         integer,intent(in)                    :: number', &
'         character(len=*),intent(out),optional :: value', &
'         integer,intent(out),optional          :: length', &
'         integer,intent(out),optional          :: status', &
'', &
'DESCRIPTION', &
'', &
'Retrieve the NUMBER-th argument that was passed on the command line when', &
'the containing program was invoked.', &
'', &
'There is not anything specifically stated about what an argument is but', &
'in practice the arguments are split on whitespace unless the arguments', &
'are quoted and IFS values (Internal Field Separators) used by common', &
'shells are ignored.', &
'', &
'OPTIONS', &
'', &
'    NUMBER', &
'        Shall be a scalar of type INTEGER, NUMBER >= 0. If NUMBER = 0,', &
'        VALUE is set to the name of the program (on systems that support', &
'        this feature).', &
'', &
'RETURNS', &
'', &
'-   VALUE :Shall be a scalar of type _character_ and of default kind.', &
'    After get_command_argument returns, the VALUE argument holds the', &
'    NUMBER-th command line argument. If VALUE can not hold the argument,', &
'    it is truncated to fit the length of VALUE. If there are less than', &
'    NUMBER arguments specified at the command line, VALUE will be filled', &
'    with blanks.', &
'', &
'-   LENGTH :(Optional) Shall be a scalar of type _integer_. The LENGTH', &
'    argument contains the length of the NUMBER-th command line argument.', &
'', &
'-   STATUS :(Optional) Shall be a scalar of type _integer_. If the', &
'    argument retrieval fails, STATUS is a positive number; if VALUE', &
'    contains a truncated command line argument, STATUS is -1; and', &
'    otherwise the STATUS is zero.', &
'', &
'EXAMPLES', &
'', &
'Sample program:', &
'', &
'    program demo_get_command_argument', &
'    implicit none', &
'    character(len=255)           :: progname', &
'    integer                      :: stat', &
'    integer                      :: count,i, longest, argument_length', &
'    integer,allocatable          :: istat(:), ilen(:)', &
'    character(len=:),allocatable :: args(:)', &
'      !', &
'      ! get number of arguments', &
'      count = command_argument_count()', &
'      write(*,*)''The number of arguments is '',count', &
'      !', &
'      ! simple usage', &
'      !', &
'      call get_command_argument (0, progname, status=stat)', &
'      if (stat == 0) then', &
'         print *, "The program''s name is " // trim (progname)', &
'      endif', &
'      !', &
'      ! showing how to make an array to hold any argument list', &
'      !', &
'      ! find longest argument', &
'      !', &
'      longest=0', &
'      do i=0,count', &
'         call get_command_argument(number=i,length=argument_length)', &
'         longest=max(longest,argument_length)', &
'      enddo', &
'      !', &
'      ! allocate string array big enough to hold command line', &
'      ! argument strings and related information', &
'      !', &
'      allocate(character(len=longest) :: args(0:count))', &
'      allocate(istat(0:count))', &
'      allocate(ilen(0:count))', &
'      !', &
'      ! read the arguments into the array', &
'      !', &
'      do i=0,count', &
'        call get_command_argument(i, args(i),status=istat(i),length=ilen(i))', &
'      enddo', &
'      !', &
'      ! show the results', &
'      !', &
'      write (*,''(i3.3,1x,i0.5,1x,i0.5,1x,"[",a,"]")'') &', &
'      & (i,istat(i),ilen(i),args(i)(:ilen(i)),i=0,count)', &
'    end program demo_get_command_argument', &
'', &
'Results:', &
'', &
'    /demo_get_command_argument a    test  ''of getting   arguments  '' "  leading"', &
'', &
'     The number of arguments is            5', &
'     The program''s name is xxx', &
'    000 00000 00003 [./test_get_command_argument]', &
'    001 00000 00001 [a]', &
'    003 00000 00004 [test]', &
'    004 00000 00024 [of getting   arguments  ]', &
'    005 00000 00018 [  leading]', &
'', &
'STANDARD', &
'', &
'Fortran 2003 and later', &
'', &
'SEE ALSO', &
'', &
'GET_COMMAND(3), COMMAND_ARGUMENT_COUNT(3)', &
'', &
'fortran-lang intrinsic descriptions (@urbanjost)', &
'']

shortname="get_command_argument"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('91','get_environment_variable')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'GET_ENVIRONMENT_VARIABLE(3) - [SYSTEM:ENVIRONMENT] Get an environmental', &
'variable', &
'', &
'SYNTAX', &
'', &
'      call get_environment_variable(name, value, length, status, trim_name)', &
'', &
'       character(len=*),intent(in) :: name', &
'       character(len=*),intent(out),optional :: value', &
'       integer,intent(out),optional :: length', &
'       integer,intent(out),optional :: status', &
'       logical,intent(out),optional :: trim_name', &
'', &
'DESCRIPTION', &
'', &
'Get the VALUE of the environmental variable NAME.', &
'', &
'Note that GET_ENVIRONMENT_VARIABLE(3) need not be thread-safe. It is the', &
'responsibility of the user to ensure that the environment is not being', &
'updated concurrently.', &
'', &
'OPTIONS', &
'', &
'    NAME', &
'        The name of the environment variable to query.', &
'', &
'    Shall be a scalar of type _character_ and of default kind.', &
'', &
'RETURNS', &
'', &
'    VALUE', &
'        The value of the environment variable being queried.', &
'', &
'    Shall be a scalar of type _character_ and of default kind. The value', &
'    of NAME is stored in VALUE. If VALUE is not large enough to hold the', &
'    data, it is truncated. If NAME is not set, VALUE will be filled with', &
'    blanks.', &
'', &
'    LENGTH', &
'        Argument LENGTH contains the length needed for storing the', &
'        environment variable NAME or zero if it is not present.', &
'', &
'    Shall be a scalar of type _integer_ and of default kind.', &
'', &
'    STATUS', &
'        STATUS is -1 if VALUE is present but too short for the', &
'        environment variable; it is 1 if the environment variable does', &
'        not exist and 2 if the processor does not support environment', &
'        variables; in all other cases STATUS is zero.', &
'', &
'    Shall be a scalar of type _integer_ and of default kind.', &
'', &
'    TRIM_NAME', &
'        If TRIM_NAME is present with the value .FALSE., the trailing', &
'        blanks in NAME are significant; otherwise they are not part of', &
'        the environment variable name.', &
'', &
'    Shall be a scalar of type _logical_ and of default kind.', &
'', &
'EXAMPLES', &
'', &
'Sample program:', &
'', &
'    program demo_getenv', &
'    implicit none', &
'    character(len=:),allocatable :: homedir', &
'    character(len=:),allocatable :: var', &
'         var=''HOME''', &
'         homedir=get_env(var)', &
'         write (*,''(a,"=""",a,"""")'')var,homedir', &
'', &
'    contains', &
'', &
'    function get_env(NAME,DEFAULT) result(VALUE)', &
'    ! a function that makes calling get_environment_variable(3) simple', &
'    implicit none', &
'    character(len=*),intent(in)          :: NAME', &
'    character(len=*),intent(in),optional :: DEFAULT', &
'    character(len=:),allocatable         :: VALUE', &
'    integer                              :: howbig', &
'    integer                              :: stat', &
'    integer                              :: length', &
'       ! get length required to hold value', &
'       length=0', &
'       VALUE=''''', &
'       if(NAME.ne.'''')then', &
'          call get_environment_variable( &', &
'          & NAME, length=howbig,status=stat,trim_name=.true.)', &
'          select case (stat)', &
'          case (1)', &
'           !*!print *, NAME, " is not defined in the environment. Strange..."', &
'           VALUE=''''', &
'          case (2)', &
'           !*!print *, &', &
'           !*!"This processor does not support environment variables. Boooh!"', &
'           VALUE=''''', &
'          case default', &
'           ! make string to hold value of sufficient size', &
'           if(allocated(VALUE))deallocate(VALUE)', &
'           allocate(character(len=max(howbig,1)) :: VALUE)', &
'           ! get value', &
'           call get_environment_variable( &', &
'           & NAME,VALUE,status=stat,trim_name=.true.)', &
'           if(stat.ne.0)VALUE=''''', &
'          end select', &
'       endif', &
'       if(VALUE.eq.''''.and.present(DEFAULT))VALUE=DEFAULT', &
'    end function get_env', &
'', &
'    end program demo_getenv', &
'', &
'Typical Results:', &
'', &
'       HOME="/home/urbanjs"', &
'', &
'STANDARD', &
'', &
'Fortran 2003 and later', &
'', &
'fortran-lang intrinsic descriptions (@urbanjost)', &
'']

shortname="get_environment_variable"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('92','gnu_free_documentation_license')

textblock=[character(len=256) :: &
'', &
'0. PREAMBLE', &
'', &
'The purpose of this License is to make a manual, textbook, or other', &
'functional and useful document "free" in the sense of freedom: to assure', &
'everyone the effective freedom to copy and redistribute it, with or', &
'without modifying it, either commercially or noncommercially.', &
'Secondarily, this License preserves for the author and publisher a way', &
'to get credit for their work, while not being considered responsible for', &
'modifications made by others.', &
'', &
'This License is a kind of "copyleft", which means that derivative works', &
'of the document must themselves be free in the same sense. It', &
'complements the GNU General Public License, which is a copyleft license', &
'designed for free software.', &
'', &
'We have designed this License in order to use it for manuals for free', &
'software, because free software needs free documentation: a free program', &
'should come with manuals providing the same freedoms that the software', &
'does. But this License is not limited to software manuals; it can be', &
'used for any textual work, regardless of subject matter or whether it is', &
'published as a printed book. We recommend this License principally for', &
'works whose purpose is instruction or reference.', &
'', &
'1. APPLICABILITY AND DEFINITIONS', &
'', &
'This License applies to any manual or other work, in any medium, that', &
'contains a notice placed by the copyright holder saying it can be', &
'distributed under the terms of this License. Such a notice grants a', &
'world-wide, royalty-free license, unlimited in duration, to use that', &
'work under the conditions stated herein. The "Document", below, refers', &
'to any such manual or work. Any member of the public is a licensee, and', &
'is addressed as "you". You accept the license if you copy, modify or', &
'distribute the work in a way requiring permission under copyright law.', &
'', &
'A "Modified Version" of the Document means any work containing the', &
'Document or a portion of it, either copied verbatim, or with', &
'modifications and/or translated into another language.', &
'', &
'A "Secondary Section" is a named appendix or a front-matter section of', &
'the Document that deals exclusively with the relationship of the', &
'publishers or authors of the Document to the Document''s overall subject', &
'(or to related matters) and contains nothing that could fall directly', &
'within that overall subject. (Thus, if the Document is in part a', &
'textbook of mathematics, a Secondary Section may not explain any', &
'mathematics.) The relationship could be a matter of historical', &
'connection with the subject or with related matters, or of legal,', &
'commercial, philosophical, ethical or political position regarding them.', &
'', &
'The "Invariant Sections" are certain Secondary Sections whose titles are', &
'designated, as being those of Invariant Sections, in the notice that', &
'says that the Document is released under this License. If a section does', &
'not fit the above definition of Secondary then it is not allowed to be', &
'designated as Invariant. The Document may contain zero Invariant', &
'Sections. If the Document does not identify any Invariant Sections then', &
'there are none.', &
'', &
'The "Cover Texts" are certain short passages of text that are listed, as', &
'Front-Cover Texts or Back-Cover Texts, in the notice that says that the', &
'Document is released under this License. A Front-Cover Text may be at', &
'most 5 words, and a Back-Cover Text may be at most 25 words.', &
'', &
'A "Transparent" copy of the Document means a machine-readable copy,', &
'represented in a format whose specification is available to the general', &
'public, that is suitable for revising the document straightforwardly', &
'with generic text editors or (for images composed of pixels) generic', &
'paint programs or (for drawings) some widely available drawing editor,', &
'and that is suitable for input to text formatters or for automatic', &
'translation to a variety of formats suitable for input to text', &
'formatters. A copy made in an otherwise Transparent file format whose', &
'markup, or absence of markup, has been arranged to thwart or discourage', &
'subsequent modification by readers is not Transparent. An image format', &
'is not Transparent if used for any substantial amount of text. A copy', &
'that is not "Transparent" is called "Opaque".', &
'', &
'Examples of suitable formats for Transparent copies include plain ASCII', &
'without markup, Texinfo input format, LaTeX input format, SGML or XML', &
'using a publicly available DTD, and standard-conforming simple HTML,', &
'PostScript or PDF designed for human modification. Examples of', &
'transparent image formats include PNG, XCF and JPG. Opaque formats', &
'include proprietary formats that can be read and edited only by', &
'proprietary word processors, SGML or XML for which the DTD and/or', &
'processing tools are not generally available, and the machine-generated', &
'HTML, PostScript or PDF produced by some word processors for output', &
'purposes only.', &
'', &
'The "Title Page" means, for a printed book, the title page itself, plus', &
'such following pages as are needed to hold, legibly, the material this', &
'License requires to appear in the title page. For works in formats which', &
'do not have any title page as such, "Title Page" means the text near the', &
'most prominent appearance of the work''s title, preceding the beginning', &
'of the body of the text.', &
'', &
'A section "Entitled XYZ" means a named subunit of the Document whose', &
'title either is precisely XYZ or contains XYZ in parentheses following', &
'text that translates XYZ in another language. (Here XYZ stands for a', &
'specific section name mentioned below, such as "Acknowledgements",', &
'"Dedications", "Endorsements", or "History".) To "Preserve the Title" of', &
'such a section when you modify the Document means that it remains a', &
'section "Entitled XYZ" according to this definition.', &
'', &
'The Document may include Warranty Disclaimers next to the notice which', &
'states that this License applies to the Document. These Warranty', &
'Disclaimers are considered to be included by reference in this License,', &
'but only as regards disclaiming warranties: any other implication that', &
'these Warranty Disclaimers may have is void and has no effect on the', &
'meaning of this License.', &
'', &
'2. VERBATIM COPYING', &
'', &
'You may copy and distribute the Document in any medium, either', &
'commercially or noncommercially, provided that this License, the', &
'copyright notices, and the license notice saying this License applies to', &
'the Document are reproduced in all copies, and that you add no other', &
'conditions whatsoever to those of this License. You may not use', &
'technical measures to obstruct or control the reading or further copying', &
'of the copies you make or distribute. However, you may accept', &
'compensation in exchange for copies. If you distribute a large enough', &
'number of copies you must also follow the conditions in section 3.', &
'', &
'You may also lend copies, under the same conditions stated above, and', &
'you may publicly display copies.', &
'', &
'3. COPYING IN QUANTITY', &
'', &
'If you publish printed copies (or copies in media that commonly have', &
'printed covers) of the Document, numbering more than 100, and the', &
'Document''s license notice requires Cover Texts, you must enclose the', &
'copies in covers that carry, clearly and legibly, all these Cover Texts:', &
'Front-Cover Texts on the front cover, and Back-Cover Texts on the back', &
'cover. Both covers must also clearly and legibly identify you as the', &
'publisher of these copies. The front cover must present the full title', &
'with all words of the title equally prominent and visible. You may add', &
'other material on the covers in addition. Copying with changes limited', &
'to the covers, as long as they preserve the title of the Document and', &
'satisfy these conditions, can be treated as verbatim copying in other', &
'respects.', &
'', &
'If the required texts for either cover are too voluminous to fit', &
'legibly, you should put the first ones listed (as many as fit', &
'reasonably) on the actual cover, and continue the rest onto adjacent', &
'pages.', &
'', &
'If you publish or distribute Opaque copies of the Document numbering', &
'more than 100, you must either include a machine-readable Transparent', &
'copy along with each Opaque copy, or state in or with each Opaque copy a', &
'computer-network location from which the general network-using public', &
'has access to download using public-standard network protocols a', &
'complete Transparent copy of the Document, free of added material. If', &
'you use the latter option, you must take reasonably prudent steps, when', &
'you begin distribution of Opaque copies in quantity, to ensure that this', &
'Transparent copy will remain thus accessible at the stated location', &
'until at least one year after the last time you distribute an Opaque', &
'copy (directly or through your agents or retailers) of that edition to', &
'the public.', &
'', &
'It is requested, but not required, that you contact the authors of the', &
'Document well before redistributing any large number of copies, to give', &
'them a chance to provide you with an updated version of the Document.', &
'', &
'4. MODIFICATIONS', &
'', &
'You may copy and distribute a Modified Version of the Document under the', &
'conditions of sections 2 and 3 above, provided that you release the', &
'Modified Version under precisely this License, with the Modified Version', &
'filling the role of the Document, thus licensing distribution and', &
'modification of the Modified Version to whoever possesses a copy of it.', &
'In addition, you must do these things in the Modified Version:', &
'', &
'-   A. Use in the Title Page (and on the covers, if any) a title', &
'    distinct from that of the Document, and from those of previous', &
'    versions (which should, if there were any, be listed in the History', &
'    section of the Document). You may use the same title as a previous', &
'    version if the original publisher of that version gives permission.', &
'-   B. List on the Title Page, as authors, one or more persons or', &
'    entities responsible for authorship of the modifications in the', &
'    Modified Version, together with at least five of the principal', &
'    authors of the Document (all of its principal authors, if it has', &
'    fewer than five), unless they release you from this requirement.', &
'-   C. State on the Title page the name of the publisher of the Modified', &
'    Version, as the publisher.', &
'-   D. Preserve all the copyright notices of the Document.', &
'-   E. Add an appropriate copyright notice for your modifications', &
'    adjacent to the other copyright notices.', &
'-   F. Include, immediately after the copyright notices, a license', &
'    notice giving the public permission to use the Modified Version', &
'    under the terms of this License, in the form shown in the Addendum', &
'    below.', &
'-   G. Preserve in that license notice the full lists of Invariant', &
'    Sections and required Cover Texts given in the Document''s license', &
'    notice.', &
'-   H. Include an unaltered copy of this License.', &
'-   I. Preserve the section Entitled "History", Preserve its Title, and', &
'    add to it an item stating at least the title, year, new authors, and', &
'    publisher of the Modified Version as given on the Title Page. If', &
'    there is no section Entitled "History" in the Document, create one', &
'    stating the title, year, authors, and publisher of the Document as', &
'    given on its Title Page, then add an item describing the Modified', &
'    Version as stated in the previous sentence.', &
'-   J. Preserve the network location, if any, given in the Document for', &
'    public access to a Transparent copy of the Document, and likewise', &
'    the network locations given in the Document for previous versions it', &
'    was based on. These may be placed in the "History" section. You may', &
'    omit a network location for a work that was published at least four', &
'    years before the Document itself, or if the original publisher of', &
'    the version it refers to gives permission.', &
'-   K. For any section Entitled "Acknowledgements" or "Dedications",', &
'    Preserve the Title of the section, and preserve in the section all', &
'    the substance and tone of each of the contributor acknowledgements', &
'    and/or dedications given therein.', &
'-   L. Preserve all the Invariant Sections of the Document, unaltered in', &
'    their text and in their titles. Section numbers or the equivalent', &
'    are not considered part of the section titles.', &
'-   M. Delete any section Entitled "Endorsements". Such a section may', &
'    not be included in the Modified Version.', &
'-   N. Do not retitle any existing section to be Entitled "Endorsements"', &
'    or to conflict in title with any Invariant Section.', &
'-   O. Preserve any Warranty Disclaimers.', &
'', &
'If the Modified Version includes new front-matter sections or appendices', &
'that qualify as Secondary Sections and contain no material copied from', &
'the Document, you may at your option designate some or all of these', &
'sections as invariant. To do this, add their titles to the list of', &
'Invariant Sections in the Modified Version''s license notice. These', &
'titles must be distinct from any other section titles.', &
'', &
'You may add a section Entitled "Endorsements", provided it contains', &
'nothing but endorsements of your Modified Version by various', &
'parties--for example, statements of peer review or that the text has', &
'been approved by an organization as the authoritative definition of a', &
'standard.', &
'', &
'You may add a passage of up to five words as a Front-Cover Text, and a', &
'passage of up to 25 words as a Back-Cover Text, to the end of the list', &
'of Cover Texts in the Modified Version. Only one passage of Front-Cover', &
'Text and one of Back-Cover Text may be added by (or through arrangements', &
'made by) any one entity. If the Document already includes a cover text', &
'for the same cover, previously added by you or by arrangement made by', &
'the same entity you are acting on behalf of, you may not add another;', &
'but you may replace the old one, on explicit permission from the', &
'previous publisher that added the old one.', &
'', &
'The author(s) and publisher(s) of the Document do not by this License', &
'give permission to use their names for publicity for or to assert or', &
'imply endorsement of any Modified Version.', &
'', &
'5. COMBINING DOCUMENTS', &
'', &
'You may combine the Document with other documents released under this', &
'License, under the terms defined in section 4 above for modified', &
'versions, provided that you include in the combination all of the', &
'Invariant Sections of all of the original documents, unmodified, and', &
'list them all as Invariant Sections of your combined work in its license', &
'notice, and that you preserve all their Warranty Disclaimers.', &
'', &
'The combined work need only contain one copy of this License, and', &
'multiple identical Invariant Sections may be replaced with a single', &
'copy. If there are multiple Invariant Sections with the same name but', &
'different contents, make the title of each such section unique by adding', &
'at the end of it, in parentheses, the name of the original author or', &
'publisher of that section if known, or else a unique number. Make the', &
'same adjustment to the section titles in the list of Invariant Sections', &
'in the license notice of the combined work.', &
'', &
'In the combination, you must combine any sections Entitled "History" in', &
'the various original documents, forming one section Entitled "History";', &
'likewise combine any sections Entitled "Acknowledgements", and any', &
'sections Entitled "Dedications". You must delete all sections Entitled', &
'"Endorsements".', &
'', &
'6. COLLECTIONS OF DOCUMENTS', &
'', &
'You may make a collection consisting of the Document and other documents', &
'released under this License, and replace the individual copies of this', &
'License in the various documents with a single copy that is included in', &
'the collection, provided that you follow the rules of this License for', &
'verbatim copying of each of the documents in all other respects.', &
'', &
'You may extract a single document from such a collection, and distribute', &
'it individually under this License, provided you insert a copy of this', &
'License into the extracted document, and follow this License in all', &
'other respects regarding verbatim copying of that document.', &
'', &
'7. AGGREGATION WITH INDEPENDENT WORKS', &
'', &
'A compilation of the Document or its derivatives with other separate and', &
'independent documents or works, in or on a volume of a storage or', &
'distribution medium, is called an "aggregate" if the copyright resulting', &
'from the compilation is not used to limit the legal rights of the', &
'compilation''s users beyond what the individual works permit. When the', &
'Document is included in an aggregate, this License does not apply to the', &
'other works in the aggregate which are not themselves derivative works', &
'of the Document.', &
'', &
'If the Cover Text requirement of section 3 is applicable to these copies', &
'of the Document, then if the Document is less than one half of the', &
'entire aggregate, the Document''s Cover Texts may be placed on covers', &
'that bracket the Document within the aggregate, or the electronic', &
'equivalent of covers if the Document is in electronic form. Otherwise', &
'they must appear on printed covers that bracket the whole aggregate.', &
'', &
'8. TRANSLATION', &
'', &
'Translation is considered a kind of modification, so you may distribute', &
'translations of the Document under the terms of section 4. Replacing', &
'Invariant Sections with translations requires special permission from', &
'their copyright holders, but you may include translations of some or all', &
'Invariant Sections in addition to the original versions of these', &
'Invariant Sections. You may include a translation of this License, and', &
'all the license notices in the Document, and any Warranty Disclaimers,', &
'provided that you also include the original English version of this', &
'License and the original versions of those notices and disclaimers. In', &
'case of a disagreement between the translation and the original version', &
'of this License or a notice or disclaimer, the original version will', &
'prevail.', &
'', &
'If a section in the Document is Entitled "Acknowledgements",', &
'"Dedications", or "History", the requirement (section 4) to Preserve its', &
'Title (section 1) will typically require changing the actual title.', &
'', &
'9. TERMINATION', &
'', &
'You may not copy, modify, sublicense, or distribute the Document except', &
'as expressly provided for under this License. Any other attempt to copy,', &
'modify, sublicense or distribute the Document is void, and will', &
'automatically terminate your rights under this License. However, parties', &
'who have received copies, or rights, from you under this License will', &
'not have their licenses terminated so long as such parties remain in', &
'full compliance.', &
'', &
'10. FUTURE REVISIONS OF THIS LICENSE', &
'', &
'The Free Software Foundation may publish new, revised versions of the', &
'GNU Free Documentation License from time to time. Such new versions will', &
'be similar in spirit to the present version, but may differ in detail to', &
'address new problems or concerns. See http://www.gnu.org/copyleft/.', &
'', &
'Each version of the License is given a distinguishing version number. If', &
'the Document specifies that a particular numbered version of this', &
'License "or any later version" applies to it, you have the option of', &
'following the terms and conditions either of that specified version or', &
'of any later version that has been published (not as a draft) by the', &
'Free Software Foundation. If the Document does not specify a version', &
'number of this License, you may choose any version ever published (not', &
'as a draft) by the Free Software Foundation.', &
'', &
'ADDENDUM: How to use this License for your documents', &
'', &
'To use this License in a document you have written, include a copy of', &
'the License in the document and put the following copyright and license', &
'notices just after the title page:', &
'', &
'    Copyright (c)  YEAR  YOUR NAME.', &
'    Permission is granted to copy, distribute and/or modify this document', &
'    under the terms of the GNU Free Documentation License, Version 1.2', &
'    or any later version published by the Free Software Foundation;', &
'    with no Invariant Sections, no Front-Cover Texts, and no Back-Cover Texts.', &
'    A copy of the license is included in the section entitled "GNU', &
'    Free Documentation License".', &
'', &
'If you have Invariant Sections, Front-Cover Texts and Back-Cover Texts,', &
'replace the "with...Texts." line with this:', &
'', &
'    with the Invariant Sections being LIST THEIR TITLES, with the', &
'    Front-Cover Texts being LIST, and with the Back-Cover Texts being LIST.', &
'', &
'If you have Invariant Sections without Cover Texts, or some other', &
'combination of the three, merge those two alternatives to suit the', &
'situation.', &
'', &
'If your document contains nontrivial examples of program code, we', &
'recommend releasing these examples in parallel under your choice of free', &
'software license, such as the GNU General Public License, to permit', &
'their use in free software.', &
'', &
'fortran-lang intrinsic descriptions', &
'']

shortname="gnu_free_documentation_license"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('93','huge')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'HUGE(3) - [NUMERIC MODEL] Largest number of a type and kind', &
'', &
'SYNTAX', &
'', &
'    result = huge(x)', &
'', &
'       TYPE(kind=KIND) function huge(x) result(answer)', &
'       TYPE(kind=KIND) :: x', &
'       TYPE(kind=KIND) :: answer', &
'', &
'where TYPE may be _real_ or _integer_ and KIND is any supported', &
'associated _kind_.', &
'', &
'DESCRIPTION', &
'', &
'HUGE(x) returns the largest number that is not an infinity for the kind', &
'and type of X.', &
'', &
'ARGUMENTS', &
'', &
'    X', &
'        Shall be an arbitrary value of type _real_ or _integer_. The', &
'        value is used merely to determine what _kind_ and _type_ of', &
'        scalar is being queried.', &
'', &
'RETURNS', &
'', &
'The return value is of the same type and kind as _x_ and is the largest', &
'value supported by the specified model.', &
'', &
'EXAMPLES', &
'', &
'Sample program:', &
'', &
'    program demo_huge', &
'    ! or, "why I have my own NINT function"', &
'    implicit none', &
'    character(len=*),parameter :: f=''(i2,1x,2(i11,1x),f14.0:,1x,l1,1x,a)''', &
'    integer :: i,j,k,biggest', &
'    real :: v, w', &
'       ! basic', &
'       print *, huge(0), huge(0.0), huge(0.0d0)', &
'       print *, tiny(0.0), tiny(0.0d0)', &
'', &
'       ! advanced', &
'       biggest=huge(0)', &
'       ! be careful when using integers in computation', &
'       do i=1,14', &
'          j=6**i   ! Danger, Danger', &
'          w=6**i   ! Danger, Danger', &
'          v=6.0**i', &
'          k=v      ! Danger, Danger', &
'          if(v.gt.biggest)then', &
'             write(*,f) i, j, k, v, v.eq.w, ''wrong j and k and w''', &
'          else', &
'             write(*,f) i, j, k, v, v.eq.w', &
'          endif', &
'       enddo', &
'    end program demo_huge', &
'', &
'Results:', &
'', &
'      2147483647  3.4028235E+38  1.797693134862316E+308', &
'      1.1754944E-38  2.225073858507201E-308', &
'', &
'        1      6           6             6. T', &
'', &
'        2      36          36            36. T', &
'', &
'        3      216         216           216. T', &
'', &
'        4      1296        1296          1296. T', &
'', &
'        5      7776        7776          7776. T', &
'', &
'        6      46656       46656         46656. T', &
'', &
'        7      279936      279936        279936. T', &
'', &
'        8      1679616     1679616       1679616. T', &
'', &
'        9      10077696    10077696      10077696. T', &
'', &
'        10     60466176    60466176      60466176. T', &
'', &
'        11     362797056   362797056     362797056. T', &
'', &
'        12 -2118184960 -2147483648', &
'               2176782336. F wrong for j and k and w', &
'', &
'        13     175792128 -2147483648   13060694016. F wrong for j and k', &
'               and w', &
'', &
'        14     1054752768 -2147483648   78364164096. F wrong for j and k and w', &
'', &
'STANDARD', &
'', &
'Fortran 95 and later', &
'', &
'SEE ALSO', &
'', &
'DIGITS(3), EPSILON(3), EXPONENT(3), FRACTION(3), MAXEXPONENT(3),', &
'MINEXPONENT(3), NEAREST(3), PRECISION(3), RADIX(3), RANGE(3),', &
'RRSPACING(3), SCALE(3), SET_EXPONENT(3), SPACING(3), TINY(3)', &
'', &
'fortran-lang intrinsic descriptions (@urbanjost)', &
'']

shortname="huge"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('94','hypot')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'HYPOT(3) - [MATHEMATICS] returns the distance between the point and the', &
'origin.', &
'', &
'SYNTAX', &
'', &
'    result = hypot(x, y)', &
'', &
'       real(kind=KIND) elemental function hypot(x,y) result(value)', &
'       real(kind=KIND),intent(in) :: x, y', &
'', &
'where X,Y,VALUE shall all be of the same KIND.', &
'', &
'DESCRIPTION', &
'', &
'HYPOT(X,Y) is referred to as the Euclidean distance function. It is', &
'equal to SQRT(X2 + Y2), without undue underflow or overflow.', &
'', &
'In mathematics, the _Euclidean distance_ between two points in Euclidean', &
'space is the length of a line segment between two points.', &
'', &
'HYPOT(X,Y) returns the distance between the point <X,Y> and the origin.', &
'', &
'ARGUMENTS', &
'', &
'    X', &
'        The type shall be _real_.', &
'', &
'    Y', &
'        The type and kind type parameter shall be the same as X.', &
'', &
'RETURNS', &
'', &
'The return value has the same type and kind type parameter as X.', &
'', &
'The result is the positive magnitude of the distance of the point <X,Y>', &
'from the origin <0.0,0.0> .', &
'', &
'EXAMPLES', &
'', &
'Sample program:', &
'', &
'    program demo_hypot', &
'    use, intrinsic :: iso_fortran_env, only : &', &
'     & real_kinds, real32, real64, real128', &
'    implicit none', &
'    real(kind=real32) :: x, y', &
'    real(kind=real32),allocatable :: xs(:), ys(:)', &
'    integer :: i', &
'    character(len=*),parameter :: f=''(a,/,SP,*(3x,g0,1x,g0:,/))''', &
'', &
'       x = 1.e0_real32', &
'       y = 0.5e0_real32', &
'', &
'       write(*,*)', &
'       write(*,''(*(g0))'')''point <'',x,'','',y,''> is '',hypot(x,y)', &
'       write(*,''(*(g0))'')''units away from the origin''', &
'       write(*,*)', &
'', &
'       ! elemental', &
'       xs=[  x,  x**2,  x*10.0,  x*15.0, -x**2  ]', &
'       ys=[  y,  y**2, -y*20.0,  y**2,   -y**2  ]', &
'', &
'       write(*,f)"the points",(xs(i),ys(i),i=1,size(xs))', &
'       write(*,f)"have distances from the origin of ",hypot(xs,ys)', &
'       write(*,f)"the closest is",minval(hypot(xs,ys))', &
'', &
'    end program demo_hypot', &
'', &
'Results:', &
'', &
'       point <1.00000000,0.500000000> is 1.11803401', &
'       units away from the origin', &
'', &
'       the points', &
'          +1.00000000 +0.500000000', &
'          +1.00000000 +0.250000000', &
'          +10.0000000 -10.0000000', &
'          +15.0000000 +0.250000000', &
'          -1.00000000 -0.250000000', &
'       have distances from the origin of', &
'          +1.11803401 +1.03077638', &
'          +14.1421356 +15.0020828', &
'          +1.03077638', &
'       the closest is', &
'          +1.03077638', &
'', &
'STANDARD', &
'', &
'Fortran 2008 and later', &
'', &
'fortran-lang intrinsic descriptions (@urbanjost)', &
'']

shortname="hypot"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('95','iachar')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'IACHAR(3) - [CHARACTER:CONVERSION] Code in ASCII collating sequence', &
'(GFDL)', &
'', &
'SYNTAX', &
'', &
'    result = iachar(c, kind)', &
'', &
'DESCRIPTION', &
'', &
'IACHAR(c) returns the code for the ASCII character in the first', &
'character position of C.', &
'', &
'ARGUMENTS', &
'', &
'    C', &
'        Shall be a scalar _character_, with _intent(in)_', &
'', &
'    KIND', &
'        (Optional) An _integer_ initialization expression indicating the', &
'        kind parameter of the result.', &
'', &
'RETURNS', &
'', &
'The return value is of type _integer_ and of kind KIND. If KIND is', &
'absent, the return value is of default integer kind.', &
'', &
'EXAMPLES', &
'', &
'Sample program:', &
'', &
'    program demo_iachar', &
'    implicit none', &
'    ! create function to convert uppercase letters to lowercase', &
'       write(*,''(a)'')lower(''abcdefg ABCDEFG'')', &
'    contains', &
'    !', &
'    elemental pure function lower(str) result (string)', &
'    ! Changes a string to lowercase', &
'    character(*), intent(In)     :: str', &
'    character(len(str))          :: string', &
'    integer                      :: i', &
'       string = str', &
'       ! step thru each letter in the string in specified range', &
'       do i = 1, len(str)', &
'          select case (str(i:i))', &
'          case (''A'':''Z'') ! change letter to miniscule', &
'             string(i:i) = char(iachar(str(i:i))+32)', &
'          case default', &
'          end select', &
'       end do', &
'    end function lower', &
'    !', &
'    end program demo_iachar', &
'', &
'Results:', &
'', &
'       abcdefg abcdefg', &
'', &
'NOTE', &
'', &
'See ICHAR(3) for a discussion of converting between numerical values and', &
'formatted string representations.', &
'', &
'STANDARD', &
'', &
'Fortran 95 and later, with KIND argument - Fortran 2003 and later', &
'', &
'SEE ALSO', &
'', &
'ACHAR(3), CHAR(3), ICHAR(3)', &
'', &
'Functions that perform operations on character strings, return lengths', &
'of arguments, and search for certain arguments:', &
'', &
'-   ELEMENTAL: ADJUSTL(3), ADJUSTR(3), INDEX(3), SCAN(3), VERIFY(3)', &
'', &
'-   NONELEMENTAL: LEN_TRIM(3), LEN(3), REPEAT(3), TRIM(3)', &
'', &
'fortran-lang intrinsic descriptions', &
'']

shortname="iachar"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('96','iall')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'IALL(3) - [BIT:LOGICAL] Bitwise and of array elements (GFDL)', &
'', &
'SYNTAX', &
'', &
'      result = iall(array, mask)', &
'', &
'        or', &
'', &
'      result = iall(array, dim, mask)', &
'', &
'DESCRIPTION', &
'', &
'Reduces with bitwise _and_ the elements of ARRAY along dimension DIM if', &
'the corresponding element in MASK is .TRUE..', &
'', &
'ARGUMENTS', &
'', &
'    ARRAY', &
'        Shall be an array of type _integer_', &
'', &
'    DIM', &
'        (Optional) shall be a scalar of type _integer_ with a value in', &
'        the range from 1 TO N, where N equals the rank of ARRAY.', &
'', &
'    MASK', &
'        (Optional) shall be of type _logical_ and either be a scalar or', &
'        an array of the same shape as ARRAY.', &
'', &
'RETURNS', &
'', &
'The result is of the same type as ARRAY.', &
'', &
'If DIM is absent, a scalar with the bitwise _all_ of all elements in', &
'ARRAY is returned. Otherwise, an array of rank N-1, where N equals the', &
'rank of ARRAY, and a shape similar to that of ARRAY with dimension DIM', &
'dropped is returned.', &
'', &
'EXAMPLES', &
'', &
'Sample program:', &
'', &
'    program demo_iall', &
'    use, intrinsic :: iso_fortran_env, only : integer_kinds, &', &
'     & int8, int16, int32, int64', &
'    implicit none', &
'    integer(kind=int8) :: a(2)', &
'', &
'       a(1) = int(b''00100100'')', &
'       a(2) = int(b''01101010'')', &
'', &
'       print ''(b8.8)'', iall(a)', &
'', &
'    end program demo_iall', &
'', &
'Results:', &
'', &
'       00100000', &
'', &
'STANDARD', &
'', &
'Fortran 2008 and later', &
'', &
'SEE ALSO', &
'', &
'IANY(3), IPARITY(3), IAND(3)', &
'', &
'fortran-lang intrinsic descriptions', &
'']

shortname="iall"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('97','iand')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'IAND(3) - [BIT:LOGICAL] Bitwise logical and (GFDL)', &
'', &
'SYNTAX', &
'', &
'    result = iand(i, j)', &
'', &
'DESCRIPTION', &
'', &
'Bitwise logical AND.', &
'', &
'ARGUMENTS', &
'', &
'    I', &
'        The type shall be _integer_.', &
'', &
'    J', &
'        The type shall be _integer_, of the same kind as I.', &
'', &
'RETURNS', &
'', &
'The return type is _integer_, of the same kind as the arguments. (If the', &
'argument kinds differ, it is of the same kind as the larger argument.)', &
'', &
'EXAMPLES', &
'', &
'Sample program:', &
'', &
'    program demo_iand', &
'    implicit none', &
'    integer :: a, b', &
'          data a / z''f'' /, b / z''3'' /', &
'          write (*,*) iand(a, b)', &
'    end program demo_iand', &
'', &
'Results:', &
'', &
'                  3', &
'', &
'STANDARD', &
'', &
'Fortran 95 and later', &
'', &
'SEE ALSO', &
'', &
'IEOR(3), IBCLR(3), NOT(3), BTEST(3), IBCLR(3), IBITS(3), IBSET(3),', &
'IOR(3), IEOR(3), MVBITS(3)', &
'', &
'fortran-lang intrinsic descriptions', &
'']

shortname="iand"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('98','iany')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'IANY(3) - [BIT:LOGICAL] Bitwise or of array elements (GFDL)', &
'', &
'SYNTAX', &
'', &
'      result = iany(array, mask)', &
'', &
'        or', &
'', &
'      result = iany(array, dim, mask)', &
'', &
'DESCRIPTION', &
'', &
'Reduces with bitwise or (inclusive or) the elements of ARRAY along', &
'dimension DIM if the corresponding element in MASK is .TRUE..', &
'', &
'ARGUMENTS', &
'', &
'    ARRAY', &
'        Shall be an array of type _integer_', &
'', &
'    DIM', &
'        (Optional) shall be a scalar of type _integer_ with a value in', &
'        the range from 1 TO N, where N equals the rank of ARRAY.', &
'', &
'    MASK', &
'        (Optional) shall be of type _logical_ and either be a scalar or', &
'        an array of the same shape as ARRAY.', &
'', &
'RETURNS', &
'', &
'The result is of the same type as ARRAY.', &
'', &
'If DIM is absent, a scalar with the bitwise _or_ of all elements in', &
'ARRAY is returned. Otherwise, an array of rank N-1, where N equals the', &
'rank of ARRAY, and a shape similar to that of ARRAY with dimension DIM', &
'dropped is returned.', &
'', &
'EXAMPLES', &
'', &
'Sample program:', &
'', &
'    program demo_iany', &
'    use, intrinsic :: iso_fortran_env, only : integer_kinds, &', &
'     & int8, int16, int32, int64', &
'    implicit none', &
'    integer(kind=int8) :: a(2)', &
'         a(1) = int(b''00100100'')', &
'         a(2) = int(b''01101010'')', &
'         print ''(b8.8)'', iany(a)', &
'    end program demo_iany', &
'', &
'Results:', &
'', &
'       01101110', &
'', &
'STANDARD', &
'', &
'Fortran 2008 and later', &
'', &
'SEE ALSO', &
'', &
'IPARITY(3), IALL(3), IOR(3)', &
'', &
'fortran-lang intrinsic descriptions', &
'']

shortname="iany"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('99','ibclr')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'IBCLR(3) - [BIT:SET] Clear bit (GFDL)', &
'', &
'SYNTAX', &
'', &
'    result = ibclr(i, pos)', &
'', &
'DESCRIPTION', &
'', &
'IBCLR returns the value of I with the bit at position POS set to zero.', &
'', &
'ARGUMENTS', &
'', &
'    I', &
'        The type shall be _integer_.', &
'', &
'    POS', &
'        The type shall be _integer_. A value of zero refers to the least', &
'        significant bit. POS is an INTENT(IN) scalar or array of type', &
'        _integer_. The value of POS must be within the range zero to', &
'        (BIT_SIZE(I)-1).', &
'', &
'RETURNS', &
'', &
'The return value is of type _integer_ and of the same kind as I.', &
'', &
'STANDARD', &
'', &
'Fortran 95 and later', &
'', &
'SEE ALSO', &
'', &
'IEOR(3), NOT(3), BTEST(3), IBCLR(3), IBITS(3), IBSET(3), IAND(3),', &
'IOR(3), IEOR(3), MVBITS(3)', &
'', &
'fortran-lang intrinsic descriptions', &
'']

shortname="ibclr"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('100','ibits')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'IBITS(3) - [BIT:COPY] Bit extraction (GFDL)', &
'', &
'SYNTAX', &
'', &
'    result = ibits(i, pos, len)', &
'', &
'DESCRIPTION', &
'', &
'IBITS extracts a field of length LEN from I, starting from bit position', &
'POS and extending left for LEN bits. The result is right-justified and', &
'the remaining bits are zeroed. The value of pos+len must be less than or', &
'equal to the value BIT_SIZE(I).', &
'', &
'ARGUMENTS', &
'', &
'    I', &
'        The type shall be _integer_.', &
'', &
'    POS', &
'        The type shall be _integer_. A value of zero refers to the least', &
'        significant bit.', &
'', &
'    LEN', &
'        The type shall be _integer_.', &
'', &
'RETURNS', &
'', &
'The return value is of type _integer_ and of the same kind as I.', &
'', &
'STANDARD', &
'', &
'Fortran 95 and later', &
'', &
'SEE ALSO', &
'', &
'IEOR(3), IBCLR(3), NOT(3), BTEST(3), IBCLR(3), IBSET(3), IAND(3),', &
'IOR(3), IEOR(3), MVBITS(3)', &
'', &
'fortran-lang intrinsic descriptions', &
'']

shortname="ibits"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('101','ibset')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'IBSET(3) - [BIT:SET] Set bit (GFDL)', &
'', &
'SYNTAX', &
'', &
'    result = ibset(i, pos)', &
'', &
'DESCRIPTION', &
'', &
'IBSET returns the value of I with the bit at position POS set to one.', &
'', &
'ARGUMENTS', &
'', &
'    I', &
'        The type shall be _integer_.', &
'', &
'    POS', &
'        The type shall be _integer_. A value of zero refers to the least', &
'        significant bit. pos is an INTENT(IN) scalar or array of type', &
'        _integer_. The value of pos must be within the range zero to', &
'        (BIT_SIZE(I)-1).', &
'', &
'RETURNS', &
'', &
'The return value is of type _integer_ and of the same kind as I.', &
'', &
'STANDARD', &
'', &
'Fortran 95 and later', &
'', &
'SEE ALSO', &
'', &
'IEOR(3), IBCLR(3), NOT(3), BTEST(3), IBCLR(3), IBITS(3), IAND(3),', &
'IOR(3), IEOR(3), MVBITS(3)', &
'', &
'fortran-lang intrinsic descriptions', &
'']

shortname="ibset"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('102','ichar')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'ICHAR(3) - [CHARACTER:CONVERSION] Character-to-integer conversion', &
'function (GFDL)', &
'', &
'SYNTAX', &
'', &
'       elemental function ichar(c,kind)', &
'', &
'        character(len=1),intent(in) :: c', &
'        integer,intent(in),optional :: kind', &
'', &
'DESCRIPTION', &
'', &
'ICHAR(C) returns the code for the character in the system''s native', &
'character set. The correspondence between characters and their codes is', &
'not necessarily the same across different Fortran implementations. For', &
'example, a platform using EBCDIC would return different values than an', &
'ASCII platform.', &
'', &
'See IACHAR(3) for specifically working with the ASCII character set.', &
'', &
'ARGUMENTS', &
'', &
'    C', &
'        Shall be a scalar _character_, with INTENT(IN)', &
'', &
'    KIND', &
'        (Optional) An _integer_ initialization expression indicating the', &
'        kind parameter of the result.', &
'', &
'RETURNS', &
'', &
'The return value is of type _integer_ and of kind KIND. If KIND is', &
'absent, the return value is of default _integer_ kind.', &
'', &
'EXAMPLES', &
'', &
'Sample program:', &
'', &
'    program demo_ichar', &
'    implicit none', &
'    integer i', &
'', &
'       write(*,*)ichar([''a'',''z'',''A'',''Z''])', &
'       do i=0,127', &
'          call printme()', &
'       enddo', &
'', &
'    contains', &
'', &
'    subroutine printme()', &
'    character(len=1) :: letter', &
'', &
'       letter=char(i)', &
'       select case(i)', &
'       case (:31,127:)', &
'          write(*,''(1x,i0.3,1x,"HEX=",z2.2,1x,i0)'')i,letter,ichar(letter)', &
'       case default', &
'          write(*,''(1x,i0.3,1x,a,1x,i0)'')i,letter,ichar(letter)', &
'       end select', &
'', &
'    end subroutine printme', &
'', &
'    end program demo_ichar', &
'', &
'NOTE', &
'', &
'No intrinsic exists to convert between a numeric value and a formatted', &
'character string representation -- for instance, given the _character_', &
'value ''154'', obtaining an _integer_ or _real_ value with the value 154,', &
'or vice versa. Instead, this functionality is provided by internal-file', &
'I/O, as in the following example:', &
'', &
'    program read_val', &
'    integer value', &
'    character(len=10) string, string2', &
'       string = ''154''', &
'', &
'       ! Convert a string to a numeric value', &
'       read (string,''(I10)'') value', &
'       print *, value', &
'', &
'       ! Convert a value to a formatted string', &
'       write (string2,''(I10)'') value', &
'       print *, string2', &
'    end program read_val', &
'', &
'Results:', &
'', &
'                154', &
'               154', &
'', &
'STANDARD', &
'', &
'Fortran 95 and later, with KIND argument -Fortran 2003 and later', &
'', &
'SEE ALSO', &
'', &
'ACHAR(3), CHAR(3), IACHAR(3)', &
'', &
'Functions that perform operations on character strings, return lengths', &
'of arguments, and search for certain arguments:', &
'', &
'-   ELEMENTAL: ADJUSTL(3), ADJUSTR(3), INDEX(3),', &
'', &
'SCAN(3), VERIFY(3)', &
'', &
'-   NONELEMENTAL: LEN_TRIM(3), LEN(3), REPEAT(3), TRIM(3)', &
'', &
'fortran-lang intrinsic descriptions', &
'']

shortname="ichar"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('103','ieor')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'IEOR(3) - [BIT:LOGICAL] Bitwise logical exclusive or (GFDL)', &
'', &
'SYNTAX', &
'', &
'    result = ieor(i, j)', &
'', &
'DESCRIPTION', &
'', &
'IEOR returns the bitwise Boolean exclusive-OR of I and J.', &
'', &
'ARGUMENTS', &
'', &
'    I', &
'        The type shall be _integer_.', &
'', &
'    J', &
'        The type shall be _integer_, of the same kind as I.', &
'', &
'RETURNS', &
'', &
'The return type is _integer_, of the same kind as the arguments. (If the', &
'argument kinds differ, it is of the same kind as the larger argument.)', &
'', &
'STANDARD', &
'', &
'Fortran 95 and later', &
'', &
'SEE ALSO', &
'', &
'IEOR(3), IBCLR(3), NOT(3), BTEST(3), IBCLR(3), IBITS(3), IBSET(3),', &
'IAND(3), IOR(3), MVBITS(3)', &
'', &
'fortran-lang intrinsic descriptions', &
'']

shortname="ieor"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('104','image_index')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'IMAGE_INDEX(3) - [COLLECTIVE] Cosubscript to image index conversion', &
'(GFDL)', &
'', &
'SYNTAX', &
'', &
'    result = image_index(coarray, sub)', &
'', &
'DESCRIPTION', &
'', &
'Returns the image index belonging to a cosubscript.', &
'', &
'ARGUMENTS', &
'', &
'    COARRAY', &
'        Coarray of any type.', &
'', &
'    SUB', &
'        default integer rank-1 array of a size equal to the corank of', &
'        COARRAY.', &
'', &
'RETURNS', &
'', &
'Scalar default integer with the value of the image index which', &
'corresponds to the cosubscripts. For invalid cosubscripts the result is', &
'zero.', &
'', &
'EXAMPLES', &
'', &
'Sample program:', &
'', &
'    program demo image_index', &
'    implicit none', &
'    integer :: array[2,-1:4,8,*]', &
'       ! Writes  28 (or 0 if there are fewer than 28 images)', &
'       write (*,*) image_index(array, [2,0,3,1])', &
'    end demo image_index', &
'', &
'STANDARD', &
'', &
'Fortran 2008 and later', &
'', &
'SEE ALSO', &
'', &
'THIS_IMAGE(3), NUM_IMAGES(3)', &
'', &
'fortran-lang intrinsic descriptions', &
'']

shortname="image_index"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('105','index')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'INDEX(3) - [CHARACTER:SEARCH] Position of a substring within a string', &
'(GFDL)', &
'', &
'SYNTAX', &
'', &
'       index(string, substring, back, kind) result(start)', &
'', &
'         character(len=*),intent(in) :: string', &
'         character(len=*),intent(in) :: substring', &
'         logical,intent(in),optional :: back', &
'         integer,intent(in),optional :: kind', &
'         integer(kind=KIND)          :: start', &
'', &
'DESCRIPTION', &
'', &
'Returns the position of the start of the leftmost or rightmost', &
'occurrence of string SUBSTRING in STRING, counting from one. If', &
'SUBSTRING is not present in STRING, zero is returned.', &
'', &
'ARGUMENTS', &
'', &
'    STRING', &
'        string to be searched', &
'', &
'    SUBSTRING', &
'        string to attempt to locate in STRING', &
'', &
'    BACK', &
'        If the BACK argument is present and true, the return value is', &
'        the start of the rightmost occurrence rather than the leftmost.', &
'', &
'    KIND', &
'        An _integer_ initialization expression indicating the kind', &
'        parameter of the result.', &
'', &
'RETURNS', &
'', &
'    START', &
'        The return value is of type _integer_ and of kind KIND. If KIND', &
'        is absent, the return value is of default integer kind.', &
'', &
'EXAMPLES', &
'', &
'Example program', &
'', &
'    program demo_index', &
'    implicit none', &
'    character(len=*),parameter :: str=&', &
'       ''Search this string for this expression''', &
'       !1234567890123456789012345678901234567890', &
'       write(*,*)&', &
'          index(str,''this'').eq.8,              &', &
'          ! return value is counted from the left end even if BACK=.TRUE.', &
'          index(str,''this'',back=.true.).eq.24, &', &
'          ! INDEX is case-sensitive', &
'          index(str,''This'').eq.0', &
'    end program demo_index', &
'', &
'Expected Results:', &
'', &
'       T T T', &
'', &
'STANDARD', &
'', &
'FORTRAN 77 and later, with KIND argument Fortran 2003 and later', &
'', &
'SEE ALSO', &
'', &
'Functions that perform operations on character strings, return lengths', &
'of arguments, and search for certain arguments:', &
'', &
'-   ELEMENTAL: ADJUSTL(3), ADJUSTR(3), INDEX(3), SCAN(3), VERIFY(3)', &
'', &
'-   NONELEMENTAL: LEN_TRIM(3), LEN(3), REPEAT(3), TRIM(3)', &
'', &
'fortran-lang intrinsic descriptions', &
'']

shortname="index"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('106','int')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'INT(3) - [TYPE:NUMERIC] Convert to integer type by truncating towards', &
'zero', &
'', &
'SYNTAX', &
'', &
'    result = int(a, kind)', &
'', &
'     integer(kind=KIND) elemental function int(a,kind)', &
'     TYPE(kind=KIND),intent(in),optional :: a', &
'     integer,optional :: kind', &
'', &
'DESCRIPTION', &
'', &
'Convert to integer type by truncating towards zero.', &
'', &
'ARGUMENTS', &
'', &
'    A', &
'        Shall be of type _integer_, _real_, or _complex_ or a', &
'        BOZ-literal-constant.', &
'', &
'    KIND', &
'        An _integer_ initialization expression indicating the kind', &
'        parameter of the result.', &
'', &
'    If not present the returned type is that of default integer type.', &
'', &
'RETURNS', &
'', &
'returns an _integer_ variable or array applying the following rules:', &
'', &
'CASE:', &
'', &
'1.  If A is of type _integer_, INT(a) = a', &
'', &
'2.  If A is of type _real_ and |A| < 1, INT(A) equals 0. If |A| >= 1,', &
'    then INT(A) equals the integer whose magnitude does not exceed A and', &
'    whose sign is the same as the sign of A.', &
'', &
'3.  If A is of type _complex_, rule 2 is applied to the _real_ part of', &
'    A.', &
'', &
'4.  If _a_ is a boz-literal constant, it is treated as an _integer_ with', &
'    the _kind_ specified.', &
'', &
'    The interpretation of a bit sequence whose most significant bit is 1', &
'    is processor dependent.', &
'', &
'The result is undefined if it cannot be represented in the specified', &
'integer type.', &
'', &
'EXAMPLES', &
'', &
'Sample program:', &
'', &
'    program demo_int', &
'    use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64', &
'    implicit none', &
'    integer :: i = 42', &
'    complex :: z = (-3.7, 1.0)', &
'    real :: x=-10.5, y=10.5', &
'', &
'       print *, int(x), int(y)', &
'', &
'       print *, int(i)', &
'', &
'       print *, int(z), int(z,8)', &
'       ! elemental', &
'       print *, int([-10.9,-10.5,-10.3,10.3,10.5,10.9])', &
'       ! note int(3) truncates towards zero', &
'', &
'       ! CAUTION:', &
'       ! a number bigger than a default integer can represent', &
'       ! produces an incorrect result and is not required to', &
'       ! be detected by the program.', &
'       x=real(huge(0))+1000.0', &
'       print *, int(x),x', &
'       ! using a larger kind', &
'       print *, int(x,kind=int64),x', &
'', &
'       print *, int(&', &
'       & B"111111111111111111111111111111111111111111111111111111111111111",&', &
'       & kind=int64)', &
'       print *, int(O"777777777777777777777",kind=int64)', &
'       print *, int(Z"7FFFFFFFFFFFFFFF",kind=int64)', &
'', &
'       ! elemental', &
'       print *', &
'       print *,int([ &', &
'       &  -2.7,  -2.5, -2.2, -2.0, -1.5, -1.0, -0.5, &', &
'       &  0.0,   &', &
'       &  +0.5,  +1.0, +1.5, +2.0, +2.2, +2.5, +2.7  ])', &
'', &
'    end program demo_int', &
'', &
'Results:', &
'', &
'                -10   10', &
'                 42', &
'                 -3  -3', &
'                -10  -10  -10   10   10  10', &
'        -2147483648   2.14748467E+09', &
'         2147484672   2.14748467E+09', &
'         9223372036854775807', &
'         9223372036854775807', &
'         9223372036854775807', &
'', &
'        -2          -2          -2          -2          -1', &
'        -1           0           0           0           1', &
'         1           2           2           2           2', &
'', &
'STANDARD', &
'', &
'FORTRAN 77 and later', &
'', &
'SEE ALSO', &
'', &
'AINT(3), ANINT(3), NINT(3), SELECTED_INT_KIND(3), CEILING(3), FLOOR(3)', &
'', &
'fortran-lang intrinsic descriptions (@urbanjost)', &
'']

shortname="int"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('107','ior')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'IOR(3) - [BIT:LOGICAL] Bitwise logical inclusive or (GFDL)', &
'', &
'SYNTAX', &
'', &
'       result = ior(i, j)', &
'        integer,intent(in) :: i', &
'        integer,intent(in) :: j', &
'', &
'DESCRIPTION', &
'', &
'IOR returns the bit-wise Boolean inclusive-OR of I and J.', &
'', &
'ARGUMENTS', &
'', &
'    I', &
'        an _integer_ scalar or array.', &
'', &
'    J', &
'        _integer_ scalar or array, of the same kind as I.', &
'', &
'RETURNS', &
'', &
'The return type is _integer_, of the same kind as the arguments. (If the', &
'argument kinds differ, it is of the same kind as the larger argument.)', &
'', &
'EXAMPLES', &
'', &
'Sample program:', &
'', &
'    program demo_ior', &
'    implicit none', &
'    integer :: i, j, k', &
'       i=53       ! i=00110101 binary (lowest order byte)', &
'       j=45       ! j=00101101 binary (lowest order byte)', &
'       k=ior(i,j) ! k=00111101 binary (lowest order byte) , k=61 decimal', &
'       write(*,''(i8,1x,b8.8)'')i,i,j,j,k,k', &
'    end program demo_ior', &
'', &
'Results:', &
'', &
'             53 00110101', &
'             45 00101101', &
'             61 00111101', &
'', &
'STANDARD', &
'', &
'Fortran 95 and later', &
'', &
'SEE ALSO', &
'', &
'IEOR(3), IBCLR(3), NOT(3), BTEST(3), IBCLR(3), IBITS(3), IBSET(3),', &
'IAND(3), IEOR(3), MVBITS(3)', &
'', &
'fortran-lang intrinsic descriptions', &
'']

shortname="ior"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('108','iparity')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'IPARITY(3) - [BIT:LOGICAL] Bitwise exclusive or of array elements (GFDL)', &
'', &
'SYNTAX', &
'', &
'      result = iparity(array, mask)', &
'', &
'       or', &
'', &
'      result = iparity(array, dim, mask)', &
'', &
'DESCRIPTION', &
'', &
'Reduces with bitwise _xor_ (exclusive _or_) the elements of ARRAY along', &
'dimension DIM if the corresponding element in MASK is .TRUE..', &
'', &
'ARGUMENTS', &
'', &
'    ARRAY', &
'        Shall be an array of type _integer_', &
'', &
'    DIM', &
'        (Optional) shall be a scalar of type _integer_ with a value in', &
'        the range from "1" TO "N", where "N" equals the rank of ARRAY.', &
'', &
'    MASK', &
'        (Optional) shall be of type _logical_ and either be a scalar or', &
'        an array of the same shape as ARRAY.', &
'', &
'RETURNS', &
'', &
'The result is of the same type as ARRAY.', &
'', &
'If DIM is absent, a scalar with the bitwise _xor_ of all elements in', &
'ARRAY is returned. Otherwise, an array of rank N-1, where N equals the', &
'rank of ARRAY, and a shape similar to that of ARRAY with dimension DIM', &
'dropped is returned.', &
'', &
'EXAMPLES', &
'', &
'Sample program:', &
'', &
'    program demo_iparity', &
'    implicit none', &
'    integer, dimension(2) :: a', &
'      a(1) = int(b''00100100'')', &
'      a(2) = int(b''01101010'')', &
'      print ''(b8.8)'', iparity(a)', &
'    end program demo_iparity', &
'', &
'Results:', &
'', &
'       01001110', &
'', &
'STANDARD', &
'', &
'Fortran 2008 and later', &
'', &
'SEE ALSO', &
'', &
'IANY(3), IALL(3), IEOR(3), PARITY(3)', &
'', &
'fortran-lang intrinsic descriptions', &
'']

shortname="iparity"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('109','is_contiguous')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'IS_CONTIGUOUS(3) - [ARRAY INQUIRY] test if object is contiguous (GFDL)', &
'', &
'SYNTAX', &
'', &
'    result = is_contiguous(a)', &
'', &
'DESCRIPTION', &
'', &
'True if and only if an object is contiguous.', &
'', &
'An object is contiguous if it is', &
'', &
'-   (1) an object with the CONTIGUOUS attribute,', &
'', &
'-   (2) a nonpointer whole array that is not assumed-shape,', &
'', &
'-   (3) an assumed-shape array that is argument associated with an array', &
'    that is contiguous,', &
'', &
'-   (4) an array allocated by an ALLOCATE statement,', &
'', &
'-   (5) a pointer associated with a contiguous target, or', &
'', &
'-   (6) a nonzero-sized array section provided that', &
'', &
'    -   (A) its base object is contiguous,', &
'', &
'    -   (B) it does not have a vector subscript,', &
'', &
'    -   (C) the elements of the section, in array element order, are a', &
'        subset of the base object elements that are consecutive in array', &
'        element order,', &
'', &
'    -   (D) if the array is of type character and a substring-range', &
'        appears, the substring-range specifies all of the characters of', &
'        the parent-string,', &
'', &
'    -   (E) only its final part-ref has nonzero rank, and', &
'', &
'    -   (F) it is not the real or imaginary part of an array of type', &
'        complex.', &
'', &
'An object is not contiguous if it is an array subobject, and', &
'', &
'-   the object has two or more elements,', &
'', &
'-   the elements of the object in array element order are not', &
'    consecutive in the elements of the base object,', &
'', &
'-   the object is not of type character with length zero, and', &
'', &
'-   the object is not of a derived type that has no ultimate components', &
'    other than zero-sized arrays and', &
'', &
'-   characters with length zero.', &
'', &
'It is processor-dependent whether any other object is contiguous.', &
'', &
'ARGUMENTS', &
'', &
'    A', &
'        may be of any type. It shall be an array. If it is a pointer it', &
'        shall be associated.', &
'', &
'RETURNS', &
'', &
'    RESULT', &
'        of type Default logical scalar. The result has the value true if', &
'        A is contiguous, and false otherwise.', &
'', &
'EXAMPLES', &
'', &
'Sample program:', &
'', &
'    program demo_is_contiguous', &
'    implicit none', &
'    intrinsic is_contiguous', &
'    real, DIMENSION (1000, 1000), TARGET :: A', &
'    real, DIMENSION (:, :), POINTER       :: IN, OUT', &
'       IN => A              ! Associate IN with target A', &
'       OUT => A(1:1000:2,:) ! Associate OUT with subset of target A', &
'       !', &
'       write(*,*)''IN is '',IS_CONTIGUOUS(IN)', &
'       write(*,*)''OUT is '',IS_CONTIGUOUS(OUT)', &
'       !', &
'    end program demo_is_contiguous', &
'', &
'Results:', &
'', &
'        IN is  T', &
'        OUT is  F', &
'', &
'STANDARD', &
'', &
'Fortran 2008 and later', &
'', &
'fortran-lang intrinsic descriptions', &
'']

shortname="is_contiguous"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('110','ishft')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'ISHFT(3) - [BIT:SHIFT] Shift bits (GFDL)', &
'', &
'SYNTAX', &
'', &
'    result = ishft(i, shift)', &
'', &
'DESCRIPTION', &
'', &
'ISHFT(3) returns a value corresponding to I with all of the bits shifted', &
'SHIFT places. A value of SHIFT greater than zero corresponds to a left', &
'shift, a value of zero corresponds to no shift, and a value less than', &
'zero corresponds to a right shift. If the absolute value of SHIFT is', &
'greater than BIT_SIZE(I), the value is undefined. Bits shifted out from', &
'the left end or right end are lost; zeros are shifted in from the', &
'opposite end.', &
'', &
'ARGUMENTS', &
'', &
'    I', &
'        The type shall be _integer_.', &
'', &
'    SHIFT', &
'        The type shall be _integer_.', &
'', &
'RETURNS', &
'', &
'The return value is of type _integer_ and of the same kind as I.', &
'', &
'STANDARD', &
'', &
'Fortran 95 and later', &
'', &
'SEE ALSO', &
'', &
'ISHFTC(3)', &
'', &
'fortran-lang intrinsic descriptions', &
'']

shortname="ishft"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('111','ishftc')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'ISHFTC(3) - [BIT:SHIFT] Shift bits circularly (GFDL)', &
'', &
'SYNTAX', &
'', &
'    result = ishftc(i, shift, size)', &
'', &
'DESCRIPTION', &
'', &
'ISHFTC(3) returns a value corresponding to I with the rightmost SIZE', &
'bits shifted circularly SHIFT places; that is, bits shifted out one end', &
'are shifted into the opposite end. A value of SHIFT greater than zero', &
'corresponds to a left shift, a value of zero corresponds to no shift,', &
'and a value less than zero corresponds to a right shift. The absolute', &
'value of SHIFT must be less than SIZE. If the SIZE argument is omitted,', &
'it is taken to be equivalent to BIT_SIZE(I).', &
'', &
'ARGUMENTS', &
'', &
'    I', &
'        The type shall be _integer_.', &
'', &
'    SHIFT', &
'        The type shall be _integer_.', &
'', &
'    SIZE', &
'        (Optional) The type shall be _integer_; the value must be', &
'        greater than zero and less than or equal to BIT_SIZE(i).', &
'', &
'RETURNS', &
'', &
'The return value is of type _integer_ and of the same kind as I.', &
'', &
'STANDARD', &
'', &
'Fortran 95 and later', &
'', &
'SEE ALSO', &
'', &
'ISHFT(3)', &
'', &
'fortran-lang intrinsic descriptions', &
'']

shortname="ishftc"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('112','is_iostat_end')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'IS_IOSTAT_END(3) - [STATE] Test for end-of-file value (GFDL)', &
'', &
'SYNTAX', &
'', &
'    function is_iostat_end(i)', &
'', &
'        logical function   :: is_iostat_end (i) result(yesno)', &
'        integer,intent(in) :: i', &
'', &
'DESCRIPTION', &
'', &
'is_iostat_end(3) tests whether a variable (assumed returned as a status', &
'from an I/O statement) has the "end of file" I/O status value.', &
'', &
'The function is equivalent to comparing the variable with the IOSTAT_END', &
'parameter of the intrinsic module ISO_FORTRAN_ENV.', &
'', &
'ARGUMENTS', &
'', &
'    I', &
'        An _integer_ status value to test if indicating end of file.', &
'', &
'RETURNS', &
'', &
'Returns a _logical_ of the default kind, .TRUE. if I has the value which', &
'indicates an end of file condition for IOSTAT= specifiers, and is', &
'.FALSE. otherwise.', &
'', &
'EXAMPLES', &
'', &
'Sample program:', &
'', &
'    program demo_iostat', &
'    implicit none', &
'    real               :: value', &
'    integer            :: ios', &
'    character(len=256) :: message', &
'       write(*,*)''Begin entering numeric values, one per line''', &
'       do', &
'          read(*,*,iostat=ios,iomsg=message)value', &
'          if(ios.eq.0)then', &
'             write(*,*)''VALUE='',value', &
'          elseif( is_iostat_end(ios) ) then', &
'             stop ''end of file. Goodbye!''', &
'          else', &
'             write(*,*)''ERROR:'',ios,trim(message)', &
'          endif', &
'          !', &
'       enddo', &
'    end program demo_iostat', &
'', &
'STANDARD', &
'', &
'Fortran 2003 and later', &
'', &
'fortran-lang intrinsic descriptions (@urbanjost)', &
'']

shortname="is_iostat_end"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('113','is_iostat_eor')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'IS_IOSTAT_EOR(3) - [STATE] Test for end-of-record value (GFDL)', &
'', &
'SYNTAX', &
'', &
'    result = is_iostat_eor(i)', &
'', &
'DESCRIPTION', &
'', &
'is_iostat_eor tests whether an variable has the value of the I/O status', &
'"end of record". The function is equivalent to comparing the variable', &
'with the iostat_eor parameter of the intrinsic module ISO_FORTRAN_ENV.', &
'', &
'ARGUMENTS', &
'', &
'    I', &
'        Shall be of the type _integer_.', &
'', &
'RETURNS', &
'', &
'Returns a _logical_ of the default kind, which .true. if I has the value', &
'which indicates an end of file condition for iostat= specifiers, and is', &
'.false. otherwise.', &
'', &
'EXAMPLES', &
'', &
'Sample program:', &
'', &
'    program demo_is_iostat_eor', &
'    implicit none', &
'    integer :: stat, i(50)', &
'', &
'      open(88, file=''test.dat'', form=''unformatted'')', &
'      read(88, iostat=stat) i', &
'', &
'      if(is_iostat_eor(stat)) stop ''end of record''', &
'', &
'    end program demo_is_iostat_eor', &
'', &
'STANDARD', &
'', &
'Fortran 2003 and later', &
'', &
'fortran-lang intrinsic descriptions', &
'']

shortname="is_iostat_eor"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('114','kind')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'KIND(3) - [KIND INQUIRY] Kind of an entity (GFDL)', &
'', &
'SYNTAX', &
'', &
'    k = kind(x)', &
'', &
'DESCRIPTION', &
'', &
'KIND(X) returns the kind value of the entity X.', &
'', &
'ARGUMENTS', &
'', &
'    X', &
'        Shall be of type _logical_, _integer_, _real_, _complex_ or', &
'        _character_.', &
'', &
'RETURNS', &
'', &
'The return value is a scalar of type _integer_ and of the default', &
'integer kind.', &
'', &
'EXAMPLES', &
'', &
'Sample program:', &
'', &
'    program demo_kind', &
'    implicit none', &
'    integer,parameter :: kc = kind('' '')', &
'    integer,parameter :: kl = kind(.true.)', &
'', &
'       print *, "The default character kind is ", kc', &
'       print *, "The default logical kind is ", kl', &
'', &
'    end program demo_kind', &
'', &
'Results:', &
'', &
'        The default character kind is            1', &
'        The default logical kind is            4', &
'', &
'STANDARD', &
'', &
'Fortran 95 and later', &
'', &
'fortran-lang intrinsic descriptions', &
'']

shortname="kind"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('115','lbound')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'LBOUND(3) - [ARRAY INQUIRY] Lower dimension bounds of an array', &
'', &
'SYNTAX', &
'', &
'    result = lbound(array, dim, kind)', &
'', &
'       TYPE(kind=KIND) elemental function lbound(array,dim,kind)', &
'       TYPE(kind=KIND),intent(in)  :: array', &
'       integer,optional,intent(in) :: dim', &
'       integer,optional,intent(in) :: kind', &
'', &
'DESCRIPTION', &
'', &
'Returns the lower bounds of an array, or a single lower bound along the', &
'DIM dimension.', &
'', &
'ARGUMENTS', &
'', &
'    ARRAY', &
'        Shall be an array, of any type.', &
'', &
'    DIM', &
'        Shall be a scalar _integer_.', &
'', &
'    KIND', &
'        An _integer_ initialization expression indicating the kind', &
'        parameter of the result.', &
'', &
'RETURNS', &
'', &
'The return value is of type _integer_ and of kind KIND. If KIND is', &
'absent, the return value is of default integer kind. If DIM is absent,', &
'the result is an array of the lower bounds of ARRAY. If DIM is present,', &
'the result is a scalar corresponding to the lower bound of the array', &
'along that dimension. If ARRAY is an expression rather than a whole', &
'array or array structure component, or if it has a zero extent along the', &
'relevant dimension, the lower bound is taken to be 1.', &
'', &
'EXAMPLES', &
'', &
'Note that in my opinion this function should not be used on assumed-size', &
'arrays or in any function without an explicit interface. Errors can', &
'occur if there is no interface defined.', &
'', &
'Sample program', &
'', &
'    ! program demo_lbound', &
'    module m_bounds', &
'    implicit none', &
'     contains', &
'        subroutine msub(arr)', &
'           !!integer,intent(in) :: arr(*)  ! cannot be assumed-size array', &
'           integer,intent(in) :: arr(:)', &
'           write(*,*)''MSUB: LOWER='',lbound(arr), &', &
'           & ''UPPER='',ubound(arr), &', &
'           & ''SIZE='',size(arr)', &
'        end subroutine msub', &
'     end module m_bounds', &
'', &
'     use m_bounds, only : msub', &
'     implicit none', &
'     interface', &
'        subroutine esub(arr)', &
'        integer,intent(in) :: arr(:)', &
'        end subroutine esub', &
'     end interface', &
'     integer :: arr(-10:10)', &
'        write(*,*)''MAIN: LOWER='',lbound(arr), &', &
'        & ''UPPER='',ubound(arr), &', &
'        & ''SIZE='',size(arr)', &
'        call csub()', &
'        call msub(arr)', &
'        call esub(arr)', &
'     contains', &
'    subroutine csub', &
'       write(*,*)''CSUB: LOWER='',lbound(arr), &', &
'       & ''UPPER='',ubound(arr), &', &
'       & ''SIZE='',size(arr)', &
'    end subroutine csub', &
'    end', &
'', &
'     subroutine esub(arr)', &
'     implicit none', &
'     integer,intent(in) :: arr(:)', &
'        ! WARNING: IF CALLED WITHOUT AN EXPLICIT INTERFACE', &
'        ! THIS WILL GIVE UNDEFINED ANSWERS (like 0,0,0)', &
'        write(*,*)''ESUB: LOWER='',lbound(arr), &', &
'        & ''UPPER='',ubound(arr), &', &
'        & ''SIZE='',size(arr)', &
'     end subroutine esub', &
'', &
'    !end program demo_lbound', &
'', &
'Results:', &
'', &
'       MAIN: LOWER=         -10 UPPER=          10 SIZE=          21', &
'       CSUB: LOWER=         -10 UPPER=          10 SIZE=          21', &
'       MSUB: LOWER=           1 UPPER=          21 SIZE=          21', &
'       ESUB: LOWER=           1 UPPER=          21 SIZE=          21', &
'', &
'STANDARD', &
'', &
'Fortran 95 and later, with KIND argument - Fortran 2003 and later', &
'', &
'SEE ALSO', &
'', &
'UBOUND(3), CO_LBOUND(3)', &
'', &
'fortran-lang intrinsic descriptions', &
'']

shortname="lbound"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('116','leadz')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'LEADZ(3) - [BIT:COUNT] Number of leading zero bits of an integer (GFDL)', &
'', &
'SYNTAX', &
'', &
'    result = leadz(i)', &
'', &
'DESCRIPTION', &
'', &
'LEADZ returns the number of leading zero bits of an integer.', &
'', &
'ARGUMENTS', &
'', &
'    I', &
'        Shall be of type _integer_.', &
'', &
'RETURNS', &
'', &
'The type of the return value is the same as a default _integer_. If all', &
'the bits of I are zero, the result value is BIT_SIZE(I).', &
'', &
'EXAMPLES', &
'', &
'Sample program:', &
'', &
'    program demo_leadz', &
'    implicit none', &
'    integer :: value, i', &
'    character(len=80) :: f', &
'      write(*,''(*(g0))'')''BIT_SIZE='',bit_size(value)', &
'      ! make a format statement for writing a value as a bit string', &
'      write(f,''("(b",i0,".",i0,")")'')bit_size(value),bit_size(value)', &
'      ! show output for various integer values', &
'      value=0', &
'      do i=0,bit_size(value)-1', &
'         write (*,''("LEADING ZERO BITS=",i3,1x)'') leadz(value)', &
'         write (*,''(" FOR VALUE ")'',advance=''no'')', &
'         write(*,f,advance=''no'') value', &
'         write(*,''(*(1x,g0))'') "OR",value', &
'         value=value+2**(i)', &
'      enddo', &
'    end program demo_leadz', &
'', &
'Results:', &
'', &
'       BIT_SIZE=32', &
'       LEADING ZERO BITS= 32', &
'        FOR VALUE 00000000000000000000000000000000 OR 0', &
'       LEADING ZERO BITS= 31', &
'        FOR VALUE 00000000000000000000000000000001 OR 1', &
'       LEADING ZERO BITS= 30', &
'        FOR VALUE 00000000000000000000000000000011 OR 3', &
'       LEADING ZERO BITS= 29', &
'        FOR VALUE 00000000000000000000000000000111 OR 7', &
'       LEADING ZERO BITS= 28', &
'        FOR VALUE 00000000000000000000000000001111 OR 15', &
'       LEADING ZERO BITS= 27', &
'        FOR VALUE 00000000000000000000000000011111 OR 31', &
'       LEADING ZERO BITS= 26', &
'        FOR VALUE 00000000000000000000000000111111 OR 63', &
'       LEADING ZERO BITS= 25', &
'        FOR VALUE 00000000000000000000000001111111 OR 127', &
'       LEADING ZERO BITS= 24', &
'        FOR VALUE 00000000000000000000000011111111 OR 255', &
'       LEADING ZERO BITS= 23', &
'        FOR VALUE 00000000000000000000000111111111 OR 511', &
'       LEADING ZERO BITS= 22', &
'        FOR VALUE 00000000000000000000001111111111 OR 1023', &
'       LEADING ZERO BITS= 21', &
'        FOR VALUE 00000000000000000000011111111111 OR 2047', &
'       LEADING ZERO BITS= 20', &
'        FOR VALUE 00000000000000000000111111111111 OR 4095', &
'       LEADING ZERO BITS= 19', &
'        FOR VALUE 00000000000000000001111111111111 OR 8191', &
'       LEADING ZERO BITS= 18', &
'        FOR VALUE 00000000000000000011111111111111 OR 16383', &
'       LEADING ZERO BITS= 17', &
'        FOR VALUE 00000000000000000111111111111111 OR 32767', &
'       LEADING ZERO BITS= 16', &
'        FOR VALUE 00000000000000001111111111111111 OR 65535', &
'       LEADING ZERO BITS= 15', &
'        FOR VALUE 00000000000000011111111111111111 OR 131071', &
'       LEADING ZERO BITS= 14', &
'        FOR VALUE 00000000000000111111111111111111 OR 262143', &
'       LEADING ZERO BITS= 13', &
'        FOR VALUE 00000000000001111111111111111111 OR 524287', &
'       LEADING ZERO BITS= 12', &
'        FOR VALUE 00000000000011111111111111111111 OR 1048575', &
'       LEADING ZERO BITS= 11', &
'        FOR VALUE 00000000000111111111111111111111 OR 2097151', &
'       LEADING ZERO BITS= 10', &
'        FOR VALUE 00000000001111111111111111111111 OR 4194303', &
'       LEADING ZERO BITS=  9', &
'        FOR VALUE 00000000011111111111111111111111 OR 8388607', &
'       LEADING ZERO BITS=  8', &
'        FOR VALUE 00000000111111111111111111111111 OR 16777215', &
'       LEADING ZERO BITS=  7', &
'        FOR VALUE 00000001111111111111111111111111 OR 33554431', &
'       LEADING ZERO BITS=  6', &
'        FOR VALUE 00000011111111111111111111111111 OR 67108863', &
'       LEADING ZERO BITS=  5', &
'        FOR VALUE 00000111111111111111111111111111 OR 134217727', &
'       LEADING ZERO BITS=  4', &
'        FOR VALUE 00001111111111111111111111111111 OR 268435455', &
'       LEADING ZERO BITS=  3', &
'        FOR VALUE 00011111111111111111111111111111 OR 536870911', &
'       LEADING ZERO BITS=  2', &
'        FOR VALUE 00111111111111111111111111111111 OR 1073741823', &
'       LEADING ZERO BITS=  1', &
'        FOR VALUE 01111111111111111111111111111111 OR 2147483647', &
'', &
'STANDARD', &
'', &
'Fortran 2008 and later', &
'', &
'SEE ALSO', &
'', &
'BIT_SIZE(3), POPCNT(3), POPPAR(3), TRAILZ(3)', &
'', &
'fortran-lang intrinsic descriptions', &
'']

shortname="leadz"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('117','len')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'LEN(3) - [CHARACTER] Length of a character entity', &
'', &
'SYNTAX', &
'', &
'       l = len(string, kind)', &
'', &
'        integer(kind=KIND) function len(string,kind) result(value)', &
'        character(len=*),intent(in) :: string', &
'        integer,optional,intent(in) :: KIND', &
'        integer(kind=KIND) :: value', &
'', &
'where the returned value is the same kind as the KIND, or of the default', &
'kind if KIND is not specified.', &
'', &
'DESCRIPTION', &
'', &
'LEN(3) Returns the length of a _character_ string.', &
'', &
'If STRING is an array, the length of an element of STRING is returned.', &
'', &
'Note that STRING need not be defined when this intrinsic is invoked, as', &
'only the length (not the content) of STRING is needed.', &
'', &
'ARGUMENTS', &
'', &
'    STRING', &
'        Shall be a scalar or array of type _character_.', &
'', &
'    KIND', &
'        An _integer_ initialization expression indicating the kind', &
'        parameter of the result.', &
'', &
'RETURNS', &
'', &
'The return value is of type _integer_ and of kind KIND. If KIND is', &
'absent, the return value is of default integer kind.', &
'', &
'STANDARD', &
'', &
'FORTRAN 77 and later; with KIND argument - Fortran 2003 and later', &
'', &
'EXAMPLES', &
'', &
'Sample program', &
'', &
'    program demo_len', &
'    implicit none', &
'    character(len=40) :: string', &
'    character(len=:),allocatable :: astring', &
'    character(len=:),allocatable :: many_strings(:)', &
'    integer :: ii', &
'', &
'       ii=len(string)', &
'      write(*,*)''length ='',ii', &
'', &
'      ! the string length will be constant for the fixed-length variable', &
'      string='' How long is this string? ''', &
'      write(*,''(a)'')'' '',string,repeat(''='',len(string))', &
'', &
'      ! the allocatable string length will be the length of LHS expression', &
'      astring='' How long is this string? ''', &
'      write(*,''(a)'')'' '',astring,repeat(''='',len(astring))', &
'', &
'       ! you can also query the length (and other attributes) of a string', &
'       ! using a "type parameter inquiry:" (available since fortran 2018)', &
'       write(*,*)''length from type parameter inquiry='',string%len', &
'', &
'       ! a scalar is returned for an array, as all values in a Fortran', &
'       ! character array must be of the same length:', &
'', &
'       ! define an allocatable array with a constructor ...', &
'         many_strings = [ character(len=7) :: ''Takata'', ''Tanaka'', ''Hayashi'' ]', &
'       write(*,*)', &
'       write(*,*)''length of ALL elements of array='',len(many_strings)', &
'', &
'       call proc_star('' how long? '')', &
'', &
'    contains', &
'', &
'       subroutine proc_star(str)', &
'       character(len=*),intent(in)  :: str', &
'       character(len=:),allocatable :: str2', &
'       ! the length of str can be used in the definitions of variables', &
'       character(len=len(str))      :: str3', &
'', &
'          if(allocated(str2))deallocate(str2)', &
'          ! syntax for allocating a scalar string', &
'          allocate(character(len=len(str)) :: str2)', &
'', &
'          write(*,*)len(str),len(str2),len(str3)', &
'          ! these are other allowable ways to define str2', &
'          str2=str', &
'          str2=repeat('' '',len(str))', &
'', &
'       end subroutine proc_star', &
'', &
'    end program demo_len', &
'', &
'Results:', &
'', &
'SEE ALSO', &
'', &
'len_trim(3), adjustr(3), trim(3), and adjustl(3) are related routines', &
'that allow you to deal with leading and trailing blanks.', &
'', &
'Functions that perform operations on character strings, return lengths', &
'of arguments, and search for certain arguments:', &
'', &
'-   ELEMENTAL: ADJUSTL(3), ADJUSTR(3), INDEX(3), SCAN(3), VERIFY(3)', &
'', &
'-   NONELEMENTAL: LEN_TRIM(3), LEN(3), REPEAT(3), TRIM(3)', &
'', &
'fortran-lang intrinsic descriptions (@urbanjost)', &
'']

shortname="len"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('118','len_trim')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'LEN_TRIM(3) - [CHARACTER:WHITESPACE] Length of a character entity', &
'without trailing blank characters', &
'', &
'SYNTAX', &
'', &
'       result = len_trim(string, kind)', &
'', &
'        integer(kind=KIND) function len_trim(string,KIND) result (value)', &
'        character(len=*),intent(in) :: string', &
'        integer,optional,intent(in) :: KIND', &
'        integer(kind=KIND) :: value', &
'', &
'DESCRIPTION', &
'', &
'Returns the length of a character string, ignoring any trailing blanks.', &
'', &
'ARGUMENTS', &
'', &
'    STRING', &
'        The input string whose length is to be measured. Shall be a', &
'        scalar of type _character_', &
'', &
'    KIND', &
'        (Optional) An _integer_ initialization expression indicating the', &
'        kind parameter of the result.', &
'', &
'RETURNS', &
'', &
'The return value is of type _integer_ and of kind KIND. If KIND is', &
'absent, the return value is of default _integer_ kind.', &
'', &
'EXAMPLES', &
'', &
'Sample program', &
'', &
'    program demo_len_trim', &
'    implicit none', &
'    character(len=:),allocatable :: string', &
'       string='' how long is this string?     ''', &
'       write(*,*)''LENGTH='',len(string)', &
'       write(*,*)''TRIMMED LENGTH='',len_trim(string)', &
'       !', &
'       ELE:block ! elemental example', &
'       character(len=:),allocatable :: tablet(:)', &
'       tablet=[character(len=256) :: &', &
'       & '' how long is this string?     '',&', &
'       & ''and this one?'']', &
'          write(*,*)''LENGTH=            '',len(tablet)', &
'          write(*,*)''TRIMMED LENGTH=    '',len_trim(tablet)', &
'          write(*,*)''SUM TRIMMED LENGTH='',sum(len_trim(tablet))', &
'       endblock ELE', &
'       !', &
'    end program demo_len_trim', &
'', &
'Results:', &
'', &
'        LENGTH=          30', &
'        TRIMMED LENGTH=          25', &
'        LENGTH=                     256', &
'        TRIMMED LENGTH=              25          13', &
'        SUM TRIMMED LENGTH=          38', &
'', &
'STANDARD', &
'', &
'Fortran 95 and later, with KIND argument - Fortran 2003 and later', &
'', &
'SEE ALSO', &
'', &
'Functions that perform operations on character strings, return lengths', &
'of arguments, and search for certain arguments:', &
'', &
'-   ELEMENTAL: ADJUSTL(3), ADJUSTR(3), INDEX(3), SCAN(3), VERIFY(3)', &
'', &
'-   NONELEMENTAL: REPEAT(3), LEN(3), TRIM(3)', &
'', &
'fortran-lang intrinsic descriptions (@urbanjost)', &
'']

shortname="len_trim"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('119','lge')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'LGE(3) - [CHARACTER:COMPARE] Lexical greater than or equal (GFDL)', &
'', &
'SYNTAX', &
'', &
'    result = lge(string_a, string_b)', &
'', &
'DESCRIPTION', &
'', &
'Determines whether one string is lexically greater than or equal to', &
'another string, where the two strings are interpreted as containing', &
'ASCII character codes. If the String A and String B are not the same', &
'length, the shorter is compared as if spaces were appended to it to form', &
'a value that has the same length as the longer.', &
'', &
'In general, the lexical comparison intrinsics LGE(3), LGT(3), LLE(3),', &
'and LLT(3) differ from the corresponding intrinsic operators .ge., .gt.,', &
'.le., and .lt., in that the latter use the processor''s character', &
'ordering (which is not ASCII on some targets), whereas the former always', &
'use the ASCII ordering.', &
'', &
'ARGUMENTS', &
'', &
'    STRING_A', &
'        Shall be of default _character_ type.', &
'', &
'    STRING_B', &
'        Shall be of default _character_ type.', &
'', &
'RETURNS', &
'', &
'Returns .true. if string_a >= string_b, and .false. otherwise, based on', &
'the ASCII ordering.', &
'', &
'STANDARD', &
'', &
'FORTRAN 77 and later', &
'', &
'SEE ALSO', &
'', &
'[[LGT(3), [[LLE(3), [[LLT(3)', &
'', &
'Functions that perform operations on character strings, return lengths', &
'of arguments, and search for certain arguments:', &
'', &
'-   ELEMENTAL: ADJUSTL(3), ADJUSTR(3), INDEX(3),', &
'', &
'SCAN(3), VERIFY(3)', &
'', &
'-   NONELEMENTAL: LEN_TRIM(3), LEN(3), REPEAT(3), TRIM(3)', &
'', &
'fortran-lang intrinsic descriptions', &
'']

shortname="lge"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('120','lgt')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'LGT(3) - [CHARACTER:COMPARE] Lexical greater than (GFDL)', &
'', &
'SYNTAX', &
'', &
'    result = lgt(string_a, string_b)', &
'', &
'DESCRIPTION', &
'', &
'Determines whether one string is lexically greater than another string,', &
'where the two strings are interpreted as containing ASCII character', &
'codes. If the String A and String B are not the same length, the shorter', &
'is compared as if spaces were appended to it to form a value that has', &
'the same length as the longer.', &
'', &
'In general, the lexical comparison intrinsics LGE, LGT, LLE, and LLT', &
'differ from the corresponding intrinsic operators .ge., .gt., .le., and', &
'.lt., in that the latter use the processor''s character ordering (which', &
'is not ASCII on some targets), whereas the former always use the ASCII', &
'ordering.', &
'', &
'ARGUMENTS', &
'', &
'    STRING_A', &
'        Shall be of default _character_ type.', &
'', &
'    STRING_B', &
'        Shall be of default _character_ type.', &
'', &
'RETURNS', &
'', &
'Returns .true. if string_a > string_b, and .false. otherwise, based on', &
'the ASCII ordering.', &
'', &
'STANDARD', &
'', &
'FORTRAN 77 and later', &
'', &
'SEE ALSO', &
'', &
'LGE(3), LLE(3), LLT(3)', &
'', &
'Functions that perform operations on character strings, return lengths', &
'of arguments, and search for certain arguments:', &
'', &
'-   ELEMENTAL: ADJUSTL(3), ADJUSTR(3), INDEX(3),', &
'', &
'SCAN(3), VERIFY(3)', &
'', &
'-   NONELEMENTAL: LEN_TRIM(3), LEN(3), REPEAT(3), TRIM(3)', &
'', &
'fortran-lang intrinsic descriptions', &
'']

shortname="lgt"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('121','lle')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'LLE(3) - [CHARACTER:COMPARE] Lexical less than or equal (GFDL)', &
'', &
'SYNTAX', &
'', &
'    result = lle(str_a, str_b)', &
'', &
'       character(len=*),intent(in) :: str_a, str_b', &
'', &
'          or', &
'', &
'       character(len=*),intent(in) :: str_a, str_b(*) logical :: result', &
'', &
'DESCRIPTION', &
'', &
'Determines whether one string is lexically less than or equal to another', &
'string, where the two strings are interpreted as containing ASCII', &
'character codes. if the STRING_A and STRING_B are not the same length,', &
'the shorter is compared as if spaces were appended to it to form a value', &
'that has the same length as the longer. Leading spaces are significant.', &
'', &
'In general, the lexical comparison intrinsics LGE, LGT, LLE, and LLT', &
'differ from the corresponding intrinsic operators .ge., .gt., .le., and', &
'.lt., in that the latter use the processor''s character ordering (which', &
'is not ASCII on some targets), whereas the former always use the ASCII', &
'ordering.', &
'', &
'ARGUMENTS', &
'', &
'    STR_A', &
'        variable or array of default _character_ type.', &
'', &
'    STR_B', &
'        variable or array of default _character_ type.', &
'', &
'    if STR_A and STR_B are both arrays they must be of the same shape.', &
'', &
'RETURNS', &
'', &
'-   RESULT Returns .TRUE. if STR_A <= STR_B, and .FALSE. otherwise,', &
'    based on the ASCII ordering.', &
'', &
'EXAMPLES', &
'', &
'Sample program:', &
'', &
'    program demo_lle', &
'    implicit none', &
'    integer             :: i', &
'       write(*,''(*(a))'')(char(i),i=32,126)', &
'         write(*,*) lle(''abc'',''ABC'')              ! F lowercase is > uppercase', &
'         write(*,*) lle(''abc'',''abc  '')            ! T trailing spaces', &
'         ! If both strings are of zero length the result is true.', &
'         write(*,*) lle('''','''')                    ! T', &
'         write(*,*) lle('''',''a'')                   ! T the null string is padded', &
'         write(*,*) lle(''a'','''')                   ! F', &
'         write(*,*) lle(''abc'',[''abc'',''123''])      ! [T,F] scalar and array', &
'         write(*,*) lle([''cba'', ''123''],''abc'')     ! [F,T]', &
'         write(*,*) lle([''abc'',''123''],[''cba'',''123'']) ! [T,T] both arrays', &
'    end program demo_lle', &
'', &
'Results:', &
'', &
'      !"#$%&''()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ', &
'      [\]^_`abcdefghijklmnopqrstuvwxyz{|}~', &
'      F', &
'      T', &
'      T', &
'      T', &
'      F', &
'      T F', &
'      F T', &
'      T T', &
'', &
'STANDARD', &
'', &
'FORTRAN 77 and later', &
'', &
'SEE ALSO', &
'', &
'LGE(3), LGT(3),, LLT(3)', &
'', &
'Functions that perform operations on character strings, return lengths', &
'of arguments, and search for certain arguments:', &
'', &
'-   ELEMENTAL: ADJUSTL(3), ADJUSTR(3), INDEX(3),', &
'', &
'SCAN(3), VERIFY(3)', &
'', &
'-   NONELEMENTAL: LEN_TRIM(3), LEN(3), REPEAT(3), TRIM(3)', &
'', &
'fortran-lang intrinsic descriptions', &
'']

shortname="lle"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('122','llt')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'LLT(3) - [CHARACTER:COMPARE] Lexical less than (GFDL)', &
'', &
'SYNTAX', &
'', &
'    result = llt(string_a, string_b)', &
'', &
'DESCRIPTION', &
'', &
'Determines whether one string is lexically less than another string,', &
'where the two strings are interpreted as containing ASCII character', &
'codes. If the STRING_A and STRING_B are not the same length, the shorter', &
'is compared as if spaces were appended to it to form a value that has', &
'the same length as the longer.', &
'', &
'In general, the lexical comparison intrinsics LGE, LGT, LLE, and LLT', &
'differ from the corresponding intrinsic operators .ge., .gt., .le., and', &
'.lt., in that the latter use the processor''s character ordering (which', &
'is not ASCII on some targets), whereas the former always use the ASCII', &
'ordering.', &
'', &
'ARGUMENTS', &
'', &
'    STRING_A', &
'        Shall be of default _character_ type.', &
'', &
'    STRING_B', &
'        Shall be of default _character_ type.', &
'', &
'RETURNS', &
'', &
'Returns .true. if string_a <= string_b, and .false. otherwise, based on', &
'the ASCII ordering.', &
'', &
'STANDARD', &
'', &
'FORTRAN 77 and later', &
'', &
'SEE ALSO', &
'', &
'LGE(3), LGT(3), LLE(3)', &
'', &
'Functions that perform operations on character strings, return lengths', &
'of arguments, and search for certain arguments:', &
'', &
'-   ELEMENTAL: ADJUSTL(3), ADJUSTR(3), INDEX(3), SCAN(3), VERIFY(3)', &
'', &
'-   NONELEMENTAL: LEN_TRIM(3), LEN(3), REPEAT(3), TRIM(3)', &
'', &
'fortran-lang intrinsic descriptions', &
'']

shortname="llt"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('123','log10')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'LOG10(3) - [MATHEMATICS] Base 10 logarithm function', &
'', &
'SYNTAX', &
'', &
'    result = log10(x)', &
'', &
'       real(kind=KIND) elemental function log10(x)', &
'       real(kind=KIND),intent(in) :: x', &
'', &
'DESCRIPTION', &
'', &
'LOG10(X) computes the base 10 logarithm of X. This is generally called', &
'the "common logarithm".', &
'', &
'ARGUMENTS', &
'', &
'    X', &
'        A _real_ value > 0 to take the log of.', &
'', &
'RETURNS', &
'', &
'The return value is of type _real_ . The kind type parameter is the same', &
'as X.', &
'', &
'EXAMPLES', &
'', &
'Sample program:', &
'', &
'    program demo_log10', &
'    use, intrinsic :: iso_fortran_env, only : real_kinds, &', &
'     & real32, real64, real128', &
'    implicit none', &
'    real(kind=real64) :: x = 10.0_real64', &
'', &
'       x = log10(x)', &
'       write(*,''(*(g0))'')''log10('',x,'') is '',log10(x)', &
'', &
'       ! elemental', &
'       write(*, *)log10([1.0, 10.0, 100.0, 1000.0, 10000.0, &', &
'                         & 100000.0, 1000000.0, 10000000.0])', &
'', &
'    end program demo_log10', &
'', &
'Results:', &
'', &
'       log10(1.0000000000000000) is 0.0000000000000000', &
'          0.00000000       1.00000000       2.00000000       3.00000000', &
'          4.00000000       5.00000000       6.00000000       7.00000000', &
'', &
'STANDARD', &
'', &
'FORTRAN 77 and later', &
'', &
'fortran-lang intrinsic descriptions', &
'']

shortname="log10"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('124','log')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'LOG(3) - [MATHEMATICS] Logarithm function (GFDL)', &
'', &
'SYNTAX', &
'', &
'    result = log(x)', &
'', &
'DESCRIPTION', &
'', &
'LOG(X) computes the natural logarithm of X, i.e. the logarithm to the', &
'base "e".', &
'', &
'ARGUMENTS', &
'', &
'    X', &
'        The type shall be _real_ or _complex_.', &
'', &
'RETURNS', &
'', &
'The return value is of type _real_ or _complex_. The kind type parameter', &
'is the same as X. If X is _complex_, the imaginary part OMEGA is in the', &
'range', &
'', &
'-PI < OMEGA <= PI.', &
'', &
'EXAMPLES', &
'', &
'Sample program:', &
'', &
'    program demo_log', &
'    implicit none', &
'      real(kind(0.0d0)) :: x = 2.71828182845904518d0', &
'      complex :: z = (1.0, 2.0)', &
'      write(*,*)x, log(x)    ! will yield (approximately) 1', &
'      write(*,*)z, log(z)', &
'    end program demo_log', &
'', &
'Results:', &
'', &
'          2.7182818284590451        1.0000000000000000', &
'       (1.00000000,2.00000000) (0.804718971,1.10714877)', &
'', &
'STANDARD', &
'', &
'FORTRAN 77 and later', &
'', &
'fortran-lang intrinsic descriptions', &
'']

shortname="log"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('125','log_gamma')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'LOG_GAMMA(3) - [MATHEMATICS] Logarithm of the Gamma function (GFDL)', &
'', &
'SYNTAX', &
'', &
'    x = log_gamma(x)', &
'', &
'DESCRIPTION', &
'', &
'LOG_GAMMA(X) computes the natural logarithm of the absolute value of the', &
'Gamma function.', &
'', &
'ARGUMENTS', &
'', &
'    X', &
'        Shall be of type _real_ and neither zero nor a negative integer.', &
'', &
'RETURNS', &
'', &
'The return value is of type _real_ of the same kind as X.', &
'', &
'EXAMPLES', &
'', &
'Sample program:', &
'', &
'    program demo_log_gamma', &
'    implicit none', &
'    real :: x = 1.0', &
'       write(*,*)x,log_gamma(x) ! returns 0.0', &
'    end program demo_log_gamma', &
'', &
'Results:', &
'', &
'          1.00000000       0.00000000', &
'', &
'STANDARD', &
'', &
'Fortran 2008 and later', &
'', &
'SEE ALSO', &
'', &
'Gamma function: GAMMA(3)', &
'', &
'fortran-lang intrinsic descriptions', &
'']

shortname="log_gamma"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('126','logical')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'LOGICAL(3) - [TYPE:LOGICAL] Converts one kind of _logical_ variable to', &
'another (GFDL)', &
'', &
'SYNTAX', &
'', &
'    result = logical(l, kind)', &
'', &
'     logical(kind=KIND) function logical(L,KIND)', &
'      logical(kind=INK),intent(in) :: L', &
'      integer,intent(in),optional :: KIND', &
'', &
'DESCRIPTION', &
'', &
'Converts one kind of _logical_ variable to another.', &
'', &
'ARGUMENTS', &
'', &
'    L', &
'        The type shall be _logical_.', &
'', &
'    KIND', &
'        (Optional) An _integer_ initialization expression indicating the', &
'        kind parameter of the result.', &
'', &
'RETURNS', &
'', &
'The return value is a _logical_ value equal to L, with a kind', &
'corresponding to KIND, or of the default logical kind if KIND is not', &
'given.', &
'', &
'EXAMPLES', &
'', &
'    program demo_logical', &
'    ! Access array containing the kind type parameter values supported by this', &
'    ! compiler for entities of logical type', &
'    use iso_fortran_env, only : logical_kinds', &
'', &
'       ! list kind values supported on this platform, which generally vary', &
'       ! in storage size', &
'       do i =1, size(logical_kinds)', &
'          write(*,*)logical_kinds(i)', &
'       enddo', &
'', &
'    end program demo_logical', &
'', &
'Results:', &
'', &
'                  1', &
'                  2', &
'                  4', &
'                  8', &
'                 16', &
'', &
'STANDARD', &
'', &
'Fortran 95 and later, related ISO_FORTRAN_ENV module - fortran 2009', &
'', &
'SEE ALSO', &
'', &
'INT(3), REAL(3), CMPLX(3)', &
'', &
'fortran-lang intrinsic descriptions', &
'']

shortname="logical"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('127','maskl')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'MASKL(3) - [BIT:SET] Generates a left justified mask', &
'', &
'SYNTAX', &
'', &
'    result = maskl(i, kind)', &
'', &
'      integer elemental function maskl(i,kind)', &
'      integer,intent(in),optional :: kind', &
'', &
'DESCRIPTION', &
'', &
'MASKL(I[, _KIND_]) has its leftmost I bits set to 1, and the remaining', &
'bits set to 0.', &
'', &
'ARGUMENTS', &
'', &
'    I', &
'        Shall be of type _integer_. Its value must be non-negative, and', &
'        less than or equal to the number of bits for the kind of the', &
'        result.', &
'', &
'    KIND', &
'        Shall be a scalar constant expression of type _integer_.', &
'', &
'RETURNS', &
'', &
'The return value is of type _integer_. If KIND is present, it specifies', &
'the kind value of the return type; otherwise, it is of the default', &
'integer kind.', &
'', &
'The leftmost I bits are set to 1 and the other bits are set to 0.', &
'', &
'EXAMPLES', &
'', &
'Sample program:', &
'', &
'    program demo_maskl', &
'    implicit none', &
'    integer :: i', &
'       i=maskl(1)', &
'       write(*,''(i0,1x,b0,/)'') i,i', &
'       ! elemental', &
'       write(*,''(*(i11,1x,b0,1x,/))'') maskl([(i,i,i=1,bit_size(0))])', &
'    end program demo_maskl', &
'', &
'Results:', &
'', &
'    -2147483648 10000000000000000000000000000000', &
'', &
'              0 0', &
'    -2147483648 10000000000000000000000000000000', &
'    -1073741824 11000000000000000000000000000000', &
'     -536870912 11100000000000000000000000000000', &
'     -268435456 11110000000000000000000000000000', &
'     -134217728 11111000000000000000000000000000', &
'      -67108864 11111100000000000000000000000000', &
'      -33554432 11111110000000000000000000000000', &
'      -16777216 11111111000000000000000000000000', &
'       -8388608 11111111100000000000000000000000', &
'       -4194304 11111111110000000000000000000000', &
'       -2097152 11111111111000000000000000000000', &
'       -1048576 11111111111100000000000000000000', &
'        -524288 11111111111110000000000000000000', &
'        -262144 11111111111111000000000000000000', &
'        -131072 11111111111111100000000000000000', &
'         -65536 11111111111111110000000000000000', &
'         -32768 11111111111111111000000000000000', &
'         -16384 11111111111111111100000000000000', &
'          -8192 11111111111111111110000000000000', &
'          -4096 11111111111111111111000000000000', &
'          -2048 11111111111111111111100000000000', &
'          -1024 11111111111111111111110000000000', &
'           -512 11111111111111111111111000000000', &
'           -256 11111111111111111111111100000000', &
'           -128 11111111111111111111111110000000', &
'            -64 11111111111111111111111111000000', &
'            -32 11111111111111111111111111100000', &
'            -16 11111111111111111111111111110000', &
'             -8 11111111111111111111111111111000', &
'             -4 11111111111111111111111111111100', &
'             -2 11111111111111111111111111111110', &
'             -1 11111111111111111111111111111111', &
'', &
'STANDARD', &
'', &
'Fortran 2008 and later', &
'', &
'SEE ALSO', &
'', &
'MASKR(3)', &
'', &
'fortran-lang intrinsic descriptions (@urbanjost)', &
'']

shortname="maskl"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('128','maskr')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'MASKR(3) - [BIT:SET] Generates a right-justified mask', &
'', &
'SYNTAX', &
'', &
'    result = maskr(i, kind)', &
'', &
'      integer elemental function maskr(i,kind)', &
'      integer,intent(in),optional :: kind', &
'', &
'DESCRIPTION', &
'', &
'MASKR(I[, KIND]) has its rightmost I bits set to 1, and the remaining', &
'bits set to 0.', &
'', &
'ARGUMENTS', &
'', &
'    I', &
'        Shall be of type _integer_. Its value must be non-negative, and', &
'        less than or equal to the number of bits for the kind of the', &
'        result.', &
'', &
'    KIND', &
'        Shall be a scalar constant expression of type _integer_.', &
'', &
'RETURNS', &
'', &
'The return value is of type _integer_. If KIND is present, it specifies', &
'the kind value of the return type; otherwise, it is of the default', &
'integer kind.', &
'', &
'It has its rightmost I bits set to 1, and the remaining bits set to 0.', &
'', &
'EXAMPLE', &
'', &
'Sample program:', &
'', &
'    program demo_maskr', &
'    implicit none', &
'    integer :: i', &
'       i=maskr(1)', &
'       write(*,''(i0,1x,b0,1x,b0/)'') i,i, shiftl(7,bit_size(0)-1)', &
'       i=maskr(5)', &
'       write(*,''(i0,1x,b0,1x,b0/)'') i,i, shiftl(7,bit_size(0)-5)', &
'       i=maskr(11)', &
'       write(*,''(i0,1x,b0,1x,b0/)'') i,i, shiftl(7,bit_size(0)-11)', &
'       ! elemental', &
'       write(*,''(*(i11,1x,b0,1x,/))'') maskr([(i,i,i=0,bit_size(0))])', &
'    end program demo_maskr', &
'', &
'Results:', &
'', &
'    1 1 10000000000000000000000000000000', &
'', &
'    31 11111 111000000000000000000000000000', &
'', &
'    2047 11111111111 111000000000000000000000', &
'', &
'              0 0', &
'              1 1', &
'              3 11', &
'              7 111', &
'             15 1111', &
'             31 11111', &
'             63 111111', &
'            127 1111111', &
'            255 11111111', &
'            511 111111111', &
'           1023 1111111111', &
'           2047 11111111111', &
'           4095 111111111111', &
'           8191 1111111111111', &
'          16383 11111111111111', &
'          32767 111111111111111', &
'          65535 1111111111111111', &
'         131071 11111111111111111', &
'         262143 111111111111111111', &
'         524287 1111111111111111111', &
'        1048575 11111111111111111111', &
'        2097151 111111111111111111111', &
'        4194303 1111111111111111111111', &
'        8388607 11111111111111111111111', &
'       16777215 111111111111111111111111', &
'       33554431 1111111111111111111111111', &
'       67108863 11111111111111111111111111', &
'      134217727 111111111111111111111111111', &
'      268435455 1111111111111111111111111111', &
'      536870911 11111111111111111111111111111', &
'     1073741823 111111111111111111111111111111', &
'     2147483647 1111111111111111111111111111111', &
'             -1 11111111111111111111111111111111', &
'', &
'STANDARD', &
'', &
'Fortran 2008 and later', &
'', &
'SEE ALSO', &
'', &
'MASKL(3)', &
'', &
'fortran-lang intrinsic descriptions (@urbanjost)', &
'']

shortname="maskr"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('129','matmul')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'MATMUL(3) - [TRANSFORMATIONAL] matrix multiplication (GFDL)', &
'', &
'SYNTAX', &
'', &
'    result = matmul(matrix_a, matrix_b)', &
'', &
'DESCRIPTION', &
'', &
'Performs a matrix multiplication on numeric or logical arguments.', &
'', &
'ARGUMENTS', &
'', &
'    MATRIX_A', &
'        An array of _integer_, _real_, _complex_, or _logical_ type,', &
'        with a rank of one or two.', &
'', &
'    MATRIX_B', &
'        An array of _integer_, _real_, or _complex_ type if MATRIX_A is', &
'        of a numeric type; otherwise, an array of _logical_ type. The', &
'        rank shall be one or two, and the first (or only) dimension of', &
'        MATRIX_B shall be equal to the last (or only) dimension of', &
'        MATRIX_A.', &
'', &
'RETURNS', &
'', &
'The matrix product of MATRIX_A and MATRIX_B. The type and kind of the', &
'result follow the usual type and kind promotion rules, as for the * or', &
'.and. operators.', &
'', &
'STANDARD', &
'', &
'Fortran 95 and later', &
'', &
'fortran-lang intrinsic descriptions', &
'']

shortname="matmul"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('130','max')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'MAX(3) - [NUMERIC] Maximum value of an argument list (GFDL)', &
'', &
'SYNTAX', &
'', &
'    result = max(a1, a2, a3, ...)', &
'', &
'DESCRIPTION', &
'', &
'Returns the argument with the largest (most positive) value.', &
'', &
'ARGUMENTS', &
'', &
'    A1', &
'        The type shall be _integer_ or _real_.', &
'', &
'    A2,A3,...', &
'        An expression of the same type and kind as A1.', &
'', &
'RETURNS', &
'', &
'The return value corresponds to the maximum value among the arguments,', &
'and has the same type and kind as the first argument.', &
'', &
'The function is both elemental and allows for an arbitrary number of', &
'arguments. This means if some elements are scalar and some are arrays', &
'that all the arrays must be of the same size, and the returned value', &
'will be an array that is the result as if multiple calls were made with', &
'all scalar values with a single element of each array used in each call.', &
'If called with all arrays the returned array is the same as if multiple', &
'calls were made with MAX(ARR1(1),ARR2(1), ...) to MAX(ARR1(N),ARR2(N)).', &
'', &
'EXAMPLES', &
'', &
'Sample program', &
'', &
'    program demo_max', &
'    implicit none', &
'    real :: arr1(4)= [10.0,11.0,30.0,-100.0]', &
'    real :: arr2(5)= [20.0,21.0,32.0,-200.0,2200.0]', &
'', &
'      !! this is simple enough because it is not being called elementally', &
'      !! because all arguments are scalar', &
'      !!', &
'', &
'      write(*,*)''scalars:'',max(10.0,11.0,30.0,-100.0)', &
'', &
'      !!', &
'      !! this is all max(3) could do before it became an elemental', &
'      !! function and is the most intuitive', &
'      !! except that it can take an arbitrary number of options,', &
'      !! which is not common in Fortran without', &
'      !! declaring a lot of optional parameters.', &
'      !!', &
'      !! That is it unless you want to use the elemental features of max(3)!', &
'', &
'      !! Error: Intrinsic    max    at (1) must have at least two arguments', &
'      !!write(*,*)max(arr1)', &
'      !! This does not work because it is like trying to return', &
'      !! [(max(arr1(i)),i=1,size(arr1))]', &
'      !! so it is trying to take the max of a single value.', &
'      !! To find the largest element of an array', &
'      !! call maxloc(3) or maxval(3).', &
'', &
'      !! Error: Different shape for arguments ''a1'' and ''a2'' for intrinsic', &
'      !! ''max'' at (1) on dimension 1 (4 and 5)', &
'      !!write(*,*)max(arr1,arr2)', &
'      !! but this will return an array of', &
'      !! [(max(arr1(N),arr2(N),N=1,size(arr1))]', &
'', &
'      write(*,*)max(arr1,arr2(1:4))', &
'', &
'      !! so this works only if all the arrays are the same size and', &
'      !! you want an array of the largest Nth elements', &
'      !! from the input arrays.', &
'      !! maybe you wanted to do maxval([arr1,arr2]) or', &
'      !! equivalently max(maxval(arr1),maxval(arr2))', &
'      !! to find the single largest element in both arrays?', &
'', &
'      !! compares all scalars to each member of array and', &
'      !! returns array of size arr2', &
'', &
'      write(*,*)''scalars and array:'',max(10.0,11.0,30.0,-100.0,arr2)', &
'', &
'      !! Error: Different shape for arguments ''a5'' and ''a6''', &
'      !! for intrinsic ''max'' at (1) on dimension 1 (5 and 4)', &
'      !! write(*,*)''scalars and array:'',max(10.0,11.0,30.0,-100.0,arr2,arr1)', &
'      !! as the same reason above when arrays are used', &
'      !! (without scalar values) all the arrays must be the same size', &
'', &
'      write(*,*)''scalars and array:'',&', &
'      & max(40.0,11.0,30.0,-100.0,arr2(:4),arr1)', &
'    end program demo_max', &
'', &
'Results:', &
'', &
'        scalars:   30.000000', &
'          20.0000000  21.000000  32.000000 -100.00000', &
'        scalars and array: 30.000000 30.000000 32.000000 30.000000 2200.0000', &
'        scalars and array: 40.000000 40.000000 40.000000 40.000000', &
'', &
'STANDARD', &
'', &
'FORTRAN 77 and later', &
'', &
'SEE ALSO', &
'', &
'MAXLOC(3), MAXVAL(3), MIN(3)', &
'', &
'fortran-lang intrinsic descriptions', &
'']

shortname="max"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('131','maxexponent')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'MAXEXPONENT(3) - [NUMERIC MODEL] Maximum exponent of a real kind (GFDL)', &
'', &
'SYNTAX', &
'', &
'    result = maxexponent(x)', &
'', &
'DESCRIPTION', &
'', &
'MAXEXPONENT(X) returns the maximum exponent in the model of the type of', &
'X.', &
'', &
'ARGUMENTS', &
'', &
'    X', &
'        Shall be of type _real_.', &
'', &
'RETURNS', &
'', &
'The return value is of type _integer_ and of the default integer kind.', &
'', &
'EXAMPLES', &
'', &
'Sample program:', &
'', &
'    program demo_maxexponent', &
'    use,intrinsic :: iso_fortran_env, only : dp=>real64,sp=>real32', &
'    implicit none', &
'    real(kind=sp) :: x', &
'    real(kind=dp) :: y', &
'', &
'       print *, minexponent(x), maxexponent(x)', &
'       print *, minexponent(y), maxexponent(y)', &
'    end program demo_maxexponent', &
'', &
'Results:', &
'', &
'               -125         128', &
'              -1021        1024', &
'', &
'STANDARD', &
'', &
'Fortran 95 and later', &
'', &
'SEE ALSO', &
'', &
'DIGITS(3), EPSILON(3), EXPONENT(3), FRACTION(3), HUGE(3),', &
'MINEXPONENT(3), NEAREST(3), PRECISION(3), RADIX(3), RANGE(3),', &
'RRSPACING(3), SCALE(3), SET_EXPONENT(3), SPACING(3), TINY(3)', &
'', &
'fortran-lang intrinsic descriptions', &
'']

shortname="maxexponent"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('132','maxloc')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'MAXLOC(3) - [ARRAY:LOCATION] Location of the maximum value within an', &
'array (GFDL)', &
'', &
'SYNTAX', &
'', &
'    result = maxloc(array, dim, mask) result = maxloc(array, mask)', &
'', &
'DESCRIPTION', &
'', &
'Determines the location of the element in the array with the maximum', &
'value, or, if the DIM argument is supplied, determines the locations of', &
'the maximum element along each row of the array in the DIM direction. If', &
'MASK is present, only the elements for which MASK is .TRUE. are', &
'considered. If more than one element in the array has the maximum value,', &
'the location returned is that of the first such element in array element', &
'order. If the array has zero size, or all of the elements of MASK are', &
'.false., then the result is an array of zeroes. Similarly, if DIM is', &
'supplied and all of the elements of MASK along a given row are zero, the', &
'result value for that row is zero.', &
'', &
'ARGUMENTS', &
'', &
'    ARRAY', &
'        Shall be an array of type _integer_, _real_, or _character_.', &
'', &
'    DIM', &
'        (Optional) Shall be a scalar of type _integer_, with a value', &
'        between one and the rank of ARRAY, inclusive. It may not be an', &
'        optional dummy argument.', &
'', &
'    MASK', &
'        Shall be an array of type _logical_, and conformable with ARRAY.', &
'', &
'RETURNS', &
'', &
'If DIM is absent, the result is a rank-one array with a length equal to', &
'the rank of ARRAY. If DIM is present, the result is an array with a rank', &
'one less than the rank of ARRAY, and a size corresponding to the size of', &
'ARRAY with the DIM dimension removed. If DIM is present and ARRAY has a', &
'rank of one, the result is a scalar. In all cases, the result is of', &
'default _integer_ type.', &
'', &
'The value returned is reference to the offset from the beginning of the', &
'array, not necessarily the subscript value if the array subscripts do', &
'not start with one.', &
'', &
'EXAMPLES', &
'', &
'sample program', &
'', &
'    program demo_maxloc', &
'    implicit none', &
'    integer      :: ii', &
'    integer,save :: i(-3:3)=[(abs(abs(ii)-50),ii=-3,3)]', &
'    integer,save :: ints(3,5)= reshape([&', &
'       1,  2,  3,  4,  5, &', &
'       10, 20, 30, 40, 50, &', &
'       11, 22, 33, 44, 55  &', &
'    ],shape(ints),order=[2,1])', &
'', &
'        write(*,*) maxloc(ints)', &
'        write(*,*) maxloc(ints,dim=1)', &
'        write(*,*) maxloc(ints,dim=2)', &
'        ! when array bounds do not start with one remember MAXLOC(3) returns the', &
'        ! offset relative to the lower bound-1 of the location of the maximum', &
'        ! value, not the subscript of the maximum value. When the lower bound of', &
'        ! the array is one, these values are the same. In other words, MAXLOC(3)', &
'        ! returns the subscript of the value assuming the first subscript of the', &
'        ! array is one no matter what the lower bound of the subscript actually', &
'        ! is.', &
'        write(*,''(g0,1x,g0)'') (ii,i(ii),ii=lbound(i,dim=1),ubound(i,dim=1))', &
'        write(*,*)maxloc(i)', &
'', &
'    end program demo_maxloc', &
'', &
'Results:', &
'', &
'          3       5', &
'          3       3       3       3       3', &
'          5       5       5', &
'       -3 47', &
'       -2 48', &
'       -1 49', &
'       0 50', &
'       1 49', &
'       2 48', &
'       3 47', &
'', &
'STANDARD', &
'', &
'Fortran 95 and later', &
'', &
'SEE ALSO', &
'', &
'MAX(3), MAXVAL(3)', &
'', &
'fortran-lang intrinsic descriptions', &
'']

shortname="maxloc"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('133','maxval')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'MAXVAL(3) - [ARRAY REDUCTION] determines the maximum value in an array', &
'or row (GFDL)', &
'', &
'SYNTAX', &
'', &
'    result = maxval(array, dim, mask)', &
'', &
'or', &
'', &
'    result = maxval(array, mask)', &
'', &
'DESCRIPTION', &
'', &
'Determines the maximum value of the elements in an array value, or, if', &
'the DIM argument is supplied, determines the maximum value along each', &
'row of the array in the DIM direction. If MASK is present, only the', &
'elements for which MASK is .TRUE. are considered. If the array has zero', &
'size, or all of the elements of MASK are .false., then the result is the', &
'most negative number of the type and kind of ARRAY if ARRAY is numeric,', &
'or a string of nulls if ARRAY is of character type.', &
'', &
'ARGUMENTS', &
'', &
'    ARRAY', &
'        Shall be an array of type _integer_, _real_, or _character_.', &
'', &
'    DIM', &
'        (Optional) Shall be a scalar of type _integer_, with a value', &
'        between one and the rank of ARRAY, inclusive. It may not be an', &
'        optional dummy argument.', &
'', &
'    MASK', &
'        (Optional) Shall be an array of type _logical_, and conformable', &
'        with ARRAY.', &
'', &
'RETURNS', &
'', &
'If DIM is absent, or if ARRAY has a rank of one, the result is a scalar.', &
'If DIM is present, the result is an array with a rank one less than the', &
'rank of ARRAY, and a size corresponding to the size of ARRAY with the', &
'DIM dimension removed. In all cases, the result is of the same type and', &
'kind as ARRAY.', &
'', &
'EXAMPLES', &
'', &
'sample program:', &
'', &
'    program demo_maxval', &
'    implicit none', &
'    integer,save :: ints(3,5)= reshape([&', &
'       1,  2,  3,  4,  5, &', &
'      10, 20, 30, 40, 50, &', &
'      11, 22, 33, 44, 55  &', &
'    ],shape(ints),order=[2,1])', &
'', &
'       write(*,*) maxval(ints)', &
'       write(*,*) maxval(ints,dim=1)', &
'       write(*,*) maxval(ints,dim=2)', &
'       ! find biggest number less than 30 with mask', &
'       write(*,*) maxval(ints,mask=ints.lt.30)', &
'    end program demo_maxval', &
'', &
'Results:', &
'', &
'       55', &
'       11     22     33     44     55', &
'        5     50     55', &
'       22', &
'', &
'STANDARD', &
'', &
'Fortran 95 and later', &
'', &
'SEE ALSO', &
'', &
'MAX(3), MAXLOC(3)', &
'', &
'fortran-lang intrinsic descriptions', &
'']

shortname="maxval"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('134','merge')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'MERGE(3) - [ARRAY CONSTRUCTION] Merge variables', &
'', &
'SYNTAX', &
'', &
'    result = merge(tsource, fsource, mask)', &
'', &
'DESCRIPTION', &
'', &
'The elemental function MERGE(3) selects values from two arrays or', &
'scalars according to a logical mask. The result is equal to an element', &
'of TSOURCE where the corresponding element of MASK is .TRUE., or an', &
'element of FSOURCE when it is .false. .', &
'', &
'Multi-dimensional arrays are supported.', &
'', &
'Note that argument expressions to MERGE(3) are not required to be', &
'short-circuited so (as an example) if the array X contains zero values', &
'in the statement below the standard does not prevent floating point', &
'divide by zero being generated; as 1.0/X may be evaluated for all values', &
'of X before the mask is used to select which value to retain:', &
'', &
'          y = merge( 1.0/x, 0.0, x /= 0.0 )', &
'', &
'Note the compiler is also free to short-circuit or to generate an', &
'infinity so this may work in many programming environments but is not', &
'recommended.', &
'', &
'For cases like this one may instead use masked assignment via the WHERE', &
'construct:', &
'', &
'          where(x .ne. 0.0)', &
'             y = 1.0/x', &
'          elsewhere', &
'             y = 0.0', &
'          endwhere', &
'', &
'instead of the more obscure', &
'', &
'          merge(1.0/merge(x,1.0,x /= 0.0), 0.0, x /= 0.0)', &
'', &
'ARGUMENTS', &
'', &
'    TSOURCE', &
'        May be of any type, including user-defined.', &
'', &
'    FSOURCE', &
'        Shall be of the same type and type parameters as TSOURCE.', &
'', &
'    MASK', &
'        Shall be of type _logical_.', &
'', &
'Note that (currently) _character_ values must be of the same length.', &
'', &
'RETURNS', &
'', &
'The result is of the same type and type parameters as TSOURCE. For any', &
'element the result is TSOURCE if MASK is true and FSOURCE otherwise.', &
'', &
'EXAMPLES', &
'', &
'The value of', &
'', &
'         merge (1.0, 0.0, k > 0)', &
'', &
'is 1.0 for K=5 and 0.0 for K=-2.', &
'', &
'    program demo_merge', &
'    implicit none', &
'    integer :: tvals(2,3), fvals(2,3), answer(2,3)', &
'    logical :: mask(2,3)', &
'    integer :: i', &
'    logical :: chooseleft', &
'', &
'       tvals(1,:)=[  10, -60,  50 ]', &
'       tvals(2,:)=[ -20,  40, -60 ]', &
'', &
'       fvals(1,:)=[ 0, 3, 2 ]', &
'       fvals(2,:)=[ 7, 4, 8 ]', &
'', &
'       mask(1,:)=[ .true.,  .false., .true. ]', &
'       mask(2,:)=[ .false., .false., .true. ]', &
'', &
'       write(*,*)''mask of logicals''', &
'       answer=merge( tvals, fvals, mask )', &
'       call printme()', &
'', &
'       write(*, *)''highest values''', &
'       answer=merge( tvals, fvals, tvals > fvals )', &
'       call printme()', &
'', &
'       write(*, *)''lowest values''', &
'       answer=merge( tvals, fvals, tvals < fvals )', &
'       call printme()', &
'', &
'       write(*, *)''zero out negative values''', &
'       answer=merge( tvals, 0, tvals < 0)', &
'       call printme()', &
'', &
'       write(*, *)''binary choice''', &
'       chooseleft=.false.', &
'       write(*, ''(3i4)'')merge([1,2,3],[10,20,30],chooseleft)', &
'       chooseleft=.true.', &
'       write(*, ''(3i4)'')merge([1,2,3],[10,20,30],chooseleft)', &
'', &
'    contains', &
'', &
'    subroutine printme()', &
'          write(*, ''(3i4)'')(answer(i, :), i=1, size(answer, dim=1))', &
'    end subroutine printme', &
'', &
'    end program demo_merge', &
'', &
'Expected Results:', &
'', &
'        mask of logicals', &
'         10   3  50', &
'          7   4 -60', &
'        highest values', &
'         10   3  50', &
'          7  40   8', &
'        lowest values', &
'          0 -60   2', &
'        -20   4 -60', &
'        zero out negative values', &
'          0 -60   0', &
'        -20   0 -60', &
'        binary choice', &
'         10  20  30', &
'          1   2   3', &
'', &
'STANDARD', &
'', &
'Fortran 95 and later', &
'', &
'SEE ALSO', &
'', &
'PACK(3), UNPACK(3), PACK(3), SPREAD(3), UNPACK(3)', &
'', &
'fortran-lang intrinsic descriptions (@urbanjost)', &
'']

shortname="merge"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('135','merge_bits')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'MERGE_BITS(3) - [BIT:COPY] Merge of bits under mask (GFDL)', &
'', &
'SYNTAX', &
'', &
'    result = mergebits(i, j, mask)', &
'', &
'DESCRIPTION', &
'', &
'MERGE_BITS(I, J, MASK) merges the bits of I and J as determined by the', &
'mask. The k-th bit of the result is equal to the k-th bit of I if the', &
'k-th bit of MASK is 1; it is equal to the k-th bit of J otherwise.', &
'', &
'ARGUMENTS', &
'', &
'    I', &
'        Shall be of type _integer_.', &
'', &
'    J', &
'        Shall be of type _integer_ and of the same kind as I.', &
'', &
'    MASK', &
'        Shall be of type _integer_ and of the same kind as I.', &
'', &
'RETURNS', &
'', &
'The result is of the same type and kind as I.', &
'', &
'STANDARD', &
'', &
'Fortran 2008 and later', &
'', &
'fortran-lang intrinsic descriptions', &
'']

shortname="merge_bits"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('136','min')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'MIN(3) - [NUMERIC] Minimum value of an argument list (GFDL)', &
'', &
'SYNTAX', &
'', &
'    result = min(a1, a2, a3, ... )', &
'', &
'DESCRIPTION', &
'', &
'Returns the argument with the smallest (most negative) value.', &
'', &
'ARGUMENTS', &
'', &
'    A1', &
'        The type shall be _integer_ or _real_.', &
'', &
'    A2, A3, ```', &
'        An expression of the same type and kind as A1.', &
'', &
'RETURNS', &
'', &
'The return value corresponds to the minimum value among the arguments,', &
'and has the same type and kind as the first argument.', &
'', &
'EXAMPLES', &
'', &
'Sample program', &
'', &
'    program demo_min', &
'    implicit none', &
'        write(*,*)min(10.0,11.0,30.0,-100.0)', &
'    end program demo_min', &
'', &
'Results:', &
'', &
'          -100.0000000', &
'', &
'STANDARD', &
'', &
'FORTRAN 77 and later', &
'', &
'SEE ALSO', &
'', &
'MAX(3), MINLOC(3), MINVAL(3)', &
'', &
'fortran-lang intrinsic descriptions', &
'']

shortname="min"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('137','minexponent')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'MINEXPONENT(3) - [NUMERIC MODEL] Minimum exponent of a real kind (GFDL)', &
'', &
'SYNTAX', &
'', &
'    result = minexponent(x)', &
'', &
'DESCRIPTION', &
'', &
'MINEXPONENT(X) returns the minimum exponent in the model of the type of', &
'X.', &
'', &
'ARGUMENTS', &
'', &
'    X', &
'        Shall be of type _real_.', &
'', &
'RETURNS', &
'', &
'The return value is of type _integer_ and of the default integer kind.', &
'', &
'EXAMPLES', &
'', &
'Sample program:', &
'', &
'    program demo_minexponent', &
'    use, intrinsic :: iso_fortran_env, only : &', &
'     &real_kinds, real32, real64, real128', &
'    implicit none', &
'    real(kind=real32) :: x', &
'    real(kind=real64) :: y', &
'        print *, minexponent(x), maxexponent(x)', &
'        print *, minexponent(y), maxexponent(y)', &
'    end program demo_minexponent', &
'', &
'Expected Results:', &
'', &
'            -125         128', &
'           -1021        1024', &
'', &
'STANDARD', &
'', &
'Fortran 95 and later', &
'', &
'SEE ALSO', &
'', &
'DIGITS(3), EPSILON(3), EXPONENT(3), FRACTION(3), HUGE(3),', &
'MAXEXPONENT(3), NEAREST(3), PRECISION(3), RADIX(3), RANGE(3),', &
'RRSPACING(3), SCALE(3), SET_EXPONENT(3), SPACING(3), TINY(3)', &
'', &
'fortran-lang intrinsic descriptions', &
'']

shortname="minexponent"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('138','minloc')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'MINLOC(3) - [ARRAY:LOCATION] Location of the minimum value within an', &
'array (GFDL)', &
'', &
'SYNTAX', &
'', &
'        result = minloc(array, dim, mask)', &
'', &
'or', &
'', &
'        result = minloc(array, mask)', &
'', &
'DESCRIPTION', &
'', &
'Determines the location of the element in the array with the minimum', &
'value, or, if the DIM argument is supplied, determines the locations of', &
'the minimum element along each row of the array in the DIM direction. If', &
'MASK is present, only the elements for which MASK is .TRUE. are', &
'considered. If more than one element in the array has the minimum value,', &
'the location returned is that of the first such element in array element', &
'order. If the array has zero size, or all of the elements of MASK are', &
'.false., then the result is an array of zeroes. Similarly, if DIM is', &
'supplied and all of the elements of MASK along a given row are zero, the', &
'result value for that row is zero.', &
'', &
'ARGUMENTS', &
'', &
'    ARRAY', &
'        Shall be an array of type _integer_, _real_, or _character_.', &
'', &
'    DIM', &
'        (Optional) Shall be a scalar of type _integer_, with a value', &
'        between one and the rank of ARRAY, inclusive. It may not be an', &
'        optional dummy argument.', &
'', &
'    MASK', &
'        Shall be an array of type _logical_, and conformable with ARRAY.', &
'', &
'RETURNS', &
'', &
'If DIM is absent, the result is a rank-one array with a length equal to', &
'the rank of ARRAY. If DIM is present, the result is an array with a rank', &
'one less than the rank of ARRAY, and a size corresponding to the size of', &
'ARRAY with the DIM dimension removed. If DIM is present and ARRAY has a', &
'rank of one, the result is a scalar. In all cases, the result is of', &
'default _integer_ type.', &
'', &
'EXAMPLES', &
'', &
'sample program:', &
'', &
'    program demo_minloc', &
'    implicit none', &
'    integer,save :: ints(3,5)= reshape([&', &
'       4, 10,  1,  7, 13, &', &
'       9, 15,  6, 12,  3, &', &
'      14,  5, 11,  2,  8  &', &
'    ],shape(ints),order=[2,1])', &
'        write(*,*) minloc(ints)', &
'        write(*,*) minloc(ints,dim=1)', &
'        write(*,*) minloc(ints,dim=2)', &
'        ! where in each column is the smallest number .gt. 10 ?', &
'        write(*,*) minloc(ints,dim=2,mask=ints.gt.10)', &
'        ! a one-dimensional array with dim=1 explicitly listed returns a scalar', &
'        write(*,*) minloc(pack(ints,.true.),dim=1) ! scalar', &
'    end program demo_minloc', &
'', &
'Results:', &
'', &
'             1       3', &
'             1       3       1       3       2', &
'             3       5       4', &
'             5       4       3', &
'             7', &
'', &
'STANDARD', &
'', &
'Fortran 95 and later', &
'', &
'SEE ALSO', &
'', &
'MIN(3), MINVAL(3)', &
'', &
'fortran-lang intrinsic descriptions', &
'']

shortname="minloc"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('139','minval')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'MINVAL(3) - [ARRAY REDUCTION] Minimum value of an array', &
'', &
'SYNTAX', &
'', &
'    result = minval(array, dim, mask) result = minval(array, mask)', &
'', &
'DESCRIPTION', &
'', &
'Determines the minimum value of the elements in an array value, or, if', &
'the DIM argument is supplied, determines the minimum value along each', &
'row of the array in the DIM direction.', &
'', &
'If MASK is present, only the elements for which MASK is .TRUE. are', &
'considered.', &
'', &
'If the array has zero size, or all of the elements of MASK are .false.,', &
'then the result is HUGE(ARRAY) if ARRAY is numeric, or a string of', &
'CHAR(LEN=255) characters if ARRAY is of character type.', &
'', &
'ARGUMENTS', &
'', &
'    ARRAY', &
'        Shall be an array of type _integer_, _real_, or _character_.', &
'', &
'    DIM', &
'        (Optional) Shall be a scalar of type _integer_, with a value', &
'        between one and the rank of ARRAY, inclusive. It may not be an', &
'        optional dummy argument.', &
'', &
'    MASK', &
'        Shall be an array of type _logical_, and conformable with ARRAY.', &
'', &
'RETURNS', &
'', &
'If DIM is absent, or if ARRAY has a rank of one, the result is a scalar.', &
'', &
'If DIM is present, the result is an array with a rank one less than the', &
'rank of ARRAY, and a size corresponding to the size of ARRAY with the', &
'DIM dimension removed. In all cases, the result is of the same type and', &
'kind as ARRAY.', &
'', &
'EXAMPLES', &
'', &
'sample program:', &
'', &
'    program demo_minval', &
'    implicit none', &
'    integer :: i', &
'    character(len=*),parameter :: g=''(3x,*(g0,1x))''', &
'', &
'    integer,save :: ints(3,5)= reshape([&', &
'           1,  -2,   3,   4,   5,  &', &
'          10,  20, -30,  40,  50,  &', &
'          11,  22,  33, -44,  55  &', &
'    ],shape(ints),order=[2,1])', &
'', &
'    integer,save :: box(3,5,2)', &
'', &
'       box(:,:,1)=ints', &
'       box(:,:,2)=-ints', &
'', &
'       write(*,*)''Given the array''', &
'       write(*,''(1x,*(g4.4,1x))'') &', &
'       & (ints(i,:),new_line(''a''),i=1,size(ints,dim=1))', &
'', &
'       write(*,*)''What is the smallest element in the array?''', &
'       write(*,g) minval(ints),''at <'',minloc(ints),''>''', &
'', &
'       write(*,*)''What is the smallest element in each column?''', &
'       write(*,g) minval(ints,dim=1)', &
'', &
'       write(*,*)''What is the smallest element in each row?''', &
'       write(*,g) minval(ints,dim=2)', &
'', &
'       ! notice the shape of the output has less columns', &
'       ! than the input in this case', &
'       write(*,*)''What is the smallest element in each column,''', &
'       write(*,*)''considering only those elements that are''', &
'       write(*,*)''greater than zero?''', &
'       write(*,g) minval(ints, dim=1, mask = ints > 0)', &
'', &
'       write(*,*)&', &
'       & ''if everything is false a zero-sized array is NOT returned''', &
'       write(*,*) minval(ints, dim=1, mask = .false.)', &
'       write(*,*)''even for a zero-sized input''', &
'       write(*,g) minval([integer ::], dim=1, mask = .false.)', &
'', &
'       write(*,*)''a scalar answer for everything false is huge()''', &
'       write(*,g) minval(ints, mask = .false.)', &
'       write(*,g) minval([integer ::], mask = .false.)', &
'', &
'       write(*,*)''some calls with three dimensions''', &
'       write(*,g) minval(box, mask = .true. )', &
'       write(*,g) minval(box, dim=1, mask = .true. )', &
'', &
'       write(*,g) minval(box, dim=2, mask = .true. )', &
'       write(*,g) ''shape of answer is '', &', &
'       & shape(minval(box, dim=2, mask = .true. ))', &
'', &
'    end program demo_minval', &
'', &
'Results:', &
'', &
'     Given the array', &
'        1   -2    3    4    5', &
'       10   20  -30   40   50', &
'       11   22   33  -44   55', &
'', &
'     What is the smallest element in the array?', &
'       -44 at < 3 4 >', &
'     What is the smallest element in each column?', &
'       1 -2 -30 -44 5', &
'     What is the smallest element in each row?', &
'       -2 -30 -44', &
'     What is the smallest element in each column,', &
'     considering only those elements that are', &
'     greater than zero?', &
'       1 20 3 4 5', &
'     if everything is false a zero-sized array is NOT returned', &
'      2147483647  2147483647  2147483647  2147483647  2147483647', &
'     even for a zero-sized input', &
'       2147483647', &
'     a scalar answer for everything false is huge()', &
'       2147483647', &
'       2147483647', &
'     some calls with three dimensions', &
'       -55', &
'       1 -2 -30 -44 5 -11 -22 -33 -40 -55', &
'       -2 -30 -44 -5 -50 -55', &
'       shape of answer is  3 2', &
'', &
'STANDARD', &
'', &
'Fortran 95 and later', &
'', &
'SEE ALSO', &
'', &
'MIN(3), MINLOC(3)', &
'', &
'fortran-lang intrinsic descriptions (@urbanjost)', &
'']

shortname="minval"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('140','mod')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'MOD(3) - [NUMERIC] Remainder function (GFDL)', &
'', &
'SYNTAX', &
'', &
'    result = mod(a, p)', &
'', &
'DESCRIPTION', &
'', &
'MOD(a,p) computes the remainder of the division of A by P.', &
'', &
'ARGUMENTS', &
'', &
'    A', &
'        Shall be a scalar of type _integer_ or _real_.', &
'', &
'    P', &
'        Shall be a scalar of the same type and kind as A and not equal', &
'        to zero.', &
'', &
'RETURNS', &
'', &
'The return value is the result of A - (INT(A/P) * P). The type and kind', &
'of the return value is the same as that of the arguments. The returned', &
'value has the same sign as A and a magnitude less than the magnitude of', &
'P.', &
'', &
'EXAMPLES', &
'', &
'Sample program:', &
'', &
'    program demo_mod', &
'    implicit none', &
'         print *, mod(17,3)           ! yields 2', &
'         print *, mod(17.5,5.5)       ! yields 1.0', &
'         print *, mod(17.5d0,5.5d0)   ! yields 1.0d0', &
'         print *, mod(17.5d0,5.5d0)   ! yields 1.0d0', &
'', &
'         print *, mod(-17,3)          ! yields -2', &
'         print *, mod(-17.5,5.5)      ! yields -1.0', &
'         print *, mod(-17.5d0,5.5d0)  ! yields -1.0d0', &
'         print *, mod(-17.5d0,5.5d0)  ! yields -1.0d0', &
'', &
'         print *, mod(17,-3)          ! yields 2', &
'         print *, mod(17.5,-5.5)      ! yields 1.0', &
'         print *, mod(17.5d0,-5.5d0)  ! yields 1.0d0', &
'         print *, mod(17.5d0,-5.5d0)  ! yields 1.0d0', &
'    end program demo_mod', &
'', &
'Results:', &
'', &
'                  2', &
'          1.00000000', &
'          1.0000000000000000', &
'          1.0000000000000000', &
'                 -2', &
'         -1.00000000', &
'         -1.0000000000000000', &
'         -1.0000000000000000', &
'                  2', &
'          1.00000000', &
'          1.0000000000000000', &
'          1.0000000000000000', &
'', &
'STANDARD', &
'', &
'FORTRAN 77 and later', &
'', &
'SEE ALSO', &
'', &
'MODULO(3)', &
'', &
'fortran-lang intrinsic descriptions', &
'']

shortname="mod"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('141','modulo')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'MODULO(3) - [NUMERIC] Modulo function (GFDL)', &
'', &
'SYNTAX', &
'', &
'    result = modulo(a, p)', &
'', &
'DESCRIPTION', &
'', &
'MODULO(A,P) computes the A modulo P.', &
'', &
'ARGUMENTS', &
'', &
'    A', &
'        Shall be a scalar of type _integer_ or _real_.', &
'', &
'    P', &
'        Shall be a scalar of the same type and kind as A. It shall not', &
'        be zero.', &
'', &
'RETURNS', &
'', &
'The type and kind of the result are those of the arguments.', &
'', &
'-   If A and P are of type _integer_: MODULO(A,P) has the value of A -', &
'    FLOOR (REAL(A) / REAL(P)) * P.', &
'', &
'-   If A and P are of type _real_: MODULO(A,P) has the value of A -', &
'    FLOOR (A / P) * P.', &
'', &
'The returned value has the same sign as P and a magnitude less than the', &
'magnitude of P.', &
'', &
'EXAMPLES', &
'', &
'Sample program:', &
'', &
'    program demo_modulo', &
'    implicit none', &
'         print *, modulo(17,3)        ! yields 2', &
'         print *, modulo(17.5,5.5)    ! yields 1.0', &
'', &
'         print *, modulo(-17,3)       ! yields 1', &
'         print *, modulo(-17.5,5.5)   ! yields 4.5', &
'', &
'         print *, modulo(17,-3)       ! yields -1', &
'         print *, modulo(17.5,-5.5)   ! yields -4.5', &
'    end program demo_modulo', &
'', &
'Results:', &
'', &
'                  2', &
'          1.00000000', &
'                  1', &
'          4.50000000', &
'                 -1', &
'         -4.50000000', &
'', &
'STANDARD', &
'', &
'Fortran 95 and later', &
'', &
'SEE ALSO', &
'', &
'MOD(3)', &
'', &
'fortran-lang intrinsic descriptions', &
'']

shortname="modulo"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('142','move_alloc')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'MOVE_ALLOC(3) - [] Move allocation from one object to another (GFDL)', &
'', &
'SYNTAX', &
'', &
'    call move_alloc(src, dest)', &
'', &
'DESCRIPTION', &
'', &
'MOVE_ALLOC(SRC, DEST) moves the allocation from SRC to DEST. SRC will', &
'become deallocated in the process.', &
'', &
'ARGUMENTS', &
'', &
'    SRC', &
'        allocatable, INTENT(INOUT), may be of any type and kind.', &
'', &
'    DEST', &
'        allocatable, INTENT(OUT), shall be of the same type, kind and', &
'        rank as SRC.', &
'', &
'EXAMPLES', &
'', &
'Basic Sample program to allocate a bigger grid', &
'', &
'    program demo_move_alloc', &
'    implicit none', &
'    ! Example to allocate a bigger GRID', &
'    real, allocatable :: grid(:), tempgrid(:)', &
'    integer :: n, i', &
'', &
'       ! initialize small GRID', &
'       n = 3', &
'       allocate (grid(1:n))', &
'       grid = [ (real (i), i=1,n) ]', &
'', &
'       ! initialize TEMPGRID which will be used to replace GRID', &
'       allocate (tempgrid(1:2*n))    ! Allocate bigger grid', &
'       tempgrid(::2)  = grid         ! Distribute values to new locations', &
'       tempgrid(2::2) = grid + 0.5   ! initialize other values', &
'', &
'       ! move TEMPGRID to GRID', &
'       call MOVE_ALLOC (from=tempgrid, to=grid)', &
'', &
'       ! TEMPGRID should no longer be allocated', &
'       ! and GRID should be the size TEMPGRID was', &
'       if (size (grid) /= 2*n .or. allocated (tempgrid)) then', &
'          print *, "Failure in move_alloc!"', &
'       endif', &
'       print *, allocated(grid), allocated(tempgrid)', &
'       print ''(99f8.3)'', grid', &
'    end program demo_move_alloc', &
'', &
'Results:', &
'', &
'        T F', &
'          1.000   1.500   2.000   2.500   3.000   3.500', &
'', &
'STANDARD', &
'', &
'Fortran 2003 and later', &
'', &
'SEE ALSO', &
'', &
'ALLOCATED(3)', &
'', &
'fortran-lang intrinsic descriptions', &
'']

shortname="move_alloc"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('143','mvbits')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'MVBITS(3) - [BIT:COPY] Move bits from one integer to another (GFDL)', &
'', &
'SYNTAX', &
'', &
'    call mvbits(from, frompos, len, to, topos)', &
'', &
'DESCRIPTION', &
'', &
'Moves LEN bits from positions FROMPOS through FROMPOS+LEN-1 of FROM to', &
'positions TOPOS through TOPOS+LEN-1 of TO. The portion of argument TO', &
'not affected by the movement of bits is unchanged. The values of', &
'FROMPOS+LEN-1 and TOPOS+LEN-1 must be less than BIT_SIZE(from).', &
'', &
'ARGUMENTS', &
'', &
'    FROM', &
'        The type shall be _integer_.', &
'', &
'    FROMPOS', &
'        The type shall be _integer_.', &
'', &
'    LEN', &
'        The type shall be _integer_.', &
'', &
'    TO', &
'        The type shall be _integer_, of the same kind as FROM.', &
'', &
'    TOPOS', &
'        The type shall be _integer_.', &
'', &
'STANDARD', &
'', &
'Fortran 95 and later', &
'', &
'SEE ALSO', &
'', &
'IEOR(3), IBCLR(3), NOT(3), BTEST(3), IBCLR(3), IBITS(3), IBSET(3),', &
'IAND(3), IOR(3), IEOR(3)', &
'', &
'fortran-lang intrinsic descriptions', &
'']

shortname="mvbits"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('144','nearest')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'NEAREST(3) - [MODEL_COMPONENTS] Nearest representable number (GFDL)', &
'', &
'SYNTAX', &
'', &
'    result = nearest(x, s)', &
'', &
'DESCRIPTION', &
'', &
'NEAREST(X, S) returns the processor-representable number nearest to X in', &
'the direction indicated by the sign of S.', &
'', &
'ARGUMENTS', &
'', &
'    X', &
'        Shall be of type _real_.', &
'', &
'    S', &
'        Shall be of type _real_ and not equal to zero.', &
'', &
'RETURNS', &
'', &
'The return value is of the same type as X. If S is positive, NEAREST', &
'returns the processor-representable number greater than X and nearest to', &
'it. If S is negative, NEAREST returns the processor-representable number', &
'smaller than X and nearest to it.', &
'', &
'EXAMPLES', &
'', &
'Sample program:', &
'', &
'    program demo_nearest', &
'    implicit none', &
'', &
'       real :: x, y', &
'       x = nearest(42.0, 1.0)', &
'       y = nearest(42.0, -1.0)', &
'       write (*,"(3(g20.15))") x, y, x - y', &
'', &
'    !  write (*,"(3(g20.15))") &', &
'    !   nearest(tiny(0.0),1.0), &', &
'    !   nearest(tiny(0.0),-1.0), &', &
'    !   nearest(tiny(0.0),1.0) -nearest(tiny(0.0),-1.0)', &
'', &
'    !  write (*,"(3(g20.15))") &', &
'    !   nearest(huge(0.0),1.0), &', &
'    !   nearest(huge(0.0),-1.0), &', &
'    !   nearest(huge(0.0),1.0)- nearest(huge(0.0),-1.0)', &
'', &
'    end program demo_nearest', &
'', &
'Results:', &
'', &
'       42.0000038146973    41.9999961853027    .762939453125000E-05', &
'', &
'STANDARD', &
'', &
'Fortran 95 and later', &
'', &
'SEE ALSO', &
'', &
'DIGITS(3), EPSILON(3), EXPONENT(3), FRACTION(3), HUGE(3),', &
'MAXEXPONENT(3), MINEXPONENT(3), PRECISION(3), RADIX(3), RANGE(3),', &
'RRSPACING(3), SCALE(3), SET_EXPONENT(3), SPACING(3), TINY(3)', &
'', &
'fortran-lang intrinsic descriptions', &
'']

shortname="nearest"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('145','new_line')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'NEW_LINE(3) - [CHARACTER] new-line character', &
'', &
'SYNTAX', &
'', &
'    result = new_line(c)', &
'', &
'       character(len=1,kind=kind(c)) :: new_line(c)', &
'       character(len=1),intent(in) :: c(..)', &
'', &
'DESCRIPTION', &
'', &
'NEW_LINE(C) returns the new-line character.', &
'', &
'Case (i) : If A is default _character_ and the character in position 10', &
'of the ASCII collating sequence is representable in the default', &
'character set, then the result is ACHAR(10).', &
'', &
'Case (ii) : If A is an ASCII character or an ISO 10646 character, then', &
'the result is CHAR(10, KIND (A)).', &
'', &
'Case (iii) : Otherwise, the result is a processor-dependent character', &
'that represents a newline in output to files connected for formatted', &
'stream output if there is such a character.', &
'', &
'Case (iv) : Otherwise, the result is the blank character.', &
'', &
'ARGUMENTS', &
'', &
'    C', &
'        The argument shall be a scalar or array of the type _character_.', &
'', &
'RETURNS', &
'', &
'Returns a _character_ scalar of length one with the new-line character', &
'of the same kind as parameter C.', &
'', &
'EXAMPLES', &
'', &
'Sample program:', &
'', &
'    program demo_new_line', &
'    implicit none', &
'    character,parameter :: nl=new_line(''a'')', &
'    character(len=:),allocatable :: string', &
'', &
'       string=''This is record 1.''//nl//''This is record 2.''', &
'       write(*,''(a)'') string', &
'', &
'       write(*,''(*(a))'',advance=''no'') &', &
'          nl,''This is record 1.'',nl,''This is record 2.'',nl', &
'', &
'    end program demo_new_line', &
'', &
'Results:', &
'', &
'       This is record 1.', &
'       This is record 2.', &
'', &
'       This is record 1.', &
'       This is record 2.', &
'', &
'STANDARD', &
'', &
'Fortran 2003 and later', &
'', &
'fortran-lang intrinsic descriptions (@urbanjost)', &
'']

shortname="new_line"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('146','nint')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'NINT(3) - [TYPE:NUMERIC] Nearest whole number', &
'', &
'SYNTAX', &
'', &
'        elemental function nint(x [, kind=NN]) result(ANSWER)', &
'         real(kind=??),intent(in) :: x', &
'         integer(kind=NN) :: ANSWER', &
'', &
'DESCRIPTION', &
'', &
'NINT(X) rounds its argument to the nearest whole number with its sign', &
'preserved.', &
'', &
'The user must ensure the value is a valid value for the range of the', &
'KIND returned. If the processor cannot represent the result in the kind', &
'specified, the result is undefined.', &
'', &
'If X is greater than zero, NINT(X) has the value INT(X+0.5).', &
'', &
'If X is less than or equal to zero, NINT(X) has the value INT(A-0.5).', &
'', &
'ARGUMENTS', &
'', &
'    X', &
'        The type of the argument shall be _real_.', &
'', &
'    KIND', &
'        (Optional) A constant _integer_ expression indicating the kind', &
'        parameter of the result. Otherwise, the kind type parameter is', &
'        that of default _integer_ type.', &
'', &
'RETURNS', &
'', &
'    ANSWER', &
'        The result is the integer nearest X, or if there are two', &
'        integers equally near X, the result is whichever such _integer_', &
'        has the greater magnitude.', &
'', &
'    The result is undefined if it cannot be represented in the specified', &
'    integer type.', &
'', &
'EXAMPLES', &
'', &
'Sample program:', &
'', &
'    program demo_nint', &
'    implicit none', &
'    integer,parameter :: dp=kind(0.0d0)', &
'    real              :: x4 = 1.234E0', &
'    real(kind=dp)     :: x8 = 4.721_dp', &
'', &
'    ! basic use', &
'       print *, nint(x4), nint(x8),nint(-x8)', &
'       ! elemental', &
'       print *,nint([ &', &
'       &  -2.7,  -2.5, -2.2, -2.0, -1.5, -1.0, -0.5, &', &
'       &  0.0,   &', &
'       &  +0.5,  +1.0, +1.5, +2.0, +2.2, +2.5, +2.7  ])', &
'', &
'    ! issues', &
'    ISSUES: block', &
'    use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64', &
'    integer :: icheck', &
'       ! make sure input is in range for the type returned', &
'       write(*,*)''Range limits for typical KINDS:''', &
'       write(*,''(1x,g0,1x,g0)'')  &', &
'       & int8,huge(0_int8),   &', &
'       & int16,huge(0_int16), &', &
'       & int32,huge(0_int32), &', &
'       & int64,huge(0_int64)', &
'', &
'       ! the standard does not require this to be an error ...', &
'       x8=12345.67e15 ! too big of a number', &
'       icheck=selected_int_kind(ceiling(log10(x8)))', &
'       write(*,*)''Any KIND big enough? ICHECK='',icheck', &
'       print *, ''These are all wrong answers for '',x8', &
'       print *, nint(x8,kind=int8)', &
'       print *, nint(x8,kind=int16)', &
'       print *, nint(x8,kind=int32)', &
'       print *, nint(x8,kind=int64)', &
'    endblock ISSUES', &
'', &
'    end program demo_nint', &
'', &
'Results:', &
'', &
'         1    5   -5', &
'        -3   -3   -2   -2   -2', &
'        -1   -1    0    1    1', &
'         2    2    2    3    3', &
'        Range limits for typical KINDS:', &
'        1 127', &
'        2 32767', &
'        4 2147483647', &
'        8 9223372036854775807', &
'        Any KIND big enough? ICHECK=          16', &
'        These are all wrong answers for    1.2345669499901444E+019', &
'           0', &
'             0', &
'                  0', &
'        -9223372036854775808', &
'', &
'STANDARD', &
'', &
'FORTRAN 77 and later, with KIND argument - Fortran 90 and later', &
'', &
'SEE ALSO', &
'', &
'AINT(3), ANINT(3), INT(3), SELECTED_INT_KIND(3), CEILING(3), FLOOR(3)', &
'', &
'fortran-lang intrinsic descriptions (@urbanjost)', &
'']

shortname="nint"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('147','norm2')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'NORM2(3) - [MATHEMATICS] Euclidean vector norm (GFDL)', &
'', &
'SYNTAX', &
'', &
'    result = norm2(array, dim)', &
'', &
'    real function result norm2(array, dim)', &
'', &
'       real,intent(in) :: array(..)', &
'       integer,intent(in),optional :: dim', &
'', &
'DESCRIPTION', &
'', &
'Calculates the Euclidean vector norm (L_2 norm) of ARRAY along dimension', &
'DIM.', &
'', &
'ARGUMENTS', &
'', &
'    ARRAY', &
'        Shall be an array of type _real_.', &
'', &
'    DIM', &
'        shall be a scalar of type _integer_ with a value in the range', &
'        from 1 to RANK(ARRAY).', &
'', &
'RETURNS', &
'', &
'The result is of the same type as ARRAY.', &
'', &
'If DIM is absent, a scalar with the square root of the sum of squares of', &
'the elements of ARRAY is returned.', &
'', &
'Otherwise, an array of rank N-1, where N equals the rank of ARRAY, and a', &
'shape similar to that of ARRAY with dimension DIM dropped is returned.', &
'', &
'EXAMPLES', &
'', &
'Sample program:', &
'', &
'    program demo_norm2', &
'    implicit none', &
'    integer :: i', &
'', &
'    real :: x(3,3) = reshape([ &', &
'       1, 2, 3, &', &
'       4, 5 ,6, &', &
'       7, 8, 9  &', &
'    ],shape(x),order=[2,1])', &
'', &
'    write(*,*) ''x=''', &
'    write(*,''(4x,3f4.0)'')transpose(x)', &
'', &
'    write(*,*) ''norm2(x)='',norm2(x)', &
'', &
'    write(*,*) ''x**2=''', &
'    write(*,''(4x,3f4.0)'')transpose(x**2)', &
'    write(*,*)''sqrt(sum(x**2))='',sqrt(sum(x**2))', &
'', &
'    end program demo_norm2', &
'', &
'Results:', &
'', &
'     x=', &
'          1.  2.  3.', &
'          4.  5.  6.', &
'          7.  8.  9.', &
'     norm2(x)=   16.88194', &
'     x**2=', &
'          1.  4.  9.', &
'         16. 25. 36.', &
'         49. 64. 81.', &
'     sqrt(sum(x**2))=   16.88194', &
'', &
'STANDARD', &
'', &
'Fortran 2008 and later', &
'', &
'SEE ALSO', &
'', &
'PRODUCT(3), SUM(3), HYPOT(3)', &
'', &
'fortran-lang intrinsic descriptions', &
'']

shortname="norm2"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('148','not')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'NOT(3) - [BIT:LOGICAL] Logical negation', &
'', &
'SYNTAX', &
'', &
'    result = not(i)', &
'', &
'DESCRIPTION', &
'', &
'NOT returns the bitwise Boolean inverse of I.', &
'', &
'ARGUMENTS', &
'', &
'    I', &
'        The type shall be _integer_.', &
'', &
'RETURNS', &
'', &
'The return type is _integer_, of the same kind as the argument.', &
'', &
'EXAMPLES', &
'', &
'Sample program', &
'', &
'    program demo_not', &
'    implicit none', &
'    integer :: i', &
'', &
'       i=13741', &
'       write(*,''(b32.32,1x,i0)'')i,i', &
'       write(*,''(b32.32,1x,i0)'')not(i),not(i)', &
'', &
'    end program demo_not', &
'', &
'Results:', &
'', &
'       00000000000000000011010110101101 13741', &
'       11111111111111111100101001010010 -13742', &
'', &
'STANDARD', &
'', &
'Fortran 95 and later', &
'', &
'SEE ALSO', &
'', &
'IAND(3), IOR(3), IEOR(3), IBITS(3), IBSET(3),', &
'', &
'IBCLR(3)', &
'', &
'fortran-lang intrinsic descriptions (@urbanjost)', &
'']

shortname="not"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('149','null')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'NULL(3) - [TRANSFORMATIONAL] Function that returns a disassociated', &
'pointer (GFDL)', &
'', &
'SYNTAX', &
'', &
'    ptr => null(mold)', &
'', &
'DESCRIPTION', &
'', &
'Returns a disassociated pointer.', &
'', &
'If MOLD is present, a disassociated pointer of the same type is', &
'returned, otherwise the type is determined by context.', &
'', &
'In _Fortran 95_, MOLD is optional. Please note that _Fortran 2003_', &
'includes cases where it is required.', &
'', &
'ARGUMENTS', &
'', &
'    MOLD', &
'        (Optional) shall be a pointer of any association status and of', &
'        any type.', &
'', &
'RETURNS', &
'', &
'A disassociated pointer or an unallocated allocatable entity.', &
'', &
'EXAMPLES', &
'', &
'Sample program:', &
'', &
'    !program demo_null', &
'    module showit', &
'    implicit none', &
'    private', &
'    character(len=*),parameter :: g=''(*(g0,1x))''', &
'    public gen', &
'    ! a generic interface that only differs in the', &
'    ! type of the pointer the second argument is', &
'    interface gen', &
'     module procedure s1', &
'     module procedure s2', &
'    end interface', &
'', &
'    contains', &
'', &
'    subroutine s1 (j, pi)', &
'     integer j', &
'     integer, pointer :: pi', &
'       if(associated(pi))then', &
'          write(*,g)''Two integers in S1:,'',j,''and'',pi', &
'       else', &
'          write(*,g)''One integer in S1:,'',j', &
'       endif', &
'    end subroutine s1', &
'', &
'    subroutine s2 (k, pr)', &
'     integer k', &
'     real, pointer :: pr', &
'       if(associated(pr))then', &
'          write(*,g)''integer and real in S2:,'',k,''and'',pr', &
'       else', &
'          write(*,g)''One integer in S2:,'',k', &
'       endif', &
'    end subroutine s2', &
'', &
'    end module showit', &
'', &
'    use showit, only : gen', &
'', &
'    real,target :: x = 200.0', &
'    integer,target :: i = 100', &
'', &
'    real, pointer :: real_ptr', &
'    integer, pointer :: integer_ptr', &
'', &
'    ! so how do we call S1() or S2() with a disassociated pointer?', &
'', &
'    ! the answer is the null() function with a mold value', &
'', &
'    ! since s1() and s2() both have a first integer', &
'    ! argument the NULL() pointer must be associated', &
'    ! to a real or integer type via the mold option', &
'    ! so the following can distinguish whether s1(1)', &
'    ! or s2() is called, even though the pointers are', &
'    ! not associated or defined', &
'', &
'    call gen (1, null (real_ptr) )    ! invokes s2', &
'    call gen (2, null (integer_ptr) ) ! invokes s1', &
'    real_ptr => x', &
'    integer_ptr => i', &
'    call gen (3, real_ptr ) ! invokes s2', &
'    call gen (4, integer_ptr ) ! invokes s1', &
'', &
'    end', &
'    !end program demo_null', &
'', &
'Results:', &
'', &
'       One integer in S2:, 1', &
'       One integer in S1:, 2', &
'       integer and real in S2:, 3 and 200.000000', &
'       Two integers in S1:, 4 and 100', &
'', &
'STANDARD', &
'', &
'Fortran 95 and later', &
'', &
'SEE ALSO', &
'', &
'ASSOCIATED(3)', &
'', &
'fortran-lang intrinsic descriptions', &
'']

shortname="null"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('150','num_images')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'NUM_IMAGES(3) - [COLLECTIVE] Number of images (GFDL)', &
'', &
'SYNTAX', &
'', &
'    result = num_images(distance, failed)', &
'', &
'DESCRIPTION', &
'', &
'Returns the number of images.', &
'', &
'ARGUMENTS', &
'', &
'    DISTANCE', &
'        (optional, INTENT(IN)) Nonnegative scalar integer', &
'', &
'    FAILED', &
'        (optional, INTENT(IN)) Scalar logical expression', &
'', &
'RETURNS', &
'', &
'Scalar default-kind _integer_. If DISTANCE is not present or has value', &
'0, the number of images in the current team is returned. For values', &
'smaller or equal distance to the initial team, it returns the number of', &
'images index on the ancestor team which has a distance of DISTANCE from', &
'the invoking team. If DISTANCE is larger than the distance to the', &
'initial team, the number of images of the initial team is returned. If', &
'FAILED is not present the total number of images is returned; if it has', &
'the value .true., the number of failed images is returned, otherwise,', &
'the number of images which do have not the failed status.', &
'', &
'EXAMPLES', &
'', &
'Sample program:', &
'', &
'    program demo_num_images', &
'    implicit none', &
'    integer :: value[*]', &
'    integer :: i', &
'', &
'       value = this_image()', &
'       sync all', &
'       if (this_image() == 1) then', &
'         do i = 1, num_images()', &
'           write(*,''(2(a,i0))'') ''value['', i, ''] is '', value[i]', &
'         end do', &
'       endif', &
'', &
'    end program demo_num_images', &
'', &
'STANDARD', &
'', &
'Fortran 2008 and later. With DISTANCE or FAILED argument, TS 18508 or', &
'later', &
'', &
'SEE ALSO', &
'', &
'THIS_IMAGE(3), IMAGE_INDEX(3)', &
'', &
'fortran-lang intrinsic descriptions', &
'']

shortname="num_images"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('151','pack')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'PACK(3) - [ARRAY CONSTRUCTION] Pack an array into an array of rank one', &
'', &
'SYNTAX', &
'', &
'    result = pack(array, mask,vector)', &
'', &
'       TYPE(kind=KIND) function pack(array,mask,vector)', &
'       TYPE(kind=KIND),option(in) :: array(*)', &
'       logical  :: mask(*)', &
'       TYPE(kind=KIND),option(in),optional :: vector(*)', &
'', &
'where TYPE(kind=KIND) may be any type, where ARRAY and VECTOR and the', &
'returned value must by of the same type. MASK may be a scalar as well an', &
'an array.', &
'', &
'DESCRIPTION', &
'', &
'Stores the elements of ARRAY in an array of rank one.', &
'', &
'The beginning of the resulting array is made up of elements whose MASK', &
'equals .TRUE.. Afterwards, positions are filled with elements taken from', &
'VECTOR.', &
'', &
'ARGUMENTS', &
'', &
'    ARRAY', &
'        Shall be an array of any type.', &
'', &
'    MASK', &
'        Shall be an array of type _logical_ and of the same size as', &
'        ARRAY. Alternatively, it may be a _logical_ scalar.', &
'', &
'    VECTOR', &
'        (Optional) shall be an array of the same type as ARRAY and of', &
'        rank one. If present, the number of elements in VECTOR shall be', &
'        equal to or greater than the number of true elements in MASK. If', &
'        MASK is scalar, the number of elements in VECTOR shall be equal', &
'        to or greater than the number of elements in ARRAY.', &
'', &
'RETURNS', &
'', &
'The result is an array of rank one and the same type as that of ARRAY.', &
'If VECTOR is present, the result size is that of VECTOR, the number of', &
'.TRUE. values in MASK otherwise.', &
'', &
'EXAMPLES', &
'', &
'Sample program:', &
'', &
'    program demo_pack', &
'    implicit none', &
'       call test1()', &
'       call test2()', &
'       call test3()', &
'    contains', &
'    !', &
'    subroutine test1()', &
'    ! gathering nonzero elements from an array:', &
'    integer :: m(6)', &
'', &
'       m = [ 1, 0, 0, 0, 5, 0 ]', &
'       write(*, fmt="(*(i0, '' ''))") pack(m, m /= 0)  ! "1 5"', &
'', &
'    end subroutine test1', &
'    !', &
'    subroutine test2()', &
'    ! Gathering nonzero elements from an array and appending elements', &
'    ! from VECTOR till the size of the mask array (or array size if the', &
'    ! mask is scalar):', &
'    integer :: m(4)', &
'', &
'       m = [ 1, 0, 0, 2 ]', &
'       write(*, fmt="(*(i0, '' ''))") pack(m, m /= 0, [ 0, 0, 3, 4 ])', &
'', &
'    end subroutine test2', &
'    !', &
'    subroutine test3()', &
'    ! select strings whose second character is "a"', &
'    character(len=10) :: m(4)', &
'', &
'    m = [ character(len=10) :: ''ape'', ''bat'', ''cat'', ''dog'']', &
'       write(*, fmt="(*(g0, '' ''))") pack(m, m(:)(2:2) == ''a'' )', &
'', &
'    end subroutine test3', &
'    !', &
'    end program demo_pack', &
'', &
'Results:', &
'', &
'       1 5', &
'       1 2 3 4', &
'       bat        cat', &
'', &
'STANDARD', &
'', &
'Fortran 95 and later', &
'', &
'SEE ALSO', &
'', &
'UNPACK(3), MERGE(3), PACK(3), SPREAD(3), UNPACK(3)', &
'', &
'fortran-lang intrinsic descriptions (@urbanjost)', &
'']

shortname="pack"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('152','parity')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'PARITY(3) - [TRANSFORMATIONAL] Reduction with exclusive OR() (GFDL)', &
'', &
'SYNTAX', &
'', &
'    result = parity(mask, dim)', &
'', &
'DESCRIPTION', &
'', &
'Calculates the parity (i.e. the reduction using .xor.) of MASK along', &
'dimension DIM.', &
'', &
'ARGUMENTS', &
'', &
'    MASK', &
'        Shall be an array of type _logical_.', &
'', &
'    DIM', &
'        (Optional) shall be a scalar of type _integer_ with a value in', &
'        the range from 1 TO N, where N equals the rank of ARRAY.', &
'', &
'RETURNS', &
'', &
'The result is of the same type as MASK.', &
'', &
'If DIM is absent, a scalar with the parity of all elements in MASK is', &
'returned: .TRUE. if an odd number of elements are .TRUE. and', &
'', &
'where N equals the rank of MASK, and a shape similar to that of MASK', &
'with dimension DIM dropped is returned.', &
'', &
'EXAMPLES', &
'', &
'Sample program:', &
'', &
'    program demo_parity', &
'    implicit none', &
'    logical :: x(2) = [ .true., .false. ]', &
'       print *, parity(x) ! T', &
'    end program demo_parity', &
'', &
'Results:', &
'', &
'        T', &
'', &
'STANDARD', &
'', &
'Fortran 2008 and later', &
'', &
'fortran-lang intrinsic descriptions', &
'']

shortname="parity"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('153','popcnt')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'POPCNT(3) - [BIT:COUNT] Number of bits set (GFDL)', &
'', &
'SYNTAX', &
'', &
'    result = popcnt(i)', &
'', &
'DESCRIPTION', &
'', &
'Returns the number of bits set in the binary representation of an', &
'_integer_.', &
'', &
'ARGUMENTS', &
'', &
'    I', &
'        Shall be of type _integer_.', &
'', &
'RETURNS', &
'', &
'The return value is of type _integer_ and of the default integer kind.', &
'', &
'EXAMPLES', &
'', &
'Sample program:', &
'', &
'    program demo_popcnt', &
'    use, intrinsic :: iso_fortran_env, only : integer_kinds, &', &
'       & int8, int16, int32, int64', &
'    implicit none', &
'         print *, popcnt(127),       poppar(127)', &
'         print *, popcnt(huge(0)), poppar(huge(0))', &
'         print *, popcnt(huge(0_int8)), poppar(huge(0_int8))', &
'         print *, popcnt(huge(0_int16)), poppar(huge(0_int16))', &
'         print *, popcnt(huge(0_int32)), poppar(huge(0_int32))', &
'         print *, popcnt(huge(0_int64)), poppar(huge(0_int64))', &
'    end program demo_popcnt', &
'', &
'Results:', &
'', &
'            7           1', &
'           31           1', &
'            7           1', &
'           15           1', &
'           31           1', &
'           63           1', &
'', &
'STANDARD', &
'', &
'Fortran 2008 and later', &
'', &
'SEE ALSO', &
'', &
'POPPAR(3), LEADZ(3), TRAILZ(3)', &
'', &
'fortran-lang intrinsic descriptions', &
'']

shortname="popcnt"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('154','poppar')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'POPPAR(3) - [BIT:COUNT] Parity of the number of bits set (GFDL)', &
'', &
'SYNTAX', &
'', &
'    result = poppar(i)', &
'', &
'DESCRIPTION', &
'', &
'Returns the parity of an integer''s binary representation (i.e., the', &
'parity of the number of bits set).', &
'', &
'ARGUMENTS', &
'', &
'    I', &
'        Shall be of type _integer_.', &
'', &
'RETURNS', &
'', &
'The return value is equal to 0 if I has an even number of bits set and 1', &
'if an odd number of bits are set.', &
'', &
'It is of type _integer_ and of the default _integer_ kind.', &
'', &
'EXAMPLES', &
'', &
'Sample program:', &
'', &
'    program demo_popcnt', &
'    use, intrinsic :: iso_fortran_env, only : integer_kinds, &', &
'       & int8, int16, int32, int64', &
'    implicit none', &
'       print  *,  popcnt(127),            poppar(127)', &
'       print  *,  popcnt(huge(0_int8)),   poppar(huge(0_int8))', &
'       print  *,  popcnt(huge(0_int16)),  poppar(huge(0_int16))', &
'       print  *,  popcnt(huge(0_int32)),  poppar(huge(0_int32))', &
'       print  *,  popcnt(huge(0_int64)),  poppar(huge(0_int64))', &
'    end program demo_popcnt', &
'', &
'Results:', &
'', &
'                  7           1', &
'                  7           1', &
'                 15           1', &
'                 31           1', &
'                 63           1', &
'', &
'STANDARD', &
'', &
'Fortran 2008 and later', &
'', &
'SEE ALSO', &
'', &
'POPCNT(3), LEADZ(3), TRAILZ(3)', &
'', &
'fortran-lang intrinsic descriptions', &
'']

shortname="poppar"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('155','precision')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'PRECISION(3) - [NUMERIC MODEL] Decimal precision of a real kind (GFDL)', &
'', &
'SYNTAX', &
'', &
'    result = precision(x)', &
'', &
'DESCRIPTION', &
'', &
'PRECISION(X) returns the decimal precision in the model of the type of', &
'X.', &
'', &
'ARGUMENTS', &
'', &
'    X', &
'        Shall be of type _real_ or _complex_.', &
'', &
'RETURNS', &
'', &
'The return value is of type _integer_ and of the default integer kind.', &
'', &
'EXAMPLES', &
'', &
'Sample program:', &
'', &
'    program demo_precision', &
'    use,intrinsic :: iso_fortran_env, only : dp=>real64,sp=>real32', &
'    implicit none', &
'    real(kind=sp) :: x(2)', &
'    complex(kind=dp) :: y', &
'', &
'       print *, precision(x), range(x)', &
'       print *, precision(y), range(y)', &
'    end program demo_precision', &
'', &
'Results:', &
'', &
'                  6          37', &
'                 15         307', &
'', &
'STANDARD', &
'', &
'Fortran 95 and later', &
'', &
'SEE ALSO', &
'', &
'DIGITS(3), EPSILON(3), EXPONENT(3), FRACTION(3), HUGE(3),', &
'MAXEXPONENT(3), MINEXPONENT(3), NEAREST(3), RADIX(3), RANGE(3),', &
'RRSPACING(3), SCALE(3), SET_EXPONENT(3), SPACING(3), TINY(3)', &
'', &
'fortran-lang intrinsic descriptions', &
'']

shortname="precision"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('156','present')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'PRESENT(3) - [STATE] Determine whether an optional dummy argument is', &
'specified (GFDL)', &
'', &
'SYNTAX', &
'', &
'    result = present(a)', &
'', &
'DESCRIPTION', &
'', &
'Determines whether an optional dummy argument is present.', &
'', &
'ARGUMENTS', &
'', &
'    A', &
'        May be of any type and may be a pointer, scalar or array value,', &
'        or a dummy procedure. It shall be the name of an optional dummy', &
'        argument accessible within the current subroutine or function.', &
'', &
'RETURNS', &
'', &
'Returns either .TRUE. if the optional argument A is present, or .FALSE.', &
'otherwise.', &
'', &
'EXAMPLES', &
'', &
'Sample program:', &
'', &
'    program demo_present', &
'    implicit none', &
'       write(*,*) func(), func(42)', &
'    contains', &
'', &
'    integer function func(x)', &
'    integer, intent(in), optional :: x', &
'       if(present(x))then', &
'         func=x**2', &
'       else', &
'         func=0', &
'       endif', &
'    end function', &
'', &
'    end program demo_present', &
'', &
'Results:', &
'', &
'         0        1764', &
'', &
'STANDARD', &
'', &
'Fortran 95 and later', &
'', &
'fortran-lang intrinsic descriptions', &
'']

shortname="present"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('157','product')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'PRODUCT(3) - [ARRAY REDUCTION] Product of array elements', &
'', &
'SYNTAX', &
'', &
'      result = product(array, dim, mask)', &
'', &
'        NUMERIC,intent(in) :: array(..)', &
'        integer,intent(in),optional :: dim', &
'        logical,intent(in),optional :: mask(..)', &
'', &
'where NUMERIC is any numeric type', &
'', &
'DESCRIPTION', &
'', &
'Multiplies together all the selected elements of ARRAY, or along', &
'dimension DIM if the corresponding element in MASK is .TRUE..', &
'', &
'If DIM is absent, a scalar with the product of all elements in ARRAY is', &
'returned. (Note a zero-sized ARRAY returns 1).', &
'', &
'When DIM is present, If the masked array has a dimension of one (ie. is', &
'a vector) the result is a scalar. Otherwise, an array of rank N-1, where', &
'N equals the rank of ARRAY, and a shape similar to that of ARRAY with', &
'dimension DIM dropped is returned.', &
'', &
'ARGUMENTS', &
'', &
'    ARRAY', &
'        Shall be an array of type _integer_, _real_ or _complex_.', &
'', &
'    DIM', &
'        shall be a scalar of type _integer_ with a value in the range', &
'        from 1 TO N, where N equals the rank of ARRAY.', &
'', &
'    MASK', &
'        shall be of type _logical_ and either be a scalar or an array of', &
'        the same shape as ARRAY.', &
'', &
'RETURNS', &
'', &
'The result is of the same type as ARRAY.', &
'', &
'EXAMPLES', &
'', &
'Sample program:', &
'', &
'    program demo_product', &
'    implicit none', &
'    character(len=*),parameter :: all=''(*(g0,1x))'' ! a handy format', &
'    character(len=1),parameter :: nl=new_line(''a'')', &
'', &
'    NO_DIM: block', &
'    !    If DIM is not specified, the result is the product of all the', &
'    !    selected array elements.', &
'    integer :: i,n, p1, p2', &
'    integer,allocatable :: array(:)', &
'       ! all elements are selected by default', &
'       do n=1,10', &
'          print all, ''factorial of '',n,'' is '', product([(real(i),i=1,n)])', &
'       enddo', &
'', &
'       ! using a mask', &
'       array=[10,12,13,15,20,25,30]', &
'       p1=product(array, mask=mod(array, 2)==1) ! only odd elements', &
'       p2=product(array, mask=mod(array, 2)/=1) ! only even elements', &
'       print all, nl,''product of all elements'',product(array) ! all elements', &
'       print all, '' odd * even ='',nl,p1,''*'',p2,''='',p1*p2', &
'', &
'       ! NOTE: If ARRAY is a zero-sized array, the result is equal to one', &
'       print all', &
'       print all, ''zero-sized array=>'',product([integer :: ])', &
'       ! NOTE: If nothing in the mask is true, this also results in a null', &
'       !       array', &
'       print all, ''all elements have a false mask=>'',product(array,mask=.false.)', &
'', &
'    endblock NO_DIM', &
'', &
'    WITH_DIM: block', &
'    integer :: rect(2,3)', &
'    integer :: box(2,3,4)', &
'', &
'    !  lets fill a few arrays', &
'       rect = reshape([ &', &
'         1, 2, 3,       &', &
'         4, 5, 6        &', &
'       ],shape(rect),order=[2,1])', &
'       call print_matrix_int(''rect'',rect)', &
'', &
'    !  Find the product of each column in RECT.', &
'       print all, ''product of columns='',product(rect, dim = 1)', &
'', &
'    ! Find the product of each row in RECT.', &
'       print all, ''product of rows='',product(rect, dim = 2)', &
'', &
'    ! now lets try a box', &
'       box(:,:,1)=rect', &
'       box(:,:,2)=rect*(+10)', &
'       box(:,:,3)=rect*(-10)', &
'       box(:,:,4)=rect*2', &
'       ! lets look at the values', &
'       call print_matrix_int(''box 1'',box(:,:,1))', &
'       call print_matrix_int(''box 2'',box(:,:,2))', &
'       call print_matrix_int(''box 3'',box(:,:,3))', &
'       call print_matrix_int(''box 4'',box(:,:,4))', &
'', &
'       ! remember without dim= even a box produces a scalar', &
'       print all, ''no dim gives a scalar'',product(real(box))', &
'', &
'       ! only one plane has negative values, so note all the "1" values', &
'       ! for vectors with no elements', &
'       call print_matrix_int(''negative values'',product(box,mask=box < 0,dim=1))', &
'', &
'    !   If DIM is specified and ARRAY has rank greater than one, the', &
'    !   result is a new array in which dimension DIM has been eliminated.', &
'', &
'       ! pick a dimension to multiply though', &
'       call print_matrix_int(''dim=1'',product(box,dim=1))', &
'', &
'       call print_matrix_int(''dim=2'',product(box,dim=2))', &
'', &
'       call print_matrix_int(''dim=3'',product(box,dim=3))', &
'', &
'    endblock WITH_DIM', &
'', &
'    contains', &
'', &
'    subroutine print_matrix_int(title,arr)', &
'    implicit none', &
'', &
'    !@(#) print small 2d integer arrays in row-column format', &
'', &
'    character(len=*),intent(in)  :: title', &
'    integer,intent(in)           :: arr(:,:)', &
'    integer                      :: i', &
'    character(len=:),allocatable :: biggest', &
'', &
'       print all', &
'       print all, trim(title),'':('',shape(arr),'')''  ! print title', &
'       biggest=''           ''  ! make buffer to write integer into', &
'       ! find how many characters to use for integers', &
'       write(biggest,''(i0)'')ceiling(log10(real(maxval(abs(arr)))))+2', &
'       ! use this format to write a row', &
'       biggest=''(" > [",*(i''//trim(biggest)//'':,","))''', &
'       ! print one row of array at a time', &
'       do i=1,size(arr,dim=1)', &
'          write(*,fmt=biggest,advance=''no'')arr(i,:)', &
'          write(*,''(" ]")'')', &
'       enddo', &
'', &
'    end subroutine print_matrix_int', &
'', &
'    end program demo_product', &
'', &
'Results:', &
'', &
'    factorial of  1  is  1.000000', &
'    factorial of  2  is  2.000000', &
'    factorial of  3  is  6.000000', &
'    factorial of  4  is  24.00000', &
'    factorial of  5  is  120.0000', &
'    factorial of  6  is  720.0000', &
'    factorial of  7  is  5040.000', &
'    factorial of  8  is  40320.00', &
'    factorial of  9  is  362880.0', &
'    factorial of  10  is  3628800.', &
'', &
'     product of all elements 351000000', &
'     odd * even =', &
'     4875 * 72000 = 351000000', &
'', &
'    zero-sized array=> 1', &
'    all elements have a false mask=> 1', &
'', &
'    rect :( 2 3 )', &
'     > [  1,  2,  3 ]', &
'     > [  4,  5,  6 ]', &
'    product of columns= 4 10 18', &
'    product of rows= 6 120', &
'', &
'    box 1 :( 2 3 )', &
'     > [  1,  2,  3 ]', &
'     > [  4,  5,  6 ]', &
'', &
'    box 2 :( 2 3 )', &
'     > [  10,  20,  30 ]', &
'     > [  40,  50,  60 ]', &
'', &
'    box 3 :( 2 3 )', &
'     > [ -10, -20, -30 ]', &
'     > [ -40, -50, -60 ]', &
'', &
'    box 4 :( 2 3 )', &
'     > [   2,   4,   6 ]', &
'     > [   8,  10,  12 ]', &
'    no dim gives a scalar .1719927E+26', &
'', &
'    negative values :( 3 4 )', &
'     > [     1,     1,   400,     1 ]', &
'     > [     1,     1,  1000,     1 ]', &
'     > [     1,     1,  1800,     1 ]', &
'', &
'    dim=1 :( 3 4 )', &
'     > [     4,   400,   400,    16 ]', &
'     > [    10,  1000,  1000,    40 ]', &
'     > [    18,  1800,  1800,    72 ]', &
'', &
'    dim=2 :( 2 4 )', &
'     > [       6,    6000,   -6000,      48 ]', &
'     > [     120,  120000, -120000,     960 ]', &
'', &
'    dim=3 :( 2 3 )', &
'     > [    -200,   -3200,  -16200 ]', &
'     > [  -51200, -125000, -259200 ]', &
'', &
'STANDARD', &
'', &
'Fortran 95 and later', &
'', &
'SEE ALSO', &
'', &
'SUM(3), note that an element by element multiplication is done directly', &
'using the star character.', &
'', &
'fortran-lang intrinsic descriptions (@urbanjost)', &
'']

shortname="product"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('158','radix')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'RADIX(3) - [NUMERIC MODEL] Base of a model number (GFDL)', &
'', &
'SYNTAX', &
'', &
'    result = radix(x)', &
'', &
'DESCRIPTION', &
'', &
'RADIX(X) returns the base of the model representing the entity X.', &
'', &
'ARGUMENTS', &
'', &
'    X', &
'        Shall be of type _integer_ or _real_', &
'', &
'RETURNS', &
'', &
'The return value is a scalar of type _integer_ and of the default', &
'integer kind.', &
'', &
'EXAMPLES', &
'', &
'Sample program:', &
'', &
'    program demo_radix', &
'    implicit none', &
'       print *, "The radix for the default integer kind is", radix(0)', &
'       print *, "The radix for the default real kind is", radix(0.0)', &
'       print *, "The radix for the doubleprecsion real kind is", radix(0.0d0)', &
'    end program demo_radix', &
'', &
'Results:', &
'', &
'        The radix for the default integer kind is           2', &
'        The radix for the default real kind is           2', &
'        The radix for the doubleprecsion real kind is           2', &
'', &
'STANDARD', &
'', &
'Fortran 95 and later', &
'', &
'SEE ALSO', &
'', &
'DIGITS(3), EPSILON(3), EXPONENT(3), FRACTION(3), HUGE(3),', &
'MAXEXPONENT(3), MINEXPONENT(3), NEAREST(3), PRECISION(3), RANGE(3),', &
'RRSPACING(3), SCALE(3), SET_EXPONENT(3), SPACING(3), TINY(3)', &
'', &
'fortran-lang intrinsic descriptions', &
'']

shortname="radix"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('159','random_number')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'RANDOM_NUMBER(3) - [MATHEMATICS:RANDOM] Pseudo-random number (GFDL)', &
'', &
'SYNTAX', &
'', &
'       random_number(harvest)', &
'', &
'DESCRIPTION', &
'', &
'Returns a single pseudorandom number or an array of pseudorandom numbers', &
'from the uniform distribution over the range 0 <= x < 1.', &
'', &
'ARGUMENTS', &
'', &
'    HARVEST', &
'        Shall be a scalar or an array of type _real_.', &
'', &
'EXAMPLES', &
'', &
'Sample program:', &
'', &
'    program demo_random_number', &
'    use, intrinsic :: iso_fortran_env, only : dp=>real64', &
'    implicit none', &
'    integer, allocatable :: seed(:)', &
'    integer              :: n', &
'    integer              :: first,last', &
'    integer              :: i', &
'    integer              :: rand_int', &
'    integer,allocatable  :: count(:)', &
'    real(kind=dp)        :: rand_val', &
'       call random_seed(size = n)', &
'       allocate(seed(n))', &
'       call random_seed(get=seed)', &
'       first=1', &
'       last=10', &
'       allocate(count(last-first+1))', &
'       ! To have a discrete uniform distribution on the integers', &
'       ! [first, first+1, ..., last-1, last] carve the continuous', &
'       ! distribution up into last+1-first equal sized chunks,', &
'       ! mapping each chunk to an integer.', &
'       !', &
'       ! One way is:', &
'       !   call random_number(rand_val)', &
'       ! choose one from last-first+1 integers', &
'       !   rand_int = first + FLOOR((last+1-first)*rand_val)', &
'          count=0', &
'          ! generate a lot of random integers from 1 to 10 and count them.', &
'          ! with a large number of values you should get about the same', &
'          ! number of each value', &
'          do i=1,100000000', &
'             call random_number(rand_val)', &
'             rand_int=first+floor((last+1-first)*rand_val)', &
'             if(rand_int.ge.first.and.rand_int.le.last)then', &
'                count(rand_int)=count(rand_int)+1', &
'             else', &
'                write(*,*)rand_int,'' is out of range''', &
'             endif', &
'          enddo', &
'          write(*,''(i0,1x,i0)'')(i,count(i),i=1,size(count))', &
'    end program demo_random_number', &
'', &
'Results:', &
'', &
'       1 10003588', &
'       2 10000104', &
'       3 10000169', &
'       4 9997996', &
'       5 9995349', &
'       6 10001304', &
'       7 10001909', &
'       8 9999133', &
'       9 10000252', &
'       10 10000196', &
'', &
'STANDARD', &
'', &
'Fortran 95 and later', &
'', &
'SEE ALSO', &
'', &
'RANDOM_SEED(3)', &
'', &
'fortran-lang intrinsic descriptions', &
'']

shortname="random_number"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('160','random_seed')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'RANDOM_SEED(3) - [MATHEMATICS:RANDOM] Initialize a pseudo-random number', &
'sequence (GFDL)', &
'', &
'SYNTAX', &
'', &
'    call random_seed(size, put, get)', &
'', &
'DESCRIPTION', &
'', &
'Restarts or queries the state of the pseudorandom number generator used', &
'by random_number.', &
'', &
'If random_seed is called without arguments, it is seeded with random', &
'data retrieved from the operating system.', &
'', &
'ARGUMENTS', &
'', &
'    SIZE', &
'        (Optional) Shall be a scalar and of type default _integer_, with', &
'        INTENT(OUT). It specifies the minimum size of the arrays used', &
'        with the PUT and GET arguments.', &
'', &
'    PUT', &
'        (Optional) Shall be an array of type default _integer_ and rank', &
'        one. It is INTENT(IN) and the size of the array must be larger', &
'        than or equal to the number returned by the SIZE argument.', &
'', &
'    GET', &
'        (Optional) Shall be an array of type default _integer_ and rank', &
'        one. It is INTENT(OUT) and the size of the array must be larger', &
'        than or equal to the number returned by the SIZE argument.', &
'', &
'EXAMPLES', &
'', &
'Sample program:', &
'', &
'    program demo_random_seed', &
'    implicit none', &
'    integer, allocatable :: seed(:)', &
'    integer :: n', &
'', &
'       call random_seed(size = n)', &
'       allocate(seed(n))', &
'       call random_seed(get=seed)', &
'       write (*, *) seed', &
'', &
'    end program demo_random_seed', &
'', &
'Results:', &
'', &
'         -674862499 -1750483360  -183136071  -317862567   682500039', &
'         349459   344020729 -1725483289', &
'', &
'STANDARD', &
'', &
'Fortran 95 and later', &
'', &
'SEE ALSO', &
'', &
'RANDOM_NUMBER(3)', &
'', &
'fortran-lang intrinsic descriptions', &
'']

shortname="random_seed"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('161','range')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'RANGE(3) - [NUMERIC MODEL] Decimal exponent range of a real kind (GFDL)', &
'', &
'SYNTAX', &
'', &
'    result = range(x)', &
'', &
'DESCRIPTION', &
'', &
'RANGE(X) returns the decimal exponent range in the model of the type of', &
'X.', &
'', &
'ARGUMENTS', &
'', &
'    X', &
'        Shall be of type _real_ or _complex_.', &
'', &
'RETURNS', &
'', &
'The return value is of type _integer_ and of the default integer kind.', &
'', &
'EXAMPLES', &
'', &
'Sample program:', &
'', &
'    program demo_range', &
'    use,intrinsic :: iso_fortran_env, only : dp=>real64,sp=>real32', &
'    implicit none', &
'    real(kind=sp)    :: x(2)', &
'    complex(kind=dp) :: y', &
'       print *, precision(x), range(x)', &
'       print *, precision(y), range(y)', &
'    end program demo_range', &
'', &
'Results:', &
'', &
'                  6          37', &
'                 15         307', &
'', &
'STANDARD', &
'', &
'Fortran 95 and later', &
'', &
'SEE ALSO', &
'', &
'DIGITS(3), EPSILON(3), EXPONENT(3), FRACTION(3), HUGE(3),', &
'MAXEXPONENT(3), MINEXPONENT(3), NEAREST(3), PRECISION(3), RADIX(3),', &
'RRSPACING(3), SCALE(3), SET_EXPONENT(3), SPACING(3), TINY(3)', &
'', &
'fortran-lang intrinsic descriptions', &
'']

shortname="range"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('162','rank')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'RANK(3) - [ARRAY INQUIRY] Rank of a data object (GFDL)', &
'', &
'SYNTAX', &
'', &
'    result = rank(a)', &
'', &
'DESCRIPTION', &
'', &
'RANK(A) returns the rank of a scalar or array data object.', &
'', &
'ARGUMENTS', &
'', &
'    A', &
'        can be of any type', &
'', &
'RETURNS', &
'', &
'The return value is of type _integer_ and of the default integer kind.', &
'For arrays, their rank is returned; for scalars zero is returned.', &
'', &
'EXAMPLES', &
'', &
'Sample program:', &
'', &
'    program demo_rank', &
'    implicit none', &
'    integer :: a', &
'    real, allocatable :: b(:,:)', &
'    real  :: c(10,20,30)', &
'       print *, rank(a), rank(b), rank(c)', &
'    end program demo_rank', &
'', &
'Results:', &
'', &
'       0           2           3', &
'', &
'STANDARD', &
'', &
'TS 29113', &
'', &
'fortran-lang intrinsic descriptions', &
'']

shortname="rank"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('163','real')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'REAL(3) - [TYPE:NUMERIC] Convert to real type (GFDL)', &
'', &
'SYNTAX', &
'', &
'    result = real(x, kind)', &
'', &
'DESCRIPTION', &
'', &
'REAL(X, KIND) converts its argument X to a real type.', &
'', &
'ARGUMENTS', &
'', &
'    X', &
'        Shall be _integer_, _real_, or _complex_.', &
'', &
'    KIND', &
'        (Optional) An _integer_ initialization expression indicating the', &
'        kind parameter of the result.', &
'', &
'RETURNS', &
'', &
'These functions return a _real_ variable or array under the following', &
'rules:', &
'', &
'1.  REAL(x) is converted to a default _real_ type if X is an _integer_', &
'    or _real_ variable.', &
'', &
'2.  REAL(x) is converted to a real type with the kind type parameter of', &
'    X if X is a _complex_ variable.', &
'', &
'3.  REAL(X, KIND) is converted to a _real_ type with kind type parameter', &
'    KIND if X is a _complex_, _integer_, or _real_ variable.', &
'', &
'EXAMPLES', &
'', &
'Sample program:', &
'', &
'    program demo_real', &
'    use,intrinsic :: iso_fortran_env, only : dp=>real64', &
'    implicit none', &
'    complex              :: zr = (1.0, 2.0)', &
'    doubleprecision      :: xd=huge(3.0d0)', &
'    complex(kind=dp) :: zd=cmplx(4.0e0_dp,5.0e0_dp,kind=dp)', &
'', &
'       print *, real(zr), aimag(zr)', &
'       print *, dble(zd), aimag(zd)', &
'', &
'       write(*,*)xd,real(xd,kind=kind(0.0d0)),dble(xd)', &
'    end program demo_real', &
'', &
'Results:', &
'', &
'     1.00000000       2.00000000', &
'     4.0000000000000000       5.0000000000000000', &
'     1.7976931348623157E+308  1.7976931348623157E+308  1.7976931348623157E+308', &
'', &
'STANDARD', &
'', &
'FORTRAN 77 and later', &
'', &
'SEE ALSO', &
'', &
'DBLE(3), FLOAT(3)', &
'', &
'fortran-lang intrinsic descriptions', &
'']

shortname="real"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('164','repeat')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'REPEAT(3) - [CHARACTER] Repeated string concatenation (GFDL)', &
'', &
'SYNTAX', &
'', &
'    result = repeat(string, ncopies)', &
'', &
'       character(len=len(string)*ncopies) :: repeat', &
'       character(len=*),intent(in)        :: string', &
'       integer,intent(in)                 :: ncopies', &
'', &
'DESCRIPTION', &
'', &
'Concatenates NCOPIES copies of a string.', &
'', &
'ARGUMENTS', &
'', &
'    STRING', &
'        The input string to repeatedly generate. Shall be scalar and of', &
'        type _character_.', &
'', &
'    NCOPIES', &
'        Number of copies to make of _string_, greater than or equal to', &
'        zero (0). Shall be scalar and of type _integer_.', &
'', &
'RETURNS', &
'', &
'A new scalar of type _character_ built up from NCOPIES copies of STRING.', &
'', &
'EXAMPLES', &
'', &
'Sample program:', &
'', &
'    program demo_repeat', &
'    implicit none', &
'    integer :: i, j', &
'        write(*,''(a)'') repeat("^v", 36)         ! line break', &
'        write(*,''(a)'') repeat("_", 72)          ! line break', &
'        write(*,''(a)'') repeat("1234567890", 7)  ! number line', &
'        do i=80,0,-1 ! a simple progress bar', &
'            write(*,''(a)'',advance=''no'') &', &
'            & repeat("#", i)//repeat('' '',80-i)//char(13)', &
'            !do something slow', &
'        enddo', &
'    end program demo_repeat', &
'', &
'Results:', &
'', &
'       ^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v', &
'       ________________________________________________________________________', &
'       1234567890123456789012345678901234567890123456789012345678901234567890', &
'', &
'STANDARD', &
'', &
'Fortran 95 and later', &
'', &
'SEE ALSO', &
'', &
'Functions that perform operations on character strings:', &
'', &
'-   ELEMENTAL: ADJUSTL(3), ADJUSTR(3), INDEX(3), SCAN(3), VERIFY(3)', &
'', &
'-   NON-ELEMENTAL: LEN_TRIM(3), LEN(3), REPEAT(3), TRIM(3)', &
'', &
'fortran-lang intrinsic descriptions', &
'']

shortname="repeat"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('165','reshape')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'RESHAPE(3) - [ARRAY RESHAPE] Function to reshape an array (GFDL)', &
'', &
'SYNTAX', &
'', &
'    result = reshape(source, shape, pad, order)', &
'', &
'DESCRIPTION', &
'', &
'Reshapes array SOURCE to correspond to SHAPE. If necessary, the new', &
'array may be padded with elements from PAD or permuted as defined by', &
'ORDER.', &
'', &
'ARGUMENTS', &
'', &
'    SOURCE', &
'        an array of any type.', &
'', &
'    SHAPE', &
'        an array of rank one and type _integer_. Its values must be', &
'        positive or zero.', &
'', &
'    PAD', &
'        (Optional) an array of the same type as SOURCE.', &
'', &
'    ORDER', &
'        (Optional) an array of type _integer_ and the same shape as', &
'        SHAPE. Its values shall be a permutation of the numbers from 1', &
'        to n, where n is the size of SHAPE. If ORDER is absent, the', &
'        natural ordering shall be assumed.', &
'', &
'RETURNS', &
'', &
'The result is an array of shape SHAPE with the same type as SOURCE.', &
'', &
'EXAMPLES', &
'', &
'Sample program:', &
'', &
'    program demo_reshape', &
'    implicit none', &
'    integer :: i', &
'    integer, dimension(4) :: x=[(i,i=10,40,10)]', &
'    real :: xx(3,4)', &
'    real,allocatable :: v(:)', &
'        ! x is originally a vector with four elements', &
'        write(*,*) shape(x) ! what is the current shape of the array?', &
'        write(*,*) shape(reshape(x, [2, 2]))    ! prints "2 2"', &
'', &
'        ! pack any array into a vector', &
'        xx=1.0', &
'        v=reshape(xx,[size(xx)])', &
'        write(*,*)shape(v),ubound(v)', &
'    end program demo_reshape', &
'', &
'Results:', &
'', &
'                  4', &
'                  2           2', &
'                 12          12', &
'', &
'STANDARD', &
'', &
'Fortran 95 and later', &
'', &
'SEE ALSO', &
'', &
'SHAPE(3)', &
'', &
'fortran-lang intrinsic descriptions', &
'']

shortname="reshape"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('166','rrspacing')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'RRSPACING(3) - [MODEL_COMPONENTS] Reciprocal of the relative spacing', &
'(GFDL)', &
'', &
'SYNTAX', &
'', &
'    result = rrspacing(x)', &
'', &
'DESCRIPTION', &
'', &
'RRSPACING(X) returns the reciprocal of the relative spacing of model', &
'numbers near X.', &
'', &
'ARGUMENTS', &
'', &
'    X', &
'        Shall be of type _real_.', &
'', &
'RETURNS', &
'', &
'The return value is of the same type and kind as X. The value returned', &
'is equal to ABS(FRACTION(X)) * FLOAT(RADIX(X))**DIGITS(X).', &
'', &
'STANDARD', &
'', &
'Fortran 95 and later', &
'', &
'SEE ALSO', &
'', &
'DIGITS(3), EPSILON(3), EXPONENT(3), FRACTION(3), HUGE(3),', &
'MAXEXPONENT(3), MINEXPONENT(3), NEAREST(3), PRECISION(3), RADIX(3),', &
'RANGE(3), SCALE(3), SET_EXPONENT(3), SPACING(3), TINY(3)', &
'', &
'fortran-lang intrinsic descriptions', &
'']

shortname="rrspacing"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('167','same_type_as')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'SAME_TYPE_AS(3) - [STATE] Query dynamic types for equality (GFDL)', &
'', &
'SYNTAX', &
'', &
'    result = same_type_as(a, b)', &
'', &
'DESCRIPTION', &
'', &
'Query dynamic types for equality.', &
'', &
'ARGUMENTS', &
'', &
'    A', &
'        Shall be an object of extensible declared type or unlimited', &
'        polymorphic.', &
'', &
'    B', &
'        Shall be an object of extensible declared type or unlimited', &
'        polymorphic.', &
'', &
'RETURNS', &
'', &
'The return value is a scalar of type default logical. It is true if and', &
'only if the dynamic type of A is the same as the dynamic type of B.', &
'', &
'STANDARD', &
'', &
'Fortran 2003 and later', &
'', &
'SEE ALSO', &
'', &
'EXTENDS_TYPE_OF(3)', &
'', &
'fortran-lang intrinsic descriptions', &
'']

shortname="same_type_as"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('168','scale')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'SCALE(3) - [MODEL_COMPONENTS] Scale a real value by a whole power of the', &
'radix (GFDL)', &
'', &
'SYNTAX', &
'', &
'    result = scale(x, i)', &
'', &
'       real(kind=KIND),intent(in) :: x', &
'       integer,intent(in)         :: i', &
'', &
'DESCRIPTION', &
'', &
'SCALE(X,I) returns x * RADIX(X)**I.', &
'', &
'ARGUMENTS', &
'', &
'    X', &
'        The type of the argument shall be a _real_.', &
'', &
'    I', &
'        The type of the argument shall be a _integer_.', &
'', &
'RETURNS', &
'', &
'The return value is of the same type and kind as X. Its value is X *', &
'RADIX(X)**I.', &
'', &
'EXAMPLES', &
'', &
'Sample program:', &
'', &
'    program demo_scale', &
'    implicit none', &
'    real :: x = 178.1387e-4', &
'    integer :: i = 5', &
'       print *, scale(x,i), x*radix(x)**i', &
'    end program demo_scale', &
'', &
'Results:', &
'', &
'        0.570043862      0.570043862', &
'', &
'STANDARD', &
'', &
'Fortran 95 and later', &
'', &
'SEE ALSO', &
'', &
'DIGITS(3), EPSILON(3), EXPONENT(3), FRACTION(3), HUGE(3),', &
'MAXEXPONENT(3), MINEXPONENT(3), NEAREST(3), PRECISION(3), RADIX(3),', &
'RANGE(3), RRSPACING(3), SET_EXPONENT(3), SPACING(3), TINY(3)', &
'', &
'fortran-lang intrinsic descriptions', &
'']

shortname="scale"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('169','scan')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'SCAN(3) - [CHARACTER:SEARCH] Scan a string for the presence of a set of', &
'characters (GFDL)', &
'', &
'SYNTAX', &
'', &
'    result = scan(string, set[, back [, kind]])', &
'', &
'DESCRIPTION', &
'', &
'Scans a STRING for any of the characters in a SET of characters.', &
'', &
'If BACK is either absent or equals .FALSE., this function returns the', &
'position of the leftmost character of STRING that is in SET. If BACK', &
'equals .TRUE., the rightmost position is returned. If no character of', &
'SET is found in STRING, the result is zero.', &
'', &
'ARGUMENTS', &
'', &
'    STRING', &
'        Shall be of type _character_.', &
'', &
'    SET', &
'        Shall be of type _character_.', &
'', &
'    BACK', &
'        (Optional) shall be of type _logical_.', &
'', &
'    KIND', &
'        (Optional) An _integer_ initialization expression indicating the', &
'        kind parameter of the result.', &
'', &
'RETURNS', &
'', &
'The return value is of type _integer_ and of kind KIND. If KIND is', &
'absent, the return value is of default integer kind.', &
'', &
'EXAMPLES', &
'', &
'Sample program:', &
'', &
'    program demo_scan', &
'    implicit none', &
'       write(*,*) scan("fortran", "ao")          ! 2, found ''o''', &
'       write(*,*) scan("fortran", "ao", .true.)  ! 6, found ''a''', &
'       write(*,*) scan("fortran", "c++")         ! 0, found none', &
'    end program demo_scan', &
'', &
'Results:', &
'', &
'                  2', &
'                  6', &
'                  0', &
'', &
'STANDARD', &
'', &
'Fortran 95 and later, with KIND argument - Fortran 2003 and later', &
'', &
'SEE ALSO', &
'', &
'Functions that perform operations on character strings, return lengths', &
'of arguments, and search for certain arguments:', &
'', &
'-   ELEMENTAL: ADJUSTL(3), ADJUSTR(3), INDEX(3), SCAN(3), VERIFY(3)', &
'', &
'-   NONELEMENTAL: LEN_TRIM(3), LEN(3), REPEAT(3), TRIM(3)', &
'', &
'fortran-lang intrinsic descriptions', &
'']

shortname="scan"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('170','selected_char_kind')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'SELECTED_CHAR_KIND(3) - [KIND] Choose character kind such as "Unicode"', &
'(GFDL)', &
'', &
'SYNTAX', &
'', &
'    result = selected_char_kind(name)', &
'', &
'DESCRIPTION', &
'', &
'SELECTED_CHAR_KIND(NAME) returns the kind value for the character set', &
'named NAME, if a character set with such a name is supported, or -1', &
'otherwise. Currently, supported character sets include "ASCII" and', &
'"DEFAULT" (iwhich are equivalent), and "ISO_10646" (Universal Character', &
'Set, UCS-4) which is commonly known as "Unicode".', &
'', &
'ARGUMENTS', &
'', &
'    NAME', &
'        Shall be a scalar and of the default character type.', &
'', &
'EXAMPLES', &
'', &
'Sample program:', &
'', &
'    program demo_selected_char_kind', &
'    use iso_fortran_env', &
'    implicit none', &
'    integer, parameter :: ascii = selected_char_kind ("ascii")', &
'    integer, parameter :: ucs4  = selected_char_kind (''ISO_10646'')', &
'', &
'    character(kind=ascii, len=26) :: alphabet', &
'    character(kind=ucs4,  len=30) :: hello_world', &
'', &
'       alphabet = ascii_"abcdefghijklmnopqrstuvwxyz"', &
'       hello_world = ucs4_''Hello World and Ni Hao -- '' &', &
'                     // char (int (z''4F60''), ucs4)     &', &
'                     // char (int (z''597D''), ucs4)', &
'', &
'       write (*,*) alphabet', &
'', &
'       open (output_unit, encoding=''UTF-8'')', &
'       write (*,*) trim (hello_world)', &
'    end program demo_selected_char_kind', &
'', &
'Results:', &
'', &
'        abcdefghijklmnopqrstuvwxyz', &
'        Hello World and Ni Hao -- 你好', &
'', &
'STANDARD', &
'', &
'Fortran 2003 and later', &
'', &
'fortran-lang intrinsic descriptions', &
'']

shortname="selected_char_kind"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('171','selected_int_kind')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'SELECTED_INT_KIND(3) - [KIND] Choose integer kind (GFDL)', &
'', &
'SYNTAX', &
'', &
'    result = selected_int_kind(r)', &
'', &
'DESCRIPTION', &
'', &
'SELECTED_INT_KIND(R) return the kind value of the smallest integer type', &
'that can represent all values ranging from -10**R (exclusive) to 10**R', &
'(exclusive). If there is no integer kind that accommodates this range,', &
'selected_int_kind returns -1.', &
'', &
'ARGUMENTS', &
'', &
'    R', &
'        Shall be a scalar and of type _integer_.', &
'', &
'EXAMPLES', &
'', &
'Sample program:', &
'', &
'    program demo_selected_int_kind', &
'    implicit none', &
'    integer,parameter :: k5 = selected_int_kind(5)', &
'    integer,parameter :: k15 = selected_int_kind(15)', &
'    integer(kind=k5) :: i5', &
'    integer(kind=k15) :: i15', &
'', &
'        print *, huge(i5), huge(i15)', &
'', &
'        ! the following inequalities are always true', &
'        print *, huge(i5) >= 10_k5**5-1', &
'        print *, huge(i15) >= 10_k15**15-1', &
'    end program demo_selected_int_kind', &
'', &
'Results:', &
'', &
'         2147483647  9223372036854775807', &
'        T', &
'        T', &
'', &
'STANDARD', &
'', &
'Fortran 95 and later', &
'', &
'SEE ALSO', &
'', &
'AINT(3), ANINT(3), INT(3), NINT(3), CEILING(3), FLOOR(3)', &
'', &
'fortran-lang intrinsic descriptions', &
'']

shortname="selected_int_kind"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('172','selected_real_kind')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'SELECTED_REAL_KIND(3) - [KIND] Choose real kind (GFDL)', &
'', &
'SYNTAX', &
'', &
'    result = selected_real_kind(p, r, radix)', &
'', &
'DESCRIPTION', &
'', &
'SELECTED_REAL_KIND(P, R, RADIX) return the kind value of a real data', &
'type with decimal precision of at least P digits, exponent range of at', &
'least R, and with a radix of RADIX.', &
'', &
'ARGUMENTS', &
'', &
'    P', &
'        (Optional) shall be a scalar and of type _integer_.', &
'', &
'    R', &
'        (Optional) shall be a scalar and of type _integer_.', &
'', &
'    RADIX', &
'        (Optional) shall be a scalar and of type _integer_.', &
'', &
'Before FORTRAN 2008, at least one of the arguments R or P shall be', &
'present; since FORTRAN 2008, they are assumed to be zero if absent.', &
'', &
'RETURNS', &
'', &
'selected_real_kind returns the value of the kind type parameter of a', &
'real data type with decimal precision of at least P digits, a decimal', &
'exponent range of at least R, and with the requested RADIX. If the RADIX', &
'parameter is absent, real kinds with any radix can be returned. If more', &
'than one real data type meet the criteria, the kind of the data type', &
'with the smallest decimal precision is returned. If no real data type', &
'matches the criteria, the result is', &
'', &
'-   -1 if the processor does not support a real data type with a', &
'    precision greater than or equal to P, but the R and RADIX', &
'    requirements can be fulfilled', &
'', &
'    -   -2 if the processor does not support a real type with an', &
'        exponent range greater than or equal to R, but P and RADIX are', &
'        fulfillable', &
'', &
'    -   -3 if RADIX but not P and R requirements are fulfillable', &
'', &
'    -   -4 if RADIX and either P or R requirements are fulfillable', &
'', &
'    -   -5 if there is no real type with the given RADIX', &
'', &
'EXAMPLES', &
'', &
'Sample program:', &
'', &
'    program demo_selected_real_kind', &
'    implicit none', &
'    integer,parameter :: p6 = selected_real_kind(6)', &
'    integer,parameter :: p10r100 = selected_real_kind(10,100)', &
'    integer,parameter :: r400 = selected_real_kind(r=400)', &
'    real(kind=p6) :: x', &
'    real(kind=p10r100) :: y', &
'    real(kind=r400) :: z', &
'', &
'       print *, precision(x), range(x)', &
'       print *, precision(y), range(y)', &
'       print *, precision(z), range(z)', &
'    end program demo_selected_real_kind', &
'', &
'Results:', &
'', &
'                  6          37', &
'                 15         307', &
'                 18        4931', &
'', &
'STANDARD', &
'', &
'Fortran 95 and later; with RADIX - Fortran 2008 and later', &
'', &
'SEE ALSO', &
'', &
'PRECISION(3), RANGE(3), RADIX(3)', &
'', &
'fortran-lang intrinsic descriptions', &
'']

shortname="selected_real_kind"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('173','set_exponent')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'SET_EXPONENT(3) - [MODEL_COMPONENTS] Set the exponent of the model', &
'(GFDL)', &
'', &
'SYNTAX', &
'', &
'    result = set_exponent(x, i)', &
'', &
'DESCRIPTION', &
'', &
'SET_EXPONENT(X, I) returns the real number whose fractional part is that', &
'of X and whose exponent part is I.', &
'', &
'ARGUMENTS', &
'', &
'    X', &
'        Shall be of type _real_.', &
'', &
'    I', &
'        Shall be of type _integer_.', &
'', &
'RETURNS', &
'', &
'The return value is of the same type and kind as X. The real number', &
'whose fractional part is that that of X and whose exponent part if I is', &
'returned; it is FRACTION(X) * RADIX(X)**I.', &
'', &
'EXAMPLES', &
'', &
'Sample program:', &
'', &
'    program demo_setexp', &
'    implicit none', &
'    real :: x = 178.1387e-4', &
'    integer :: i = 17', &
'       print *, set_exponent(x, i), fraction(x) * radix(x)**i', &
'    end program demo_setexp', &
'', &
'Results:', &
'', &
'          74716.7891       74716.7891', &
'', &
'STANDARD', &
'', &
'Fortran 95 and later', &
'', &
'SEE ALSO', &
'', &
'DIGITS(3), EPSILON(3), EXPONENT(3), FRACTION(3), HUGE(3),', &
'MAXEXPONENT(3), MINEXPONENT(3), NEAREST(3), PRECISION(3), RADIX(3),', &
'RANGE(3), RRSPACING(3), SCALE(3), SPACING(3), TINY(3)', &
'', &
'fortran-lang intrinsic descriptions', &
'']

shortname="set_exponent"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('174','shape')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'SHAPE(3) - [ARRAY INQUIRY] Determine the shape of an array (GFDL)', &
'', &
'SYNTAX', &
'', &
'    result = shape(source, kind)', &
'', &
'DESCRIPTION', &
'', &
'Determines the shape of an array.', &
'', &
'ARGUMENTS', &
'', &
'    SOURCE', &
'        Shall be an array or scalar of any type. If SOURCE is a pointer', &
'        it must be associated and allocatable arrays must be allocated.', &
'', &
'    KIND', &
'        (Optional) An _integer_ initialization expression indicating the', &
'        kind parameter of the result.', &
'', &
'RETURNS', &
'', &
'An _integer_ array of rank one with as many elements as SOURCE has', &
'dimensions. The elements of the resulting array correspond to the extend', &
'of SOURCE along the respective dimensions. If SOURCE is a scalar, the', &
'result is the rank one array of size zero. If KIND is absent, the return', &
'value has the default integer kind otherwise the specified kind.', &
'', &
'EXAMPLES', &
'', &
'Sample program:', &
'', &
'    program demo_shape', &
'    implicit none', &
'    character(len=*),parameter :: all=''(*(g0,1x))''', &
'    integer, dimension(-1:1, -1:2) :: a', &
'       print all, ''shape of array='',shape(a)', &
'       print all, ''shape of constant='',shape(42)', &
'       print all, ''size of shape of constant='',size(shape(42))', &
'       print all, ''ubound of array='',ubound(a)', &
'       print all, ''lbound of array='',lbound(a)', &
'    end program demo_shape', &
'', &
'Results:', &
'', &
'       shape of array= 3 4', &
'       shape of constant=', &
'       size of shape of constant= 0', &
'       ubound of array= 1 2', &
'       lbound of array= -1 -1', &
'', &
'STANDARD', &
'', &
'Fortran 95 and later; with KIND argument Fortran 2003 and later', &
'', &
'SEE ALSO', &
'', &
'RESHAPE(3), SIZE(3)', &
'', &
'fortran-lang intrinsic descriptions', &
'']

shortname="shape"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('175','shifta')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'SHIFTA(3) - [BIT:SHIFT] shift bits right with fill (GFDL)', &
'', &
'SYNTAX', &
'', &
'    result = shifta(i, shift)', &
'', &
'DESCRIPTION', &
'', &
'Returns a value corresponding to I with all of the bits shifted right by', &
'SHIFT places. If the absolute value of SHIFT is greater than', &
'BIT_SIZE(I), the value is undefined. Bits shifted out from the right end', &
'are lost. The fill is arithmetic: the bits shifted in from the left end', &
'are equal to the leftmost bit, which in two''s complement representation', &
'is the sign bit.', &
'', &
'ARGUMENTS', &
'', &
'    I', &
'        The type shall be _integer_.', &
'', &
'    SHIFT', &
'        The type shall be _integer_.', &
'', &
'RETURNS', &
'', &
'The return value is of type _integer_ and of the same kind as I.', &
'', &
'STANDARD', &
'', &
'Fortran 2008 and later', &
'', &
'SEE ALSO', &
'', &
'SHIFTL(3), SHIFTR(3)', &
'', &
'fortran-lang intrinsic descriptions', &
'']

shortname="shifta"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('176','shiftl')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'SHIFTL(3) - [BIT:SHIFT] shift bits left (GFDL)', &
'', &
'SYNTAX', &
'', &
'    result = shiftl(i, shift)', &
'', &
'DESCRIPTION', &
'', &
'Returns a value corresponding to I with all of the bits shifted left by', &
'SHIFT places. If the absolute value of SHIFT is greater than', &
'BIT_SIZE(I), the value is undefined. Bits shifted out from the left end', &
'are lost, and bits shifted in from the right end are set to 0.', &
'', &
'ARGUMENTS', &
'', &
'    I', &
'        The type shall be _integer_.', &
'', &
'    SHIFT', &
'        The type shall be _integer_.', &
'', &
'RETURNS', &
'', &
'The return value is of type _integer_ and of the same kind as I.', &
'', &
'STANDARD', &
'', &
'Fortran 2008 and later', &
'', &
'SEE ALSO', &
'', &
'SHIFTA(3), SHIFTR(3)', &
'', &
'fortran-lang intrinsic descriptions', &
'']

shortname="shiftl"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('177','shiftr')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'SHIFTR(3) - [BIT:SHIFT] shift bits right (GFDL)', &
'', &
'SYNTAX', &
'', &
'    result = shiftr(i, shift)', &
'', &
'DESCRIPTION', &
'', &
'Returns a value corresponding to I with all of the bits shifted right by', &
'SHIFT places. If the absolute value of SHIFT is greater than', &
'BIT_SIZE(I), the value is undefined. Bits shifted out from the right end', &
'are lost, and bits shifted in from the left end are set to 0.', &
'', &
'ARGUMENTS', &
'', &
'    I', &
'        The type shall be _integer_.', &
'', &
'    SHIFT', &
'        The type shall be _integer_.', &
'', &
'RETURNS', &
'', &
'The return value is of type _integer_ and of the same kind as I.', &
'', &
'STANDARD', &
'', &
'Fortran 2008 and later', &
'', &
'SEE ALSO', &
'', &
'SHIFTA(3), SHIFTL(3)', &
'', &
'fortran-lang intrinsic descriptions', &
'']

shortname="shiftr"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('178','sign')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'SIGN(3) - [NUMERIC] Sign copying function (GFDL)', &
'', &
'SYNTAX', &
'', &
'    result = sign(a, b)', &
'', &
'DESCRIPTION', &
'', &
'SIGN(a,b) returns the value of A with the sign of B.', &
'', &
'ARGUMENTS', &
'', &
'    A', &
'        Shall be of type _integer_ or _real_', &
'', &
'    B', &
'        Shall be of the same type and kind as A', &
'', &
'RETURNS', &
'', &
'The kind of the return value is that of A and B. If B >= 0 then the', &
'result is ABS(A), else it is -ABS(A).', &
'', &
'EXAMPLES', &
'', &
'Sample program:', &
'', &
'    program demo_sign', &
'    implicit none', &
'       print *, sign(-12,1)', &
'       print *, sign(-12,0)', &
'       print *, sign(-12,-1)', &
'', &
'       print *, sign(-12.,1.)', &
'       print *, sign(-12.,0.)', &
'       print *, sign(-12.,-1.)', &
'    end program demo_sign', &
'', &
'Results:', &
'', &
'                 12', &
'                 12', &
'                -12', &
'          12.0000000', &
'          12.0000000', &
'         -12.0000000', &
'', &
'STANDARD', &
'', &
'FORTRAN 77 and later', &
'', &
'fortran-lang intrinsic descriptions', &
'']

shortname="sign"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('179','sin')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'SIN(3) - [MATHEMATICS:TRIGONOMETRIC] Sine function', &
'', &
'SYNTAX', &
'', &
'    result = sin(x)', &
'', &
'        elemental TYPE(kind=KIND) function sin(x)', &
'        TYPE(kind=KIND) :: x', &
'', &
'Where the returned value has the kind of the input value and TYPE may be', &
'_real_ or _complex_', &
'', &
'DESCRIPTION', &
'', &
'SIN(X) computes the sine of an angle given the size of the angle in', &
'radians.', &
'', &
'The sine of an angle in a right-angled triangle is the ratio of the', &
'length of the side opposite the given angle divided by the length of the', &
'hypotenuse.', &
'', &
'ARGUMENTS', &
'', &
'    X', &
'        The type shall be _real_ or _complex_ in radians.', &
'', &
'RETURNS', &
'', &
'    RESULT', &
'        The return value has the same type and kind as X.', &
'', &
'EXAMPLES', &
'', &
'Sample program:', &
'', &
'    program sample_sin', &
'    implicit none', &
'    real :: x = 0.0', &
'       x = sin(x)', &
'    end program sample_sin', &
'', &
'HAVERSINE FORMULA', &
'', &
'From the article on "Haversine formula" in Wikipedia:', &
'', &
'    The haversine formula is an equation important in navigation,', &
'    giving great-circle distances between two points on a sphere from', &
'    their longitudes and latitudes.', &
'', &
'So to show the great-circle distance between the Nashville International', &
'Airport (BNA) in TN, USA, and the Los Angeles International Airport', &
'(LAX) in CA, USA you would start with their latitude and longitude,', &
'commonly given as', &
'', &
'    BNA: N 36 degrees 7.2'',   W 86 degrees 40.2''', &
'    LAX: N 33 degrees 56.4'',  W 118 degrees 24.0''', &
'', &
'which converted to floating-point values in degrees is:', &
'', &
'         Latitude Longitude', &
'', &
'       - BNA', &
'         36.12, -86.67', &
'', &
'       - LAX', &
'         33.94, -118.40', &
'', &
'And then use the haversine formula to roughly calculate the distance', &
'along the surface of the Earth between the locations:', &
'', &
'Sample program:', &
'', &
'    program demo_sin', &
'    implicit none', &
'    real :: d', &
'        d = haversine(36.12,-86.67, 33.94,-118.40) ! BNA to LAX', &
'        print ''(A,F9.4,A)'', ''distance: '',d,'' km''', &
'    contains', &
'    function haversine(latA,lonA,latB,lonB) result (dist)', &
'    !', &
'    ! calculate great circle distance in kilometers', &
'    ! given latitude and longitude in degrees', &
'    !', &
'    real,intent(in) :: latA,lonA,latB,lonB', &
'    real :: a,c,dist,delta_lat,delta_lon,lat1,lat2', &
'    real,parameter :: radius = 6371 ! mean earth radius in kilometers,', &
'    ! recommended by the International Union of Geodesy and Geophysics', &
'', &
'    ! generate constant pi/180', &
'    real, parameter :: deg_to_rad = atan(1.0)/45.0', &
'       delta_lat = deg_to_rad*(latB-latA)', &
'       delta_lon = deg_to_rad*(lonB-lonA)', &
'       lat1 = deg_to_rad*(latA)', &
'       lat2 = deg_to_rad*(latB)', &
'       a = (sin(delta_lat/2))**2 + &', &
'              & cos(lat1)*cos(lat2)*(sin(delta_lon/2))**2', &
'       c = 2*asin(sqrt(a))', &
'       dist = radius*c', &
'    end function haversine', &
'    end program demo_sin', &
'', &
'Results:', &
'', &
'        distance: 2886.4446 km', &
'', &
'STANDARD', &
'', &
'FORTRAN 77 and later', &
'', &
'SEE ALSO', &
'', &
'-   Wikipedia:sine and cosine', &
'', &
'ASIN(3), COS(3), TAN(3)', &
'', &
'fortran-lang intrinsic descriptions (@urbanjost)', &
'']

shortname="sin"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('180','sinh')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'SINH(3) - [MATHEMATICS:TRIGONOMETRIC] Hyperbolic sine function', &
'', &
'SYNTAX', &
'', &
'    result = sinh(x)', &
'', &
'        elemental TYPE(kind=KIND) function sinh(x)', &
'        TYPE(kind=KIND) :: x', &
'', &
'Where the returned value has the kind of the input value and TYPE may be', &
'_real_ or _complex_', &
'', &
'DESCRIPTION', &
'', &
'SINH(X) computes the hyperbolic sine of X.', &
'', &
'The hyperbolic sine of x is defined mathematically as:', &
'', &
'SINH(X) = (EXP(X) - EXP(-X)) / 2.0', &
'', &
'If X is of type _complex_ its imaginary part is regarded as a value in', &
'radians.', &
'', &
'ARGUMENTS', &
'', &
'    X', &
'        The type shall be _real_ or _complex_.', &
'', &
'RETURNS', &
'', &
'The return value has same type and kind as X.', &
'', &
'EXAMPLES', &
'', &
'Sample program:', &
'', &
'    program demo_sinh', &
'    use, intrinsic :: iso_fortran_env, only : &', &
'    & real_kinds, real32, real64, real128', &
'    implicit none', &
'    real(kind=real64) :: x = - 1.0_real64', &
'    real(kind=real64) :: nan, inf', &
'    character(len=20) :: line', &
'', &
'       print *, sinh(x)', &
'       print *, (exp(x)-exp(-x))/2.0', &
'', &
'       ! sinh(3) is elemental and can handle an array', &
'       print *, sinh([x,2.0*x,x/3.0])', &
'', &
'       ! a NaN input returns NaN', &
'       line=''NAN''', &
'       read(line,*) nan', &
'       print *, sinh(nan)', &
'', &
'       ! a Inf input returns Inf', &
'       line=''Infinity''', &
'       read(line,*) inf', &
'       print *, sinh(inf)', &
'', &
'       ! an overflow returns Inf', &
'       x=huge(0.0d0)', &
'       print *, sinh(x)', &
'', &
'    end program demo_sinh', &
'', &
'Results:', &
'', &
'      -1.1752011936438014', &
'      -1.1752011936438014', &
'      -1.1752011936438014       -3.6268604078470190      -0.33954055725615012', &
'                           NaN', &
'                      Infinity', &
'                      Infinity', &
'', &
'STANDARD', &
'', &
'Fortran 95 and later, for a complex argument Fortran 2008 or later', &
'', &
'SEE ALSO', &
'', &
'-   Wikipedia:hyperbolic functions', &
'', &
'ASINH(3)', &
'', &
'fortran-lang intrinsic descriptions', &
'']

shortname="sinh"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('181','size')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'SIZE(3) - [ARRAY INQUIRY] Determine the size of an array (GFDL)', &
'', &
'SYNTAX', &
'', &
'    result = size(array, dim, kind)', &
'', &
'DESCRIPTION', &
'', &
'Determine the extent of ARRAY along a specified dimension DIM, or the', &
'total number of elements in ARRAY if DIM is absent.', &
'', &
'ARGUMENTS', &
'', &
'    ARRAY', &
'        be an array of any type. If ARRAY is a pointer it must be', &
'        associated and allocatable arrays must be allocated.', &
'', &
'    DIM', &
'        shall be a scalar of type _integer_ and its value shall be in', &
'        the range from 1 to n, where n equals the rank of ARRAY.', &
'', &
'    KIND', &
'        An _integer_ initialization expression indicating the kind', &
'        parameter of the result.', &
'', &
'RETURNS', &
'', &
'The return value is of type _integer_ and of kind KIND. If KIND is', &
'absent, the return value is of default _integer_ kind.', &
'', &
'EXAMPLES', &
'', &
'Sample program:', &
'', &
'    program demo_size', &
'    implicit none', &
'    integer :: i, j', &
'    integer :: arr(0:2,-5:5)=reshape([(((i-1)*11+j,i=1,3),j=1,11)],[3,11])', &
'       write(*,*) ''SIZE of simple one-dimensional array='', &', &
'       & size([ 11, 22, 33 ])    ! 3', &
'', &
'       write(*,*)''body''', &
'       write(*,*)''SHAPE(arr)       :'',shape(arr)', &
'       write(*,*)''SIZE(arr)        :'',size(arr)', &
'       write(*,*)''SIZE(arr,DIM=1)  :'',size(arr,dim=1)', &
'       write(*,*)''SIZE(arr,DIM=2)  :'',size(arr,dim=2)', &
'       write(*,*)''note lower bound is not "1"''', &
'       write(*,*)''LBOUND(arr)      :'',lbound(arr)', &
'       write(*,*)''UBOUND(arr)      :'',ubound(arr)', &
'       write(*,*)''LBOUND(arr,DIM=1):'',lbound(arr,dim=1)', &
'       write(*,*)''UBOUND(arr,DIM=1):'',ubound(arr,dim=1)', &
'       write(*,*)''LBOUND(arr,DIM=2):'',lbound(arr,dim=2)', &
'       write(*,*)''UBOUND(arr,DIM=2):'',ubound(arr,dim=2)', &
'', &
'       call interfaced(arr,arr)', &
'       call nointerface(arr)', &
'    contains', &
'', &
'    subroutine interfaced(arr,arr2)', &
'    integer,intent(in)  :: arr(:,:)', &
'    integer,intent(in)  :: arr2(2,*)', &
'       !', &
'       write(*,*)''interfaced assumed-shape arr2ay''', &
'       !', &
'       ! source argument of shape intrinsic at (1) must not be', &
'       ! an assumed size array', &
'       !!write(*,*)''SHAPE(arr2)       :'',shape(arr2)', &
'       ! The upper bound in the last dimension must appear in the reference', &
'       ! to the assumed size array    arr2    at (1)', &
'       !!write(*,*)''SIZE(arr2)        :'',size(arr2)', &
'       write(*,*)''SIZE(arr2,DIM=1)  :'',size(arr2,dim=1)', &
'       !    dim    argument of    size    intrinsic at (1) is not', &
'       !a valid dimension index', &
'       !!write(*,*)''SIZE(arr2,DIM=2)  :'',size(arr2,dim=2)', &
'       write(*,*)''note lower bound is "1"''', &
'       write(*,*)''LBOUND(arr2)      :'',lbound(arr2)', &
'       write(*,*)''LBOUND(arr2)      :'',lbound(arr2)', &
'       ! The upper bound in the last dimension must appear in the', &
'       ! reference to the assumed size array    arr2    at (1)', &
'       !!write(*,*)''UBOUND(arr2)      :'',ubound(arr2)', &
'       write(*,*)''LBOUND(arr2,DIM=1):'',lbound(arr2,dim=1)', &
'       write(*,*)''UBOUND(arr2,DIM=1):'',ubound(arr2,dim=1)', &
'       write(*,*)''LBOUND(arr2,DIM=2):'',lbound(arr2,dim=2)', &
'       !    dim    argument of    ubound    intrinsic at (1) is not', &
'       ! a valid dimension index', &
'       !!write(*,*)''UBOUND(arr2,DIM=2):'',ubound(arr2,dim=2)', &
'       !', &
'       write(*,*)''interfaced''', &
'       !', &
'       write(*,*)''SHAPE(arr)       :'',shape(arr)', &
'       write(*,*)''SIZE(arr)        :'',size(arr)', &
'       write(*,*)''SIZE(arr,DIM=1)  :'',size(arr,dim=1)', &
'       write(*,*)''SIZE(arr,DIM=2)  :'',size(arr,dim=2)', &
'       write(*,*)''note lower bound is "1"''', &
'       write(*,*)''LBOUND(arr)      :'',lbound(arr)', &
'       write(*,*)''LBOUND(arr)      :'',lbound(arr)', &
'       write(*,*)''UBOUND(arr)      :'',ubound(arr)', &
'       write(*,*)''LBOUND(arr,DIM=1):'',lbound(arr,dim=1)', &
'       write(*,*)''UBOUND(arr,DIM=1):'',ubound(arr,dim=1)', &
'       write(*,*)''LBOUND(arr,DIM=2):'',lbound(arr,dim=2)', &
'       write(*,*)''UBOUND(arr,DIM=2):'',ubound(arr,dim=2)', &
'       !', &
'    end subroutine interfaced', &
'    !!', &
'    ! NOTE: If NOINTERFACE(3) had an assumed-shape argument with :', &
'    !       for dimensions it could only be properly called with', &
'    !       an explicit interface', &
'    !!', &
'    subroutine nointerface(arr)', &
'    integer,intent(in) :: arr(3,*)', &
'       write(*,*)''nointerface''', &
'     ! SHAPE(3) CANNOT BE USED ON AN ASSUMED SIZE ARRAY', &
'     !!write(*,*)''SHAPE(arr)       :'',shape(arr)', &
'     !!write(*,*)''SIZE(arr)        :'',size(arr)', &
'       write(*,*)''SIZE(arr,DIM=1)  :'',size(arr,dim=1)', &
'     ! CANNOT DETERMINE SIZE OF ASSUMED SIZE ARRAY LAST DIMENSION', &
'     !!write(*,*)''SIZE(arr,DIM=2)  :'',size(arr,dim=2)', &
'       write(*,*)''note lower bound is "1"''', &
'       write(*,*)''LBOUND(arr)      :'',lbound(arr)', &
'     !!write(*,*)''UBOUND(arr)      :'',ubound(arr)', &
'       write(*,*)''LBOUND(arr,DIM=1):'',lbound(arr,dim=1)', &
'       write(*,*)''UBOUND(arr,DIM=1):'',ubound(arr,dim=1)', &
'       write(*,*)''LBOUND(arr,DIM=2):'',lbound(arr,dim=2)', &
'     !!write(*,*)''UBOUND(arr,DIM=2):'',ubound(arr,dim=2)', &
'    end subroutine nointerface', &
'    !!', &
'    end program demo_size', &
'', &
'Results:', &
'', &
'        SIZE of simple one-dimensional array=           3', &
'        body', &
'        SHAPE(arr)       :           3          11', &
'        SIZE(arr)        :          33', &
'        SIZE(arr,DIM=1)  :           3', &
'        SIZE(arr,DIM=2)  :          11', &
'        note lower bound is not "1"', &
'        LBOUND(arr)      :           0          -5', &
'        UBOUND(arr)      :           2           5', &
'        LBOUND(arr,DIM=1):           0', &
'        UBOUND(arr,DIM=1):           2', &
'        LBOUND(arr,DIM=2):          -5', &
'        UBOUND(arr,DIM=2):           5', &
'        interfaced assumed-shape arr2ay', &
'        SIZE(arr2,DIM=1)  :           2', &
'        note lower bound is "1"', &
'        LBOUND(arr2)      :           1           1', &
'        LBOUND(arr2)      :           1           1', &
'        LBOUND(arr2,DIM=1):           1', &
'        UBOUND(arr2,DIM=1):           2', &
'        LBOUND(arr2,DIM=2):           1', &
'        interfaced', &
'        SHAPE(arr)       :           3          11', &
'        SIZE(arr)        :          33', &
'        SIZE(arr,DIM=1)  :           3', &
'        SIZE(arr,DIM=2)  :          11', &
'        note lower bound is "1"', &
'        LBOUND(arr)      :           1           1', &
'        LBOUND(arr)      :           1           1', &
'        UBOUND(arr)      :           3          11', &
'        LBOUND(arr,DIM=1):           1', &
'        UBOUND(arr,DIM=1):           3', &
'        LBOUND(arr,DIM=2):           1', &
'        UBOUND(arr,DIM=2):          11', &
'        nointerface', &
'        SIZE(arr,DIM=1)  :           3', &
'        note lower bound is "1"', &
'        LBOUND(arr)      :           1           1', &
'        LBOUND(arr,DIM=1):           1', &
'        UBOUND(arr,DIM=1):           3', &
'        LBOUND(arr,DIM=2):           1', &
'', &
'STANDARD', &
'', &
'Fortran 95 and later, with KIND argument - Fortran 2003 and later', &
'', &
'SEE ALSO', &
'', &
'SHAPE(3), [RESHAPE(3)])(RESHAPE)', &
'', &
'fortran-lang intrinsic descriptions', &
'']

shortname="size"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('182','spacing')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'SPACING(3) - [MODEL_COMPONENTS] Smallest distance between two numbers of', &
'a given type (GFDL)', &
'', &
'SYNTAX', &
'', &
'    result = spacing(x)', &
'', &
'DESCRIPTION', &
'', &
'Determines the distance between the argument X and the nearest adjacent', &
'number of the same type.', &
'', &
'ARGUMENTS', &
'', &
'    X', &
'        Shall be of type _real_.', &
'', &
'RETURNS', &
'', &
'The result is of the same type as the input argument X.', &
'', &
'EXAMPLES', &
'', &
'Sample program:', &
'', &
'    program demo_spacing', &
'    implicit none', &
'    integer, parameter :: sgl = selected_real_kind(p=6, r=37)', &
'    integer, parameter :: dbl = selected_real_kind(p=13, r=200)', &
'', &
'       write(*,*) spacing(1.0_sgl)      ! "1.1920929e-07"          on i686', &
'       write(*,*) spacing(1.0_dbl)      ! "2.220446049250313e-016" on i686', &
'    end program demo_spacing', &
'', &
'Results:', &
'', &
'          1.19209290E-07', &
'          2.2204460492503131E-016', &
'', &
'STANDARD', &
'', &
'Fortran 95 and later', &
'', &
'SEE ALSO', &
'', &
'DIGITS(3), EPSILON(3), EXPONENT(3), FRACTION(3), HUGE(3),', &
'MAXEXPONENT(3), MINEXPONENT(3), NEAREST(3), PRECISION(3), RADIX(3),', &
'RANGE(3), RRSPACING(3), SCALE(3), SET_EXPONENT(3), TINY(3)', &
'', &
'fortran-lang intrinsic descriptions', &
'']

shortname="spacing"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('183','spread')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'SPREAD(3) - [ARRAY CONSTRUCTION] Add a dimension to an array (GFDL)', &
'', &
'SYNTAX', &
'', &
'    result = spread(source, dim, ncopies)', &
'', &
'      TYPE(kind=KIND) function spread(source, dim, ncopies)', &
'', &
'       TYPE(kind=KIND)    :: source(..)', &
'       integer,intent(in) :: dim', &
'       integer,intent(in) :: ncopies', &
'', &
'DESCRIPTION', &
'', &
'Replicates a SOURCE array NCOPIES times along a specified dimension DIM.', &
'', &
'If SOURCE is scalar, the shape of the result is (MAX (NCOPIES, 0)). and', &
'each element of the result has a value equal to SOURCE.', &
'', &
'ARGUMENTS', &
'', &
'    SOURCE', &
'        Shall be a scalar or an array of any type and a rank less than', &
'        fifteen.', &
'', &
'    DIM', &
'        Shall be a scalar of type _integer_ with a value in the range', &
'        from 1 to N+1, where N equals the rank of SOURCE.', &
'', &
'    NCOPIES', &
'        Shall be a scalar of type _integer_.', &
'', &
'RETURNS', &
'', &
'The result is an array of the same type as SOURCE and has rank N+1 where', &
'N equals the rank of SOURCE.', &
'', &
'EXAMPLES', &
'', &
'Sample program:', &
'', &
'    program demo_spread', &
'    implicit none', &
'    integer :: a = 1, b(2) = [ 1, 2 ]', &
'', &
'       write(*,*) spread(a, 1, 2)            ! "1 1"', &
'       write(*,*) spread(b, 1, 2)            ! "1 1 2 2"', &
'', &
'    end program demo_spread', &
'', &
'    program example_spread', &
'    !  Author:', &
'    !    John Burkardt, 03 July 2006', &
'    implicit none', &
'         !', &
'    integer ( kind = 4 ) a1(4,3)', &
'    integer ( kind = 4 ) a2(3,4)', &
'    integer i', &
'    integer ( kind = 4 ) s', &
'    integer ( kind = 4 ) v(4)', &
'         !', &
'         write ( *, ''(a)'' ) '' ''', &
'         write ( *, ''(a)'' ) ''TEST_SPREAD''', &
'         write ( *, ''(a)'' ) ''  SPREAD is a FORTRAN90 function which replicates''', &
'         write ( *, ''(a)'' ) ''  an array by adding a dimension.''', &
'         write ( *, ''(a)'' ) '' ''', &
'         !', &
'         s = 99', &
'         !', &
'         write ( *, ''(a,i6)'' ) ''  Suppose we have a scalar S = '', s', &
'         write ( *, ''(a)'' ) '' ''', &
'         !', &
'         v = spread ( s, 1, 4 )', &
'         !', &
'         write ( *, ''(a)'' ) ''  V = spread ( s, 1, 4 )''', &
'         write ( *, ''(a)'' ) '' ''', &
'         write ( *, ''(a)'' ) ''  adds a new dimension (1) of extent 4''', &
'         write ( *, ''(a)'' ) '' ''', &
'         write ( *, ''(4i6)'' ) v(1:4)', &
'         write ( *, ''(a)'' ) '' ''', &
'         write ( *, ''(a)'' ) ''  Now first reset V to (1,2,3,4)''', &
'         v = [ 1, 2, 3, 4 ]', &
'         !', &
'         a1 = spread ( v, 2, 3 )', &
'         !', &
'         write ( *, ''(a)'' ) '' ''', &
'         write ( *, ''(a)'' ) ''  A1 = spread ( v, 2, 3 )''', &
'         write ( *, ''(a)'' ) '' ''', &
'         write ( *, ''(a)'' ) ''  adds a new dimension (2) of extent 3''', &
'         write ( *, ''(a)'' ) '' ''', &
'         do i = 1, 4', &
'           write ( *, ''(3i6)'' ) a1(i,1:3)', &
'         end do', &
'         !', &
'         a2 = spread ( v, 1, 3 )', &
'         !', &
'         write ( *, ''(a)'' ) '' ''', &
'         write ( *, ''(a)'' ) ''  A2 = spread ( v, 1, 3 )''', &
'         write ( *, ''(a)'' ) '' ''', &
'         write ( *, ''(a)'' ) ''  adds a new dimension (1) of extent 3''', &
'         write ( *, ''(a)'' ) '' ''', &
'         do i = 1, 3', &
'           write ( *, ''(4i6)'' ) a2(i,1:4)', &
'         end do', &
'    end program example_spread', &
'', &
'STANDARD', &
'', &
'Fortran 95 and later', &
'', &
'SEE ALSO', &
'', &
'PACK(3), UNPACK(3), MERGE(3), PACK(3), UNPACK(3)', &
'', &
'fortran-lang intrinsic descriptions', &
'']

shortname="spread"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('184','sqrt')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'SQRT(3) - [MATHEMATICS] Square-root function', &
'', &
'SYNTAX', &
'', &
'    result = sqrt(x)', &
'', &
'       TYPE(kind=KIND) elemental function sqrt(x) result(value)', &
'       TYPE(kind=KIND),intent(in) :: x', &
'       TYPE(kind=KIND) :: value', &
'', &
'Where TYPE may be _real_ or _complex_ and KIND may be any kind valid for', &
'the declared type.', &
'', &
'DESCRIPTION', &
'', &
'SQRT(X) computes the principal square root of X.', &
'', &
'In mathematics, a square root of a number X is a number Y such that', &
'__y*y = x__.', &
'', &
'The number whose square root is being considered is known as the', &
'_radicand_.', &
'', &
'Every nonnegative number _x_ has two square roots of the same unique', &
'magnitude, one positive and one negative. The nonnegative square root is', &
'called the principal square root.', &
'', &
'The principal square root of 9 is 3, for example, even though (-3)*(-3)', &
'is also 9.', &
'', &
'A _real_, _radicand_ must be positive.', &
'', &
'Square roots of negative numbers are a special case of complex numbers,', &
'where the components of the _radicand_ need not be positive in order to', &
'have a valid square root.', &
'', &
'ARGUMENTS', &
'', &
'    X', &
'        If X is real its value must be greater than or equal to zero.', &
'        The type shall be _real_ or _complex_.', &
'', &
'RETURNS', &
'', &
'The return value is of type _real_ or _complex_. The kind type parameter', &
'is the same as X.', &
'', &
'EXAMPLES', &
'', &
'Sample program:', &
'', &
'    program demo_sqrt', &
'    use, intrinsic :: iso_fortran_env, only : real_kinds, &', &
'     & real32, real64, real128', &
'    implicit none', &
'    real(kind=real64) :: x, x2', &
'    complex :: z, z2', &
'', &
'       x = 2.0_real64', &
'       z = (1.0, 2.0)', &
'       write(*,*)x,z', &
'', &
'       x2 = sqrt(x)', &
'       z2 = sqrt(z)', &
'       write(*,*)x2,z2', &
'', &
'       x2 = x**0.5', &
'       z2 = z**0.5', &
'       write(*,*)x2,z2', &
'', &
'    end program demo_sqrt', &
'', &
'Results:', &
'', &
'      2.0000000000000000    (1.00000000,2.00000000)', &
'      1.4142135623730951    (1.27201962,0.786151350)', &
'      1.4142135623730951    (1.27201962,0.786151350)', &
'', &
'STANDARD', &
'', &
'FORTRAN 77 and later', &
'', &
'fortran-lang intrinsic descriptions (@urbanjost)', &
'']

shortname="sqrt"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('185','storage_size')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'STORAGE_SIZE(3) - [BIT:INQUIRY] Storage size in bits (GFDL)', &
'', &
'SYNTAX', &
'', &
'    result = storage_size(a, kind)', &
'', &
'DESCRIPTION', &
'', &
'Returns the storage size of argument A in bits.', &
'', &
'ARGUMENTS', &
'', &
'    A', &
'        Shall be a scalar or array of any type.', &
'', &
'    KIND', &
'        (Optional) shall be a scalar integer constant expression.', &
'', &
'RETURNS', &
'', &
'The result is a scalar integer with the kind type parameter specified by', &
'KIND (or default integer type if KIND is missing). The result value is', &
'the size expressed in bits for an element of an array that has the', &
'dynamic type and type parameters of A.', &
'', &
'EXAMPLES', &
'', &
'Sample program', &
'', &
'    program demo_storage_size', &
'    implicit none', &
'       write(*,*)''size of integer       '',storage_size(0)', &
'       write(*,*)''size of real          '',storage_size(0.0)', &
'       write(*,*)''size of logical       '',storage_size(.true.)', &
'       write(*,*)''size of complex       '',storage_size((0.0,0.0))', &
'       write(*,*)''size of integer array '',storage_size([0,1,2,3,4,5,6,7,8,9])', &
'    end program demo_storage_size', &
'', &
'Results:', &
'', &
'        size of integer                 32', &
'        size of real                    32', &
'        size of logical                 32', &
'        size of complex                 64', &
'        size of integer array           32', &
'', &
'STANDARD', &
'', &
'Fortran 2008 and later', &
'', &
'SEE ALSO', &
'', &
'C_SIZEOF(3)', &
'', &
'fortran-lang intrinsic descriptions', &
'']

shortname="storage_size"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('186','sum')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'SUM(3) - [ARRAY REDUCTION] sum the elements of an array (GFDL)', &
'', &
'SYNTAX', &
'', &
'       result = sum(array[, mask])', &
'       result = sum(array, dim[, mask])', &
'', &
'DESCRIPTION', &
'', &
'Adds the elements of ARRAY along dimension DIM if the corresponding', &
'element in MASK is TRUE.', &
'', &
'ARGUMENTS', &
'', &
'    ARRAY', &
'        Shall be an array of type _integer_, _real_ or _complex_.', &
'', &
'    DIM', &
'        (Optional) shall be a scalar of type _integer_ with a value in', &
'        the range from 1 to n, where n equals the rank of ARRAY.', &
'', &
'    MASK', &
'        (Optional) shall be of type _logical_ and either be a scalar or', &
'        an array of the same shape as ARRAY.', &
'', &
'RETURNS', &
'', &
'The result is of the same type as ARRAY.', &
'', &
'If DIM(3) is absent, a scalar with the sum of all elements in ARRAY is', &
'returned. Otherwise, an array of rank n-1, where n equals the rank of', &
'ARRAY, and a shape similar to that of ARRAY with dimension DIM dropped', &
'is returned.', &
'', &
'EXAMPLES', &
'', &
'Sample program:', &
'', &
'    program simple_sum', &
'    implicit none', &
'    integer :: x(5) = [ 1, 2, 3, 4 ,5 ]', &
'       print *, sum(x)                        ! all elements, sum = 15', &
'       print *, sum(x, mask=mod(x, 2)==1)     ! odd elements, sum = 9', &
'    end program simple_sum', &
'', &
'Demonstrate Fortran 90 SUM function with MASK option', &
'', &
'    program demo_sum', &
'    ! John Mahaffy  2/16/96', &
'    implicit none', &
'    integer nd, ndh, nduh, j', &
'    parameter (nd=10,ndh=nd/2, nduh=nd-ndh)', &
'    real csum, cpsum, cbpsum', &
'    real, dimension(nd):: c=[(j, j=-1,nd-2)], b', &
'    data b/ndh*-1.0, nduh*2.0/', &
'       csum= sum(c(1:nd))', &
'       cpsum= sum (c(1:nd), mask=c.gt.0)', &
'       cbpsum= sum(c(1:nd), mask=b.gt.0.0)', &
'       print *, ''Sum of all elements in c = '' , csum', &
'       print *, ''Sum of Positive elements in c = '', cpsum', &
'       print *, ''Sum of elements in c when corresponding elements in b>0'', &', &
'       & '' ='', cbpsum', &
'    end program demo_sum', &
'', &
'Results:', &
'', &
'     Sum of all elements in c =    35.0000000', &
'     Sum of Positive elements in c =    36.0000000', &
'     Sum of elements in c when corresponding elements in b>0 =   30.0000000', &
'', &
'STANDARD', &
'', &
'Fortran 95 and later', &
'', &
'SEE ALSO', &
'', &
'intrinsics', &
'', &
'fortran-lang intrinsic descriptions', &
'']

shortname="sum"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('187','system_clock')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'SYSTEM_CLOCK(3) - [SYSTEM:TIME] Return numeric data from a real-time', &
'clock. (GFDL)', &
'', &
'SYNTAX', &
'', &
'    subroutine system_clock(count, count_rate, count_max)', &
'', &
'       integer,intent(out),optional  :: count', &
'       integer,intent(out),optional  :: count_rate', &
'        ! or !', &
'       real,intent(out),optional     :: count_rate', &
'       integer,intent(out),optional  :: count_max', &
'', &
'DESCRIPTION', &
'', &
'SYSTEM_CLOCK lets you measure durations of time with the precision of', &
'the smallest time increment generally available on a system by returning', &
'processor-dependent values based on the current value of the processor', &
'clock. The CLOCK value is incremented by one for each clock count until', &
'the value COUNT_MAX is reached and is then reset to zero at the next', &
'count. CLOCK therefore is a modulo value that lies in the range 0 TO', &
'COUNT_MAX. COUNT_RATE and COUNT_MAX are assumed constant (even though', &
'CPU rates can vary on a single platform).', &
'', &
'COUNT_RATE is system dependent and can vary depending on the kind of the', &
'arguments.', &
'', &
'If there is no clock, or querying the clock fails, COUNT is set to', &
'-HUGE(COUNT), and COUNT_RATE and COUNT_MAX are set to zero.', &
'', &
'SYSTEM_CLOCK is typically used to measure short time intervals (system', &
'clocks may be 24-hour clocks or measure processor clock ticks since', &
'boot, for example). It is most often used for measuring or tracking the', &
'time spent in code blocks in lieu of using profiling tools.', &
'', &
'ARGUMENTS', &
'', &
'    COUNT', &
'        (optional) shall be an _integer_ scalar. It is assigned a', &
'        processor-dependent value based on the current value of the', &
'        processor clock, or -HUGE(COUNT) if there is no clock. The', &
'        processor-dependent value is incremented by one for each clock', &
'        count until the value COUNT_MAX is reached and is reset to zero', &
'        at the next count. It lies in the range 0 to COUNT_MAX if there', &
'        is a clock.', &
'', &
'    COUNT_RATE', &
'        (optional) shall be an _integer_ or _real_ scalar. It is', &
'        assigned a processor-dependent approximation to the number of', &
'        processor clock counts per second, or zero if there is no clock.', &
'', &
'    COUNT_MAX', &
'        (optional) shall be an _integer_ scalar. It is assigned the', &
'        maximum value that COUNT can have, or zero if there is no clock.', &
'', &
'EXAMPLES', &
'', &
'Sample program:', &
'', &
'    program demo_system_clock', &
'    implicit none', &
'    integer, parameter :: wp = kind(1.0d0)', &
'    integer :: count, count_rate, count_max', &
'    integer :: start, finish', &
'    real    :: time_read', &
'', &
'       call system_clock(count, count_rate, count_max)', &
'       write(*,*) count, count_rate, count_max', &
'', &
'       call system_clock(start, count_rate)', &
'       ! <<<< code to time', &
'       call system_clock(finish)', &
'       time_read=(finish-start)/real(count_rate,wp)', &
'       write(*,''(a30,1x,f7.4,1x,a)'') ''time * : '', time_read, '' seconds''', &
'', &
'    end program demo_system_clock', &
'', &
'If the processor clock is a 24-hour clock that registers time at', &
'approximately 18.20648193 ticks per second, at 11:30 A.M. the reference', &
'', &
'          call system_clock (count = c, count_rate = r, count_max = m)', &
'', &
'defines', &
'', &
'          C = (11*3600+30*60)*18.20648193 = 753748,', &
'          R = 18.20648193, and', &
'          M = 24*3600*18.20648193-1 = 1573039.', &
'', &
'STANDARD', &
'', &
'Fortran 95 and later', &
'', &
'SEE ALSO', &
'', &
'DATE_AND_TIME(3), CPU_TIME(3)', &
'', &
'fortran-lang intrinsic descriptions', &
'']

shortname="system_clock"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('188','tan')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'TAN(3) - [MATHEMATICS:TRIGONOMETRIC] Tangent function (GFDL)', &
'', &
'SYNTAX', &
'', &
'    result = tan(x)', &
'', &
'DESCRIPTION', &
'', &
'TAN(X) computes the tangent of X.', &
'', &
'ARGUMENTS', &
'', &
'    X', &
'        The type shall be _real_ or _complex_.', &
'', &
'RETURNS', &
'', &
'The return value has the same type and kind as X.', &
'', &
'EXAMPLES', &
'', &
'Sample program:', &
'', &
'    program demo_tan', &
'    use, intrinsic :: iso_fortran_env, only : real_kinds, &', &
'    & real32, real64, real128', &
'    implicit none', &
'    real(kind=real64) :: x = 0.165_real64', &
'         write(*,*)x, tan(x)', &
'    end program demo_tan', &
'', &
'Results:', &
'', &
'         0.16500000000000001       0.16651386310913616', &
'', &
'STANDARD', &
'', &
'FORTRAN 77 and later. For a complex argument, Fortran 2008 or later.', &
'', &
'SEE ALSO', &
'', &
'ATAN(3), COS(3), SIN(3)', &
'', &
'fortran-lang intrinsic descriptions', &
'']

shortname="tan"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('189','tanh')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'TANH(3) - [MATHEMATICS:TRIGONOMETRIC] Hyperbolic tangent function (GFDL)', &
'', &
'SYNTAX', &
'', &
'    x = tanh(x)', &
'', &
'DESCRIPTION', &
'', &
'TANH(X) computes the hyperbolic tangent of X.', &
'', &
'ARGUMENTS', &
'', &
'    X', &
'        The type shall be _real_ or _complex_.', &
'', &
'RETURNS', &
'', &
'The return value has same type and kind as X. If X is complex, the', &
'imaginary part of the result is in radians. If X is _real_, the return', &
'value lies in the range', &
'', &
'          -1 <= tanh(x) <= 1.', &
'', &
'EXAMPLES', &
'', &
'Sample program:', &
'', &
'    program demo_tanh', &
'    use, intrinsic :: iso_fortran_env, only : &', &
'    & real_kinds, real32, real64, real128', &
'    implicit none', &
'    real(kind=real64) :: x = 2.1_real64', &
'       write(*,*)x, tanh(x)', &
'    end program demo_tanh', &
'', &
'Results:', &
'', &
'          2.1000000000000001       0.97045193661345386', &
'', &
'STANDARD', &
'', &
'FORTRAN 77 and later, for a complex argument Fortran 2008 or later', &
'', &
'SEE ALSO', &
'', &
'-   Wikipedia:hyperbolic functions', &
'', &
'ATANH(3)', &
'', &
'fortran-lang intrinsic descriptions', &
'']

shortname="tanh"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('190','this_image')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'THIS_IMAGE(3) - [COLLECTIVE] Cosubscript index of this image (GFDL)', &
'', &
'SYNTAX', &
'', &
'    result = this_image() result = this_image(distance) &', &
'             & result = this_image(coarray, dim)', &
'', &
'DESCRIPTION', &
'', &
'Returns the cosubscript for this image.', &
'', &
'ARGUMENTS', &
'', &
'    DISTANCE', &
'        (optional, INTENT(IN)) Nonnegative scalar integer (not permitted', &
'        together with COARRAY).', &
'', &
'    COARRAY', &
'        Coarray of any type (optional; if DIM present, required).', &
'', &
'    DIM', &
'        default integer scalar (optional). If present, DIM shall be', &
'        between one and the corank of COARRAY.', &
'', &
'RETURNS', &
'', &
'Default integer. If COARRAY is not present, it is scalar; if DISTANCE is', &
'not present or has value 0, its value is the image index on the invoking', &
'image for the current team, for values smaller or equal distance to the', &
'initial team, it returns the image index on the ancestor team which has', &
'a distance of DISTANCE from the invoking team. If DISTANCE is larger', &
'than the distance to the initial team, the image index of the initial', &
'team is returned. Otherwise when the COARRAY is present, if DIM is not', &
'present, a rank-1 array with corank elements is returned, containing the', &
'cosubscripts for COARRAY specifying the invoking image. If DIM is', &
'present, a scalar is returned, with the value of the DIM element of', &
'THIS_IMAGE(COARRAY).', &
'', &
'EXAMPLES', &
'', &
'Sample program:', &
'', &
'    program demo_this_image', &
'    implicit none', &
'    integer :: value[*]', &
'    integer :: i', &
'       value = this_image()', &
'       sync all', &
'       if (this_image() == 1) then', &
'          do i = 1, num_images()', &
'             write(*,''(2(a,i0))'') ''value['', i, ''] is '', value[i]', &
'          end do', &
'       endif', &
'    end program demo_this_image', &
'', &
'Results:', &
'', &
'       value[1] is 1', &
'', &
'! ! Check whether the current image is the initial image if', &
'(this_image(huge(1)) /= this_image()) error stop "something is rotten', &
'here" ```', &
'', &
'STANDARD', &
'', &
'Fortran 2008 and later. With DISTANCE argument, TS 18508 or later', &
'', &
'SEE ALSO', &
'', &
'NUM_IMAGES(3), IMAGE_INDEX(3)', &
'', &
'fortran-lang intrinsic descriptions', &
'']

shortname="this_image"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('191','tiny')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'TINY(3) - [NUMERIC MODEL] Smallest positive number of a real kind (GFDL)', &
'', &
'SYNTAX', &
'', &
'    result = tiny(x)', &
'       real(kind=KIND) function(x)', &
'       real(kind=KIND) :: x', &
'', &
'where KIND may be any kind supported by type _real_', &
'', &
'DESCRIPTION', &
'', &
'TINY(X) returns the smallest positive (non zero) number of the type and', &
'kind of X.', &
'', &
'ARGUMENTS', &
'', &
'    X', &
'        Shall be of type _real_.', &
'', &
'RETURNS', &
'', &
'The smallest positive value for the _real_ type of the specified kind.', &
'', &
'The return value is of the same type and kind as X.', &
'', &
'EXAMPLES', &
'', &
'Sample program:', &
'', &
'    program demo_tiny', &
'    implicit none', &
'       print *, ''default real is from'',tiny(0.0) ,''to'',huge(0.0)', &
'       print *, ''doubleprecision is from '',tiny(0.0d0),''to'',huge(0.0d0)', &
'    end program demo_tiny', &
'', &
'Results:', &
'', &
'     default real is from 1.17549435E-38 to 3.40282347E+38', &
'     doubleprecision is from 2.2250738585072014E-308 to 1.7976931348623157E+308', &
'', &
'STANDARD', &
'', &
'Fortran 95 and later', &
'', &
'SEE ALSO', &
'', &
'DIGITS(3), EPSILON(3), EXPONENT(3), FRACTION(3), HUGE(3),', &
'MAXEXPONENT(3), MINEXPONENT(3), NEAREST(3), PRECISION(3), RADIX(3),', &
'RANGE(3), RRSPACING(3), SCALE(3), SET_EXPONENT(3), SPACING(3)', &
'', &
'fortran-lang intrinsic descriptions', &
'']

shortname="tiny"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('192','trailz')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'TRAILZ(3) - [BIT:COUNT] Number of trailing zero bits of an integer', &
'', &
'SYNTAX', &
'', &
'       result = trailz(i) integer :: result', &
'       integer(kind=NNN),intent(in) :: i', &
'', &
'DESCRIPTION', &
'', &
'TRAILZ(3) returns the number of trailing zero bits of an _integer_ value', &
'', &
'ARGUMENTS', &
'', &
'    I', &
'        Shall be of type _integer_.', &
'', &
'RETURNS', &
'', &
'The type of the return value is the default _integer_. If all the bits', &
'of I are zero, the result value is BIT_SIZE(I).', &
'', &
'EXAMPLES', &
'', &
'Sample program:', &
'', &
'    program demo_trailz', &
'    use, intrinsic :: iso_fortran_env, only : integer_kinds, &', &
'    & int8, int16, int32, int64', &
'    implicit none', &
'    integer(kind=int64) :: i, value', &
'       write(*,*)''Default integer:''', &
'       write(*,*)''bit_size='',bit_size(0)', &
'       write(*,''(1x,i3,1x,i3,1x,b0)'')-1,trailz(1),-1', &
'       write(*,''(1x,i3,1x,i3,1x,b0)'')0,trailz(0),0', &
'       write(*,''(1x,i3,1x,i3,1x,b0)'')1,trailz(1),1', &
'       write(*,''(" huge(0)=",i0,1x,i0,1x,b0)'') &', &
'       & huge(0),trailz(huge(0)),huge(0)', &
'       write(*,*)', &
'       write(*,*)''integer(kind=int64):''', &
'', &
'       do i=-1,62,5', &
'          value=2**i', &
'          write(*,''(1x,i19,1x,i3)'')value,trailz(value)', &
'       enddo', &
'       value=huge(i)', &
'       write(*,''(1x,i19,1x,i3,"(huge(0_int64))")'')value,trailz(value)', &
'', &
'       do i=-1,62,5', &
'          value=2**i', &
'          write(*,''(1x,i3,2x,b64.64)'')i,value', &
'       enddo', &
'       value=huge(i)', &
'       write(*,''(1x,a,1x,b64.64)'') "huge",value', &
'', &
'    end program demo_trailz', &
'', &
'Results:', &
'', &
'     Default integer:', &
'     bit_size=          32', &
'      -1   0 11111111111111111111111111111111', &
'       0  32 0', &
'       1   0 1', &
'     huge(0)=2147483647 0 1111111111111111111111111111111', &
'', &
'     integer(kind=int64):', &
'                       0  64', &
'                      16   4', &
'                     512   9', &
'                   16384  14', &
'                  524288  19', &
'                16777216  24', &
'               536870912  29', &
'             17179869184  34', &
'            549755813888  39', &
'          17592186044416  44', &
'         562949953421312  49', &
'       18014398509481984  54', &
'      576460752303423488  59', &
'     9223372036854775807   0(huge(0_int64))', &
'      -1  0000000000000000000000000000000000000000000000000000000000000000', &
'       4  0000000000000000000000000000000000000000000000000000000000010000', &
'       9  0000000000000000000000000000000000000000000000000000001000000000', &
'      14  0000000000000000000000000000000000000000000000000100000000000000', &
'      19  0000000000000000000000000000000000000000000010000000000000000000', &
'      24  0000000000000000000000000000000000000001000000000000000000000000', &
'      29  0000000000000000000000000000000000100000000000000000000000000000', &
'      34  0000000000000000000000000000010000000000000000000000000000000000', &
'      39  0000000000000000000000001000000000000000000000000000000000000000', &
'      44  0000000000000000000100000000000000000000000000000000000000000000', &
'      49  0000000000000010000000000000000000000000000000000000000000000000', &
'      54  0000000001000000000000000000000000000000000000000000000000000000', &
'      59  0000100000000000000000000000000000000000000000000000000000000000', &
'     huge 0111111111111111111111111111111111111111111111111111111111111111', &
'', &
'STANDARD', &
'', &
'Fortran 2008 and later', &
'', &
'SEE ALSO', &
'', &
'BIT_SIZE(3), POPCNT(3), POPPAR(3), LEADZ(3)', &
'', &
'fortran-lang intrinsic descriptions (@urbanjost)', &
'']

shortname="trailz"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('193','transfer')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'TRANSFER(3) - [TYPE:MOLD] Transfer bit patterns (GFDL)', &
'', &
'SYNTAX', &
'', &
'    result = transfer(source, mold, size)', &
'', &
'DESCRIPTION', &
'', &
'Interprets the bitwise representation of SOURCE in memory as if it is', &
'the representation of a variable or array of the same type and type', &
'parameters as MOLD.', &
'', &
'This is approximately equivalent to the C concept of *casting* one type', &
'to another.', &
'', &
'ARGUMENTS', &
'', &
'    SOURCE', &
'        Shall be a scalar or an array of any type.', &
'', &
'    MOLD', &
'        Shall be a scalar or an array of any type.', &
'', &
'    SIZE', &
'        (Optional) shall be a scalar of type _integer_.', &
'', &
'RETURNS', &
'', &
'The result has the same type as MOLD, with the bit level representation', &
'of SOURCE. If SIZE is present, the result is a one-dimensional array of', &
'length SIZE. If SIZE is absent but MOLD is an array (of any size or', &
'shape), the result is a one-dimensional array of the minimum length', &
'needed to contain the entirety of the bitwise representation of SOURCE.', &
'If SIZE is absent and MOLD is a scalar, the result is a scalar.', &
'', &
'If the bitwise representation of the result is longer than that of', &
'SOURCE, then the leading bits of the result correspond to those of', &
'SOURCE and any trailing bits are filled arbitrarily.', &
'', &
'When the resulting bit representation does not correspond to a valid', &
'representation of a variable of the same type as MOLD, the results are', &
'undefined, and subsequent operations on the result cannot be guaranteed', &
'to produce sensible behavior. For example, it is possible to create', &
'_logical_ variables for which VAR and .not. var both appear to be true.', &
'', &
'EXAMPLES', &
'', &
'Sample program:', &
'', &
'    program demo_transfer', &
'    use,intrinsic :: iso_fortran_env, only : int32, real32', &
'    integer(kind=int32) :: i = 2143289344', &
'    real(kind=real32)   :: x', &
'    character(len=10)   :: string', &
'    character(len=1)    :: chars(10)', &
'       x=transfer(i, 1.0)    ! prints "nan" on i686', &
'       ! the bit patterns are the same', &
'       write(*,''(b0,1x,g0)'')x,x ! create a NaN', &
'       write(*,''(b0,1x,g0)'')i,i', &
'', &
'       ! a string to an array of characters', &
'       string=''abcdefghij''', &
'       chars=transfer(string,chars)', &
'       write(*,''(*("[",a,"]":,1x))'')string', &
'       write(*,''(*("[",a,"]":,1x))'')chars', &
'    end program demo_transfer', &
'', &
'Results:', &
'', &
'       1111111110000000000000000000000 NaN', &
'       1111111110000000000000000000000 2143289344', &
'       [abcdefghij]', &
'       [a] [b] [c] [d] [e] [f] [g] [h] [i] [j]', &
'', &
'COMMENTS', &
'', &
'_Joe Krahn_: Fortran uses MOLDING rather than CASTING.', &
'', &
'Casting, as in C, is an in-place reinterpretation. A cast is a device', &
'that is built around an object to change its shape.', &
'', &
'Fortran TRANSFER reinterprets data out-of-place. It can be considered', &
'MOLDING rather than casting. A MOLD is a device that confers a shape', &
'onto an object placed into it.', &
'', &
'The advantage of molding is that data is always valid in the context of', &
'the variable that holds it. For many cases, a decent compiler should', &
'optimize TRANSFER into a simple assignment.', &
'', &
'There are disadvantages of this approach. It is problematic to define a', &
'union of data types because you must know the largest data object, which', &
'can vary by compiler or compile options. In many cases, an EQUIVALENCE', &
'would be far more effective, but Fortran Standards committees seem', &
'oblivious to the benefits of EQUIVALENCEs when used sparingly.', &
'', &
'STANDARD', &
'', &
'Fortran 90 and later', &
'', &
'fortran-lang intrinsic descriptions', &
'']

shortname="transfer"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('194','transpose')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'TRANSPOSE(3) - [ARRAY MANIPULATION] Transpose an array of rank two', &
'(GFDL)', &
'', &
'SYNTAX', &
'', &
'    result = transpose(matrix)', &
'', &
'DESCRIPTION', &
'', &
'Transpose an array of rank two. Element (i, j) of the result has the', &
'value MATRIX(J, I), for all i, j.', &
'', &
'ARGUMENTS', &
'', &
'    MATRIX', &
'        Shall be an array of any type and have a rank of two.', &
'', &
'RETURNS', &
'', &
'The result has the same type as MATRIX, and has shape [ m, n ] if MATRIX', &
'has shape [ n, m ].', &
'', &
'EXAMPLES', &
'', &
'Sample program:', &
'', &
'    program demo_transpose', &
'    implicit none', &
'    integer,save :: xx(3,5)= reshape([&', &
'        1,  2,  3,  4,  5,    &', &
'       10, 20, 30, 40, 50,    &', &
'       11, 22, 33, 44, -1055  &', &
'     ],shape(xx),order=[2,1])', &
'', &
'    call print_matrix_int(''xx array:'',xx)', &
'    call print_matrix_int(''xx array transposed:'',transpose(xx))', &
'', &
'    contains', &
'', &
'    subroutine print_matrix_int(title,arr)', &
'    ! print small 2d integer arrays in row-column format', &
'    implicit none', &
'    character(len=*),intent(in)  :: title', &
'    integer,intent(in)           :: arr(:,:)', &
'    integer                      :: i', &
'    character(len=:),allocatable :: biggest', &
'       write(*,*)trim(title)  ! print title', &
'       biggest=''           ''  ! make buffer to write integer into', &
'       ! find how many characters to use for integers', &
'       write(biggest,''(i0)'')ceiling(log10(real(maxval(abs(arr)))))+2', &
'       ! use this format to write a row', &
'       biggest=''(" > [",*(i''//trim(biggest)//'':,","))''', &
'       ! print one row of array at a time', &
'       do i=1,size(arr,dim=1)', &
'          write(*,fmt=biggest,advance=''no'')arr(i,:)', &
'          write(*,''(" ]")'')', &
'       enddo', &
'    end subroutine print_matrix_int', &
'', &
'    end program demo_transpose', &
'', &
'Results:', &
'', &
'        xx array:', &
'        > [     1,     2,     3,     4,     5 ]', &
'        > [    10,    20,    30,    40,    50 ]', &
'        > [    11,    22,    33,    44, -1055 ]', &
'        xx array transposed:', &
'        > [     1,    10,    11 ]', &
'        > [     2,    20,    22 ]', &
'        > [     3,    30,    33 ]', &
'        > [     4,    40,    44 ]', &
'        > [     5,    50, -1055 ]', &
'', &
'STANDARD', &
'', &
'Fortran 95 and later', &
'', &
'fortran-lang intrinsic descriptions', &
'']

shortname="transpose"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('195','trim')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'TRIM(3) - [CHARACTER:WHITESPACE] Remove trailing blank characters of a', &
'string (GFDL)', &
'', &
'SYNTAX', &
'', &
'    result = trim(string)', &
'', &
'DESCRIPTION', &
'', &
'Removes trailing blank characters of a string.', &
'', &
'ARGUMENTS', &
'', &
'    STRING', &
'        Shall be a scalar of type _character_.', &
'', &
'RETURNS', &
'', &
'A scalar of type _character_ which length is that of STRING less the', &
'number of trailing blanks.', &
'', &
'EXAMPLES', &
'', &
'Sample program:', &
'', &
'    program demo_trim', &
'    implicit none', &
'    character(len=10), parameter :: s = "gfortran  "', &
'       write(*,*) len(s), len(trim(s))  ! "10 8", with/without trailing blanks', &
'', &
'       ! with/without trailing blanks', &
'       write(*,*) len(s), len(trim(''   leading''))', &
'       write(*,*) len(s), len(trim(''   trailing    ''))', &
'       write(*,*) len(s), len(trim(''               ''))', &
'', &
'    end program demo_trim', &
'', &
'Results:', &
'', &
'          10           8', &
'          10          10', &
'          10          11', &
'          10           0', &
'', &
'STANDARD', &
'', &
'Fortran 95 and later', &
'', &
'SEE ALSO', &
'', &
'Functions that perform operations on character strings, return lengths', &
'of arguments, and search for certain arguments:', &
'', &
'-   ELEMENTAL: ADJUSTL(3), ADJUSTR(3), INDEX(3),', &
'', &
'SCAN(3), VERIFY(3)', &
'', &
'-   NONELEMENTAL: LEN_TRIM(3), LEN(3), REPEAT(3), TRIM(3)', &
'', &
'fortran-lang intrinsic descriptions', &
'']

shortname="trim"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('196','ubound')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'UBOUND(3) - [ARRAY INQUIRY] Upper dimension bounds of an array', &
'', &
'SYNTAX', &
'', &
'    result = ubound(array, dim, kind)', &
'', &
'DESCRIPTION', &
'', &
'Returns the upper bounds of an array, or a single upper bound along the', &
'DIM dimension.', &
'', &
'ARGUMENTS', &
'', &
'    ARRAY', &
'        Shall be an array, of any type.', &
'', &
'    DIM', &
'        (Optional) Shall be a scalar _integer_.', &
'', &
'    KIND', &
'        (Optional) An _integer_ initialization expression indicating the', &
'        kind parameter of the result.', &
'', &
'RETURNS', &
'', &
'The return value is of type _integer_ and of kind KIND. If KIND is', &
'absent, the return value is of default integer kind.', &
'', &
'If DIM is absent, the result is an array of the upper bounds of ARRAY.', &
'', &
'If DIM is present, the result is a scalar corresponding to the upper', &
'bound of the array along that dimension.', &
'', &
'If ARRAY is an expression rather than a whole array or array structure', &
'component, or if it has a zero extent along the relevant dimension, the', &
'upper bound is taken to be the number of elements along the relevant', &
'dimension.', &
'', &
'EXAMPLES', &
'', &
'Note this function should not be used on assumed-size arrays or in any', &
'function without an explicit interface. Errors can occur if there is no', &
'interface defined.', &
'', &
'Sample program', &
'', &
'    ! program demo_ubound', &
'    module m2_bounds', &
'    implicit none', &
'', &
'    contains', &
'', &
'    subroutine msub(arr)', &
'    !!integer,intent(in) :: arr(*)  ! cannot be assumed-size array', &
'    integer,intent(in) :: arr(:)', &
'       write(*,*)''MSUB: LOWER='',lbound(arr),''UPPER='',ubound(arr), &', &
'       & ''SIZE='',size(arr)', &
'    end subroutine msub', &
'', &
'    end module m2_bounds', &
'', &
'    use m2_bounds, only : msub', &
'    implicit none', &
'    interface', &
'       subroutine esub(arr)', &
'       integer,intent(in) :: arr(:)', &
'       end subroutine esub', &
'    end interface', &
'    integer :: arr(-10:10)', &
'       write(*,*)''MAIN: LOWER='',lbound(arr),''UPPER='',ubound(arr), &', &
'       & ''SIZE='',size(arr)', &
'       call csub()', &
'       call msub(arr)', &
'       call esub(arr)', &
'    contains', &
'    subroutine csub', &
'       write(*,*)''CSUB: LOWER='',lbound(arr),''UPPER='',ubound(arr), &', &
'       & ''SIZE='',size(arr)', &
'    end subroutine csub', &
'', &
'    end', &
'', &
'    subroutine esub(arr)', &
'    implicit none', &
'    integer,intent(in) :: arr(:)', &
'       ! WARNING: IF CALLED WITHOUT AN EXPLICIT INTERFACE', &
'       ! THIS WILL GIVE UNDEFINED ANSWERS (like 0,0,0)', &
'       write(*,*)''ESUB: LOWER='',lbound(arr),''UPPER='',ubound(arr), &', &
'       & ''SIZE='',size(arr)', &
'    end subroutine esub', &
'    !end program demo_ubound', &
'', &
'Results:', &
'', &
'      MAIN: LOWER=         -10 UPPER=          10 SIZE=          21', &
'      CSUB: LOWER=         -10 UPPER=          10 SIZE=          21', &
'      MSUB: LOWER=           1 UPPER=          21 SIZE=          21', &
'      ESUB: LOWER=           1 UPPER=          21 SIZE=          21', &
'', &
'STANDARD', &
'', &
'Fortran 95 and later, with KIND argument Fortran 2003 and later', &
'', &
'SEE ALSO', &
'', &
'LBOUND(3), CO_UBOUND(3), [CO_LBOUND(3)(CO_LBOUND)]', &
'', &
'fortran-lang intrinsic descriptions', &
'']

shortname="ubound"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('197','unpack')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'UNPACK(3) - [ARRAY CONSTRUCTION] Store the elements of a vector in an', &
'array of higher rank (GFDL)', &
'', &
'SYNTAX', &
'', &
'    result = unpack(vector, mask, field)', &
'', &
'DESCRIPTION', &
'', &
'Store the elements of VECTOR in an array of higher rank.', &
'', &
'ARGUMENTS', &
'', &
'    VECTOR', &
'        Shall be an array of any type and rank one. It shall have at', &
'        least as many elements as MASK has .TRUE. values.', &
'', &
'    MASK', &
'        Shall be an array of type _logical_.', &
'', &
'    FIELD', &
'        Shall be of the same type as VECTOR and have the same shape as', &
'        MASK.', &
'', &
'RETURNS', &
'', &
'The resulting array corresponds to FIELD with .TRUE. elements of MASK', &
'replaced by values from VECTOR in array element order.', &
'', &
'EXAMPLES', &
'', &
'Sample program:', &
'', &
'    program demo_unpack', &
'    implicit none', &
'    integer :: vector(2)  = [1,1]', &
'    logical :: mask(4)  = [ .true., .false., .false., .true. ]', &
'    integer :: field(2,2) = 0, unity(2,2)', &
'', &
'       ! result: unity matrix', &
'       unity = unpack(vector, reshape(mask, [2,2]), field)', &
'       write(*,*)unity,size(unity),shape(unity)', &
'', &
'    end program demo_unpack', &
'', &
'Results:', &
'', &
'                  1           0           0           1           4', &
'                  2           2', &
'', &
'STANDARD', &
'', &
'Fortran 95 and later', &
'', &
'SEE ALSO', &
'', &
'PACK(3), MERGE(3), PACK(3), SPREAD(3), UNPACK(3)', &
'', &
'fortran-lang intrinsic descriptions', &
'']

shortname="unpack"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('198','verify')

textblock=[character(len=256) :: &
'', &
'NAME', &
'', &
'VERIFY(3) - [CHARACTER:SEARCH] Scan a string for the absence of a set of', &
'characters', &
'', &
'SYNTAX', &
'', &
'    result = verify(string, set, back, kind)', &
'', &
'      integer(kind=KIND) elemental function verify(string,set,back,kind)', &
'', &
'       character(len=*),intent(in) :: string', &
'       character(len=*),intent(in) :: set', &
'       logical,intent(in),optional :: back', &
'       integer,intent(in),optional :: KIND', &
'', &
'DESCRIPTION', &
'', &
'Verifies that all the characters in STRING belong to the set of', &
'characters in SET by identifying the first character in the string(s)', &
'that is not in the set(s).', &
'', &
'If BACK is either absent or equals .FALSE., this function returns the', &
'position of the leftmost character of STRING that is not in SET.', &
'', &
'If BACK equals .TRUE., the rightmost position is returned.', &
'', &
'If all characters of STRING are found in SET, the result is zero.', &
'', &
'This makes it easy to verify strings are all uppercase or lowercase,', &
'follow a basic syntax, only contain printable characters, and many of', &
'the conditions tested for with the C routines ISALNUM(3c), ISALPHA(3c),', &
'ISASCII(3c), ISBLANK(3c), ISCNTRL(3c), ISDIGIT(3c), ISGRAPH(3c),', &
'ISLOWER(3c), ISPRINT(3c), ISPUNCT(3c), ISSPACE(3c), ISUPPER(3c), and', &
'ISXDIGIT(3c); but for a string as well an an array of characters.', &
'', &
'ARGUMENTS', &
'', &
'    STRING', &
'        Shall be of type _character_.', &
'', &
'    SET', &
'        Shall be of type _character_.', &
'', &
'    BACK', &
'        shall be of type _logical_.', &
'', &
'    KIND', &
'        An _integer_ initialization expression indicating the kind', &
'        parameter of the result.', &
'', &
'RETURNS', &
'', &
'The return value is of type _integer_ and of kind KIND. If KIND is', &
'absent, the return value is of default integer kind.', &
'', &
'EXAMPLES', &
'', &
'Sample program I:', &
'', &
'    program demo_verify', &
'    implicit none', &
'    character(len=*),parameter :: int=''0123456789''', &
'    character(len=*),parameter :: hex=''abcdef0123456789''', &
'    character(len=*),parameter :: low=''abcdefghijklmnopqrstuvwxyz''', &
'    character(len=*),parameter :: upp=''ABCDEFGHIJKLMNOPQRSTUVWXYZ''', &
'    character(len=20):: string=''   Howdy There!''', &
'    character(len=6) :: strings(2)=["Howdy ","there!"]', &
'    character(len=2) :: sets(2)=["de","gh"]', &
'', &
'       write(*,*)''first non-blank character '',verify(string, '' '')', &
'       ! NOTE: same as len_trim(3)', &
'       write(*,*)''last non-blank character'',verify(string, '' '',back=.true.)', &
'', &
'       ! first non-lowercase non-blank character', &
'       write(*,*) verify(string,low//'' '')', &
'', &
'       !! elemental -- using arrays for both strings and for sets', &
'', &
'       ! first character in "Howdy" not in "de", and first letter in "there!"', &
'       ! not in "gh"', &
'       write(*,*) verify(strings,sets)', &
'', &
'       ! check each string from right to left for non-letter', &
'       write(*,*) ''last non-letter'',verify(strings,upp//low,back=.true.)', &
'', &
'       ! note character variables in an array have to be of same length', &
'       ! find last non-uppercase character in "Howdy"', &
'       ! and first non-lowercase in "There!"', &
'       write(*,*) verify(strings,[upp,low],back=[.true.,.false.])', &
'', &
'       write(*,*) verify("fortran", "", .true.)  ! 7, found ''n''', &
'       ! 0'' found none unmatched', &
'       write(*,*) verify("fortran", "nartrof")', &
'', &
'        !! CHECK IF STRING IS OF FORM NN-HHHHH', &
'        CHECK : block', &
'           logical                    :: lout', &
'           character(len=80)          :: chars', &
'', &
'           chars=''32-af43d''', &
'           lout=.true.', &
'', &
'           ! are the first two characters integer characters?', &
'           lout = lout.and.(verify(chars(1:2), int) == 0)', &
'', &
'           ! is the third character a dash?', &
'           lout = lout.and.(verify(chars(3:3), ''-'') == 0)', &
'', &
'           ! is remaining string a valid representation of a hex value?', &
'           lout = lout.and.(verify(chars(4:8), hex) == 0)', &
'', &
'           if(lout)then', &
'              write(*,*)trim(chars),'' passed''', &
'           endif', &
'', &
'        endblock CHECK', &
'    end program demo_verify', &
'', &
'Results:', &
'', &
'        first non-blank character            4', &
'        last non-blank character          15', &
'                  4', &
'                  1           1', &
'        last non-letter           6           6', &
'                  6           6', &
'                  7', &
'                  0', &
'        32-af43d passed', &
'', &
'Sample program II:', &
'', &
'Determine if strings are valid integer representations', &
'', &
'    program fortran_ints', &
'    implicit none', &
'    integer :: i', &
'    character(len=*),parameter :: ints(*)=[character(len=10) :: &', &
'     ''+1 '', &', &
'     ''3044848 '', &', &
'     ''30.40 '', &', &
'     ''September '', &', &
'     ''1 2 3'', &', &
'     ''  -3000 '', &', &
'     '' '']', &
'', &
'       write(*,''("|",*(g0,"|"))'') ints', &
'       write(*,''("|",*(1x,l1,8x,"|"))'') isint(ints)', &
'', &
'    contains', &
'', &
'    elemental function isint(line) result (lout)', &
'    !', &
'    ! determine if string is a valid integer representation', &
'    ! ignoring trailing spaces and leading spaces', &
'    !', &
'    character(len=*),parameter   :: digits=''0123456789''', &
'    character(len=*),intent(in)  :: line', &
'    character(len=:),allocatable :: name', &
'    logical                      :: lout', &
'       lout=.false.', &
'       ! make sure at least two characters long to simplify tests', &
'       name=adjustl(line)//''  ''', &
'       ! blank string', &
'       if( name .eq. '''' )return', &
'       ! allow one leading sign', &
'       if( verify(name(1:1),''+-'') == 0 ) name=name(2:)', &
'       ! was just a sign', &
'       if( name .eq. '''' )return', &
'       lout=verify(trim(name), digits)  == 0', &
'    end function isint', &
'', &
'    end program fortran_ints', &
'', &
'Results:', &
'', &
'    |+1       |3044848  |30.40    |September|1 2 3    |  -3000  |         |', &
'    | T       | T       | F       | F       | F       | T       | F       |', &
'', &
'Sample program III:', &
'', &
'Determine if strings represent valid Fortran symbol names', &
'', &
'    program fortran_symbol_name', &
'    implicit none', &
'    integer :: i', &
'    character(len=*),parameter :: symbols(*)=[character(len=10) :: &', &
'     ''A_ '', &', &
'     ''10 '', &', &
'     ''September '', &', &
'     ''A B'', &', &
'     ''_A '', &', &
'     '' '']', &
'', &
'       write(*,''("|",*(g0,"|"))'') symbols', &
'       write(*,''("|",*(1x,l1,8x,"|"))'') fortran_name(symbols)', &
'', &
'    contains', &
'', &
'    elemental function fortran_name(line) result (lout)', &
'    !', &
'    ! determine if a string is a valid Fortran name', &
'    ! ignoring trailing spaces (but not leading spaces)', &
'    !', &
'    character(len=*),parameter   :: int=''0123456789''', &
'    character(len=*),parameter   :: lower=''abcdefghijklmnopqrstuvwxyz''', &
'    character(len=*),parameter   :: upper=''ABCDEFGHIJKLMNOPQRSTUVWXYZ''', &
'    character(len=*),parameter   :: allowed=upper//lower//int//''_''', &
'', &
'    character(len=*),intent(in)  :: line', &
'    character(len=:),allocatable :: name', &
'    logical                      :: lout', &
'       name=trim(line)', &
'       if(len(name).ne.0)then', &
'          ! first character is alphameric', &
'          lout = verify(name(1:1), lower//upper) == 0  &', &
'           ! other characters are allowed in a symbol name', &
'           & .and. verify(name,allowed) == 0           &', &
'           ! allowable length', &
'           & .and. len(name) <= 63', &
'       else', &
'          lout = .false.', &
'       endif', &
'    end function fortran_name', &
'', &
'    end program fortran_symbol_name', &
'', &
'Results:', &
'', &
'    |A_        |10        |September |A B       |_A        |          |', &
'    | T        | F        | T        | F        | F        | F        |', &
'', &
'STANDARD', &
'', &
'Fortran 95 and later, with KIND argument - Fortran 2003 and later', &
'', &
'SEE ALSO', &
'', &
'Functions that perform operations on character strings, return lengths', &
'of arguments, and search for certain arguments:', &
'', &
'-   ELEMENTAL: ADJUSTL(3), ADJUSTR(3), INDEX(3), SCAN(3), VERIFY(3)', &
'', &
'-   NONELEMENTAL: LEN_TRIM(3), LEN(3), REPEAT(3), TRIM(3)', &
'', &
'fortran-lang intrinsic descriptions (@urbanjost)', &
'']

shortname="verify"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif

case default
   allocate (character(len=256) :: textblock(0))
end select
end function help_intrinsics_one
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine sort_name(lines)
!@(#) sort_name(3fp):sort strings(a-z) over specified field using shell sort starting with [ character
character(len = *)                :: lines(:)
   character(len = :),allocatable :: ihold
   integer                        :: n, igap, i, j, k, jg
   n = size(lines)
   if(n.gt.0)then
      allocate(character(len = len(lines(1))) :: ihold)
   else
      ihold = ''
   endif
   igap = n
   INFINITE: do
      igap = igap/2
      if(igap.eq.0) exit INFINITE
      k = n-igap
      i = 1
      INNER: do
         j = i
         INSIDE: do
            jg = j+igap
            if( lle( lower(lines(j)), lower(lines(jg)) ) )exit INSIDE
            ihold = lines(j)
            lines(j) = lines(jg)
            lines(jg) = ihold
            j = j-igap
            if(j.lt.1) exit INSIDE
         enddo INSIDE
         i = i+1
         if(i.gt.k) exit INNER
      enddo INNER
   enddo INFINITE
end subroutine sort_name
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
elemental pure function lower(str) result (string)
!@(#) M_strings::lower(3f): Changes a string to lowercase over specified range
character(*), intent(In)     :: str
character(len(str))          :: string
integer                      :: i
   string = str
   do i = 1, len_trim(str)     ! step thru each letter in the string
      select case (str(i:i))
      case ('A':'Z')
         string(i:i) = char(iachar(str(i:i))+32) ! change letter to miniscule
      case default
      end select
   end do
end function lower
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!elemental pure function compact(str,char) result (outstr)
function compact(str,char) result (outstr)

!$@(#) M_strings::compact(3f): Converts white-space to single spaces; removes leading spaces

character(len=*),intent(in)          :: str
character(len=*),intent(in),optional :: char
character(len=len(str))              :: outstr
character(len=1)                     :: ch
integer                              :: i
integer                              :: position_in_output
logical                              :: last_was_space
character(len=1)                     :: char_p
logical                              :: nospace
if(present(char))then
   char_p=char
   if(len(char).eq.0)then
      nospace=.true.
   else
      nospace=.false.
   endif
else
   char_p=' '
   nospace=.false.
endif
   outstr=' '
   last_was_space=.false.
   position_in_output=0

   IFSPACE: do i=1,len_trim(str)
     ch=str(i:i)
     select case(iachar(ch))
       case(0:32,127)                                         ! space or tab character or control character
         if(position_in_output.eq.0)then                      ! still at beginning so ignore leading whitespace
            cycle IFSPACE
         elseif(.not.last_was_space) then                     ! if have not already put out a space output one
           if(.not.nospace)then
              position_in_output=position_in_output+1
              outstr(position_in_output:position_in_output)=char_p
           endif
         endif
         last_was_space=.true.
       case(:-1,33:126,128:)                                  ! not a space, quote, or control character so copy it
         position_in_output=position_in_output+1
         outstr(position_in_output:position_in_output)=ch
         last_was_space=.false.
     end select
   enddo IFSPACE

end function compact
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
end module M_intrinsics
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
