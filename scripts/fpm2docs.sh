#!/bin/bash
#@(#) create man-pages, markdown and html slidy(1) files of fpm(1) help text using txt2man(1) and pandoc(1)
# liked results better tnan from txt to man-pages using pandoc
# can use groff to turn man-pages into a lot of formats as well
export FPMPATH="$(dirname $(which fpm) )"
source $(dirname $0)/sourceme.sh
export PATH="$PATH:$FPMPATH"

DOCS=$BASE/docs
MANDIR=$BASE/man
mkdir -p $DOCS
mkdir -p $MANDIR/man1
###############################################################################
echo "$DOCS/slidy.md" 1>&2
cat >$DOCS/slidy.md <<\EOF
# FPM
## Fortran Package Manager
EOF
(
echo "$DOCS/fpm_slidy.md" 1>&2
for NAME in fpm new build run test runner install update list help version
do
   echo "$DOCS/$NAME.md" 1>&2
   echo "$MANDIR/man/man1/$NAME.1 " 1>&2
   echo "# $NAME"
   echo " "
   fpm help $NAME|
   txt2man | 
   tee $MANDIR/man1/$NAME.1 |
   pandoc -t gfm -f man --columns=72 |
   sed -e 's/fpm: *Leaving directory.*//' |
   sed -e 's@/\*\*fpm\*\*@/fpm@g' >/$DOCS/$NAME.md
   cat $DOCS/$NAME.md |
   sed -e 's/^>//'|
   sed -e 's/\(^# \)\([A-Z][A-Z ]*\)/\1__\L\2__/'|
   sed -e 's/^# /## /'
done
) >>$DOCS/fpm_slidy.md
###############################################################################
echo "$DOCS/fpm_slidy.html" 1>&2
pandoc -f gfm \
 --columns=72 \
 --slide-level=1 \
 --metadata pagetitle="FPM command" \
 --self-contained \
 --standalone -o $DOCS/fpm_slidy.html \
 -t slidy \
 $DOCS/fpm_slidy.md
###############################################################################
echo 'see docs/ and man/ directories for output'
exit
###############################################################################
