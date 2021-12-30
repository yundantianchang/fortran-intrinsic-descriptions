#!/bin/bash
git checkout gh-pages
<<<<<<< HEAD
=======
# update gh-pages with anything from main
git merge origin/main
git push origin gh-pages
git checkout main
>>>>>>> origin/main
#################################################
PROBLEMS(){
# some expected problems for now
cd $BASE ||exit
mkdir -p expected
mv example/c_f_pointer.f90 example/c_f_procpointer.f90 example/c_funloc.f90 expected
}
#################################################
PURGE(){
cd $BASE ||exit
rm -frv man expected example
rm -fv src/M_intrinsics.f90
rm -fv docs/*
rm -fv bin/fman
rm -rfv build
}
#################################################
export FPMPATH="$(dirname $(which fpm) )"

source $(dirname $0)/sourceme.sh
# also need fpm
export PATH="$PATH:$FPMPATH"

cd $BASE
(
exec 2>&1

PURGE

mkdir -p $BASE/man/man1 $BASE/docs $BASE/example
mkdir -p $BASE/man/man3  $BASE/man/cat3

panman.sh # rebuild man-pages and M_intrinsics.f90

slidy.sh  # make HTML slides page

# build new version of fman
PROBLEMS
fpm build 
fpm install --prefix $BASE
#################################################
(
# extract demo programs
cd $BASE/intrinsics
for NAME in *.md
do
case "$NAME" in
index.md|*_index.md);;
GNU_Free_Documentation_License.md);;
*)
   TOPIC=$(basename ${NAME,,} .md)
   echo $NAME $TOPIC
   fman -d $TOPIC > $BASE/example/$TOPIC.f90
;;
esac
done
)
#################################################
# build new version of demo programs
PROBLEMS
fpm build 
#################################################
<<<<<<< HEAD
# make fpm documentation too
=======
# build fpm documenation from help too
>>>>>>> origin/main
fpm2docs.sh
#################################################
#fman manual|spell
#fman manual|findll -l 80
#################################################
)|tee /tmp/all.log
<<<<<<< HEAD
=======
cd $BASE
git add .
git commit -m 'update docs'
>>>>>>> origin/main
git checkout main
exit
#################################################
