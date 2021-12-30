export PATH=/usr/bin:/bin
HERE=$(realpath $(dirname $0))
BASE=$(dirname $HERE)
export PATH="$HERE:$PATH"
export PATH="$BASE/bin:$PATH"
export INTRINSICS=$BASE/intrinsics
export MANPATH=$BASE/man
