#!/bin/bash
# $1: tpi, $2: m, $3: max.outros, $4: pop, $5: yearsR, $6: pyearsR, $7: pn, $8: n.tp, $9: n.sim
# Save original location
CWD=$PWD
# Copy files to local scratch
#rsync -aq ./ {TMPDIR}
mkdir $TMPDIR/scripts/
mkdir $TMPDIR/data/
rsync -aq ../data/12_resce_$4_*_$7.data $TMPDIR/data/
rsync -aq ../data/12_printh_$4_*_$7.data $TMPDIR/data/
rsync -aq ../data/behav_$4.data $TMPDIR/data/
rsync -aq s_run-model_12dd.R $TMPDIR/scripts/
# Run commands
# cd $TMPDIR
# do_my_calculation
cd $TMPDIR/scripts/
Rscript s_run-model_12dd.R $1 $2 $3 $4 $5 $6 $7 $8 $9
# Copy new and changed files back
# rsync -auq ${TMPDIR}/ $CWD
rsync -auq $TMPDIR/data/ $CWD/../data/
