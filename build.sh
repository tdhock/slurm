#!/bin/bash
cd ..
set -o errexit
rm -rf slurm-release
cp -r slurm slurm-release
grep -v Remotes slurm/DESCRIPTION > slurm-release/DESCRIPTION
PKG_TGZ=$(R CMD build slurm-release|grep building|sed 's/.*‘//'|sed 's/’.*//')
R CMD INSTALL $PKG_TGZ
R CMD check --as-cran $PKG_TGZ
