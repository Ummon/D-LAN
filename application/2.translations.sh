#!/usr/bin/env bash
# Generate the 'ts' files the compile them to 'qm' files
#set -o errexit

TS_DIR=translations
QM_DIR=languages

cd $TS_DIR

# We should use the project files, but there is a bug described here: https://bugreports.qt-project.org/browse/QTBUG-24587
# lupdate Core.pro
# lupdate GUI.pro
lupdate -no-obsolete -codecfortr UTF-8 ../GUI ../Common -ts d_lan_gui.fr.ts d_lan_gui.ko.ts
lupdate -no-obsolete -codecfortr UTF-8 ../Core ../Common -ts d_lan_core.fr.ts d_lan_core.ko.ts

mkdir ../GUI/output/debug/$QM_DIR
mkdir ../Core/output/debug/$QM_DIR

rm *.qm

lrelease *.ts

mv *gui*.qm ../GUI/output/debug/$QM_DIR
mv *core*.qm ../Core/output/debug/$QM_DIR
