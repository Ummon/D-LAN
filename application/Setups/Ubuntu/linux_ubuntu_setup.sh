#!/bin/bash

DIR=../..
INST_DIR=$DIR/Installations
WORK_DIR=$INST_DIR/packaging
DEB_DIR=$WORK_DIR/d-lan
APP_DIR=$DEB_DIR/usr/share/d-lan
STYLES_DIR=$APP_DIR/styles
I18N_DIR=$APP_DIR/languages
DESKTOP_DIR=$DEB_DIR/usr/share/applications
ICON_16_DIR=$DEB_DIR/usr/share/icons/hicolor/16x16/apps
ICON_24_DIR=$DEB_DIR/usr/share/icons/hicolor/24x24/apps
ICON_32_DIR=$DEB_DIR/usr/share/icons/hicolor/32x32/apps
ICON_48_DIR=$DEB_DIR/usr/share/icons/hicolor/48x48/apps
ICON_64_DIR=$DEB_DIR/usr/share/icons/hicolor/64x64/apps
ICON_128_DIR=$DEB_DIR/usr/share/icons/hicolor/128x128/apps
ICON_256_DIR=$DEB_DIR/usr/share/icons/hicolor/256x256/apps

STYLES_FILES=$DIR/styles/*
I18N_FILES=$DIR/translations/*.qm
CORE_FILE=$DIR/Core/output/release/D-LAN.Core
GUI_FILE=$DIR/GUI/output/release/D-LAN.GUI
ICON_FILE=$DIR/Common/ressources/icon.ico

sudo rm -Rf $DEB_DIR
mkdir -p $DESKTOP_DIR $STYLES_DIR $I18N_DIR $ICON_16_DIR $ICON_24_DIR $ICON_32_DIR $ICON_48_DIR $ICON_64_DIR $ICON_128_DIR $ICON_256_DIR

cp -R etc $DEB_DIR/
cp -R $STYLES_FILES $STYLES_DIR/
cp -R $I18N_FILES $I18N_DIR/
cp $CORE_FILE $APP_DIR/
cp $GUI_FILE $APP_DIR/

rm -Rf $WORK_DIR/temp
mkdir -p $WORK_DIR/temp
convert $ICON_FILE $WORK_DIR/temp/icon.png
cp $WORK_DIR/temp/icon-0.png $ICON_16_DIR/d-lan.png
cp $WORK_DIR/temp/icon-1.png $ICON_24_DIR/d-lan.png
cp $WORK_DIR/temp/icon-2.png $ICON_32_DIR/d-lan.png
cp $WORK_DIR/temp/icon-3.png $ICON_48_DIR/d-lan.png
cp $WORK_DIR/temp/icon-4.png $ICON_64_DIR/d-lan.png
cp $WORK_DIR/temp/icon-5.png $ICON_128_DIR/d-lan.png
cp $WORK_DIR/temp/icon-6.png $ICON_256_DIR/d-lan.png

cp d-lan.desktop $DESKTOP_DIR/

cp -R ./DEBIAN $DEB_DIR/
echo "For wich architecture ?"
select arch in "i386" "amd64"; do
    sed -i "s/_ARCH_/$arch/g" $DEB_DIR/DEBIAN/control
    break
done

sed -i "s/_INST_SIZE_/$(du -sx --exclude DEBIAN $DEB_DIR | cut -f 1)/g" $DEB_DIR/DEBIAN/control

version=$($CORE_FILE --version) 
vhead=$(echo $version | cut -d' ' -f 1)
vtag=$(echo $version | cut -d' ' -f 2)
vdate=$(echo $version | cut -d' ' -f 3)
 

sed -i "s/_VERSION_/$vhead-$vtag/g" $DEB_DIR/DEBIAN/control

cd $WORK_DIR
sudo chown -R root:root $DEB_DIR
dpkg-deb --build d-lan
mv d-lan.deb $INST_DIR/D-LAN-$vhead$vtag-$vdate-$arch.deb
 
rm -Rf $WORK_DIR/temp
