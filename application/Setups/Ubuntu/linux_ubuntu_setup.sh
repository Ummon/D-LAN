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

# Default dependencies, it may change depending the architecture and the Linux distribution.
DEP_QTCORE=">= 4:4.8"
DEP_QTGUI=">= 4:4.8"
DEP_QTNETWORK=">= 4:4.8"
DEP_PROTOBUF=">= 2.4.1"
DEP_LIBC=">= 2.15"
DEP_LIBSTDCPP=">= 4.6.3"
DEP_LIBGCC=">= 1:4.6.3"

sudo rm -Rf $DEB_DIR
mkdir -p $DESKTOP_DIR $STYLES_DIR $I18N_DIR $ICON_16_DIR $ICON_24_DIR $ICON_32_DIR $ICON_48_DIR $ICON_64_DIR $ICON_128_DIR $ICON_256_DIR

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
echo "For which architecture ?"
select arch in "i386" "amd64" "armhf"; do
    CONTROL="$DEB_DIR/DEBIAN/control"
    case $arch in
       "armhf")
          DEP_LIBC=">=2.13"
          ;;
    esac
    sed -i "s/_ARCH_/$arch/g" $CONTROL
    sed -i "s/_DEP_QTCORE_/$DEP_QTCORE/g" $CONTROL
    sed -i "s/_DEP_QTGUI_/$DEP_QTGUI/g" $CONTROL
    sed -i "s/_DEP_QTNETWORK_/$DEP_QTNETWORK/g" $CONTROL
    sed -i "s/_DEP_PROTOBUF_/$DEP_PROTOBUF/g" $CONTROL
    sed -i "s/_DEP_LIBC_/$DEP_LIBC/g" $CONTROL
    sed -i "s/_DEP_LIBSTDCPP_/$DEP_LIBSTDCPP/g" $CONTROL
    sed -i "s/_DEP_LIBGCC_/$DEP_LIBGCC/g" $CONTROL
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
sudo chmod -R 0644 $DEB_DIR
sudo chmod -R +X $DEB_DIR
sudo chmod 0755 $DEB_DIR/DEBIAN/postinst $DEB_DIR/DEBIAN/prerm
sudo chmod 0777 $APP_DIR/D-LAN.Core $APP_DIR/D-LAN.GUI
dpkg-deb --build d-lan
mv d-lan.deb $INST_DIR/D-LAN-$vhead$vtag-$vdate-$arch.deb
 
rm -Rf $WORK_DIR/temp
