TEMPLATE = subdirs
CONFIG += ordered
MAKEFILE = Makefile-GUI
SUBDIRS = Common \
   Common/LogManager \
   Common/RemoteCoreController \
   GUI
TRANSLATIONS = translations/d_lan_gui.fr.ts \
   translations/d_lan_gui.ko.ts
CODECFORTR = UTF-8
