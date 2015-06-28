TEMPLATE = subdirs
CONFIG += ordered
MAKEFILE = Makefile-Core
SUBDIRS = Common \
   Common/LogManager \
   Core/HashCache \
   Core/FileManager \
   Core/PeerManager \
   Core/UploadManager \
   Core/DownloadManager \
   Core/NetworkListener \
   Core/ChatSystem \
   Core/RemoteControlManager \
   Core

TRANSLATIONS = translations/d_lan_core.fr.ts \
   translations/d_lan_core.ko.ts
CODECFORTR = UTF-8
