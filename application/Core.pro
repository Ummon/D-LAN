TEMPLATE = subdirs
CONFIG += ordered
MAKEFILE = Makefile-Core
SUBDIRS = Common \
   Common/LogManager \
   Core/FileManager \
   Core/PeerManager \
   Core/UploadManager \
   Core/DownloadManager \
   Core/NetworkListener \
   Core/RemoteControlManager \
   Core
