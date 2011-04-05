infile(config.pri, SOLUTIONS_LIBRARY, yes): CONFIG += qtservice-uselib
TEMPLATE += fakelib
QTSERVICE_LIBNAME = $$qtLibraryTarget(QtSolutions_Service-2.6)
TEMPLATE -= fakelib
QTSERVICE_LIBDIR = $$PWD/lib
unix:qtservice-uselib:!qtservice-buildlib:QMAKE_RPATHDIR += $$QTSERVICE_LIBDIR
