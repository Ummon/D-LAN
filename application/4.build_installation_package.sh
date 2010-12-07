#!/usr/bin/env bash
# Makes the installation package, depending the platform it can be an .exe, deb, rpm, etc..

set -o errexit

# MS.Windows. Inno Setup (isetup) + QuickStart Pack (ispack) must be both installed. The Inno Setup directory must be placed in the PATH variable environnment.
iscc aybabtu.iss
