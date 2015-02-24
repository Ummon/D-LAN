[code]
#define QtDir "D:/Qt/5.4/mingw491_32"
#define MingwDir "D:/Qt/Tools/mingw491_32"
#define ProtoBufDir "D:/protobuf"
#define ApplicationDir "../.."

#define AppName "D-LAN"
#define ExePath ApplicationDir + "/Core/output/release/D-LAN.Core.exe"
#define Version GetStringFileInfo(ExePath, 'ProductVersion')
#define VersionTag GetStringFileInfo(ExePath, 'VersionTag')
#define BuildTime GetStringFileInfo(ExePath, 'BuildTime')

[Setup]
AppName={#AppName}
AppVersion={#Version} {#VersionTag} - {#BuildTime}
SetupIconFile={#ApplicationDir}/Common/ressources/icon.ico
DefaultDirName={pf}/{#AppName}
DefaultGroupName={#AppName}
UninstallDisplayIcon={app}/D-LAN.Core.exe
Compression=lzma2
SolidCompression=yes
OutputDir=Installations
OutputBaseFilename={#AppName}-{#Version}{#VersionTag}-{#BuildTime}-Setup

[Files]
Source: "{#ApplicationDir}/Core/output/release/D-LAN.Core.exe"; DestDir: "{app}"; Flags: comparetimestamp
Source: "{#ApplicationDir}/GUI/output/release/D-LAN.GUI.exe"; DestDir: "{app}"; Flags: comparetimestamp
Source: "{#ApplicationDir}/translations/*.qm"; DestDir: "{app}/languages"; Flags: comparetimestamp
Source: "{#ApplicationDir}/styles/*"; DestDir: "{app}/styles"; Flags: comparetimestamp recursesubdirs createallsubdirs
Source: "{#ApplicationDir}/GUI/ressources/emoticons/*"; DestDir: "{app}/Emoticons"; Flags: comparetimestamp recursesubdirs createallsubdirs
Source: "{#QtDir}/bin/Qt5Core.dll"; DestDir: "{app}"; Flags: comparetimestamp
Source: "{#QtDir}/bin/Qt5Gui.dll"; DestDir: "{app}"; Flags: comparetimestamp
Source: "{#QtDir}/bin/Qt5Network.dll"; DestDir: "{app}"; Flags: comparetimestamp
Source: "{#QtDir}/bin/Qt5Widgets.dll"; DestDir: "{app}"; Flags: comparetimestamp
Source: "{#QtDir}/bin/Qt5WinExtras.dll"; DestDir: "{app}"; Flags: comparetimestamp
Source: "{#QtDir}/bin/Qt5Xml.dll"; DestDir: "{app}"; Flags: comparetimestamp
Source: "{#QtDir}/bin/icuin53.dll"; DestDir: "{app}"; Flags: comparetimestamp
Source: "{#QtDir}/bin/icuuc53.dll"; DestDir: "{app}"; Flags: comparetimestamp
Source: "{#QtDir}/bin/libgcc_s_dw2-1.dll"; DestDir: "{app}"; Flags: comparetimestamp
Source: "{#QtDir}/bin/libwinpthread-1.dll"; DestDir: "{app}"; Flags: comparetimestamp
Source: "{#QtDir}/bin/libstdc++-6.dll"; DestDir: "{app}"; Flags: comparetimestamp
Source: "{#QtDir}/plugins/platforms/qwindows.dll"; DestDir: "{app}/platforms"; Flags: comparetimestamp
;Source: {#ProtoBufDir}/src/.libs/libprotobuf-7.dll; DestDir: {app}; Flags: comparetimestamp;

[Icons]
Name: "{group}\D-LAN"; Filename: "{app}/D-LAN.GUI.exe"; WorkingDir: "{app}"

[Languages]
; Name has to be coded as ISO-639 (two letters).
Name: "en"; MessagesFile: "compiler:Default.isl,{#ApplicationDir}/translations/d_lan.en.isl"
Name: "fr"; MessagesFile: "compiler:Languages/French.isl,{#ApplicationDir}/translations/d_lan.fr.isl"

[Tasks]
Name: "Firewall"; Description: {cm:firewallException}; MinVersion: 0,5.01.2600sp2;
Name: "ResetSettings"; Description: {cm:resetSettings}

[Run]
Filename: "{sys}/netsh.exe"; Parameters: "firewall add allowedprogram ""{app}/D-LAN.Core.exe"" ""D-LAN.Core"" ENABLE ALL"; Flags: runhidden; MinVersion: 0,5.01.2600sp2; Tasks: Firewall
Filename: "{app}/D-LAN.Core.exe"; Parameters: "--reset-settings"; Flags: RunHidden; Description: "Reset settings"; Tasks: ResetSettings
Filename: "{app}/D-LAN.Core.exe"; Parameters: "-i --lang {language}"; Flags: RunHidden; Description: "Install the D-LAN service and define the language"
Filename: "{app}/D-LAN.GUI.exe"; Parameters: "--lang {language}"; Flags: RunHidden; Description: "Define the language for the GUI"
Filename: "{app}/D-LAN.GUI.exe"; Flags: nowait postinstall runasoriginaluser; Description: "{cm:launchDLAN}"

[UninstallRun]
Filename: {app}/D-LAN.Core.exe; Parameters: -u;
Filename: {sys}/netsh.exe; Parameters: "firewall delete allowedprogram program=""{app}/D-LAN.Core.exe"""; Flags: runhidden; MinVersion: 0,5.01.2600sp2; Tasks: Firewall; 

[code] 
// Will stop the Core service.
function PrepareToInstall(var NeedsRestart: Boolean): String;
var
  ResultCode: integer;
begin
  Exec(ExpandConstant('{sys}/sc.exe'), 'stop "D-LAN Core"', '', SW_HIDE, ewWaitUntilTerminated, ResultCode);
  NeedsRestart := False;
  Result := '';
end;
