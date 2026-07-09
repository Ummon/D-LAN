[code]

#define BundleDir "setup_bundle"

#define AppName "D-LAN"
#define ExePath BundleDir + "/D-LAN.Core.exe"
#define Version GetStringFileInfo(ExePath, 'ProductVersion')
#define VersionTag GetStringFileInfo(ExePath, 'VersionTag')
#define BuildTime GetStringFileInfo(ExePath, 'BuildTime')

[Setup]
AppName={#AppName}
AppVersion={#Version} {#VersionTag} - {#BuildTime}
SetupIconFile=../../Common/resources/icon.ico
DefaultDirName={commonpf}/{#AppName}
DefaultGroupName={#AppName}
UninstallDisplayIcon={app}/D-LAN.Core.exe
Compression=lzma2
SolidCompression=yes
OutputDir=Installations
OutputBaseFilename={#AppName}-{#Version}{#VersionTag}-{#BuildTime}-Setup
ArchitecturesInstallIn64BitMode=x64compatible
ArchitecturesAllowed=x64compatible

[Files]
Source: "{#BundleDir}/*"; DestDir: "{app}"; Flags: comparetimestamp recursesubdirs createallsubdirs

[Icons]
Name: "{group}\D-LAN"; Filename: "{app}/D-LAN.GUI.exe"; WorkingDir: "{app}"

[Languages]
; Name has to be coded as ISO-639 (two letters).
Name: "en"; MessagesFile: "compiler:Default.isl,../../translations/d_lan.en.isl"
Name: "fr"; MessagesFile: "compiler:Languages/French.isl,../../translations/d_lan.fr.isl"
Name: "de"; MessagesFile: "compiler:Languages/German.isl,../../translations/d_lan.de.isl"
Name: "it"; MessagesFile: "compiler:Languages/Italian.isl,../../translations/d_lan.it.isl"
Name: "de"; MessagesFile: "compiler:Languages/German.isl,../../translations/d_lan.de.isl"
Name: "de"; MessagesFile: "compiler:Languages/German.isl,../../translations/d_lan.de.isl"

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
Filename: {app}/D-LAN.Core.exe; Parameters: -u; RunOnceId: "DLANUninstall";
Filename: {sys}/netsh.exe; Parameters: "firewall delete allowedprogram program=""{app}/D-LAN.Core.exe"""; Flags: runhidden; MinVersion: 0,5.01.2600sp2; Tasks: Firewall; RunOnceId: "DLANFirewallUninstall";


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
