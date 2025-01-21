Name "Catala Setup"
Outfile "catala-setup.exe"

InstallDir $PROGRAMFILES64\Catala

LicenseData LICENSE.txt
Icon logo.ico

Page license
Page directory
Page instfiles
UninstPage uninstConfirm
UninstPage instfiles

# Install section
Section "catala-install"

  SetOutPath $INSTDIR
  File catala.exe
  File catala-lsp.exe
  File libgmp-10.dll
# TODO catala-format
  WriteUninstaller $INSTDIR\catala-uninstall.exe

  ReadRegStr $0 HKCU "Environment" Path
  StrCpy $1 "$INSTDIR"
  StrCpy $2 "$1;$0"
  WriteRegStr HKCU "Environment" Path "$2"

SectionEnd

# create a section to define what the uninstaller does.
# the section will always be named "Uninstall"
Section "un.catala-uninstall"

  # marche moyen..
  EnVar::DeleteValue "PATH" "$INSTDIR"
  POP $0
  DetailPrint "EnVar::DeleteValue returned=|$0|"

  # Delete installed file
  Delete $INSTDIR\catala.exe
  Delete $INSTDIR\catala-lsp.exe
  Delete $INSTDIR\libgmp-10.dll

  # Delete the uninstaller
  Delete $INSTDIR\catala-uninstall.exe

  # Delete the directory
  RMDir $INSTDIR

SectionEnd
