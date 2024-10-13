; Script generated by the HM NIS Edit Script Wizard.

; HM NIS Edit Wizard helper defines
!define PRODUCT_NAME "Brassica"
!define PRODUCT_VERSION "1.0.0"
!define PRODUCT_WEB_SITE "http://bradrn.com/brassica"
!define PRODUCT_DIR_REGKEY "Software\Microsoft\Windows\CurrentVersion\App Paths\Brassica"
!define PRODUCT_UNINST_KEY "Software\Microsoft\Windows\CurrentVersion\Uninstall\${PRODUCT_NAME}"
!define PRODUCT_UNINST_ROOT_KEY "HKLM"
!define PRODUCT_STARTMENU_REGVAL "NSIS:StartMenuDir"

; MUI 1.67 compatible ------
!include "MUI.nsh"

; MUI Settings
!define MUI_ABORTWARNING
!define MUI_ICON "${NSISDIR}\Contrib\Graphics\Icons\modern-install.ico"
!define MUI_UNICON "${NSISDIR}\Contrib\Graphics\Icons\modern-uninstall.ico"

; Welcome page
!insertmacro MUI_PAGE_WELCOME
; License page
!insertmacro MUI_PAGE_LICENSE ".\LICENSE"
; Directory page
!insertmacro MUI_PAGE_DIRECTORY
; Start menu page
var ICONS_GROUP
!define MUI_STARTMENUPAGE_NODISABLE
!define MUI_STARTMENUPAGE_DEFAULTFOLDER "Brassica"
!define MUI_STARTMENUPAGE_REGISTRY_ROOT "${PRODUCT_UNINST_ROOT_KEY}"
!define MUI_STARTMENUPAGE_REGISTRY_KEY "${PRODUCT_UNINST_KEY}"
!define MUI_STARTMENUPAGE_REGISTRY_VALUENAME "${PRODUCT_STARTMENU_REGVAL}"
!insertmacro MUI_PAGE_STARTMENU Application $ICONS_GROUP
; Instfiles page
!insertmacro MUI_PAGE_INSTFILES
; Finish page
!define MUI_FINISHPAGE_RUN "$INSTDIR\brassica-gui.exe"
!insertmacro MUI_PAGE_FINISH

; Uninstaller pages
!insertmacro MUI_UNPAGE_INSTFILES

; Language files
!insertmacro MUI_LANGUAGE "English"

; MUI end ------

Name "${PRODUCT_NAME} ${PRODUCT_VERSION}"
OutFile "brassica-setup-${PRODUCT_VERSION}.exe"
InstallDir "$PROGRAMFILES\Brassica"
InstallDirRegKey HKLM "${PRODUCT_DIR_REGKEY}" ""
ShowInstDetails show
ShowUnInstDetails show

Section "MainSection" SEC01
  SetOutPath "$INSTDIR"
  SetOverwrite try
  File ".\deploy\brassica-gui.exe"
  File ".\deploy\brassica.exe"
  File ".\deploy\brassica-pb.exe"
  File ".\deploy\D3Dcompiler_47.dll"
  SetOutPath "$INSTDIR\examples"
  File ".\deploy\examples\english.bsc"
  File ".\deploy\examples\english.lex"
  File ".\deploy\examples\latin2port.bsc"
  File ".\deploy\examples\latin2port.lex"
  File ".\deploy\examples\thai.bsc"
  File ".\deploy\examples\thai.lex"
  SetOutPath "$INSTDIR\generic"
  File ".\deploy\generic\qtuiotouchplugin.dll"
  SetOutPath "$INSTDIR\iconengines"
  File ".\deploy\iconengines\qsvgicon.dll"
  SetOutPath "$INSTDIR\imageformats"
  File ".\deploy\imageformats\qgif.dll"
  File ".\deploy\imageformats\qico.dll"
  File ".\deploy\imageformats\qjpeg.dll"
  File ".\deploy\imageformats\qsvg.dll"
  SetOutPath "$INSTDIR"
  File ".\deploy\libgcc_s_seh-1.dll"
  File ".\deploy\libstdc++-6.dll"
  File ".\deploy\libwinpthread-1.dll"
  SetOutPath "$INSTDIR\networkinformation"
  File ".\deploy\networkinformation\qnetworklistmanager.dll"
  SetOutPath "$INSTDIR"
  File ".\deploy\opengl32sw.dll"
  SetOutPath "$INSTDIR\platforms"
  File ".\deploy\platforms\qwindows.dll"
  SetOutPath "$INSTDIR"
  File ".\deploy\Qt6Core.dll"
  File ".\deploy\Qt6Gui.dll"
  File ".\deploy\Qt6Network.dll"
  File ".\deploy\Qt6Svg.dll"
  File ".\deploy\Qt6Widgets.dll"
  SetOutPath "$INSTDIR\styles"
  File ".\deploy\styles\qwindowsvistastyle.dll"
  SetOutPath "$INSTDIR\tls"
  File ".\deploy\tls\qcertonlybackend.dll"
  File ".\deploy\tls\qopensslbackend.dll"
  File ".\deploy\tls\qschannelbackend.dll"
  SetOutPath "$INSTDIR\translations"
  File ".\deploy\translations\qt_ar.qm"
  File ".\deploy\translations\qt_bg.qm"
  File ".\deploy\translations\qt_ca.qm"
  File ".\deploy\translations\qt_cs.qm"
  File ".\deploy\translations\qt_da.qm"
  File ".\deploy\translations\qt_de.qm"
  File ".\deploy\translations\qt_en.qm"
  File ".\deploy\translations\qt_es.qm"
  File ".\deploy\translations\qt_fa.qm"
  File ".\deploy\translations\qt_fi.qm"
  File ".\deploy\translations\qt_fr.qm"
  File ".\deploy\translations\qt_gd.qm"
  File ".\deploy\translations\qt_he.qm"
  File ".\deploy\translations\qt_hr.qm"
  File ".\deploy\translations\qt_hu.qm"
  File ".\deploy\translations\qt_it.qm"
  File ".\deploy\translations\qt_ja.qm"
  File ".\deploy\translations\qt_ko.qm"
  File ".\deploy\translations\qt_lv.qm"
  File ".\deploy\translations\qt_nl.qm"
  File ".\deploy\translations\qt_nn.qm"
  File ".\deploy\translations\qt_pl.qm"
  File ".\deploy\translations\qt_pt_BR.qm"
  File ".\deploy\translations\qt_ru.qm"
  File ".\deploy\translations\qt_sk.qm"
  File ".\deploy\translations\qt_tr.qm"
  File ".\deploy\translations\qt_uk.qm"
  File ".\deploy\translations\qt_zh_CN.qm"
  File ".\deploy\translations\qt_zh_TW.qm"

; Shortcuts
  !insertmacro MUI_STARTMENU_WRITE_BEGIN Application
  CreateDirectory "$SMPROGRAMS\$ICONS_GROUP"
  CreateShortCut "$SMPROGRAMS\$ICONS_GROUP\Brassica.lnk" "$INSTDIR\brassica-gui.exe"
  CreateShortCut "$DESKTOP\Brassica.lnk" "$INSTDIR\brassica-gui.exe"
  !insertmacro MUI_STARTMENU_WRITE_END
SectionEnd

Section -AdditionalIcons
  SetOutPath $INSTDIR
  !insertmacro MUI_STARTMENU_WRITE_BEGIN Application
  CreateShortCut "$SMPROGRAMS\$ICONS_GROUP\Uninstall.lnk" "$INSTDIR\uninst.exe"
  !insertmacro MUI_STARTMENU_WRITE_END
SectionEnd

Section -Post
  WriteUninstaller "$INSTDIR\uninst.exe"
  WriteRegStr HKLM "${PRODUCT_DIR_REGKEY}" "" "$INSTDIR\brassica-gui.exe"
  WriteRegStr ${PRODUCT_UNINST_ROOT_KEY} "${PRODUCT_UNINST_KEY}" "DisplayName" "$(^Name)"
  WriteRegStr ${PRODUCT_UNINST_ROOT_KEY} "${PRODUCT_UNINST_KEY}" "UninstallString" "$INSTDIR\uninst.exe"
  WriteRegStr ${PRODUCT_UNINST_ROOT_KEY} "${PRODUCT_UNINST_KEY}" "DisplayIcon" "$INSTDIR\brassica-gui.exe"
  WriteRegStr ${PRODUCT_UNINST_ROOT_KEY} "${PRODUCT_UNINST_KEY}" "DisplayVersion" "${PRODUCT_VERSION}"
  WriteRegStr ${PRODUCT_UNINST_ROOT_KEY} "${PRODUCT_UNINST_KEY}" "URLInfoAbout" "${PRODUCT_WEB_SITE}"
SectionEnd


Function un.onUninstSuccess
  HideWindow
  MessageBox MB_ICONINFORMATION|MB_OK "Brassica was successfully removed from your computer."
FunctionEnd

Function un.onInit
  MessageBox MB_ICONQUESTION|MB_YESNO|MB_DEFBUTTON2 "Are you sure you want to completely remove Brassica and all of its components?" IDYES +2
  Abort
FunctionEnd

Section Uninstall
  !insertmacro MUI_STARTMENU_GETFOLDER "Application" $ICONS_GROUP
  Delete "$INSTDIR\uninst.exe"
  Delete "$INSTDIR\translations\qt_zh_TW.qm"
  Delete "$INSTDIR\translations\qt_zh_CN.qm"
  Delete "$INSTDIR\translations\qt_uk.qm"
  Delete "$INSTDIR\translations\qt_tr.qm"
  Delete "$INSTDIR\translations\qt_sk.qm"
  Delete "$INSTDIR\translations\qt_ru.qm"
  Delete "$INSTDIR\translations\qt_pt_BR.qm"
  Delete "$INSTDIR\translations\qt_pl.qm"
  Delete "$INSTDIR\translations\qt_nn.qm"
  Delete "$INSTDIR\translations\qt_nl.qm"
  Delete "$INSTDIR\translations\qt_lv.qm"
  Delete "$INSTDIR\translations\qt_ko.qm"
  Delete "$INSTDIR\translations\qt_ja.qm"
  Delete "$INSTDIR\translations\qt_it.qm"
  Delete "$INSTDIR\translations\qt_hu.qm"
  Delete "$INSTDIR\translations\qt_hr.qm"
  Delete "$INSTDIR\translations\qt_he.qm"
  Delete "$INSTDIR\translations\qt_gd.qm"
  Delete "$INSTDIR\translations\qt_fr.qm"
  Delete "$INSTDIR\translations\qt_fi.qm"
  Delete "$INSTDIR\translations\qt_fa.qm"
  Delete "$INSTDIR\translations\qt_es.qm"
  Delete "$INSTDIR\translations\qt_en.qm"
  Delete "$INSTDIR\translations\qt_de.qm"
  Delete "$INSTDIR\translations\qt_da.qm"
  Delete "$INSTDIR\translations\qt_cs.qm"
  Delete "$INSTDIR\translations\qt_ca.qm"
  Delete "$INSTDIR\translations\qt_bg.qm"
  Delete "$INSTDIR\translations\qt_ar.qm"
  Delete "$INSTDIR\tls\qschannelbackend.dll"
  Delete "$INSTDIR\tls\qopensslbackend.dll"
  Delete "$INSTDIR\tls\qcertonlybackend.dll"
  Delete "$INSTDIR\styles\qwindowsvistastyle.dll"
  Delete "$INSTDIR\Qt6Widgets.dll"
  Delete "$INSTDIR\Qt6Svg.dll"
  Delete "$INSTDIR\Qt6Network.dll"
  Delete "$INSTDIR\Qt6Gui.dll"
  Delete "$INSTDIR\Qt6Core.dll"
  Delete "$INSTDIR\platforms\qwindows.dll"
  Delete "$INSTDIR\opengl32sw.dll"
  Delete "$INSTDIR\networkinformation\qnetworklistmanager.dll"
  Delete "$INSTDIR\libwinpthread-1.dll"
  Delete "$INSTDIR\libstdc++-6.dll"
  Delete "$INSTDIR\libgcc_s_seh-1.dll"
  Delete "$INSTDIR\imageformats\qsvg.dll"
  Delete "$INSTDIR\imageformats\qjpeg.dll"
  Delete "$INSTDIR\imageformats\qico.dll"
  Delete "$INSTDIR\imageformats\qgif.dll"
  Delete "$INSTDIR\iconengines\qsvgicon.dll"
  Delete "$INSTDIR\generic\qtuiotouchplugin.dll"
  Delete "$INSTDIR\examples\thai.lex"
  Delete "$INSTDIR\examples\thai.bsc"
  Delete "$INSTDIR\examples\latin2port.lex"
  Delete "$INSTDIR\examples\latin2port.bsc"
  Delete "$INSTDIR\examples\english.lex"
  Delete "$INSTDIR\examples\english.bsc"
  Delete "$INSTDIR\D3Dcompiler_47.dll"
  Delete "$INSTDIR\brassica.exe"
  Delete "$INSTDIR\brassica-pb.exe"
  Delete "$INSTDIR\brassica-gui.exe"

  Delete "$SMPROGRAMS\$ICONS_GROUP\Uninstall.lnk"
  Delete "$DESKTOP\Brassica.lnk"
  Delete "$SMPROGRAMS\$ICONS_GROUP\Brassica.lnk"

  RMDir "$SMPROGRAMS\$ICONS_GROUP"
  RMDir "$INSTDIR\translations"
  RMDir "$INSTDIR\tls"
  RMDir "$INSTDIR\styles"
  RMDir "$INSTDIR\platforms"
  RMDir "$INSTDIR\networkinformation"
  RMDir "$INSTDIR\imageformats"
  RMDir "$INSTDIR\iconengines"
  RMDir "$INSTDIR\generic"
  RMDir "$INSTDIR\examples"
  RMDir "$INSTDIR"

  DeleteRegKey ${PRODUCT_UNINST_ROOT_KEY} "${PRODUCT_UNINST_KEY}"
  DeleteRegKey HKLM "${PRODUCT_DIR_REGKEY}"
  SetAutoClose true
SectionEnd