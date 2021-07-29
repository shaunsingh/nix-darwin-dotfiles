#!/bin/bash

echo " "
echo "Warning: the following script will install multiple applications, packages, and files on your computer. Make sure to read through the file before proceding"
echo "The script will take a while to run, as several tools need to run from source. I recommend leaving it overnight if you have a slower machine"
echo "Alternatively, you can Install the prebuilt releases for neovim and emacs, and run the select parts of the script manually"
echo " "
echo " "
read -n 1 -s -r -p "Press any key to continue"

echo "Install homebrew"
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

echo "Enrolling in homebrew dev branch"
export HOMEBREW_DEVELOPER=1

echo "Installing fonts"
brew tap homebrew/cask-fonts
brew install --cask font-sf-pro
brew install --cask font-sf-mono

echo "Installing Dependencies"
brew install ranger htop ripgrep
brew install yabai
brew install skhd
brew install neofetch

echo "Install doom emacs"
brew tap d12frosted/emacs-plus
brew install emacs-plus@28 --with-xwidgets --with-native-comp --with-elrumo2-icon
git clone --depth 1 https://github.com/hlissner/doom-emacs ~/.emacs.d
 ~/.emacs.d/bin/doom install

echo "setting up email"
brew install mu
brew install isync
brew install msmtp
mkdir ~/.mbsync
mu init --maildir=~/.mbsync --my-address=shaunsingh0207@gmail.com
mu index
mbsync --all

echo "setting up org"
cd
git clone https://github.com/shaunsingh/org.git

echo "Installing Latex Packages"
brew install --cask basictex
sudo tlmgr install dvipng dvisvgm l3packages xcolor soul adjustbox collectbox amsmath siunitx cancel mathalpha capt-of chemfig wrapfig mhchem

#Clone bar into default Übersicht location
brew install --cask ubersicht
echo "installing bar"
cd vimrc-dotfiles
cp -r powerbar $HOME/Library/Application\ Support/Übersicht/widgets/powerbar
cd

echo "Building Neovim nightly"
brew install neovim --HEAD
nvim --headless +PackerSync +qa

echo "grabbing wallpapers"
cd ~vimrc-dotfiles
cp -R wallpapers ~

echo "Setting up fish"
brew install --cask kitty
brew install aspell fish starship

echo "Setting fish as Default Prompt"
sudo sh -c 'echo $(which fish) >> /etc/shells'
chsh -s $(which fish)

echo "Setting up iterm"
defaults write com.googlecode.iterm2.plist PrefsCustomFolder -string "~/.config/iterm2"
defaults write com.googlecode.iterm2.plist LoadPrefsFromCustomFolder -bool true

echo "Setting preferences"
 osascript -e 'tell application "System Preferences" to quit'
# Keep-alive: update existing `sudo` time stamp until `.macos` has finished
while true; do sudo -n true; sleep 60; kill -0 "$$" || exit; done 2>/dev/null &

# Set computer name (as done via System Preferences → Sharing)
sudo scutil --set ComputerName "macOS"
sudo scutil --set HostName "macOS"
sudo scutil --set LocalHostName "macOS"
sudo defaults write /Library/Preferences/SystemConfiguration/com.apple.smb.server NetBIOSName -string "macOS"
# Disable the sound effects on boot
sudo nvram SystemAudioVolume=" "
# Increase window resize speed for Cocoa applications
defaults write NSGlobalDomain NSWindowResizeTime -float 0.001
# Save to disk (not to iCloud) by default
defaults write NSGlobalDomain NSDocumentSaveNewDocumentsToCloud -bool false
# Disable the “Are you sure you want to open this application?” dialog
defaults write com.apple.LaunchServices LSQuarantine -bool false
# Remove duplicates in the “Open With” menu (also see `lscleanup` alias)
/System/Library/Frameworks/CoreServices.framework/Frameworks/LaunchServices.framework/Support/lsregister -kill -r -domain local -domain system -domain user
# Disable Resume system-wide
defaults write com.apple.systempreferences NSQuitAlwaysKeepsWindows -bool false
# Disable automatic termination of inactive apps
defaults write NSGlobalDomain NSDisableAutomaticTermination -bool true
# Disable the crash reporter
defaults write com.apple.CrashReporter DialogType -string "none"
# Disable smart dashes as they’re annoying when typing code
defaults write NSGlobalDomain NSAutomaticDashSubstitutionEnabled -bool false
# Disable automatic period substitution as it’s annoying when typing code
defaults write NSGlobalDomain NSAutomaticPeriodSubstitutionEnabled -bool false
# Disable “natural” (Lion-style) scrolling
defaults write NSGlobalDomain com.apple.swipescrolldirection -bool false
# Increase sound quality for Bluetooth headphones/headsets
defaults write com.apple.BluetoothAudioAgent "Apple Bitpool Min (editable)" -int 40
# Enable full keyboard access for all controls
# (e.g. enable Tab in modal dialogs)
defaults write NSGlobalDomain AppleKeyboardUIMode -int 3

    # Enable lid wakeup
    sudo pmset -a lidwake 1

    # Sleep the display after 15 minutes
    sudo pmset -a displaysleep 15

    # Remove the sleep image file to save disk space
    sudo rm /private/var/vm/sleepimage
    # Create a zero-byte file instead…
    sudo touch /private/var/vm/sleepimage
    # …and make sure it can’t be rewritten
    sudo chflags uchg /private/var/vm/sleepimage

    # Require password immediately after sleep or screen saver begins
    defaults write com.apple.screensaver askForPassword -int 1
    defaults write com.apple.screensaver askForPasswordDelay -int 0

    # Save screenshots to the desktop
    defaults write com.apple.screencapture location -string "${HOME}/Desktop"

    # Save screenshots in PNG format (other options: BMP, GIF, JPG, PDF, TIFF)
    defaults write com.apple.screencapture type -string "png"

    # Enable subpixel font rendering on non-Apple LCDs
    defaults write NSGlobalDomain AppleFontSmoothing -int 1

    # Enable HiDPI display modes (requires restart)
    sudo defaults write /Library/Preferences/com.apple.windowserver DisplayResolutionEnabled -bool true

    #========
    # Finder
    #========

    # Dont show icons for hard drives, servers, and removable media on the desktop
    defaults write com.apple.finder ShowExternalHardDrivesOnDesktop -bool false
    defaults write com.apple.finder ShowHardDrivesOnDesktop -bool false
    defaults write com.apple.finder ShowMountedServersOnDesktop -bool false
    defaults write com.apple.finder ShowRemovableMediaOnDesktop -bool false

    # Finder: show all filename extensions
    defaults write NSGlobalDomain AppleShowAllExtensions -bool true

    # Disable the warning when changing a file extension
    defaults write com.apple.finder FXEnableExtensionChangeWarning -bool false

    # Avoid creating .DS_Store files on network or USB volumes
    defaults write com.apple.desktopservices DSDontWriteNetworkStores -bool true
    defaults write com.apple.desktopservices DSDontWriteUSBStores -bool true

    # Disable disk image verification
    defaults write com.apple.frameworks.diskimages skip-verify -bool true
    defaults write com.apple.frameworks.diskimages skip-verify-locked -bool true
    defaults write com.apple.frameworks.diskimages skip-verify-remote -bool true

    # Enable snap-to-grid for icons on the desktop and in other icon views
    /usr/libexec/PlistBuddy -c "Set :DesktopViewSettings:IconViewSettings:arrangeBy grid" ~/Library/Preferences/com.apple.finder.plist
    /usr/libexec/PlistBuddy -c "Set :FK_StandardViewSettings:IconViewSettings:arrangeBy grid" ~/Library/Preferences/com.apple.finder.plist
    /usr/libexec/PlistBuddy -c "Set :StandardViewSettings:IconViewSettings:arrangeBy grid" ~/Library/Preferences/com.apple.finder.plist

    # Increase grid spacing for icons on the desktop and in other icon views
    # TODO: check
    # /usr/libexec/PlistBuddy -c "Set :DesktopViewSettings:IconViewSettings:gridSpacing 100" ~/Library/Preferences/com.apple.finder.plist

    # Increase the size of icons on the desktop and in other icon views
    /usr/libexec/PlistBuddy -c "Set :DesktopViewSettings:IconViewSettings:iconSize 52" ~/Library/Preferences/com.apple.finder.plist

    # Show the /Volumes folder
    sudo chflags nohidden /Volumes

    # Expand the following File Info panes:
    # “General”, “Open with”, and “Sharing & Permissions”
    defaults write com.apple.finder FXInfoPanesExpanded -dict \
        General -bool true \
        OpenWith -bool true \
        Privileges -bool true

    #====================
    # Dock and Dashboard
    #====================

    # Show indicator lights for open applications in the Dock
    defaults write com.apple.dock show-process-indicators -bool true

    # Speed up Mission Control animations
    defaults write com.apple.dock expose-animation-duration -float 0.1

    # Disable Dashboard
    defaults write com.apple.dashboard mcx-disabled -bool true

    # Don’t show Dashboard as a Space
    defaults write com.apple.dock dashboard-in-overlay -bool true

    # Don’t automatically rearrange Spaces based on most recent use
    defaults write com.apple.dock mru-spaces -bool false

    # Automatically hide and show the Dock
    # TODO: maybe change this
    defaults write com.apple.dock autohide -bool true

    # Disable send and reply animations in Mail.app
    defaults write com.apple.mail DisableReplyAnimations -bool true
    defaults write com.apple.mail DisableSendAnimations -bool true

    # Copy email addresses as `foo@example.com` instead of `Foo Bar <foo@example.com>` in Mail.app
    defaults write com.apple.mail AddressesIncludeNameOnPasteboard -bool false

    # Disable inline attachments (just show the icons)
    defaults write com.apple.mail DisableInlineAttachmentViewing -bool true

    #===========
    # Spotlight
    #===========

    # Change indexing order and disable some search results
    defaults write com.apple.spotlight orderedItems -array \
        '{"enabled" = 1;"name" = "APPLICATIONS";}' \
        '{"enabled" = 1;"name" = "SYSTEM_PREFS";}' \
        '{"enabled" = 1;"name" = "DIRECTORIES";}' \
        '{"enabled" = 1;"name" = "PDF";}' \
        '{"enabled" = 1;"name" = "DOCUMENTS";}' \
        '{"enabled" = 0;"name" = "FONTS";}' \
        '{"enabled" = 0;"name" = "MESSAGES";}' \
        '{"enabled" = 0;"name" = "CONTACT";}' \
        '{"enabled" = 0;"name" = "EVENT_TODO";}' \
        '{"enabled" = 0;"name" = "IMAGES";}' \
        '{"enabled" = 0;"name" = "BOOKMARKS";}' \
        '{"enabled" = 0;"name" = "MUSIC";}' \
        '{"enabled" = 0;"name" = "MOVIES";}' \
        '{"enabled" = 0;"name" = "PRESENTATIONS";}' \
        '{"enabled" = 0;"name" = "SPREADSHEETS";}' \
        '{"enabled" = 0;"name" = "SOURCE";}'

    # Load new settings before rebuilding the index
    killall mds > /dev/null 2>&1
    # Make sure indexing is enabled for the main volume
    sudo mdutil -i on / > /dev/null
    # Rebuild the index from scratch
    sudo mdutil -E / > /dev/null

    #==================
    # Activity Monitor
    #==================

    # Show the main window when launching Activity Monitor
    defaults write com.apple.ActivityMonitor OpenMainWindow -bool true

    # Show all processes in Activity Monitor
    defaults write com.apple.ActivityMonitor ShowCategory -int 0

    # Sort Activity Monitor results by CPU usage
    defaults write com.apple.ActivityMonitor SortColumn -string "CPUUsage"
    defaults write com.apple.ActivityMonitor SortDirection -int 0

    #===========================================================
    # Address Book, Dashboard, iCal, TextEdit, and Disk Utility
    #===========================================================

    # Enable the debug menu in Address Book
    defaults write com.apple.addressbook ABShowDebugMenu -bool true

    # Enable Dashboard dev mode (allows keeping widgets on the desktop)
    defaults write com.apple.dashboard devmode -bool true

    # Enable the debug menu in iCal (pre-10.8)
    defaults write com.apple.iCal IncludeDebugMenu -bool true

    # Use plain text mode for new TextEdit documents
    defaults write com.apple.TextEdit RichText -int 0
    # Open and save files as UTF-8 in TextEdit
    defaults write com.apple.TextEdit PlainTextEncoding -int 4
    defaults write com.apple.TextEdit PlainTextEncodingForWrite -int 4

    # Enable the debug menu in Disk Utility
    defaults write com.apple.DiskUtility DUDebugMenuEnabled -bool true
    defaults write com.apple.DiskUtility advanced-image-options -bool true

    # Auto-play videos when opened with QuickTime Player
    defaults write com.apple.QuickTimePlayerX MGPlayMovieOnOpen -bool true

    #===============
    # Mac App Store
    #===============

    # Enable the WebKit Developer Tools in the Mac App Store
    defaults write com.apple.appstore WebKitDeveloperExtras -bool true

    # Enable Debug Menu in the Mac App Store
    defaults write com.apple.appstore ShowDebugMenu -bool true

    # Allow the App Store to reboot machine on macOS updates
    defaults write com.apple.commerce AutoUpdateRestartRequired -bool true

    #========
    # Photos
    #========

    # Prevent Photos from opening automatically when devices are plugged in
    defaults -currentHost write com.apple.ImageCapture disableHotPlug -bool true

    #============================
    # Kill affected applications
    #============================

    for app in "Activity Monitor" \
        "Address Book" \
        "Calendar" \
        "cfprefsd" \
        "Contacts" \
        "Dock" \
        "Finder" \
        "Google Chrome" \
        "Mail" \
        "Photos" \
        "SystemUIServer" \
        "Terminal" \
        "iCal"; do
        killall "${app}" &> /dev/null
    done

    echo "macOS system preferences ✅"

echo "Cleanup"
osascript -l JavaScript -e "Application('System Events').appearancePreferences.darkMode = true"
#brew services start yabai
#brew services start skhd
#brew services start emacs-plus@28
brew cleanup -s
echo "Done!"

echo "                Further User Setup                   "
echo "-----------------------------------------------------"
echo "                                                     "
echo "     Install Vimium and Firenvim for Vi in Chrome    "
echo "                                                     "
echo "      Wallpapers are stored in ~/wallpapers          "
echo "   Cloned dotfiles are stored in ~/vimrc-dotfiles    "
echo "                                                     "
echo "  Install the ligaturized sf mono font from ~/fonts  "
echo "                                                     "
echo "      Thats it, thanks for downloading, enjoy :)     "
