# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running `nixos-help`).

{ config, pkgs, st , ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];


# Enable Flakes and the new command-line tool
  nix.settings.experimental-features = [ "nix-command" "flakes" ];



  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "archimedes"; # Define your hostname.
  # Pick only one of the below networking options.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.
  networking.networkmanager.enable = true;  # Easiest to use and most distros use this by default.

  # Set your time zone.
  time.timeZone = "Asia/Jakarta";

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Select internationalisation properties.
  # i18n.defaultLocale = "en_US.UTF-8";
  # console = {
  #   font = "Lat2-Terminus16";
  #   keyMap = "us";
  #   useXkbConfig = true; # use xkbOptions in tty.
  # };
  #hardware.opengl.enable = true;
  #hardware.opengl.driSupport = true;
  # Enable the X11 windowing system.
  # services.xserver.enable = true;
  services.xserver = {
	enable = true;
	autorun= true;
	windowManager = {
		xmonad = { 
			enable = true;
			enableContribAndExtras = true;
			extraPackages = hpkgs: [
				hpkgs.xmobar
				];
			};
                dwm = {
			enable = true;
			};
		awesome = {
      			enable = true;
      			luaModules = with pkgs.luaPackages; [
        		luarocks # is the package manager for Lua modules
        		luadbi-mysql # Database abstraction layer
			vicious
			];

    			};
		};
	displayManager = {
                #startx.enable = true;
		defaultSession = "none+xmonad";
		#lightdm.greeters.gtk = { 
		gdm = {
                       enable = true;
		       wayland = true;
			};
	
   		};
	};
# W O R K E D DWM :D 
   nixpkgs.overlays = [
    (final: prev: {
      dwm = prev.dwm.overrideAttrs (old: { src = /home/mhabibmanan/Suckless/dwm ;});
      })
   # (final: prev: {
   #   dmenu = prev.dmenu.overrideAttrs (old: { src = /home/mhabibmanan/Suckless/dmenu ;});
   # })
    (self: super: { 
      waybar = super.waybar.overrideAttrs (oldAttrs: {
        mesonFlags = oldAttrs.mesonFlags ++ [ "-Dexperimental=true" ];
      });
      })
];


# hyprland
programs.hyprland = {
    enable = true;
    xwayland.enable = true;
};

  # Configure keymap in X11
  # services.xserver.layout = "us";
  # services.xserver.xkbOptions = "eurosign:e,caps:escape";

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  # Enable sound.
#  sound.enable = false;
#  hardware.pulseaudio.enable = true;
# Remove sound.enable or turn it off if you had it set previously, it seems to cause conflicts with pipewire
#sound.enable = false;

# rtkit is optional but recommended
  security.rtkit.enable = true;
  services.pipewire = {
  enable = true;
  alsa.enable = true;
  alsa.support32Bit = true;
  pulse.enable = true;
  # If you want to use JACK applications, uncomment this
  #jack.enable = true;
  };

  # Enable touchpad support (enabled default in most desktopManager).
  services.xserver.libinput.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.mhabibmanan = {
     isNormalUser = true;
     extraGroups = [ "wheel" "networkmanager" "video"]; # Enable ‘sudo’ for the user.
     packages = with pkgs; [
       #firefox
       tree
     ];
   };
  nixpkgs.config.allowUnfree = true;
  # List packages installed in system profile. To search, run:
  # $ nix search wget
   environment.systemPackages = with pkgs; [
     vim # Do not forget to add an editor to edit configuration.nix! The Nano editor is also installed by default.
    gcc_multi 
    git
    xorg.xinit
    xorg.libXScrnSaver
    # Terminal
     #rxvt_unicode
     alacritty
     gnumake
     binutils
     nix
     #XMonad
     stack
     ghc
     cabal-install
     haskellPackages.xmobar
     haskellPackages.xmonad
     haskellPackages.xmonad-contrib
     haskellPackages.xmonad-extras
     # S U C K L E S S
     #st
     st.packages."${system}".st-snazzy  
     imlib2-nox
     dmenu
     #(dmenu.override { conf = /home/mhabibmanan/Suckless/dmenu/config.h; })  # W O R K E D    
     wofi
     rofi
     scrot
     xclip
     trayer
     maim
     dunst
    #Bar
     waybar
     eww
     eww-wayland
     polybar
   #SystemTools
     killall
     alsa-utils
     gvfs
     pavucontrol
     acpi
     ps_mem
     htop
     neofetch
     light
     picom
     libnotify
     feh
     networkmanagerapplet
     networkmanager-openvpn
     bash-completion
     nix-bash-completions
     ffmpeg
     ffmpegthumbnailer
   # Apps
     telegram-desktop
     pcmanfm
     vscodium
     mpv
     jamesdsp
     simplescreenrecorder
     kdeconnect
     #LibraryQt5
     libsForQt5.kdeconnect-kde
     libsForQt5.kirigami2
     libsForQt5.kirigami-addons
     libsForQt5.ffmpegthumbs
     spotify
     krita
   #Theme
    lxappearance
    papirus-icon-theme
    yaru-remix-theme
    bibata-cursors

   #Github 
    ruby
    jekyll
    gitAndTools.gitFull
    bundler
    bundix 
    gh
]; 


fonts.fonts = with pkgs; [
  (nerdfonts.override { fonts = [ "FiraCode" "JetBrainsMono" "Terminus" ]; })
  noto-fonts
  noto-fonts-cjk
  noto-fonts-emoji
  liberation_ttf
  font-awesome
];
nixpkgs.config.packageOverrides = pkgs: {
    vaapiIntel = pkgs.vaapiIntel.override { enableHybridCodec = true; };
  };
  hardware.opengl = {
    enable = true;
    extraPackages = with pkgs; [
      intel-media-driver # LIBVA_DRIVER_NAME=iHD
      vaapiIntel         # LIBVA_DRIVER_NAME=i965 (older but works better for Firefox/Chromium)
      vaapiVdpau
      libvdpau-va-gl
    ];
  };
xdg = {
  portal = {
    enable = true;
    extraPortals = with pkgs; [
      xdg-desktop-portal-wlr
      xdg-desktop-portal-gtk
    ];
  };
};
  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  programs.kdeconnect.enable = true;
  programs.light.enable = true;
# programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };

  # List services that you want to enable:
  services.gvfs.enable = true;
  # Enable the OpenSSH daemon.
    services.openvpn.servers.vpn = {
    config = "config /home/mhabibmanan/ProtonVPN/jp-free-05.protonvpn.net.udp.ovpn";
    autoStart = true;
    authUserPass.password = "LGfcpSKwpClDdtbggfMRAWTw520G20wM";
    authUserPass.username = "pNIPO1a9QsbsQ6rRGXdsV7cnz5DyWnkQ";
    updateResolvConf = true;
  };


  #ENABLE ZRAM
  zramSwap.enable = true;
  zramSwap.memoryPercent = 200;
  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;
 nix.settings.auto-optimise-store = true;
  # Copy the NixOS configuration file and link it from the resulting system
  # (/run/current-system/configuration.nix). This is useful in case you
  # accidentally delete configuration.nix.
# system.copySystemConfiguration = true;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It's perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "23.05"; # Did you read the comment?

}

