{ config, pkgs, ... }:

{
  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "mhabibmanan";
  home.homeDirectory = "/home/mhabibmanan";

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "23.05";
  home.packages = with pkgs; [
  pfetch
  discord
  firefox-esr
  ];
  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
programs.neovim = {
  enable = true;
  extraConfig = ''
    set number relativenumber
  '';

};


}
