{ lib, config, pkgs, ... }:

with lib;

let
  cfg = config.mine.keylayout;

  compiledLayout = pkgs.runCommand "layout.xkm" {
    nativeBuildInputs = [ pkgs.xorg.xkbcomp ];
  } "xkbcomp ${cfg.layoutFile} $out";

  xcapeExpression = concatStringsSep ";" (mapAttrsToList
    (name: value: "${name}=${concatStringsSep "|" (toList value)}")
    cfg.xcapeConfig);

  loadLayout = pkgs.writeScriptBin "lll" ''
    #!${pkgs.stdenv.shell}
    ${pkgs.xorg.xkbcomp}/bin/xkbcomp ${compiledLayout} $DISPLAY
    ${optionalString (cfg.xcapeConfig != {}) ''
      ${pkgs.procps-ng}/bin/pkill -u $USER xcape
      ${pkgs.xcape}/bin/xcape -e ${escapeShellArg xcapeExpression} &
    ''}
  '';

in

{

  options.mine.keylayout = {
    enable = mkEnableOption "my keylayout config";

    layoutFile = mkOption {
      type = types.path;
      description = "Path to xkb layout file.";
    };

    xcapeConfig = mkOption {
      type = with types; attrsOf (uniq str);
      default = {};
      visible = true;
      description = ''
        Xcape allows a modifier key to be used as another key when it is pressed
        and released on its own. This is the mapping from a modifier key to a
        list of keys that should be pressed and released in sequence. A list of
        keys can be found in
        <filename>${pkgs.xlibs.xorgproto}/include/X11/keysymdef.h</filename>
        (without the <literal>XK_</literal> prefix).
        </para><para>
        Note that shifted keys must
        be specified as a shift key followed by the key to be pressed rather
        than the actual name of the character. For example to generate
        <literal>{</literal> the expression
        <literal>ModKey = [ "Shift_L" "bracketleft" ]</literal> could be used
        (assuming that you have a key with <literal>{</literal> above
        <literal>[</literal>).
        </para><para>
        You can also specify the modifier keys in decimal (prefix #), octal
        (#0), or hexadecimal (#0x). It will be interpreted as a keycode unless
        no corresponding key name is found.
      '';
      example = literalExample ''
        {
          Shift_L = "Escape";
          Control_L = [ "Control_L" "O" ];
        }
      '';
    };
  };

  config = mkIf cfg.enable {

    # When in the DigitalOcean web console: `setxkbmap -layout us` (on local machine) and type in correct programmer dvorak
    i18n.consoleUseXkbConfig = true;

    environment.systemPackages = mkIf config.services.xserver.enable [ loadLayout ];

    services.xserver = {
      autoRepeatDelay = 200;
      autoRepeatInterval = 25;

      # Basic keymap, is used for i18n virtual consoles
      layout = "us";
      xkbVariant = "dvp";
      xkbOptions = "caps:backspace";
      displayManager.sessionCommands = "${loadLayout}/bin/lll";
    };

  };
}
