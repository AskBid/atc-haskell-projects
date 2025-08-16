{ system ? builtins.currentSystem
, obelisk ? import ./.obelisk/impl {
    inherit system;
    iosSdkVersion = "16.1";
  }
}:
with obelisk;
project ./. ({ pkgs, ... }: {
  overrides = self: super: {  
    beam-core = self.callHackage "beam-core" "0.10.0.0" {};
    beam-postgres = (self.callHackageDirect {
      pkg = "beam-postgres";
      ver = "0.5.3.1";
      sha256 = "0v3mhhmnw8x5ll463msjfa7q2p6h4zqm7ql14nmhj8fz09a40wwa";
    } {}).overrideAttrs (oldAttrs: {doCheck = false;});
    beam-automigrate = self.callHackageDirect {
      pkg = "beam-automigrate";
      ver = "0.1.7.0";
      sha256 = "zQTdesKW82An7SYwc61SO1RJB7Vt0ZjxDvKKcxKxUtE=";
    } {};
    aeson = self.callHackage "aeson" "2.0.3.0" {};
    beam-migrate = self.callHackageDirect {
      pkg = "beam-migrate";
      ver = "0.5.2.0";
      sha256 = "rzPgCMOfqnowdEK98HzhmBsSXh9xzB4vUG+E62v5o2k="; 
    } {};
  };
  android.applicationId = "systems.obsidian.obelisk.examples.minimal";
  android.displayName = "Obelisk Minimal Example";
  ios.bundleIdentifier = "systems.obsidian.obelisk.examples.minimal";
  ios.bundleName = "Obelisk Minimal Example";
})
