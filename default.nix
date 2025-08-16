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
    beam-sqlite = self.callHackageDirect {
      pkg = "beam-sqlite";
      ver = "0.5.2.0";
      sha256 = "9lWuaS4NDNB9HJdHLyU5UQrdcrXjwKc+9UPyoALBcH0=";
    } {};
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
