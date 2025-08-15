{ system ? builtins.currentSystem
, obelisk ? import ./.obelisk/impl {
    inherit system;
    iosSdkVersion = "16.1";
  }
}:
with obelisk;
project ./. ({ pkgs, ... }: {
  overrides = self: super: {                                     
    beam-core = self.callHackage "beam-core" "0.9.2.1" {};
    beam-sqlite = self.callHackage "beam-sqlite" "0.5.1.2" {};
    beam-migrate = self.callHackage "beam-migrate" "0.5.1.2" {};
    sqlite-simple = self.callHackage "sqlite-simple" "0.4.18.2" {};
    aeson = self.callHackage "aeson" "2.0.3.0" {};
  };
  android.applicationId = "systems.obsidian.obelisk.examples.minimal";
  android.displayName = "Obelisk Minimal Example";
  ios.bundleIdentifier = "systems.obsidian.obelisk.examples.minimal";
  ios.bundleName = "Obelisk Minimal Example";
})
