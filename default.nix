{ system ? builtins.currentSystem
, obelisk ? import ./.obelisk/impl {
    inherit system;
    iosSdkVersion = "16.1";

    # You must accept the Android Software Development Kit License Agreement at
    # https://developer.android.com/studio/terms in order to build Android apps.
    # Uncomment and set this to `true` to indicate your acceptance:
    # config.android_sdk.accept_license = false;

    # In order to use Let's Encrypt for HTTPS deployments you must accept
    # their terms of service at https://letsencrypt.org/repository/.
    # Uncomment and set this to `true` to indicate your acceptance:
    # terms.security.acme.acceptTerms = false;
  }
}:
with obelisk;
#
# let
#   nixpkgsOverlay = final: prev: {
#     haskellPackages = prev.haskellPackages.override {
#       overrides = hfinal: hprev: {
#         haskell-language-server = hfinal.callCabal2nix "haskell-language-server" (final.fetchFromGitHub {
#           owner = "haskell";
#           repo = "haskell-language-server";
#           rev = "2.9.0.0"; # Use the desired version of HLS
#           sha256 = "sha256-AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA="; # Replace with the correct hash
#         }) {};
#       };
#     };
#   };
# in

project ./. ({ ... }: {
  # overrides = nixpkgsOverlay;
  android.applicationId = "systems.obsidian.obelisk.examples.minimal";
  android.displayName = "Obelisk Minimal Example";
  ios.bundleIdentifier = "systems.obsidian.obelisk.examples.minimal";
  ios.bundleName = "Obelisk Minimal Example";
})
