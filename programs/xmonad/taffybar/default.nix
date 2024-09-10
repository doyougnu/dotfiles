{ mkDerivation, base, containers, directory, filepath, gtk3, lib
, X11
}:
mkDerivation {
  pname = "taffybar";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base containers directory filepath gtk3 X11
  ];
  license = "unknown";
  mainProgram = "taffybar";
}
