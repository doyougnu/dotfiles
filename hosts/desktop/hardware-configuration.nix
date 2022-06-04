# Do not modify this file!  It was generated by ‘nixos-generate-config’
# and may be overwritten by future invocations.  Please make changes
# to /etc/nixos/configuration.nix instead.
{ config, lib, pkgs, modulesPath, ... }:

{
  imports =
    [ (modulesPath + "/installer/scan/not-detected.nix")
    ];

  boot.initrd.availableKernelModules = [ "xhci_pci" "ahci" "nvme" "usbhid" "sd_mod" ];
  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ "kvm-amd" "iwlwifi" ];
  boot.kernelPackages = pkgs.linuxPackages_5_17;
  boot.extraModulePackages = [ ];
  # boot.extraModprobeConfig = ''
  #   options iwlwifi bt_coex_active=0 power_save=Y
  #   options iwldvm force_cam=N
  # '';

  fileSystems."/" =
    { device = "/dev/disk/by-uuid/a82812ea-100f-4615-945a-56cb9423b7ef";
      fsType = "ext4";
    };

  fileSystems."/home" =
    { device = "/dev/disk/by-uuid/b4918f4d-7532-47b7-ad2e-bfe8f8873322";
      fsType = "ext4";
    };

  fileSystems."/boot" =
    { device = "/dev/disk/by-uuid/80DA-660D";
      fsType = "vfat";
    };

  fileSystems."/store" =
    { device = "/dev/disk/by-uuid/db394961-c08e-406b-a7f1-4ecd7181b113";
      fsType = "ext4";
    };

  swapDevices =
    [ { device = "/dev/disk/by-uuid/26219740-ab2e-4aef-9d79-aa217b230834"; }
    ];

  hardware.cpu.amd.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;
}
