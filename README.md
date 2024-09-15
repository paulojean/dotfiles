# Nixos and home-manager configuration files

## NixOS build

```
sudo nixos-rebuild switch --flake .#linux
# or
sudo nixos-rebuild switch --flake .#wsl
```

## Home-manager build

```
home-manager switch --flake .#aiur
```
