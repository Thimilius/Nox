@echo off

pushd %~dp0\..\
odin build nox-boot -out:noxb.exe -show-timings -o:speed -disable-assert -no-bounds-check
popd
