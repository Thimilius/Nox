@echo off

pushd %~dp0\..\
odin run nox-boot -out:noxb.exe -define:TRACY_ENABLE=true -o:speed
popd
