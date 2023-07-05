@echo off

pushd %~dp0\..\
cloc nox-boot --exclude-dir=tracy
popd
