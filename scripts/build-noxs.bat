@echo off

pushd %~dp0\..\
noxb nox-self -out:noxs -verbose -verbose-more-timings -o3 -disable-assert
popd
