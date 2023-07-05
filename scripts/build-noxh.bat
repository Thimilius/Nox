@echo off

pushd %~dp0\..\
noxs nox-self -out:noxh -verbose -verbose-more-timings -o3 -disable-assert
popd
