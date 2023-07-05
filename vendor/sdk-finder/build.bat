@echo off

cl /O2 /nologo /c /EHsc source/microsoft_craziness.cpp
lib /NOLOGO /OUT:lib/windows/sdk-finder.lib microsoft_craziness.obj Ole32.lib OleAut32.lib Advapi32.lib
