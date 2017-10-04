@echo off

:: Place your MSVC15 path here :)
SET PATH=C:\MSVC15\BIN\;

:: Compile the bootloader
ML.EXE /nologo /AT /c bootload.asm

:: Link the bootloader, the warning is normal
LINK.EXE /nologo /TINY /NOD bootload.obj, bootload.bin, NUL, NUL, NUL

PAUSE