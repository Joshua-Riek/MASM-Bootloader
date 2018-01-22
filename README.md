# MASM Bootloader
This is a minimal 16 bit, real mode bootloader written in MASM! Please note
that if you are attempting to write a bootloader in MASM, I would not recommend
doing so, use NASM instead, for it is infinitely easier.

## Compiling
To compile you must set your MASM compiler onto your PATH, I recommend to
use MASM615 or equivalent.

``` batch
ML.EXE /nologo /AT /c bootload.asm

LINK.EXE /nologo /TINY /NOD bootload.obj, bootload.bin, NUL, NUL, NUL
```

## Resources
* [OSDev] Is a great website for any Hobby OS developer.
* [MASM] Used for the bootloader.
* [NASM] Please use this instead of MASM.
* [imdisk] & [dd] To write the operating system files to a floppy image.
* [QEMU] Image emulator for testing the os.

[QEMU]:   http://www.qemu.org/
[imdisk]: http://www.ltr-data.se/opencode.html/
[dd]:	    http://uranus.chrysocome.net/linux/rawwrite/dd-old.htm
[OSDev]:  http://wiki.osdev.org/Main_Page
[MASM]:   http://www2.hawaii.edu/~pager/312/masm%20615%20downloading.htm
[NASM]:   http://www.nasm.us/index.php