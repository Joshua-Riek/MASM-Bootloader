;---------------------------------------------------
;
; Details
; ----------------------------------
; This file is part of a 16 bit	MASM and C hobby os,
; developed by Joshua Riek, 2017. The file bootload.asm
; is a simple FAT12 bootloader that loads the root directory,
; scans for the kernel image and then loads it with a far jump.
;
; Copyright 2017 Joshua Riek. No part of this file may be
; reproduced, in any form or by any other means, without
; permission in writing from the author.
;
;---------------------------------------------------

.386						; Compile for a 80386 CPU
 option segment:use16				; Force 16 bit segments instead of default 32 bit
.model tiny					; Tiny memory model
.code						; Start of code segment
 org 07c00h					; Bootloader entry point
 main:

;---------------------------------------------------
; Disk description table

    jmp short bootStrap				; Jump over OEM / BIOS param block
    nop

    OEMName	      db "BOOT    "		; Disk label
    bytesPerSector    dw 512			; Bytes per sector
    sectorsPerCluster db 1			; Sectors per cluster
    reservedSectors   dw 1			; Reserved sectors
    fats	      db 2			; Number of fats
    rootDirEntries    dw 224			; Number of entries in root dir
    sectors	      dw 2880			; Logical sectors
    mediaType	      db 0f0h			; Media descriptor byte
    fatSectors        dw 9			; Sectors per FAT
    sectorsPerTrack   dw 18			; Sectors per track
    heads	      dw 2			; Number of sides/heads
    hiddenSectors     dd 0			; Hidden sectors
    hugeSectors	      dd 0			; LBA sectors
    biosDriveNum      db 0			; Drive number
    reserved	      db 0			; This is not used
    bootSignature     db 41			; Drive signature
    volumeId	      dd 0			; Volume ID
    volumeLabel	      db "BOOT VOLUME"		; Volume Label
    fatTypeLabel      db "FAT12   "		; File system type


;---------------------------------------------------
bootStrap:
;
; Summary:  Start of the main bootloader code.
;
; Date: 9/16/2017
;---------------------------------------------------
    cli
    xor ax, ax					; Location of the bootloader divided by 16
    mov ds, ax					; Set data segment to where we are loaded
    add ax, 20h					; Skip over the size of the bootloader divided by 16 (512 / 16)
    mov ss, ax					; Set segment register to current location (start of the stack)
    mov sp, 4096				; Set ss:sp to the top of the 4k stack
    sti

    mov [biosDriveNum], dl			; Store bootdrive number


;---------------------------------------------------
loadRootDir:
;
; Summary:  Load the root directory from the disk,
;	    use the folowing formulas as a reference.
;
; Formulas: startOfRoot  = (fats * fatSectors) + reservedSectors = logical 19
;	    numberOfRoot = (rootDirEntries * 32) / bytesPerSector = 14
;	    userData	 = startOfRoot + numberOfRoot = logical 33
;
; Date: 9/16/2017
;---------------------------------------------------
    xor ax, ax					; Calculate startOfRoot
    mov al, [fats]				; Move number of fats into ax
    mov bx, [fatSectors]			; Move fat sectors into bx
    mul bx					; Multiply, output in ax
    add ax, [reservedSectors] 			; Increase ax by ReservedSectors
    mov [startOfRoot], ax
						; Calculate numberOfRoot
    mov ax, [rootDirEntries]			; Move max root entries into ax
    mov bx, 32					; Move the size per entry into bx
    mul bx					; Multiply, output in ax
    mov bx, [bytesPerSector]			; Move the number of bytes per sector into bx
    div	bx					; Divide, output in ax
    mov [numberOfRoot], ax
						; Calculate userData
    mov bx, [startOfRoot]			; Move startOfRoot to bx, ax is allready numberOfRoot
    add ax, bx					; Add the two together
    mov [userData], ax

    mov ax, [startOfRoot]			; Now we read the first block of the root dir
    call LBACHS					; Calculate LBA to CHS for int 13h

    mov	bx, [diskBuffer]			; Load the root dir in diskBuffer
    mov al, BYTE PTR [numberOfRoot]		; Number of root sectors
    mov	ah, 2					; Read Sectors func of int 13h
    pusha

  readRootDir:
    popa					; Preserve registers because int 13h alters them
    pusha

    stc						; Carry is cleared on success
    int	    13h					; Call int 13h (BIOS disk I/O)
    call resetFloppy				; Reset the floppy disk
    jnc	loadedRoot				; If no carry then root has been loaded!

    call resetFloppy				; Else reset the floppy disk
    jnc readRootDir				; If no carry try again

    mov si, offset bootFailure			; Print out the boot error message
    call print
    mov si, offset rootNotFound			; The root directory cannot be found
    call print
    jmp error					; Reboot system


;---------------------------------------------------
loadedRoot:
;
; Summary:  The root directory has now been loaded
;	    into the diskBuffer, time to find the
;	    kernel file, the name resides in the
;	    Short (8.3) Filename format.
;
; Note:	    The default system's kernel file is
;	    named kernel.bin.
;
; Date: 9/16/2017
;---------------------------------------------------
    popa					; Restore registers from reading the dir
    mov	di, [diskBuffer]			; Set di to our diskBuffer
    mov	cx, [rootDirEntries]			; Search through all of the root dir entrys for the kernel
    xor ax, ax					; Clear ax for the file entry offset

  searchRoot:
    push cx					; Save current cx value to look for the filename
    lea	si, kernelImage				; Load the kernel filename
    mov	cx, 11					; Compare first 11 bytes
    repe cmpsb					; Compare si and di cx times
    je foundFile				; We found the kernel :)

    add	ax, 32					; File entry offset
    mov	di, [diskBuffer]			; Point back to the start of the entry
    add	di, ax					; Add the offset to point to the next entry
    pop	cx
    loop searchRoot				; Continue to search for the kenel kernel

    mov si, offset bootFailure			; Print out the boot error message
    call print
    mov si, offset kernelNotFound
    call print
    jmp error					; Reboot system

  foundFile:
    mov	ax, [di + 15]				; Get the file cluster at offset 26
    mov	[cluster], ax				; Load file table

    mov ax, 1					; Convert the first sector of FAT
    call LBACHS					; Calculate LBA to CHS for int 13h

    mov	al, BYTE PTR [fatSectors]		; Number of FAT sectors
    mov	ah, 2					; Read Sectors func of int 13h
    pusha

  loadFat:
    popa					; Preserve registers because int 13h alters them
    pusha

    stc						; Carry is cleared on success
    int	    13h					; Call int 13h (BIOS disk I/O)
    call resetFloppy				; Else reset the floppy disk
    jnc	loadedFat				; If no carry then FAT has been loaded!

    call resetFloppy				; Else reset the floppy disk
    jnc	loadFat					; If no carry try again

    mov si, offset bootFailure			; Print out the boot error message
    call print
    mov si, offset kerneLoadError		; Error while loading the kernel
    call print
    jmp error					; Reboot system


;---------------------------------------------------
loadedFat:
;
; Summary:  Load the FAT from the disk by calculating
;	    the size and clusters of the kernel then
;	    jump to the now loaded kernel.
;
; Formulas: clusterStart = ((cluster) - 2) * sectorsPerCluster + userData
;
; Date: 9/24/2017
;---------------------------------------------------
    popa
    mov ax, 2000h
    mov es, ax					; We will load the kernel at 2000h:0000h
    mov bx, 0

loadFileSector:					; Calculate clusterStart
    xor dx, dx					; Clear remander
    mov ax, [cluster]				; Current cluster number
    sub	ax, 2					; Subtract 2
    mov bl, [sectorsPerCluster]			; Sectors per cluster is a byte value
    mul bx	        			; Multiply (cluster) - 2) * SectorsPerCluster
    add ax, [userData]				; Add the userData
    call LBACHS					; Calculate LBA to CHS for int 13h

    mov ax, 2000h				; Load the kernel at 2000h
    mov es, ax					; Point es:bx to where we will load the kernel
    mov bx, [pointer]				; Increase the buffer by the pointer offset
    mov	ah, 2					; Read Sectors func of int 13h
    mov al, 1					; Read one sector

    stc						; Carry is cleared on success
    int	    13h					; Call int 13h (BIOS disk I/O)
    call resetFloppy				; Else reset the floppy disk and try again
    jnc	calculateNextSector			; If no carry then find the next sector

    call resetFloppy				; Else reset the floppy disk and try again
    jmp	loadFileSector

  calculateNextSector:
    mov ax, [cluster]				; Current cluster number
    xor dx, dx					; We want to multiply by 1.5 (6/4 is equal to 1.5)
    mov bx, 6					; Multiply the cluster by the numerator
    mul bx					; Return value in ax & remainder in dx
    mov bx, 4					; Divide the cluster by the denominator
    div bx					; Return value in ax & remainder in dx

    mov si, [diskBuffer]			; Primary FAT entry point
    add si, ax					; Point to the next cluster in the FAT entry
    mov ax, [si]				; Load ax to the next cluster in FAT

    or dx, dx					; Is the cluster caluclated even?
    jz evenSector

  oddSector:
    shr ax, 4					; Drop the first 4 bits	of next cluster
    jmp nextSectorCalculated

  evenSector:
    and ax, 0fffh				; Drop the last 4 bits of next cluster

  nextSectorCalculated:
    mov [cluster], ax				; Store the new cluster
    add	[pointer], 512				; Add to the pointer offset

    cmp	ax, 0ff0h				; Are we at the end of the file?
    jl	loadFileSector				; Jump to kernel

  kernelJump:					; Time to far jump to the kernel!
    push es					; Push es for far jump
    push bx					; Push bx for far jump
    retf

  error:					; If this is hit then something went wrong :c
    mov ah, 0
    int	    16h					; Get a single keypress
    db 0eah					; Hard coded reboot
    dw 0000h
    dw 0ffffh


;---------------------------------------------------
; Bootloader routines below
;---------------------------------------------------


;---------------------------------------------------
resetFloppy PROC USES ax dx
;
; Summary:  Reset the floppy disk.
;
; Return:   Carry is set on error.
;
; Date: 9/16/2017
;---------------------------------------------------
    stc						; Set carry flag (if no error carry is cleared)
    mov ax, 0					; Reset Drive func of int 13h
    mov dl, [biosDriveNum]			; Drive to reset
    int	    13h					; Call int 13h (BIOS disk I/O)

    ret
resetFloppy ENDP


;---------------------------------------------------
LBACHS PROC USES ax
;
; Summary:  Convert Logical Block Addressing (LBA)
;	    to Cylinder/Head/Sector Addressing (CHS).
;
; Input:    AX = LBA
;
; Formulas: absoluteSector = (LBA / sectorsPerTrack) + 1
;	    absoluteTrack  = (LBA / sectorsPerTrack) / heads (Take Remainder value)
;	    absoluteHead   = (LBA / sectorsPerTrack) / heads (Take quotient value)
;
; Source:   http://www.osdever.net/tutorials/view/lba-to-chs
;
; Note:	    A Cylinder is the same as a Track.
;
; Date: 9/16/2017
;---------------------------------------------------
    push ax					; Calculate absoluteSector
    xor	dx, dx					; Prep ax:dx for output
    div	[sectorsPerTrack]			; Divide LBA by SectorsPerTrack
    inc dl					; Add one to output
    mov	cl, dl					; Move the absoluteSector to cl for int 13h

    pop ax					; Calculate absoluteHead and absoluteTrack
    xor	dx, dx					; Prep ax:dx for output
    div [sectorsPerTrack]			; Divide LBA by SectorsPerTrack
    xor	dx, dx					; Prep ax:dx for output
    div	[heads]					; Now divide by Heads
    mov	dh, dl					; Move the absoluteHead	to dl for int 13h
    mov	ch, al					; Move the absoluteTrack to ch for int 13h

    mov	dl, [biosDriveNum]			; Set correct device for int 13h

    ret
LBACHS ENDP


;---------------------------------------------------
print PROC USES ax si
;
; Summary:  Print out a string.
;
; Inputs:   String in si.
;
; Date: 9/16/2017
;---------------------------------------------------
@@loop:
    lodsb					; Load byte from si to al
    or al, al					; If al is empty stop looping
    jz @@done					; Done looping and return
    mov ah, 0eh					; Teletype output
    int     10h					; Video interupt
    jmp @@loop					; Loop untill string is null
  @@done:
    ret
print ENDP


;---------------------------------------------------
; Bootloader strings and variables
;---------------------------------------------------


    bootFailure	   db "Boot failure: 0x000", 0	; Boot failure string
    rootNotFound   db "1", 0			; Root not found error
    kernelNotFound db "2", 0			; Kernel not found error
    kerneLoadError db "3", 0			; Kernel loading error

    numberOfRoot   dw 0				; Number of root entries
    startOfRoot    dw 0				; Start of the root directory
    userData	   dw 0				; Start of where the user data begins

    cluster	   dw 0				; Cluster of the file we want to load
    pointer	   dw 0				; Pointer into Buffer, for loading kernel
    diskBuffer	   dw 24576			; Disk buffer begins at 6000h

    kernelImage	   db "KERNEL  BIN"		; Kernel name in 8.3 format
		   byte 510 - ($ - main) dup (0); Pad remainder of boot sector with zeros
		   dw 0aa55h			; Boot signature
END main

