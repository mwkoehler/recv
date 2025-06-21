                name    RECV
                title   'NetBIOS Message Receiver'
;**********************************************************************
;*                                                                    *
;* RECV:  NetBIOS message receiver.                                   *
;*                                                                    *
;* General Description:                                               *
;*                                                                    *
;*       This program is an MS-DOS TSR that will wait for and display *
;* incoming NetBIOS datagrams.  Simply put it displays messages sent  *
;* to you from elsewhere in the network.                              *
;*                                                                    *
;*       This program was written based on NetBIOS documentation from *
;* both IBM and DEC and also on the SMB specification as documented   *
;* in "Microsoft Network/OpenNet File Sharing Protocol"  Intel PN     *
;* 138446 (Document version 2.0.  Nov. 7, 1988).                      *
;*                                                                    *
;*       This program acts as a replacement for the RCV.EXE utility   *
;* provided with DEC Pathworks v4.0.  In addition to providing the    *
;* features of DEC's RCV, this program provides the following         *
;* additional features:                                               *
;*                                                                    *
;*       1) The ability to unload the TSR                             *
;*       2) The ability to send messages as well as receive them.     *
;*       3) The ability to reply to messages                          *
;*       4) A "flash" feature that causes the message display to      *
;*          "time-out" and flash on and off the screen until the      *
;*          user acknowledges the message.  This allows RECV to be    *
;*          used without interfering with unattended applications or  *
;*          screen savers.                                            *
;*       5) Messages received from "Pathworks Mail" will "reply" back *
;*          to the mail name that sent the mail message.              *
;*       6) The ability to set a different 1 to 3 tone beep via a     *
;*          command line option                                       *
;*       7) The ability to temporarily disable reception of messages  *
;*                                                                    *
;*                                                                    *
;* Known problems:                                                    *
;*                                                                    *
;*       At this time RECV has the following known problems:          *
;*                                                                    *
;*       1) If RECV is unloaded from high-memory, the UMB isn't freed *
;*       2) If RECV is unloaded the outstanding receive request isn't *
;*          cleared properly.  This was working correctly in the past *
;*          it is believed that some change to RECV or DEC NetBIOS    *
;*          implementation has caused the logic to break.             *
;*       3) From time to time, if RECV pops up over PCMAIL.EXE,       *
;*          INT 16 will not report key presses to RECV.  The result   *
;*          is a hang until RECV times out and hands control back to  *
;*          PCMAIL.EXE.  After which everything is fine.              *
;*       4) RECV will freeze if it pops up over DOS 5's EDIT/QBASIC.  *
;*          This is caused by QBASIC modifying INT 16.  The result    *
;*          is that a key press is reported but the call to get the   *
;*          key causes a lock up.  This is a QBASIC problem.          *
;*       5) Some graphics mode apps (Lotus Allways) do not tell the   *
;*          BIOS that they have switched to graphics mode.  The       *
;*          result is that RECV pops up and displays garbage.         *
;*                                                                    *
;* Syntax (note that case is not significant):                        *
;*                                                                    *
;*       RECV </U> </TD #> </T1 #> </T2 #> </T3 #> </D|/E>            *
;*                                                                    *
;*       Where:   /U    = Uninstall                                   *
;*                /TD   = Set tone delay time in seconds              *
;*                /T1   = Set first tone pitch                        *
;*                /T2   = Set second tone pitch                       *
;*                /T3   = Set third tone pitch                        *
;*                /D    = Disable receiver                            *
;*                /E    = Enable receiver                             *
;*                                                                    *
;* Author:  Michael W. Koehler                                        *
;*          Simpson, Thacher & Bartlett                               *
;* Written: May 11, 1992                                              *
;*                                                                    *
;* Record of changes:                                                 *
;*                                                                    *
;* CHANGE  CHANGE    CHANGE  CHANGE                                   *
;* NUMBER  DATE      AUTHOR  DESCRIPTION                              *
;* ------  --------  ------  ---------------------------------------  *
;* 000001  05/11/92  MWK     Program Written.                         *
;* 000002  01/15/93  MWK     Fixed bug on some IBM AT's where INT 10h *
;*                           call would corrupt AH register.          *
;* 000003  01/29/93  MWK     Fixed to recognize new Pathworks 4.1     *
;*                           Notify Signature not old 4.0 signature.  *
;* 000004  04/05/93  MWK     Fixed to not pop up if keyboard input    *
;*                           is disabled.                             *
;* 000005  05/11/93  MWK     Fixed to not pop up if Windows is        *
;*                           running.  This doesn't necessarily       *
;*                           allow another Windows based receiver     *
;*                           to operate; just prevents weird lockups. *
;* 000006  05/24/93  MWK     Added wait before allowing popup after   *
;*                           Windows exits so that screen is not      *
;*                           blacked out when popup occurs.           *
;* 000007  06/24/93  MWK     Added support for /T? switches.  Also    *
;*                           changed tone code to use timer not code  *
;*                           loop for delay.                          *
;* 000008  08/13/93  MWK     Added support for short-cut text keys.   *
;* 000009  08/25/93  MWK     Extended get_string to allow line wrap.  *
;* 000010  10/18/93  MWK     Added check to block reply to node::name *
;*                           since there is no such NetBIOS name      *
;*                           defined to receive such messages.        *
;* 000011  12/14/93  MWK     Added [F7] = "Come in, please" on reply  *
;* 000012  01/03/94  MWK     Added /D switch to disable reception.    *
;*                           Added /E switch to enable reception.     *
;*                           Also added INT 2F enable/disable call.   *
;* 000013  01/06/94  MWK     Added display of time msg was recv'd.    *
;* 000014  01/10/94  MWK     Added support for VGA 640x480 mode 12h.  *
;* 000015  01/27/94  MWK     Moved message queue to EMS               *
;* 000016  02/16/94  MWK     Added support for 132 column text screen *
;*                           modes.  Also added STI to start of beep  *
;*                           routine since it depends on interrupt 8  *
;*                           for timing the beeps.                    *
;**********************************************************************
                .286
                .MODEL  TINY
                JUMPS
                LOCALS
                .CODE
                jmp     main


;**********************************************************************
;*  Constants                                                         *
;**********************************************************************
MSG_SIZE        equ     200
MAX_MSGS        equ     20
MSGS_START      equ     0D800h
MSGS_END        equ     MSGS_START+(MSG_SIZE*MAX_MSGS)

RES_STACK       equ     512

WINDOWS_ID      equ     16h
WIN_START       equ     05h
WIN_STOP        equ     06h

MULTIPLEX_ID    equ     0D9h
INSTALL_CHECK   equ     00h
SET_FROM_NAME   equ     01h
SET_DISABLED    equ     02h

NCB_SEND_DATAG  equ     20h
NCB_RECV_DATAG  equ     21h
NCB_ADD_NAME    equ     30h
NCB_DEL_NAME    equ     31h
NCB_CANCEL      equ     35h
NCB_ASYNC       equ     80h

WINDOW_HEIGHT   equ     5

VID_GRAPH       equ     0A000h
VID_MONO_TEXT   equ     0B000h
VID_COLOR_TEXT  equ     0B800h

SEG40           equ     40h             ; Keyboard buffer segment
KBFLAG          equ     17h             ; Offset of KBFLAG in SEG 40H
KBDATAP         equ     60h             ; Keyboard DATA port address
KBCMDBP         equ     61h             ; Keyboard COMMAND port B(?) address
KBCMDP          equ     64h             ; Keyboard COMMAND port address
PIC             equ     20h             ; Programmable Interrupt Controller
EOI             equ     20h             ; End of Interrupt code
CTRL            equ     04h             ; CTRL Shift flag
ALT             equ     08h             ; ALT  Shift flag


;**********************************************************************
;*  Structures                                                        *
;**********************************************************************
ncb_struc       struc
ncb_cmd         db      ?
ncb_rc          db      ?
ncb_lsn         db      ?
ncb_num         db      ?
ncb_buf         dd      ?
ncb_len         dw      ?
ncb_calln       db      16 dup (?)
ncb_name        db      16 dup (?)
ncb_rto         db      ?
ncb_sto         db      ?
ncb_post        dd      ?
ncb_lana        db      ?
ncb_cplt        db      ?
ncb_reservd     db      14 dup (?)
ncb_struc       ends


;**********************************************************************
;*  Miscellaneous functions (Resident)                                *
;**********************************************************************
;
;    Check if it is safe to do anything
;
;      Input:   None
;     Output:   AX = 0 if safe, -1 if not
;
safe            proc    near
                push    bx
                push    es
                ;
                ;  Check if DOS is busy
                ;
                les     bx, dword ptr cs:indos_ptr
                cmp     byte ptr cs:in_int28, 0
                je      @@Strict
                cmp     byte ptr es:[bx], 1
                jg      @@No
                jmp     @@Skip
@@Strict:       cmp     byte ptr es:[bx], 0
                jne     @@No
@@Skip:         les     bx, dword ptr cs:criterr_ptr
                cmp     byte ptr es:[bx], 0
                jne     @@No
                ;
                ;  Check if IRQ's are pending
                ;
                ;      Putting a 0Bh to the PIC (port 20h) asks for
                ;      status of IRQ's.  A 1 bit in the return
                ;      means the corresponding IRQ is pending.
                ;
                mov     al, 0Bh
                out     PIC, al
                in      al, PIC
                test    al, al
                jnz     @@No
                ;
                ;  Don't pop up if keyboard is disabled
                ;
                in      al, KBCMDP
                test    al, 10h
                jz      @@No
                ;
                ;  It's OK, return 0 in AX
                ;
@@Yes:
                xor     ax, ax
                jmp     @@Exit
                ;
                ;  Dangerous, return -1 in AX
                ;
@@No:
                mov     ax, 0FFh
                ;
                ;  Restore registers and return
                ;
@@Exit:
                pop     es
                pop     bx
                ret
safe            endp
;
;  Pop up if there's work and we can
;
;      Input:   None
;     Output:   None
;
pop_up          proc    near
                push    ax
                ;
                ;  Flag entry
                ;
                inc     byte ptr cs:busy
                ;
                ;  Is there work for us?
                ;
                cmp     byte ptr cs:send_flg, 1
                je      @@CanWe
                cmp     word ptr cs:msg_count, 0
                je      @@Exit
                ;
                ;  Can we pop up?
                ;
@@CanWe:
                cmp     byte ptr cs:busy, 1
                jne     @@Exit
                cmp     byte ptr cs:windows, 0
                jne     @@Exit
                cmp     word ptr cs:key_timer, 0
                jne     @@Exit
                cmp     word ptr cs:timer, 0
                jne     @@Exit
                ;
                ;  Check if it is safe
                ;
                call    safe
                test    ax, ax
                jnz     @@Exit
                ;
                ;  Enable interrupts
                ;
                sti
                ;
                ;  Do the work
                ;
                call    process
                ;
                ;  Done!
                ;
@@Exit:         dec     byte ptr cs:busy
                pop     ax
                ret
pop_up          endp
;
;    Beep
;
;      Input:   None
;     Output:   None
;
beep            proc    near
                push    ax
                push    cx
                ;
                ;  Enable interrupts (because we need the timer tick)
                ;
                sti
                ;
                ;  Issue beeps
                ;
                mov     ax, word ptr cs:tone1
                mov     cx, word ptr cs:tone_time
                call    sound
                mov     ax, word ptr cs:tone2
                call    sound
                mov     ax, word ptr cs:tone3
                call    sound
                ;
                ;  Turn sound off
                ;
                in      al, 61h
                and     al, 0FCh
                out     61h, al
                ;
                ;  Done!
                ;
                pop     cx
                pop     ax
                ret
beep            endp
;
;    Sound
;
;      Input:   AH = frequency ???
;               CX = delay
;     Output:   None
;
sound           proc    near
                ;
                ;  Turn speaker on
                ;
                in      al, 61h
                test    al, 03
                jne     @@Skip
                or      al, 03
                out     61h, al
@@Skip:
                mov     al, 0B6h
                out     43h, al
                ;
                ;  Set frequency
                ;
                out     42h, al
                mov     al, ah
                out     42h, al
                ;
                ;  Let it play awhile
                ;
                mov     word ptr cs:timer, cx
@@Delay:
                cmp     word ptr cs:timer, 0
                jne     @@Delay
                ret
sound           endp
;
;    String copy
;
;      Input:   ds:si   -> source string
;               es:di   -> destination string
;     Output:   es:di   -> points past end of string
;
strcpy          proc    near
                push    ax
                push    cx
                ;
                push    di
                push    si
                push    es
                ;
                ;  Get length of source
                ;
                mov     di, ds              ; Make es:di point to ds:si
                mov     es, di              ;   long enough to get the
                mov     di, si              ;   length of the string
                ;
                xor     al, al
                mov     cx, -1
                cld
                repne   scasb
                not     cx
                ;
                ;  Restore pointers
                ;
                pop     es
                pop     si
                pop     di
                ;
                ;  Copy source to destination
                ;
                push    si
                rep     movsb
                pop     si
                ;
                ;  Done!
                ;
                pop     cx
                pop     ax
                ret
strcpy          endp
;
;    Upper-case a string
;
;    Input:    ds:si -> string to upper-case
;
strupr          proc    near
                push    ax
                push    si
                ;
                ;  Point before the first character
                ;
                dec     si
                ;
                ;  Uppercase all following characters until null
                ;
@@Loop:
                inc     si          ; get next character
                mov     al, [si]    
                ;
                cmp     al, 0       ; check for end of string
                jz      @@End
                cmp     al, 'a'     ; Skip non-lowercase letters
                jb      @@Loop
                cmp     al, 'z'
                ja      @@Loop
                ;
                sub     al, 'a'-'A' ; Upper-case the character
                ;
                mov     [si], al    ; Store it back
                jmp     @@Loop
@@End:
                pop     si
                pop     ax
                ret
strupr          endp
;
;    Convert BCD to printable form
;
;    Input:     AH      = Two BCD digits to convert
;    Output:    AX      = Two printable digits
;
conv_bcd        proc    near
                mov     al, 0
                shr     ax, 4
                shr     al, 4
                add     ax, '00'
                ret
conv_bcd        endp


;**********************************************************************
;*  EMS functions                                                     *
;**********************************************************************
;
;    Map EMS memory
;
;      Input:   DX = Handle for saving state
;     Output:   Carry clear if OK, Carry set if no good
;               Interrupts are enabled
;
ems_map         proc    near
                push    ax
                push    bx
                push    dx
                ;
                ;  Save EMS context
                ;
                mov     ah, 47h
                int     67h
                cmp     ah, 0
                jne     @@Error
                ;
                ;  Get our memory mapped in
                ;
                mov     dx, cs:EMSHandle
                xor     bx, bx
@@EMSLoop:
                mov     ah, 44h
                mov     al, bl
                int     67h
                cmp     ah, 0
                jne     @@Error
                inc     bx
                cmp     bx, 4
                jl      @@EMSLoop
                ;
                ;  Success...
                ;
@@Ok:
                clc
                jmp     @@End
                ;
                ;  Error occurred, back out
                ;
@@Error:
                call   ems_unmap
                stc
                ;
                ;  Done
                ;
@@End:
                pop     dx
                pop     bx
                pop     ax
                ret
ems_map         endp
;
;    Unmap EMS memory
;
;      Input:   DX = Handle for restoring state
;     Output:   Interrupts are enabled
;
ems_unmap       proc    near
                push    ax
                ;
                ;  Restore prior state
                ;
                mov     ah, 48h
                int     67h
                ;
                ;  Done
                ;
@@End:
                pop     ax
                ret
ems_unmap       endp


;**********************************************************************
;*  Keyboard I/O functions                                            *
;**********************************************************************
;
;    Clear keyboard buffer
;
;      Input:	None
;     Output:   None
;
clear_keys      proc    near
                push    ax
                ;
                ;  Check if key is pending
                ;
@@ClearKeys:
                mov     ah, 1
                int     16h
                jz      @@EndClear
                ;
                ;  Key pending: Eat it
                ;
                xor     ax, ax
                int     16h
                jmp     @@ClearKeys
                ;
                ;  Done!
                ;
@@EndClear:
                pop     ax
                ret
clear_keys      endp
;
;    Get a keystroke
;
;      Input:	None
;     Output:   AH = scan code
;               AL = key pressed
;               (AX = 0 if time-out occurred)
;
get_key         proc    near
                push    bx
                ;
                ;  Force message count to be displayed first time
                ;
                xor     bx, bx
                ;
                ;  Set timer
                ;
                mov     ax, word ptr wait_time
                mov     word ptr key_timer, ax
                ;
                ;  Wait for keypress or time-out
                ;
@@WaitKey:
                mov     ah, 1                   ; Check if key pressed
                int     16h
                jnz     @@GetKey
                ;
                ;  Check if message count needs to be displayed
                ;
                cmp     word ptr msg_count, 2
                jl      @@ChkTime
                cmp     word ptr msg_count, bx
                je      @@ChkTime
                ;
                ;  Save registers
                ;
                push    cx
                push    dx
                push    si
                ;
                ;  Save cursor position
                ;
                call    get_cur_pos
                mov     cx, dx
                ;
                ;  Display message count
                ;
                mov     dl, byte ptr vid_cols   ; Right justify msg
                sub     dl, 11
                mov     dh, 4                   ; On 5th (0-4) line of window
                call    win_set_pos
                mov     si, offset MsgCount
                mov     bl, byte ptr attr_brdr
                call    cputs
                ;
                ;  Convert count to string
                ;
                mov     ax, word ptr msg_count
                mov     bx, 10
                div     bl                      ; AL=Qoutient; AH=Remainder
                add     al, '0'
                mov     byte ptr temp_string,   al
                add     ah, '0'
                mov     byte ptr temp_string+1, ah
                ;
                ;  Display on screen
                ;
                add     dl, 7                   ; Set cursor pos
                call    win_set_pos
                mov     si, offset temp_string
                mov     bl, byte ptr attr_brdr
                call    cputs
                ;
                ;  Restore cursor position
                ;
                mov     dx, cx
                call    set_cur_pos
                ;
                ;  Restore registers
                ;
                pop     si
                pop     dx
                pop     cx
                ;
                ;  Update flag so we don't constantly update display
                ;
                mov     bx, word ptr msg_count
                ;
                ;  Check if we timed out
                ;
@@ChkTime:
                cmp     word ptr key_timer, 0
                jne     @@WaitKey
                ;
                ;  We timed out:  return 0
                ;
                xor     ax, ax
                jmp     @@Done
                ;
                ;  Get the key and return it
                ;
@@GetKey:
                xor     ax, ax
                int     16h
                ;
                ;  Reset timer
                ;
                mov     word ptr key_timer, 0
                ;
                ;  Done!
                ;
@@Done:
                pop     bx
                ret
get_key         endp
;
;    Get a string
;
;      Input:   DS:SI = buffer for string
;               BL    = video attribute/color
;               DI    = maximum length
;     Output:   AL    = final key (Esc or Enter)
;               CX    = length
;
get_string      proc    near
                push    dx
                push    si
                ;
                ; Clear length to zero
                ;
                xor     cx, cx
                ;
                ;  Get text to send into ds:si buffer
                ;
@@GetKey:
                call    get_key
                cmp     al, 1Bh                   ; Esc?
                je      @@Done
                cmp     al, 0Dh                   ; Enter?
                je      @@Done
                cmp     al, 08h                   ; Backspace?
                je      @@Backspace
                cmp     al, 20h                   ; Ignore non-text keys...
                jl      @@GetKey
                cmp     al, 7Eh
                jg      @@GetKey
                ;
                ;  Process text
                ;
                cmp     cx, di                    ; Check for max length
                jl      @@Store
                ;
                ;  Hit end of line
                ;
                call    beep                      ; Too long!
                jmp     @@GetKey                  ; Get next key...
                ;
                ; Store character in buffer
                ;
@@Store:
                call    cputc                     ; Display character
                ;
                mov     byte ptr [si], al         ; save character
                inc     si                        ; point to next spot
                inc     cx                        ; increment length
                ;
                jmp     @@GetKey                  ; Get next key...
                ;
                ;  Backspace
                ;
@@Backspace:
                ;
                ;  Do not backspace if length is zero
                ;
                cmp     cx, 1
                jl      @@GetKey
                ;
                ;  Get current cursor pos
                ;
                call    get_cur_pos
                ;
                ;  See if we have to wrap to previous line or not
                ;
                cmp     dl, 1
                jnl     @@NoWrap
                ;
                ;   Wrap to previous line
                ;
                dec     dh            ; Previous row
                mov     dl, vid_cols  ;   at end of line
                ;
                ;   Just skip back one position
                ;
@@NoWrap:
                dec     dl
                ;
                ;  Erase last char from buffer
                ;
                dec     si                        ; point to previous spot
                dec     cx                        ; decrement length
                ;
                ;  Hop back
                ;
                call    set_cur_pos
                ;
                ;  Blank out character
                ;
                mov     al, ' '
                cmp     byte ptr vid_mode, 12h
                jne     @@NotGraph
                mov     al, byte ptr [si]         ; XOR char over itself
@@NotGraph:
                call    cputc
                ;
                ;  And hop back again
                ;
                call    set_cur_pos
                jmp     @@GetKey
                ;
                ;  Done!
                ;
@@Done:
                pop     si
                pop     dx
                ret
get_string      endp


;**********************************************************************
;*  Video I/O functions                                               *
;**********************************************************************
;
;    Get the current cursor position
;
;      Input:   None
;     Output:   DH = row, DL = column
;
get_cur_pos     proc    near
                push    ax
                push    bx
                push    cx
                ;
                mov     ah, 03h        ; INT 10: Get Cursor Pos
                mov     bh, cs:vid_page
                int     10h
                ;
                pop     cx
                pop     bx
                pop     ax
                ret
get_cur_pos     endp
;
;    Set the current cursor position
;
;      Input:   DH = row, DL = column
;     Output:   None
;
set_cur_pos     proc    near
                push    ax
                push    bx
                ;
                mov     ah, 02h        ; INT 10: Set Cursor Pos
                mov     bh, cs:vid_page
                int     10h
                ;
                pop     bx
                pop     ax
                ret
set_cur_pos     endp
;
;    Set the current cursor position within our "window"
;
;      Input:   DH = row, DL = column
;     Output:   None
;
win_set_pos     proc    near
                push    dx
                ;
                ;  Translate window line to screen line
                ;
                add     dh, cs:vid_rows
                sub     dh, WINDOW_HEIGHT
                ;
                ;  Position cursor
                ;
                call    set_cur_pos
                ;
                pop     dx
                ret
win_set_pos     endp
;
;    Write a character to the screen
;
;      Input:   AL = character to write
;               BL = video attribute/color
;     Output:   None
;
cputc           proc    near
                push    bx
                push    cx
                ;
                ;  Display character
                ;
                mov     ah, 0Eh        ; INT 10: Teletype Output
                mov     bh, cs:vid_page
                ;
                ;    In graphics mode we first need to do the following:
                ;
                ;        1) "Cut" the character out of the background color
                ;        2) Backspace
                ;        3) "Twiddle" the attribute so its just the foreground
                ;           color with the 80h bit set (XOR)
                ;
                cmp     byte ptr cs:vid_mode, 12h
                jne     @@NotGraph
                cmp     al, ' '        ; No special code for control chars
                jl      @@NotGraph
                ;
                mov     ch, al         ; Save character
                mov     cl, bl         ; Save attribute
                ;
                shr     bl, 4          ; Get background color
                or      bl, 80h        ;   and set to XOR
                int     10h            ; XOR out the character
                mov     al, 08h        ;   and then backspace
                int     10h
                ;
                mov     al, ch         ; Restore character
                mov     bl, cl         ; Restore attribute
                and     bl, 0Fh        ; Use foreground color only
                or      bl, 80h        ;   and set bit to XOR onto background
@@NotGraph:
                int     10h            ; write character
                ;
                ;  Done!
                ;
                pop     cx
                pop     bx
                ret
cputc           endp
;
;    Write a string to the screen
;
;      Input:   DS:SI -> string to write
;               BL    =  video attribute/color
;     Output:   None
;
cputs           proc    near
                push    ax
                push    si
                ;
                ;  Put characters until NULL byte is reached
                ;
@@Loop:
                mov     al, [si]       ; get next character
                cmp     al, 0          ; check for end of string
                jz      @@End
                ;
                call    cputc          ; Write character
                ;
                inc     si
                jmp     @@Loop
@@End:
                pop     si
                pop     ax
                ret
cputs           endp
;
;    Get/Put screen region
;
;      Input:   dh = start line
;               si = end line + 1
;               bl = direction (0 = get, 1 = put)
;     Output:   None
;
gp_screen       proc    near
                push    ax
                push    bx
                push    cx
                push    dx
                push    di
                push    si
                push    ds
                push    es
                push    bp
                ;
                ;  Set up pointer to EMS save area (Frame Seg: 0)
                ;
                mov     ax, EMSFrame
                mov     es, ax
                mov     di, 0
                ;
                ;  Use method appropriate to video mode
                ;
                cmp     byte ptr vid_mode, 12h
                jne     @@Text
                ;
                ;  Save/restore graphics mode
                ;
                ;  Convert window start line to video pixel offset
                ;
                xor     ax, ax
                mov     al, dh              ; convert window start line
                add     al, byte ptr vid_rows
                sub     al, WINDOW_HEIGHT   ;   to screen line
                ;
                mov     cx, 16 * 80         ; Screen line * height * width
                mul     cx                  ;   = offset of start pixel
                mov     si, ax
                ;
                mov     ax, VID_GRAPH
                mov     ds, ax
                ;
                mov     cx, (54*1024) - 2   ; Max size of save area (54K)
                mov     bp, WINDOW_HEIGHT * 16 * 80 ; Num video bytes to save/restore
                cmp     bl, 1
                je      @@PutGraph
                call    get_graphics
                jmp     @@End
@@PutGraph:
                call    put_graphics
                jmp     @@End
                ;
                ;  Save/restore text mode
                ;
@@Text:
                call    gp_text
                ;
                ;  Clean up and exit
                ;
@@End:
                pop     bp
                pop     es
                pop     ds
                pop     si
                pop     di
                pop     dx
                pop     cx
                pop     bx
                pop     ax
                ret
gp_screen       endp
;
;    Get/Put screen region (text mode)
;
;      Input:   es:di -> buffer (lines * columns * 2 bytes)
;               dh = start line
;               si = end line + 1
;               bl = direction (0 = get, 1 = put)
;     Output:   None
;     Note:     Registers are not preserved.  That is left to gp_screen.
;
gp_text         proc    near
                ;
                ;  Compute length to save/restore
                ;
                push    dx
                mov     ax, si
                xor     cx, cx
                mov     cl, dh
                sub     ax, cx                      ; Number of lines
                mov     cl, vid_cols
                mul     cx                          ;   times line width
                mov     cx, ax                      ;   == number of words
                pop     dx
                ;
                ;  Convert window start line to screen line
                ;
                xor     ax, ax
                mov     al, dh                      ; window start line
                add     al, byte ptr vid_rows       ;   + screen height
                sub     al, WINDOW_HEIGHT           ;   - window height
                ;
                ;  Compute offset of video memory to save/restore
                ;
                push    cx
                xor     cx, cx
                mov     cl, vid_cols                ; get line width
                mul     cx                          ; start line * line width
                shl     ax, 1                       ;   * 2 (2 bytes per col.)
                mov     si, ax                      ;   = starting byte offset
                pop     cx
                ;
                ;  Point DS to video memory
                ;
                mov     ax, VID_COLOR_TEXT
                cmp     byte ptr vid_mode, 7
                jne     @@Set
                mov     ax, VID_MONO_TEXT
@@Set:
                mov     ds, ax
                ;
                ;  Set up per direction
                ;
                cmp     bl, 0
                je      @@Copy
                ;
                ;  Set up for Put
                ;
                mov     ax, ds
                mov     bx, es
                mov     es, ax
                mov     ds, bx
                xchg    si, di
                ;
                ;  Do the copy
                ;
@@Copy:
                cld
                rep     movsw
                ;
                ;  Done!
                ;
                ret
gp_text         endp
;
;    Get screen region (graphics mode)
;
;      Input:   es:di -> buffer
;               ds:si -> start of video memory to save
;               bp = count of video bytes to save
;               cx = offset of buffer end + 1
;     Output:   None
;     Note:     Registers are not preserved.  That is left to gp_graphics
;
get_graphics    proc    near
                ;
                ;  Save VGA state
                ;
                call    vga_save
                ;
                ;  Init
                ;
                mov     bl, 80h             ; Bit mask (start at left)
                ;
                mov     dx, 3CEh            ; Graphic controller
                mov     ax, 0F07h           ; Color compare = match color
                out     dx, ax              ; Set it
                ;
                ;  Read the color by combining the bits from each plane
                ;
@@NewColor:
                ;
                ;  Select read mode 0
                ;
                mov     al, 5               ; Select the mode register
                out     dx, al
                inc     dx
                in      al, dx              ; Get the current mode
                and     al, 11110111b       ; Read Mode 0
                out     dx, al              ; Set read mode
                dec     dx
                ;
                ;  Process each plane from 3 to 0
                ;
                push    cx
                mov     ax, 0304h
                xor     cx, cx
                ;
                ;  Select plane
                ;
@@NextPlane:
                out     dx, ax              ; Select it
                mov     bh, [si]            ; Get pixel info
                test    bh, bl              ; Was the masked bit set?
                jz      @@NoAdd             ; No, don't add to color
                inc     cx                  ; Yes, add to color
@@NoAdd:
                dec     ah                  ; Next plane
                cmp     ah, -1
                je      @@EndPlanes
                shl     cx, 1               ; Make room for next plane
                jmp     @@NextPlane
@@EndPlanes:
                ;
                ;  Now count how many pixels follow with the same color
                ;
                mov     ax, cx              ; Get color
                mov     ah, 1               ; Set count
                xchg    ah, al              ; AH = color, AL = count
                pop     cx                  ; Restore count of pixels to save
                ;
                ;  Switch to color compare mode
                ;
                push    ax
                mov     al, 5               ; Select Mode Register
                out     dx, al
                inc     dx
                in      al, dx              ; Get current mode
                ;
                or      al, 00001000b       ; Set read mode 1 (color compare)
                out     dx, al              ; Select the mode
                dec     dx
                ;
                mov     al, 2               ; AH = color, AL = Compare Color
                out     dx, ax
                pop     ax
                ;
                mov     bh, [si]            ; Get video data using color compare
@@NextBit:
                shr     bl, 1               ; Check the next bit
                jz      @@NextByte          ; Done with this byte, go get next
                ;
                ;  Check if color matches
                ;
@@CheckColor:
                test    bh, bl              ; Does color match?
                jz      @@NextColor
                ;
                ;  Color matches
                ;
                inc     al                  ; Increment color count
                jnz     @@NextBit           ; and continue...
                ;
                ;  Overflowed the count...
                ;
                dec     al                  ; set back to 255
                mov     es:[di], ah         ; store color
                inc     di
                mov     es:[di], al         ; store count
                inc     di
                cmp     di, cx
                ja      @@Exit              ; End of Buffer!  Bail out...
                mov     al, 1               ; Start counting again
                jmp     @@NextBit           ; and continue...
                ;
                ;  Next byte of video memory
                ;
@@NextByte:
                inc     si                  ; point to next byte
                dec     bp                  ; decrement byte count
                cmp     bp, 0               ; If last byte
                je      @@Done              ;   then we're done!
                ;
                mov     bl, 80h             ; Reset mask to high bit
                mov     bh, [si]            ; Get next byte
                jmp     @@CheckColor        ; and go check it
                ;
                ;  We're done!  Clear CX to force exit
                ;
@@Done:
                xor     cx, cx              ; Clear to force exit
                ;
                ;  A new color or the end!  Save the count...
                ;
@@NextColor:
                cmp     al, 15              ; Count needs a full byte?
                ja      @@ByteCount
                ;
                ;  Count is 1 to 15.  Store color and count in single byte.
                ;
                shl     al, 4               ; Put count in upper nybble
                or      al, ah              ; Stuff color in lower nybble
                jmp     @@EndStore
@@ByteCount:
                ;
                ;  Count is 16 or more.  Store as color byte followed
                ;    by count byte
                ;
                mov     es:[di], ah         ; store color
                inc     di
@@EndStore:
                mov     es:[di], al         ; store count (maybe w/ color too)
                inc     di
                ;
                ;  End of the road?
                ;
                cmp     di, cx
                jna     @@NewColor
@@Exit:
                call    vga_restore
                ret
get_graphics    endp
;
;    Put screen region (graphics mode)
;
;      Input:   es:di -> buffer
;               ds:si -> start of video memory to restore
;               bp = count of video bytes to restore
;     Output:   None
;     Note:     Registers are not preserved.  That is left to gp_graphics
;
put_graphics    proc    near
                ;
                ;  Save VGA state
                ;
                call    vga_save
                ;
                ;  Init
                ;
                xor     bh, bh                 ; Current bit pos = 0
                ;
                ;  Set VGA to Write Mode #2
                ;
                mov     dx, 3CEh               ; Graphics controller
                mov     al, 5                  ; Mode register
                out     dx, al                 ; Select it
                ;
                inc     dx
                in      al, dx                 ; get current mode
                or      al, 10b                ; Select Write Mode 2
                and     al, 11111110b
                out     dx, al                 ; Set it
                dec     dx
                ;
                ;  Put back each color/count
                ;
@@NextColor:
                ;
                ;  Get next color/count
                ;
                mov     dh, byte ptr es:[di]   ; Get pixel color
                inc     di
                ;
                cmp     dh, 15                 ; If color > 15 then
                ja      @@Byte                 ;   count is in same byte
                ;
                ;  Following byte is the count
                ;
                mov     dl, byte ptr es:[di]   ; Get pixel count
                inc     di
                jmp     @@EndGet
                ;
                ;  Count is store in same byte as color
                ;
@@Byte:
                mov     dl, dh                 ; Copy byte to count as well
                and     dh, 0Fh                ; Clear count from color
                shr     dl, 4                  ; Clear color from count
@@EndGet:
                ;
                ;  Double-check our input
                ;
                cmp     dl, 0                  ; 0 pixels?
                je      @@Done                 ;   No good! Bail out...
                ;
                ;  Create mask
                ;
@@SameColor:
                dec     dl                     ; Reduce count by one
                mov     cl, dl                 ; Compute num bits this time
                cmp     cl, 7                  ; Max of 7 more bits at a time
                jna     @@MakeMask
                mov     cl, 7                  ; Fill the max
@@MakeMask:
                mov     bl, 80h                ; Use at least 1 bit in mask
                sar     bl, cl                 ; Fill mask on left
                xchg    bh, cl                 ; Get pos (and save count)
                shr     bl, cl                 ; Shift the mask into place
                xchg    bh, cl                 ; Restore
                ;
                ;  Compute number of pixels we are about to set
                ;
                mov     al, 1                  ; Original bit in mask
                add     al, cl                 ;   + bits we added to mask
                add     al, bh                 ;   + positions we shifted
                ;
                cmp     al, 8
                jae     @@Overflow
                ;
                ;  Mask stayed entirely within the mask byte
                ;
                inc     cl                     ; Num pixels
                jmp     @@EndCount
                ;
                ;  Some pixels overflowed out of the mask byte
                ;
@@Overflow:
                mov     cl, 8                  ; Total that could be
                sub     cl, bh                 ;   - num 0's shifted in
@@EndCount:
                ;
                ;  Set mask
                ;
                mov     ah, bl                 ; Put mask in AH
                mov     al, 8                  ; Select map mask register
                push    dx
                mov     dx, 3CEh               ; Graphics controller
                out     dx, ax                 ; Set the mask
                pop     dx
                ;
                ;  Write the color
                ;
                mov     al, ds:[si]            ; Latch data
                mov     ds:[si], dh            ; Write color
                ;
                ;  Move bit position up by number written
                ;
                add     bh, cl                 ; Inc. pos by pixels written
                ;
                ;  Reduce color count by num pixels just set
                ;
                sub     dl, cl                 ; (Count - 1) - pixels written
                inc     dl                     ;   + 1
                ;
                ;  See if we're done with this video byte
                ;
                cmp     bh, 8                  ; Finished bits in this byte?
                jb      @@EndByte              ; No, continue
                ;
                ;  Move to next video byte
                ;
                xor     bh, bh                 ; Reset bit position
                inc     si                     ; Bext video byte
                dec     bp                     ; Reduce count to go by 1
                cmp     bp, 0                  ; Done?
                je      @@Done                 ; No, not yet
@@EndByte:
                ;
                ;  Are we done with this color?
                ;
                cmp     dl, 0                  ; Has count gone zero?
                je      @@NextColor            ; Yes, get next color
                jmp     @@SameColor            ; No, keep writing this one
                ;
                ;  Done!  Restore VGA mode
                ;
@@Done:
                ;
                ;  Restore VGA state
                ;
                call    vga_restore
                ret
put_graphics    endp
;
;    Save VGA state
;
;      Input:   None
;     Output:   None
;
vga_save        proc    near
                push    ax
                push    dx
                push    si
                ;
                ;  Save Address Register (3CEh)
                ;
                mov     dx, 3CEh
                in      al, dx
                mov     byte ptr cs:vga_state, al
                ;
                ;  Save other registers
                ;
                mov     ah, 2               ; Save 2 through 8
                mov     si, offset vga_state+1
                ;
@@Loop:
                mov     al, ah              ; Get register to save
                out     dx, al              ; Select it
                inc     dx
                in      al, dx              ; Get it
                dec     dx
                mov     byte ptr cs:[si], al    ; Save it
                inc     si
                inc     ah                  ; Next one
                cmp     ah, 6               ; Register 6?
                jne     @@End6
                inc     ah                  ; Skip register 6
@@End6:
                cmp     ah, 9               ; End?
                jb      @@Loop
                ;
                ;  Restore Address Register (3CEh)
                ;
                mov     al, byte ptr cs:vga_state
                out     dx, al
                ;
                ;  Done
                ;
                pop     si
                pop     dx
                pop     ax
                ret
vga_save        endp
;
;    Restore VGA state
;
;      Input:   None
;     Output:   None
;
vga_restore     proc    near
                push    ax
                push    dx
                push    si
                ;
                ;  Init
                ;
                mov     dx, 3CEh
                ;
                ;  Restore registers
                ;
                mov     al, 2               ; Restore 2 through 8
                mov     si, offset vga_state+1
                ;
@@Loop:
                mov     ah, byte ptr cs:[si]    ; Get old value
                out     dx, ax              ; Restore it
                inc     si                  ; Next one
                inc     al
                cmp     al, 6               ; Register 6?
                jne     @@End6
                inc     al                  ; Skip register 6
@@End6:
                cmp     al, 9               ; End?
                jb      @@Loop
                ;
                ;  Restore Address Register (3CEh)
                ;
                mov     al, byte ptr cs:vga_state
                out     dx, al
                ;
                ;  Done
                ;
                pop     si
                pop     dx
                pop     ax
                ret
vga_restore     endp
;
;    Clear display area and set screen attributes
;
;      Input:   None
;     Output:   None
;
clear_area      proc    near
                push    ax
                push    bx
                push    cx
                push    dx
                ;
                ;  Initialize
                ;
                mov     bh, vid_page
                xor     ch, ch        ; For width of screen
                mov     cl, vid_cols
                ;
                ;  Video mode dependent initialization
                ;
                mov     al, ' '
                mov     bl, attr_brdr
                ;
                cmp     byte ptr vid_mode, 12h
                jne     @@NotGraph
                mov     al, 219       ; Block character
                shr     bl, 4
@@NotGraph:
                ;
                ;  Top and Bottom border
                ;
                mov     dx, 0000h     ; line = 0, col = 0
                call    win_set_pos
                mov     ah, 09h       ; Display char and attribute
                int     10h
                mov     dx, 0300h     ; line = 3, col = 0
                call    win_set_pos
                mov     ah, 09h       ; Display char and attribute
                int     10h
                mov     dx, 0400h     ; line = 4, col = 0
                call    win_set_pos
                mov     ah, 09h       ; Display char and attribute
                int     10h
                ;
                ;  Message area
                ;
                mov     al, ' '
                mov     bl, attr_msg
                ;
                mov     dx, 0100h     ; line = 1, col = 0
                call    win_set_pos
                mov     ah, 09h       ; Display char and attribute
                int     10h
                mov     dx, 0200h     ; line = 2, col = 0
                call    win_set_pos
                mov     ah, 09h       ; Display char and attribute
                int     10h
                ;
                ;  Done!
                ;
                pop     dx
                pop     cx
                pop     bx
                pop     ax
                ret
clear_area      endp


;**********************************************************************
;*  NetBIOS functions                                                 *
;**********************************************************************
;
;  Execute NetBIOS function:
;
;      Input:   es:bx -> NCB
;     Output:   None
;
NetBios         proc    near
                push    ax
                ;
                ;  Set command and set complete flag to "pending"
                ;
                mov     byte ptr es:[bx.ncb_cmd],  al
                mov     byte ptr es:[bx.ncb_cplt], 0FFh
                ;
                ;  Execute
                ;
                int     5Ch
                ;
                ;  Done!
                ;
                pop     ax
                ret
NetBios         endp
;
;  Set memory block to a particular byte value
;
;      Input:   es:bx -> NCB
;     Output:   None
;
net_zero        proc    near
                ;
                ;  Initialize
                ;
                push    ax
                push    cx
                push    di
                mov     di, bx      ; convert es:bx to es:di
                ;
                ;  Zero NCB
                ;
                mov     cx, size ncb_struc
                xor     ax, ax
                cld
                rep     stosb
                ;
                ;  Done!
                ;
                pop     di
                pop     cx
                pop     ax
                ret
net_zero        endp
;
;  Issue NetBIOS Receive Datagram (No Wait)
;
;      Input:   ds:si -> buffer
;     Output:   None
;
net_receive     proc    near
                ;
                ;  Initialize
                ;
                push    ax
                push    bx
                ;
                ;  Zero out the NCB
                ;
                mov     bx, offset rcv_ncb
                call    net_zero
                ;
                ;  Receive for all defined names
                ;
                mov     byte ptr rcv_ncb.ncb_num, 0FFh
                ;
                ;  Set buffer ptr and length
                ;
                mov     word ptr rcv_ncb.ncb_len,   MSG_SIZE
                mov     word ptr rcv_ncb.ncb_buf+2, ds
                mov     word ptr rcv_ncb.ncb_buf,   si
                ;
                ;  Point to "Post" function
                ;
                push    si
                mov     si, offset cs:post_routine
                mov     word ptr rcv_ncb.ncb_post+2, cs
                mov     word ptr rcv_ncb.ncb_post,   si
                pop     si
                ;
                ;  Issue RECEIVE DATAGRAM command
                ;
                mov     al, NCB_RECV_DATAG+NCB_ASYNC
                call    NetBios
                ;
                ;  Done!
                ;
                pop     bx
                pop     ax
                ret
net_receive     endp
;
;  Issue NetBIOS Send Datagram
;
;      Input:   ds:si -> buffer
;               ds:di -> name to send message to
;               cx    -> length of buffer
;     Output:   None
;
net_send        proc    near
                ;
                ;  Initialize
                ;
                push    ax
                push    bx
                push    cx
                push    di
                push    si
                ;
                ;  Zero out the NCB
                ;
                mov     bx, offset temp_ncb
                call    net_zero
                ;
                ;  Set buffer ptr and length
                ;
                mov     word ptr [es:bx].ncb_len,   cx
                mov     word ptr [es:bx].ncb_buf+2, ds
                mov     word ptr [es:bx].ncb_buf,   si
                ;
                ;  Set ncb_num to our name's number
                ;
                mov     al, name_num
                mov     byte ptr [es:bx].ncb_num, al
                ;
                ;  Set ncb_calln to the name we are sending to
                ;
                mov     si, di
                ;
                ;       Save offset of NCB
                ;
                push    bx
                ;
                ;       Set ncb_calln field to spaces
                ;
                mov     di, bx                   ; es:di -> call name field
                add     di, ncb_calln
                push    di                       ; Save offset of ncb_calln
                mov     cx, 15                   ; Set field size
                mov     al, ' '                  ; Set to blank
                cld
                rep     stosb
                ;
                ;       Get length of the call name
                ;
                mov     di, si                   ; Point es:di to name
                xor     al, al
                mov     cx, -1
                repne   scasb
                not     cx
                dec     cx
                ;
                ;       Restore pointer to ncb_calln
                ;
                pop     di
                ;
                ;       Copy our destination name to ncb_calln
                ;
                rep     movsb
                ;
                ;       Set 16th byte to 0x03
                ;           - 0x03 suffix indicates a name for msg delivery
                ;             0x05 suffix indicates a forwarded name
                ;           See SMB doc.
                ;
                pop     bx                       ; Restore NCB offset
                mov     byte ptr [es:bx].ncb_calln+15, 03h
                ;
                ;  Issue SEND DATAGRAM command
                ;
                mov     al, NCB_SEND_DATAG
                call    NetBios
                ;
                ;  Done!
                ;
                pop     si
                pop     di
                pop     cx
                pop     bx
                pop     ax
                ret
net_send        endp


;**********************************************************************
;*  Resident program entry points                                     *
;**********************************************************************
;
;    Interrupt 08:    Timer tick
;
int08           proc    far
                ;
                ;  Process old INT 08 call
                ;
                pushf
                call    cs:oldint08
                ;
                ;  Update timers
                ;
                cmp     word ptr cs:key_timer, 0
                je      @@Timer
                dec     word ptr cs:key_timer
@@Timer:
                cmp     word ptr cs:timer, 0
                je      @@Pop
                dec     word ptr cs:timer
                ;
                ;  Pop up if there's work
                ;
@@Pop:
                call    pop_up
                iret
int08           endp
;
;    Interrupt 09:    Keyboard Input
;
int09           proc    far
                ;
                ;  Save registers
                ;
                push    ax
                push    si
                push    es
                ;
                ;  Don't do anything if we are popped up
                ;
                cmp     byte ptr cs:busy, 0
                jne     @@Old
                ;
                ;  Clear timer on key press
                ;
                mov     word ptr cs:key_timer, 0
                ;
                ;  Get current shift state
                ;
                push    SEG40           ; Point ES:[SI] to BIOS shift state
                pop     es
                mov     si, KBFLAG
                mov     ah, es:[si]     ; Get shift state
                ;
                ;  Check if this is a CTRL/ALT key
                ;
                test    ah, CTRL        ; Is Control Key pressed?
                jz      @@Old           ; No, pass the key to BIOS routine
                test    ah, ALT         ; Is Alt Key pressed?
                jz      @@Old           ; No, pass the key to BIOS routine
                ;
                ;  Check if Ctrl-Alt-S
                ;
                in      al, KBDATAP     ; Get the key code
                cmp     al, 31          ; 'S'?
                jne     @@Old           ; No, just pass it along
                ;
                ;  Eat the key
                ;
                in      al, KBCMDBP     ; Read the KB command port
                mov     ah, al          ; Copy it to AH
                or      al, 80h         ; Set RESET bit
                out     KBCMDBP, al     ;   and write to the command port
                mov     al, ah          ; Clear the RESET bit
                out     KBCMDBP, al     ;   and write to the command port
                mov     al, EOI         ; Send END OF INTERRUPT
                out     PIC, al         ;   to the interrupt controller
                ;
                ;  Remember that user requested a Send
                ;
                mov     byte ptr cs:send_flg, 1
                ;
                ;  Restore registers
                ;
                pop     es
                pop     si
                pop     ax
                ;
                ;  Done!
                ;
                iret
                ;
                ;  Call old handler
                ;
@@Old:
                ;
                ;  Restore registers
                ;
                pop     es
                pop     si
                pop     ax
                ;
                ;  Call old handler
                ;
                jmp     cs:oldint09
int09           endp
;
;    Interrupt 28:    DOS Idle Interrupt
;
int28           proc    far
                inc     byte ptr cs:in_int28
                ;
                ;  Process old INT 28 call
                ;
                pushf
                call    cs:oldint28
                ;
                ;  Pop up if there's work
                ;
                call    pop_up
                dec     byte ptr cs:in_int28
                iret
int28           endp
;
;    Interrupt 2F:    Multiplex Interrupt - Resident API
;
int2F           proc    far
                ;
                ;  Is this a Windows broadcast?
                ;
                cmp     ah, WINDOWS_ID
                je      @@Windows
                ;
                ;  Is this for us?
                ;
                cmp     ah, MULTIPLEX_ID
                jne     @@OldInt
                ;
                ;  Check for one of our API's
                ;
                cmp     al, INSTALL_CHECK
                je      @@Install
                cmp     al, SET_FROM_NAME
                je      @@SetName
                cmp     al, SET_DISABLED
                jne     @@OldInt

                ;
                ; Clear "disabled" flag
                ;
@@SetDisabled:
                mov     byte ptr cs:disabled, bl
                jmp     @@Exit

                ;
                ;  Set "From" name to name pointed to by ds:si
                ;
@@SetName:
                push    ax
                push    di
                push    es
                ;
                mov     ax, cs
                mov     es, ax
                mov     di, offset SendFrom
                call    strcpy
                ;
                pop     es
                pop     di
                pop     ax
                jmp     @@Exit
                ;
                ;  Tell caller that we are installed
                ;
@@Install:
                ;
                ;  Set AL so they know we are here
                ;
                mov     al, 0FFh
                ;
                ;  Hand our segment (CS=DS) back to caller in BX
                ;    so that they can unload us if they wish
                ;
                mov     bx, cs
                jmp     @@Exit
                ;
                ;  Check if Windows is loading or unloading
                ;
@@Windows:
                ;
                ;  Windows loading?
                ;
                cmp     al, WIN_START
                jne     @@Win2
                inc     byte ptr cs:windows
                jmp     @@OldInt
                ;
                ;  Windows unloading?
                ;
@@Win2:
                cmp     al, WIN_STOP
                jne     @@OldInt
                ;
                ;  Windows is gone, clear flag but set a wait
                ;  until the screen settles down
                ;
                push    ax
                mov     ax, cs:second_wait
                mov     word ptr cs:timer, ax
                pop     ax
                dec     byte ptr cs:windows
                ;
                ;  Not for us; call old INT 2F handler
                ;
@@OldInt:       jmp     cs:oldint2F
                ;
                ;  Done!
                ;
@@Exit:
                iret
int2F           endp
;
;  Receive Datagram Post routine
;
;       - receives messages and stores them in our queue
;
post_routine    proc    far
                ;
                ;  Save registers
                ;
                push    ax
                push    bx
                push    cx
                push    dx
                push    di
                push    si
                push    ds
                push    es
                ;
                ;  Switch to our data segment
                ;
                mov     ax, cs
                mov     ds, ax
                mov     es, ax
                ;
                ;  Enable interrupts
                ;
                sti
                ;
                ;  Check if we are off
                ;
                cmp     byte ptr off, 0
                jne     @@Exit
                ;
                ;  Check if queue is full
                ;
                cmp     word ptr msg_count, MAX_MSGS
                jae     @@End
                ;
                ;  Check if we are disabled
                ;
                cmp     byte ptr disabled, 0
                jne     @@End
                ;
                ;  Verify that data received is an SMB block
                ;
                mov     si, offset recv_msg
                cmp     byte ptr [si+0], 0FFh
                jne     @@End
                cmp     byte ptr [si+1], 'S'
                jne     @@End
                cmp     byte ptr [si+2], 'M'
                jne     @@End
                cmp     byte ptr [si+3], 'B'
                jne     @@End
                cmp     byte ptr [si+4], 0D0h
                jne     @@End
                ;
                ;  Ignore messages from names starting w/ $$
                ;    -- the OS/2 receiver uses these and displaying
                ;    -- them in DOS is no good.
                ;
                cmp     word ptr [si+36], '$$'
                je      @@End
                ;
                ;  Copy caller name onto front of SMB block
                ;    (just because it's a convenient place to keep it)
                ;
                mov     si, offset rcv_ncb.ncb_calln    ; source (Seg DS)
                mov     di, offset recv_msg             ; dest (Seg ES)
                call    strcpy                          ; di points past end
                ;
                ;  Chop caller name to 15 bytes
                ;
                mov     si, offset recv_msg
                mov     byte ptr [si+15], 0
                ;
                ;  Get current date
                ;
                mov     ah, 4                       ; Get current date
                int     1Ah
                mov     word ptr [si+16], dx        ; DH = BCD Month, DL = Day
                ;
                ;  Get current time
                ;
                mov     ah, 2                       ; Get current time
                int     1Ah
                mov     word ptr [si+18], cx        ; CH = Hour, CL = Minutes
                ;
                ;  Move data up to EMS
                ;
                mov     dx, EMSHandle2
                call    ems_map
                jc      @@End                       ; EMS error...
                ;
                mov     ax, EMSFrame
                mov     es, ax
                ;
                xor     di, di
                cmp     word ptr es:[0FFFCh], 'ER'
                jne     @@BadSig
                cmp     word ptr es:[0FFFEh], 'VC'
                jne     @@BadSig
                jmp     @@Move
@@BadSig:
                jmp     @@NoMove
                ;
@@Move:
                mov     di, word ptr next_msg       ; dest (ES:DI)
                mov     cx, MSG_SIZE
                rep     movsb
                ;
@@NoMove:
                mov     ax, ds                      ; Restore ES
                mov     es, ax
                ;
                call    ems_unmap
                ;
                ;  Point to next buffer
                ;
                cmp     di, 0                       ; If no move
                je      @@NoInc                     ;   then no inc
                add     word ptr next_msg, MSG_SIZE
                inc     word ptr msg_count
@@NoInc:
                ;
                ;  Check for queue wrap
                ;
                cmp     word ptr next_msg, MSGS_END
                jl      short @@End
                mov     word ptr next_msg, MSGS_START
                ;
                ;  Clear timer so we pop up again
                ;
@@End:
                cmp     byte ptr busy, 0
                jne     @@EndTimer
                mov     word ptr key_timer, 0
@@EndTimer:
                ;
                ;  Re-issue async datagram receive
                ;
                mov     si, offset recv_msg
                call    net_receive
                ;
                ;  Restore registers
                ;
@@Exit:         pop     es
                pop     ds
                pop     si
                pop     di
                pop     dx
                pop     cx
                pop     bx
                pop     ax
                iret
post_routine    endp


;**********************************************************************
;*  Pop up function                                                   *
;**********************************************************************
;
;  Pop up and process send or receive
;
;      Input:   None
;     Output:   None
;
process         proc    near
                ;
                ;  Switch to our stack
                ;
                cli
                mov     word ptr cs:old_ss, ss
                mov     word ptr cs:old_sp, sp
                mov     ss, word ptr cs:our_ss
                mov     sp, word ptr cs:our_sp
                sti
                ;
                ;  Save resisters
                ;
                push    ax
                push    bx
                push    cx
                push    dx
                push    di
                push    si
                push    ds
                push    es
                ;
                ;  Switch to our data segment
                ;
                mov     ax, cs
                mov     ds, ax
                mov     es, ax
                ;
                ;  Switch to our PSP
                ;
                mov     ax, 5100h     ; Get current PSP
                int     21h
                mov     word ptr oldpsp, bx
                ;
                mov     ax, 5000h     ; Set current PSP
                mov     bx, word ptr ourpsp
                int     21h
                ;
                ;  Get current video mode
                ;
                mov     ah, 0Fh        ; INT 10: Get Video Mode
                int     10h
                mov     vid_page, bh
                mov     vid_cols, ah
                mov     vid_mode, al
                ;
                ;  If video mode not text then we can't display
                ;
                cmp     al, 02h       ; AL = video mode
                je      @@Text
                cmp     al, 03h
                je      @@Text
                cmp     al, 07h
                je      @@Text
                cmp     al, 54h       ; S3 132x43 text mode
                je      @@Text
                cmp     al, 55h       ; S3 132x25 text mode
                je      @@Text
                cmp     al, 12h       ; VGA 640x480 graphics?
                jne     @@Graphics
                ;
                ;  If 640x480 by 1 because 80 columns don't quite fit
                ;
                dec     ah            ; Reduce columns by one
                mov     vid_cols, ah
                jmp     @@Text
                ;
                ;  Don't beep if we did it already
                ;
@@Graphics:
                cmp     byte ptr graphics, 1
                je      @@Exit
                ;
                ;  Remember that we couldn't pop up
                ;
                mov     byte ptr graphics, 1
                ;
                ;  Beep so they know we got something
                ;
@@Beep:
                call    beep
                jmp     @@Exit
                ;
                ;  We are in text mode, so we can pop up
                ;
@@Text:
                ;
                ;  Check if we were just in graphics
                ;
                cmp     byte ptr graphics, 0
                je      @@Text2
                ;
                ;  We just came out of graphics...
                ;
                ;       Since the application may have blanked the screen
                ;       while it switched back to text, we should delay
                ;       a little so application has time to turn the screen
                ;       back on.
                ;
                mov     byte ptr graphics,  0
                mov     word ptr key_timer, 28
                jmp     @@Exit
@@Text2:
                ;
                ;  Get screen height
                ;
                push    es
                push    bp
                mov     ax, 1130h      ; INT 10: Get font info
                mov     bh, 6          ; 8x16 font
                mov     dl, 25-1       ; Assume 25 lines (for non-EGA/VGA)
                int     10h
                ;  -- at this point DL = screen rows - 1
                ;     and           CX = bytes/character
                inc     dl             ; (rows - 1) + 1
                mov     byte ptr vid_rows, dl
                pop     bp
                pop     es
                ;
                ;  Save cursor position and type
                ;
                call    get_cur_pos
                mov     word ptr vid_cur_pos, dx
                ;
                ;  Get EMS memory
                ;
                mov     dx, EMSHandle
                call    ems_map
                jc      @@Beep
                ;
                ;  Save the screen area
                ;
                mov     dh, 0                  ; start line
                mov     si, WINDOW_HEIGHT      ; end line
                mov     bl, 0                  ; 0 = get from screen
                call    gp_screen
                ;
                ;  Beep to announce reception
                ;
                call    beep
                ;
                ;  Clear keyboard buffer
                ;
                call    clear_keys
                ;
                ;  Check if we want to send or display
                ;
                cmp     byte ptr send_flg, 1
                jne     @@Display
                ;
                ;  Send a message
                ;
                call    send
                jmp     @@Drop
                ;
                ;  Display the message we recieved
                ;
@@Display:
                ;
                ;  Clear screen area
                ;
                call    clear_area
                ;
                ;  Display "Message from:"
                ;
                mov     dx, 0000h     ; line = 0, col = 0
                call    win_set_pos
                mov     si, offset MsgFrom
                mov     bl, byte ptr attr_brdr
                call    cputs
                ;
                ;  Display "To:"
                ;
                mov     dx, 0025h     ; line = 0, col = 37
                call    win_set_pos
                mov     si, offset MsgTo
                call    cputs
                ;
                ;  Get address of next SMB block in ES:DI
                ;
                mov     ax, EMSFrame
                mov     es, ax
                mov     di, word ptr first_msg
                ;
                ;  Display date/time received
                ;
                mov     si, offset temp_time
                ;
                ;  Get month digits
                ;
                mov     ah, byte ptr es:[di+17]
                call    conv_bcd
                mov     byte ptr [si+8], ah
                mov     byte ptr [si+9], al
                ;
                ;  Get day digits
                ;
                mov     ah, byte ptr es:[di+16]
                call    conv_bcd
                mov     byte ptr [si+11], ah
                mov     byte ptr [si+12], al
                ;
                ;  Get hour digits
                ;
                mov     ah, byte ptr es:[di+19]
                call    conv_bcd
                mov     byte ptr [si+14], ah
                mov     byte ptr [si+15], al
                ;
                ;  Get minute digits
                ;
                mov     ah, byte ptr es:[di+18]
                call    conv_bcd
                mov     byte ptr [si+17], ah
                mov     byte ptr [si+18], al
                ;
                mov     dx, 003Bh     ; line = 0, col = 59
                call    win_set_pos
                call    cputs
                ;
                ;  Point DS to EMS.   (DS==ES==Page Frame!)
                ;
                mov     ax, es
                mov     ds, ax
                ;
                ;  If SMB doesn't have "From"
                ;
                mov     si, di        ; Point ds:si to "from" name
                add     si, 36
                cmp     byte ptr [si], 0
                jne     short @@GotCaller
                ;
                ;    then use NCB caller
                ;
                mov     si, di
@@GotCaller:
                ;
                ;  Display the caller's name
                ;
                mov     dx, 000Eh     ; line = 0, col = 14
                call    win_set_pos
                call    cputs
                ;
                ;  Save caller's name for possible reply
                ;
                mov     ax, cs
                mov     es, ax
                mov     di, offset SendTo      ; Point es:di to SendTo
                call    strcpy
                mov     ax, ds
                mov     es, ax
                ;
                ;  Point past "from" to "to"
                ;
                mov     di, word ptr cs:first_msg
                mov     si, di        ; Point ds:si to from name
                add     si, 36
@@next2:
                lodsb                 ; Search forward for NULL
                and     al, al
                jnz     @@next2
                inc     si            ; Skip Past 04h
                ;
                ;  Print "to" name
                ;
                mov     dx, 0029h     ; line = 0, col = 41
                call    win_set_pos
                call    cputs
                ;
                ;  Point past "to" to "text"
                ;
@@next3:
                lodsb                 ; Search forward for NULL
                and     al, al
                jnz     @@next3
                inc     si            ; Skip Past 01h
                ;
                ;  Get length of message and NULL terminate it
                ;
                mov     bx, word ptr [si]
                cmp     bx, 128
                jl      @@lenOk
                mov     bx, 128
@@lenOk:
                inc     si
                inc     si
                mov     byte ptr [si+bx], 0
                ;
                ;  Save offset of text for annotation (if any)
                ;
                mov     word ptr cs:text_offset, si
                ;
                ;  Convert any NULLs embedded in text to spaces
                ;
                mov     cx, bx          ; get length
                dec     cx              ; -1
                ;
@@Nulls:
                mov     bx, cx
                cmp     byte ptr [si+bx], 0
                jne     @@NotNull
                mov     byte ptr [si+bx], ' '
@@NotNull:
                loopnz  @@Nulls
                ;
                ;  Display "Text" part of message
                ;
                mov     dx, 0100h            ; line = 1, col = 0
                call    win_set_pos
                mov     bl, byte ptr cs:attr_msg
                call    cputs
                ;
                ;  Restore DS
                ;
                mov     ax, cs
                mov     ds, ax
                ;
                ;  Reset flags
                ;
                mov     byte ptr NoReply, 0
                mov     byte ptr NoShort, 0
                ;
                ;  If SendTo is "Message Center"
                ;
                cmp     word ptr SendTo+0,  'eM'
                jne     @@EndMsgC
                cmp     word ptr SendTo+2,  'ss'
                jne     @@EndMsgC
                cmp     word ptr SendTo+4,  'ga'
                jne     @@EndMsgC
                cmp     word ptr SendTo+6,  ' e'
                jne     @@EndMsgC
                mov     byte ptr NoReply, 1
                mov     byte ptr NoShort, 1
                jmp     @@PressEsc
@@EndMsgC:

                ;
                ;  If SendTo is "FAXDEPT"
                ;
                cmp     word ptr SendTo+0,  'AF'
                jne     @@EndFaxD
                cmp     word ptr SendTo+2,  'DX'
                jne     @@EndFaxD
                cmp     word ptr SendTo+4,  'PE'
                jne     @@EndFaxD
                mov     byte ptr cs:NoReply, 1
                mov     byte ptr cs:NoShort, 1
                jmp     @@PressEsc
@@EndFaxD:
                ;
                ;  If SendTo is "Node::name"
                ;
                cmp     word ptr SendTo+1,  '::'
                je      @@YesNode
                cmp     word ptr SendTo+2,  '::'
                je      @@YesNode
                cmp     word ptr SendTo+3,  '::'
                je      @@YesNode
                cmp     word ptr SendTo+4,  '::'
                je      @@YesNode
                cmp     word ptr SendTo+5,  '::'
                je      @@YesNode
                cmp     word ptr SendTo+6,  '::'
                je      @@YesNode
                jmp     @@EndNode
@@YesNode:
                mov     byte ptr NoReply, 1
                mov     byte ptr NoShort, 1
                jmp     @@PressEsc
@@EndNode:

                ;
                ;  If SendTo is "PATHWORKS Mail"
                ;
                cmp     word ptr SendTo+0,  'AP'
                jne     @@NotPCSA
                cmp     word ptr SendTo+2,  'HT'
                jne     @@NotPCSA
                cmp     word ptr SendTo+4,  'OW'
                jne     @@NotPCSA
                cmp     word ptr SendTo+6,  'KR'
                jne     @@NotPCSA
                cmp     word ptr SendTo+8,  ' S'
                jne     @@NotPCSA
                ;
                ;    make sure message contains "from:"
                ;
                add     si, 13
                cmp     byte ptr es:[si], ':'
                jne     @@NotPCSA
                ;
                ;    find end of "from:" name
                ;
                add     si, 2
                mov     di, si
                mov     al, ' '
                mov     cx, 16
                cld
                repne   scasb
                jcxz    @@NotPCSA
                ;
                ;    null-terminate "from:" name
                ;
                mov     byte ptr es:[di], 0
                ;
                ;    extract SendTo from message
                ;
                mov     ax, es
                mov     ds, ax                 ; DS to EMS
                mov     ax, cs
                mov     es, ax                 ; ES to Data
                ;
                mov     di, offset SendTo      ; Point es:di to SendTo
                call    strcpy
                ;
                mov     ax, ds
                mov     es, ax                 ; ES to EMS
                mov     ax, cs
                mov     ds, ax                 ; DS to Data
                ;
                ;    and disable Shortcut keys
                ;
                mov     byte ptr NoShort, 1
                jmp     @@PressEsc
                ;
@@NotPCSA:
                ;
                ;  Display Shortcut key help
                ;
                mov     dx, 0300h            ; line = 3, col = 0
                call    win_set_pos
                mov     si, offset MsgShort
                mov     bl, byte ptr attr_brdr
                call    cputs
                ;
@@PressEsc:
                ;
                ;  Restore ES
                ;
                mov     ax, ds
                mov     es, ax
                ;
                ;  Display "Press Esc" message
                ;
                mov     dx, 0400h            ; line = 4, col = 0
                call    win_set_pos
                mov     si, offset MsgEsc1
                call    cputs
                ;
                cmp     byte ptr NoReply, 1
                je      @@GetKey
                ;
                ;  If Reply is not disabled
                ;
                mov     si, offset MsgEsc2
                call    cputs
                ;
                ;  Get key
                ;
@@GetKey:
                call    get_key
                cmp     ax, 0000h               ; Did we time out
                jne     @@CheckKey
                ;
                ;  We timed out:  drop for awhile to let other thing run
                ;
                mov     ax, word ptr drop_time
                mov     word ptr key_timer, ax
                mov     ax, word ptr second_wait
                mov     word ptr wait_time, ax
                jmp     @@Drop
                ;
@@CheckKey:
                cmp     al, 1Bh                 ; Esc?
                je      @@Done
                cmp     ax, 7500h               ; Ctrl-End?
                je      @@ClearQueue
                ;
                cmp     byte ptr NoShort, 1
                je      @@TryReply
                ;
                cmp     ax, 3B00h               ; F1?
                je      @@F1
                cmp     ax, 3C00h               ; F2?
                je      @@F2
                cmp     ax, 3D00h               ; F3?
                je      @@F3
                cmp     ax, 3E00h               ; F4?
                je      @@F4
                cmp     ax, 3F00h               ; F5?
                je      @@F5
                cmp     ax, 4000h               ; F6?
                je      @@F6
                cmp     ax, 4100h               ; F7?
                je      @@F7
@@TryReply:
                cmp     byte ptr NoReply, 1
                je      @@GetKey
                cmp     al, 12h                 ; Ctrl-R?
                jne     @@GetKey
                ;
                ;  Allow reply to message
                ;
@@Send:
                call    send
                jmp     @@Done
                ;
                ;  Clear out message queue
                ;
@@ClearQueue:
                mov     word ptr msg_count, 1       ; Pretend this is only msg
                mov     ax, word ptr next_msg       ; First msg == next_msg
                sub     ax, MSG_SIZE
                mov     word ptr first_msg, ax
                jmp     @@Done
                ;
                ;  F1 Shortcut key
                ;
@@F1:
                mov     word ptr msg_offset, offset ShortF1
                jmp     @@Send
                ;
                ;  F2 Shortcut key
                ;
@@F2:           mov     word ptr msg_offset, offset ShortF2
                jmp     @@Send
                ;
                ;  F3 Shortcut key
                ;
@@F3:           mov     word ptr msg_offset, offset ShortF3
                jmp     @@Send
                ;
                ;  F4 Shortcut key
                ;
@@F4:           mov     word ptr msg_offset, offset ShortF4
                jmp     @@Send
                ;
                ;  F5 Shortcut key
                ;
@@F5:           mov     word ptr msg_offset, offset ShortF5
                jmp     @@Send
                ;
                ;  F6 Shortcut key
                ;
@@F6:           mov     word ptr msg_offset, offset ShortF6
                jmp     @@Send
                ;
                ;  F7 Shortcut key
                ;
@@F7:           mov     word ptr msg_offset, offset ShortF7
                jmp     @@Send
                ;
@@Done:
                ;
                ;  Reset flags
                ;
                mov     byte ptr NoReply, 0
                mov     byte ptr NoShort, 0
                ;
                ;  Reset time-out value
                ;
                mov     ax, word ptr first_wait
                mov     word ptr wait_time, ax
                ;
                ;  Point to next message
                ;
                add     word ptr first_msg, MSG_SIZE
                dec     word ptr msg_count
                ;
                ;  Check for queue wrap
                ;
                cmp     word ptr first_msg, MSGS_END
                jl      @@Drop
                mov     word ptr first_msg, MSGS_START
                ;
                ;  Restore screen area
                ;
@@Drop:
                ;
                ;  Put screen area back
                ;
                mov     dh, 0                    ; start line
                mov     si, WINDOW_HEIGHT        ; end line
                mov     bl, 1                    ; 1 = put to screen
                call    gp_screen
                ;
                ;  Restore cursor position
                ;
                mov     dx, vid_cur_pos
                call    set_cur_pos
                ;
                ;  Restore EMS
                ;
                mov     dx, EMSHandle
                call    ems_unmap
                ;
                ;  Done!
                ;
@@Exit:
                ;
                ;  Make sure flags are clear
                ;
                mov     byte ptr send_flg, 0
                mov     word ptr msg_offset, 0
                ;
                ;  Switch back to old PSP
                ;
                mov     ax, 5000h
                mov     bx, word ptr oldpsp
                int     21h
                ;
                ;  Restore registers
                ;
                pop     es
                pop     ds
                pop     si
                pop     di
                pop     dx
                pop     cx
                pop     bx
                pop     ax
                ;
                ;  Switch back to their stack
                ;
                cli
                mov     ss, word ptr cs:old_ss
                mov     sp, word ptr cs:old_sp
                sti
                ;
                ret
process         endp
;
;  Send a message
;
;      Input:   None
;     Output:   None
;
send            proc    near
                push    ax
                push    bx
                push    cx
                push    dx
                push    di
                push    si
                ;
                ;  If sending due to shortcut key then no display
                ;
                cmp     word ptr msg_offset, 0
                jne     @@StartMsg
                ;
                ;  Clear display area
                ;
                call    clear_area
                ;
                ;  Display help
                ;
                mov     dx, 0400h       ; line = 4, col = 0
                call    win_set_pos
                mov     si, offset SendHelp
                mov     bl, byte ptr attr_brdr
                call    cputs
                ;
                ;  Display text stop mark
                ;
                cmp     byte ptr vid_cols, 128
                jae     @@EndStop
                ;
                mov     dh, 02h       ; line = 2
                mov     dl, 128       ; col = max len
                sub     dl, vid_cols  ;       - width of row 1
                call    win_set_pos
                ;
                mov     al, 220
                mov     bh, vid_page
                mov     bl, attr_block
                mov     ch, 0
                mov     cl, vid_cols  ; Length = width of line
                sub     cl, dl        ;          - amount of row used
                mov     ah, 09h       ; Display char and attribute
                int     10h
@@EndStop:
                ;
                ;  Display title
                ;
                mov     dx, 0000h                 ; line = 0, col = 0
                call    win_set_pos
                mov     si, offset SendTitle
                mov     bl, byte ptr attr_brdr
                call    cputs
                ;
                ;  Get destination if we don't have it
                ;
                cmp     byte ptr send_flg, 0
                je      @@Display
                ;
                ;  Get name of destination
                ;
                mov     si, offset SendTo
                mov     di, 15                    ; Set max length
                mov     bl, byte ptr attr_brdr
                call    get_string
                ;
                cmp     al, 1Bh                   ; Esc?
                je      @@Done
                add     si, cx                    ; Null-terminate string
                mov     byte ptr [si], 0
                mov     si, offset SendTo
                ;
                ;  Blank out string in graphics mode
                ;
                mov     dx, 0009h                 ; line = 0, col = 9
                call    win_set_pos
                mov     bl, byte ptr attr_brdr
                call    cputs
                ;
                ;  Upper-case the string
                ;
                call    strupr
                ;
                ;  Display destination name
                ;
@@Display:
                mov     dx, 0009h                 ; line = 0, col = 9
                call    win_set_pos
                mov     si, offset SendTo
                mov     bl, byte ptr attr_brdr
                call    cputs
                ;
                ;  Position cursor
                ;
@@GetMsg:
                mov     dx, 0100h                 ; line = 1, col = 0
                call    win_set_pos
@@StartMsg:
                ;
                ;  Clear SMB to zeroes
                ;
                mov     di, offset send_msg       ; point es:di to send SMB
                xor     ax, ax
                mov     cx, MSG_SIZE
                cld
                rep     stosb
                ;
                ;  Build SMB
                ;
                mov     di, offset send_msg
                mov     byte ptr [di+0],  0FFh    ; Header
                mov     byte ptr [di+1],  'S'
                mov     byte ptr [di+2],  'M'
                mov     byte ptr [di+3],  'B'
                mov     byte ptr [di+4],  0D0h    ; Send single block
                add     di, 35
                mov     byte ptr [di], 04h        ; Start of originator
                inc     di
                ;
                ;  Copy SendFrom to es:[di]
                ;
                mov     si, offset SendFrom
                call    strcpy
                mov     byte ptr [di], 04h        ; Start of destination
                inc     di
                ;
                ;  Copy SendTo to es:[di]
                ;
                mov     si, offset SendTo
                call    strcpy
                mov     byte ptr [di], 01h        ; Start of message text
                inc     di
                ;
                ;  At this point [di] points to the word which will
                ;  contain the message's length.  We point [si] past that
                ;  to point at the message area itself.
                ;
                mov     si, di
                inc     si
                inc     si
                ;
                ;  See if this was a shortcut key
                ;
                cmp     word ptr msg_offset, 0
                je      @@GetText
                ;
                ;  Move shortcut message into SMB
                ;
                push    si
                push    di
                ;
                ;  Set offset to copy to
                ;
                mov     di, si
                ;
                ;  Get text we're replying to
                ;
                mov     bx, si              ; Save register
                mov     si, word ptr text_offset
                push    ds
                mov     ax, EMSFrame
                mov     ds, ax
                call    strcpy
                pop     ds
                mov     si, bx              ; Restore register
                dec     di
                ;
                ;  At this point the original message is in the SMB
                ;    as the message to be sent.
                ;
                ;  SI = the start of the message in the SMB
                ;  DI = the last character in the message in the SMB
                ;
                ;
                ;  Find the CR/NL if any
                ;
                xor     bx, bx

@@CrNl:
                cmp     byte ptr [si+bx], 13
                je      @@FoundEOL
                cmp     byte ptr [si+bx], 10
                je      @@FoundEOL
                inc     bx
                ;
                ;  Check for end of string
                ;
                mov     ax, si
                add     ax, bx
                cmp     ax, di
                jg      @@Ann
                ;
                ;  and end of first line
                ;
                mov     ah, 0
                mov     al, byte ptr vid_cols       ; Max is columns
                dec     ax                          ;   - 1 for the CR
                dec     ax                          ;   - 1 for the NL
                dec     ax                          ;   - 1 to prevent wrap
                cmp     bx, ax
                jl      @@CrNl
                ;
                ;  Found a cr/nl or 80 bytes -- start annotation here
                ;
@@FoundEOL:
                mov     di, si          ; Start of message
                add     di, bx          ;   + offset of cr/nl
                ;
                ;  Add annotation mark -- DI points to spot in message
                ;
@@Ann:
                mov     bx, si          ; Save start of message
                mov     si, offset Annotate
                call    strcpy
                dec     di
                ;
                ;  Add reply
                ;
                mov     si, word ptr msg_offset
                call    strcpy
                ;
                ;  Calculate total length of reply
                ;
                mov     cx, di          ; Get end of message + 1
                sub     cx, bx          ;   and subtract start of message
                ;
                pop     di
                pop     si
                jmp     @@StoreLen
                ;
                ;  Get text to send into ds:si buffer
                ;
@@GetText:
                push    di
                mov     di, 128                   ; Max length is 128
                mov     bl, byte ptr attr_msg
                call    get_string
                pop     di
                cmp     al, 1Bh                   ; Esc?
                je      @@Done
                ;
                ; Store length of string at [di]
                ;
@@StoreLen:
                mov     word ptr [di], cx
                ;
                ; Calculate length of smb.buf and store at smb.bcc
                ;
                add     si, cx                 ; Point to end of string
                mov     ax, si                 ; Save ending offset in AX
                mov     di, offset send_msg    ; Get offset of SMB block
                add     di, 35                 ;   and point to smb.buf
                sub     si, di                 ; Calculate the length
                mov     di, offset send_msg    ; Get offset of SMB block
                add     di, 33                 ;   and point to smb.bcc
                mov     word ptr [di], si      ; Store length
                ;
                ;  Calculate size of SMB block and pass in CX
                ;
                mov     cx, ax                 ; Getting ending offset
                mov     si, offset send_msg    ; Get offset of SMB block
                sub     cx, si                 ; Calculate length of SMB block
                ;
                ; And send it...
                ;
                mov     di, offset SendTo      ; point ds:di to send name
                call    net_send               ; Send it
                ;
                ;  Done!
                ;
@@Done:
                pop     si
                pop     di
                pop     dx
                pop     cx
                pop     bx
                pop     ax
                ret
send            endp


;**********************************************************************
;*  Resident data                                                     *
;**********************************************************************
oldint08        dd      0
oldint09        dd      0
oldint28        dd      0
oldint2F        dd      0

indos_ptr       dd      0
criterr_ptr     dd      0

first_msg       dw      MSGS_START
next_msg        dw      MSGS_START
msg_count       dw      0

;
;  Timers:
;
;           Decremented to 0 every 18.2 times a second.
;           key_timer and timer are the same except that
;           key_timer is cleared to 0 when a key is pressed.
;           Be careful not to allow conflicts between
;           different blocks of code.
;
key_timer       dw      0
timer           dw      0

;
;  Timer values
;
wait_time       dw      2184            ; Time to wait before drop
drop_time       dw      5460            ; Time to stay dropped
first_wait      dw      2184            ; First time wait_time
second_wait     dw      1092            ; Second+ time wait_time

;
;  Beep tone information
;
tone1           dw      0500h
tone2           dw      0700h
tone3           dw      0600h
tone_time       dw      1               ; Delay for beep tones

our_ss          dw      0
our_sp          dw      0
ourpsp          dw      0

old_ss          dw      0
old_sp          dw      0
oldpsp          dw      0

msg_offset      dw      0
text_offset     dw      0

;
;  EMS globals
;
EMSFrame        dw      0
EMSHandle       dw      0
EMSHandle2      dw      0

;
;  Video globals
;
vid_cur_pos     dw      0
vid_start       db      20
vid_mode        db      0
vid_page        db      0
vid_rows        db      0
vid_cols        db      0
vga_state       db      7    dup (0)

attr_brdr       db      71h
attr_block      db      07h
attr_msg        db      0Ah

;
;  Flags
;
busy            db      0
off             db      0
disabled        db      0
windows         db      0
in_int28        db      0
set_name        db      1
graphics        db      0
send_flg        db      0
NoReply         db      0
NoShort         db      0

temp_string     db      'XX', 0
temp_time       db      'Recv', 39 ,'d: MM/DD HH:MM', 0      ; 39 is a '

name_num        db      0
machine_name    db      16   dup (0)

SendTo          db      16   dup (0)
SendFrom        db      16   dup (0)

rcv_ncb         db      64   dup (0)
temp_ncb        db      64   dup (0)

send_msg        db      (MSG_SIZE) dup (0)
recv_msg        db      (MSG_SIZE) dup (0)

SendTitle       db      'Send to: ', 0
SendHelp        db      'Press [Esc] to exit or [Enter] to send.', 0

MsgFrom         db      'Message from:', 0
MsgTo           db      'To:', 0
MsgShort        db      'F1=Thanks  F2=Hold  F3=Call Back  F4=Regarding?  F5=Yes/OK  F6=No  F7=Come in', 0
MsgEsc1         db      'Press [Esc] to exit.', 0
MsgEsc2         db      08, '.', 08, ' or [Ctrl-R] to reply.', 0
MsgCount        db      '[Msgs:   ]', 0

Annotate        db      13, 10, '>> ', 0

ShortF1         db      'Thanks', 0
ShortF2         db      'Hold', 0
ShortF3         db      'Call Back', 0
ShortF4         db      'Regarding?', 0
ShortF5         db      'Yes/OK', 0
ShortF6         db      'No', 0
ShortF7         db      'Come in, please', 0

our_stack       dw      0


;**********************************************************************
;*  Non-resident data                                                 *
;**********************************************************************
os_major        db      0

load_flag       db      0
tsr_ds          dw      0

options_set     db      0

EMSName1        db      'RECV1', 0, 0, 0
EMSName2        db      'RECV2', 0, 0, 0

ErrDos          db      'RECV: DOS versions before 3.0 are not supported.', 13, 10, 0
ErrLoad         db      'RECV: Already loaded.', 13, 10, 0
ErrUnload       db      'RECV: Not already loaded.', 13, 10, 0
ErrMach         db      'RECV: Machine name not set.', 13, 10, 0
ErrNoNet        db      'RECV: NetBios not loaded.', 13, 10, 0
ErrCantUn       db      'RECV: Cannot unload at this time.', 13, 10, 0
ErrNetBIOS      db      'RECV: NetBios error #', 13, 10, 0
ErrNoEMS        db      'RECV: EMS not loaded.', 13, 10, 0
ErrEMSMem       db      'RECV: Failed allocating 64k of EMS.', 13, 10, 0

MsgOptions      db      'RECV: Option(s) set.', 13, 10, 0

MsgUnload       db      'RECV: Unloaded.', 13, 10, 0


;**********************************************************************
;*  NetBIOS functions (Non-resident)                                  *
;**********************************************************************
;
;  Put name into NCB
;
;      Input:  es:bx -> NCB
;              ds:si -> name string
;     Output:  None
;
net_put_name    proc    near
                ;
                ;  Initialize
                ;
                push    ax
                push    cx
                push    di
                push    bx
                ;
                ;  Set ncb_name field to spaces
                ;
                mov     di, bx                   ; es:di -> name field
                add     di, ncb_name
                push    di                       ; Save offset of ncb_name
                mov     cx, 15                   ; Set field size
                mov     al, ' '                  ; Set to blank
                cld
                rep     stosb
                ;
                ;  Get length of name
                ;
                mov     di, si                   ; Point es:di to name
                xor     al, al
                mov     cx, -1
                repne   scasb
                not     cx
                dec     cx
                ;
                ;  Copy name to ncb_name
                ;
                pop     di                       ; Restore offset of ncb_name
                rep     movsb
                ;
                ;  Set 16th byte to 0x03
                ;    - 0x03 suffix indicates a user name for msg delivery
                ;      0x05 suffix indicates a forwarded name
                ;      See SMB doc.
                ;
                pop     bx
                mov     byte ptr [es:bx].ncb_name+15, 03h
                ;
                ;  Done
                ;
                pop     di
                pop     cx
                pop     ax
                ret
net_put_name    endp
;
;  Issue NetBIOS Add Name function
;
;      Input:  ds:si -> name string
;     Output:  None
;
net_add_name    proc    near
                ;
                ;  Initialize
                ;
                push    ax
                push    bx
                ;
                ;  Zero out the NCB
                ;
                mov     bx, offset temp_ncb
                call    net_zero
                ;
                ;  Put name into NCB
                ;
                call    net_put_name
                ;
                ;  Issue ADD NAME command
                ;
                mov     al, NCB_ADD_NAME
                call    NetBios
                ;
                ;  Save number assigned
                ;
                mov     al, temp_ncb.ncb_num
                mov     byte ptr name_num, al
                ;
                ;  Done!
                ;
                pop     bx
                pop     ax
                ret
net_add_name    endp
;
;  Issue NetBIOS Delete Name function
;
;      Input:  ds:si -> name string
;     Output:  None
;
net_del_name    proc    near
                ;
                ;  Initialize
                ;
                push    ax
                push    bx
                ;
                ;  Zero out the NCB
                ;
                mov     bx, offset temp_ncb
                call    net_zero
                ;
                ;  Put name into NCB
                ;
                call    net_put_name
                ;
                ;  Issue DELETE NAME command
                ;
                mov     al, NCB_DEL_NAME
                call    NetBios
                ;
                ;  Done!
                ;
                pop     bx
                pop     ax
                ret
net_del_name    endp
;
;  Issue NetBIOS Cancel function
;
net_cancel      proc    near
                ;
                ;  Initialize
                ;
                push    ax
                push    bx
                ;
                ;  Zero out the NCB
                ;
                mov     bx, offset temp_ncb
                call    net_zero
                ;
                ;  Specify rcv_ncb to be cancelled
                ;
                mov     bx, offset rcv_ncb
                mov     word ptr [es:bx].ncb_buf+2, ds
                mov     word ptr [es:bx].ncb_buf,   bx
                ;
                ;  Issue CANCEL command
                ;
                mov     al, NCB_CANCEL
                mov     bx, offset temp_ncb
                call    NetBios
                ;
                ;  Done!
                ;
                pop     bx
                pop     ax
                ret
net_cancel      endp


;**********************************************************************
;*  Utility functions (non-resident)                                  *
;**********************************************************************
;
;    Convert string number to a number
;
;      Input:  es:di -> Number string
;     Output:  cx    =  Number value
;
atoi            proc
                push    ax
                push    si
                push    ds
                ;
                ;  Point ds:si to same place as es:di
                ;
                mov     si, di                  ; Make ds:si == es:di
                mov     ax, es
                mov     ds, ax
                ;
                ;  Initialize
                ;
                sub     cx, cx
                ;
                ;  Process string
                ;
@@Loop:
                cmp     BYTE PTR [si], '0'  ;check if below zero
                jb      @@Done
                cmp     BYTE PTR [si], '9'  ;check if above nine
                ja      @@Done
                ;
                ;  Still more so keep going
                ;
                mov     ax, 10              ;base ten
                mul     cx                  ;multiply
                jc      @@Overflow          ;jump if overflow
                mov     cx, ax              ;back into total
                lodsb                       ;load number
                sub     al, '0'             ;convert to binary
                sub     ah, ah
                add     cx, ax              ;add to total
                jmp     @@Loop
                ;
                ;  Overflow
                ;
@@Overflow:
                sub     cx, cx              ; Return 0
                ;
                ;  Done!
                ;
@@Done:
                pop     ds
                pop     si
                pop     ax
                ret
atoi            endp


;**********************************************************************
;*  Option switch functions                                           *
;**********************************************************************
;
;    Unload TSR if possible
;
Unload:
                ;
                ;  Make sure we are loaded (else why unload?)
                ;
                cmp     byte ptr load_flag, 0
                jne     Loaded
                ;
                ;  "RECV: Not already loaded."
                ;
                mov     si, offset ErrUnload
                jmp     Error
Loaded:
                ;
                ;  Make sure that INT 08 can be unhooked
                ;
                mov     ax, 03508h              ; Get vector int es:bx
                int     21h
                cmp     bx, offset int08        ; Same offset?
                jne     UnloadFail
                mov     bx, es
                cmp     bx, tsr_ds              ; Same segment?
                jne     UnloadFail
                ;
                ;  Make sure that INT 09 can be unhooked
                ;
                mov     ax, 03509h              ; Get vector int es:bx
                int     21h
                cmp     bx, offset int09        ; Same offset?
                jne     UnloadFail
                mov     bx, es
                cmp     bx, tsr_ds              ; Same segment?
                jne     UnloadFail
                ;
                ;  Make sure that INT 28 can be unhooked
                ;
                mov     ax, 03528h              ; Get vector int es:bx
                int     21h
                cmp     bx, offset int28        ; Same offset?
                jne     UnloadFail
                mov     bx, es
                cmp     bx, tsr_ds              ; Same segment?
                jne     UnloadFail
                ;
                ;  Make sure that INT 2F can be unhooked
                ;
                mov     ax, 0352Fh              ; Get vector int es:bx
                int     21h
                cmp     bx, offset int2F        ; Same offset?
                jne     UnloadFail
                mov     bx, es
                cmp     bx, tsr_ds              ; Same segment?
                jne     UnloadFail
                ;
                ;  Make sure that we aren't popped up
                ;
                mov     ax, tsr_ds              ; Switch to TSR ds
                mov     ds, ax
                inc     byte ptr busy
                cmp     byte ptr busy, 1
                je      UnloadOK
                ;
                ;  No good, display message and return
                ;
UnloadFail:
                dec     byte ptr busy
                mov     ax, cs                  ; Switch back to our ds
                mov     ds, ax
                mov     si, offset ErrCantUn    ; Set error message
                jmp     UnloadDone
                ;
                ;  OK to unload
                ;
UnloadOK:
                ;
                ;  Switch to being the TSR
                ;
                mov     ax, 5100h               ; Get current PSP
                int     21h
                mov     word ptr oldpsp, bx
                ;
                mov     ax, 5000h               ; Set current PSP
                mov     bx, word ptr ourpsp
                int     21h
                ;
                ;  Make ES == DS
                ;
                mov     ax, ds
                mov     es, ax
                ;
                ;  Restore interrupt vectors
                ;
                push    ds
                mov     ax, 02508h                  ; INT 08
                lds     dx, dword ptr es:oldint08
                int     21h
                mov     ax, 02509h                  ; INT 09
                lds     dx, dword ptr es:oldint09
                int     21h
                mov     ax, 02528h                  ; INT 28
                lds     dx, dword ptr es:oldint28
                int     21h
                mov     ax, 0252Fh                  ; INT 2F
                lds     dx, dword ptr es:oldint2F
                int     21h
                pop     ds
                ;
                ;  Free EMS memory
                ;
                mov     ah, 45h
                mov     dx, word ptr EMSHandle
                int     67h
                ;
                mov     ah, 45h
                mov     dx, word ptr EMSHandle2
                int     67h
                ;
                ;  Cancel outstanding NetBIOS receive
                ;
                ; call    net_cancel
                ;
                ;  Except!
                ;
                ;       Because the Cancel doesn't seem to work...
                ;
                ;       Send a message to clear the outstanding receive
                ;       (the "off" flag will prevent another receive being issued)
                ;
                mov     byte ptr off,  1
                mov     cx, MSG_SIZE
                mov     si, offset send_msg
                mov     di, offset machine_name
                call    net_send
                ;
                ;  Delete the PC's receive name
                ;
                cmp     byte ptr set_name, 0
                je      SkipDel
                mov     si, offset machine_name
                call    net_del_name
SkipDel:
                ;
                ;  Ok we are unhooked, now let's terminate
                ;
                ;       Set the TSR PSP owner to be us
                ;
                mov     ax, ourpsp      ; point es:di to parent PSP field
                mov     es, ax          ; ** Note that ES != DS now!
                mov     di, 16h
                mov     bx, oldpsp
                mov     word ptr es:[di], bx
                ;
                ;       Set the TSR termination address to be UnloadResume:
                ;
                mov     di, 0Ah         ; point es:di to terminate addr field
                mov     word ptr es:[di],   offset UnloadResume
                mov     word ptr es:[di+2], cs
                ;
                ;  Save our stack
                ;
                mov     word ptr cs:our_ss, ss
                mov     word ptr cs:our_sp, sp
                ;
                ;       Terminate the TSR
                ;
                mov     ax,4C00h                ; Exit to DOS
                int     21h
                ;
                ;       What!!  What are we doing here?
                ;
                mov     ax,4C01h                ; Exit to DOS
                int     21h
                ;
                ;  We resume here when the TSR has terminated
                ;
                ;       Note:  all registers are trashed at this point
                ;
UnloadResume:
                ;
                ;  Restore data segment
                ;
                mov     ax, cs
                mov     ds, ax
                mov     es, ax
                ;
                ;  Restore stack
                ;
                cli
                mov     ss, word ptr our_ss
                mov     sp, word ptr our_sp
                sti
                ;
                ;  Set message saying we are done
                ;
                mov     si, offset MsgUnload
UnloadDone:
                ;
                ;  Display message
                ;
                mov     bl, byte ptr attr_msg
                call    cputs
                ;
                mov     ax,4C00h                ; Exit to DOS
                int     21h


;**********************************************************************
;*  Main routine                                                      *
;**********************************************************************
Main:
                ;
                ;  Initialize
                ;
                mov     ax, cs          ; Set data segment
                mov     ds, ax
                ;
                ;  Save our PSP segment
                ;
                mov     word ptr ourpsp, es
                ;
                ;  Get current video mode
                ;
                mov     ah, 0Fh        ; INT 10: Get Video Mode
                int     10h
                mov     vid_page, bh
                mov     vid_cols, ah
                ;
                ;  Color
                ;
                ;
                ;  Get DOS major version
                ;
                mov     ax, 3000h       ; Get DOS version
                int     21h
                mov     byte ptr os_major, al
                ;
                ;  Check DOS Version
                ;
                cmp     byte ptr os_major, 3
                jae     short DOSOk
                ;
                ;  "RECV: Early versions of DOS are not supported."
                ;
                mov     si, offset ErrDOS
                jmp     Error
                ;
DOSOk:
                ;
                ;  See if we are loaded already
                ;
                mov     ah, MULTIPLEX_ID
                mov     al, INSTALL_CHECK
                int     2Fh
                mov     byte ptr load_flag, al     ; 0 is not loaded, FF is loaded
                mov     word ptr tsr_ds,    bx
                ;
                ;  If we are not already loaded then set tsr_ds to current ds
                ;
                cmp     al, 0
                jne     CmdLine
                mov     ax, ds
                mov     word ptr tsr_ds, ax
                ;
                ;  Get command line options
                ;
CmdLine:
                mov     ax, ourpsp      ; point es:di to command length
                mov     es, ax
                mov     di, 80h         
                xor     cx, cx          ; and get length into CX
                mov     cl, byte ptr es:[di]
                inc     di              ; point past length to command itself
                ;
                ;  Scan for next /
                ;
Scan:
                mov     al, '/'         ; Search for '/' character
                cld
                repne   scasb
                ;
                ;  If no more switches then stop looking
                ;
                jnz     ScanEnd
                ;
                ;  If next char is a 'U' or a 'u' then unload
                ;
                cmp     byte ptr es:[di], 'U'
                je      SetUnload
                cmp     byte ptr es:[di], 'u'
                je      SetUnload
                ;
                ;  If next char is a 'T' or a 't' then tone setting
                ;
                cmp     byte ptr es:[di], 'T'
                je      SetTone
                cmp     byte ptr es:[di], 't'
                je      SetTone
                ;
                ;  If next char is a 'D' or a 'd' then disable recv'r
                ;
                cmp     byte ptr es:[di], 'D'
                je      SetDisabled
                cmp     byte ptr es:[di], 'd'
                je      SetDisabled
                ;
                ;  If next char is a 'E' or a 'e' then enable recv'r
                ;
                cmp     byte ptr es:[di], 'E'
                je      SetEnabled
                cmp     byte ptr es:[di], 'e'
                je      SetEnabled
                ;
                ;  Search for next option
                ;
                jmp     Scan
                ;
                ;  Set Unload flag -- actually just go do it.
                ;
SetUnload:
                jmp     Unload
                ;
                ;  Set Tone Options
                ;
SetTone:        inc     di                      ; Point past the T/t
                ;
                cmp     byte ptr es:[di], 'D'
                je      SetTTime
                cmp     byte ptr es:[di], 'd'
                je      SetTTime
                cmp     byte ptr es:[di], '1'
                je      SetTone1
                cmp     byte ptr es:[di], '2'
                je      SetTone2
                cmp     byte ptr es:[di], '3'
                je      SetTone3
                ;
                jmp     Scan
                ;
                ;  Set Tone 1
                ;
SetTone1:       inc     di
                call    atoi
                push    ax
                push    es
                mov     ax, word ptr tsr_ds
                mov     es, ax
                mov     word ptr es:tone1, cx
                pop     es
                pop     ax
                mov     byte ptr options_set, 1
                jmp     Scan
                ;
                ;  Set Tone 2
                ;
SetTone2:       inc     di
                call    atoi
                push    ax
                push    es
                mov     ax, word ptr tsr_ds
                mov     es, ax
                mov     word ptr es:tone2, cx
                pop     es
                pop     ax
                mov     byte ptr options_set, 1
                jmp     Scan
                ;
                ;  Set Tone 3
                ;
SetTone3:       inc     di
                call    atoi
                push    ax
                push    es
                mov     ax, word ptr tsr_ds
                mov     es, ax
                mov     word ptr es:tone3, cx
                pop     es
                pop     ax
                mov     byte ptr options_set, 1
                jmp     Scan
                ;
                ;  Set Tone Time
                ;
SetTTime:
                inc     di                      ; Point past the D/d
                call    atoi
                push    ax
                push    es
                mov     ax, word ptr tsr_ds
                mov     es, ax
                mov     word ptr es:tone_time, cx
                pop     es
                pop     ax
                mov     byte ptr options_set, 1
                jmp     Scan
                ;
                ;  Set Disabled Mode
                ;
SetDisabled:
                push    bx
                mov     bl, 1
                jmp     SetEnaDis
                ;
                ;  Set Enabled Mode
                ;
SetEnabled:
                push    bx
                mov     bl, 0
;
SetEnaDis:
                push    ax
                push    es
                mov     ax, word ptr tsr_ds
                mov     es, ax
                mov     byte ptr es:disabled, bl
                pop     es
                pop     ax
                pop     bx
                ;
                mov     byte ptr options_set, 1
                ;
                jmp     Scan
ScanEnd:
                ;
                ;  Make sure we aren't loaded already
                ;
                cmp     byte ptr load_flag, 0
                je      NotLoaded
                ;
                ;  If options set then no error
                ;
                cmp     byte ptr options_set, 0
                je      LoadMsg
                mov     si, offset MsgOptions
                jmp     Error
                ;
                ;  "RECV: Already loaded."
                ;
LoadMsg:
                mov     si, offset ErrLoad
                jmp     Error
                ;
NotLoaded:
                ;
                ;  Color or B&W?
                ;
                push    es                      ; Save ES register
                mov     ax, 40h                 ; Point ES to BIOS data seg
                mov     es, ax
                mov     ax, es:[10h]            ; Get installed HW
                pop     es                      ; Restore ES register
                and     ax, 0030h               ; Mask out video mode
                cmp     ax, 0030h               ; If not B&W
                jne     @@EndVideo              ;   continue
                ;
                ;  If B&W modify default attributes
                ;
                mov     byte ptr attr_msg,  07h
                mov     byte ptr attr_brdr, 70h
                ;
                ;  If B&W, Is display Herc/Ramfont?
                ;
@@EndVideo:
                ;
                ;  Set up stack for later
                ;
                mov     word ptr our_ss, ds
                mov     ax, offset our_stack
                add     ax, RES_STACK
                mov     word ptr our_sp, ax
                ;
                ;  Get address of In-DOS flag
                ;
                mov     ax, 3400h
                int     21h
                mov     word ptr indos_ptr+2, es
                mov     word ptr indos_ptr,   bx
                ;
                ;  Get address of Critical error flag
                ;
                ;        criterr_ptr = es:bx-1
                ;
                dec     bx
                mov     word ptr criterr_ptr+2, es
                mov     word ptr criterr_ptr,   bx
                ;
                ;  Make sure EMS is available
                ;
                mov     ax, 3567h               ; Get int vector 67h
                int     21h
                cmp     byte ptr es:[10], 'E'
                jne     NoEMS
                cmp     byte ptr es:[11], 'M'
                jne     NoEMS
                cmp     byte ptr es:[12], 'M'
                jne     NoEMS
                cmp     byte ptr es:[13], 'X'
                jne     NoEMS
                cmp     byte ptr es:[14], 'X'
                jne     NoEMS
                cmp     byte ptr es:[15], 'X'
                jne     NoEMS
                cmp     byte ptr es:[16], 'X'
                jne     NoEMS
                cmp     byte ptr es:[17], '0'
                je      EMSOk
                ;
NoEMS:
                ;
                ;  No EMS in system
                ;
                mov     si, offset ErrNoEMS
                jmp     Error
                ;
EMSOk:
                ;
                ;  Get the page frame segment
                ;
                mov     ah, 41h
                int     67h
                mov     EMSFrame, bx
                ;
                ;  Allocate 64K of EMS for saving screen, msgs, etc
                ;
                mov     ah, 43h
                mov     bx, 4             ; 4 pages of 16K each
                int     67h
                cmp     ah, 0
                jne     EMSMemErr
                ;
                ;  Save memory handle
                ;
                mov     word ptr EMSHandle, dx
                ;
                ;  Allocate a second handle to save context
                ;
                mov     ax, 5A00h
                mov     bx, 0
                int     67h
                cmp     ah, 0
                je      EMSMemOk
                ;
                ;  Error allocating memory
                ;
EMSMemErr:
                mov     si, offset ErrEMSMem
                jmp     Error
EMSMemOk:
                ;
                ;  Save memory handle
                ;
                mov     word ptr EMSHandle2, dx
                ;
                ;  Name the handles we just allocated (just to be nice)
                ;
                mov     ax, 5301h           ; Set EMS handle name
                mov     si, offset EMSName2
                int     67h
                ;
                mov     ax, 5301h           ; Set EMS handle name
                mov     dx, EMSHandle
                mov     si, offset EMSName1
                int     67h
                ;
                ;  Tag the EMS memory so we can verify it later
                ;
                call    ems_map
                jc      Error
                mov     ax, EMSFrame
                mov     es, ax
                ;
                mov     word ptr es:[0FFFCh], 'ER'
                mov     word ptr es:[0FFFEh], 'VC'
                ;
                call    ems_unmap
                mov     ax, ds
                mov     es, ax
                ;
                ;  Get machine name
                ;
                mov     dx, offset machine_name
                mov     ax, 5E00h
                int     21h
                ;
                or      ch, ch
                je      short MachineBad
                ;
                cmp     byte ptr machine_name, ' '
                jne     short MachineOk
                ;
MachineBad:
                mov     si, offset ErrMach
                jmp     Error
                ;
MachineOK:
                ;
                ;  Limit machine name to 15 bytes
                ;
                mov     byte ptr machine_name+15, 0
                ;
                ;  Convert to upper-case
                ;
                mov     si, offset machine_name
                call    strupr
                ;
                ;  Make ES == DS
                ;
                mov     di, ds
                mov     es, di
                ;
                ;  Point si to last non-null character
                ;
                mov     di, si            ; Point to string
                xor     al, al
                mov     cx, -1
                repne   scasb             ; Find null byte
                mov     si, di            ; Point ds:si to char before null
                dec     si
                dec     si
                ;
                ;  Strip trailing blanks, if any
                ;
Trim:           cmp     byte ptr [si], ' '
                jne     EndTrim
                mov     byte ptr [si], 0
                dec     si
                jmp     Trim
EndTrim:
                ;
                ;  Set SendFrom to machine's name by default
                ;
                mov     si, offset machine_name
                mov     di, offset SendFrom
                call    strcpy
                ;
                ;  Make sure NetBIOS is loaded
                ;
                mov     ax, 0355Ch    ; Get INT 5C vector
                int     021h
                ;
                ;  If 5C vector is NULL, NetBIOS not loaded
                ;
                mov     ax, es
                or      ax, ax
                jne     short NetBiosOK
                ;
                ;  "RECV: NetBios not loaded."
                ;
                mov     si, offset ErrNoNet
                jmp     Error
                ;
NetBiosOK:
                ;
                ;  Make ES == DS
                ;
                mov     bx, ds
                mov     es, bx
                ;
                ;  Make sure it's really NetBIOS
                ;
                mov     bx, offset temp_ncb
                call    net_zero
                ;
                mov     al, 7Fh
                call    NetBios
                cmp     byte ptr temp_ncb.ncb_rc, 03h
                je      short NetOK
                ;
                ;  "RECV: NetBios not loaded."
                ;
                mov     si, offset ErrNoNet
                jmp     Error
                ;
NetOK:
                ;
                ;  Add machine name to list of receive names
                ;
                mov     si, offset machine_name
                call    net_add_name
                ;
WaitCplt:
                cmp     byte ptr temp_ncb.ncb_cplt, 0FFh
                je      short WaitCplt
                ;
                cmp     byte ptr temp_ncb.ncb_cplt, 0
                je      short AddOK
                mov     byte ptr set_name, 0             ; We did not set name
                cmp     byte ptr temp_ncb.ncb_cplt, 0Dh  ; Ok if already added
                je      short AddOK
                ;
                ;  Display error message
                ;
                mov     si, offset ErrNetBIOS
                mov     bl, byte ptr attr_msg
                call    cputs
                ;
                ;  Get error code in printable form
                ;
                mov     al, byte ptr temp_ncb.ncb_cplt ; get rc
                mov     ah, al                         ; save it
                shr     al, 4                          ; get high nybble
                add     al, 90h                        ; convert
                daa
                adc     al, 40h
                daa
                mov     temp_string, al               ; save
                ;
                mov     al, ah                        ; get value back
                and     al, 0Fh                       ; mask off upper nybble
                add     al, 90h                       ; convert
                daa
                adc     al, 40h
                daa
                mov     temp_string+1, al             ; save
                ;
                xor     al, al                        ; null terminate
                mov     temp_string+2, al
                ;
                ;  Display NCB error code in hex
                ;
                mov     si, offset temp_string
                mov     bl, byte ptr attr_msg
                call    cputs
                ;
                mov     ax, 4C01h
                int     21h                     ; Exit to DOS
                ;
AddOK:
                ;
                ;  Hook up to INT 08 so we get some CPU
                ;
                mov     ax, 03508h               ; Get vector
                int     21h
                mov     word ptr oldint08+2, es
                mov     word ptr oldint08,   bx
                ;
                mov     ax, 02508h               ; Set vector
                mov     dx, offset int08
                int     21h
                ;
                ;  Hook up to INT 09 so we can see keystrokes go by
                ;
                mov     ax, 03509h               ; Get vector
                int     21h
                mov     word ptr oldint09+2, es
                mov     word ptr oldint09,   bx
                ;
                mov     ax, 02509h               ; Set vector
                mov     dx, offset int09
                int     21h
                ;
                ;  Hook up to INT 28 so we can pop up over DOS prompt
                ;
                mov     ax, 03528h               ; Get vector
                int     21h
                mov     word ptr oldint28+2, es
                mov     word ptr oldint28,   bx
                ;
                mov     ax, 02528h               ; Set vector
                mov     dx, offset int28
                int     21h
                ;
                ;  Hook up to INT 2F so people can talk to us
                ;
                mov     ax, 0352Fh               ; Get vector
                int     21h
                mov     word ptr oldint2F+2, es
                mov     word ptr oldint2F,   bx
                ;
                mov     ax, 0252Fh               ; Set vector
                mov     dx, offset int2F
                int     21h
                ;
                ;  Make ES == DS
                ;
                mov     bx, ds
                mov     es, bx
                ;
                ;  Issue async datagram receive
                ;
                mov     si, offset recv_msg
                call    net_receive
                ;
                ;  Free environment
                ;
                mov     es, word ptr ourpsp
                mov     bx, 2Ch
                mov     es, es:[bx]
                mov     ah, 49h
                int     21h
                ;
                ;  Compute resident memory size
                ;
                ;    paragraphs = (our_ss + (our_sp >> 4) + 1) - ourpsp;
                ;
                mov     ax, word ptr our_sp
                shr     ax, 4
                mov     dx, word ptr our_ss
                add     dx, ax
                inc     dx
                sub     dx, word ptr ourpsp
                ;
                ;  Go TSR: keep(0, paragraphs (dx) );
                ;
                mov     ax, 03100h
                int     21h
;
;  Display error message and exit with errorlevel 1
;
Error:
                mov     bl, byte ptr attr_msg
                call    cputs
                inc     sp
                inc     sp
                ;
                mov     ax,4C01h
                int     21h                     ; Exit to DOS
_TEXT           ends

_STACK          segment STACK 'STACK'
                db      256 dup (?)
                ends

                end
