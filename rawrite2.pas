{$m 8192, 32768, 65536}
program rawrite2;

uses dos;

type
    TSector = array [0..1024] of Byte;
    TRW = (RW_read, RW_write);

    TBPB = packed record
        BytesPerSector: Word;
        SectorsPerCluster: Byte;
        NumOfReservedSectors: Word;
        NumOfFATs: Byte;
        NumOfRootEntries: Word;
        SectorsPerDisk: Word;
        MediaDesc: Byte;
        SectorsPerFAT: Word;
        SectorsPerTrack: Word;
        NumOfHeads: Word;
        Unused: array [1..4] of Word;
        DriveNumber: Byte;
        Unused2: Byte;
        ExtBootSignature: Byte;
        VolSerialNumber: array [1..4] of Byte;
        VolumeLabel: array [1..11] of Char;
        FSType: array [1..8] of Byte;
    end;

    TBootSector = packed record
        JmpInstr: array [1..3] of Byte;
        OemName: array [1..8] of Char;
        BPB: TBPB;
        BootCode: array [1..448] of Byte;
        Signature: Word;
    end;

    TRWSectorsFunc = function(Action: TRW; Track, Head, Sector, Count: Byte;
                              BufPtr: Pointer): Word;

const
    MaxNumOfTracks = 84;
    MaxNumOfSectors = 128;
    MaxBytesPerSector = 4096;

var
    DiskError: Byte;
    CursorShape: Word;
    DriveLetter: Char;
    FirstFileArg: Word;

    {parameters}
    Disk, NumOfHeads, NumOfSectors, NumOfTracks: Byte;
    BytesPerSector: Word;
    RWSectorsFunc: TRWSectorsFunc;

function ResetDisk: Byte; assembler;
asm
    xor ah, ah
    mov dl, [Disk]
    int $13
    mov al, ah
end;

{ Needed to put this in assembler or the program will hang. }
function RWSectorsDosAsm(Action: TRW; Sector: Word; Count: Word;
                         BufPtr: Pointer): Word; assembler;
asm
    mov ah, [Action]
    mov al, [Disk]
    mov dx, [Sector]
    mov cx, [Count]
    push ds
    lds bx, [BufPtr]
    push bp
    cmp ah, RW_write
    je @int26
    int $25
    jmp @fin
@int26:
    int $26
@fin:
    pop cx
    pop bp
    pop ds
    jc @err
    xor ax, ax
@err:
    mov [DiskError], ah
    mov ax, [Count]
end;

function RWSectorsDos(Action: TRW; Track, Head, Sector, Count: Byte;
                      BufPtr: Pointer): Word; far;
begin
    RWSectorsDos := RWSectorsDosAsm(Action, Head * NumOfSectors
                                    + Track * NumOfSectors * NumOfHeads
                                    + Sector - 1, Count, BufPtr);
end;

function RWSectorsBios(Action: TRW; Track, Head, Sector, Count: Byte;
                       BufPtr: Pointer): Word; far;
var r: Registers;
    i: Integer;
    Func: Byte;
begin
    i := 3;

    if Action = RW_write then
        Func := 3
    else
        Func := 2;

    repeat
        with r do
        begin
            dl := Disk;
            dh := Head;
            ch := Track;
            cl := Sector;
            al := Count;
            es := Seg(BufPtr^);
            bx := Ofs(BufPtr^);
            ah := Func;
        end;
        Intr($13, r);
        Dec(i);
        if r.ah <> 0 then ResetDisk;
    until (r.ah = 0) or (i = 0);

    RWSectorsBios := r.al;
    DiskError := r.ah;
end;

function ReadSectors(Track, Head, Sector, Count: Byte;
                     BufPtr: Pointer): Boolean;
begin
    RWSectorsFunc(RW_read, Track, Head, Sector, Count, BufPtr);
    ReadSectors := DiskError = 0;
end;

function WriteSectors(Track, Head, Sector, Count: Byte;
                      BufPtr: Pointer): Boolean;
begin
    RWSectorsFunc(RW_write, Track, Head, Sector, Count, BufPtr);
    WriteSectors := DiskError = 0;
end;

procedure Die(const msg: String);
begin
    WriteLn(msg);
    Halt(1);
end;

function GetMemOrDie(Size: LongInt): Pointer;
var Buf: Pointer;
begin
    if (MaxAvail < Size) then Die('Not enough memory');
    GetMem(Buf, Size);
    GetMemOrDie := Buf;
end;

function ExpectKeys(const Accept: String): Char;
var Key: Char;

    function ReadKey: Char; assembler;
    asm
        xor ah, ah
        int 16h;
    end;

begin
    repeat
        Key := Upcase(ReadKey);
        if Pos(Key, Accept) <> 0 then break
        else if Key = #3 then Die(''); { CTRL-C }
    until False;
    ExpectKeys := Key;
end;

procedure ReadYOrHalt;
var Key: Char;
begin
    Write(' (y/n)');
    Key := ExpectKeys('YN');
    WriteLn;
    if Key = 'N' then Halt(1);
end;

function GetFileArg(Number: Word): String;
begin
    GetFileArg := ParamStr(FirstFileArg + Number - 1);
end;

procedure ClearLine;
begin
    Write(#13, '': 79, #13);
end;

function GetDriveNumber(DriveLetter: Char): Byte;
begin
    GetDriveNumber := Ord(DriveLetter) - Ord('A');
end;

function GetBytesPerTrack: Word;
begin
    GetBytesPerTrack := BytesPerSector * NumOfSectors;
end;

function GetBytesPerDisk: LongInt;
begin
    GetBytesPerDisk := LongInt(GetBytesPerTrack) * NumOfTracks * NumOfHeads;
end;

procedure WriteTrackOrDie(Track, Head: Byte; BufPtr: Pointer);
var Sector0: TSector;
begin
    repeat
        Write('Writing track ', Track, ' head ', Head, '': 2, #13);

        if WriteSectors(Track, Head, 1, NumOfSectors, BufPtr) then break;

        WriteLn(#7, 'Error writing track ', Track, ' head ', Head);
        Write('   (R)etry or (C)ancel?', #13);
        if ExpectKeys('RC') = 'C' then Halt(1);
        ReadSectors(0, 0, 1, 1, @Sector0);
    until False;
end;

function ReadSectorOrDie(Track, Head, Sector: Byte; BufPtr: Pointer;
                         CanIgnore: Boolean): Boolean;
var Action: Char;
begin
    ReadSectorOrDie := True;
    repeat
        Write('Reading track ', Track,
                      ' head ', Head,
                    ' sector ', Sector, '   ', #13);

        if ReadSectors(Track, Head, Sector, 1, BufPtr) then break;

        WriteLn(#7, 'Error reading track ', Track,
                                  ' head ', Head,
                                ' sector ', Sector);

        if CanIgnore then
        begin
            Write('   (R)etry (I)gnore (C)ancel?', #13);
            Action := ExpectKeys('RIC');
        end
        else
        begin
            Write('   (R)etry (C)ancel?', #13);
            Action := ExpectKeys('RC');
        end;

        case Action of
            'C': Halt(1);
            'I':
            begin
                FillChar(BufPtr^, BytesPerSector, 0);
                ReadSectorOrDie := False;
                break;
            end;
        end;
    until False;
end;

procedure PrintGeometry;
begin
    WriteLn('Disk ', DriveLetter, ':    Heads: ', NumOfHeads,
            '    Tracks: ', NumOfTracks,
            '    Sectors: ', NumOfSectors,
            '    Bytes per sector: ', BytesPerSector);
end;

procedure GetGeometryFromBPB(const BPB: TBPB);
begin
    BytesPerSector := BPB.BytesPerSector;
    NumOfSectors := BPB.SectorsPerTrack;
    NumOfHeads := BPB.NumOfHeads;

    if (BPB.SectorsPerDisk = 0)
       or (BytesPerSector = 0)
       or (BytesPerSector mod 128 <> 0)
       or not(NumOfHeads in [1, 2])
       or (NumOfSectors = 0) then
        Die('Sector 0 has wrong format');

    NumOfTracks := BPB.SectorsPerDisk div (NumOfSectors * NumOfHeads);
end;

procedure DumpDiskToFile(const Filename: String);
var Track, Head, Sector: Byte;
    ImageFile: File;
    SectorBufPtr: ^TSector;
    BytesPerTrack: Word;
    TrackBufPtr: Pointer;
    NumOfSectorsRead: Word;
begin
    BytesPerTrack := GetBytesPerTrack;
    TrackBufPtr := GetMemOrDie(BytesPerTrack);
    NumOfSectorsRead := 0;
    Assign(ImageFile, Filename);
    Rewrite(ImageFile, 1);

    for Track := 0 to NumOfTracks - 1 do
    begin
        for Head := 0 to NumOfHeads - 1 do
        begin
           Write('Reading track ', Track, ' head ', Head, #13);

           if ReadSectors(Track, Head, 1, NumOfSectors, TrackBufPtr) then
               Inc(NumOfSectorsRead, NumOfSectors)
           else
           begin { Error reading track? Retry reading sectors. }
               SectorBufPtr := TrackBufPtr;
               for Sector := 1 to NumOfSectors do
               begin
                   if (ReadSectorOrDie(Track, Head, Sector, SectorBufPtr,
                                       True)) then
                      Inc(NumOfSectorsRead);
                   SectorBufPtr := Addr(SectorBufPtr^[BytesPerSector]);
               end;
               ClearLine;
           end;

           BlockWrite(ImageFile, TrackBufPtr^, BytesPerTrack);
       end;
    end;

    Close(ImageFile);
    FreeMem(TrackBufPtr, BytesPerTrack);

    WriteLn('Reading completed: ', NumOfSectorsRead, ' of ',
            NumOfSectors * NumOfTracks * NumOfHeads,
            ' sectors have been read')
end;

procedure CheckOutputFile(const Filename: String; Size: LongInt);
var ImageFile: File;
    Fullname: String;
begin
    Fullname := FExpand(Filename);

    if (DiskFree(GetDriveNumber(Fullname[1]) + 1) < Size) then
        Die('Not enough disk space for image file');

    Assign(ImageFile, Fullname);
    {$i-}Reset(ImageFile, 1);{$i+}
    if IOResult = 0 then
    begin
        Close(ImageFile);
        Write('Destination file exists. Overwrite?');
        ReadYOrHalt;
    end;

    {$i-}Rewrite(ImageFile, 1);{$i+}
    if IOResult <> 0 then Die('Error writing to disk, write protected?');
    Close(ImageFile)
end;

procedure DoDiskToImage(const Filename: String; FindOutGeometry: Boolean);
var BootSector: TBootSector;
begin
    if FindOutGeometry then
    begin
        ReadSectorOrDie(0, 0, 1, @BootSector, False);
        GetGeometryFromBPB(BootSector.BPB);
    end;

    CheckOutputFile(Filename, GetBytesPerDisk);
    PrintGeometry;
    DumpDiskToFile(Filename);
end;

procedure WriteFilesToDisk(NumOfFiles: Word);
var Track, Head: Byte;
    FileNum, Len, Len2: Word;
    ImageFile: File;
    BytesPerTrack: Word;
    TrackBufPtr: Pointer;
begin
    BytesPerTrack := GetBytesPerTrack;
    TrackBufPtr := GetMemOrDie(BytesPerTrack);
    FileNum := 1;
    Assign(ImageFile, GetFileArg(1));
    Reset(ImageFile, 1);

    for Track := 0 to NumOfTracks - 1 do
    begin
        for Head := 0 to NumOfHeads - 1 do
        begin
            BlockRead(ImageFile, TrackBufPtr^, BytesPerTrack, Len);
            while (Len <> BytesPerTrack) and (FileNum < NumOfFiles) do
            begin
                Close(ImageFile);
                Inc(FileNum);
                Assign(ImageFile, GetFileArg(FileNum));
                Reset(ImageFile, 1);
                BlockRead(ImageFile, (PChar(TrackBufPtr) + Len)^,
                          BytesPerTrack - Len, Len2);
                Inc(Len, Len2);
            end;
            WriteTrackOrDie(Track, Head, TrackBufPtr);
            if Len <> BytesPerTrack then break;
        end;
        if Len <> BytesPerTrack then break;
    end;

    Close(ImageFile);
    FreeMem(TrackBufPtr, BytesPerTrack);
end;

procedure CheckInputFiles(NumOfFiles: Word; Size: LongInt);
var ImageFile: File;
    ImageSize: LongInt;
    Filename: String;
    i: Integer;
begin
    ImageSize := 0;
    for i := 1 to NumOfFiles do
    begin
        Filename := GetFileArg(i);
        Assign(ImageFile, Filename);
        {$i-}Reset(ImageFile, 1);{$i+}
        if IOResult <> 0 then Die('Error reading image file');
        Inc(ImageSize, FileSize(ImageFile));
        Close(ImageFile);
        if ImageSize > Size then
            WriteLn('Warning: file ', Filename, ' won''t fit to the floppy');
    end;
end;

procedure DoImageToDisk(NumOfFiles: Word; FindOutGeometry: Boolean);
var DiskBS, ImageBS: TBootSector;
    ImageFile: File;
begin
    FileMode := 0; { We only read files in this mode. }

    if FindOutGeometry then
    begin
        ReadSectorOrDie(0, 0, 1, @DiskBS, False);
        ClearLine;

        Assign(ImageFile, GetFileArg(1));
        {$i-}Reset(ImageFile, 1);
        BlockRead(ImageFile, ImageBS, SizeOf(ImageBS));{$i+}
        if IOResult <> 0 then Die('Error reading image file');
        Close(ImageFile);

        if (ImageBS.BPB.BytesPerSector <> DiskBS.BPB.BytesPerSector)
           or (ImageBS.BPB.SectorsPerDisk <> DiskBS.BPB.SectorsPerDisk)
           or (ImageBS.BPB.SectorsPerTrack <> DiskBS.BPB.SectorsPerTrack)
           or (ImageBS.BPB.NumOfHeads <> DiskBS.BPB.NumOfHeads) then
             WriteLn('Floppy sector 0 parameters and image sector 0 parameters differ');

        GetGeometryFromBPB(ImageBS.BPB);
    end;

    CheckInputFiles(NumOfFiles, GetBytesPerDisk);
    PrintGeometry;
    WriteFilesToDisk(NumOfFiles);
end;

procedure Help;
begin
WriteLn('RAWRITE2      A floppy disk image utility      Version 2.0    16-06-2023');
WriteLn('(c) 2023 Vitaly Sinilin, (c) 2000 Miguel Angel Alvarez Cruz');
WriteLn('This program is distributed under GPL 2.0. Use only at your own risk!');
WriteLn;
WriteLn('Usage: to make a disk image            RAWRITE2 U: file [options]');
WriteLn('       to copy an image to disk        RAWRITE2 file(s)... U: [options]');
WriteLn;
WriteLn('where  "U:"    is A: or B:');
WriteLn('       "file"  is the image file name');
WriteLn;
WriteLn('Options:');
WriteLn('       /Fxxx   for xxx size floppy (default 1.44 MB)');
WriteLn('       /R      read size from sector 0 (only DOS floppies)');
WriteLn('       /N      do not use BIOS, use DOS instead');
WriteLn('       /Txx    xx = number of tracks (max 84, default 80)');
WriteLn('       /Sxx    xx = number of sectors (default 18)');
WriteLn('       /Bxxx   xxx = sector size in bytes (default 512)');
WriteLn('       /Hx     x = number of heads (1 or 2, default 2)');
WriteLn;
WriteLn('The destination disk must be previously formatted.');
WriteLn('Note:  For non-standard formats you will need the 2m or fdread utilities.');
WriteLn('       Unfortunately, Windows NT only supports standard floppy formats.');
WriteLn('Note2: For any format but 1.44 MB you should supply the format option for');
WriteLn('       both reading or writing an image file, since the images are raw.');
Write('Press ENTER to read next page...');
ExpectKeys(#13);
WriteLn;
WriteLn('Floppy size options:');
WriteLn('       /F360          360 KB  (old 5.25" floppy)');
WriteLn('       /F720          720 KB');
WriteLn('       /F800          800 KB  (fdformat)');
WriteLn('       /F820          820 KB  (fdformat)');
WriteLn('       /F120          1.2 MB  (5.25" floppy)');
WriteLn('       /F144          1.44 MB (default)');
WriteLn('       /F148          1.48 MB (fdformat)');
WriteLn('       /F160          1.6 MB  (fdformat)');
WriteLn('       /F164          1.64 MB (fdformat)');
WriteLn('       /F168          1.68 MB (fdformat)');
WriteLn('       /F172          1.72 MB (fdformat)');
WriteLn('       /F180          1.8 MB  (2m)');
WriteLn('       /F188          1.88 MB (2m /m)');
WriteLn;
WriteLn('Some examples: ');
WriteLn('       make a 1.44 MB floppy image              RAWRITE2 A: 144.ima');
WriteLn('       make a 1.72 MB floppy image              RAWRITE2 A: 172.ima /f172');
WriteLn('       write the last image to a floppy         RAWRITE2 172.ima A: /f172');
WriteLn('       make an image of an unknown DOS floppy   RAWRITE2 A: disk.ima /r');
WriteLn('       dump several files to a floppy           RAWRITE2 file1 file2 A:');
Halt(1);
end;

procedure Main;
var FindOutGeometry, DiskToImage: Boolean;
    Filename, Arg: String;
    ImageFile: File;
    NumOfFiles: Word;
    i: Integer;

    function GetDriveLetter(const s: String): Char;
    var Letter: Char;
    begin
        GetDriveLetter := #0;
        if (Length(s) = 2) and (s[2] = ':') then
        begin
            Letter := Upcase(s[1]);
            if Letter in ['A'..'Z'] then
               GetDriveLetter := Letter;
        end;
    end;

    procedure ConfirmIfProceed;
    begin
        Write('Proceed anyway?');
        ReadYOrHalt;
    end;

    procedure InvalidOption(const Option: String);
    begin
        WriteLn('Invalid option: ', Option);
        Halt(1);
    end;

    function GetOptionValue(const Option: String): Word;
    var Value: Word;
        Code: Word;
    begin
        Val(Copy(Option, 3, Length(Option)), Value, Code);
        if Code <> 0 then InvalidOption(Option);
        GetOptionValue := Value;
    end;

    function IsRemoteDrive(Disk: Byte): Boolean; assembler;
    asm
        mov ax, $4409;
        mov bl, [Disk]
        inc bl
        int $21
        jc @err
        and dh, $90
        jnz @remote
        xor al, al
        jmp @coda
    @err:
        and ax, $9200
        jz @coda
    @remote:
        mov al, 1
    @coda:
    end;

    function IsFixedDisk(Disk: Byte): Boolean; assembler;
    asm
        mov ax, $4408
        mov bl, [Disk]
        inc bl
        int $21
    end;

    function GetCursorShape: Word; assembler;
    asm
        mov ah, 3
        xor bh, bh
        int $10
        mov ax, cx
    end;

    procedure SetCursorShape(Shape: Word); assembler;
    asm
        mov ah, $0F { get current video mode into AL }
        int $10
        mov ah, $01 { set text-mode cursor shape }
        mov cx, [Shape]
        int $10
    end;

    procedure HideCursor;
    begin
        SetCursorShape($2000);
    end;

    procedure OnExit; far;
    begin
        SetCursorShape(CursorShape);
    end;

begin
    if Swap(DosVersion) < $314 then
        Die('This program requires DOS 3.20 or higher');

    ExitProc := @OnExit;
    CursorShape := GetCursorShape;
    HideCursor;

    if ParamCount < 2 then Help;

    { Find drive letter argument. }
    for i := 1 to ParamCount do
    begin
        DriveLetter := GetDriveLetter(ParamStr(i));
        if DriveLetter <> #0 then break;
    end;

    if DriveLetter = #0 then Die('Drive letter is not specified')
    else if i = 1 then
    begin
        DiskToImage := True;
        NumOfFiles := 1;
        FirstFileArg := 2;
    end
    else
    begin
        DiskToImage := False;
        NumOfFiles := i - 1;
        FirstFileArg := 1;
    end;

    { Check image path. }
    for i := 1 to NumOfFiles do
    begin
        Filename := FExpand(GetFileArg(i));
        if DriveLetter = Filename[1] then
            if DiskToImage then
                Die('Can''t write image to the source disk')
            else
                Die('Can''t read image from the destination disk');
    end;

    { Default values. }
    RWSectorsFunc := RWSectorsBios;
    FindOutGeometry := False;
    NumOfTracks := 80;
    NumOfSectors := 18;
    NumOfHeads := 2;
    BytesPerSector := 512;

    { Command line parameters. }
    for i := NumOfFiles + 2 to ParamCount do
    begin
        Arg := ParamStr(i);

        if (Length(Arg) < 2)
           or not(Arg[1] in ['/', '-']) then InvalidOption(Arg);

        case Upcase(Arg[2]) of
            'T':
            begin
                NumOfTracks := GetOptionValue(Arg);
                if NumOfTracks > MaxNumOfTracks then
                begin
                    WriteLn('Tracks must be less than ', MaxNumOfTracks + 1);
                    Halt(1);
                end;
            end;
            'S':
            begin
                NumOfSectors := GetOptionValue(Arg);
                if NumOfSectors > MaxNumOfSectors then
                begin
                    WriteLn('Sectors must be less than ',
                            MaxNumOfSectors + 1);
                    Halt(1);
                end;
            end;
            'B':
            begin
                BytesPerSector := GetOptionValue(Arg);
                if (BytesPerSector > MaxBytesPerSector)
                   or (BytesPerSector < 128)
                   or (BytesPerSector mod 128 <> 0) then
                begin
                    WriteLn('Bytes per sector must be multiple of 128' +
                            ' and less than ',
                            MaxBytesPerSector + 1);
                    Halt(1);
                end;
            end;
            'H':
            begin
                NumOfHeads := GetOptionValue(Arg);
                if not(NumOfHeads in [1, 2]) then Die('Heads must be 1 or 2');
            end;
            'R': FindOutGeometry := True;
            'N': RWSectorsFunc := RWSectorsDos;
            'F':
            case GetOptionValue(Arg) of
                {defaults: NumOfTracks := 80; NumOfSectors := 18; NumOfHeads := 2}
                360: begin NumOfTracks := 40; NumOfSectors := 9; end;
                720: begin                    NumOfSectors := 9; end;
                800: begin                    NumOfSectors := 10; end;
                820: begin NumOfTracks := 82; NumOfSectors := 10; end;
                120: begin                    NumOfSectors := 15; end;
                144: ;
                160: begin                    NumOfSectors := 20; end;
                164: begin NumOfTracks := 82; NumOfSectors := 20; end;
                168: begin                    NumOfSectors := 21; end;
                172: begin NumOfTracks := 82; NumOfSectors := 21; end;
                180: begin NumOfTracks := 82; NumOfSectors := 22; end;
                188: begin NumOfTracks := 82; NumOfSectors := 23; end;
                else InvalidOption(Arg);
            end;
            else InvalidOption(Arg);
        end;
    end;

    if not(DiskToImage) and not(DriveLetter in ['A', 'B']) then
    begin
        WriteLn('Supposed floppy drives under DOS are A: or B:');
        WriteLn('If the destination disk is a hard disk this can destroy ALL data');
        ConfirmIfProceed;
    end;

    Disk := GetDriveNumber(DriveLetter);

    if IsFixedDisk(Disk) then
    begin
        WriteLn('Warning! ', DriveLetter,
                ': does not seem to be a floppy drive.');
        if not(DiskToImage) then
            WriteLn('This can destroy all data on this drive!');
        ConfirmIfProceed;
    end;

    if IsRemoteDrive(Disk) then
    begin
        WriteLn('Warning! ', DriveLetter,
                ': seems to be a network/SUBST drive.');
        ConfirmIfProceed;
    end;

    if DiskToImage then
        DoDiskToImage(GetFileArg(1), FindOutGeometry)
    else
        DoImageToDisk(NumOfFiles, FindOutGeometry);
end;

begin Main end.
