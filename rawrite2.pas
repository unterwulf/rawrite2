{$m 4096,32768,65536}
program rawrite2;

 uses dos;

 type
  TSector=array[0..1024]of byte;

 const
   ntracksmax=84;
   nheadsmax=2;
   nsectorsmax=128;
   bytessectormax=4096;

 var
  diskerror:integer;

  {buffers}
  sector0,sector1:tsector;
  psector:^tsector;
  pistai:pointer;

  {parameters}

  drive:string;
  path:array[1..10] of string;
  imagef:file;
  disco,nheads,nsectors,ntracks:byte;
  totalsectores,bytessector,bytespista:word;
  recon,disktoimage,nobios:boolean;

  {temp vars}
  i,j,k,l,m:integer;
  s:longint;
  nfiles:integer;
  q:char;
  pstr:string;

 procedure diskreset(disk:byte);
 var
  r:registers;
 begin
  r.ah:=0;
  r.dl:=disk;
  intr($13,r);
  DiskError:=r.ah;
 end;

 {needed to put this in assembler or the program will hang}
 function dosrwsectors(disk:byte;sector:word;nsectors:word;rw:char;pbuffer:pointer):word;assembler;
 asm
  mov ah,[rw]
  or ah,$20
  mov al,[disk]
  mov dx,[sector]
  mov cx,[nsectors]
  push ds
  lds bx,[pbuffer]
  push bp
  cmp ah,'W'
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
  xor ax,ax
 @err:
  mov al,ah
  xor ah,ah
  mov [diskerror],ax
  mov ax,[nsectors]
 end;

 function rwsectors(disk,head,track,sector,nsect:byte;rw:char;pbuffer:pointer):word;
 var
  r:registers;
  i:integer;
 begin
  if nobios then {access only trought DOS int25&26}
   rwsectors:=dosrwsectors(disk,head*nsectors+track*nsectors*nheads+sector-1,nsectors,rw,pbuffer)
  else
  begin
   i:=3;
   if (rw='w')or(rw='W') then rw:=chr(3) else rw:=chr(2);
   repeat
    with r do
    begin
     dl:=disk;
     dh:=head;
     ch:=track;
     cl:=sector;
     al:=nsect;
     es:=seg(pbuffer^);
     bx:=ofs(pbuffer^);
     ah:=ord(rw);
    end;
    intr($13,r);
    dec(i);
    if r.ah<>0 then diskreset(disk);
   until (r.ah=0)or(i=0);
   rwsectors:=r.al;
   diskerror:=r.ah;
  end;
 end;

 procedure readtrack(disk,head,track,nsectors:byte;ptrack:pointer);
 begin
  rwsectors(disk,head,track,1,nsectors,'r',ptrack);
 end;

 procedure writetrack(disk,head,track,nsectors:byte;ptrack:pointer);
 begin
  rwsectors(disk,head,track,1,nsectors,'w',ptrack);
 end;

 function upstring(s:string):string;
  var
   i:byte;
  begin
   for i:=1 to length(s) do s[i]:=upcase(s[i]);
   upstring:=s;
  end;

 {------------------------------------------------------------------------}
 function NetDrive(disco:byte):boolean;
 var
  r:registers;
 begin
  r.ax:=$4409;
  r.bl:=disco+1;
  intr($21,r);
  if (FCarry and r.flags)<>0 then
   NetDrive:=((r.ax and $9200)<>0)
  else
   NetDrive:=false;
 end;

 function FixedDisk(disco:byte):boolean;
 var
  r:registers;
 begin
  r.ax:=$4408;
  r.bl:=disco+1;
  intr($21,r);
  FixedDisk:=(r.ax and 1)<>0
 end;

 procedure cambia(i:integer);
  begin

   case i of
    1:writeln(chr(7),'Insert SOURCE floppy and press [ENTER]');
    2:writeln(chr(7),'Insert DESTINATION floppy and press [ENTER]');
   end;
   readln;

   diskreset(disco);
   rwsectors(disco,0,0,1,1,'r',@sector0);
   while diskerror<>0 do
   begin
    write(chr(7),'Error reading Sector 0:  (R)etry (C)ancel (I)gnore ');
    readln(q);
    case upcase(q) of
     'C':halt;
     'R':rwsectors(disco,0,0,1,1,'r',@sector0);
     'I':exit;
    end;
   end;

  end;

 procedure escribe(i,j:integer);
  begin
   write('Writing ',i:3,',',j:1,chr(13));
   writetrack(disco,j,i,nsectors,pistai);
   while diskerror<>0 do
    begin
     write(chr(7),'Writing Error: (R)etry or (C)ancel?');
     readln(q);
     case upcase(q) of
     'C':halt;
     'R':
      begin
       rwsectors(disco,0,0,1,1,'r',@sector0);
       writetrack(disco,j,i,nsectors,pistai);
      end;
     end{case};
    end{while};
  end;

  PROCEDURE DosError;
  BEGIN
    WriteLn('This program requires DOS 3.20 or higher.');
    Halt(1);
  END;

 procedure help;
  begin
   writeln('RAWRITE2 ver 1.1   8-5-2000');
   writeln('A floppy disk image utility');
   writeln('This program is (C) Miguel Angel Alvarez Cruz and is distributed under GPL 2.0');
   writeln('Use only at your own risk!');
   writeln('Usage:');
   writeln('To make a disk image:		RAWRITE2 U: file [options]');
   writeln('To copy a image to disk:	RAWRITE2 file [file file ...] U: [options]');
   writeln(' where		"U:" is    A: o B:');
   writeln('		"file" is the image file name');
   writeln('		"options" is one of');
   writeln('			/Fxxx	for xxx size floppy (default 1.44 MB)');
   writeln('			/R	read size from sector 0 (only DOS floppies)');
   writeln('			/N      do not use BIOS, use DOS instead');
   writeln('		or a combination of');
   writeln('			/Txx	xx=number of tracks (max 84, default=80)');
   writeln('			/Sxx	xx=number of sectors (default=18)');
   writeln('			/Bxxx	xxx=sector size in bytes (default=512)');
   writeln('			/Hx	x=number of heads (1 or 2, default=2)');
   writeln('The destination disk must be previously formatted');
   writeln('Note: for non standard formats you will need the 2m or fdread utilities');
   writeln('      Unfortunately Windows NT only supports standard floppy formats');
   writeln('Note2: for any format but 1.44 Mb you should supply the format option');
   writeln('       both reading or writing an image file, since the images are raw');
   writeln('Press enter to read next page ...');
   readln;
   writeln('Floppy sizes option');
   writeln('       /F360          360 KB (old 5" floppy)');
   writeln('       /F720          720 KB ');
   writeln('       /F800          800 KB (fdformat)');
   writeln('       /F820          820 KB (fdformat)');
   writeln('       /F120          1.2 MB (5" floppy)');
   writeln('       /F144          1.44 MB (default)');
   writeln('       /F148          1.48 MB (fdformat)');
   writeln('       /F160          1.6 MB (fdformat)');
   writeln('       /F164          1.64 MB (fdformat)');
   writeln('       /F168          1.68 MB (fdformat)');
   writeln('       /F172          1.72 MB (fdformat)');
   writeln('       /F180          1.8 MB (2m)');
   writeln('       /F188          1.88 MB (2m /m)');
   writeln;
   writeln('Some examples:');
   writeln('Make a 1.44 MB floppy image:                rawrite2 a: image.144');
   writeln('Make a 1.72 MB floppy image:                rawrite2 a: image.172 /f172');
   writeln('Dump the last image to floppy:              rawrite2 image.172 a: /f172');
   writeln('Make an image of a DOS unknown floppy:      rawrite2 a: image.img /r');
   writeln('Dump several files to floppy:               rawrite2 file1 file2 a:');
   halt(1);
  end;

 {main}
 begin

  IF Swap(DosVersion)<$314 THEN DosError;
  if paramcount<2 then help;
  disktoimage:=true;
  nfiles:=1;
  drive:=upstring(paramstr(1));
  path[1]:=paramstr(2);
  if (drive[2]<>':')or(length(drive)<>2) then
  begin
   disktoimage:=false;
   while ((drive[2]<>':')or(length(drive)<>2)) and (nfiles<paramcount) do
   begin
    path[nfiles]:=paramstr(nfiles);
    drive:=upstring(paramstr(nfiles+1));
    inc(nfiles);
    if nfiles>10 then
    begin
     writeln('The maximun number of image files is 10');
     halt(1);
    end;
   end;
   dec(nfiles);
  end;
  if (drive[2]<>':')or(length(drive)<>2)or(drive[1]<'A')or(drive[1]>'Z')or(path[1]='/') then help;
  if (drive<>'A:')and(drive<>'B:') then
  begin
   writeln('Supposed floppy drives under DOS are A: or B:');
   writeln('If the destination disk is a hard disk this can destroy ALL data');
   writeln('Proceed anyway? (y/n)');
   readln(q);
   if not(upstring(q)='Y') then halt;
  end;
  disco:=ord(drive[1])-ord('A');

  if FixedDisk(disco) then
  begin
   writeln('Warning!, this disk does not seems a floppy drive.');
   if not(disktoimage) then writeln('This can destroy all data on this drive!');
   writeln('Proceed anyway? (y/n)');
   readln(q);
   if not(upstring(q)='Y') then halt;
  end;

  if NetDrive(disco) then
  begin
   writeln('Warning!, this disk seems a network/SUBST drive. Proceed anyway? (y/n)');
   readln(q);
   if not(upstring(q)='Y') then halt;
  end;

  {default values}
  ntracks:=80;
  nsectors:=18;
  nheads:=2;
  bytessector:=512;

  {command line parameters}
  if paramcount>nfiles+1 then
   for i:=nfiles+2 to paramcount do
   begin
     pstr:=upstring(paramstr(i));
     if (pstr[1]<>'/')and(pstr[1]<>'-') then help;
     j:=0;
     case pstr[2] of
      'T':
	  begin
	   val(copy(pstr,3,length(pstr)),ntracks,j);
	   if ntracks>ntracksmax then
	   begin
	    writeln('Tracks must be less than ',ntracksmax+1);
	    halt(1);
	   end;
	  end;
      'S':
	  begin
	   val(copy(pstr,3,length(pstr)),nsectors,j);
	   if nsectors>nsectorsmax then
	   begin
	    writeln('Sectors must be less than ',nsectorsmax+1);
	    halt(1);
	   end;
	  end;
      'B':
	  begin
	   val(copy(pstr,3,length(pstr)),bytessector,j);
	   if (bytessector>bytessectormax)or(bytessector<128)or(bytessector mod 128<>0) then
	   begin
	    writeln('Bytes sector must be multiple of 128 and less than ',bytessectormax+1);
	    halt(1);
	   end;
	  end;
      'H':
	  begin
	   val(copy(pstr,3,length(pstr)),nheads,j);
	   if (nheads<>1)and(nheads<>2) then
	   begin
	    writeln('Heads must be 1 or 2');
	    halt(1);
	   end;
	  end;
      'R': recon:=true;
      'N': nobios:=true;
      'F':
	  begin
	   val(copy(pstr,3,length(pstr)),k,j);
	   case k of
           {defaults: ntracks=80, nsectors=18, nheads=2}
           360: begin ntracks:=40;nsectors:=9;end;
	   720: nsectors:=9;
           800: nsectors:=10;
           820: begin ntracks:=82;nsectors:=10;end;
           120: nsectors:=15;
	   144: ;
           160: nsectors:=20;
           164: begin ntracks:=82;nsectors:=20;end;
           168: nsectors:=21;
	   172: begin ntracks:=82;nsectors:=21;end;
	   180: begin ntracks:=82;nsectors:=22;end;
           188: begin ntracks:=82;nsectors:=23;end;
	   else
	    help;
	   end;
	  end;
     end;
     if j<>0 then
     begin
      writeln('Error reading parameter:',pstr);
      halt(1);
     end;
   end{for};

  totalsectores:=longint(ntracks)*nsectors*nheads;

  {check image path}
  for i:=1 to nfiles do
  begin
   path[i]:=Fexpand(path[i]);
   if drive=copy(path[i],1,2) then
   begin
    writeln('Error: can`t read/write image on the source disk');
    halt(1);
   end;
  end;

  {read parameters from sector 0 if /R supplied}
  if recon then
  begin
   if disktoimage then
    cambia(1)
   else
   begin
    cambia(2);
    sector1:=sector0;
    assign(imagef,path[1]);
    {$i-}reset(imagef,1);{$i+}
    {$i-}blockread(imagef,sector0,512);{$i+}
    if ioresult<>0 then
    begin
     writeln('Error reading image file');
     halt(1);
    end;
    close(imagef);
    if (sector0[$0b]<>sector1[$0b])or(sector0[$0c]<>sector1[$0c])or
       (sector0[$13]<>sector1[$13])or(sector0[$14]<>sector1[$14])or
       (sector0[$18]<>sector1[$18])or(sector0[$19]<>sector1[$19])or
       (sector0[$1a]<>sector1[$1a])then
       writeln('Floppy sector 0 parameters and image sector 0 parameters differ');
   end;
   bytessector:=sector0[$0b]+sector0[$0c]*256;
   totalsectores:=sector0[$13]+sector0[$14]*256;
   nsectors:=sector0[$18]+sector0[$19]*256;
   nheads:=sector0[$1a];
   {media:=sector0[$15];}
   if (totalsectores=0)or(bytessector=0)or(bytessector mod 128<>0)
     or(nheads<1)or(nheads>2)or(nsectors=0) then
   begin
    writeln('Sector 0 has wrong format');
    halt(1);
   end;
   ntracks:=totalsectores div (nsectors*nheads);
  end;

  bytespista:=bytessector*nsectors;

  {check memory}
  if (maxavail<bytespista) then
   begin
    writeln('Not enough memory');
    halt(1);
   end;

  {check image file}
  s:=0;
  for i:=1 to nfiles do
  begin
   assign(imagef,path[i]);
   if not(disktoimage) then
   begin
    {$i-}reset(imagef,1);{$i+}
    if ioresult<>0 then
    begin
     writeln('Error reading image file');
     halt(1);
    end;
    s:=s+filesize(imagef);
    if s>longint(bytessector)*totalsectores*nheads then
    begin
     writeln('Warning: This image file does not fit in the floppy:',path[i]);
    end;
   end
   else
   begin
    if (diskfree(ord(path[i][1])-ord('A')+1)<longint(bytessector)*(totalsectores+1)) then
    begin
     writeln('Not enough disk space for image file');
     halt(1);
    end;
    {$i-}reset(imagef,1);close(imagef);{$i+}
    if ioResult=0 then
    begin
     writeln('Destination file exists. Overwrite? (y/n)');
     read(q);
     if not(upstring(q)='Y') then halt;
    end;
    {$i-}rewrite(imagef,1);{$i+}
    if ioresult<>0 then
    begin
     writeln('Error writing to disk, write protected?');
     halt(1);
    end;
   end{if};
   close(imagef);
  end;

  writeln('Disk ',drive,'  Heads:',nheads,'    Tracks:',ntracks,
	   '    Sectors:',nsectors,'    Bytes sector:',bytessector);

  getmem(pistai,bytespista);{pistai es el buffer entrada-salida}

  assign(imagef,path[1]);
  if disktoimage then
  begin
   rewrite(imagef,1);
   for i:=0 to ntracks-1 do
   begin
     for j:=0 to nheads-1 do
     begin
      write('Reading ',i:3,',',j:1,'       ',chr(13));
      readtrack(disco,j,i,nsectors,pistai);
      if diskerror<>0 then {Error reading track? Retry reading sectors}
      begin
       psector:=pistai;
       for k:=1 to nsectors do
       begin
	 rwsectors(disco,j,i,k,1,'r',psector);
	 while diskerror<>0 do
	 begin
	   writeln(chr(7),'Error reading sector:',k,' track:',i,' head:',j);
	   write('   (R)etry (I)gnore (C)ancel?');
	   readln(q);
	   case upcase(q) of
	   'R':rwsectors(disco,j,i,k,1,'r',psector);
	   'C':halt;
	   'I':diskerror:=0;
	   end{case};
	 end{while};
         psector:=addr(psector^[bytessector]);
       end{for};
     end{if};
     blockwrite(imagef,pistai^,bytespista);
    end{for j};
   end{for i};
  end
  else
  begin
   l:=1;
   reset(imagef,1);
   for i:=0 to ntracks-1 do
   begin
     for j:=0 to nheads-1 do
     begin
      blockread(imagef,pistai^,bytespista,k);
      while (k<>bytespista)and(l<nfiles) do
      begin
       close(imagef);
       inc(l);
       assign(imagef,path[l]);
       reset(imagef,1);
       blockread(imagef,(pchar(pistai)+k)^,bytespista-k,m);
       k:=k+m;
      end;
      escribe(i,j);
      if k<>bytespista then break;
     end{for j};
     if k<>bytespista then break;
   end{for i};
  end;
  close(imagef);

 end.