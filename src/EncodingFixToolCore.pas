unit EncodingFixToolCore;

{
  Delphi *.pas encoding fix tool
  --------------------------------
  - Gathers files by path / recursive / extensions
  - Parallel processing (TParallel.For)
  - For each file:
      * Try strict UTF-8 decode; if OK => (verbose: "OK"), skip save
      * Else fix per line (mixed encodings):
          - Try UTF-8 (strict), CP1250 (Polish/Central Europe), CP1252 (Western/German)
          - Score decodes and pick the best for each line
      * Save as UTF-8 with configurable BOM (default: y)
      * Optionally create a backup in bkp-dir preserving relative paths
  - Modes:
      dry, silent, verbose, help
  - Summary: time elapsed, files changed

  Notes:
  * Default UTF-8 BOM = yes (Delphi IDE/compiler behave best with BOM).
}

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  System.Types,
  System.IOUtils,
  System.SyncObjs,
  System.Diagnostics,
  System.StrUtils,
  System.Character,
  System.Threading;

type
  TEncodingFixTool = class
  public type
    TOptions = record
      DryRun: Boolean;
      Silent: Boolean;
      Verbose: Boolean; // not compatible with Silent
      Path: string;
      Recursive: Boolean; // default y
      Exts: TArray<string>; // normalized: ".pas", ".dpr", etc.
      Utf8Bom: Boolean; // default y
      BackupDir: string; // if <> '', create backups preserving relative path
    end;
  private
    type
      TSafeConsole = class
      strict private
        class var FCrit: TCriticalSection;
      public
        class constructor Create;
        class destructor Destroy;
        class procedure WriteLine(const aMsg: string);
      end;

  private
    class function ParseCommandLine(out aOptions: TOptions): Integer; static;
    class function ShowHelp: Integer; static;
    class function NormalizeExtList(const aCSV: string): TArray<string>; static;
    class function CollectFiles(const aOptions: TOptions): TArray<string>; static;
    class function HasWantedExt(const aFile: string; const aExts: TArray<string>): Boolean; static;

    // Encoding helpers
    class function IsUtf8Strict(const aBytes: TBytes): Boolean; static;
    class function SplitLinesByBytes(const aBytes: TBytes): TArray<TBytes>; static;
    class function DecodeBestPerLine(const aLineBytes: TBytes): string; static;
    class function ScoreDecoded(const aText: string): Integer; static;
    class function ContainsSpecials(const aText: string): Integer; static;

    class function FixFile(const aFile: string; const aOptions: TOptions; out aChanged: Boolean; out aReason: string): Boolean; static;
    class function SaveTextUTF8(const aFile: string; const aText: string; aWithBOM: Boolean): Boolean; static;
    class function MakeBackupPath(const aOptions: TOptions; const aRootPath, aFile: string): string; static;

  public
    class function RunFromCommandLine: Integer; static;
  end;

implementation

{ =====================  Utilities  ===================== }

class constructor TEncodingFixTool.TSafeConsole.Create;
begin
  FCrit := TCriticalSection.Create;
end;

class destructor TEncodingFixTool.TSafeConsole.Destroy;
begin
  FCrit.Free;
end;

class procedure TEncodingFixTool.TSafeConsole.WriteLine(const aMsg: string);
begin
  FCrit.Acquire;
  try
    Writeln(aMsg);
  finally
    FCrit.Release;
  end;
end;

class function TEncodingFixTool.NormalizeExtList(const aCSV: string): TArray<string>;
var
  lParts: TArray<string>;
  i: Integer;
  s: string;
begin
  lParts := aCSV.Split([',', ';', ' '], TStringSplitOptions.ExcludeEmpty);
  SetLength(Result, Length(lParts));
  for i := 0 to High(lParts) do
  begin
    s := Trim(LowerCase(lParts[i]));
    if s.StartsWith('*.') then
    begin
      s := s.Substring(1); // "*.pas" -> ".pas"
    end else
    if (s <> '') and (s[1] <> '.') then
    begin
      s := '.' + s; // "pas" -> ".pas"
    end;
    Result[i] := s;
  end;
end;

class function TEncodingFixTool.HasWantedExt(const aFile: string; const aExts: TArray<string>): Boolean;
var
  lExt: string;
  lWanted: string;
begin
  lExt := LowerCase(ExtractFileExt(aFile));
  for lWanted in aExts do
  begin
    if lExt = lWanted then
    begin
      Exit(True);
    end;
  end;
  Result := False;
end;

class function TEncodingFixTool.CollectFiles(const aOptions: TOptions): TArray<string>;
var
  lFiles: TList<string>;
  lSearchOpt: TSearchOption;
  lAll: TArray<string>;
  f: string;
begin
  lFiles := TList<string>.Create;
  try
    if aOptions.Recursive then
    begin
      lSearchOpt := TSearchOption.soAllDirectories;
    end else
    begin
      lSearchOpt := TSearchOption.soTopDirectoryOnly;
    end;

    lAll := TDirectory.GetFiles(aOptions.Path, '*.*', lSearchOpt);
    for f in lAll do
    begin
      if HasWantedExt(f, aOptions.Exts) then
      begin
        lFiles.Add(f);
      end;
    end;

    Result := lFiles.ToArray;
  finally
    lFiles.Free;
  end;
end;

class function TEncodingFixTool.IsUtf8Strict(const aBytes: TBytes): Boolean;
var
  lUTF8: TEncoding;
  lS: string;
  lBom, lBytes: TBytes;
begin
  lUTF8 := TEncoding.Utf8;
  lBytes:= aBytes;

  lBom:= lUtf8.GetPreamble
  if (length(lBytes)>=length9lBom)) then
    if CompareMem(lBytes[0], lBom[0], length(lBom)) then
      lBytes:= Copy(lBytes, Length(lBom), Length(lBytes)-Length(lBom));

    try
      lS := lUTF8.GetString(aBytes);
      Result := True;
    except
      on E: EEncodingError do
      begin
        Result := False;
      end;
    end;
end;

class function TEncodingFixTool.SplitLinesByBytes(const aBytes: TBytes): TArray<TBytes>;
var
  i, lStart: Integer;
  b: Byte;
  lLine: TBytes;

  procedure PushLine(aEndExclusive: Integer; aConsumeCR: Boolean);
  var
    lLen: Integer;
  begin
    lLen := aEndExclusive - lStart;
    if lLen < 0 then
    begin
      lLen := 0;
    end;
    SetLength(lLine, lLen);
    if lLen > 0 then
    begin
      Move(aBytes[lStart], lLine[0], lLen);
    end;
    Result := Result + [lLine];
    if aConsumeCR then
    begin
      // if CRLF, skip the CR at the end of line
    end;
  end;
begin
  SetLength(Result, 0);
  lStart := 0;
  i := 0;
  while i < Length(aBytes) do
  begin
    b := aBytes[i];
    if (b = $0A) then
    begin
      // LF: line ends before this char, also check if previous byte was CR
      PushLine(i, False);
      Inc(i);
      lStart := i;
    end else
    if (b = $0D) then
    begin
      // CR: line ends here; handle CRLF or standalone CR
      PushLine(i, True);
      Inc(i);
      if (i < Length(aBytes)) and (aBytes[i] = $0A) then
      begin
        Inc(i); // consume LF
      end;
      lStart := i;
    end else
    begin
      Inc(i);
    end;
  end;

  // last line (no trailing EOL)
  if lStart <= Length(aBytes) - 1 then
  begin
    PushLine(Length(aBytes), False);
  end;
end;

class function TEncodingFixTool.ContainsSpecials(const aText: string): Integer;
const
  // Polish + German diacritics; count good matches positively.
  POL = 'ąćęłńóśźżĄĆĘŁŃÓŚŹŻ';
  GER = 'äöüÄÖÜßẞ';
var
  lScore: Integer;
  ch: Char;
begin
  lScore := 0;
  for ch in aText do
  begin
    if (Pos(ch, POL) > 0) or (Pos(ch, GER) > 0) then
    begin
      Inc(lScore);
    end;
  end;
  Result := lScore;
end;

class function TEncodingFixTool.ScoreDecoded(const aText: string): Integer;
var
  lScore: Integer;
  ch: Char;
begin
  // Base heuristic:
  // +2 for known PL/DE diacritics,
  // +1 for typical source-text ASCII range,
  // -2 for control chars (excluding tab),
  // -1 for � replacement if it appears (just in case)
  lScore := 0;

  Inc(lScore, 2 * ContainsSpecials(aText));

  for ch in aText do
  begin
    if ch = #$FFFD then
    begin
      Dec(lScore, 1);
    end else
    if (ch = #9) or (ch = #10) or (ch = #13) then
    begin
      // ignore tabs/newlines here
    end else
    if ch.IsControl then
    begin
      Dec(lScore, 2);
    end else
    if ch.IsLetterOrDigit or ch.IsWhiteSpace or CharInSet(ch, ['.', ',', ';', ':', '-', '_', '(', ')', '[', ']', '{', '}', '''', '"', '/', '\', '+', '*', '=', '<', '>', '!', '?', '@', '#', '$', '%', '^', '&', '|']) then
    begin
      Inc(lScore, 1);
    end;
  end;

  Result := lScore;
end;

class function TEncodingFixTool.DecodeBestPerLine(const aLineBytes: TBytes): string;
var
  lUTF8, lCP1250, lCP1252, lANSI: TEncoding;
  sUTF8, s1250, s1252, sANSI: string;
  bestS: string;
  bestScore, sc: Integer;
begin
  // 1) Try strict UTF-8 first
  lUTF8 := UTF8Strict(False);
  try
    try
      sUTF8 := lUTF8.GetString(aLineBytes);
      // strict success => take it
      Exit(sUTF8);
    except
      on E: EEncodingError do
      begin
        // fall through
      end;
    end;
  finally
    lUTF8.Free;
  end;

  // 2) Try single-byte candidates; they never fail, so score them.
  lCP1250 := TEncoding.GetEncoding(1250); // Polish/Central Europe
  lCP1252 := TEncoding.GetEncoding(1252); // Western/German
  lANSI   := TEncoding.ANSI;

  s1250 := lCP1250.GetString(aLineBytes);
  s1252 := lCP1252.GetString(aLineBytes);
  sANSI  := lANSI.GetString(aLineBytes);

  bestS := s1250;
  bestScore := ScoreDecoded(s1250);

  sc := ScoreDecoded(s1252);
  if sc > bestScore then
  begin
    bestScore := sc;
    bestS := s1252;
  end;

  sc := ScoreDecoded(sANSI);
  if sc > bestScore then
  begin
    bestScore := sc;
    bestS := sANSI;
  end;

  Result := bestS;
end;

class function TEncodingFixTool.MakeBackupPath(const aOptions: TOptions; const aRootPath, aFile: string): string;
var
  lRel: string;
begin
  // Normalize root and compute relative path
  // Ensure trailing delimiter on root
  lRel := aFile;
  if aRootPath <> '' then
  begin
    // Make relative to root path if possible
    // We compare case-insensitively on Windows
    if SameText(Copy(aFile, 1, Length(IncludeTrailingPathDelimiter(aRootPath))), IncludeTrailingPathDelimiter(aRootPath)) then
    begin
      lRel := Copy(aFile, Length(IncludeTrailingPathDelimiter(aRootPath)) + 1, MaxInt);
    end;
  end;

  Result := TPath.Combine(IncludeTrailingPathDelimiter(aOptions.BackupDir), lRel);
end;

class function TEncodingFixTool.SaveTextUTF8(const aFile: string; const aText: string; aWithBOM: Boolean): Boolean;
var
  lSL: TStringList;
  lEnc: TEncoding;
begin
  Result := False;
  lSL := TStringList.Create;
  try
    lEnc := TUTF8Encoding.Create(aWithBOM);
    try
      lSL.Text := aText;
      lSL.SaveToFile(aFile, lEnc);
      Result := True;
    finally
      lEnc.Free;
    end;
  finally
    lSL.Free;
  end;
end;

class function TEncodingFixTool.FixFile(const aFile: string; const aOptions: TOptions; out aChanged: Boolean; out aReason: string): Boolean;
var
  lBytes: TBytes;
  lLinesBytes: TArray<TBytes>;
  lLine: TBytes;
  lFixedLines: TStringBuilder;
  lFirst: Boolean;
  lRoot: string;
  lBackupPath: string;
begin
  Result := False;
  aChanged := False;
  aReason := '';

  lBytes := TFile.ReadAllBytes(aFile);

  // Quick path: strict UTF-8 decodes whole content fine?
  if IsUtf8Strict(lBytes) then
  begin
    // no change needed
    Result := True;
    aChanged := False;
    aReason := 'UTF-8 OK';
    Exit;
  end;

  // Mixed encodings possible: split by raw CR/LF bytes, decode per line.
  lLinesBytes := SplitLinesByBytes(lBytes);

  lFixedLines := TStringBuilder.Create(Length(lBytes) + 1024);
  try
    lFirst := True;
    for lLine in lLinesBytes do
    begin
      if not lFirst then
      begin
        lFixedLines.AppendLine;
      end else
      begin
        lFirst := False;
      end;
      lFixedLines.Append(DecodeBestPerLine(lLine));
    end;

    // If dry-run, do not write anything—just indicate change.
    if aOptions.DryRun then
    begin
      aChanged := True;
      aReason := 'Would fix (dry-run)';
      Result := True;
      Exit;
    end;

    // Backup if requested
    if aOptions.BackupDir <> '' then
    begin
      lRoot := IncludeTrailingPathDelimiter(ExpandFileName(aOptions.Path));
      lBackupPath := MakeBackupPath(aOptions, lRoot, aFile);
      TDirectory.CreateDirectory(ExtractFileDir(lBackupPath));
      TFile.Copy(aFile, lBackupPath, True);
    end;

    // Save fixed version
    if SaveTextUTF8(aFile, lFixedLines.ToString, aOptions.Utf8Bom) then
    begin
      aChanged := True;
      aReason := 'Fixed & saved';
      Result := True;
    end else
    begin
      aChanged := False;
      aReason := 'Save failed';
      Result := False;
    end;
  finally
    lFixedLines.Free;
  end;
end;

class function TEncodingFixTool.ShowHelp: Integer;
const
  HELP_TEXT: PChar =
  'Delphi *.pas Encoding Fix Tool' + sLineBreak +
  sLineBreak +
  'Usage:' + sLineBreak +
  '  EncodingFixTool [params]' + sLineBreak +
  sLineBreak +
  'Params:' + sLineBreak +
  '  help                  : Show this help text.' + sLineBreak +
  '  dry                   : Dry run (no files are changed).' + sLineBreak +
  '  s | silent            : No console output.' + sLineBreak +
  '  v | verbose           : More output (not compatible with silent).' + sLineBreak +
  '  path=<dir>            : Directory to scan. Default: current working dir.' + sLineBreak +
  '  recursive=y|n         : Recurse into subfolders. Default: y.' + sLineBreak +
  '  ext=<csv>             : Extensions list. Default: pas,dpr. Accepts "pas", ".pas", "*.pas".' + sLineBreak +
  '  utf8-bom=y|n          : Save with UTF-8 BOM (default: y).' + sLineBreak +
  '  bkp-dir=<dir>         : If set, save a backup copy before overwriting.' + sLineBreak +
  sLineBreak +
  'How it works:' + sLineBreak +
  '- Files are processed in parallel (TParallel.For).' + sLineBreak +
  '- Each file is first tested for strict UTF-8.' + sLineBreak +
  '- If that fails, lines are split by raw CR/LF and decoded per line using heuristics' + sLineBreak +
  '  between UTF-8, Windows-1250 (PL/CE), and Windows-1252 (DE/Western).' + sLineBreak +
  '- Fixed text is saved as UTF-8 (with/without BOM per option).' + sLineBreak +
  sLineBreak +
  'Examples:' + sLineBreak +
  '  EncodingFixTool dry path=c:\src ext=pas,dpr recursive=n' + sLineBreak +
  '  EncodingFixTool path=./src ext="*.pas,*.dpr" utf8-bom=n v' + sLineBreak +
  '  EncodingFixTool path=c:\tmp bkp-dir=c:\bkp' + sLineBreak +
  sLineBreak;
begin
  Writeln(HELP_TEXT);
  Result := 0;
end;

class function TEncodingFixTool.ParseCommandLine(out aOptions: TOptions): Integer;
var
  i: Integer;
  p, key, val: string;
  eqPos: Integer;

  function AsYN(const s: string; const aDefault: Boolean): Boolean;
  var
    l: string;
  begin
    l := LowerCase(s.Trim);
    if (l = '') then
    begin
      Exit(aDefault);
    end else
    if (l = 'y') or (l = 'yes') or (l = '1') or (l = 'true') then
    begin
      Exit(True);
    end else
    if (l = 'n') or (l = 'no') or (l = '0') or (l = 'false') then
    begin
      Exit(False);
    end else
    begin
      Exit(aDefault);
    end;
  end;

begin
  // Defaults
  aOptions.DryRun   := False;
  aOptions.Silent   := False;
  aOptions.Verbose  := False;
  aOptions.Path     := GetCurrentDir;
  aOptions.Recursive:= True;
  aOptions.Exts     := NormalizeExtList('pas,dpr');
  aOptions.Utf8Bom  := True; // default y (Delphi-friendly)
  aOptions.BackupDir:= '';

  // Parse
  for i := 1 to ParamCount do
  begin
    p := Trim(ParamStr(i));
    if p = '' then
    begin
      Continue;
    end;

    // allow "key=value" or "key:value" or single flag
    eqPos := p.IndexOf('=');
    if eqPos < 0 then
    begin
      eqPos := p.IndexOf(':');
    end;

    if eqPos > 0 then
    begin
      key := LowerCase(Trim(Copy(p, 1, eqPos)));
      val := Trim(Copy(p, eqPos + 2, MaxInt));
    end else
    begin
      key := LowerCase(p);
      val := '';
    end;

    if (key = 'help') or (key = '--help') or (key = '-h') then
    begin
      Exit(ShowHelp);
    end else
    if (key = 'dry') then
    begin
      aOptions.DryRun := True;
    end else
    if (key = 's') or (key = 'silent') then
    begin
      aOptions.Silent := True;
      aOptions.Verbose := False;
    end else
    if (key = 'v') or (key = 'verbose') then
    begin
      if not aOptions.Silent then
      begin
        aOptions.Verbose := True;
      end;
    end else
    if (key = 'path') then
    begin
      if val <> '' then
      begin
        aOptions.Path := ExpandFileName(val);
      end;
    end else
    if (key = 'recursive') then
    begin
      aOptions.Recursive := AsYN(val, True);
    end else
    if (key = 'ext') then
    begin
      if val <> '' then
      begin
        aOptions.Exts := NormalizeExtList(val);
      end;
    end else
    if (key = 'utf8-bom') then
    begin
      aOptions.Utf8Bom := AsYN(val, True);
    end else
    if (key = 'bkp-dir') then
    begin
      if val <> '' then
      begin
        aOptions.BackupDir := ExpandFileName(val);
      end;
    end else
    begin
      // Unknown param: ignore silently (or we could warn if verbose)
    end;
  end;

  Result := -1; // continue execution
end;

class function TEncodingFixTool.RunFromCommandLine: Integer;
var
  lOptions: TOptions;
  lParseRes: Integer;
  lFiles: TArray<string>;
  lChangedCount: Integer;
  lStopwatch: TStopwatch;
  lSilent: Boolean;
  lVerbose: Boolean;
begin
  Result := 0;

  lParseRes := ParseCommandLine(lOptions);
  if lParseRes >= 0 then
  begin
    Exit(lParseRes); // help shown or early exit
  end;

  lSilent := lOptions.Silent;
  lVerbose := lOptions.Verbose and (not lSilent);

  if not TDirectory.Exists(lOptions.Path) then
  begin
    if not lSilent then
    begin
      TSafeConsole.WriteLine('ERROR: path not found: ' + lOptions.Path);
    end;
    Exit(2);
  end;

  if (lOptions.BackupDir <> '') and (not TDirectory.Exists(lOptions.BackupDir)) then
  begin
    // create backup root if needed
    TDirectory.CreateDirectory(lOptions.BackupDir);
  end;

  lFiles := CollectFiles(lOptions);
  if (Length(lFiles) = 0) and (not lSilent) then
  begin
    TSafeConsole.WriteLine('No files found.');
  end;

  lChangedCount := 0;
  lStopwatch := TStopwatch.StartNew;

  TParallel.&For(0, High(lFiles),
    procedure (const idx: Integer)
    var
      lFile: string;
      lChanged: Boolean;
      lReason: string;
      lMsg: string;
      lLocalChanged: Integer;
    begin
      lFile := lFiles[idx];
      lLocalChanged := 0;

      try
        if FixFile(lFile, lOptions, lChanged, lReason) then
        begin
          if lChanged then
          begin
            lLocalChanged := 1;
            if not lSilent then
            begin
              TSafeConsole.WriteLine('Fixed: ' + lFile);
            end;
          end else
          begin
            if lVerbose and (not lSilent) then
            begin
              TSafeConsole.WriteLine('OK   : ' + lFile + ' (' + lReason + ')');
            end;
          end;
        end else
        begin
          if not lSilent then
          begin
            TSafeConsole.WriteLine('FAIL : ' + lFile + ' (' + lReason + ')');
          end;
        end;
      except
        on E: Exception do
        begin
          if not lSilent then
          begin
            lMsg := Format('ERROR: %s (%s)', [lFile, E.Message]);
            TSafeConsole.WriteLine(lMsg);
          end;
        end;
      end;

      if lLocalChanged <> 0 then
      begin
        TInterlocked.Add(lChangedCount, lLocalChanged);
      end;
    end
  );

  lStopwatch.Stop;

  if not lSilent then
  begin
    TSafeConsole.WriteLine('');
    TSafeConsole.WriteLine(Format('Done in %s. Files changed: %d',
      [FormatDateTime('nn:ss.zzz',  // pretty mm:ss.mmm
        EncodeTime(0, lStopwatch.Elapsed.Minutes, lStopwatch.Elapsed.Seconds, lStopwatch.Elapsed.Milliseconds)
      ),
       lChangedCount]));
  end;

  Result := 0;
end;

end.
