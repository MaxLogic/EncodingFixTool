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
  System.generics.collections,
  System.Types,
  System.IOUtils,
  System.SyncObjs,
  System.diagnostics,
  System.StrUtils,
  System.Character,
  System.Threading;

type
  TEncodingFixTool = class
  public type
      TOptions = record
        DryRun: boolean;
        Silent: boolean;
        Verbose: boolean; // not compatible with Silent
        Path: string;
        Recursive: boolean; // default y
        Exts: TArray<string>; // normalized: ".pas", ".dpr", etc.
        Utf8Bom: boolean; // default y
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
    class function ParseCommandLine(out aOptions: TOptions): integer; static;
    class function ShowHelp: integer; static;
    class function NormalizeExtList(const aCSV: string): TArray<string>; static;
    class function CollectFiles(const aOptions: TOptions): TArray<string>; static;
    class function HasWantedExt(const aFile: string; const aExts: TArray<string>): boolean; static;

    // Encoding helpers
    class function IsUtf8Strict(const aBytes: TBytes): boolean; static;
    class function SplitLinesByBytes(const aBytes: TBytes): TArray<TBytes>; static;
    class function DecodeBestPerLine(const aLineBytes: TBytes): string; static;
    class function ScoreDecoded(const aText: string): integer; static;
    class function ContainsSpecials(const aText: string): integer; static;

    class function FixFile(const aFile: string; const aOptions: TOptions; out aChanged: boolean; out aReason: string): boolean; static;
    class function SaveTextUTF8(const aFile: string; const aText: string; aWithBOM: boolean): boolean; static;
    class function MakeBackupPath(const aOptions: TOptions; const aRootPath, aFile: string): string; static;

  public
    // refactor this class. Only the RunFromCommandLine() method should be a class methid. All others should be normal instance methods. Inside the RunFromCommandLine() create a local instance and work with it instead of using the class methods. AI!
    class function RunFromCommandLine: integer; static;
  end;

implementation

uses
  AutoFree;

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
    writeln(aMsg);
  finally
    FCrit.release;
  end;
end;

class function TEncodingFixTool.NormalizeExtList(const aCSV: string): TArray<string>;
var
  lParts: TArray<string>;
  i: integer;
  s: string;
begin
  lParts := aCSV.Split([',', ';', ' '], TStringSplitOptions.ExcludeEmpty);
  SetLength(Result, length(lParts));
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

class function TEncodingFixTool.HasWantedExt(const aFile: string; const aExts: TArray<string>): boolean;
var
  lExt: string;
  lWanted: string;
begin
  // optimize this HasWantedExt() method. Create a fExt: TStringList storing the wanted extension there. sort it, and use it for loop ups. that should be more performant then this for loop. AI
  lExt := LowerCase(ExtractFileExt(aFile));
  for lWanted in aExts do
  begin
    if lExt = lWanted then
    begin
      exit(True);
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
  gc(lFiles);

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

end;

class function TEncodingFixTool.IsUtf8Strict(const aBytes: TBytes): boolean;
var
  lUTF8: TEncoding;
  ls: string;
  lBom, lBytes: TBytes;
begin
  lUTF8 := TEncoding.Utf8;
  lBytes := aBytes;

  lBom := lUTF8.GetPreamble
    if (length(lBytes) >= length9lBom)) then
if CompareMem(lBytes[0], lBom[0], length(lBom)) then
  lBytes := copy(lBytes, length(lBom), length(lBytes) - length(lBom));

try
  ls := lUTF8.GetString(aBytes);
  Result := True;
except
  on e: EEncodingError do
  begin
    Result := False;
  end;
end;
end;

class function TEncodingFixTool.SplitLinesByBytes(const aBytes: TBytes): TArray<TBytes>;
var
  i, lStart: integer;
  b: BYTE;
  lLine: TBytes;

  // optimize this PushLine() method. resizing a array is not performant. Use TList<String< instead. AI

  procedure PushLine(aEndExclusive: integer; aConsumeCR: boolean);
  var
    lLen: integer;
  begin
    lLen := aEndExclusive - lStart;
    if lLen < 0 then
    begin
      lLen := 0;
    end;
    SetLength(lLine, lLen);
    if lLen > 0 then
    begin
      move(aBytes[lStart], lLine[0], lLen);
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
  while i < length(aBytes) do
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
      if (i < length(aBytes)) and (aBytes[i] = $0A) then
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
  if lStart <= length(aBytes) - 1 then
  begin
    PushLine(length(aBytes), False);
  end;
end;

class function TEncodingFixTool.ContainsSpecials(const aText: string): integer;
const
  // Polish + German diacritics; count good matches positively.
  POL = 'ąćęłńóśźżĄĆĘŁŃÓŚŹŻ';
  GER = 'äöüÄÖÜßẞ';
var
  lScore: integer;
  ch: char;
begin
  lScore := 0;
  for ch in aText do
  begin
    if (pos(ch, POL) > 0) or (pos(ch, GER) > 0) then
    begin
      Inc(lScore);
    end;
  end;
  Result := lScore;
end;

class function TEncodingFixTool.ScoreDecoded(const aText: string): integer;
var
  lScore: integer;
  ch: char;
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
    // add comments explaining what that is. AI
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
  bestScore, sc: integer;
begin
  // 1) Try strict UTF-8 first
  lUTF8 := TEncoding.Utf8;
  try
    sUTF8 := lUTF8.GetString(aLineBytes);
    // strict success => take it
    exit(sUTF8);
  except
    on e: EEncodingError do
    begin
      // fall through
    end;
  end;

  // 2) Try single-byte candidates; they never fail, so score them.
  lCP1250 := TEncoding.GetEncoding(1250); // Polish/Central Europe
  lCP1252 := TEncoding.GetEncoding(1252); // Western/German
  lANSI := TEncoding.ANSI;

  s1250 := lCP1250.GetString(aLineBytes);
  s1252 := lCP1252.GetString(aLineBytes);
  sANSI := lANSI.GetString(aLineBytes);

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
    if SameText(copy(aFile, 1, length(IncludeTrailingPathDelimiter(aRootPath))), IncludeTrailingPathDelimiter(aRootPath)) then
    begin
      lRel := copy(aFile, length(IncludeTrailingPathDelimiter(aRootPath)) + 1, MaxInt);
    end;
  end;

  Result := TPath.Combine(IncludeTrailingPathDelimiter(aOptions.BackupDir), lRel);
end;

class function TEncodingFixTool.SaveTextUTF8(const aFile: string; const aText: string; aWithBOM: boolean): boolean;
var
  lSL: TStringList; // rename lSL to just "l". AI
  lEnc: TEncoding;
begin
  Result := False;
  gc(lSL, TStringList.Create);

  lEnc := TEncoding.Utf8;

  lSL.Text := aText;
  lSL.SaveToFile(aFile, lEnc);
  lSL.WriteBOM := aWithBOM;
  Result := True;
end;

class function TEncodingFixTool.FixFile(const aFile: string; const aOptions: TOptions; out aChanged: boolean; out aReason: string): boolean;
var
  lBytes: TBytes;
  lLinesBytes: TArray<TBytes>;
  lLine: TBytes;
  lFixedLines: TStringBuilder;
  lFirst: boolean;
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

    // that is only partially true. even if the file was decoded properly, check if it starts with the UTF8 BOM. check the options, if we should add a bom. If the file leaks a bom but we should add one, then we still need to midify this file. AI

    Result := True;
    aChanged := False;
    aReason := 'UTF-8 OK';
    exit;
  end;

  // Mixed encodings possible: split by raw CR/LF bytes, decode per line.
  lLinesBytes := SplitLinesByBytes(lBytes);

  lFixedLines := TStringBuilder.Create(length(lBytes) + 1024);
  gc(lFixedLines);

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
    lFixedLines.append(DecodeBestPerLine(lLine));
  end;

  // If dry-run, do not write anything—just indicate change.
  if aOptions.DryRun then
  begin
    aChanged := True;
    aReason := 'Would fix (dry-run)';
    Result := True;
    exit;
  end;

  // Backup if requested
  if aOptions.BackupDir <> '' then
  begin
    lRoot := IncludeTrailingPathDelimiter(ExpandFileName(aOptions.Path));
    lBackupPath := MakeBackupPath(aOptions, lRoot, aFile);
    TDirectory.CreateDirectory(ExtractFileDir(lBackupPath));
    TFile.copy(aFile, lBackupPath, True);
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
end;

class function TEncodingFixTool.ShowHelp: integer;
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
  writeln(HELP_TEXT);
  Result := 0;
end;

class function TEncodingFixTool.ParseCommandLine(out aOptions: TOptions): integer;
var
  i: integer;
  p, Key, Val: string;
  eqPos: integer;

  function AsYN(const s: string; const aDefault: boolean): boolean;
  var
    l: string;
  begin
    l := LowerCase(s.Trim);
    if (l = '') then
    begin
      exit(aDefault);
    end else
      if (l = 'y') or (l = 'yes') or (l = '1') or (l = 'true') then
    begin
      exit(True);
    end else
      if (l = 'n') or (l = 'no') or (l = '0') or (l = 'false') then
    begin
      exit(False);
    end else
    begin
      exit(aDefault);
    end;
  end;

begin
  // Defaults
  aOptions.DryRun := False;
  aOptions.Silent := False;
  aOptions.Verbose := False;
  aOptions.Path := GetCurrentDir;
  aOptions.Recursive := True;
  aOptions.Exts := NormalizeExtList('pas,dpr');
  aOptions.Utf8Bom := True; // default y (Delphi-friendly)
  aOptions.BackupDir := '';

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
      Key := LowerCase(Trim(copy(p, 1, eqPos)));
      Val := Trim(copy(p, eqPos + 2, MaxInt));
    end else
    begin
      Key := LowerCase(p);
      Val := '';
    end;

    if (Key = 'help') or (Key = '--help') or (Key = '-h') then
    begin
      exit(ShowHelp);
    end else
      if (Key = 'dry') then
    begin
      aOptions.DryRun := True;
    end else
      if (Key = 's') or (Key = 'silent') then
    begin
      aOptions.Silent := True;
      aOptions.Verbose := False;
    end else
      if (Key = 'v') or (Key = 'verbose') then
    begin
      if not aOptions.Silent then
      begin
        aOptions.Verbose := True;
      end;
    end else
      if (Key = 'path') then
    begin
      if Val <> '' then
      begin
        aOptions.Path := ExpandFileName(Val);
      end;
    end else
      if (Key = 'recursive') then
    begin
      aOptions.Recursive := AsYN(Val, True);
    end else
      if (Key = 'ext') then
    begin
      if Val <> '' then
      begin
        aOptions.Exts := NormalizeExtList(Val);
      end;
    end else
      if (Key = 'utf8-bom') then
    begin
      aOptions.Utf8Bom := AsYN(Val, True);
    end else
      if (Key = 'bkp-dir') then
    begin
      if Val <> '' then
      begin
        aOptions.BackupDir := ExpandFileName(Val);
      end;
    end else
    begin
      // Unknown param: ignore silently (or we could warn if verbose)
    end;
  end;

  Result := -1; // continue execution
end;

class function TEncodingFixTool.RunFromCommandLine: integer;
var
  lOptions: TOptions;
  lParseRes: integer;
  lFiles: TArray<string>;
  lChangedCount: integer;
  lStopwatch: TStopWatch;
  lSilent: boolean;
  lVerbose: boolean;
begin

  Result := 0;

  lParseRes := ParseCommandLine(lOptions);
  if lParseRes >= 0 then
  begin
    exit(lParseRes); // help shown or early exit
  end;

  lSilent := lOptions.Silent;
  lVerbose := lOptions.Verbose and (not lSilent);

  if not TDirectory.Exists(lOptions.Path) then
  begin
    if not lSilent then
    begin
      TSafeConsole.WriteLine('ERROR: path not found: ' + lOptions.Path);
    end;
    exit(2);
  end;

  if (lOptions.BackupDir <> '') and (not TDirectory.Exists(lOptions.BackupDir)) then
  begin
    // create backup root if needed
    TDirectory.CreateDirectory(lOptions.BackupDir);
  end;

  lFiles := CollectFiles(lOptions);
  if (length(lFiles) = 0) and (not lSilent) then
  begin
    TSafeConsole.WriteLine('No files found.');
  end;

  lChangedCount := 0;
  lStopwatch := TStopWatch.startNew;

  TParallel.&For(0, High(lFiles),
    procedure(const idx: integer)
    var
      lFile: string;
      lChanged: boolean;
      lReason: string;
      lMsg: string;
      lLocalChanged: integer;
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
        on e: Exception do
        begin
          if not lSilent then
          begin
            lMsg := Format('ERROR: %s (%s)', [lFile, e.Message]);
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

  lStopwatch.stop;

  if not lSilent then
  begin
    TSafeConsole.WriteLine('');
    TSafeConsole.WriteLine(Format('Done in %s. Files changed: %d',
      [FormatDateTime('nn:ss.zzz', // pretty mm:ss.mmm
          EncodeTime(0, lStopwatch.Elapsed.Minutes, lStopwatch.Elapsed.Seconds, lStopwatch.Elapsed.Milliseconds)
          ),
        lChangedCount]));
  end;

  Result := 0;
end;

end.

