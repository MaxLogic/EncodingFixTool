unit EncodingFixTool.IntegrationTests;

interface

uses
  System.SysUtils,
  DUnitX.TestFramework;

type
  [TestFixture]
  TEncodingFixToolIntegrationTests = class
  private
    class function AppendBytes(const aLeft, aRight: TBytes): TBytes; static;
    class function BytesOfCP1250(const aText: string): TBytes; static;
    class function BytesOfUtf16LEWithBom(const aText: string): TBytes; static;
    class function BytesOfUtf8(const aText: string; aWithBom: Boolean): TBytes; static;
    class function HasUtf8Bom(const aBytes: TBytes): Boolean; static;
    class function QuoteArg(const aArg: string): string; static;
    class function ReadUtf8TextWithoutBom(const aFileName: string): string; static;
    class function RepoRoot: string; static;
    class function RunProcess(const aCommandLine, aWorkingDirectory: string): Cardinal; static;
    class function RunTool(const aRootPath: string; const aArgs: TArray<string>): Cardinal; static;
    class function ToolPath: string; static;
    class procedure AssertBytesEqual(const aExpected, aActual: TBytes); static;
    class function WriteBytes(const aRootPath, aRelativeFileName: string; const aBytes: TBytes): string; static;
    class procedure DeleteTree(const aPath: string); static;
    class procedure RequireTool; static;
  public
    [Test]
    procedure AddsUtf8BomToNonAsciiUtf8File;

    [Test]
    procedure RemovesUtf8BomWhenRequested;

    [Test]
    procedure LeavesAsciiFilesWithoutBom;

    [Test]
    procedure ConvertsUtf16LEFileToUtf8;

    [Test]
    procedure RepairsCP1250FileToUtf8;

    [Test]
    procedure RepairsMixedUtf8AndCP1250Lines;

    [Test]
    procedure CreatesBackupBeforeOverwriting;

    [Test]
    procedure RejectsInvalidBooleanOption;

    [Test]
    procedure MissingPathReturnsExitCodeTwo;

    [Test]
    [Category('Dfm')]
    procedure SkipsBinaryDfmWithoutModification;

    [Test]
    [Category('Dfm')]
    procedure RepairsTextDfmThroughNormalEncodingPath;

    [Test]
    [Category('Dfm')]
    procedure NormalizesTextDfmLineEndingsWhenRequested;

    [Test]
    [Category('AiWorkflow')]
    procedure DelphiAiPresetNormalizesDelphiFiles;

    [Test]
    [Category('AiWorkflow')]
    procedure GitChangedScopeProcessesOnlyChangedFiles;

    [Test]
    [Category('PresetConfig')]
    procedure ExplicitConfigPresetAppliesToIncludedExtensions;

    [Test]
    [Category('PresetConfig')]
    procedure CliArgumentsOverrideConfigPresetRegardlessOfOrder;

    [Test]
    [Category('PresetConfig')]
    procedure ExplicitConfigOverridesRepoConfigAndMayAppearBeforePath;

    [Test]
    [Category('PresetConfig')]
    procedure RepoConfigOverridesUserConfig;

    [Test]
    [Category('PresetConfig')]
    procedure RepoConfigIsDiscoveredFromScanRootParents;

    [Test]
    [Category('PresetConfig')]
    procedure InvalidPresetFailsWithoutRewritingFiles;

    [Test]
    [Category('PresetConfig')]
    procedure MalformedConfigFailsWithoutRewritingFiles;

    [Test]
    [Category('PresetConfig')]
    procedure InvalidPresetOptionFailsWithoutRewritingFiles;

    [Test]
    [Category('LineEnding')]
    procedure DryRunLeavesAsciiLfUnchangedWhenNormalizingEol;

    [Test]
    [Category('LineEnding')]
    procedure NormalizesAsciiLfToCrlfWhenRequested;

    [Test]
    [Category('LineEnding')]
    procedure NormalizesAsciiCrToCrlfWhenRequested;

    [Test]
    [Category('LineEnding')]
    procedure NormalizesCP1250LfToCrlfWhenRequested;

    [Test]
    [Category('LineEnding')]
    procedure NormalizesUtf8LfToCrlfWhenRequested;

    [Test]
    [Category('LineEnding')]
    procedure PreservesAsciiLfWhenExplicitlyRequested;

    [Test]
    [Category('LineEnding')]
    procedure PreservesCP1250LfDuringEncodingRepairByDefault;

    [Test]
    [Category('LineEnding')]
    procedure PreservesAsciiLfByDefault;
  end;

implementation

uses
  System.IOUtils,
  Winapi.Windows,
  DUnitX.Assert;

const
  cUtf8Bom0 = Byte($EF);
  cUtf8Bom1 = Byte($BB);
  cUtf8Bom2 = Byte($BF);

class function TEncodingFixToolIntegrationTests.AppendBytes(const aLeft, aRight: TBytes): TBytes;
begin
  SetLength(Result, Length(aLeft) + Length(aRight));
  if Length(aLeft) > 0 then
  begin
    Move(aLeft[0], Result[0], Length(aLeft));
  end;
  if Length(aRight) > 0 then
  begin
    Move(aRight[0], Result[Length(aLeft)], Length(aRight));
  end;
end;

class procedure TEncodingFixToolIntegrationTests.AssertBytesEqual(const aExpected, aActual: TBytes);
var
  i: Integer;
begin
  Assert.AreEqual(Length(aExpected), Length(aActual), 'Byte array length mismatch.');
  for i := 0 to High(aExpected) do
  begin
    Assert.AreEqual(Integer(aExpected[i]), Integer(aActual[i]), Format('Byte mismatch at index %d.', [i]));
  end;
end;

class function TEncodingFixToolIntegrationTests.BytesOfCP1250(const aText: string): TBytes;
var
  lEncoding: TEncoding;
begin
  lEncoding := TEncoding.GetEncoding(1250);
  try
    Result := lEncoding.GetBytes(aText);
  finally
    lEncoding.Free;
  end;
end;

class function TEncodingFixToolIntegrationTests.BytesOfUtf16LEWithBom(const aText: string): TBytes;
var
  lBom: TBytes;
  lContent: TBytes;
begin
  SetLength(lBom, 2);
  lBom[0] := $FF;
  lBom[1] := $FE;
  lContent := TEncoding.Unicode.GetBytes(aText);
  Result := AppendBytes(lBom, lContent);
end;

class function TEncodingFixToolIntegrationTests.BytesOfUtf8(const aText: string; aWithBom: Boolean): TBytes;
var
  lBom: TBytes;
  lContent: TBytes;
begin
  lContent := TEncoding.UTF8.GetBytes(aText);
  if not aWithBom then
  begin
    Exit(lContent);
  end;

  SetLength(lBom, 3);
  lBom[0] := cUtf8Bom0;
  lBom[1] := cUtf8Bom1;
  lBom[2] := cUtf8Bom2;
  Result := AppendBytes(lBom, lContent);
end;

class procedure TEncodingFixToolIntegrationTests.DeleteTree(const aPath: string);
begin
  if TDirectory.Exists(aPath) then
  begin
    TDirectory.Delete(aPath, True);
  end;
end;

class function TEncodingFixToolIntegrationTests.HasUtf8Bom(const aBytes: TBytes): Boolean;
begin
  Result := (Length(aBytes) >= 3) and (aBytes[0] = cUtf8Bom0) and (aBytes[1] = cUtf8Bom1) and
    (aBytes[2] = cUtf8Bom2);
end;

class function TEncodingFixToolIntegrationTests.ReadUtf8TextWithoutBom(const aFileName: string): string;
var
  lBytes: TBytes;
begin
  lBytes := TFile.ReadAllBytes(aFileName);
  if HasUtf8Bom(lBytes) then
  begin
    lBytes := Copy(lBytes, 3, Length(lBytes) - 3);
  end;
  Result := TEncoding.UTF8.GetString(lBytes);
end;

class function TEncodingFixToolIntegrationTests.RepoRoot: string;
begin
  Result := TPath.GetFullPath(TPath.Combine(ExtractFilePath(ParamStr(0)), '..'));
end;

class function TEncodingFixToolIntegrationTests.RunProcess(const aCommandLine, aWorkingDirectory: string): Cardinal;
var
  lCommandLine: string;
  lCommandLineChars: TArray<Char>;
  lProcessInformation: TProcessInformation;
  lStartupInfo: TStartupInfo;
  lWaitResult: Cardinal;
begin
  lCommandLine := aCommandLine;
  lCommandLineChars := lCommandLine.ToCharArray;
  SetLength(lCommandLineChars, Length(lCommandLineChars) + 1);
  ZeroMemory(@lProcessInformation, SizeOf(lProcessInformation));
  ZeroMemory(@lStartupInfo, SizeOf(lStartupInfo));
  lStartupInfo.cb := SizeOf(lStartupInfo);

  if not CreateProcess(nil, PChar(lCommandLineChars), nil, nil, False, CREATE_NO_WINDOW, nil, PChar(aWorkingDirectory),
    lStartupInfo, lProcessInformation) then
  begin
    RaiseLastOSError;
  end;
  try
    lWaitResult := WaitForSingleObject(lProcessInformation.hProcess, 30000);
    if lWaitResult <> WAIT_OBJECT_0 then
    begin
      TerminateProcess(lProcessInformation.hProcess, 1);
      Assert.Fail('Process timed out: ' + aCommandLine);
    end;
    if not GetExitCodeProcess(lProcessInformation.hProcess, Result) then
    begin
      RaiseLastOSError;
    end;
  finally
    CloseHandle(lProcessInformation.hThread);
    CloseHandle(lProcessInformation.hProcess);
  end;
end;

class procedure TEncodingFixToolIntegrationTests.RequireTool;
begin
  Assert.IsTrue(TFile.Exists(ToolPath), 'Build EncodingFixTool.exe before running integration tests: ' + ToolPath);
end;

class function TEncodingFixToolIntegrationTests.QuoteArg(const aArg: string): string;
begin
  Result := '"' + StringReplace(aArg, '"', '\"', [rfReplaceAll]) + '"';
end;

class function TEncodingFixToolIntegrationTests.RunTool(const aRootPath: string; const aArgs: TArray<string>): Cardinal;
var
  i: Integer;
  lCommandLine: string;
  lProcessInformation: TProcessInformation;
  lStartupInfo: TStartupInfo;
  lWaitResult: Cardinal;
begin
  RequireTool;

  lCommandLine := QuoteArg(ToolPath);
  if aRootPath <> '' then
  begin
    lCommandLine := lCommandLine + ' ' + QuoteArg('path=' + aRootPath);
  end;
  for i := 0 to High(aArgs) do
  begin
    lCommandLine := lCommandLine + ' ' + QuoteArg(aArgs[i]);
  end;

  ZeroMemory(@lProcessInformation, SizeOf(lProcessInformation));
  ZeroMemory(@lStartupInfo, SizeOf(lStartupInfo));
  lStartupInfo.cb := SizeOf(lStartupInfo);

  if not CreateProcess(nil, PChar(lCommandLine), nil, nil, False, CREATE_NO_WINDOW, nil, PChar(RepoRoot),
    lStartupInfo, lProcessInformation) then
  begin
    RaiseLastOSError;
  end;
  try
    lWaitResult := WaitForSingleObject(lProcessInformation.hProcess, 30000);
    if lWaitResult <> WAIT_OBJECT_0 then
    begin
      TerminateProcess(lProcessInformation.hProcess, 1);
      Assert.Fail('EncodingFixTool.exe timed out.');
    end;
    if not GetExitCodeProcess(lProcessInformation.hProcess, Result) then
    begin
      RaiseLastOSError;
    end;
  finally
    CloseHandle(lProcessInformation.hThread);
    CloseHandle(lProcessInformation.hProcess);
  end;
end;

class function TEncodingFixToolIntegrationTests.ToolPath: string;
begin
  Result := TPath.Combine(RepoRoot, 'bin\EncodingFixTool.exe');
end;

class function TEncodingFixToolIntegrationTests.WriteBytes(const aRootPath, aRelativeFileName: string;
  const aBytes: TBytes): string;
begin
  Result := TPath.Combine(aRootPath, aRelativeFileName);
  TDirectory.CreateDirectory(ExtractFilePath(Result));
  TFile.WriteAllBytes(Result, aBytes);
end;

procedure TEncodingFixToolIntegrationTests.AddsUtf8BomToNonAsciiUtf8File;
var
  lBytes: TBytes;
  lFileName: string;
  lRootPath: string;
begin
  lRootPath := TPath.Combine(TPath.GetTempPath, 'EncodingFix-DUnitX-' + TGuid.NewGuid.ToString);
  TDirectory.CreateDirectory(lRootPath);
  try
    lFileName := WriteBytes(lRootPath, 'utf8-no-bom.pas', BytesOfUtf8('unit Demo; const S = ''zażółć'';', False));

    Assert.AreEqual(0, Integer(RunTool(lRootPath, ['recursive=n', 'ext=pas', 's'])));

    lBytes := TFile.ReadAllBytes(lFileName);
    Assert.IsTrue(HasUtf8Bom(lBytes), 'Expected UTF-8 BOM to be added.');
    Assert.AreEqual('unit Demo; const S = ''zażółć'';', ReadUtf8TextWithoutBom(lFileName));
  finally
    DeleteTree(lRootPath);
  end;
end;

procedure TEncodingFixToolIntegrationTests.CreatesBackupBeforeOverwriting;
var
  lBackupFileName: string;
  lBackupPath: string;
  lFileName: string;
  lRootPath: string;
begin
  lRootPath := TPath.Combine(TPath.GetTempPath, 'EncodingFix-DUnitX-' + TGuid.NewGuid.ToString);
  lBackupPath := TPath.Combine(TPath.GetTempPath, 'EncodingFix-DUnitX-Backup-' + TGuid.NewGuid.ToString);
  TDirectory.CreateDirectory(lRootPath);
  try
    lFileName := WriteBytes(lRootPath, 'nested\legacy.pas', BytesOfCP1250('unit Demo; const S = ''zażółć'';'));

    Assert.AreEqual(0, Integer(RunTool(lRootPath, ['recursive=y', 'ext=pas', 's', 'bkp-dir=' + lBackupPath])));

    lBackupFileName := TPath.Combine(lBackupPath, 'nested\legacy.pas');
    Assert.IsTrue(TFile.Exists(lBackupFileName), 'Expected backup file.');
    AssertBytesEqual(BytesOfCP1250('unit Demo; const S = ''zażółć'';'), TFile.ReadAllBytes(lBackupFileName));
    Assert.IsTrue(HasUtf8Bom(TFile.ReadAllBytes(lFileName)), 'Expected fixed file to be UTF-8 with BOM.');
  finally
    DeleteTree(lRootPath);
    DeleteTree(lBackupPath);
  end;
end;

procedure TEncodingFixToolIntegrationTests.ConvertsUtf16LEFileToUtf8;
var
  lBytes: TBytes;
  lFileName: string;
  lRootPath: string;
begin
  lRootPath := TPath.Combine(TPath.GetTempPath, 'EncodingFix-DUnitX-' + TGuid.NewGuid.ToString);
  TDirectory.CreateDirectory(lRootPath);
  try
    lFileName := WriteBytes(lRootPath, 'utf16le.pas', BytesOfUtf16LEWithBom('unit Demo; const S = ''zażółć'';'));

    Assert.AreEqual(0, Integer(RunTool(lRootPath, ['recursive=n', 'ext=pas', 's'])));

    lBytes := TFile.ReadAllBytes(lFileName);
    Assert.IsTrue(HasUtf8Bom(lBytes), 'Expected UTF-8 BOM after conversion.');
    Assert.AreEqual('unit Demo; const S = ''zażółć'';', ReadUtf8TextWithoutBom(lFileName));
  finally
    DeleteTree(lRootPath);
  end;
end;

procedure TEncodingFixToolIntegrationTests.LeavesAsciiFilesWithoutBom;
var
  lBytes: TBytes;
  lFileName: string;
  lRootPath: string;
begin
  lRootPath := TPath.Combine(TPath.GetTempPath, 'EncodingFix-DUnitX-' + TGuid.NewGuid.ToString);
  TDirectory.CreateDirectory(lRootPath);
  try
    lFileName := WriteBytes(lRootPath, 'ascii.pas', TEncoding.ASCII.GetBytes('unit Demo; interface end.'));

    Assert.AreEqual(0, Integer(RunTool(lRootPath, ['recursive=n', 'ext=pas', 's'])));

    lBytes := TFile.ReadAllBytes(lFileName);
    Assert.IsFalse(HasUtf8Bom(lBytes), 'ASCII files must stay BOM-free.');
    Assert.AreEqual('unit Demo; interface end.', TEncoding.ASCII.GetString(lBytes));
  finally
    DeleteTree(lRootPath);
  end;
end;

procedure TEncodingFixToolIntegrationTests.MissingPathReturnsExitCodeTwo;
var
  lMissingPath: string;
begin
  lMissingPath := TPath.Combine(TPath.GetTempPath, 'EncodingFix-DUnitX-Missing-' + TGuid.NewGuid.ToString);

  Assert.AreEqual(2, Integer(RunTool(lMissingPath, ['recursive=n', 'ext=pas', 's'])));
end;

procedure TEncodingFixToolIntegrationTests.SkipsBinaryDfmWithoutModification;
var
  lBytes: TBytes;
  lFileName: string;
  lRootPath: string;
begin
  lRootPath := TPath.Combine(TPath.GetTempPath, 'EncodingFix-DUnitX-' + TGuid.NewGuid.ToString);
  TDirectory.CreateDirectory(lRootPath);
  try
    lBytes := TBytes.Create($54, $50, $46, $30, $00, $01, $FF, $80, $0D, $0A, $00, $02);
    lFileName := WriteBytes(lRootPath, 'binary.dfm', lBytes);

    Assert.AreEqual(0, Integer(RunTool(lRootPath, ['recursive=n', 'ext=dfm', 'v'])));

    AssertBytesEqual(lBytes, TFile.ReadAllBytes(lFileName));
  finally
    DeleteTree(lRootPath);
  end;
end;

procedure TEncodingFixToolIntegrationTests.RepairsTextDfmThroughNormalEncodingPath;
var
  lFileName: string;
  lRootPath: string;
begin
  lRootPath := TPath.Combine(TPath.GetTempPath, 'EncodingFix-DUnitX-' + TGuid.NewGuid.ToString);
  TDirectory.CreateDirectory(lRootPath);
  try
    lFileName := WriteBytes(lRootPath, 'text.dfm', BytesOfCP1250('object Form1: TForm'#13#10'  Caption = ''zażółć'''#13#10'end'));

    Assert.AreEqual(0, Integer(RunTool(lRootPath, ['recursive=n', 'ext=dfm', 's'])));

    Assert.IsTrue(HasUtf8Bom(TFile.ReadAllBytes(lFileName)), 'Expected UTF-8 BOM after text DFM conversion.');
    Assert.AreEqual('object Form1: TForm'#13#10'  Caption = ''zażółć'''#13#10'end', ReadUtf8TextWithoutBom(lFileName));
  finally
    DeleteTree(lRootPath);
  end;
end;

procedure TEncodingFixToolIntegrationTests.NormalizesTextDfmLineEndingsWhenRequested;
var
  lFileName: string;
  lRootPath: string;
begin
  lRootPath := TPath.Combine(TPath.GetTempPath, 'EncodingFix-DUnitX-' + TGuid.NewGuid.ToString);
  TDirectory.CreateDirectory(lRootPath);
  try
    lFileName := WriteBytes(lRootPath, 'text-lf.dfm', BytesOfCP1250('object Form1: TForm'#10'  Caption = ''zażółć'''#10'end'));

    Assert.AreEqual(0, Integer(RunTool(lRootPath, ['recursive=n', 'ext=dfm', 'eol=crlf', 's'])));

    Assert.IsTrue(HasUtf8Bom(TFile.ReadAllBytes(lFileName)), 'Expected UTF-8 BOM after text DFM conversion.');
    Assert.AreEqual('object Form1: TForm'#13#10'  Caption = ''zażółć'''#13#10'end', ReadUtf8TextWithoutBom(lFileName));
  finally
    DeleteTree(lRootPath);
  end;
end;

procedure TEncodingFixToolIntegrationTests.DelphiAiPresetNormalizesDelphiFiles;
var
  lBinaryDfmBytes: TBytes;
  lBinaryDfmFileName: string;
  lDprojFileName: string;
  lIncFileName: string;
  lNestedFileName: string;
  lRootPath: string;
begin
  lRootPath := TPath.Combine(TPath.GetTempPath, 'EncodingFix-DUnitX-' + TGuid.NewGuid.ToString);
  TDirectory.CreateDirectory(lRootPath);
  try
    lIncFileName := WriteBytes(lRootPath, 'generated.inc', TEncoding.ASCII.GetBytes('const A = 1;'#10'const B = 2;'));
    lNestedFileName := WriteBytes(lRootPath, 'nested\nonascii.pas', BytesOfUtf8('unit Demo;'#10'const S = ''zażółć'';', False));
    lDprojFileName := WriteBytes(lRootPath, 'Project1.dproj', TEncoding.ASCII.GetBytes('<Project>'#10'</Project>'));
    lBinaryDfmBytes := TBytes.Create($54, $50, $46, $30, $00, $01, $FF, $80);
    lBinaryDfmFileName := WriteBytes(lRootPath, 'binary.dfm', lBinaryDfmBytes);

    Assert.AreEqual(0, Integer(RunTool(lRootPath, ['preset=delphi-ai', 's'])));

    AssertBytesEqual(TEncoding.ASCII.GetBytes('const A = 1;'#13#10'const B = 2;'), TFile.ReadAllBytes(lIncFileName));
    Assert.IsTrue(HasUtf8Bom(TFile.ReadAllBytes(lNestedFileName)), 'Expected UTF-8 BOM for nested non-ASCII Delphi file.');
    Assert.AreEqual('unit Demo;'#13#10'const S = ''zażółć'';', ReadUtf8TextWithoutBom(lNestedFileName));
    AssertBytesEqual(TEncoding.ASCII.GetBytes('<Project>'#13#10'</Project>'), TFile.ReadAllBytes(lDprojFileName));
    AssertBytesEqual(lBinaryDfmBytes, TFile.ReadAllBytes(lBinaryDfmFileName));
  finally
    DeleteTree(lRootPath);
  end;
end;

procedure TEncodingFixToolIntegrationTests.GitChangedScopeProcessesOnlyChangedFiles;
var
  lChangedFileName: string;
  lRootPath: string;
  lScanPath: string;
  lUnchangedFileName: string;
  lUntrackedFileName: string;
begin
  lRootPath := TPath.Combine(TPath.GetTempPath, 'EncodingFix-DUnitX-' + TGuid.NewGuid.ToString);
  lScanPath := TPath.Combine(lRootPath, 'src');
  TDirectory.CreateDirectory(lRootPath);
  try
    lChangedFileName := WriteBytes(lRootPath, 'src\changed.pas', TEncoding.ASCII.GetBytes('unit Changed;'#10'end.'));
    lUnchangedFileName := WriteBytes(lRootPath, 'src\unchanged.pas', TEncoding.ASCII.GetBytes('unit Unchanged;'#10'end.'));

    Assert.AreEqual(0, Integer(RunProcess('git init', lRootPath)));
    Assert.AreEqual(0, Integer(RunProcess('git config user.email test@example.invalid', lRootPath)));
    Assert.AreEqual(0, Integer(RunProcess('git config user.name EncodingFix Test', lRootPath)));
    Assert.AreEqual(0, Integer(RunProcess('git add src/changed.pas src/unchanged.pas', lRootPath)));
    Assert.AreEqual(0, Integer(RunProcess('git commit -m initial', lRootPath)));

    TFile.WriteAllBytes(lChangedFileName, TEncoding.ASCII.GetBytes('unit Changed;'#10'interface'#10'end.'));
    lUntrackedFileName := WriteBytes(lRootPath, 'src\untracked.inc', TEncoding.ASCII.GetBytes('const A = 1;'#10'end.'));

    Assert.AreEqual(0, Integer(RunTool(lScanPath, ['preset=delphi-ai', 'scope=git-changed', 's'])));

    AssertBytesEqual(TEncoding.ASCII.GetBytes('unit Changed;'#13#10'interface'#13#10'end.'),
      TFile.ReadAllBytes(lChangedFileName));
    AssertBytesEqual(TEncoding.ASCII.GetBytes('const A = 1;'#13#10'end.'), TFile.ReadAllBytes(lUntrackedFileName));
    AssertBytesEqual(TEncoding.ASCII.GetBytes('unit Unchanged;'#10'end.'), TFile.ReadAllBytes(lUnchangedFileName));
  finally
    DeleteTree(lRootPath);
  end;
end;

procedure TEncodingFixToolIntegrationTests.ExplicitConfigPresetAppliesToIncludedExtensions;
var
  lFileName: string;
  lRootPath: string;
begin
  lRootPath := TPath.Combine(TPath.GetTempPath, 'EncodingFix-DUnitX-' + TGuid.NewGuid.ToString);
  TDirectory.CreateDirectory(lRootPath);
  try
    WriteBytes(lRootPath, 'encodingfix.json', TEncoding.UTF8.GetBytes('{"presets":{"quick":{"ext":"inc","eol":"crlf"}}}'));
    lFileName := WriteBytes(lRootPath, 'generated.inc', TEncoding.ASCII.GetBytes('const A = 1;'#10'end.'));

    Assert.AreEqual(0, Integer(RunTool(lRootPath, ['config=encodingfix.json', 'preset=quick', 's'])));

    AssertBytesEqual(TEncoding.ASCII.GetBytes('const A = 1;'#13#10'end.'), TFile.ReadAllBytes(lFileName));
  finally
    DeleteTree(lRootPath);
  end;
end;

procedure TEncodingFixToolIntegrationTests.CliArgumentsOverrideConfigPresetRegardlessOfOrder;
var
  lFileName: string;
  lRootPath: string;
begin
  lRootPath := TPath.Combine(TPath.GetTempPath, 'EncodingFix-DUnitX-' + TGuid.NewGuid.ToString);
  TDirectory.CreateDirectory(lRootPath);
  try
    WriteBytes(lRootPath, 'encodingfix.json', TEncoding.UTF8.GetBytes('{"presets":{"quick":{"ext":"inc","eol":"crlf"}}}'));
    lFileName := WriteBytes(lRootPath, 'generated.inc', TEncoding.ASCII.GetBytes('const A = 1;'#10'end.'));

    Assert.AreEqual(0, Integer(RunTool(lRootPath, ['eol=preserve', 'config=encodingfix.json', 'preset=quick', 's'])));

    AssertBytesEqual(TEncoding.ASCII.GetBytes('const A = 1;'#10'end.'), TFile.ReadAllBytes(lFileName));
  finally
    DeleteTree(lRootPath);
  end;
end;

procedure TEncodingFixToolIntegrationTests.ExplicitConfigOverridesRepoConfigAndMayAppearBeforePath;
var
  lFileName: string;
  lRootPath: string;
begin
  lRootPath := TPath.Combine(TPath.GetTempPath, 'EncodingFix-DUnitX-' + TGuid.NewGuid.ToString);
  TDirectory.CreateDirectory(lRootPath);
  try
    WriteBytes(lRootPath, '.encodingfix.json', TEncoding.UTF8.GetBytes('{"presets":{"shared":{"ext":"inc","eol":"preserve"}}}'));
    WriteBytes(lRootPath, 'encodingfix.json', TEncoding.UTF8.GetBytes('{"presets":{"shared":{"ext":"inc","eol":"crlf"}}}'));
    lFileName := WriteBytes(lRootPath, 'generated.inc', TEncoding.ASCII.GetBytes('const A = 1;'#10'end.'));

    Assert.AreEqual(0, Integer(RunProcess(QuoteArg(ToolPath) + ' config=encodingfix.json path=' + QuoteArg(lRootPath) +
      ' preset=shared s', RepoRoot)));

    AssertBytesEqual(TEncoding.ASCII.GetBytes('const A = 1;'#13#10'end.'), TFile.ReadAllBytes(lFileName));
  finally
    DeleteTree(lRootPath);
  end;
end;

procedure TEncodingFixToolIntegrationTests.RepoConfigOverridesUserConfig;
var
  lAppDataPath: string;
  lConfigDir: string;
  lFileName: string;
  lOldAppData: string;
  lRootPath: string;
begin
  lRootPath := TPath.Combine(TPath.GetTempPath, 'EncodingFix-DUnitX-' + TGuid.NewGuid.ToString);
  lAppDataPath := TPath.Combine(lRootPath, 'appdata');
  lConfigDir := TPath.Combine(TPath.Combine(TPath.Combine(lAppDataPath, 'MaxLogic'), 'EncodingFixTool'), '');
  TDirectory.CreateDirectory(lRootPath);
  try
    TDirectory.CreateDirectory(lConfigDir);
    WriteBytes(lConfigDir, 'config.json', TEncoding.UTF8.GetBytes('{"presets":{"shared":{"ext":"inc","eol":"preserve"}}}'));
    WriteBytes(lRootPath, '.encodingfix.json', TEncoding.UTF8.GetBytes('{"presets":{"shared":{"ext":"inc","eol":"crlf"}}}'));
    lFileName := WriteBytes(lRootPath, 'generated.inc', TEncoding.ASCII.GetBytes('const A = 1;'#10'end.'));

    lOldAppData := GetEnvironmentVariable('APPDATA');
    SetEnvironmentVariable('APPDATA', PChar(lAppDataPath));
    try
      Assert.AreEqual(0, Integer(RunTool(lRootPath, ['preset=shared', 's'])));
    finally
      SetEnvironmentVariable('APPDATA', PChar(lOldAppData));
    end;

    AssertBytesEqual(TEncoding.ASCII.GetBytes('const A = 1;'#13#10'end.'), TFile.ReadAllBytes(lFileName));
  finally
    DeleteTree(lRootPath);
  end;
end;

procedure TEncodingFixToolIntegrationTests.RepoConfigIsDiscoveredFromScanRootParents;
var
  lFileName: string;
  lNestedPath: string;
  lRootPath: string;
begin
  lRootPath := TPath.Combine(TPath.GetTempPath, 'EncodingFix-DUnitX-' + TGuid.NewGuid.ToString);
  lNestedPath := TPath.Combine(lRootPath, 'src\nested');
  TDirectory.CreateDirectory(lNestedPath);
  try
    WriteBytes(lRootPath, '.encodingfix.json', TEncoding.UTF8.GetBytes('{"presets":{"repo":{"ext":"inc","eol":"crlf"}}}'));
    lFileName := WriteBytes(lNestedPath, 'generated.inc', TEncoding.ASCII.GetBytes('const A = 1;'#10'end.'));

    Assert.AreEqual(0, Integer(RunTool(lNestedPath, ['preset=repo', 's'])));

    AssertBytesEqual(TEncoding.ASCII.GetBytes('const A = 1;'#13#10'end.'), TFile.ReadAllBytes(lFileName));
  finally
    DeleteTree(lRootPath);
  end;
end;

procedure TEncodingFixToolIntegrationTests.InvalidPresetFailsWithoutRewritingFiles;
var
  lFileName: string;
  lRootPath: string;
begin
  lRootPath := TPath.Combine(TPath.GetTempPath, 'EncodingFix-DUnitX-' + TGuid.NewGuid.ToString);
  TDirectory.CreateDirectory(lRootPath);
  try
    WriteBytes(lRootPath, '.encodingfix.json', TEncoding.UTF8.GetBytes('{"presets":{"repo":{"ext":"inc","eol":"crlf"}}}'));
    lFileName := WriteBytes(lRootPath, 'generated.inc', TEncoding.ASCII.GetBytes('const A = 1;'#10'end.'));

    Assert.AreEqual(1, Integer(RunTool(lRootPath, ['preset=missing', 's'])));

    AssertBytesEqual(TEncoding.ASCII.GetBytes('const A = 1;'#10'end.'), TFile.ReadAllBytes(lFileName));
  finally
    DeleteTree(lRootPath);
  end;
end;

procedure TEncodingFixToolIntegrationTests.MalformedConfigFailsWithoutRewritingFiles;
var
  lFileName: string;
  lRootPath: string;
begin
  lRootPath := TPath.Combine(TPath.GetTempPath, 'EncodingFix-DUnitX-' + TGuid.NewGuid.ToString);
  TDirectory.CreateDirectory(lRootPath);
  try
    WriteBytes(lRootPath, 'bad.json', TEncoding.UTF8.GetBytes('{"presets":'));
    lFileName := WriteBytes(lRootPath, 'generated.inc', TEncoding.ASCII.GetBytes('const A = 1;'#10'end.'));

    Assert.AreEqual(1, Integer(RunTool(lRootPath, ['config=bad.json', 'preset=repo', 's'])));

    AssertBytesEqual(TEncoding.ASCII.GetBytes('const A = 1;'#10'end.'), TFile.ReadAllBytes(lFileName));
  finally
    DeleteTree(lRootPath);
  end;
end;

procedure TEncodingFixToolIntegrationTests.InvalidPresetOptionFailsWithoutRewritingFiles;
var
  lFileName: string;
  lRootPath: string;
begin
  lRootPath := TPath.Combine(TPath.GetTempPath, 'EncodingFix-DUnitX-' + TGuid.NewGuid.ToString);
  TDirectory.CreateDirectory(lRootPath);
  try
    WriteBytes(lRootPath, 'encodingfix.json', TEncoding.UTF8.GetBytes('{"presets":{"bad":{"ext":"inc","eol":"unix"}}}'));
    lFileName := WriteBytes(lRootPath, 'generated.inc', TEncoding.ASCII.GetBytes('const A = 1;'#10'end.'));

    Assert.AreEqual(1, Integer(RunTool(lRootPath, ['config=encodingfix.json', 'preset=bad', 's'])));

    AssertBytesEqual(TEncoding.ASCII.GetBytes('const A = 1;'#10'end.'), TFile.ReadAllBytes(lFileName));
  finally
    DeleteTree(lRootPath);
  end;
end;

procedure TEncodingFixToolIntegrationTests.DryRunLeavesAsciiLfUnchangedWhenNormalizingEol;
var
  lFileName: string;
  lRootPath: string;
begin
  lRootPath := TPath.Combine(TPath.GetTempPath, 'EncodingFix-DUnitX-' + TGuid.NewGuid.ToString);
  TDirectory.CreateDirectory(lRootPath);
  try
    lFileName := WriteBytes(lRootPath, 'lf-only.pas', TEncoding.ASCII.GetBytes('unit Demo;'#10'interface'#10'end.'));

    Assert.AreEqual(0, Integer(RunTool(lRootPath, ['recursive=n', 'ext=pas', 'eol=crlf', 'dry'])));

    AssertBytesEqual(TEncoding.ASCII.GetBytes('unit Demo;'#10'interface'#10'end.'), TFile.ReadAllBytes(lFileName));
  finally
    DeleteTree(lRootPath);
  end;
end;

procedure TEncodingFixToolIntegrationTests.NormalizesAsciiLfToCrlfWhenRequested;
var
  lFileName: string;
  lRootPath: string;
begin
  lRootPath := TPath.Combine(TPath.GetTempPath, 'EncodingFix-DUnitX-' + TGuid.NewGuid.ToString);
  TDirectory.CreateDirectory(lRootPath);
  try
    lFileName := WriteBytes(lRootPath, 'lf-only.pas', TEncoding.ASCII.GetBytes('unit Demo;'#10'interface'#10'end.'));

    Assert.AreEqual(0, Integer(RunTool(lRootPath, ['recursive=n', 'ext=pas', 'eol=crlf', 's'])));

    AssertBytesEqual(TEncoding.ASCII.GetBytes('unit Demo;'#13#10'interface'#13#10'end.'),
      TFile.ReadAllBytes(lFileName));
  finally
    DeleteTree(lRootPath);
  end;
end;

procedure TEncodingFixToolIntegrationTests.NormalizesAsciiCrToCrlfWhenRequested;
var
  lFileName: string;
  lRootPath: string;
begin
  lRootPath := TPath.Combine(TPath.GetTempPath, 'EncodingFix-DUnitX-' + TGuid.NewGuid.ToString);
  TDirectory.CreateDirectory(lRootPath);
  try
    lFileName := WriteBytes(lRootPath, 'cr-only.pas', TEncoding.ASCII.GetBytes('unit Demo;'#13'interface'#13'end.'));

    Assert.AreEqual(0, Integer(RunTool(lRootPath, ['recursive=n', 'ext=pas', 'eol=crlf', 's'])));

    AssertBytesEqual(TEncoding.ASCII.GetBytes('unit Demo;'#13#10'interface'#13#10'end.'),
      TFile.ReadAllBytes(lFileName));
  finally
    DeleteTree(lRootPath);
  end;
end;

procedure TEncodingFixToolIntegrationTests.NormalizesCP1250LfToCrlfWhenRequested;
var
  lFileName: string;
  lRootPath: string;
begin
  lRootPath := TPath.Combine(TPath.GetTempPath, 'EncodingFix-DUnitX-' + TGuid.NewGuid.ToString);
  TDirectory.CreateDirectory(lRootPath);
  try
    lFileName := WriteBytes(lRootPath, 'cp1250-lf.pas', BytesOfCP1250('unit Demo;'#10'const S = ''zażółć'';'#10'end.'));

    Assert.AreEqual(0, Integer(RunTool(lRootPath, ['recursive=n', 'ext=pas', 'eol=crlf', 's'])));

    Assert.IsTrue(HasUtf8Bom(TFile.ReadAllBytes(lFileName)), 'Expected UTF-8 BOM after conversion.');
    Assert.AreEqual('unit Demo;'#13#10'const S = ''zażółć'';'#13#10'end.', ReadUtf8TextWithoutBom(lFileName));
  finally
    DeleteTree(lRootPath);
  end;
end;

procedure TEncodingFixToolIntegrationTests.NormalizesUtf8LfToCrlfWhenRequested;
var
  lFileName: string;
  lRootPath: string;
begin
  lRootPath := TPath.Combine(TPath.GetTempPath, 'EncodingFix-DUnitX-' + TGuid.NewGuid.ToString);
  TDirectory.CreateDirectory(lRootPath);
  try
    lFileName := WriteBytes(lRootPath, 'utf8-lf.pas', BytesOfUtf8('unit Demo;'#10'const S = ''zażółć'';'#10'end.',
      False));

    Assert.AreEqual(0, Integer(RunTool(lRootPath, ['recursive=n', 'ext=pas', 'eol=crlf', 's'])));

    Assert.IsTrue(HasUtf8Bom(TFile.ReadAllBytes(lFileName)), 'Expected UTF-8 BOM to be added.');
    Assert.AreEqual('unit Demo;'#13#10'const S = ''zażółć'';'#13#10'end.', ReadUtf8TextWithoutBom(lFileName));
  finally
    DeleteTree(lRootPath);
  end;
end;

procedure TEncodingFixToolIntegrationTests.PreservesAsciiLfWhenExplicitlyRequested;
var
  lFileName: string;
  lRootPath: string;
begin
  lRootPath := TPath.Combine(TPath.GetTempPath, 'EncodingFix-DUnitX-' + TGuid.NewGuid.ToString);
  TDirectory.CreateDirectory(lRootPath);
  try
    lFileName := WriteBytes(lRootPath, 'lf-only.pas', TEncoding.ASCII.GetBytes('unit Demo;'#10'interface'#10'end.'));

    Assert.AreEqual(0, Integer(RunTool(lRootPath, ['recursive=n', 'ext=pas', 'eol=preserve', 's'])));

    AssertBytesEqual(TEncoding.ASCII.GetBytes('unit Demo;'#10'interface'#10'end.'), TFile.ReadAllBytes(lFileName));
  finally
    DeleteTree(lRootPath);
  end;
end;

procedure TEncodingFixToolIntegrationTests.PreservesCP1250LfDuringEncodingRepairByDefault;
var
  lFileName: string;
  lRootPath: string;
begin
  lRootPath := TPath.Combine(TPath.GetTempPath, 'EncodingFix-DUnitX-' + TGuid.NewGuid.ToString);
  TDirectory.CreateDirectory(lRootPath);
  try
    lFileName := WriteBytes(lRootPath, 'cp1250-lf.pas', BytesOfCP1250('unit Demo;'#10'const S = ''zażółć'';'#10'end.'));

    Assert.AreEqual(0, Integer(RunTool(lRootPath, ['recursive=n', 'ext=pas', 's'])));

    Assert.IsTrue(HasUtf8Bom(TFile.ReadAllBytes(lFileName)), 'Expected UTF-8 BOM after conversion.');
    Assert.AreEqual('unit Demo;'#10'const S = ''zażółć'';'#10'end.', ReadUtf8TextWithoutBom(lFileName));
  finally
    DeleteTree(lRootPath);
  end;
end;

procedure TEncodingFixToolIntegrationTests.PreservesAsciiLfByDefault;
var
  lFileName: string;
  lRootPath: string;
begin
  lRootPath := TPath.Combine(TPath.GetTempPath, 'EncodingFix-DUnitX-' + TGuid.NewGuid.ToString);
  TDirectory.CreateDirectory(lRootPath);
  try
    lFileName := WriteBytes(lRootPath, 'lf-only.pas', TEncoding.ASCII.GetBytes('unit Demo;'#10'interface'#10'end.'));

    Assert.AreEqual(0, Integer(RunTool(lRootPath, ['recursive=n', 'ext=pas', 's'])));

    AssertBytesEqual(TEncoding.ASCII.GetBytes('unit Demo;'#10'interface'#10'end.'), TFile.ReadAllBytes(lFileName));
  finally
    DeleteTree(lRootPath);
  end;
end;

procedure TEncodingFixToolIntegrationTests.RejectsInvalidBooleanOption;
var
  lFileName: string;
  lRootPath: string;
begin
  lRootPath := TPath.Combine(TPath.GetTempPath, 'EncodingFix-DUnitX-' + TGuid.NewGuid.ToString);
  TDirectory.CreateDirectory(lRootPath);
  try
    lFileName := WriteBytes(lRootPath, 'unchanged.pas', BytesOfUtf8('unit Demo; const S = ''zażółć'';', False));

    Assert.AreEqual(1, Integer(RunTool(lRootPath, ['recursive=n', 'ext=pas', 'utf8-bom=maybe', 's'])));

    Assert.IsFalse(HasUtf8Bom(TFile.ReadAllBytes(lFileName)), 'Invalid options must not rewrite files.');
  finally
    DeleteTree(lRootPath);
  end;
end;

procedure TEncodingFixToolIntegrationTests.RemovesUtf8BomWhenRequested;
var
  lBytes: TBytes;
  lFileName: string;
  lRootPath: string;
begin
  lRootPath := TPath.Combine(TPath.GetTempPath, 'EncodingFix-DUnitX-' + TGuid.NewGuid.ToString);
  TDirectory.CreateDirectory(lRootPath);
  try
    lFileName := WriteBytes(lRootPath, 'utf8-bom.pas', BytesOfUtf8('unit Demo; const S = ''zażółć'';', True));

    Assert.AreEqual(0, Integer(RunTool(lRootPath, ['recursive=n', 'ext=pas', 'utf8-bom=n', 's'])));

    lBytes := TFile.ReadAllBytes(lFileName);
    Assert.IsFalse(HasUtf8Bom(lBytes), 'Expected UTF-8 BOM to be removed.');
    Assert.AreEqual('unit Demo; const S = ''zażółć'';', ReadUtf8TextWithoutBom(lFileName));
  finally
    DeleteTree(lRootPath);
  end;
end;

procedure TEncodingFixToolIntegrationTests.RepairsCP1250FileToUtf8;
var
  lFileName: string;
  lRootPath: string;
begin
  lRootPath := TPath.Combine(TPath.GetTempPath, 'EncodingFix-DUnitX-' + TGuid.NewGuid.ToString);
  TDirectory.CreateDirectory(lRootPath);
  try
    lFileName := WriteBytes(lRootPath, 'cp1250.pas', BytesOfCP1250('unit Demo; const S = ''zażółć'';'));

    Assert.AreEqual(0, Integer(RunTool(lRootPath, ['recursive=n', 'ext=pas', 's'])));

    Assert.IsTrue(HasUtf8Bom(TFile.ReadAllBytes(lFileName)), 'Expected rewritten file to have UTF-8 BOM.');
    Assert.AreEqual('unit Demo; const S = ''zażółć'';', ReadUtf8TextWithoutBom(lFileName));
  finally
    DeleteTree(lRootPath);
  end;
end;

procedure TEncodingFixToolIntegrationTests.RepairsMixedUtf8AndCP1250Lines;
var
  lBytes: TBytes;
  lFileName: string;
  lRootPath: string;
begin
  lRootPath := TPath.Combine(TPath.GetTempPath, 'EncodingFix-DUnitX-' + TGuid.NewGuid.ToString);
  TDirectory.CreateDirectory(lRootPath);
  try
    lBytes := AppendBytes(BytesOfUtf8('unit Demo;'#13#10, False), BytesOfCP1250('const S = ''zażółć'';'#13#10));
    lFileName := WriteBytes(lRootPath, 'mixed.pas', lBytes);

    Assert.AreEqual(0, Integer(RunTool(lRootPath, ['recursive=n', 'ext=pas', 's'])));

    Assert.IsTrue(HasUtf8Bom(TFile.ReadAllBytes(lFileName)), 'Expected mixed file to be rewritten as UTF-8.');
    Assert.AreEqual('unit Demo;'#13#10'const S = ''zażółć'';'#13#10, ReadUtf8TextWithoutBom(lFileName));
  finally
    DeleteTree(lRootPath);
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TEncodingFixToolIntegrationTests);

end.
