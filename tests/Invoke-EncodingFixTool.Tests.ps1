param(
  [string]$ToolPath = (Join-Path $PSScriptRoot '..\bin\EncodingFixTool.exe')
)

$ErrorActionPreference = 'Stop'

function Assert-True {
  param(
    [bool]$Condition,
    [string]$Message
  )

  if (-not $Condition) {
    throw $Message
  }
}

function New-TestRoot {
  $lRoot = Join-Path $env:TEMP ('EncodingFixToolTests-' + [guid]::NewGuid().ToString('N'))
  New-Item -ItemType Directory -Path $lRoot | Out-Null
  return $lRoot
}

function Invoke-Tool {
  param(
    [string[]]$Arguments
  )

  $lOutput = & $ToolPath @Arguments 2>&1
  return @{
    ExitCode = $LASTEXITCODE
    Output = ($lOutput -join "`n")
  }
}

Assert-True (Test-Path -LiteralPath $ToolPath) "Tool not found: $ToolPath"

$lRoot = New-TestRoot
try {
  $lReadOnlyFile = Join-Path $lRoot 'readonly.pas'
  [System.IO.File]::WriteAllBytes(
    $lReadOnlyFile,
    [System.Text.UTF8Encoding]::new($false).GetBytes("unit readonly; interface const S = 'zażółć'; implementation end.")
  )
  Set-ItemProperty -LiteralPath $lReadOnlyFile -Name IsReadOnly -Value $true

  $lResult = Invoke-Tool @("path=$lRoot", 'recursive=n', 'ext=pas', 'v')

  Assert-True ($lResult.ExitCode -eq 1) "Read-only failure path should return exit code 1."
  Assert-True ($lResult.Output -match 'FAIL : readonly\.pas') "Expected read-only file to be reported as FAIL."
  Assert-True ($lResult.Output -match 'Save failed: .+') "Expected save failure to include the underlying error message."

  Set-ItemProperty -LiteralPath $lReadOnlyFile -Name IsReadOnly -Value $false

  $lUtf8File = Join-Path $lRoot 'utf8.pas'
  [System.IO.File]::WriteAllBytes(
    $lUtf8File,
    [System.Text.UTF8Encoding]::new($false).GetBytes("unit utf8; interface const S = 'zażółć'; implementation end.")
  )

  $lResult = Invoke-Tool @("path=$lRoot", 'recursive=n', 'ext=pas')
  Assert-True ($lResult.ExitCode -eq 0) "UTF-8 conversion should succeed."
  Assert-True ($lResult.Output -match 'Fixed: utf8\.pas \(Added UTF-8 BOM\)') "Expected non-ASCII UTF-8 file to receive BOM."

  $lBytes = [System.IO.File]::ReadAllBytes($lUtf8File)
  Assert-True (($lBytes[0] -eq 0xEF) -and ($lBytes[1] -eq 0xBB) -and ($lBytes[2] -eq 0xBF)) "UTF-8 file should start with BOM."

  $lAsciiFile = Join-Path $lRoot 'ascii.pas'
  [System.IO.File]::WriteAllBytes(
    $lAsciiFile,
    [System.Text.Encoding]::ASCII.GetBytes('unit ascii; interface implementation end.')
  )

  $lResult = Invoke-Tool @("path=$lRoot", 'recursive=n', 'ext=pas', 'v')
  Assert-True ($lResult.ExitCode -eq 0) "ASCII check should succeed."
  Assert-True ($lResult.Output -match 'OK   : ascii\.pas \(US-ASCII OK\)') "Expected ASCII file to remain unchanged."

  $lBytes = [System.IO.File]::ReadAllBytes($lAsciiFile)
  Assert-True (-not (($lBytes.Length -ge 3) -and ($lBytes[0] -eq 0xEF) -and ($lBytes[1] -eq 0xBB) -and ($lBytes[2] -eq 0xBF))) "ASCII file must not receive BOM."

  $lUtf16File = Join-Path $lRoot 'utf16le.pas'
  $lUtf16 = [System.Text.UnicodeEncoding]::new($false, $true)
  [System.IO.File]::WriteAllBytes(
    $lUtf16File,
    $lUtf16.GetPreamble() + $lUtf16.GetBytes("unit utf16le; interface const S = 'zażółć'; implementation end.")
  )

  $lResult = Invoke-Tool @("path=$lRoot", 'recursive=n', 'ext=pas')
  Assert-True ($lResult.ExitCode -eq 0) "UTF-16 LE conversion should succeed."
  Assert-True ($lResult.Output -match 'Fixed: utf16le\.pas \(detected UTF-16 LE; saved UTF-8 \(BOM=Y\)\)') "Expected UTF-16 LE file to be converted deliberately."

  $lBytes = [System.IO.File]::ReadAllBytes($lUtf16File)
  Assert-True (($lBytes[0] -eq 0xEF) -and ($lBytes[1] -eq 0xBB) -and ($lBytes[2] -eq 0xBF)) "Converted UTF-16 LE file should start with UTF-8 BOM."
  $lConvertedText = [System.Text.UTF8Encoding]::new($true).GetString($lBytes)
  Assert-True ($lConvertedText.Contains("zażółć")) "Converted UTF-16 LE file should preserve Unicode text."

  $lResult = Invoke-Tool @("path=$lRoot", 'recursive=n', 'utf8-bom=maybe')
  Assert-True ($lResult.ExitCode -eq 1) "Invalid boolean values should return exit code 1."
  Assert-True ($lResult.Output -match 'ERROR: invalid utf8-bom value: maybe') "Invalid boolean value should be explained."

  $lLfOnlyFile = Join-Path $lRoot 'lfonly.pas'
  [System.IO.File]::WriteAllBytes(
    $lLfOnlyFile,
    [System.Text.Encoding]::ASCII.GetBytes("unit lfonly;`ninterface`nimplementation`nend.")
  )

  $lResult = Invoke-Tool @("path=$lRoot", 'recursive=n', 'ext=pas', 'eol=crlf', 'dry')
  Assert-True ($lResult.ExitCode -eq 0) "EOL dry-run should succeed."
  Assert-True ($lResult.Output -match 'Would fix: lfonly\.pas \(Would normalize EOL to CRLF \(dry-run\)\)') "Expected dry-run to report EOL-only normalization."

  $lBytes = [System.IO.File]::ReadAllBytes($lLfOnlyFile)
  Assert-True (($lBytes | Where-Object { $_ -eq 13 }).Count -eq 0) "Dry-run must not rewrite LF-only file."

  $lVerboseFile = Join-Path $lRoot 'verbose-eol.pas'
  [System.IO.File]::WriteAllBytes(
    $lVerboseFile,
    [System.Text.Encoding]::ASCII.GetBytes("unit verboseeol;`ninterface`nimplementation`nend.")
  )

  $lResult = Invoke-Tool @("path=$lRoot", 'recursive=n', 'ext=pas', 'eol=crlf', 'v')
  Assert-True ($lResult.ExitCode -eq 0) "Verbose EOL normalization should succeed."
  Assert-True ($lResult.Output -match 'Fixed: verbose-eol\.pas \(Normalized EOL to CRLF\)') "Expected verbose run to report EOL-only normalization."

  $lBinaryDfmFile = Join-Path $lRoot 'binary.dfm'
  $lBinaryDfmBytes = [byte[]](0x54, 0x50, 0x46, 0x30, 0x00, 0x01, 0xFF, 0x80, 0x0D, 0x0A, 0x00, 0x02)
  [System.IO.File]::WriteAllBytes($lBinaryDfmFile, $lBinaryDfmBytes)

  $lResult = Invoke-Tool @("path=$lRoot", 'recursive=n', 'ext=dfm', 'v')
  Assert-True ($lResult.ExitCode -eq 0) "Binary DFM skip should succeed."
  Assert-True ($lResult.Output -match 'OK\s+: binary\.dfm \(Skipped binary DFM\)') "Expected verbose run to report skipped binary DFM."
  Assert-True ([Convert]::ToBase64String($lBinaryDfmBytes) -eq [Convert]::ToBase64String([System.IO.File]::ReadAllBytes($lBinaryDfmFile))) "Binary DFM bytes must remain unchanged."

  $lJsonRoot = Join-Path $lRoot 'json'
  New-Item -ItemType Directory -Path $lJsonRoot | Out-Null
  [System.IO.File]::WriteAllBytes(
    (Join-Path $lJsonRoot 'generated.pas'),
    [System.Text.Encoding]::ASCII.GetBytes("unit generated;`ninterface`nend.")
  )
  [System.IO.File]::WriteAllBytes((Join-Path $lJsonRoot 'binary.dfm'), $lBinaryDfmBytes)

  $lResult = Invoke-Tool @("path=$lJsonRoot", 'preset=delphi-ai', 'format=json')
  Assert-True ($lResult.ExitCode -eq 0) "JSON AI workflow run should succeed."
  $lJson = $lResult.Output | ConvertFrom-Json
  Assert-True (($lJson.scanned -eq 2) -and ($lJson.changed -eq 1) -and ($lJson.skipped -eq 1) -and ($lJson.failed -eq 0)) "Expected compact JSON summary counts for AI workflow."

  Write-Host 'EncodingFixTool CLI tests passed.'
} finally {
  if (Test-Path -LiteralPath $lRoot) {
    Get-ChildItem -LiteralPath $lRoot -Recurse -Force | ForEach-Object {
      if (-not $_.PSIsContainer) {
        $_.IsReadOnly = $false
      }
    }
    Remove-Item -LiteralPath $lRoot -Recurse -Force
  }
}
