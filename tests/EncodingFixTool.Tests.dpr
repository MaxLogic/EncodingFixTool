program EncodingFixTool.Tests;

{$APPTYPE CONSOLE}
{$STRONGLINKTYPES ON}

uses
  System.SysUtils,
  DUnitX.Loggers.Console,
  DUnitX.TestFramework,
  DUnitX.TestRunner,
  EncodingFixTool.IntegrationTests in 'EncodingFixTool.IntegrationTests.pas';

var
  lLogger: ITestLogger;
  lResults: IRunResults;
  lRunner: ITestRunner;
begin
  try
    TDUnitX.CheckCommandLine;

    lRunner := TDUnitX.CreateRunner;
    lRunner.UseRTTI := True;
    lRunner.FailsOnNoAsserts := True;

    lLogger := TDUnitXConsoleLogger.Create(False);
    lRunner.AddLogger(lLogger);

    lResults := lRunner.Execute;
    if not lResults.AllPassed then
    begin
      System.ExitCode := 1;
    end;
  except
    on e: Exception do
    begin
      Writeln(e.ClassName + ': ' + e.Message);
      System.ExitCode := 1;
    end;
  end;
end.
