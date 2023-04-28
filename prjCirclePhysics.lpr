program prjCirclePhysics;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, tachartlazaruspkg, frmCirclePhysicsMain, unCirclePhysics,
  unBoardRenderer, unTrajectoryPaths, uncirclephysicsconstants, 
unHelperInterfaces, unOtherCircles, unPhysicsInterfaces, unPhysicsArea;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

