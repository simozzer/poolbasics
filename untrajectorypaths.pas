unit unTrajectoryPaths;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, unCirclePhysics, FGL;

type
  { TTrajectoryPath }

  TTrajectoryPath= class
  private
    FTrajectories: TList;
  public
    property Trajectories : TList read FTrajectories;
    function GetXAtTime(const dTime : Double): Double;
    function GetYAtTime(const dTime : Double): Double;
    function GetVectorForTime(const dTime: Double): TBasicVector;
    constructor Create;
    destructor Destroy; override;
  end;



implementation

{ TTrajectoryPath }

function TTrajectoryPath.GetVectorForTime(const dTime: Double): TBasicVector;
var
  AVector : TBasicVector;
  i : Integer;
begin
  i := 0;
  RESULT := nil;
  while (RESULT = nil) and (i < FTrajectories.Count) do
  begin
    AVector := TBasicVector(FTrajectories[i]);
    if (dTime >= AVector.StartTime) and (dTime <= AVector.EndTime) then
      RESULT := AVector
    else
      inc(i);
  end;
  if (Result = nil) and (FTrajectories.Count > 0) then
    AVector := TBasicVector(FTrajectories[FTrajectories.Count-1]);

end;

function TTrajectoryPath.GetXAtTime(const dTime: Double): Double;
var
  AVector : TBasicVector;
  dTimeInVector : Double;
begin
  RESULT := 0;
  AVector := GetVectorForTime(dTime);
  if (AVector <> nil) then
  begin
    dTimeInVector := dTime - AVector.StartTime;
    RESULT := AVector.GetXAtTime(dTimeInVector);
  end;
end;

function TTrajectoryPath.GetYAtTime(const dTime: Double): Double;
var
  AVector : TBasicVector;
  dTimeInVector : Double;
begin
  RESULT := 0;
  AVector := GetVectorForTime(dTime);
  if (AVector <> nil) then
  begin
    dTimeInVector := dTime - AVector.StartTime;
    RESULT := AVector.GetYAtTime(dTimeInVector);
  end;
end;

constructor TTrajectoryPath.Create;
begin
  FTrajectories := TList.Create;
end;

destructor TTrajectoryPath.Destroy;
begin
  FTrajectories.Free;
  inherited Destroy;
end;

end.

