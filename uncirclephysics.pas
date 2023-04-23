unit unCirclePhysics;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

type

  { T2DVector }

  T2DVector = class
  private
    FdMagnitude: double;
    FdXLength: double;
    FdYLength: double;
    FdAngle: double;
  public
    constructor CreateWithAngle(dMagnitude, dAngle: double);
    constructor Create(dXLength, dYLength: double);
  end;

  { TBasicVector }

  TBasicVector = class(TPersistent)
  private
    FdInitialVelocity: double;
    FdOriginX: double;
    FdOriginY: double;
    FdFinalX: double;
    FdFinalY: double;
    FdAngle: double;
    FdEndTime: double;
    FdStartTime: double;
    procedure SetAngle(AValue: double);
    procedure SetInitialVelocity(AValue: double);
    function GetXVelAtXDeplacement(const dDesplacement: double): double;
    function GetYVelAtYDeplacement(const dDesplacement: double): double;
    function GetInitialVelX(): double;
    function GetInitialVelY(): double;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(const dOriginX, dOriginY, dVelocity, dAngle, dStartTime: double);
    function GetXAtTime(const dTime: double): double;
    function GetYAtTime(const dTime: double): double;
    function GetTimeToXDeplacement(const dDeplacement: double): double;
    function GetTimeToYDeplacement(const dDeplacement: double): double;
    function GetDecelX(): double;
    function GetDecelY(): double;
    procedure ReverseX();
    procedure ReverseY();
    function ToString(): string;
    property InitialVelocity: double read FdInitialVelocity write SetInitialVelocity;
    property OriginX: double read FdOriginX write FdOriginX;
    property OriginY: double read FdOriginY write FdOriginY;
    property Angle: double read FdAngle write SetAngle;
    property EndTime: double read FdEndTime write FdEndTime;
    property StartTime: double read FdStartTime;
  end;

  { TBasicMotion }

  TBasicMotion = class
  public
    class function GetTimeToStop(const dVelocity: double): double;
    class function GetDistanceToStop(const dVelocity: double): double;
    class function GetDistanceAtTime(const dVelocity: double;
      const dTime: double): double;
    class function GetVelocityAtTime(const dVelocity: double;
      const dTime: double): double;
    class function GetVelocityAtDistance(const dVelocity: double;
      const dDistance: double): double;
    class function RadToDeg(const dRadians: double): double;
    class function DegToRad(const dDegrees: double): double;
  end;




implementation

uses
  LazLogger, Math;

{*
v = u + at.
v² = u² + 2as.
s = ut + ½at²
*}

const
  DECELERATION = -0.00125;

{ T2DVector }

constructor T2DVector.CreateWithAngle(dMagnitude, dAngle: double);
begin
  FdMagnitude := dMagnitude;
  FdAngle := dAngle;
  FdXLength := FdMagnitude * Cos(dAngle);
  FdYLength := FdMagnitude * Sin(FdAngle);
end;

constructor T2DVector.Create(dXLength, dYLength: double);
begin
  FdXLength := dXLength;
  FdYLength := dYLength;
  FdAngle := 0;
  FdMagnitude := 0;
  if (FdXLength <> 0) or (FdYLength <> 0) then
  begin
    FdMagnitude := Sqrt(sqr(FdXLength) + Sqr(FdYLength));

    if (dXLength <> 0) then
      FdAngle := ArcTan(dYLength / dXLength)
    else
      FdAngle := Math.ArcSin(dYLength / FdMagnitude);
  end;
end;

{ TBasicMotion }

class function TBasicMotion.RadToDeg(const dRadians: double): double;
begin
  Result := (180 / pi) * dRadians;
end;

class function TBasicMotion.DegToRad(const dDegrees: double): double;
begin
  Result := (pi / 180) * dDegrees;
end;

{ TBasicVector }

procedure TBasicVector.SetInitialVelocity(AValue: double);
begin
  if FdInitialVelocity = AValue then Exit;
  if FdInitialVelocity < 0 then Exit;
  FdInitialVelocity := AValue;
end;

function TBasicVector.GetXVelAtXDeplacement(const dDesplacement: double): double;
begin
  Result := Sqr(GetInitialVelX()) + (2 * GetDecelX() * dDesplacement);
end;

function TBasicVector.GetYVelAtYDeplacement(const dDesplacement: double): double;
begin
  Result := Sqr(GetInitialVelY()) + (2 * GetDecelY() * dDesplacement);
end;

function TBasicVector.GetInitialVelX: double;
begin
  Result := FdInitialVelocity * cos(FdAngle);
end;

function TBasicVector.GetInitialVelY: double;
begin
  Result := FdInitialVelocity * Sin(FdAngle);
end;

procedure TBasicVector.AssignTo(Dest: TPersistent);
var
  ADest: TBasicVector;
begin
  ADest := TBasicVector(Dest);
  ADest.FdInitialVelocity := FdInitialVelocity;
  ADest.FdOriginX := FdOriginX;
  ADest.FdOriginY := FdOriginY;
  ADest.FdAngle := FdAngle;
end;

procedure TBasicVector.SetAngle(AValue: double);
begin
  if FdAngle = AValue then Exit;
  while (AValue > (2 * Pi)) do
  begin
    AValue := AValue - (2 * Pi);
  end;
  while (AValue < 0) do
  begin
    AValue := AValue + (2 * pi);
  end;
  FdAngle := AValue;
end;

constructor TBasicVector.Create(const dOriginX, dOriginY, dVelocity,
  dAngle, dStartTime: double);
begin
  FdOriginX := dOriginX;
  FdOriginY := dOriginY;
  FdInitialVelocity := dVelocity;
  FdAngle := dAngle;
  FdStartTime := dStartTime;
  FdEndTime := FdStartTime + TBasicMotion.GetTimeToStop(FdInitialVelocity);
end;

function TBasicVector.GetXAtTime(const dTime: double): double;
begin
  Result := FdOriginX + (TBasicMotion.GetDistanceAtTime(FdInitialVelocity, dTime) *
    cos(FdAngle));
end;

function TBasicVector.GetYAtTime(const dTime: double): double;
begin
  Result := FdOriginY + (TBasicMotion.GetDistanceAtTime(FdInitialVelocity, dTime) *
    sin(FdAngle));
end;

function TBasicVector.GetTimeToXDeplacement(const dDeplacement: double): double;
  // from V=U + AT
  // => AT = V - U
  // => T = (V-U)/A
var
  dVectorDeplacement: double;
begin
  dVectorDeplacement := abs(dDeplacement / Cos(FdAngle));
  Result := Abs((FdInitialVelocity - TBasicMotion.GetVelocityAtDistance(
    FdInitialVelocity, dVectorDeplacement)) / DECELERATION);
end;

function TBasicVector.GetTimeToYDeplacement(const dDeplacement: double): double;
var
  dVectorDeplacement: double;
begin
  dVectorDeplacement := abs(dDeplacement / sin(FdAngle));
  Result := Abs((FdInitialVelocity - TBasicMotion.GetVelocityAtDistance(
    FdInitialVelocity, dVectorDeplacement)) / DECELERATION);
end;

function TBasicVector.GetDecelX: double;
begin
  Result := DECELERATION * Cos(FdAngle);
end;

function TBasicVector.GetDecelY: double;
begin
  Result := DECELERATION * Sin(FdAngle);
end;

procedure TBasicVector.ReverseX;
begin
  Angle := pi - Angle;
end;

procedure TBasicVector.ReverseY;
begin
  Angle := -Angle;
end;

function TBasicVector.ToString: string;
begin
  Result := Format('%F X:%F, Y:%F, V:%F, A:%F (%F)',
    [StartTime, OriginX, OriginY, InitialVelocity, Angle, (180 / pi) * Angle]);
end;


{ TBasicPhysics }

class function TBasicMotion.GetTimeToStop(const dVelocity: double): double;
  // From V = U + AT
  // -> 0 = U + AT
  // -> -U = A/T
  // -> T = -U/A;
begin
  Result := Abs((dVelocity) / DECELERATION);
end;


class function TBasicMotion.GetDistanceToStop(const dVelocity: double): double;
  // From V*V = (U * U) + 2AX
  // -> 0 = (U * U) + 2AX
  // -> -(U*U) = 2AX
  // -> X = -(U*U)/ 2A;
begin
  Result := (-(Sqr(dVelocity)) / (2 * DECELERATION));
end;


class function TBasicMotion.GetDistanceAtTime(const dVelocity: double;
  const dTime: double): double;
  // FROM X = UT + ((0.5A) * (T * T));
begin
  if (dTime > GetTimeToStop(dVelocity)) then
    Result := GetDistanceToStop(dVelocity)
  else
    Result := (dVelocity * dTime) + ((DECELERATION / 2) * Sqr(dTime));
end;

class function TBasicMotion.GetVelocityAtTime(const dVelocity: double;
  const dTime: double): double;
begin
  if (dTime > GetTimeToStop(dVelocity)) then
    Result := 0.0
  else
    Result := dVelocity + (DECELERATION * dTime);
end;

class function TBasicMotion.GetVelocityAtDistance(const dVelocity: double;
  const dDistance: double): double;
  // FROM (v*v) = (u*U) + (2AX)
begin
  if (dDistance > GetDistanceToStop(dVelocity)) then
    Result := 0.0
  else
    Result := Sqrt(Sqr(dVelocity) + (2 * DECELERATION * dDistance));
end;



end.
