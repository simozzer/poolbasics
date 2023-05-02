unit unCirclePhysics;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, matrix, unHelperInterfaces;

type

  { T2DVector }

  T2DVector = class
  private
    FVector: Tvector2_double;
    function GetMagnitude: double;
    function GetAngle: double;
    function GetVector: Tvector2_double;
  public
    constructor CreateWithAngle(const dMagnitude, dAngle: double);
    constructor Create(const dXLength, dYLength: double);
    function GetNormalised: T2DVector;
    function GetDotProduct(const AVector: T2DVector): double;
    function GetDotProductV(const AVec: Tvector2_double): double;
    function Minus(const AVector: T2DVector): T2DVector;
    property Magnitude: double read GetMagnitude;
    property Angle: double read GetAngle;
    property Vector: Tvector2_double read GetVector;
  end;

  { TBasicVector }

  TBasicVector = class(TInterfacedObject, IBasicVector)
  private
    FdInitialVelocity: double;
    FdOriginX: double;
    FdOriginY: double;
    FdAngle: double;
    FdEndTime: double;
    FdStartTime: double;
    function GetVector: Tvector2_double;
    function GetAngle: double;
    procedure SetAngle(AValue: double);
    function GetInitialVelocity: double;
    procedure SetInitialVelocity(const AValue: double);
    function GetOriginX: double;
    function GetOriginY: double;
    procedure SetOriginX(const dX: double);
    procedure SetOriginY(const dY: double);
    function GetEndTime: double;
    procedure SetEndTime(const dTime: double);
    function GetStartTime: double;
  public
    constructor Create(const dOriginX, dOriginY, dVelocity, dAngle, dStartTime: double);
    function GetXAtTime(const dTime: double): double;
    function GetYAtTime(const dTime: double): double;
    function GetTimeToXDeplacement(const dDeplacement: double): double;
    function GetTimeToYDeplacement(const dDeplacement: double): double;
    function GetDisplacementXAtStop: double;
    function GetDisplacementYAtStop: double;
    function GetTimeToStop: double;
    procedure ReverseX();
    procedure ReverseY();
    function GetVelocityVectorAtTime(const dTime: double): Tvector2_double;
    function ToString(): string; override;
    property InitialVelocity: double read FdInitialVelocity write SetInitialVelocity;
    property OriginX: double read GetOriginX write SetOriginX;
    property OriginY: double read GetOriginY write SetOriginY;
    property Angle: double read GetAngle write SetAngle;
    property EndTime: double read GetEndTime write SetEndTime;
    property StartTime: double read GetStartTime;
    property Vector: Tvector2_double read GetVector;
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
    class function GetTimeToDistance(const dVelocity: double; dDistance: double): double;
    class function RadToDeg(const dRadians: double): double;
    class function DegToRad(const dDegrees: double): double;
  end;




implementation

uses
  LazLogger, Math, uncirclephysicsconstants;

{*
v = u + at.
v² = u² + 2as.
s = ut + ½at²
*}


{ T2DVector }

function T2DVector.GetMagnitude: double;
begin
  Result := FVector.length;
end;

function T2DVector.GetAngle: double;
begin
  Result := ArcTan2(FVector.Data[1], FVector.Data[0]);
end;

function T2DVector.GetVector: Tvector2_double;
begin
  Result := FVector;
end;

constructor T2DVector.CreateWithAngle(const dMagnitude, dAngle: double);
begin
  FVector.init(dMagnitude * Cos(dAngle), dMagnitude * Sin(dAngle));
end;

constructor T2DVector.Create(const dXLength, dYLength: double);
begin
  FVector.init(dXLength, dYLength);
end;

function T2DVector.GetNormalised: T2DVector;
begin
  if (FVector.length > 0) then
    Result := T2DVector.Create(FVector.Data[0] / FVector.length,
      FVector.Data[1] / FVector.length)
  else
    Result := T2DVector.Create(0, 0);
end;

function T2DVector.GetDotProduct(const AVector: T2DVector): double;
begin
  Result := FVector.Data[0] * AVector.Vector.Data[0] + FVector.Data[1] *
    AVector.Vector.Data[1];
end;

function T2DVector.GetDotProductV(const AVec: Tvector2_double): double;
begin
  Result := FVector.Data[0] * AVec.Data[0] + FVector.Data[1] * AVec.Data[1];
end;

function T2DVector.Minus(const AVector: T2DVector): T2DVector;
var
  Vec: Tvector2_double;
begin
  Vec := AVector.Vector - FVector;
  Result := T2DVector.Create(Vec.Data[0], Vec.Data[1]);
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

procedure TBasicVector.SetInitialVelocity(const AValue: double);
begin
  if FdInitialVelocity = AValue then Exit;
  if FdInitialVelocity < 0 then Exit;
  FdInitialVelocity := AValue;
end;

function TBasicVector.GetOriginX: double;
begin
  Result := FdOriginX;
end;

function TBasicVector.GetOriginY: double;
begin
  Result := FdOriginY;
end;

procedure TBasicVector.SetOriginX(const dX: double);
begin
  FdOriginX := dX;
end;

procedure TBasicVector.SetOriginY(const dY: double);
begin
  FdOriginY := dY;
end;

function TBasicVector.GetEndTime: double;
begin
  Result := FdEndTime;
end;

procedure TBasicVector.SetEndTime(const dTime: double);
begin
  FdEndTime := dTime;
end;

function TBasicVector.GetStartTime: double;
begin
  Result := FdStartTime;
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

function TBasicVector.GetInitialVelocity: double;
begin
  Result := FdInitialVelocity;
end;

function TBasicVector.GetVector: Tvector2_double;
begin
  Result.init(GetXAtTime(FdEndTime - FdStartTime), GetYAtTime(FdEndTime - FdStartTime));
end;

function TBasicVector.GetAngle: double;
begin
  Result := FdAngle;
end;

constructor TBasicVector.Create(
  const dOriginX, dOriginY, dVelocity, dAngle, dStartTime: double);
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
  if (FdAngle = 0) then
    dVectorDeplacement := dDeplacement
  else
    dVectorDeplacement := abs(dDeplacement / sin(FdAngle));
  Result := Abs((FdInitialVelocity - TBasicMotion.GetVelocityAtDistance(
    FdInitialVelocity, dVectorDeplacement)) / DECELERATION);
end;

function TBasicVector.GetDisplacementXAtStop: double;
begin
  Result := GetXAtTime(GetTimeToStop);
end;

function TBasicVector.GetDisplacementYAtStop: double;
begin
  Result := GetYAtTime(GetTimeToStop);
end;

function TBasicVector.GetTimeToStop: double;
begin
  Result := TBasicMotion.GetTimeToStop(InitialVelocity);
end;

procedure TBasicVector.ReverseX;
begin
  Angle := pi - Angle;
end;

procedure TBasicVector.ReverseY;
begin
  Angle := -Angle;
end;

function TBasicVector.GetVelocityVectorAtTime(const dTime: double): Tvector2_double;
var
  dVel: double;
begin
  dVel := TBasicMotion.GetVelocityAtTime(FdInitialVelocity, dTime);
  Result.init(dVel * Cos(FdAngle), dVel * Sin(FdAngle));
end;

function TBasicVector.ToString: string;
begin
  Result := Format('%F X:%F, Y:%F, V:%F, A:%F (%F)',
    [StartTime, OriginX, OriginY, InitialVelocity, Angle, TBasicMotion.RadToDeg(Angle)]);
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

class function TBasicMotion.GetTimeToDistance(const dVelocity: double;
  dDistance: double): double;
  // from V=U + AT
  // => AT = V - U
  // => T = (V-U)/A
begin

  Result := Abs((dVelocity - TBasicMotion.GetVelocityAtDistance(dVelocity, dDistance)) /
    DECELERATION);
end;



end.
