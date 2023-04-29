unit unCirclePhysics;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, matrix;

type

  { T2DVector }

  T2DVector = class
  private
    FVector: Tvector2_double;
    function GetMagnitude: Double;
    function GetAngle: Double;
    function GetVector: Tvector2_double;
  public
    constructor CreateWithAngle(const dMagnitude, dAngle: double);
    constructor Create(const dXLength, dYLength: double);
    function GetNormalised: T2DVector;
    function GetDotProduct(const AVector: T2DVector): Double;
    function GetDotProductV(const AVec: Tvector2_double): Double;
    function Minus(const AVector : T2DVector): T2DVector;
    property Magnitude: double read GetMagnitude;
    property Angle: double read GetAngle;
    property Vector : Tvector2_double read GetVector;
  end;

  { TBasicVector }

  TBasicVector = class(TObject)
  private
    FdInitialVelocity: double;
    FdOriginX: double;
    FdOriginY: double;
    FdAngle: double;
    FdEndTime: double;
    FdStartTime: double;
    function GetVector: Tvector2_double;
    procedure SetAngle(AValue: double);
    procedure SetInitialVelocity(const AValue: double);
    function GetXVelAtXDeplacement(const dDesplacement: double): double;
    function GetYVelAtYDeplacement(const dDesplacement: double): double;
    function GetInitialVelX(): double;
    function GetInitialVelY(): double;
  public
    constructor Create(const dOriginX, dOriginY, dVelocity, dAngle, dStartTime: double);
    function GetXAtTime(const dTime: double): double;
    function GetYAtTime(const dTime: double): double;
    function GetTimeToXDeplacement(const dDeplacement: double): double;
    function GetTimeToYDeplacement(const dDeplacement: double): double;
    function GetDisplacementXAtStop: double;
    function GetDisplacementYAtStop: double;
    function GetTimeToStop: Double;
    function GetDecelX(): double;
    function GetDecelY(): double;
    procedure ReverseX();
    procedure ReverseY();
    function GetVelocityVectorAtTime(const dTime: Double): Tvector2_double;
    function ToString(): string; override;
    property InitialVelocity: double read FdInitialVelocity write SetInitialVelocity;
    property OriginX: double read FdOriginX write FdOriginX;
    property OriginY: double read FdOriginY write FdOriginY;
    property Angle: double read FdAngle write SetAngle;
    property EndTime: double read FdEndTime write FdEndTime;
    property StartTime: double read FdStartTime;
    property Vector : Tvector2_double read GetVector;
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

function T2DVector.GetMagnitude: Double;
begin
  RESULT := FVector.length;
end;

function T2DVector.GetAngle: Double;
begin
  RESULT := ArcTan2(FVector.data[1], FVector.data[0])
  {
  RESULT := 0;
  if (FVector.data[0] <> 0) or (FVector.data[1] <> 0) then
    begin
      if (FVector.data[0] <> 0) then
        RESULT := ArcTan(FVector.data[1] / FVector.data[0])
      else if FVector.data[0] > 0 then
        RESULT := PI/2
      else
        RESULT := 1.5 * PI;
    end;
    }
end;

function T2DVector.GetVector: Tvector2_double;
begin
  RESULT := FVector;
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
    RESULT := T2DVector.Create(FVector.data[0]/FVector.length, FVector.data[1]/FVector.length)
  else
    RESULT := T2DVector.Create(0,0);
end;

function T2DVector.GetDotProduct(const AVector: T2DVector): Double;
begin
  RESULT := FVector.data[0] * AVector.Vector.Data[0] + FVector.Data[1] * AVector.Vector.Data[1];
end;

function T2DVector.GetDotProductV(const AVec: Tvector2_double): Double;
begin
RESULT := FVector.data[0] * AVec.Data[0] + FVector.Data[1] * AVec.Data[1];
end;

function T2DVector.Minus(const AVector: T2DVector): T2DVector;
var
  Vec : Tvector2_double;
begin
  Vec := AVector.Vector - FVector;
  RESULT := T2DVector.Create(Vec.Data[0], Vec.Data[1]);
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

function TBasicVector.GetVector: Tvector2_double;
begin
  RESULT.init(GetXAtTime(FdEndTime- FdStartTime),GetYAtTime(FdEndTime- FdStartTime));
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
  if (dTime <= GetTimeToStop) then
    Result := FdOriginX + (TBasicMotion.GetDistanceAtTime(FdInitialVelocity, dTime) *
      cos(FdAngle))
 // else
 //   RESULT := FdOriginX + GetDisplacementXAtStop;
end;

function TBasicVector.GetYAtTime(const dTime: double): double;
begin
  if (dTime <= GetTimeToStop) then
     Result := FdOriginY + (TBasicMotion.GetDistanceAtTime(FdInitialVelocity, dTime) *
     sin(FdAngle))
//  else
 //   Result := FdOriginY + GetDisplacementYAtStop;
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

function TBasicVector.GetDisplacementXAtStop: double;
begin
  RESULT := GetXAtTime(GetTimeToStop);
end;

function TBasicVector.GetDisplacementYAtStop: double;
begin
  RESULT := GetYAtTime(GetTimeToStop);
end;

function TBasicVector.GetTimeToStop: Double;
begin
  RESULT := TBasicMotion.GetTimeToStop(InitialVelocity)
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

function TBasicVector.GetVelocityVectorAtTime(const dTime: Double
  ): Tvector2_double;
var
  dVel : Double;
begin
  dVel := TBasicMotion.GetVelocityAtTime(FdInitialVelocity, dTime);
  RESULT.init(dVel * Cos(FdAngle), dVel * Sin(FdAngle));
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

class function TBasicMotion.GetTimeToDistance(const dVelocity: double;
  dDistance: double): double;
// from V=U + AT
// => AT = V - U
// => T = (V-U)/A
begin

Result := Abs((dVelocity - TBasicMotion.GetVelocityAtDistance(
  dVelocity, dDistance)) / DECELERATION);
end;



end.
